"""Check the platform MAC contract, including the actual Xilinx model."""

from amaranth import *
from amaranth.back import verilog
from amaranth.hdl.ir import Fragment

from troom.matrix import IntMac, MacArray
from roomsoc.platform.kc705 import KC705Platform
from roomsoc.platform.ku040 import KU040Platform

import os
from pathlib import Path
import shutil
import subprocess

import pytest


class Chain(Elaboratable):

    def __init__(self, first=False):
        self.first = first
        self.a = Signal(signed(8))
        self.b = Signal(signed(8))
        self.ci = Signal(signed(48))
        self.p = Signal(signed(48))

    def elaborate(self, platform):
        m = Module()
        x = IntMac(first=self.first)
        y = IntMac()
        prefix = 'dsp' if isinstance(platform, KU040Platform) else 'generic'
        m.submodules[f'{prefix}_x'] = x
        m.submodules[f'{prefix}_y'] = y
        m.d.comb += [
            x.a.eq(self.a),
            x.b.eq(self.b),
            x.cascade_in.eq(self.ci),
            y.a.eq(self.b),
            y.b.eq(self.a),
            y.cascade_in.eq(x.cascade_out),
            self.p.eq(y.result)
        ]
        return m


def emit(platform, name, first=False):
    dut = Chain(first=first)
    return verilog.convert(dut,
                           platform=platform,
                           name=name,
                           ports=[dut.a, dut.b, dut.ci, dut.p])


def test_platform_selection():
    assert 'DSP48E2' in emit(KU040Platform(), 'dsp_chain')
    assert 'DSP48E2' not in emit(None, 'generic_chain')
    assert 'DSP48E2' not in emit(KC705Platform(), 'series7_chain')


def test_factory_errors_propagate():

    class Broken:

        def get_int_mac(self, mac):
            raise ValueError('backend failure')

    with pytest.raises(ValueError, match='backend failure'):
        Fragment.get(IntMac(), Broken())


@pytest.mark.parametrize('first', [False, True])
def test_dsp48e2_matches_behavioral(tmp_path, first):
    """Two cascaded cells: signed extremes, overflow, zeros, random, reset."""
    unisim = os.environ.get('VIVADO_UNISIM')
    if not unisim or not shutil.which('iverilog'):
        pytest.skip('set VIVADO_UNISIM to Vivado data/verilog/src/unisims')
    unisim = Path(unisim)
    (tmp_path / 'dsp.v').write_text(emit(KU040Platform(), 'dsp_chain', first))
    (tmp_path / 'ref.v').write_text(emit(None, 'ref_chain', first))
    (tmp_path / 'tb.v').write_text('''
`timescale 1ns/1ps
module tb;
  reg clk=0, rst=1;
  always #5 clk=~clk;
  reg signed [7:0] a=0, b=0;
  reg [47:0] ci=0;
  wire [47:0] actual, expected;
  dsp_chain dut(.clk(clk), .rst(rst), .a(a), .b(b), .ci(ci), .p(actual));
  ref_chain refdut(.clk(clk), .rst(rst), .a(a), .b(b), .ci(ci), .p(expected));
  integer i;
  initial begin
    #200;
    for (i=0; i<300; i=i+1) begin
      @(negedge clk);
      rst = (i < 2 || i == 77 || i == 78 || i == 199);
      case (i % 7)
        0: begin a=-128; b=-128; end
        1: begin a=-128; b=127; end
        2: begin a=127; b=127; end
        3: begin a=0; b=0; end
        default: begin a=$random; b=$random; end
      endcase
      ci = (i % 3 == 0) ? 48'h7fffffffffff : {$random, $random};
      @(posedge clk); #1;
      if (actual !== expected) $fatal(1, "cycle %0d: %h != %h", i, actual, expected);
    end
    $display("PASS DSP48E2 cascade equivalence");
    $finish;
  end
endmodule
''')
    subprocess.run([
        'iverilog', '-g2012', '-s', 'tb', '-s', 'glbl', '-o',
        str(tmp_path / 'sim'),
        str(tmp_path / 'tb.v'),
        str(tmp_path / 'dsp.v'),
        str(tmp_path / 'ref.v'),
        str(unisim / 'DSP48E2.v'),
        str(unisim.parent / 'glbl.v')
    ],
                   check=True,
                   capture_output=True,
                   text=True)
    result = subprocess.run(['vvp', str(tmp_path / 'sim')],
                            check=True,
                            capture_output=True,
                            text=True)
    assert 'PASS DSP48E2 cascade equivalence' in result.stdout


def test_mac_array_cascade_drc(tmp_path):
    """Vendor simulation cannot detect illegal PCOUT-to-fabric routing."""
    vivado = os.environ.get('VIVADO_BIN')
    if not vivado:
        pytest.skip('set VIVADO_BIN to enable the Vivado cascade DRC test')
    dut = MacArray({})
    ports = [
        dut.a_in, *dut.w_in, *dut.meta_in.fields.values(), dut.segs.valid,
        dut.segs.ready, *dut.segs.bits.fields.values()
    ]
    (tmp_path / 'mac.v').write_text(
        verilog.convert(dut,
                        platform=KU040Platform(),
                        name='mac_array',
                        ports=ports))
    (tmp_path / 'check.tcl').write_text('''
read_verilog mac.v
synth_design -top mac_array -part xcku040-ffva1156-2-i -mode out_of_context
opt_design
create_clock -period 10 [get_ports clk]
place_design
report_drc -checks [get_drc_checks {REQP-1940 AVAL-321 DSPS-2}] -file cascade_drc.rpt
set violations [get_drc_violations -quiet]
if {[llength $violations] != 0} {error "Illegal DSP PCOUT connection"}
set pes [get_cells -hier -filter {REF_NAME == DSP48E2 && NAME =~ pe*/dsp}]
if {[llength $pes] != 64} {error "Expected 64 explicit MAC DSPs"}
puts "PASS MAC cascade DRC"
''')
    result = subprocess.run([
        vivado, '-mode', 'batch', '-source', 'check.tcl', '-log', 'vivado.log',
        '-journal', 'vivado.jou'
    ],
                            cwd=tmp_path,
                            capture_output=True,
                            text=True,
                            timeout=300)
    assert result.returncode == 0, result.stdout + result.stderr
    assert 'PASS MAC cascade DRC' in result.stdout, result.stdout
