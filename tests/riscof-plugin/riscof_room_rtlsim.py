import os
import re
import shutil
import subprocess
import shlex
import logging
import random
import string
from string import Template
import sys

import riscof.utils as utils
from riscof.pluginTemplate import pluginTemplate

logger = logging.getLogger()


class room_rtlsim(pluginTemplate):
    __model__ = "room"

    __version__ = "XXX"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        config = kwargs.get('config')

        if config is None:
            print("Please enter input file paths in configuration.")
            raise SystemExit(1)

        self.room_root = config['room_root']
        self.sd_image = config['sd_image']

        self.dut_exe = os.path.join(self.room_root, 'build', 'rtlsim',
                                    'rtlsim')

        self.num_jobs = str(config['jobs'] if 'jobs' in config else 1)

        self.pluginpath = os.path.abspath(config['pluginpath'])

        self.isa_spec = os.path.abspath(config['ispec'])
        self.platform_spec = os.path.abspath(config['pspec'])

        if 'target_run' in config and config['target_run'] == '0':
            self.target_run = False
        else:
            self.target_run = True

    def initialise(self, suite, work_dir, archtest_env):
        self.work_dir = work_dir
        self.suite_dir = suite

        self.compile_cmd = 'riscv{1}-unknown-elf-gcc -march={0} \
         -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles -g\
         -T ' + self.pluginpath + '/env/link.ld\
         -I ' + self.pluginpath + '/env/\
         -I ' + self.room_root + '/include/\
         -I ' + archtest_env + ' {2} -o {3} {4}'

    def build(self, isa_yaml, platform_yaml):
        ispec = utils.load_yaml(isa_yaml)['hart0']

        self.xlen = ('64' if 64 in ispec['supported_xlen'] else '32')

        self.isa = 'rv' + self.xlen
        if "I" in ispec["ISA"]:
            self.isa += 'i'
        if "E" in ispec["ISA"]:
            self.isa += 'e'
        if "M" in ispec["ISA"]:
            self.isa += 'm'
        if "A" in ispec["ISA"]:
            self.isa += 'a'
        if "F" in ispec["ISA"]:
            self.isa += 'f'
        if "D" in ispec["ISA"]:
            self.isa += 'd'
        if "Q" in ispec["ISA"]:
            self.isa += 'q'
        if "C" in ispec["ISA"]:
            self.isa += 'c'
        if "V" in ispec["ISA"]:
            self.isa += 'v'
        if "Zicsr" in ispec["ISA"]:
            self.isa += '_Zicsr'
        if "Zicond" in ispec["ISA"]:
            self.isa += '_Zicond'
        if "Zicboz" in ispec["ISA"]:
            self.isa += '_Zicboz'
        if "Zimop" in ispec["ISA"]:
            self.isa += '_Zimop'
        if "Zfa" in ispec["ISA"]:
            self.isa += '_Zfa'
        if "Zfh" in ispec["ISA"]:
            self.isa += '_Zfh'
        if "Zca" in ispec["ISA"]:
            self.isa += '_Zca'
        if "Zcb" in ispec["ISA"]:
            self.isa += '_Zcb'
        if "Zcmop" in ispec["ISA"]:
            self.isa += '_Zcmop'
        if "Zba" in ispec["ISA"]:
            self.isa += '_Zba'
        if "Zbb" in ispec["ISA"]:
            self.isa += '_Zbb'
        if "Zbc" in ispec["ISA"]:
            self.isa += '_Zbc'
        if "Zbs" in ispec["ISA"]:
            self.isa += '_Zbs'
        if "Zbkb" in ispec["ISA"]:
            self.isa += '_Zbkb'
        if "Zbkc" in ispec["ISA"]:
            self.isa += '_Zbkc'
        if "Zknd" in ispec["ISA"]:
            self.isa += '_Zknd'
        if "Zkne" in ispec["ISA"]:
            self.isa += '_Zkne'
        if "Zbkx" in ispec["ISA"]:
            self.isa += '_Zbkx'
        if "Zknh" in ispec["ISA"]:
            self.isa += '_Zknh'
        if "Zksh" in ispec["ISA"]:
            self.isa += '_Zksh'
        if "Zksed" in ispec["ISA"]:
            self.isa += '_Zksed'

        self.compile_cmd = self.compile_cmd + ' -mabi=' + (
            'lp64 ' if 64 in ispec['supported_xlen'] else
            ('ilp32e ' if "E" in ispec["ISA"] else 'ilp32 '))
        if 'pmp-grain' in ispec['PMP']:
            self.granularity = pow(2, ispec['PMP']['pmp-grain'] + 2)
        else:
            self.granularity = 4

    def runTests(self, testList):
        if os.path.exists(self.work_dir + "/Makefile." + self.name[:-1]):
            os.remove(self.work_dir + "/Makefile." + self.name[:-1])
        make = utils.makeUtil(
            makefilePath=os.path.join(self.work_dir, "Makefile." +
                                      self.name[:-1]))

        make.makeCommand = 'make -k -j' + self.num_jobs

        for testname in testList:
            testentry = testList[testname]
            test = testentry['test_path']
            test_dir = testentry['work_dir']
            elf = os.path.join(test_dir, "my.elf")

            sig_file = os.path.join(test_dir, self.name[:-1] + ".signature")

            compile_macros = ' -D' + " -D".join(testentry['macros'])

            cmd = self.compile_cmd.format(testentry['isa'].lower(), self.xlen,
                                          test, elf, compile_macros)

            if self.target_run:
                simcmd = self.dut_exe + ' {0} -b {1} --signature={2} --signature-granularity=8'.format(
                    self.sd_image, elf, sig_file)
            else:
                simcmd = 'echo "NO RUN"'

            execute = 'cd {}; {}; {};'.format(testentry['work_dir'], cmd,
                                              simcmd)

            make.add_target(execute)

        make.execute_all(self.work_dir, timeout=3600)

        if not self.target_run:
            raise SystemExit(0)
