set_property IOSTANDARD LVCMOS33 [get_ports sys_clk]
set_property IOSTANDARD LVCMOS18 [get_ports rst_n]
set_property IOSTANDARD LVCMOS33 [get_ports uart_rx]
set_property IOSTANDARD LVCMOS33 [get_ports uart_tx]
set_property IOSTANDARD LVCMOS33 [get_ports sdio_clk_0]
set_property IOSTANDARD LVCMOS33 [get_ports sdio_cmd_0]
set_property IOSTANDARD LVCMOS33 [get_ports {sdio_data_0[*]}]

set_property PACKAGE_PIN M25 [get_ports sys_clk]
set_property PACKAGE_PIN C12 [get_ports rst_n]
set_property PACKAGE_PIN P21 [get_ports uart_rx]
set_property PACKAGE_PIN L22 [get_ports uart_tx]

set_property PACKAGE_PIN AG12 [get_ports sdio_clk_0]
set_property PACKAGE_PIN AH13 [get_ports sdio_cmd_0]
set_property PACKAGE_PIN AJ13 [get_ports {sdio_data_0[0]}]
set_property PACKAGE_PIN AK13 [get_ports {sdio_data_0[1]}]
set_property PACKAGE_PIN AE13 [get_ports {sdio_data_0[2]}]
set_property PACKAGE_PIN AF13 [get_ports {sdio_data_0[3]}]

set_property PACKAGE_PIN AB6 [get_ports pcie_refclk_clk_p]
set_property PACKAGE_PIN AB5 [get_ports pcie_refclk_clk_n]
create_clock -period 10.000 -name pcie_ref_clk [get_ports pcie_refclk_clk_p]

set_property PACKAGE_PIN K22 [get_ports pcie_rst_n]
set_property PULLUP true [get_ports pcie_rst_n]
set_property IOSTANDARD LVCMOS33 [get_ports pcie_rst_n]

set_property PACKAGE_PIN AB2 [get_ports {pcie_mgt_rxp[0]}]
set_property PACKAGE_PIN AC4 [get_ports {pcie_mgt_txp[0]}]
set_property PACKAGE_PIN AD2 [get_ports {pcie_mgt_rxp[1]}]
set_property PACKAGE_PIN AE4 [get_ports {pcie_mgt_txp[1]}]
set_property PACKAGE_PIN AF2 [get_ports {pcie_mgt_rxp[2]}]
set_property PACKAGE_PIN AG4 [get_ports {pcie_mgt_txp[2]}]
set_property PACKAGE_PIN AH2 [get_ports {pcie_mgt_rxp[3]}]
set_property PACKAGE_PIN AH6 [get_ports {pcie_mgt_txp[3]}]

create_clock -period 20.000 -name sys_clk -waveform {0.000 10.000} [get_ports sys_clk]

set main_clock [get_clocks -of_objects [get_ports sys_clk]]
set main_clock_period [get_property -min PERIOD $main_clock]

set tck_pin [get_pins system_i/soc_top_0/inst/jtagtunnel/BSCANE2_inst/TCK]
create_clock -name jtag_clock -period 20.000 $tck_pin

set jtag_clock [get_clocks -of_objects $tck_pin]
set jtag_clock_period [get_property -min PERIOD $jtag_clock]

set_max_delay -reset_path -from $main_clock -to $jtag_clock -datapath_only $jtag_clock_period
set_max_delay -reset_path -from $jtag_clock -to $main_clock -datapath_only $main_clock_period
set_max_delay -from $jtag_clock -to [get_pins system_i/soc_top_0/inst/jtagtunnel/BSCANE2_inst/TDO] [expr $jtag_clock_period / 2]
