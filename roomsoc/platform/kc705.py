from amaranth import *

from roomsoc.platform.xilinx import XilinxPlatform


class KC705Platform(XilinxPlatform):
    device = "xc7k325t"
    package = "ffg676"
    speed = "2"
    resources = []
    connectors = []
