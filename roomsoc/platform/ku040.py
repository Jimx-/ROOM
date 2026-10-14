from roomsoc.platform.xilinx import XilinxPlatform


class KU040Platform(XilinxPlatform):
    device = "xcku040"
    package = "ffva1156"
    speed = "2"
    resources = []
    connectors = []

    def get_int_mac(self, mac):
        return self.get_dsp48e2_int_mac(mac)
