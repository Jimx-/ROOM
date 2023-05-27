from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.stream import Decoupled


class _AXIStreamLayout(Record):

    def __init__(self,
                 data_width,
                 keep_width,
                 id_width,
                 dest_width,
                 user_width,
                 layout=None,
                 name=None,
                 src_loc_at=0):
        if layout is not None:
            payload_layout = layout
        else:
            payload_layout = [
                ('data', max(1, data_width), Direction.FANOUT),
                ('keep', max(1, keep_width), Direction.FANOUT),
            ]

        payload_layout += [
            ('id', max(1, id_width), Direction.FANOUT),
            ('dest', max(1, dest_width), Direction.FANOUT),
            ('user', max(1, user_width), Direction.FANOUT),
            ('last', 1, Direction.FANOUT),
        ]

        super().__init__(payload_layout, name=name, src_loc_at=1 + src_loc_at)


class AXIStreamInterface(Decoupled):

    def __init__(self,
                 data_width=0,
                 keep_width=None,
                 id_width=0,
                 dest_width=0,
                 user_width=0,
                 layout=None,
                 name=None):
        self.data_width = data_width
        self.keep_width = data_width // 8 if keep_width is None else keep_width
        self.id_width = id_width
        self.dest_width = dest_width
        self.user_width = user_width

        super().__init__(_AXIStreamLayout,
                         self.data_width,
                         self.keep_width,
                         self.id_width,
                         self.dest_width,
                         self.user_width,
                         layout,
                         name=name,
                         src_loc_at=1)
