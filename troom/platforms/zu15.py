"""TROOM dual-DDR build profile for ZU15."""

from dataclasses import dataclass

from troom.stream import ReadPort, ReadRegion, ReadTopology
from troom.types import HasTRoomParams


@dataclass(frozen=True)
class AddressMap:
    mmio_base: int
    mmio_size: int
    mem_base: int
    mem_size: int


ZU15_ADDRESS_MAP = AddressMap(mmio_base=0xff000000,
                              mmio_size=0x01000000,
                              mem_base=0x80000000,
                              mem_size=0x7f000000)
# Identity-mapped low PS DDR, excluding page zero and the PMU reservation.
ZU15_SECONDARY_WINDOW = (0x00001000, 0x7ff00000)


def zu15_read_topology(params, *, fast_hp=False, lean=False):
    if lean and not fast_hp:
        raise ValueError("lean ZU15 topology requires fast HP ports")
    address_map = ZU15_ADDRESS_MAP
    primary_end = address_map.mem_base + address_map.mem_size
    hp = HasTRoomParams(params)
    # Two 64-byte returns supply up to four Q8_0 payload windows per cycle.
    wide_reader = hp.n_outputs > 4
    return ReadTopology(
        ports=(
            *(ReadPort(f'hp{i}',
                       issue_queue_depth=2,
                       return_buffer_beats=32 if fast_hp and not lean else 16,
                       axi_data_width=256 if fast_hp else None)
              for i in range(4)),
            ReadPort('pl',
                     issue_queue_depth=0,
                     return_buffer_beats=hp.sg_pay_fifo),
        ),
        regions=(
            ReadRegion(address_map.mem_base, primary_end, (4, )),
            ReadRegion(*ZU15_SECONDARY_WINDOW, (0, 1, 2, 3),
                       stripe_bytes=1024),
        ),
        primary_region=0,
        scale_region=0,
        shared_port=4,
        secondary_region=1,
        retirement_lanes=3 if fast_hp and (not lean or wide_reader) else 2,
        dual_bank_shared_port=fast_hp and lean and wide_reader,
    )
