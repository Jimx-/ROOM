from enum import IntEnum

from amaranth import *
from amaranth.hdl.ast import Value
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import (AXIStreamDepacketizer,
                                      AXIStreamInterface, AXIStreamPacketizer)
from roomsoc.interconnect.stream import Decoupled, Queue, SkidBuffer

from .ip import IpProtocol
from .ipv4 import Ipv4Depacketizer, Ipv4HeaderExtract, Ipv4Metadata, Ipv4Packetizer

__all__ = ("IcmpServer", "IcmpType", "ICMP_HEADER_LAYOUT")

ICMP_HEADER_LAYOUT = [
    ("type", 8, Direction.FANOUT),
    ("code", 8, Direction.FANOUT),
    ("checksum", 16, Direction.FANOUT),
    ("rest", 32, Direction.FANOUT),
]


class IcmpType(IntEnum):
    ECHO_REPLY = 0x00
    DESTINATION_UNREACHABLE = 0x03
    ECHO_REQUEST = 0x08


def _ones_fold(value):
    """Collapse an exact binary sum into its 16-bit one's-complement sum.

    Amaranth additions grow by one bit, so every intermediate value is
    exact; the iterations perform the end-around carries and converge for
    any input below 2**24.
    """
    value = Value.cast(value).as_unsigned()
    for _ in range(5):
        value = value[:16] + value[16:]
    return value[:16]


def _trim_keep(keep, remain, beat_bytes):
    """Mask off byte lanes beyond the remaining payload length."""
    lane_bits = (beat_bytes - 1).bit_length()
    full_beat = remain[lane_bits:].any()
    return keep & Cat(full_beat | (remain[:lane_bits] > i)
                      for i in range(beat_bytes))


def _payload_checksum(m, data, keep, fire, first, seed, prefix):
    """Accumulate one word per lane per cycle and reduce the lane sums."""
    sums = []
    valid = Signal(name=f"{prefix}_word_valid")
    start = Signal(name=f"{prefix}_word_first")
    m.d.sync += valid.eq(fire)
    with m.If(fire):
        m.d.sync += start.eq(first)

    for i in range(len(data) // 16):
        word = Signal(16, name=f"{prefix}_word{i}")
        subtotal = Signal(16, name=f"{prefix}_sum{i}")
        lo = data[16 * i:16 * i + 8]
        hi = data[16 * i + 8:16 * i + 16]
        with m.If(fire):
            m.d.sync += word.eq(
                Cat(Mux(keep[2 * i + 1], hi, 0), Mux(keep[2 * i], lo, 0)))
        acc = Signal(17, name=f"{prefix}_acc{i}")
        m.d.comb += acc.eq(Mux(start, 0, subtotal) + word)
        with m.If(valid):
            m.d.sync += subtotal.eq(acc[:16] + acc[16])
        sums.append(subtotal)

    # These stages see the final beat's sums one cycle after it accumulates.
    pairs = []
    for i in range(0, len(sums), 2):
        pair = Signal(17, name=f"{prefix}_pair{i // 2}")
        m.d.sync += pair.eq(sums[i] + sums[i + 1])
        pairs.append(pair)
    total = Signal(20, name=f"{prefix}_total")
    m.d.sync += total.eq(seed + sum(pairs))
    folded = Signal(16, name=f"{prefix}_folded")
    m.d.sync += folded.eq(_ones_fold(total))
    return folded


class IcmpServer(Elaboratable):
    """ICMP responder answering echo requests and closed UDP ports.

    ``rx_data_in`` receives protocol-ICMP packets (full IP packet, as
    dispatched by :class:`Ipv4Handler`). Echo requests (type 8, code 0)
    with a valid ICMP checksum are answered by an echo reply mirroring
    the request payload; its checksum is derived incrementally from the
    request checksum (the type byte 0x08 is the high half of the first
    header word, so 0x0800 is added in one's-complement arithmetic).

    ``udp_rx_data_in`` mirrors every protocol-UDP packet (full IP packet,
    e.g. from ``Ipv4Handler(..., udp_icmp_tap=True)``). A datagram whose
    destination port is not in ``open_udp_ports`` is answered with ICMP
    destination-unreachable code 3 (port unreachable) embedding the
    original datagram truncated to ``unreachable_cap`` bytes, keeping the
    reply within the 576-byte advisory limit. The IPv4 layer upstream
    already validated the IP header checksum and our address; no reply is
    generated for broadcast datagrams, non-first fragments or datagrams
    carrying IP options.

    The server never back-pressures its ingress streams: packets are
    drained at line rate while payload bytes are written into a
    single-frame store (memory writes cannot stall) and the reply is
    emitted once the store holds a complete committed frame. A packet
    arriving while a previous reply is still being read from the store is
    silently dropped; a ping host's own retry covers that. Echo traffic
    beyond the store capacity (a full-MTU echo payload by default) is
    likewise dropped without a reply.

    The two ingress streams share the store through a packet-grant
    register: a granted ingress owns the store until its packet ends
    (released on no-commit) or until the reply egress completes
    (commit). The echo path only claims actual echo requests, and the UDP
    path releases its claim as soon as the destination port is known to
    be open, so open-port traffic (for example RoCE) never occupies the
    store.
    """

    def __init__(self,
                 data_width,
                 open_udp_ports=(),
                 payload_depth=None,
                 unreachable_cap=528):
        # The 8-byte ICMP header must stay beat-aligned in the ingress
        # depacketizer chain (guaranteed for data_width <= 64), the
        # checksum accumulators need at least one 16-bit lane, and the
        # UDP tap captures the total-length and destination-port bytes
        # from a single beat (data_width >= 32).
        if data_width not in (32, 64):
            raise ValueError("IcmpServer data_width must be 32 or 64")

        self.data_width = data_width
        self.open_udp_ports = tuple(int(port) for port in open_udp_ports)
        self.unreachable_cap = unreachable_cap

        beat_bytes = data_width // 8
        if payload_depth is None:
            # A full-MTU echo payload (1500 - 20 - 8 bytes).
            need = max(16, (1472 + beat_bytes - 1) // beat_bytes)
            payload_depth = 1 << (need - 1).bit_length()
        if payload_depth & (payload_depth - 1):
            raise ValueError("payload_depth must be a power of two")
        if not 0 < unreachable_cap <= payload_depth * beat_bytes:
            raise ValueError(
                "unreachable_cap must be positive and fit in the store")
        self.payload_depth = payload_depth

        self.my_ip_addr = Signal(32)

        # Full IP packets with protocol ICMP, from the IPv4 dispatcher.
        self.rx_data_in = AXIStreamInterface(data_width=data_width)
        # Mirror of every UDP packet (full IP packet) for port
        # unreachable generation.
        self.udp_rx_data_in = AXIStreamInterface(data_width=data_width)

        # Full IP packet (protocol ICMP); user[-32:] carries the
        # destination address for the MAC/ARP encoder, matching
        # UdpStack.tx_data_out.
        self.tx_data_out = AXIStreamInterface(data_width=data_width,
                                              user_width=128)

    def elaborate(self, platform):
        m = Module()
        beat_bytes = self.data_width // 8
        store_bytes = self.payload_depth * beat_bytes

        store = Memory(width=self.data_width + beat_bytes,
                       depth=self.payload_depth)
        store_wport = m.submodules.store_wport = store.write_port()
        store_rport = m.submodules.store_rport = store.read_port(domain='comb')

        # Store ownership: 0 = free, 1 = echo ingress, 2 = UDP ingress.
        # A committed frame holds its grant until the reply egress
        # completes, which is what limits the server to one reply in
        # flight. Claims require grant == 0 in the current cycle, so a
        # claim can never coincide with a release of the same cycle.
        grant = Signal(2, reset=0)
        pending = Signal()
        pending_unreachable = Signal()
        pending_peer = Signal(32)
        pending_len = Signal(16)
        pending_beats = Signal(range(self.payload_depth + 1))
        pending_rest = Signal(32)
        pending_csum = Signal(16)

        # ------------------------------------------------------------------
        # Echo ingress: strip the IPv4 and ICMP headers, verify the ICMP
        # checksum over the whole message, store the (trimmed) payload.
        # ------------------------------------------------------------------
        ipv4_dep = m.submodules.ipv4_dep = Ipv4Depacketizer(
            data_width=self.data_width)
        icmp_dep = m.submodules.icmp_dep = AXIStreamDepacketizer(
            Record, ICMP_HEADER_LAYOUT, data_width=self.data_width)

        ip_meta_queue = m.submodules.ip_meta_queue = Queue(2, Ipv4Metadata)
        m.d.comb += [
            self.rx_data_in.connect(ipv4_dep.data_in),
            ipv4_dep.data_out.connect(icmp_dep.sink),
            ipv4_dep.meta_out.connect(ip_meta_queue.enq),
            # Drain at line rate: the store write is fire-and-forget.
            icmp_dep.source.ready.eq(1),
            ip_meta_queue.deq.ready.eq(1),
        ]

        echo_peer = Signal(32)
        echo_payload_len = Signal(16)
        with m.If(ip_meta_queue.deq.fire):
            m.d.sync += [
                echo_peer.eq(ip_meta_queue.deq.bits.peer_addr),
                # total_len - IPv4 header - ICMP header.
                echo_payload_len.eq(ip_meta_queue.deq.bits.length - 28),
            ]

        echo_seed = Signal(20)
        echo_rest = Signal(32)
        echo_csum = Signal(16)
        echo_beat = Signal(16)
        echo_count = Signal(range(self.payload_depth + 1))
        echo_bytes = Signal(range(store_bytes + 1))
        echo_owned = Signal()

        echo_fire = icmp_dep.source.valid
        echo_first = echo_fire & (echo_beat == 0)
        echo_last = echo_fire & icmp_dep.source.bits.last
        echo_request = (icmp_dep.header.type
                        == IcmpType.ECHO_REQUEST) & (icmp_dep.header.code == 0)
        echo_claim = echo_first & echo_request & (grant == 0)
        echo_owner = echo_owned | echo_claim
        echo_granted = (grant == 1) | echo_claim
        echo_overflow = echo_payload_len > store_bytes

        # Natural-order header words (record fields hold byte-swapped
        # wire data; single-byte fields do not need swapping).
        csum_field = Signal(16)
        m.d.comb += csum_field.eq(
            Cat(icmp_dep.header.checksum[8:], icmp_dep.header.checksum[:8]))
        rest = icmp_dep.header.rest
        seed_words = Signal(20)
        m.d.comb += seed_words.eq(
            Cat(icmp_dep.header.code, icmp_dep.header.type) + csum_field +
            Cat(rest[8:16], rest[:8]) + Cat(rest[24:32], rest[16:24]))

        echo_remain = Signal(range(store_bytes + 1))
        m.d.comb += echo_remain.eq(echo_payload_len - echo_bytes)
        echo_keep_trim = Signal(beat_bytes)
        m.d.comb += echo_keep_trim.eq(
            _trim_keep(icmp_dep.source.bits.keep, echo_remain, beat_bytes))
        echo_store = Signal()
        m.d.comb += echo_store.eq(echo_fire & echo_owner & echo_granted
                                  & ~echo_overflow & echo_keep_trim.any())

        echo_popcount = Signal(range(beat_bytes + 1))
        m.d.comb += echo_popcount.eq(
            sum(echo_keep_trim[i] for i in range(beat_bytes)))
        with m.If(echo_store):
            m.d.sync += [
                echo_count.eq(echo_count + 1),
                echo_bytes.eq(echo_bytes + echo_popcount),
            ]

        # The claim must not outlive a single-beat packet: the clear at
        # the packet's last beat below takes priority.
        with m.If(echo_claim & ~echo_last):
            m.d.sync += echo_owned.eq(1)

        with m.If(echo_first):
            m.d.sync += [
                echo_seed.eq(seed_words),
                echo_rest.eq(icmp_dep.header.rest),
                echo_csum.eq(csum_field),
            ]

        # Values as of this cycle (the registers above only settle at the
        # end of the cycle), so single-beat packets commit correctly.
        echo_seed_now = Mux(echo_first, seed_words, echo_seed)
        echo_rest_now = Mux(echo_first, icmp_dep.header.rest, echo_rest)
        csum_field_now = Mux(echo_first, csum_field, echo_csum)
        echo_beats_now = echo_count + Mux(echo_store, 1, 0)

        csum_reply = csum_field_now + 0x0800
        csum_reply = (csum_reply + (csum_reply >> 16)) & 0xffff
        echo_candidate = Signal()
        m.d.comb += echo_candidate.eq(echo_last & echo_owner & echo_granted
                                      & ~echo_overflow
                                      & (echo_popcount == echo_remain))
        echo_final_seed = Signal(20)
        echo_checksum = _payload_checksum(m, icmp_dep.source.bits.data,
                                          echo_keep_trim, echo_fire,
                                          echo_first, echo_final_seed, "echo")
        with m.If(echo_candidate):
            m.d.sync += [
                echo_final_seed.eq(echo_seed_now),
                pending_unreachable.eq(0),
                pending_peer.eq(echo_peer),
                pending_len.eq(echo_payload_len + 8),
                pending_beats.eq(echo_beats_now),
                pending_rest.eq(echo_rest_now),
                pending_csum.eq(csum_reply),
            ]

        with m.If(echo_fire):
            with m.If(echo_last):
                m.d.sync += [
                    echo_beat.eq(0),
                    echo_count.eq(0),
                    echo_bytes.eq(0),
                    echo_owned.eq(0),
                ]
            with m.Else():
                m.d.sync += echo_beat.eq(echo_beat + 1)

        # ------------------------------------------------------------------
        # UDP-unreachable ingress: tap of every UDP packet. Capture the
        # IPv4 header fields while passing through, capture the UDP
        # destination port on its beat, and store the original packet
        # (truncated to unreachable_cap) if the port is closed.
        #
        # The store trim needs the IPv4 total length from beat 0, but the
        # header extract only reports it once bytes 0-19 have passed.
        # Capture the total-length bytes positionally from the live
        # stream and run the per-beat logic one beat behind, so the
        # length register is stable before the first beat is stored.
        # ------------------------------------------------------------------
        udp_src = Signal(32)
        udp_dst = Signal(32)
        udp_ihl = Signal(4)
        udp_frag_offset = Signal(13)

        def handle_udp_header(m, header):
            m.d.comb += header.ready.eq(1)
            with m.If(header.valid):
                m.d.sync += [
                    udp_src.eq(header.bits.src_addr),
                    udp_dst.eq(header.bits.dst_addr),
                    udp_ihl.eq(header.bits.ihl),
                    udp_frag_offset.eq(
                        Cat(header.bits.frag_offset_l,
                            header.bits.frag_offset_h)),
                ]

        udp_extract = m.submodules.udp_extract = Ipv4HeaderExtract(
            data_width=self.data_width, handler=handle_udp_header)

        m.d.comb += self.udp_rx_data_in.connect(udp_extract.data_in)

        udp_live = udp_extract.data_out
        m.d.comb += udp_live.ready.eq(1)
        live_fire = udp_live.valid

        udp_live_beat = Signal(16)
        with m.If(live_fire):
            with m.If(udp_live.bits.last):
                m.d.sync += udp_live_beat.eq(0)
            with m.Else():
                m.d.sync += udp_live_beat.eq(udp_live_beat + 1)

        total_len_bytes = [
            Signal(8, name=f"total_len_b{index}") for index in (2, 3)
        ]
        for index, byte_reg in zip((2, 3), total_len_bytes):
            lane = (index % beat_bytes) * 8
            with m.If(live_fire & (udp_live_beat == index // beat_bytes)):
                m.d.sync += byte_reg.eq(udp_live.bits.data[lane:lane + 8])
        udp_total_len = Signal(16)
        m.d.comb += udp_total_len.eq(
            Cat(total_len_bytes[1], total_len_bytes[0]))

        # One registered stage: the per-beat logic runs exactly one beat
        # behind the live stream, so the captured total length is stable
        # from the first stored beat while the commit still reads this
        # packet's registers (the next packet can only overwrite them one
        # cycle after this packet's last beat).
        delay_stages = 3 // beat_bytes + 1
        udp_stream = udp_live
        for stage in range(delay_stages):
            skid = m.submodules[f"udp_delay_{stage}"] = SkidBuffer(
                AXIStreamInterface, data_width=self.data_width)
            m.d.comb += [
                udp_stream.connect(skid.enq),
                skid.deq.ready.eq(1),
            ]
            udp_stream = skid.deq

        udp_fire = udp_stream.valid
        udp_beat = Signal(16)
        udp_first = udp_fire & (udp_beat == 0)
        udp_last = udp_fire & udp_stream.bits.last
        # Match the shared store's echo-first arbitration on simultaneous
        # arrivals so the losing UDP packet never acquires ownership.
        udp_claim = udp_first & (grant == 0) & ~echo_claim
        udp_owned = Signal()
        udp_owner = udp_owned | udp_claim
        udp_granted = (grant == 2) | udp_claim
        # Set once the destination port is known to be open; the rest of
        # the packet is drained without touching the store.
        udp_skip = Signal()

        udp_dport = Signal(16)
        dport_beat = 22 // beat_bytes
        dport_lane = 22 % beat_bytes
        dport_now = Signal(16)
        m.d.comb += dport_now.eq(
            Cat(
                udp_stream.bits.data[(dport_lane + 1) * 8:(dport_lane + 2) *
                                     8],
                udp_stream.bits.data[dport_lane * 8:(dport_lane + 1) * 8]))
        dport_now_open = Signal()
        m.d.comb += dport_now_open.eq(
            Cat(dport_now == port for port in self.open_udp_ports).any(
            ) if self.open_udp_ports else Const(0))

        udp_release = Signal()
        m.d.comb += udp_release.eq(udp_fire & (udp_beat == dport_beat)
                                   & udp_owned & dport_now_open)
        with m.If(udp_fire & (udp_beat == dport_beat)):
            m.d.sync += udp_dport.eq(dport_now)

        with m.If(udp_release):
            m.d.sync += [
                udp_owned.eq(0),
                udp_skip.eq(1),
            ]

        udp_dport_closed = Signal()
        m.d.comb += udp_dport_closed.eq(
            Cat(udp_dport != port for port in self.open_udp_ports).all(
            ) if self.open_udp_ports else Const(1))

        udp_count = Signal(range(self.payload_depth + 1))
        udp_bytes = Signal(range(self.unreachable_cap + 1))
        udp_expected = Signal(range(self.unreachable_cap + 1))
        m.d.comb += udp_expected.eq(
            Mux(udp_total_len > self.unreachable_cap, self.unreachable_cap,
                udp_total_len))
        udp_remain = Signal.like(udp_expected)
        m.d.comb += udp_remain.eq(udp_expected - udp_bytes)
        udp_keep_trim = Signal(beat_bytes)
        m.d.comb += udp_keep_trim.eq(
            _trim_keep(udp_stream.bits.keep, udp_remain, beat_bytes))
        udp_store = Signal()
        m.d.comb += udp_store.eq(udp_fire & udp_owner & udp_granted
                                 & ~udp_skip & udp_keep_trim.any())

        udp_popcount = Signal(range(beat_bytes + 1))
        m.d.comb += udp_popcount.eq(
            sum(udp_keep_trim[i] for i in range(beat_bytes)))
        with m.If(udp_store):
            m.d.sync += [
                udp_count.eq(udp_count + 1),
                udp_bytes.eq(udp_bytes + udp_popcount),
            ]

        with m.If(udp_claim & ~udp_last):
            m.d.sync += udp_owned.eq(1)

        udp_beats_now = udp_count + Mux(udp_store, 1, 0)

        udp_checksum = _payload_checksum(m, udp_stream.bits.data,
                                         udp_keep_trim, udp_fire, udp_first,
                                         Const(0x0303), "udp")
        udp_candidate = Signal()
        m.d.comb += udp_candidate.eq(udp_last & udp_owner & udp_granted
                                     & ~udp_skip & ~udp_release
                                     & udp_dport_closed & (udp_ihl == 5)
                                     & (udp_frag_offset == 0)
                                     & (udp_dst != 0xffffffff)
                                     & (udp_total_len >= 28)
                                     & (udp_popcount == udp_remain))
        with m.If(udp_candidate):
            m.d.sync += [
                pending_unreachable.eq(1),
                pending_peer.eq(udp_src),
                pending_len.eq(udp_expected + 8),
                pending_beats.eq(udp_beats_now),
                pending_rest.eq(0),
            ]

        # Keep the grant while the final word accumulates and the lane sums
        # pass through the pair, total and fold stages.
        echo_finish = Signal(5)
        udp_finish = Signal(5)
        m.d.sync += [
            echo_finish.eq(Cat(echo_candidate, echo_finish[:-1])),
            udp_finish.eq(Cat(udp_candidate, udp_finish[:-1])),
        ]
        echo_checksum_ok = echo_checksum == 0xffff
        with m.If(echo_finish[-1] & echo_checksum_ok):
            m.d.sync += pending.eq(1)
        with m.If(udp_finish[-1]):
            m.d.sync += [pending.eq(1), pending_csum.eq(~udp_checksum)]

        with m.If(udp_fire):
            with m.If(udp_last):
                m.d.sync += [
                    udp_beat.eq(0),
                    udp_count.eq(0),
                    udp_bytes.eq(0),
                    udp_owned.eq(0),
                    udp_skip.eq(0),
                ]
            with m.Else():
                m.d.sync += udp_beat.eq(udp_beat + 1)

        # ------------------------------------------------------------------
        # Shared store write port. The two ingresses are mutually
        # exclusive through the grant register.
        # ------------------------------------------------------------------
        m.d.comb += [
            store_wport.en.eq(echo_store | udp_store),
            store_wport.addr.eq(Mux(echo_store, echo_count, udp_count)),
            store_wport.data.eq(
                Cat(
                    Mux(echo_store, icmp_dep.source.bits.data,
                        udp_stream.bits.data),
                    Mux(echo_store, echo_keep_trim, udp_keep_trim))),
        ]

        # ------------------------------------------------------------------
        # Reply egress: regenerate the ICMP header, stream the stored
        # payload behind a fresh IPv4 header (new checksum) and hand the
        # destination address to the MAC/ARP encoder.
        # ------------------------------------------------------------------
        icmp_tx = m.submodules.icmp_tx = AXIStreamPacketizer(
            Record, ICMP_HEADER_LAYOUT, data_width=self.data_width)
        ipv4_tx = m.submodules.ipv4_tx = Ipv4Packetizer(
            data_width=self.data_width)

        tx_meta = Decoupled(Ipv4Metadata)
        m.d.comb += [
            ipv4_tx.my_ip_addr.eq(self.my_ip_addr),
            ipv4_tx.protocol.eq(IpProtocol.ICMP),
            icmp_tx.source.connect(ipv4_tx.data_in),
            tx_meta.connect(ipv4_tx.meta_in),
            ipv4_tx.data_out.connect(self.tx_data_out),
            icmp_tx.header.type.eq(
                Mux(pending_unreachable, IcmpType.DESTINATION_UNREACHABLE,
                    IcmpType.ECHO_REPLY)),
            icmp_tx.header.code.eq(Mux(pending_unreachable, 0x03, 0)),
            icmp_tx.header.checksum.eq(Cat(pending_csum[8:],
                                           pending_csum[:8])),
            icmp_tx.header.rest.eq(pending_rest),
        ]

        egress_count = Signal(range(self.payload_depth))
        egress_last = Signal()
        egress_done = Signal()
        with m.FSM() as egress:
            with m.State("IDLE"):
                m.d.comb += [
                    tx_meta.valid.eq(pending),
                    tx_meta.bits.peer_addr.eq(pending_peer),
                    tx_meta.bits.length.eq(pending_len),
                ]

                with m.If(tx_meta.fire):
                    m.d.sync += egress_count.eq(0)
                    m.next = "READ"

            with m.State("READ"):
                # A bare echo header (no payload) emits a single empty
                # beat so the packetizer still starts and frames it.
                m.d.comb += egress_last.eq((egress_count + 1) >= pending_beats)
                m.d.comb += egress_done.eq(icmp_tx.sink.fire & egress_last)
                m.d.comb += [
                    store_rport.addr.eq(egress_count),
                    icmp_tx.sink.valid.eq(1),
                    icmp_tx.sink.bits.data.eq(
                        store_rport.data[:self.data_width]),
                    icmp_tx.sink.bits.keep.eq(
                        Mux(
                            pending_beats == 0, 0,
                            Mux(egress_count + 1 == pending_beats,
                                store_rport.data[self.data_width:],
                                Const(1).replicate(beat_bytes)))),
                    icmp_tx.sink.bits.last.eq(egress_last),
                ]

                with m.If(icmp_tx.sink.fire):
                    with m.If(egress_last):
                        m.d.sync += pending.eq(0)
                        m.next = "IDLE"
                    with m.Else():
                        m.d.sync += egress_count.eq(egress_count + 1)

        # Single driver for the grant register: egress completion and
        # uncommitted owners release, otherwise a claim takes over.
        grant_release = egress_done | udp_release \
            | (echo_finish[-1] & ~echo_checksum_ok) \
            | (echo_last & echo_owner & ~echo_candidate) \
            | (udp_last & udp_owner & ~udp_candidate & ~udp_release)
        with m.If(grant_release):
            m.d.sync += grant.eq(0)
        with m.Elif(echo_claim):
            m.d.sync += grant.eq(1)
        with m.Elif(udp_claim):
            m.d.sync += grant.eq(2)

        return m
