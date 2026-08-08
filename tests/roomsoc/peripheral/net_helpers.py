ETHERTYPE_IPV4 = 0x0800
ETHERNET_HEADER_SIZE = 14


def is_ipv4_frame(frame):
    frame = bytes(frame)
    return (len(frame) >= ETHERNET_HEADER_SIZE
            and int.from_bytes(frame[12:14], "big") == ETHERTYPE_IPV4)


def assert_valid_ipv4_checksum(frame):
    """Assert that an Ethernet frame has a valid IPv4 header checksum."""
    frame = bytes(frame)
    assert is_ipv4_frame(frame), "frame does not contain an IPv4 packet"

    ip_offset = ETHERNET_HEADER_SIZE
    assert frame[ip_offset] >> 4 == 4, "EtherType is IPv4 but version is not 4"
    header_size = (frame[ip_offset] & 0x0f) * 4
    assert header_size >= 20, "invalid IPv4 header length"
    assert len(frame) >= ip_offset + header_size, "truncated IPv4 header"

    header = frame[ip_offset:ip_offset + header_size]
    total = sum(int.from_bytes(header[offset:offset + 2], "big")
                for offset in range(0, header_size, 2))
    while total >> 16:
        total = (total & 0xffff) + (total >> 16)

    stored = int.from_bytes(header[10:12], "big")
    assert total == 0xffff, (
        f"invalid IPv4 header checksum 0x{stored:04x} "
        f"(one's-complement sum 0x{total:04x})")
