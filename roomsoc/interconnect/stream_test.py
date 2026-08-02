from amaranth import Record, Signal
from amaranth.sim import Simulator

from roomsoc.interconnect.axi.axi_stream import AXIStreamInterface
from roomsoc.interconnect.stream import Decoupled, Queue, SkidBuffer


def test_decoupled_clone_preserves_payload():
    interface = Decoupled(Record, [('data', 32), ('last', 1)])
    clone = interface.clone(name='clone')

    assert type(clone) is Decoupled
    assert clone is not interface
    assert clone.bits is not interface.bits
    assert len(clone.bits) == len(interface.bits)
    assert set(clone.bits.fields) == {'data', 'last'}


def test_queue_clones_axi_stream_interface():
    layout = [('data', 32), ('keep', 4), ('tag', 3)]
    interface = AXIStreamInterface(data_width=32,
                                   id_width=2,
                                   dest_width=3,
                                   user_width=4,
                                   layout=layout)
    queue = Queue(2, interface)

    assert isinstance(queue.enq, AXIStreamInterface)
    assert isinstance(queue.deq, AXIStreamInterface)
    assert queue.enq is not interface
    assert queue.deq is not interface
    assert queue.enq.data_width == 32
    assert queue.enq.id_width == 2
    assert queue.enq.dest_width == 3
    assert queue.enq.user_width == 4
    assert set(queue.enq.bits.fields) == {
        'data', 'keep', 'tag', 'id', 'dest', 'user', 'last'
    }
    assert not hasattr(queue.enq.bits, 'bits')
    assert layout == [('data', 32), ('keep', 4), ('tag', 3)]

    Simulator(queue)


def test_skid_buffer_clones_axi_stream_interface():
    interface = AXIStreamInterface(data_width=64, user_width=8)
    skid = SkidBuffer(interface)

    assert isinstance(skid.enq, AXIStreamInterface)
    assert isinstance(skid.deq, AXIStreamInterface)
    assert not hasattr(skid.enq.bits, 'bits')

    Simulator(skid)


def test_buffers_construct_decoupled_interface_type_directly():
    queue = Queue(2,
                  AXIStreamInterface,
                  data_width=32,
                  id_width=2,
                  user_width=4)
    skid = SkidBuffer(AXIStreamInterface,
                      data_width=64,
                      dest_width=3,
                      user_width=8)

    assert isinstance(queue.enq, AXIStreamInterface)
    assert isinstance(queue.deq, AXIStreamInterface)
    assert queue.enq.data_width == 32
    assert queue.enq.id_width == 2
    assert queue.enq.user_width == 4
    assert not hasattr(queue.enq.bits, 'bits')

    assert isinstance(skid.enq, AXIStreamInterface)
    assert isinstance(skid.deq, AXIStreamInterface)
    assert skid.enq.data_width == 64
    assert skid.enq.dest_width == 3
    assert skid.enq.user_width == 8
    assert not hasattr(skid.enq.bits, 'bits')

    Simulator(queue)
    Simulator(skid)


def test_payload_constructor_remains_supported():
    queue = Queue(2, Signal, 8, name='queue_payload')
    skid = SkidBuffer(Record, [('data', 8)])

    assert type(queue.enq) is Decoupled
    assert len(queue.enq.bits) == 8
    assert type(skid.enq) is Decoupled
    assert len(skid.enq.bits) == 8

    Simulator(queue)
    Simulator(skid)
