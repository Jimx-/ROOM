import sys
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))


def pytest_addoption(parser):
    rdma = parser.getgroup("rdma")
    rdma.addoption(
        "--rdma-packet-capture",
        metavar="PATH",
        default=None,
        help=("write RDMA ingress and egress packets as a direction-marked "
              "text2pcap ASCII capture"),
    )
