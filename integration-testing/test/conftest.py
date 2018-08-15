def pytest_addoption(parser):
    parser.addoption(
        "--peer-count", action="store", default="1", help="number of peers in the network (excluding bootstrap node)"
    )