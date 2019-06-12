import pytest

@pytest.mark.skip(reason="Skipped because we don't support bonding/unbonding in the new PoS yet")
# see previous implementation for reference
def test_heterogenous_validators() -> None:
    pass
