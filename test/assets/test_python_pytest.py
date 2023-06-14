import pytest


def test_root_level():
    for i in range(5):
        assert i < 5
    assert True


class TestWithNamespace:

    def test_inside_namespace(self):
        for i in range(5):
            assert i < 5
        assert True
