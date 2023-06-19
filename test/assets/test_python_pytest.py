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

    def test_nested_function(self):
        def foo():
            return 5

        assert foo() == 5

    @pytest.mark.django_db
    def test_decorated(self):
        assert 1 == 1.0
