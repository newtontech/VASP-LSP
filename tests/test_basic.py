"""
Basic tests for vasp-lsp
"""

import pytest


def test_import():
    """Test that the package can be imported"""
    from vasp_lsp import __version__
    assert __version__ == "0.1.0"


def test_placeholder():
    """Placeholder test - replace with actual tests"""
    assert True
