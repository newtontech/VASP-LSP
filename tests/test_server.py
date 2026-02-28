"""Test LSP server functionality."""

import pytest
from vasp_lsp.server import VASPLanguageServer


@pytest.fixture
def server():
    """Create a test server instance."""
    return VASPLanguageServer()


def test_server_creation(server):
    """Test server can be created."""
    assert server is not None
    assert server.name == "vasp-lsp"
