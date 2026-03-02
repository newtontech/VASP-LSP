"""
Tests for VASP Language Server.
"""

import pytest
from unittest.mock import Mock, patch

from vasp_lsp.server import VASPLanguageServer, server


class TestVASPLanguageServer:
    """Test cases for VASPLanguageServer."""

    def test_server_creation(self):
        """Test that server can be created."""
        assert server is not None
        assert server.name == "vasp-lsp"

    def test_document_content_management(self):
        """Test document content caching."""
        test_server = VASPLanguageServer()
        uri = "file:///test/INCAR"
        content = "ENCUT = 520"
        
        # Set content
        test_server.set_document_content(uri, content)
        
        # Get content
        retrieved = test_server.get_document_content(uri)
        assert retrieved == content

    def test_get_nonexistent_document(self):
        """Test getting a document that doesn't exist."""
        test_server = VASPLanguageServer()
        content = test_server.get_document_content("file:///nonexistent")
        assert content is None

    def test_providers_initialization(self):
        """Test that providers are initialized."""
        test_server = VASPLanguageServer()
        
        assert test_server.completion_provider is not None
        assert test_server.hover_provider is not None
        assert test_server.diagnostics_provider is not None
