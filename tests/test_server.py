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

    def test_formatting_provider_initialization(self):
        """Test that formatting provider is initialized."""
        test_server = VASPLanguageServer()
        assert test_server.formatting_provider is not None


class TestServerFormatting:
    """Test server formatting feature."""
    
    def test_formatting_with_content(self):
        """Test formatting when content exists."""
        from lsprotocol.types import DocumentFormattingParams, FormattingOptions
        from vasp_lsp.server import server, formatting
        
        # Set up document
        uri = "file:///test/INCAR"
        content = "ENCUT=500\nISMEAR=0"
        server.set_document_content(uri, content)
        
        # Create params
        params = DocumentFormattingParams(
            text_document=Mock(uri=uri),
            options=FormattingOptions(tab_size=4, insert_spaces=True)
        )
        
        # Call formatting
        result = formatting(params)
        
        assert result is not None
        assert len(result) == 1
    
    def test_formatting_without_content(self):
        """Test formatting when content doesn't exist."""
        from lsprotocol.types import DocumentFormattingParams, FormattingOptions
        from unittest.mock import Mock
        from vasp_lsp.server import server, formatting
        
        # Clear document
        uri = "file:///test/unknown"
        if uri in server.documents:
            del server.documents[uri]
        
        # Create params
        params = DocumentFormattingParams(
            text_document=Mock(uri=uri),
            options=FormattingOptions(tab_size=4, insert_spaces=True)
        )
        
        # Call formatting
        result = formatting(params)
        
        assert result is None
