"""Tests for VASP LSP server."""
import pytest
from unittest.mock import MagicMock, patch
from lsprotocol.types import (
    CompletionParams,
    HoverParams,
    DidOpenTextDocumentParams,
    DidChangeTextDocumentParams,
    DidSaveTextDocumentParams,
    TextDocumentItem,
    TextDocumentIdentifier,
    VersionedTextDocumentIdentifier,
    Position,
)

from vasp_lsp.server import (
    VASPLanguageServer,
    server,
    text_document_did_open,
    text_document_did_change,
    text_document_did_save,
    completions,
    hover,
    main,
)


class TestVASPLanguageServer:
    """Test VASP Language Server."""

    def test_server_initialization(self):
        """Test server initialization."""
        test_server = VASPLanguageServer()
        assert test_server.name == "vasp-lsp"
        assert test_server.version == "0.1.0"
        assert test_server.documents == {}

    def test_get_document_content(self):
        """Test getting document content."""
        test_server = VASPLanguageServer()
        test_server.documents["file:///test.vasp"] = "test content"
        assert test_server.get_document_content("file:///test.vasp") == "test content"
        assert test_server.get_document_content("file:///nonexistent") is None

    def test_set_document_content(self):
        """Test setting document content."""
        test_server = VASPLanguageServer()
        test_server.set_document_content("file:///test.vasp", "content")
        assert test_server.documents["file:///test.vasp"] == "content"
        
    def test_initialize(self):
        """Test server initialize method."""
        test_server = VASPLanguageServer()
        params = MagicMock()
        params.client_info = MagicMock()
        params.client_info.name = "test-client"
        result = test_server._initialize(params)
        assert result.capabilities is not None
        assert result.capabilities.hover_provider is True

    def test_initialize_without_client_info(self):
        """Test initialization without client info."""
        test_server = VASPLanguageServer()
        params = MagicMock()
        params.client_info = None
        result = test_server._initialize(params)
        assert result.capabilities is not None


class TestTextDocumentDidOpen:
    """Test did open feature."""

    @patch('vasp_lsp.server._publish_diagnostics')
    def test_did_open(self, mock_publish):
        """Test document open."""
        params = DidOpenTextDocumentParams(
            text_document=TextDocumentItem(
                uri="file:///test.vasp",
                language_id="vasp",
                version=1,
                text="test content",
            )
        )
        text_document_did_open(params)
        assert server.get_document_content("file:///test.vasp") == "test content"
        mock_publish.assert_called_once()


class TestTextDocumentDidChange:
    """Test did change feature."""

    @patch('vasp_lsp.server._publish_diagnostics')
    def test_did_change(self, mock_publish):
        """Test document change."""
        server.set_document_content("file:///test.vasp", "old")
        params = DidChangeTextDocumentParams(
            text_document=VersionedTextDocumentIdentifier(
                uri="file:///test.vasp",
                version=2,
            ),
            content_changes=[MagicMock(text="new content")],
        )
        text_document_did_change(params)
        assert server.get_document_content("file:///test.vasp") == "new content"
        mock_publish.assert_called_once()

    @patch('vasp_lsp.server._publish_diagnostics')
    def test_did_change_empty_changes(self, mock_publish):
        """Test document change with empty changes."""
        server.set_document_content("file:///test.vasp", "old")
        params = DidChangeTextDocumentParams(
            text_document=VersionedTextDocumentIdentifier(
                uri="file:///test.vasp",
                version=2,
            ),
            content_changes=[],
        )
        text_document_did_change(params)
        mock_publish.assert_not_called()


class TestTextDocumentDidSave:
    """Test did save feature."""

    @patch('vasp_lsp.server._publish_diagnostics')
    def test_did_save(self, mock_publish):
        """Test document save."""
        server.set_document_content("file:///test.vasp", "content")
        params = DidSaveTextDocumentParams(
            text_document=TextDocumentIdentifier(uri="file:///test.vasp"),
        )
        text_document_did_save(params)
        mock_publish.assert_called_once()

    @patch('vasp_lsp.server._publish_diagnostics')
    def test_did_save_no_content(self, mock_publish):
        """Test document save without content."""
        params = DidSaveTextDocumentParams(
            text_document=TextDocumentIdentifier(uri="file:///nonexistent.vasp"),
        )
        text_document_did_save(params)
        mock_publish.assert_not_called()


class TestCompletions:
    """Test completions feature."""

    def test_completions_no_content(self):
        """Test completions without content."""
        params = CompletionParams(
            text_document=TextDocumentIdentifier(uri="file:///nonexistent.vasp"),
            position=Position(line=0, character=0),
        )
        result = completions(params)
        assert result is None

    @patch.object(server.completion_provider, 'get_completions')
    def test_completions_with_content(self, mock_get):
        """Test completions with content."""
        server.set_document_content("file:///test.vasp", "content")
        mock_get.return_value = MagicMock()
        params = CompletionParams(
            text_document=TextDocumentIdentifier(uri="file:///test.vasp"),
            position=Position(line=0, character=0),
        )
        result = completions(params)
        mock_get.assert_called_once()


class TestHover:
    """Test hover feature."""

    def test_hover_no_content(self):
        """Test hover without content."""
        params = HoverParams(
            text_document=TextDocumentIdentifier(uri="file:///nonexistent.vasp"),
            position=Position(line=0, character=0),
        )
        result = hover(params)
        assert result is None

    @patch.object(server.hover_provider, 'get_hover')
    def test_hover_with_content(self, mock_get):
        """Test hover with content."""
        server.set_document_content("file:///test.vasp", "content")
        mock_get.return_value = MagicMock()
        params = HoverParams(
            text_document=TextDocumentIdentifier(uri="file:///test.vasp"),
            position=Position(line=0, character=0),
        )
        result = hover(params)
        mock_get.assert_called_once()


class TestPublishDiagnostics:
    """Test publish diagnostics."""

    @patch.object(server, 'publish_diagnostics')
    @patch.object(server.diagnostics_provider, 'get_diagnostics')
    def test_publish_diagnostics(self, mock_get, mock_publish):
        """Test publishing diagnostics."""
        from vasp_lsp.server import _publish_diagnostics
        mock_get.return_value = [MagicMock()]
        _publish_diagnostics("file:///test.vasp", "content")
        mock_get.assert_called_once_with("content", "file:///test.vasp")
        mock_publish.assert_called_once()


class TestMain:
    """Test main entry point."""

    @patch('vasp_lsp.server.server.start_io')
    def test_main_stdio(self, mock_start):
        """Test main with stdio."""
        with patch('sys.argv', ['vasp-lsp', '--stdio']):
            main()
        mock_start.assert_called_once()

    @patch('vasp_lsp.server.server.start_tcp')
    def test_main_tcp(self, mock_start):
        """Test main with TCP."""
        with patch('sys.argv', ['vasp-lsp', '--tcp', '--host', '127.0.0.1', '--port', '2087']):
            main()
        mock_start.assert_called_once_with('127.0.0.1', 2087)

    def test_main_version(self):
        """Test main with version flag."""
        with pytest.raises(SystemExit) as exc_info:
            with patch('sys.argv', ['vasp-lsp', '--version']):
                main()
        assert exc_info.value.code == 0

    @patch('vasp_lsp.server.server.start_io')
    def test_main_default(self, mock_start):
        """Test main default behavior."""
        with patch('sys.argv', ['vasp-lsp']):
            main()
        mock_start.assert_called_once()
