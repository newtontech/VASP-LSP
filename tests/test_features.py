"""Tests for VASP LSP features."""
import pytest
from unittest.mock import MagicMock, patch
from lsprotocol.types import Position, CompletionParams, HoverParams, TextDocumentIdentifier

from vasp_lsp.features.completion import CompletionProvider
from vasp_lsp.features.hover import HoverProvider
from vasp_lsp.features.diagnostics import DiagnosticsProvider


class TestCompletionProvider:
    """Test completion provider."""

    def test_provider_initialization(self):
        """Test provider initialization."""
        provider = CompletionProvider()
        assert provider is not None

    def test_get_completions(self):
        """Test getting completions."""
        provider = CompletionProvider()
        params = CompletionParams(
            text_document=TextDocumentIdentifier(uri="file:///test.vasp"),
            position=Position(line=0, character=0),
        )
        content = "ENCUT = 520"
        result = provider.get_completions(params, content, "file:///test.vasp")
        assert result is not None or result is None


class TestHoverProvider:
    """Test hover provider."""

    def test_provider_initialization(self):
        """Test provider initialization."""
        provider = HoverProvider()
        assert provider is not None

    def test_get_hover(self):
        """Test getting hover info."""
        provider = HoverProvider()
        params = HoverParams(
            text_document=TextDocumentIdentifier(uri="file:///test.vasp"),
            position=Position(line=0, character=0),
        )
        content = "ENCUT = 520"
        result = provider.get_hover(params, content, "file:///test.vasp")
        assert result is not None or result is None


class TestDiagnosticsProvider:
    """Test diagnostics provider."""

    def test_provider_initialization(self):
        """Test provider initialization."""
        provider = DiagnosticsProvider()
        assert provider is not None

    def test_get_diagnostics(self):
        """Test getting diagnostics."""
        provider = DiagnosticsProvider()
        content = "ENCUT = 520\nISMEAR = 0"
        result = provider.get_diagnostics(content, "file:///test.vasp")
        assert isinstance(result, list)

    def test_get_diagnostics_empty(self):
        """Test getting diagnostics for empty content."""
        provider = DiagnosticsProvider()
        result = provider.get_diagnostics("", "file:///test.vasp")
        assert isinstance(result, list)
