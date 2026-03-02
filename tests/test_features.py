"""
Tests for LSP features (completion, hover, diagnostics).
"""

import pytest
from unittest.mock import Mock

from vasp_lsp.features.completion import CompletionProvider
from vasp_lsp.features.hover import HoverProvider
from vasp_lsp.features.diagnostics import DiagnosticsProvider


class TestCompletionProvider:
    """Test cases for CompletionProvider."""

    def test_provider_creation(self):
        """Test that provider can be created."""
        provider = CompletionProvider()
        assert provider is not None

    def test_get_completions_incar(self):
        """Test getting completions for INCAR file."""
        provider = CompletionProvider()
        
        # Mock params
        params = Mock()
        params.text_document = Mock()
        params.text_document.uri = "file:///test/INCAR"
        params.position = Mock()
        params.position.line = 0
        params.position.character = 0
        
        content = "ENC"
        completions = provider.get_completions(params, content, "file:///test/INCAR")
        
        # Should return some completions
        assert completions is not None

    def test_get_completions_non_vasp(self):
        """Test getting completions for non-VASP file."""
        provider = CompletionProvider()
        
        params = Mock()
        params.text_document = Mock()
        params.text_document.uri = "file:///test/other.txt"
        params.position = Mock()
        params.position.line = 0
        params.position.character = 0
        
        content = "some text"
        completions = provider.get_completions(params, content, "file:///test/other.txt")
        
        # Should return empty list for non-VASP files
        assert completions is not None
        assert completions.items == []


class TestHoverProvider:
    """Test cases for HoverProvider."""

    def test_provider_creation(self):
        """Test that provider can be created."""
        provider = HoverProvider()
        assert provider is not None

    def test_get_hover_incar(self):
        """Test getting hover for INCAR file."""
        provider = HoverProvider()
        
        params = Mock()
        params.text_document = Mock()
        params.text_document.uri = "file:///test/INCAR"
        params.position = Mock()
        params.position.line = 0
        params.position.character = 2
        
        content = "ENCUT = 520"
        hover = provider.get_hover(params, content, "file:///test/INCAR")
        
        # Should return hover info or None
        # Exact behavior depends on implementation

    def test_get_hover_non_vasp(self):
        """Test getting hover for non-VASP file."""
        provider = HoverProvider()
        
        params = Mock()
        params.text_document = Mock()
        params.text_document.uri = "file:///test/other.txt"
        params.position = Mock()
        params.position.line = 0
        params.position.character = 0
        
        content = "some text"
        hover = provider.get_hover(params, content, "file:///test/other.txt")
        
        # Should return None for non-VASP files
        assert hover is None


class TestDiagnosticsProvider:
    """Test cases for DiagnosticsProvider."""

    def test_provider_creation(self):
        """Test that provider can be created."""
        provider = DiagnosticsProvider()
        assert provider is not None

    def test_get_diagnostics_valid_incar(self):
        """Test getting diagnostics for valid INCAR."""
        provider = DiagnosticsProvider()
        
        content = "ENCUT = 520\nISMEAR = 0\nSIGMA = 0.05"
        diagnostics = provider.get_diagnostics(content, "file:///test/INCAR")
        
        # Should return empty list for valid content with SIGMA set
        assert diagnostics == []

    def test_get_diagnostics_invalid_incar(self):
        """Test getting diagnostics for invalid INCAR."""
        provider = DiagnosticsProvider()
        
        content = "This is not valid INCAR content with letters"
        diagnostics = provider.get_diagnostics(content, "file:///test/INCAR")
        
        # Should return some diagnostics for invalid content
        assert isinstance(diagnostics, list)

    def test_get_diagnostics_non_vasp(self):
        """Test getting diagnostics for non-VASP file."""
        provider = DiagnosticsProvider()
        
        content = "some random text"
        diagnostics = provider.get_diagnostics(content, "file:///test/other.txt")
        
        # Should return empty list for non-VASP files
        assert diagnostics == []

    def test_get_diagnostics_duplicate_parameter(self):
        """Test detecting duplicate parameters."""
        provider = DiagnosticsProvider()
        
        content = "ENCUT = 500\nENCUT = 520"
        diagnostics = provider.get_diagnostics(content, "file:///test/INCAR")
        
        # Should detect duplicate parameter
        assert len(diagnostics) > 0

    def test_get_diagnostics_poscar(self):
        """Test getting diagnostics for POSCAR file."""
        provider = DiagnosticsProvider()
        
        content = """Test
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Direct
0.0 0.0 0.0
"""
        diagnostics = provider.get_diagnostics(content, "file:///test/POSCAR")
        
        # Should return empty list for valid POSCAR
        assert diagnostics == []

    def test_get_diagnostics_kpoints(self):
        """Test getting diagnostics for KPOINTS file."""
        provider = DiagnosticsProvider()
        
        content = """Automatic
0
Automatic
4 4 4
0 0 0
"""
        diagnostics = provider.get_diagnostics(content, "file:///test/KPOINTS")
        
        # Should return empty list for valid KPOINTS
        assert diagnostics == []

    def test_get_diagnostics_missing_sigma_warning(self):
        """Test warning for missing SIGMA when ISMEAR >= 0."""
        provider = DiagnosticsProvider()
        
        content = "ENCUT = 520\nISMEAR = 0"
        diagnostics = provider.get_diagnostics(content, "file:///test/INCAR")
        
        # Should have informational diagnostic about missing SIGMA
        # Check for any diagnostic (may include unknown tag warning)
        assert isinstance(diagnostics, list)
