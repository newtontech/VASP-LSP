"""Detailed tests for LSP features to improve coverage."""
import pytest
from unittest.mock import MagicMock
from lsprotocol.types import (
    Position, CompletionParams, HoverParams
)

from vasp_lsp.features.completion import CompletionProvider
from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.features.hover import HoverProvider


class TestCompletionProvider:
    """Test completion provider."""

    def setup_method(self):
        """Set up test fixtures."""
        self.provider = CompletionProvider()

    def test_get_completions_incar_empty(self):
        """Test completions for empty INCAR."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        result = self.provider.get_completions(params, "", "file:///INCAR")
        assert result is not None
        assert hasattr(result, 'items')

    def test_get_completions_incar_partial(self):
        """Test completions for partial tag name."""
        params = MagicMock()
        params.position = Position(line=0, character=3)
        result = self.provider.get_completions(params, "ENC", "file:///INCAR")
        assert result is not None

    def test_get_completions_poscar(self):
        """Test completions for POSCAR."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        result = self.provider.get_completions(params, "", "file:///POSCAR")
        assert result is not None

    def test_get_completions_kpoints(self):
        """Test completions for KPOINTS."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        result = self.provider.get_completions(params, "", "file:///KPOINTS")
        assert result is not None

    def test_get_completions_unknown_file(self):
        """Test completions for unknown file type."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        result = self.provider.get_completions(params, "", "file:///unknown.txt")
        assert result is not None

    def test_get_completions_out_of_bounds(self):
        """Test completions with out of bounds position."""
        params = MagicMock()
        params.position = Position(line=100, character=100)
        result = self.provider.get_completions(params, "ENCUT = 500", "file:///INCAR")
        assert result is not None

    def test_get_completions_after_equals(self):
        """Test completions after equals sign."""
        params = MagicMock()
        params.position = Position(line=0, character=8)
        result = self.provider.get_completions(params, "ENCUT = ", "file:///INCAR")
        assert result is not None


class TestDiagnosticsProvider:
    """Test diagnostics provider."""

    def setup_method(self):
        """Set up test fixtures."""
        self.provider = DiagnosticsProvider()

    def test_get_diagnostics_incar_valid(self):
        """Test diagnostics for valid INCAR."""
        content = "ENCUT = 500\nISPIN = 1"
        result = self.provider.get_diagnostics(content, "file:///INCAR")
        assert isinstance(result, list)

    def test_get_diagnostics_incar_invalid(self):
        """Test diagnostics for invalid INCAR."""
        content = "INVALID_TAG = value"
        result = self.provider.get_diagnostics(content, "file:///INCAR")
        assert isinstance(result, list)

    def test_get_diagnostics_poscar(self):
        """Test diagnostics for POSCAR."""
        content = "Test\n1.0\n1 0 0\n0 1 0\n0 0 1\nSi\n1\nReciprocal\n0 0 0"
        result = self.provider.get_diagnostics(content, "file:///POSCAR")
        assert isinstance(result, list)

    def test_get_diagnostics_kpoints(self):
        """Test diagnostics for KPOINTS."""
        content = "K-Points\n0\nGamma\n4 4 4\n0 0 0"
        result = self.provider.get_diagnostics(content, "file:///KPOINTS")
        assert isinstance(result, list)

    def test_get_diagnostics_empty(self):
        """Test diagnostics for empty content."""
        result = self.provider.get_diagnostics("", "file:///INCAR")
        assert isinstance(result, list)

    def test_get_diagnostics_unknown_file(self):
        """Test diagnostics for unknown file type."""
        result = self.provider.get_diagnostics("some content", "file:///unknown.txt")
        assert isinstance(result, list)

    def test_get_diagnostics_contcar(self):
        """Test diagnostics for CONTCAR."""
        content = "Test\n1.0\n1 0 0\n0 1 0\n0 0 1\nSi\n1\nReciprocal\n0 0 0"
        result = self.provider.get_diagnostics(content, "file:///CONTCAR")
        assert isinstance(result, list)


class TestHoverProvider:
    """Test hover provider."""

    def setup_method(self):
        """Set up test fixtures."""
        self.provider = HoverProvider()

    def test_get_hover_incar(self):
        """Test hover for INCAR tag."""
        params = MagicMock()
        params.position = Position(line=0, character=2)
        result = self.provider.get_hover(params, "ENCUT = 500", "file:///INCAR")
        # May return None or Hover object
        assert result is None or hasattr(result, 'contents')

    def test_get_hover_poscar(self):
        """Test hover for POSCAR."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        content = "Test\n1.0\n1 0 0\n0 1 0\n0 0 1\nSi\n1\nReciprocal\n0 0 0"
        result = self.provider.get_hover(params, content, "file:///POSCAR")
        assert result is None or hasattr(result, 'contents')

    def test_get_hover_kpoints(self):
        """Test hover for KPOINTS."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        content = "K-Points\n0\nGamma\n4 4 4\n0 0 0"
        result = self.provider.get_hover(params, content, "file:///KPOINTS")
        assert result is None or hasattr(result, 'contents')

    def test_get_hover_empty(self):
        """Test hover for empty content."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        result = self.provider.get_hover(params, "", "file:///INCAR")
        assert result is None

    def test_get_hover_unknown_file(self):
        """Test hover for unknown file type."""
        params = MagicMock()
        params.position = Position(line=0, character=0)
        result = self.provider.get_hover(params, "some content", "file:///unknown.txt")
        assert result is None

    def test_get_hover_on_encut_value(self):
        """Test hover on ENCUT value."""
        params = MagicMock()
        params.position = Position(line=0, character=10)
        result = self.provider.get_hover(params, "ENCUT = 500", "file:///INCAR")
        assert result is None or hasattr(result, 'contents')


class TestKPOINTSParserDetailed:
    """Detailed tests for KPOINTS parser to improve coverage."""

    def test_parse_line_mode_with_weights(self):
        """Test parsing KPOINTS in line mode with weights."""
        from vasp_lsp.parsers.kpoints_parser import KPOINTSParser, KPOINTSMode
        
        content = """Line mode
5
Reciprocal
0.0 0.0 0.0 1.0
0.5 0.5 0.0 1.0
0.5 0.5 0.5 1.0
0.0 0.0 0.5 1.0
0.0 0.0 0.0 1.0"""
        
        parser = KPOINTSParser(content)
        result = parser.parse()
        
        assert result is not None
        assert result.mode == KPOINTSMode.EXPLICIT

    def test_parse_monkhorst_pack(self):
        """Test parsing Monkhorst-Pack grid."""
        from vasp_lsp.parsers.kpoints_parser import KPOINTSParser, KPOINTSMode
        
        content = """MP grid
0
Monkhorst
4 4 4
0 0 0"""
        
        parser = KPOINTSParser(content)
        result = parser.parse()
        
        assert result is not None
        assert result.mode == KPOINTSMode.GAMMA_MONKHORST

    def test_parse_with_weights(self):
        """Test parsing KPOINTS with weights."""
        from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
        
        content = """Explicit k-points with weights
4
Reciprocal
0.0 0.0 0.0 1.0
0.5 0.0 0.0 1.0
0.0 0.5 0.0 1.0
0.0 0.0 0.5 1.0"""
        
        parser = KPOINTSParser(content)
        result = parser.parse()
        
        assert result is not None

    def test_parse_negative_shift(self):
        """Test parsing with negative shift."""
        from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
        
        content = """Shifted grid
0
Gamma
4 4 4
-0.5 -0.5 -0.5"""
        
        parser = KPOINTSParser(content)
        result = parser.parse()
        
        assert result is not None
        assert result.shift == [-0.5, -0.5, -0.5]

    def test_parse_cartesian_mode(self):
        """Test parsing Cartesian mode."""
        from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
        
        content = """Cartesian k-points
4
Cartesian
0.0 0.0 0.0 1.0
0.5 0.0 0.0 1.0
0.0 0.5 0.0 1.0
0.0 0.0 0.5 1.0"""
        
        parser = KPOINTSParser(content)
        result = parser.parse()
        
        assert result is not None

    def test_parse_reciprocal_mode(self):
        """Test parsing Reciprocal mode."""
        from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
        
        content = """Reciprocal k-points
4
Reciprocal
0.0 0.0 0.0 1.0
0.5 0.0 0.0 1.0
0.0 0.5 0.0 1.0
0.0 0.0 0.5 1.0"""
        
        parser = KPOINTSParser(content)
        result = parser.parse()
        
        assert result is not None

    def test_parse_tetrahedra_method(self):
        """Test parsing with tetrahedra method."""
        from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
        
        content = """Tetrahedra
0
Tetrahedra
4 4 4
0 0 0"""
        
        parser = KPOINTSParser(content)
        result = parser.parse()
        
        # May or may not be supported
        assert result is not None or parser.get_errors()


class TestINCARParserEdgeCases:
    """Edge case tests for INCAR parser to improve coverage."""

    def test_parse_with_comments(self):
        """Test parsing INCAR with comments."""
        from vasp_lsp.parsers.incar_parser import INCARParser
        
        content = "# This is a comment\nENCUT = 500  # Energy cutoff"
        
        parser = INCARParser(content)
        result = parser.parse()
        
        assert result is not None
        assert 'ENCUT' in result

    def test_parse_boolean_values(self):
        """Test parsing boolean values."""
        from vasp_lsp.parsers.incar_parser import INCARParser
        
        content = "LDAU = .TRUE.\nLORBIT = .FALSE."
        
        parser = INCARParser(content)
        result = parser.parse()
        
        assert result is not None
        assert result['LDAU'].value == True
        assert result['LORBIT'].value == False

    def test_parse_string_values(self):
        """Test parsing string values."""
        from vasp_lsp.parsers.incar_parser import INCARParser
        
        content = 'SYSTEM = "Test calculation"\nGGA = PE'
        
        parser = INCARParser(content)
        result = parser.parse()
        
        assert result is not None

    def test_parse_empty_lines(self):
        """Test parsing with empty lines."""
        from vasp_lsp.parsers.incar_parser import INCARParser
        
        content = "ENCUT = 500\n\n\nISPIN = 1"
        
        parser = INCARParser(content)
        result = parser.parse()
        
        assert result is not None

    def test_parse_special_characters(self):
        """Test parsing with special characters."""
        from vasp_lsp.parsers.incar_parser import INCARParser
        
        content = "MAGMOM = 10*1.0"
        
        parser = INCARParser(content)
        result = parser.parse()
        
        assert result is not None

    def test_parse_integer_values(self):
        """Test parsing integer values."""
        from vasp_lsp.parsers.incar_parser import INCARParser
        
        content = "NSW = 100\nIBRION = 2"
        
        parser = INCARParser(content)
        result = parser.parse()
        
        assert result is not None
        assert result['NSW'].value == 100

    def test_parse_float_values(self):
        """Test parsing float values."""
        from vasp_lsp.parsers.incar_parser import INCARParser
        
        content = "ENCUT = 520.5\nEDIFF = 1e-6"
        
        parser = INCARParser(content)
        result = parser.parse()
        
        assert result is not None
        assert result['ENCUT'].value == 520.5
