"""Additional tests for 100% coverage of formatting.py."""

import pytest
from vasp_lsp.features.formatting import FormattingProvider


class TestFormattingCoverage:
    """Tests for missing coverage in formatting.py."""
    
    def test_format_incar_empty_params(self):
        """Test INCAR formatting with empty/comment-only content (line 123)."""
        provider = FormattingProvider()
        content = "# Just a comment\n# Another comment"
        result = provider._format_incar(content)
        # Empty params should return empty list
        assert result == []
    
    def test_format_poscar_invalid_lattice_values(self):
        """Test POSCAR formatting with invalid lattice values (lines 206-207, 211)."""
        provider = FormattingProvider()
        # POSCAR with invalid lattice values that cause ValueError
        content = """Test POSCAR
   1.0
     invalid   0.0    0.0
     0.0   invalid    0.0
     0.0   0.0    invalid
H
1
Direct
   0.0   0.0   0.0"""
        result = provider._format_poscar(content)
        # Should handle ValueError and append original lines
        assert len(result) == 1
        assert "Test POSCAR" in result[0].new_text
    
    def test_format_poscar_short_lattice_line(self):
        """Test POSCAR formatting with short lattice lines (line 211)."""
        provider = FormattingProvider()
        content = """Test POSCAR
   1.0
     1.0
     0.0   1.0   0.0
     0.0   0.0   1.0
H
1
Direct
   0.0   0.0   0.0"""
        result = provider._format_poscar(content)
        assert len(result) == 1
    
    def test_format_poscar_invalid_coordinates(self):
        """Test POSCAR formatting with invalid coordinate values (lines 260-261)."""
        provider = FormattingProvider()
        content = """Test POSCAR
   1.0
     1.0   0.0   0.0
     0.0   1.0   0.0
     0.0   0.0   1.0
H
1
Direct
   invalid   0.0   0.0
   0.0   invalid   0.0
   # This is a comment
   F F F
   T T T"""
        result = provider._format_poscar(content)
        assert len(result) == 1
        # Should preserve comments and invalid lines as-is
        assert "# This is a comment" in result[0].new_text
    
    def test_format_kpoints_short_content(self):
        """Test KPOINTS formatting with short content."""
        provider = FormattingProvider()
        content = """KPOINTS
0"""
        result = provider._format_kpoints(content)
        assert result == []
    
    def test_format_kpoints_invalid_grid(self):
        """Test KPOINTS with invalid grid values (lines 346-348)."""
        provider = FormattingProvider()
        content = """KPOINTS
0
Gamma
  invalid 5 5"""
        result = provider._format_kpoints(content)
        assert len(result) == 1
        # Should preserve original line if parsing fails
        assert "invalid" in result[0].new_text
    
    def test_format_kpoints_six_values_invalid(self):
        """Test KPOINTS with invalid six-value line (lines 359-360)."""
        provider = FormattingProvider()
        content = """KPOINTS
0
Gamma
  invalid 5 5 0 0 invalid"""
        result = provider._format_kpoints(content)
        assert len(result) == 1
    
    def test_format_kpoints_invalid_kpoint_coords(self):
        """Test KPOINTS with invalid k-point coordinates."""
        provider = FormattingProvider()
        content = """KPOINTS
4
Line-mode
  invalid_kpoint
   0.0   0.0   0.0   1.0
   0.5   0.5   0.0   1.0"""
        result = provider._format_kpoints(content)
        assert len(result) == 1
