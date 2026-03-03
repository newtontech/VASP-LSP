"""Tests for formatting coverage edge cases to reach 100%."""

import pytest
from lsprotocol.types import TextEdit

from vasp_lsp.features.formatting import FormattingProvider


@pytest.fixture
def formatter():
    """Create a formatting provider fixture."""
    return FormattingProvider()


class TestINCAREdgeCases:
    """Test INCAR edge cases for 100% coverage."""
    
    def test_format_incar_no_params(self, formatter):
        """Test formatting INCAR with only comments/no valid params (line 123)."""
        content = """# Just a comment
# Another comment
"""
        result = formatter._format_incar(content)
        # When parser returns empty params, should return empty list
        assert result == []


class TestPOSCAREdgeCases:
    """Test POSCAR edge cases for 100% coverage."""
    
    def test_format_poscar_invalid_scaling_factor(self, formatter):
        """Test POSCAR with invalid scaling factor (lines 193-194)."""
        content = """Si
invalid_scale
5 0 0
0 5 0
0 0 5
Si
1
Direct
0 0 0
"""
        result = formatter._format_poscar(content)
        assert len(result) == 1
        # When scaling factor is invalid, code keeps original line
        assert "invalid_scale" in result[0].new_text
    
    def test_format_poscar_short_content_fewer_than_5_lines(self, formatter):
        """Test POSCAR with fewer than 5 lines returns empty list."""
        content = """Si
1.0
5 0 0
"""
        result = formatter._format_poscar(content)
        # Less than 5 lines returns empty list
        assert result == []
    
    def test_format_poscar_short_lattice(self, formatter):
        """Test POSCAR with short lattice vectors (lines 206-207)."""
        content = """Si
1.0
5 0 0
0 5 0
Si
1
Direct
0 0 0
"""
        result = formatter._format_poscar(content)
        assert len(result) == 1
        text = result[0].new_text
        # Should fill missing lattice with zeros
        assert "0.0000000000" in text
    
    def test_format_poscar_invalid_lattice_values(self, formatter):
        """Test POSCAR with invalid lattice values (lines 206-207)."""
        content = """Si
1.0
invalid line
0 5 0
0 0 5
Si
1
Direct
0 0 0
"""
        result = formatter._format_poscar(content)
        assert len(result) == 1
    
    def test_format_poscar_malformed_lattice(self, formatter):
        """Test POSCAR with malformed lattice line (less than 3 parts)."""
        content = """Si
1.0
5 0
0 5 0
0 0 5
Si
1
Direct
0 0 0
"""
        result = formatter._format_poscar(content)
        assert len(result) == 1


class TestKPOINTSEdgeCases:
    """Test KPOINTS edge cases for 100% coverage."""
    
    def test_format_kpoints_invalid_grid_line(self, formatter):
        """Test KPOINTS with invalid grid line (lines 260-261)."""
        content = """Automatic mesh
0
Gamma
invalid grid
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1
        # Should preserve original line on ValueError
        assert "invalid grid" in result[0].new_text
    
    def test_format_kpoints_invalid_six_numbers(self, formatter):
        """Test KPOINTS with 6 invalid numbers (lines 260-261)."""
        content = """Automatic mesh
0
Gamma
a b c d e f
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1
        text = result[0].new_text
        assert "a b c d e f" in text
    
    def test_format_kpoints_explicit_invalid_coords(self, formatter):
        """Test KPOINTS explicit mode with invalid coordinates (lines 321-322)."""
        content = """Explicit k-points
4
Cartesian
invalid line here
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1
        text = result[0].new_text
        assert "invalid line here" in text
    
    def test_format_kpoints_mixed_invalid_coords(self, formatter):
        """Test KPOINTS with some valid and some invalid coords (lines 321-322)."""
        content = """Explicit k-points
4
Cartesian
0.1 0.2 0.3 0.25
invalid
0.4 0.5 0.6 0.25
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1


class TestFormatValueEdgeCases:
    """Test _format_value method edge cases."""
    
    def test_format_value_tuple(self, formatter):
        """Test formatting tuple values."""
        result = formatter._format_value((1, 2, 3))
        assert result == "1 2 3"
    
    def test_format_value_nested_list(self, formatter):
        """Test formatting nested list."""
        result = formatter._format_value([1, [2, 3]])
        assert "1" in result
        assert "[2, 3]" in result or result == "1 [2, 3]"


class TestDocumentFormatEdgeCases:
    """Test format_document edge cases."""
    
    def test_format_document_unknown_type(self, formatter):
        """Test format_document with unknown file type."""
        result = formatter.format_document("some content", "file:///unknown.txt")
        assert result == []


class TestINCARParserEmptyCases:
    """Test cases where INCAR parser returns empty params."""
    
    def test_format_incar_only_newlines(self, formatter):
        """Test formatting INCAR with only newlines."""
        content = "\n\n\n"
        result = formatter._format_incar(content)
        assert result == []
    
    def test_format_incar_only_whitespace(self, formatter):
        """Test formatting INCAR with only whitespace."""
        content = "   \n   \n   \n"
        result = formatter._format_incar(content)
        assert result == []
