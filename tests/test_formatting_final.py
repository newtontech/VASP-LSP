"""Final tests to reach 100% coverage on formatting.py."""

from unittest.mock import MagicMock, patch

import pytest

from vasp_lsp.features.formatting import FormattingProvider


@pytest.fixture
def formatter():
    """Create a formatting provider fixture."""
    return FormattingProvider()


class TestINCARLine123:
    """Test line 123: if not params: return []"""

    def test_format_incar_empty_params(self, formatter):
        """Test that empty params returns empty list (line 123)."""
        # Mock the INCARParser to return empty params
        with patch("vasp_lsp.features.formatting.INCARParser") as MockParser:
            mock_instance = MagicMock()
            mock_instance.parse.return_value = {}
            MockParser.return_value = mock_instance

            result = formatter._format_incar("# Only comments")
            assert result == []


class TestPOSCARLines206207211:
    """Test lines 206-207, 211: POSCAR edge cases."""

    def test_format_poscar_fifth_lattice_line(self, formatter):
        """Test POSCAR when line 5 is missing (lines 206-207)."""
        content = """Si
1.0
1 0 0
0 1 0
Si
1
Direct
0 0 0
"""
        result = formatter._format_poscar(content)
        assert len(result) == 1
        # Should add fallback lattice line
        text = result[0].new_text
        lines = text.split("\n")
        # Check that there are properly formatted lattice lines
        assert any("0.0000000000" in line for line in lines)

    def test_format_poscar_line6_present(self, formatter):
        """Test POSCAR when line 6 (element symbols) is present (line 211)."""
        content = """Si
1.0
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
        assert "Si" in result[0].new_text


class TestKPOINTSLines260261:
    """Test lines 260-261: KPOINTS ValueError handling."""

    def test_format_kpoints_coord_parse_error(self, formatter):
        """Test KPOINTS when coordinate parsing fails (lines 260-261)."""
        content = """Explicit
4
Cartesian
abc def ghi
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1
        # Should preserve original stripped line on error
        assert "abc def ghi" in result[0].new_text


class TestKPOINTSLines346348359360:
    """Test lines 346-348, 359-360: KPOINTS explicit coordinate handling."""

    def test_format_kpoints_with_comments(self, formatter):
        """Test KPOINTS parsing with comments (lines 346-348)."""
        content = """Explicit k-points
4
Cartesian
0.1 0.2 0.3 0.25  # Gamma point
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1
        text = result[0].new_text
        assert "Gamma point" in text

    def test_format_kpoints_short_line(self, formatter):
        """Test KPOINTS with less than 3 parts (lines 359-360)."""
        content = """Explicit k-points
4
Cartesian
0.1 0.2
short
0.3 0.4 0.5
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1
        text = result[0].new_text
        assert "short" in text


class TestKPOINTSLineModeAndGrid:
    """Additional KPOINTS coverage tests."""

    def test_format_kpoints_line_mode_formatting(self, formatter):
        """Test KPOINTS line mode with proper formatting."""
        content = """Line-mode
20
Line-mode
Reciprocal
0 0 0 1
0.5 0 0 1
"""
        result = formatter._format_kpoints(content)
        assert len(result) == 1
        assert "Line-mode" in result[0].new_text
