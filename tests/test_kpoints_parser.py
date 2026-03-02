"""Tests for KPOINTS parser - TDD approach."""
import pytest
from vasp_lsp.parsers.kpoints_parser import KPOINTSParser, KPOINTSData, KPOINTSMode


class TestKpointsParserFirstLine:
    """Test KPOINTS parser correctly handles first line (comment)."""

    def test_parse_kpoints_with_comment_first_line(self):
        """Test that KPOINTS parser correctly extracts comment from first line."""
        content = """K-Points
0
Gamma
4 4 4
0 0 0"""

        parser = KPOINTSParser(content)
        result = parser.parse()

        assert result is not None
        assert result.comment == "K-Points"

    def test_parse_kpoints_with_multiline_comment(self):
        """Test that KPOINTS parser handles multiline comments in first line."""
        content = """Silicon band structure - Gamma centered mesh
0
Gamma
4 4 4
0 0 0"""

        parser = KPOINTSParser(content)
        result = parser.parse()

        assert result is not None
        assert "Silicon" in result.comment
        assert "band structure" in result.comment

    def test_parse_kpoints_with_empty_first_line(self):
        """Test that KPOINTS parser handles empty first line with error."""
        content = """
0
Gamma
4 4 4
0 0 0"""

        parser = KPOINTSParser(content)
        result = parser.parse()

        # Empty comment should still parse, but return empty string
        assert result is not None
        assert result.comment == ""

    def test_parse_kpoints_from_fixture_file(self):
        """Test parsing the actual fixture KPOINTS file."""
        content = """K-Points
0
Gamma
4 4 4
0 0 0"""

        parser = KPOINTSParser(content)
        result = parser.parse()
        errors = parser.get_errors()

        # Should parse successfully
        assert result is not None
        assert result.comment == "K-Points"
        assert result.mode == KPOINTSMode.GAMMA_MONKHORST
        assert result.grid == [4, 4, 4]
        assert result.shift == [0.0, 0.0, 0.0]

        # Should have no errors
        assert len(errors) == 0

    def test_parse_kpoints_invalid_format_captures_error(self):
        """Test that parser captures errors for invalid KPOINTS format."""
        content = """K-Points
invalid_number
Gamma
4 4 4
0 0 0"""

        parser = KPOINTSParser(content)
        result = parser.parse()
        errors = parser.get_errors()

        # Should return None for invalid format
        assert result is None
        # Should capture error
        assert len(errors) > 0
        assert "Invalid k-point specification" in errors[0]['message']

    def test_parse_kpoints_too_short_captures_error(self):
        """Test that parser captures error for too short KPOINTS file."""
        content = """K-Points
0"""

        parser = KPOINTSParser(content)
        result = parser.parse()
        errors = parser.get_errors()

        # Should return None for incomplete file
        assert result is None
        # Should capture error
        assert len(errors) > 0
        assert "end of file" in errors[0]['message'].lower()

    def test_parse_kpoints_with_special_chars_in_comment(self):
        """Test that KPOINTS parser handles special characters in comment."""
        content = """K-Points: Γ-centered 4x4x4 mesh for Si!
0
Gamma
4 4 4
0 0 0"""

        parser = KPOINTSParser(content)
        result = parser.parse()

        assert result is not None
        assert "Γ-centered" in result.comment or "K-Points" in result.comment
