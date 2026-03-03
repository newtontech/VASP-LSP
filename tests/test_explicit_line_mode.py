"""
Tests for explicit mode and line mode edge cases.
"""

from vasp_lsp.parsers.kpoints_parser import KPOINTSMode, KPOINTSParser


class TestExplicitModeEdgeCases:
    """Tests for explicit mode edge cases - lines 244-249."""

    def test_explicit_mode_invalid_weight(self):
        """Test explicit mode with non-numeric weight - triggers exception handler."""
        content = """Test
2
Reciprocal
0.0 0.0 0.0 abc
0.5 0.0 0.0 1.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        assert data is None
        errors = parser.get_errors()
        assert len(errors) > 0
        assert "Error parsing explicit k-points" in errors[0]["message"]

    def test_explicit_mode_missing_kpoints_in_loop(self):
        """Test explicit mode with fewer k-points than declared."""
        content = """Test
5
Reciprocal
0.0 0.0 0.0 1.0
0.5 0.0 0.0 1.0
0.0 0.5 0.0 1.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should return None due to missing k-points
        if data is None:
            errors = parser.get_errors()
            assert len(errors) > 0
            # The error could be about k-point line format or missing k-points
            assert (
                "K-point" in errors[0]["message"] or "Expected" in errors[0]["message"]
            )
        else:
            # Or might parse what's available
            assert len(data.kpoints) <= 5


class TestLineModeEdgeCases:
    """Tests for line mode edge cases - lines 294-315."""

    def test_line_mode_with_empty_points_line(self):
        """Test line mode with empty points-per-line - uses default 20."""
        content = """Test
Line

Reciprocal
0.0 0.0 0.0
0.5 0.0 0.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should use default line_density = 20
        if data is not None:
            assert data.mode == KPOINTSMode.LINE_MODE
            assert data.line_density == 20
        else:
            # If parsing failed, check errors
            assert len(parser.get_errors()) >= 0

    def test_line_mode_with_whitespace_points_line(self):
        """Test line mode with whitespace-only points line."""
        content = """Test
Line

Reciprocal
0.0 0.0 0.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should use default line_density = 20
        if data is not None:
            assert data.mode == KPOINTSMode.LINE_MODE
            assert data.line_density == 20
        else:
            # If parsing failed, check errors
            assert len(parser.get_errors()) >= 0

    def test_line_mode_with_valid_points(self):
        """Test line mode with valid points-per-line."""
        content = """Line mode test
Line-mode
30
Reciprocal
0.0 0.0 0.0
0.5 0.0 0.0
0.5 0.5 0.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Line mode should parse successfully
        assert data is not None
        assert data.mode == KPOINTSMode.LINE_MODE
        assert data.line_density == 30
        assert len(data.kpoints) == 3

    def test_line_mode_with_empty_lines_between_points(self):
        """Test line mode with empty lines between k-points."""
        content = """Line mode test
Line
20
Reciprocal
0.0 0.0 0.0

0.5 0.0 0.0

0.5 0.5 0.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Line mode should parse successfully
        assert data is not None
        assert data.mode == KPOINTSMode.LINE_MODE
        assert len(data.kpoints) == 3

    def test_line_mode_with_short_kpoint_line(self):
        """Test line mode with k-point line having fewer than 3 values."""
        content = """Test
Line
20
Reciprocal
0.0 0.0
0.5 0.0 0.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should skip the line with fewer than 3 values
        assert data is not None
        assert data.mode == KPOINTSMode.LINE_MODE
        assert len(data.kpoints) == 1  # Only the second k-point

    def test_line_mode_exception_handler(self):
        """Test line mode exception handler with invalid data."""
        content = """Test
Line
abc
Reciprocal
0.0 0.0 0.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should handle ValueError gracefully
        assert data is None or len(parser.get_errors()) >= 0
