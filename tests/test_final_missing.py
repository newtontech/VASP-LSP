"""
Final tests for remaining missing coverage lines.
"""

import pytest
from vasp_lsp.parsers.kpoints_parser import KPOINTSParser, KPOINTSMode


class TestFinalMissingCoverage:
    """Tests for remaining missing lines."""

    def test_explicit_mode_missing_kpoints_triggers_244_249(self):
        """Test explicit mode with fewer k-points than declared - triggers lines 244-249."""
        # No trailing newline to ensure we hit the 'line_idx >= len(self.lines)' check
        content = """Test
3
Reciprocal
0.0 0.0 0.0 1.0
0.5 0.0 0.0 1.0"""
        parser = KPOINTSParser(content)
        data = parser.parse()
        
        assert data is None
        errors = parser.get_errors()
        assert len(errors) > 0
        assert "Expected 3 k-points, found 2" in errors[0]['message']
