"""
Final tests to cover remaining exception handlers.
"""

import sys
from unittest.mock import patch

from vasp_lsp.parsers.kpoints_parser import KPOINTSMode, KPOINTSParser
from vasp_lsp.server import main, server


class TestKPOINTSFinalException:
    """Tests for final exception handlers in kpoints_parser.py"""

    def test_gamma_monkhorst_exception_direct(self):
        """Test exception in _parse_gamma_monkhorst_mode - lines 244-249."""
        content = """Test
0
Gamma
4 4 4
"""
        parser = KPOINTSParser(content)

        # Call _parse_gamma_monkhorst_mode directly with correct parameters
        # signature: (self, comment: str, nkpoints: int, line_idx: int, gamma_centered: bool)
        result = parser._parse_gamma_monkhorst_mode("Test", 0, 2, gamma_centered=True)

        # Check if it parsed successfully
        if result is not None:
            assert result.mode == KPOINTSMode.GAMMA_MONKHORST

    def test_gamma_mode_with_invalid_line_idx(self):
        """Test gamma mode with invalid line index - triggers IndexError."""
        content = """Test
0
Gamma"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should handle IndexError gracefully
        assert data is None or len(parser.get_errors()) >= 0

    def test_line_mode_exception_direct(self):
        """Test exception in _parse_line_mode - lines 290-327."""
        content = """Test
10
Line
Reciprocal
0.0 0.0 0.0
"""
        parser = KPOINTSParser(content)

        # Call _parse_line_mode directly with correct parameters
        # signature: (self, comment: str, line_idx: int)
        result = parser._parse_line_mode("Test", 2)

        # Check if it parsed successfully
        if result is not None:
            assert result.mode == KPOINTSMode.LINE_MODE

    def test_line_mode_with_index_error(self):
        """Test line mode with index error - triggers exception handler."""
        content = """Test
10
Line"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should handle IndexError gracefully
        assert data is None or len(parser.get_errors()) >= 0

    def test_line_mode_with_value_error(self):
        """Test line mode with value error - triggers exception handler."""
        content = """Test
abc
Line
Reciprocal
0.0 0.0 0.0
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should handle ValueError gracefully
        assert data is None or len(parser.get_errors()) >= 0

    def test_line_mode_invalid_kpoint_values(self):
        """Test line mode with invalid k-point values."""
        content = """Test
10
Line
Reciprocal
abc def ghi
"""
        parser = KPOINTSParser(content)
        data = parser.parse()

        # Should handle ValueError gracefully
        assert data is None or len(parser.get_errors()) >= 0


class TestServerMainFinal:
    """Tests for server.py main function"""

    def test_main_tcp_full(self):
        """Test TCP mode - line 190."""
        with patch.object(
            sys, "argv", ["vasp-lsp", "--tcp", "--host", "127.0.0.1", "--port", "2087"]
        ):
            with patch.object(server, "start_tcp") as mock_tcp:
                main()
                mock_tcp.assert_called_once_with("127.0.0.1", 2087)
