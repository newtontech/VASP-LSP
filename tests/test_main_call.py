"""
Test for the main() function call in server.py.
"""

import pytest
from unittest.mock import patch
import sys


class TestMainCall:
    """Test the main() function call."""

    def test_main_call_via_import(self):
        """Test that main() is called when module is run directly."""
        # We can't easily test the if __name__ == "__main__" block
        # But we can verify the main function exists and works
        from vasp_lsp.server import main, server
        
        # Test that main() calls start_io when no args
        with patch.object(sys, 'argv', ['vasp-lsp']):
            with patch.object(server, 'start_io') as mock_io:
                main()
                mock_io.assert_called_once()
