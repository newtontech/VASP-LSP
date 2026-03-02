"""Tests for VASP parsers."""
import pytest
from unittest.mock import MagicMock

from vasp_lsp.parsers import incar_parser, kpoints_parser, poscar_parser


class TestIncarParser:
    """Test INCAR parser."""

    def test_parser_import(self):
        """Test parser module imports."""
        assert incar_parser is not None


class TestKpointsParser:
    """Test KPOINTS parser."""

    def test_parser_import(self):
        """Test parser module imports."""
        assert kpoints_parser is not None


class TestPoscarParser:
    """Test POSCAR parser."""

    def test_parser_import(self):
        """Test parser module imports."""
        assert poscar_parser is not None
