"""Tests for INCAR parser."""

import pytest
from vasp_lsp.parsers.incar_parser import INCARParser, INCARParameter


class TestINCARParser:
    """Test cases for INCAR parser."""
    
    def test_parse_simple_parameter(self):
        """Test parsing a simple parameter."""
        content = "ENCUT = 500"
        parser = INCARParser(content)
        params = parser.parse()
        
        assert "ENCUT" in params
        assert params["ENCUT"].value == 500
        assert params["ENCUT"].name == "ENCUT"
        
    def test_parse_multiple_parameters(self):
        """Test parsing multiple parameters."""
        content = """ENCUT = 500
ISMEAR = 0
SIGMA = 0.1"""
        parser = INCARParser(content)
        params = parser.parse()
        
        assert len(params) == 3
        assert params["ENCUT"].value == 500
        assert params["ISMEAR"].value == 0
        assert params["SIGMA"].value == 0.1
        
    def test_parse_boolean_values(self):
        """Test parsing boolean values."""
        content = """LWAVE = .TRUE.
LCHARG = .FALSE.
LHFCALC = T"""
        parser = INCARParser(content)
        params = parser.parse()
        
        assert params["LWAVE"].value is True
        assert params["LCHARG"].value is False
        assert params["LHFCALC"].value is True
        
    def test_parse_array_values(self):
        """Test parsing array values."""
        content = "MAGMOM = 1.0 2.0 3.0"
        parser = INCARParser(content)
        params = parser.parse()
        
        assert params["MAGMOM"].value == [1.0, 2.0, 3.0]
        
    def test_case_insensitivity(self):
        """Test that tag names are case-insensitive."""
        content = "encut = 500\nEncut = 600"
        parser = INCARParser(content)
        params = parser.parse()
        
        # Last value should win
        assert params["ENCUT"].value == 600
        assert len(parser.get_errors()) > 0  # Should have duplicate warning
        
    def test_ignore_comments(self):
        """Test that comments are ignored."""
        content = """ENCUT = 500 # This is a comment
! This is also a comment
ISMEAR = 0"""
        parser = INCARParser(content)
        params = parser.parse()
        
        assert params["ENCUT"].value == 500
        assert params["ISMEAR"].value == 0
        
    def test_empty_lines(self):
        """Test that empty lines are ignored."""
        content = """ENCUT = 500

ISMEAR = 0

"""
        parser = INCARParser(content)
        params = parser.parse()
        
        assert len(params) == 2
        
    def test_spelling_variations(self):
        """Test various spacing formats."""
        content = """ENCUT=500
ISMEAR =0
SIGMA= 0.1
  NSW  =  100  """
        parser = INCARParser(content)
        params = parser.parse()
        
        assert params["ENCUT"].value == 500
        assert params["ISMEAR"].value == 0
        assert params["SIGMA"].value == 0.1
        assert params["NSW"].value == 100


class TestINCARParserErrors:
    """Test error handling in INCAR parser."""
    
    def test_invalid_format(self):
        """Test detection of invalid formats."""
        content = "ENCUT 500"  # Missing equals
        parser = INCARParser(content)
        params = parser.parse()
        
        assert len(params) == 0
        assert len(parser.get_errors()) > 0
