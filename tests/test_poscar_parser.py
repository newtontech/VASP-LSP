"""
Tests for POSCAR parser.
"""

import pytest
from vasp_lsp.parsers.poscar_parser import POSCARParser, POSCARData


class TestPOSCARParser:
    """Test cases for POSCARParser."""

    def test_parse_simple_poscar(self):
        """Test parsing a simple POSCAR file."""
        content = """Simple cubic
1.0
3.0 0.0 0.0
0.0 3.0 0.0
0.0 0.0 3.0
Si
1
Direct
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is not None
        assert data.system_comment == "Simple cubic"
        assert data.scale_factor == 1.0
        assert data.lattice_vectors == [[3.0, 0.0, 0.0], [0.0, 3.0, 0.0], [0.0, 0.0, 3.0]]
        assert data.atom_types == ["Si"]
        assert data.atom_counts == [1]
        assert data.coordinate_type == "Direct"
        assert len(data.coordinates) == 1
        assert data.coordinates[0] == [0.0, 0.0, 0.0]

    def test_parse_vasp4_format(self):
        """Test parsing VASP 4 format (no atom types)."""
        content = """VASP4 format
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
2
Direct
0.0 0.0 0.0
0.5 0.5 0.5
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is not None
        assert data.atom_types == ["Type1"]  # Generic types
        assert data.atom_counts == [2]
        assert len(data.coordinates) == 2

    def test_parse_multiple_atom_types(self):
        """Test parsing with multiple atom types."""
        content = """Multi-atom
5.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si O
2 1
Direct
0.0 0.0 0.0
0.5 0.5 0.5
0.25 0.25 0.25
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is not None
        assert data.atom_types == ["Si", "O"]
        assert data.atom_counts == [2, 1]
        assert len(data.coordinates) == 3

    def test_parse_cartesian_coordinates(self):
        """Test parsing Cartesian coordinates."""
        content = """Cartesian
1.0
2.0 0.0 0.0
0.0 2.0 0.0
0.0 0.0 2.0
H
1
Cartesian
1.0 1.0 1.0
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is not None
        assert data.coordinate_type == "Cartesian"
        assert data.coordinates[0] == [1.0, 1.0, 1.0]

    def test_parse_selective_dynamics(self):
        """Test parsing with selective dynamics."""
        content = """Selective
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
2
Selective dynamics
Direct
0.0 0.0 0.0 T T T
0.5 0.5 0.5 F F F
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is not None
        assert data.selective_dynamics is not None
        assert data.selective_dynamics[0] == [True, True, True]
        assert data.selective_dynamics[1] == [False, False, False]

    def test_parse_negative_scale_factor(self):
        """Test parsing with negative scale factor."""
        content = """Negative scale
-1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is not None
        assert data.scale_factor == -1.0

    def test_parse_invalid_scale_factor(self):
        """Test parsing with invalid scale factor."""
        content = """Invalid
not_a_number
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is None
        assert len(parser.get_errors()) > 0

    def test_parse_invalid_lattice_vector(self):
        """Test parsing with invalid lattice vector."""
        content = """Invalid lattice
1.0
1.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is None
        assert len(parser.get_errors()) > 0

    def test_parse_unknown_coordinate_type(self):
        """Test parsing with unknown coordinate type."""
        content = """Unknown
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Unknown
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is None
        assert len(parser.get_errors()) > 0

    def test_parse_missing_coordinates(self):
        """Test parsing with missing coordinates."""
        content = """Missing coords
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
2
Direct
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        data = parser.parse()
        
        assert data is None
        assert len(parser.get_errors()) > 0

    def test_get_errors(self):
        """Test get_errors method."""
        content = """Invalid
not_a_number
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        parser.parse()
        
        errors = parser.get_errors()
        assert isinstance(errors, list)
        assert len(errors) > 0
