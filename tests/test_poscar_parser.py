"""Tests for POSCAR parser - comprehensive coverage."""
import pytest
from vasp_lsp.parsers.poscar_parser import POSCARParser, POSCARData


class TestPoscarParserBasic:
    """Test basic POSCAR parsing functionality."""

    def test_parse_simple_poscar_vasp5(self):
        """Test parsing a simple VASP 5 format POSCAR."""
        content = """Si bulk
1.0
5.43 0.0 0.0
0.0 5.43 0.0
0.0 0.0 5.43
Si
2
Direct
0.0 0.0 0.0
0.25 0.25 0.25"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.system_comment == "Si bulk"
        assert result.scale_factor == 1.0
        assert len(result.lattice_vectors) == 3
        assert result.atom_types == ["Si"]
        assert result.atom_counts == [2]
        assert result.coordinate_type == "Direct"
        assert len(result.coordinates) == 2
        assert result.selective_dynamics is None

    def test_parse_poscar_vasp4(self):
        """Test parsing VASP 4 format POSCAR without atom types."""
        content = """Cubic BN
3.57
0.0 1.785 1.785
1.785 0.0 1.785
1.785 1.785 0.0
2
Direct
0.00 0.00 0.00
0.25 0.25 0.25"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.system_comment == "Cubic BN"
        assert result.scale_factor == 3.57
        assert result.atom_types == ["Type1"]  # Generic type
        assert result.atom_counts == [2]
        assert result.coordinate_type == "Direct"

    def test_parse_poscar_cartesian(self):
        """Test parsing POSCAR with Cartesian coordinates."""
        content = """Test structure
1.0
4.0 0.0 0.0
0.0 4.0 0.0
0.0 0.0 4.0
O H
1 2
Cartesian
0 0 0
0.5 0.5 0.5
1.0 1.0 1.0"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.coordinate_type == "Cartesian"
        assert result.atom_types == ["O", "H"]
        assert result.atom_counts == [1, 2]

    def test_parse_poscar_with_selective_dynamics(self):
        """Test parsing POSCAR with selective dynamics."""
        content = """Surface
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 10.0
Si
4
Selective dynamics
Direct
0.0 0.0 0.0 T T T
0.5 0.5 0.0 F F T
0.0 0.0 0.5 T T F
0.5 0.5 0.5 F F F"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.selective_dynamics is not None
        assert len(result.selective_dynamics) == 4
        assert result.selective_dynamics[0] == [True, True, True]
        assert result.selective_dynamics[1] == [False, False, True]

    def test_parse_poscar_get_errors(self):
        """Test that parser captures errors correctly."""
        parser = POSCARParser("")
        result = parser.parse()
        errors = parser.get_errors()
        assert isinstance(errors, list)


class TestPoscarParserErrors:
    """Test POSCAR parser error handling."""

    def test_empty_content(self):
        """Test error handling for empty content."""
        parser = POSCARParser("")
        result = parser.parse()
        assert result is None
        errors = parser.get_errors()
        assert len(errors) > 0

    def test_invalid_scale_factor(self):
        """Test error handling for invalid scale factor."""
        content = """Test
not_a_number
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Direct
0 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None
        errors = parser.get_errors()
        assert len(errors) > 0
        assert "scale factor" in errors[0]['message'].lower()

    def test_invalid_lattice_vector(self):
        """Test error handling for invalid lattice vector."""
        content = """Test
1.0
not_valid 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Direct
0 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None
        errors = parser.get_errors()
        assert len(errors) > 0

    def test_incomplete_lattice_vectors(self):
        """Test error handling for incomplete lattice vectors."""
        content = """Test
1.0
1.0 0.0"""

        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None

    def test_invalid_atom_counts(self):
        """Test error handling for invalid atom counts."""
        content = """Test
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
not_valid
Direct
0 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None
        errors = parser.get_errors()
        assert len(errors) > 0

    def test_unknown_coordinate_type(self):
        """Test error handling for unknown coordinate type."""
        content = """Test
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
UnknownType
0 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None
        errors = parser.get_errors()
        assert len(errors) > 0
        assert "coordinate" in errors[0]['message'].lower()

    def test_invalid_coordinates(self):
        """Test error handling for invalid coordinates."""
        content = """Test
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Direct
not_valid 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None
        errors = parser.get_errors()
        assert len(errors) > 0


class TestPoscarParserSpecialCases:
    """Test special cases and edge conditions."""

    def test_parse_with_frac_scale(self):
        """Test parsing with fractional scale factor."""
        content = """Test
0.5
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Direct
0 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.scale_factor == 0.5

    def test_parse_with_kartesian(self):
        """Test parsing with 'Kartesian' spelling (German)."""
        content = """Test
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Kartesian
0 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.coordinate_type == "Cartesian"

    def test_parse_selective_without_flags(self):
        """Test parsing selective dynamics without explicit flags."""
        content = """Test
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Selective dynamics
Direct
0 0 0"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.selective_dynamics is not None
        # Default should be all True
        assert result.selective_dynamics[0] == [True, True, True]

    def test_multiple_atom_types(self):
        """Test parsing with multiple atom types."""
        content = """Multi-atom system
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si O N C
1 2 3 4
Direct
0.0 0.0 0.0
0.1 0.1 0.1
0.2 0.2 0.2
0.3 0.3 0.3
0.4 0.4 0.4
0.5 0.5 0.5
0.6 0.6 0.6
0.7 0.7 0.7
0.8 0.8 0.8
0.9 0.9 0.9"""

        parser = POSCARParser(content)
        result = parser.parse()

        assert result is not None
        assert result.atom_types == ["Si", "O", "N", "C"]
        assert result.atom_counts == [1, 2, 3, 4]
        assert len(result.coordinates) == 10
