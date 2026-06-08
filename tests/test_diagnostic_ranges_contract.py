"""Tests for exact source spans and edge-case POSCAR/KPOINTS diagnostics.

Validates that:
  - POSCAR diagnostics use exact source spans from the parser.
  - KPOINTS diagnostics use exact grid/kpoint line positions.
  - Edge cases (selective dynamics, Cartesian coords, line-mode, explicit weights) work.
"""

from lsprotocol.types import DiagnosticSeverity

from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
from vasp_lsp.parsers.poscar_parser import (
    POSCARParser,  # noqa: I001 -- ruff wants these grouped without blank line but blank line between third-party and first-party is intentional
)

# ---------------------------------------------------------------------------
# POSCAR exact source span tests
# ---------------------------------------------------------------------------


class TestPOSCARExactSourceSpans:
    """Verify that POSCAR diagnostics use exact line positions from the parser."""

    def test_negative_scale_factor_targets_scale_line(self) -> None:
        """Negative scale factor diagnostic targets the exact scale factor line."""
        content = """Test System
-1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        neg = [d for d in diags if "negative scale factor" in d.message.lower()]
        assert len(neg) >= 1
        # Scale factor is on line index 1 (0-based)
        assert neg[0].range.start.line == 1

    def test_extreme_scale_targets_scale_line(self) -> None:
        """Extreme scale factor diagnostic targets the scale factor line."""
        content = """Test System
200.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        extreme = [d for d in diags if "extreme magnitude" in d.message.lower()]
        assert len(extreme) >= 1
        assert extreme[0].range.start.line == 1

    def test_zero_atoms_targets_atom_counts_line(self) -> None:
        """Zero atom count diagnostic targets the atom counts line."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
0
Direct
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        zero = [d for d in diags if "no atoms" in d.message.lower()]
        assert len(zero) >= 1
        # Atom counts are on line index 6 (0-based: line 7 in file)
        assert zero[0].range.start.line == 6

    def test_direct_coordinate_out_of_range_targets_exact_row(self) -> None:
        """Out-of-range direct coordinate diagnostic targets the exact coordinate row."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
2
Direct
0.5 0.5 0.5
2.5 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        oor = [d for d in diags if "outside typical range" in d.message.lower()]
        assert len(oor) >= 1
        # The second atom (index 1) is on line 9 (0-based: 8)
        assert oor[0].range.start.line == 9

    def test_lattice_zero_vector_targets_exact_line(self) -> None:
        """Zero-length lattice vector targets the exact vector line."""
        content = """Test System
1.0
0.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        zero = [d for d in diags if "lattice vector length is zero" in d.message.lower()]
        assert len(zero) >= 1
        # First lattice vector is on line index 2
        assert zero[0].range.start.line == 2


# ---------------------------------------------------------------------------
# POSCAR edge cases
# ---------------------------------------------------------------------------


class TestPOSCARSelectiveDynamics:
    """Test POSCAR with selective dynamics."""

    def test_selective_dynamics_valid(self) -> None:
        """Valid POSCAR with selective dynamics produces no errors."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
2
Selective dynamics
Direct
0.0 0.0 0.0 T T T
0.5 0.5 0.5 F F F
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        errors = [d for d in diags if d.severity == DiagnosticSeverity.Error]
        assert errors == []

    def test_selective_dynamics_invalid_flags(self) -> None:
        """Invalid selective dynamics flags produce an error."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Selective dynamics
Direct
0.0 0.0 0.0 X Y Z
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        assert any("selective dynamics" in d.message.lower() for d in diags)

    def test_selective_dynamics_short_form(self) -> None:
        """Short form 'S' is accepted for selective dynamics."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
S
Direct
0.0 0.0 0.0 T T T
"""
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is not None
        assert result.has_selective_dynamics is True
        assert result.selective_dynamics is not None

    def test_selective_dynamics_offsets_coordinates(self) -> None:
        """Selective dynamics shifts coordinate_start_line by 1."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Selective dynamics
Direct
0.0 0.0 0.0 T T T
"""
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is not None
        # With selective dynamics: coordinate_start_line = 10 (1-based)
        # The S line shifts coordinate data one line further down.
        assert result.coordinate_start_line == 10

    def test_selective_dynamics_direct_coordinate_out_of_range(self) -> None:
        """Out-of-range coordinate with selective dynamics targets correct line."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Selective dynamics
Direct
2.5 0.0 0.0 T T T
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        oor = [d for d in diags if "outside typical range" in d.message.lower()]
        assert len(oor) >= 1
        # Coordinate row is on line index 9 (0-based)
        assert oor[0].range.start.line == 9


class TestPOSCARCartesianCoordinates:
    """Test POSCAR with Cartesian coordinates."""

    def test_cartesian_no_direct_range_check(self) -> None:
        """Cartesian coordinates are not checked against [0, 1] range."""
        content = """Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
H
1
Cartesian
2.5 2.5 2.5
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        oor = [d for d in diags if "outside typical range" in d.message.lower()]
        assert oor == []

    def test_cartesian_close_atoms_checked(self) -> None:
        """Close atom distance check still applies to Cartesian coordinates."""
        content = """Test System
1.0
10.0 0.0 0.0
0.0 10.0 0.0
0.0 0.0 10.0
H
2
Cartesian
0.0 0.0 0.0
0.3 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        close = [d for d in diags if "angstrom apart" in d.message.lower()]
        assert len(close) >= 1

    def test_cartesian_keyword_k(self) -> None:
        """'K' prefix is accepted as Cartesian."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Kartesian
0.5 0.5 0.5
"""
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is not None
        assert result.coordinate_type == "Cartesian"


class TestPOSCARVASP4Format:
    """Test POSCAR without explicit atom types (VASP 4 format)."""

    def test_vasp4_no_atom_types(self) -> None:
        """VASP 4 format (no atom types) parses correctly."""
        content = """Test System
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
        result = parser.parse()
        assert result is not None
        assert len(result.atom_types) == 1  # One generic type
        assert result.atom_types[0].startswith("Type")

    def test_vasp4_multiple_species(self) -> None:
        """VASP 4 format with multiple species counts."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
1 1
Direct
0.0 0.0 0.0
0.5 0.5 0.5
"""
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is not None
        assert len(result.atom_counts) == 2
        assert sum(result.atom_counts) == 2


# ---------------------------------------------------------------------------
# KPOINTS exact source span tests
# ---------------------------------------------------------------------------


class TestKPOINTSExactSourceSpans:
    """Verify that KPOINTS diagnostics use exact grid/kpoint line positions."""

    def test_grid_warning_targets_grid_line_automatic(self) -> None:
        """Sparse grid warning targets the actual grid line in automatic mode."""
        content = """Automatic
0
Gamma
1 1 1
0 0 0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        sparse = [d for d in diags if "very sparse" in d.message.lower()]
        assert len(sparse) >= 1
        # Grid line is line index 3 (0-based)
        assert sparse[0].range.start.line == 3

    def test_grid_error_targets_grid_line_gamma(self) -> None:
        """Non-positive grid error targets the grid line in Gamma mode."""
        content = """Gamma mesh
0
Gamma
0 4 4
0 0 0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        pos = [d for d in diags if "not positive" in d.message.lower()]
        assert len(pos) >= 1
        # Grid line is line index 3 (0-based)
        assert pos[0].range.start.line == 3

    def test_dense_grid_targets_grid_line(self) -> None:
        """Dense grid warning targets the actual grid line."""
        content = """Dense grid
0
Gamma
60 60 60
0 0 0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        dense = [d for d in diags if "very dense" in d.message.lower()]
        assert len(dense) >= 1
        assert dense[0].range.start.line == 3

    def test_explicit_zero_weight_targets_exact_row(self) -> None:
        """Zero-weight k-point warning targets the exact k-point row."""
        content = """Explicit k-points
3
Reciprocal
0.0 0.0 0.0 1.0
0.5 0.0 0.0 0.0
0.0 0.5 0.0 0.5
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        zero_w = [d for d in diags if "weight 0.0" in d.message.lower()]
        assert len(zero_w) >= 1
        # The zero-weight k-point (index 1) is at start_line + 1 = 3 + 1 = 4
        assert zero_w[0].range.start.line == 4

    def test_explicit_out_of_range_targets_exact_row(self) -> None:
        """Out-of-range coordinate error targets the exact k-point row."""
        content = """Explicit k-points
2
Reciprocal
0.0 0.0 0.0 0.5
1.5 0.0 0.0 0.5
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        oor = [d for d in diags if "outside the typical reciprocal range" in d.message]
        assert len(oor) >= 1
        # The out-of-range k-point (index 1) is at start_line + 1 = 3 + 1 = 4
        assert oor[0].range.start.line == 4


# ---------------------------------------------------------------------------
# KPOINTS edge cases
# ---------------------------------------------------------------------------


class TestKPOINTSAutomaticMode:
    """Test fully automatic KPOINTS mode."""

    def test_automatic_a_prefix(self) -> None:
        """'a' on line 2 triggers automatic mode."""
        content = """Automatic mesh
a
4 4 4
0 0 0
"""
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is not None
        assert result.mode.value == "automatic"
        assert result.grid == [4, 4, 4]

    def test_automatic_full_word(self) -> None:
        """'Automatic' on line 2 triggers automatic mode."""
        content = """Auto mesh
Automatic
4 4 4
0 0 0
"""
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is not None
        assert result.mode.value == "automatic"

    def test_automatic_with_shift(self) -> None:
        """Automatic mode parses shift values."""
        content = """Auto mesh
a
4 4 4
0.5 0.5 0.5
"""
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is not None
        assert result.shift == [0.5, 0.5, 0.5]


class TestKPOINTSExplicitWeights:
    """Test explicit k-point mode with various weight scenarios."""

    def test_weights_sum_to_one(self) -> None:
        """Weights summing to 1.0 produce no weight-sum info."""
        content = """Explicit
2
Reciprocal
0.0 0.0 0.0 0.5
0.5 0.5 0.5 0.5
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        sums = [d for d in diags if "sum" in d.message.lower()]
        assert sums == []

    def test_weights_sum_not_one(self) -> None:
        """Weights not summing to 1.0 produce an info diagnostic."""
        content = """Explicit
2
Reciprocal
0.0 0.0 0.0 0.3
0.5 0.5 0.5 0.3
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        sums = [d for d in diags if "sum" in d.message.lower()]
        assert len(sums) >= 1


class TestKPOINTSLineMode:
    """Test line-mode KPOINTS for band structures."""

    def test_line_mode_low_density(self) -> None:
        """Low line-mode density triggers info diagnostic."""
        content = """Line mode
5
Line
Reciprocal
0.0 0.0 0.0
0.5 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        density = [d for d in diags if "line-mode density" in d.message.lower()]
        assert len(density) >= 1

    def test_line_mode_high_density_no_warning(self) -> None:
        """High line-mode density produces no density warning."""
        content = """Line mode
20
Line
Reciprocal
0.0 0.0 0.0
0.5 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        density = [d for d in diags if "line-mode density" in d.message.lower()]
        assert density == []

    def test_line_mode_parse(self) -> None:
        """Line mode parses k-point endpoints."""
        content = """Band structure
20
Line
Reciprocal
0.0 0.0 0.0
0.5 0.0 0.0
0.5 0.5 0.0
0.0 0.5 0.0
"""
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is not None
        assert result.mode.value == "line"
        assert len(result.kpoints) == 4
        assert result.line_density == 20


class TestKPOINTSMonkhorstPack:
    """Test Monkhorst-Pack mode."""

    def test_monkhorst_pack_parse(self) -> None:
        """Monkhorst-Pack mode parses grid and shift."""
        content = """MP mesh
0
Monkhorst
6 6 6
0.0 0.0 0.0
"""
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is not None
        assert result.mode.value == "gamma_monkhorst"
        assert result.grid == [6, 6, 6]

    def test_monkhorst_pack_hint(self) -> None:
        """Monkhorst-Pack mode produces a centering hint."""
        content = """MP mesh
0
Monkhorst
6 6 6
0 0 0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        mp = [d for d in diags if "Monkhorst-Pack" in d.message]
        assert len(mp) >= 1


# ---------------------------------------------------------------------------
# Regression: existing diagnostics still work
# ---------------------------------------------------------------------------


class TestExistingPOSCARDiagnosticsStillWork:
    """Regression tests for existing POSCAR diagnostics after span changes."""

    def test_valid_poscar_no_diagnostics(self) -> None:
        """A valid POSCAR produces no diagnostics."""
        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.5 0.5 0.5
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        assert diags == []

    def test_parse_error_still_reported(self) -> None:
        """Parse errors are still reported with span changes."""
        content = """Test System
invalid
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        assert any("scale factor" in d.message.lower() for d in diags)


class TestExistingKPOINTSDiagnosticsStillWork:
    """Regression tests for existing KPOINTS diagnostics after span changes."""

    def test_valid_kpoints_only_info(self) -> None:
        """A valid KPOINTS produces at most informational hints."""
        content = """Gamma mesh
0
Gamma
4 4 4
0 0 0
"""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(content, "file:///test/KPOINTS")
        non_info = [d for d in diags if d.severity != DiagnosticSeverity.Information]
        assert non_info == []
