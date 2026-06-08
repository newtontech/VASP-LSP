"""Tests for expanded POSCAR diagnostics and quick fixes."""

from lsprotocol.types import Diagnostic, Position, Range

from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.features.quickfixes import QuickFixesProvider

# ---------------------------------------------------------------------------
# Helper: build a POSCAR string from parts
# ---------------------------------------------------------------------------


def _make_poscar(
    scale: str = "1.0",
    lattice: str = "1.0 0.0 0.0\n0.0 1.0 0.0\n0.0 0.0 1.0",
    atom_types: str = "H",
    atom_counts: str = "1",
    coord_type: str = "Direct",
    coordinates: str = "0.0 0.0 0.0",
    comment: str = "Test System",
) -> str:
    """Build a POSCAR content string from parts."""
    return (
        f"{comment}\n{scale}\n{lattice}\n{atom_types}\n{atom_counts}\n"
        f"{coord_type}\n{coordinates}\n"
    )


# ===================================================================
# New diagnostics
# ===================================================================


class TestDuplicateAtomPositions:
    """Test duplicate atom position detection."""

    def test_duplicate_direct_coordinates_warned(self) -> None:
        """Two atoms with identical Direct coordinates produce a warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="2",
            coordinates="0.25 0.25 0.25\n0.25 0.25 0.25",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("identical coordinates" in m for m in messages)

    def test_no_duplicate_no_warning(self) -> None:
        """Distinct coordinates produce no duplicate warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="2",
            coordinates="0.0 0.0 0.0\n0.5 0.5 0.5",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message for d in diags]
        assert not any("identical coordinates" in m for m in messages)

    def test_duplicate_within_tolerance(self) -> None:
        """Atoms closer than 1e-6 in each component are treated as duplicates."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="2",
            coordinates="0.5 0.5 0.5\n0.5 0.5 0.5000005",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("identical coordinates" in m for m in messages)


class TestScaleFactorMagnitude:
    """Test extreme scale factor magnitude detection."""

    def test_very_small_scale_factor_warned(self) -> None:
        """Scale factor |s| < 0.01 triggers a warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(scale="0.001")
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("extreme magnitude" in m for m in messages)

    def test_very_large_scale_factor_warned(self) -> None:
        """Scale factor |s| > 100 triggers a warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(scale="200.0")
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("extreme magnitude" in m for m in messages)

    def test_normal_scale_no_warning(self) -> None:
        """Normal scale factor (e.g. 1.0) produces no extreme magnitude warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(scale="1.0")
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message for d in diags]
        assert not any("extreme magnitude" in m for m in messages)

    def test_negative_extreme_both_warnings(self) -> None:
        """A negative and extreme scale factor triggers both negative and magnitude warnings."""
        provider = DiagnosticsProvider()
        content = _make_poscar(scale="-200.0")
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("negative scale factor" in m for m in messages)
        assert any("extreme magnitude" in m for m in messages)


class TestLatticeVectorAngles:
    """Test nearly degenerate lattice angle detection."""

    def test_nearly_parallel_vectors_warned(self) -> None:
        """Vectors a and b nearly parallel (<10 deg) trigger warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            lattice="1.0 0.0 0.0\n0.99 0.01 0.0\n0.0 0.0 1.0",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("nearly degenerate" in m for m in messages)

    def test_nearly_antiparallel_vectors_warned(self) -> None:
        """Vectors nearly antiparallel (>170 deg) trigger warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            lattice="1.0 0.0 0.0\n-0.99 0.01 0.0\n0.0 0.0 1.0",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("nearly degenerate" in m for m in messages)

    def test_normal_angles_no_warning(self) -> None:
        """Orthogonal lattice vectors produce no angle warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar()
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message for d in diags]
        assert not any("nearly degenerate" in m for m in messages)

    def test_sixty_degree_no_warning(self) -> None:
        """60 degree angle (hexagonal) produces no angle warning."""
        provider = DiagnosticsProvider()
        # a=(1,0,0), b=(0.5, sqrt(3)/2, 0) -> angle = 60 degrees
        import math

        s = math.sqrt(3) / 2
        content = _make_poscar(lattice=f"1.0 0.0 0.0\n0.5 {s:.6f} 0.0\n0.0 0.0 1.0")
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message for d in diags]
        assert not any("nearly degenerate" in m for m in messages)


class TestCoordinateCountMismatch:
    """Test coordinate count mismatch detection."""

    def test_too_few_coordinates_parse_error(self) -> None:
        """Fewer coordinate rows than atom counts triggers a parse error from the parser."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="3",
            coordinates="0.0 0.0 0.0\n0.5 0.5 0.5",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        # The parser itself detects the missing coordinate as a parse error
        assert any("invalid coordinate" in m or "coordinate" in m for m in messages)

    def test_matching_counts_no_error(self) -> None:
        """Matching coordinate count produces no mismatch error."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="2",
            coordinates="0.0 0.0 0.0\n0.5 0.5 0.5",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message for d in diags]
        assert not any("coordinate rows" in m for m in messages)

    def test_check_coordinate_count_method_directly(self) -> None:
        """Test _check_coordinate_count method detects mismatch on a mock result."""

        class MockResult:
            atom_counts = [2]
            coordinates = [[0.0, 0.0, 0.0]]
            coordinate_start_line = 8

        provider = DiagnosticsProvider()
        diags = provider._check_coordinate_count(MockResult())
        assert len(diags) == 1
        assert "expected 2 coordinate rows but found 1" in diags[0].message.lower()


class TestVeryCloseAtoms:
    """Test very close atom detection (< 0.5 Angstrom)."""

    def test_close_cartesian_atoms_warned(self) -> None:
        """Cartesian atoms closer than 0.5 A trigger a warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="2",
            coord_type="Cartesian",
            coordinates="0.0 0.0 0.0\n0.3 0.0 0.0",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("angstrom apart" in m for m in messages)

    def test_close_direct_atoms_warned(self) -> None:
        """Direct atoms closer than 0.5 A (scaled) trigger a warning."""
        provider = DiagnosticsProvider()
        # Lattice vectors are 1 A each; atoms at 0.0 and 0.3 -> 0.3 A apart
        content = _make_poscar(
            atom_counts="2",
            coord_type="Direct",
            coordinates="0.0 0.0 0.0\n0.3 0.0 0.0",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message.lower() for d in diags]
        assert any("angstrom apart" in m for m in messages)

    def test_well_separated_no_warning(self) -> None:
        """Atoms more than 0.5 A apart produce no close-atom warning."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="2",
            coord_type="Cartesian",
            coordinates="0.0 0.0 0.0\n2.0 0.0 0.0",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        messages = [d.message for d in diags]
        assert not any("angstrom apart" in m for m in messages)


class TestValidPoscarNoNewWarnings:
    """Valid POSCAR should not trigger any of the new diagnostics."""

    def test_clean_poscar(self) -> None:
        """A well-formed POSCAR produces zero diagnostics."""
        provider = DiagnosticsProvider()
        content = _make_poscar(
            atom_counts="2",
            coordinates="0.0 0.0 0.0\n0.5 0.5 0.5",
        )
        diags = provider.get_diagnostics(content, "file:///test/POSCAR")
        assert diags == []


# ===================================================================
# New quick fixes
# ===================================================================


class TestFixExtremeScaleQuickFix:
    """Test quick fix for extreme scale factor."""

    def test_extreme_scale_fix_available(self) -> None:
        """Extreme magnitude diagnostic provides a 'replace with 1.0' fix."""
        provider = QuickFixesProvider()
        content = _make_poscar(scale="200.0")
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=20)),
            message="Scale factor 200 has extreme magnitude (|scale| = 200). Consider using 1.0.",
        )
        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)
        assert any("replace extreme scale factor" in a.title.lower() for a in actions)

    def test_extreme_scale_fix_replaces_with_one(self) -> None:
        """The fix replaces scale factor line with '1.0'."""
        provider = QuickFixesProvider()
        content = _make_poscar(scale="0.001")
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=20)),
            message="Scale factor 0.001 has extreme magnitude. Consider using 1.0.",
        )
        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)
        fix = next(a for a in actions if "replace extreme" in a.title.lower())
        edit = fix.edit.changes["document"][0]
        assert edit.new_text == "1.0"


class TestRemoveDuplicateAtomsQuickFix:
    """Test quick fix for removing duplicate atoms."""

    def test_duplicate_removal_fix_available(self) -> None:
        """Duplicate coordinates diagnostic provides a removal fix."""
        provider = QuickFixesProvider()
        content = _make_poscar(
            atom_counts="2",
            coordinates="0.25 0.25 0.25\n0.25 0.25 0.25",
        )
        diagnostic = Diagnostic(
            range=Range(start=Position(line=8, character=0), end=Position(line=8, character=40)),
            message="Atoms 1 and 2 have identical coordinates.",
        )
        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)
        assert any("remove" in a.title.lower() and "duplicate" in a.title.lower() for a in actions)

    def test_duplicate_removal_fix_has_edits(self) -> None:
        """The removal fix includes text edits that delete duplicate lines."""
        provider = QuickFixesProvider()
        content = _make_poscar(
            atom_counts="3",
            coordinates="0.25 0.25 0.25\n0.5 0.5 0.5\n0.25 0.25 0.25",
        )
        diagnostic = Diagnostic(
            range=Range(start=Position(line=10, character=0), end=Position(line=10, character=40)),
            message="Atoms 1 and 3 have identical coordinates.",
        )
        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)
        fix = next(
            a for a in actions if "remove" in a.title.lower() and "duplicate" in a.title.lower()
        )
        assert fix.edit is not None
        assert len(fix.edit.changes["document"]) >= 1

    def test_no_duplicates_no_fix(self) -> None:
        """When no duplicates exist, the fix action is not created."""
        provider = QuickFixesProvider()
        content = _make_poscar(
            atom_counts="2",
            coordinates="0.0 0.0 0.0\n0.5 0.5 0.5",
        )
        diagnostic = Diagnostic(
            range=Range(start=Position(line=8, character=0), end=Position(line=8, character=40)),
            message="Atoms 1 and 2 have identical coordinates.",
        )
        # The diagnostic message says identical, but the content has no duplicates
        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)
        dup_actions = [a for a in actions if "duplicate" in a.title.lower()]
        assert dup_actions == []


class TestNoNewQuickFixesForCleanPoscar:
    """No new quick fix actions for a clean POSCAR."""

    def test_clean_poscar_no_new_actions(self) -> None:
        """A clean POSCAR with empty diagnostics list produces no actions."""
        provider = QuickFixesProvider()
        content = _make_poscar()
        actions = provider._get_poscar_code_actions(
            content,
            [],
            Range(start=Position(line=0, character=0), end=Position(line=0, character=0)),
        )
        assert actions == []
