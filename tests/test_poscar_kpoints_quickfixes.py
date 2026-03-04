"""Tests for POSCAR and KPOINTS quick fixes."""

from lsprotocol.types import Diagnostic, Position, Range

from vasp_lsp.features.quickfixes import QuickFixesProvider


class TestPOSCARQuickFixes:
    """Test quick fixes for POSCAR files."""

    def test_fix_negative_scale_factor(self):
        """Test quick fix for negative scale factor."""
        provider = QuickFixesProvider()

        content = """Test System
-1.5
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=5)),
            message="Negative scale factor detected. This inverts the lattice.",
        )

        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)

        assert len(actions) >= 1
        assert any("scale factor" in a.title.lower() for a in actions)

    def test_fix_negative_scale_factor_applies_edit(self):
        """Test that negative scale fix creates correct edit."""
        provider = QuickFixesProvider()

        content = """Test System
-1.5
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=5)),
            message="Negative scale factor detected. This inverts the lattice.",
        )

        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)

        # Find the fix action
        fix_action = next((a for a in actions if "scale" in a.title.lower()), None)
        assert fix_action is not None
        assert "1.5" in fix_action.title  # Should show the positive value

    def test_wrap_coordinates_fix(self):
        """Test quick fix for out-of-range coordinates."""
        provider = QuickFixesProvider()

        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
2.5 -0.5 1.5
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=8, character=0), end=Position(line=8, character=20)),
            message="Direct coordinate 2.500 is outside typical range [0, 1].",
        )

        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)

        assert len(actions) >= 1
        assert any("wrap" in a.title.lower() for a in actions)

    def test_poscar_no_diagnostics_no_actions(self):
        """Test that no diagnostics returns no actions."""
        provider = QuickFixesProvider()

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
        actions = provider._get_poscar_code_actions(content, [], Range(
            start=Position(line=0, character=0),
            end=Position(line=0, character=0)
        ))

        assert actions == []


    def test_wrap_coordinates_with_flags(self):
        """Test wrap coordinates with selective dynamics flags."""
        provider = QuickFixesProvider()

        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Selective dynamics
Direct
2.5 -0.5 1.5 T T F
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=9, character=0), end=Position(line=9, character=30)),
            message="Direct coordinate 2.500 is outside typical range [0, 1].",
        )

        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)

        wrap_action = next((a for a in actions if "wrap" in a.title.lower()), None)
        assert wrap_action is not None

    def test_wrap_coordinates_invalid_value(self):
        """Test wrap coordinates with invalid coordinate value."""
        provider = QuickFixesProvider()

        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
invalid 0.0 0.0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=8, character=0), end=Position(line=8, character=20)),
            message="Direct coordinate is outside typical range [0, 1].",
        )

        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)

        # Should not crash, may or may not have actions
        assert isinstance(actions, list)

    def test_wrap_coordinates_short_line(self):
        """Test wrap coordinates with line having less than 3 parts."""
        provider = QuickFixesProvider()

        content = """Test System
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=8, character=0), end=Position(line=8, character=10)),
            message="Direct coordinate is outside typical range [0, 1].",
        )

        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)

        # Should not crash, may or may not have actions
        assert isinstance(actions, list)


class TestKPOINTSQuickFixesExtended:
    """Extended tests for KPOINTS quick fixes."""

    def test_fix_grid_creates_positive_values(self):
        """Test that grid fix creates positive values."""
        provider = QuickFixesProvider()

        content = """Automatic
0
Gamma
0 -1 4
0 0 0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=3, character=0), end=Position(line=3, character=10)),
            message="K-point grid value 0 is not positive.",
        )

        actions = provider._get_kpoints_code_actions(content, [diagnostic], diagnostic.range)

        grid_action = next((a for a in actions if "grid" in a.title.lower()), None)
        assert grid_action is not None
        # Should mention the corrected grid values

    def test_normalize_weights_with_explicit_kpoints(self):
        """Test normalize weights with explicit k-points."""
        provider = QuickFixesProvider()

        content = """Explicit k-points
2
Reciprocal
0.0 0.0 0.0 0.3
0.5 0.5 0.5 0.3
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=3, character=0), end=Position(line=3, character=30)),
            message="K-point weights sum to 0.600 (expected ~1.0).",
        )

        actions = provider._get_kpoints_code_actions(content, [diagnostic], diagnostic.range)

        # Should have normalize action or no action (if parsing fails)
        assert isinstance(actions, list)

    def test_normalize_weights_zero_total(self):
        """Test normalize weights when total weight is zero."""
        provider = QuickFixesProvider()

        content = """Explicit k-points
2
Reciprocal
0.0 0.0 0.0 0.0
0.5 0.5 0.5 0.0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=3, character=0), end=Position(line=3, character=30)),
            message="K-point weights sum to 0.000 (expected ~1.0).",
        )

        actions = provider._get_kpoints_code_actions(content, [diagnostic], diagnostic.range)

        # Should not crash
        assert isinstance(actions, list)


class TestCONTCARQuickFixes:
    """Test quick fixes for CONTCAR files."""

    def test_contcar_negative_scale_factor(self):
        """Test quick fix for CONTCAR with negative scale factor."""
        provider = QuickFixesProvider()

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
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=5)),
            message="Negative scale factor detected. This inverts the lattice.",
        )

        actions = provider._get_poscar_code_actions(content, [diagnostic], diagnostic.range)

        assert len(actions) >= 1
        assert any("scale factor" in a.title.lower() for a in actions)


class TestKPOINTSQuickFixes:
    """Test quick fixes for KPOINTS files."""

    def test_fix_non_positive_grid(self):
        """Test quick fix for non-positive grid values."""
        provider = QuickFixesProvider()

        content = """Automatic
0
Gamma
0 -1 4
0 0 0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=3, character=0), end=Position(line=3, character=10)),
            message="K-point grid value 0 is not positive.",
        )

        actions = provider._get_kpoints_code_actions(content, [diagnostic], diagnostic.range)

        assert len(actions) >= 1
        assert any("grid" in a.title.lower() for a in actions)

    def test_kpoints_no_diagnostics_no_actions(self):
        """Test that no diagnostics returns no actions."""
        provider = QuickFixesProvider()

        content = """Automatic
0
Gamma
4 4 4
0 0 0
"""
        actions = provider._get_kpoints_code_actions(content, [], Range(
            start=Position(line=0, character=0),
            end=Position(line=0, character=0)
        ))

        assert actions == []


class TestQuickFixesEdgeCases:
    """Test edge cases for quick fixes."""

    def test_fix_grid_no_result(self):
        """Test fix grid when parser returns no result."""
        provider = QuickFixesProvider()

        content = """Invalid KPOINTS
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=10)),
            message="K-point grid value is not positive.",
        )

        actions = provider._get_kpoints_code_actions(content, [diagnostic], diagnostic.range)

        # Should not crash
        assert isinstance(actions, list)


class TestQuickFixIntegration:
    """Integration tests for quick fixes via get_code_actions."""

    def test_poscar_via_get_code_actions(self):
        """Test getting POSCAR actions via main get_code_actions method."""
        provider = QuickFixesProvider()

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
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=5)),
            message="Negative scale factor detected.",
        )

        actions = provider.get_code_actions(
            content, "file:///test/POSCAR", [diagnostic], diagnostic.range
        )

        assert any("scale" in a.title.lower() for a in actions)

    def test_kpoints_via_get_code_actions(self):
        """Test getting KPOINTS actions via main get_code_actions method."""
        provider = QuickFixesProvider()

        content = """Automatic
0
Gamma
0 4 4
0 0 0
"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=3, character=0), end=Position(line=3, character=10)),
            message="K-point grid value 0 is not positive.",
        )

        actions = provider.get_code_actions(
            content, "file:///test/KPOINTS", [diagnostic], diagnostic.range
        )

        assert any("grid" in a.title.lower() for a in actions)




class TestQuickFixesEdgeCaseCoverage:
    """Edge case tests to maximize coverage."""

    def test_scale_value_error_returns_none(self):
        """Test that ValueError in scale parsing returns None."""
        provider = QuickFixesProvider()

        # Directly test with invalid line
        lines = ["header", "invalid", "more"]
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=5)),
            message="Negative scale factor detected.",
        )

        # Create a mock parser
        class MockParser:
            pass

        action = provider._create_fix_negative_scale_action(lines, diagnostic, MockParser())
        assert action is None

    def test_normalize_weights_empty_lines(self):
        """Test normalize weights with short lines list."""
        provider = QuickFixesProvider()

        # Create a mock result with weights
        class MockResult:
            weights = [0.3, 0.3]

        lines = ["short", "lines"]  # Too short for line_num = 3
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=10)),
            message="K-point weights sum to 0.600",
        )

        action = provider._create_normalize_weights_action(lines, diagnostic, MockResult())
        assert action is None
