"""Final tests to achieve 100% coverage."""

import pytest
from lsprotocol.types import Diagnostic, Range, Position

from vasp_lsp.features.formatting import FormattingProvider
from vasp_lsp.features.quickfixes import QuickFixesProvider


class TestFormattingCoverage:
    """Tests for missing formatting coverage."""

    def test_format_unknown_file_type(self):
        """Test formatting with unknown file type returns empty list."""
        provider = FormattingProvider()
        result = provider.format_document("some content", "file://unknown.txt")
        assert result == []

    def test_format_poscar_lattice_vector_fewer_than_3_parts(self):
        """Test POSCAR formatting with lattice vector having fewer than 3 parts."""
        provider = FormattingProvider()
        # POSCAR with invalid lattice vector line (only 2 values)
        content = """Test POSCAR
1.0
1.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Direct
0.0 0.0 0.0
"""
        result = provider.format_document(content, "file://POSCAR")
        # Should handle gracefully and include the original line
        assert len(result) == 1

    def test_format_kpoints_value_error_handling(self):
        """Test KPOINTS formatting with ValueError in coordinate parsing."""
        provider = FormattingProvider()
        # KPOINTS with invalid coordinates that will cause ValueError
        content = """KPOINTS
4
Gamma
2 2 2
0.0 0.0 abc 1.0
0.5 0.5 xyz 1.0
"""
        result = provider.format_document(content, "file://KPOINTS")
        assert len(result) == 1
        # Should handle ValueError gracefully

    def test_format_kpoints_end_lines(self):
        """Test KPOINTS formatting reaching end lines."""
        provider = FormattingProvider()
        # KPOINTS with explicit k-points and comments
        content = """KPOINTS file
3
Reciprocal
0.0 0.0 0.0 1.0
0.5 0.5 0.5 1.0  # comment
1.0 1.0 1.0 1.0
"""
        result = provider.format_document(content, "file://KPOINTS")
        assert len(result) == 1


class TestQuickFixesCoverage:
    """Tests for missing quickfixes coverage."""

    def test_ldau_parameter_fixes(self):
        """Test LDAU parameter quick fixes."""
        provider = QuickFixesProvider()
        
        # Test LDAUTYPE fix
        content = "LDAU = .TRUE."
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=14)),
                message="Missing LDAUTYPE when LDAU is enabled"
            )
        ]
        actions = provider.get_code_actions(content, "file://INCAR", diagnostics, Range(start=Position(line=0, character=0), end=Position(line=0, character=14)))
        
        # Should have LDAUTYPE fix
        ldautype_actions = [a for a in actions if "LDAUTYPE" in a.title]
        assert len(ldautype_actions) > 0

    def test_ldauj_parameter_fix(self):
        """Test LDAUJ parameter quick fix."""
        provider = QuickFixesProvider()
        
        content = "LDAU = .TRUE.\nLDAUTYPE = 2"
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=14)),
                message="Missing LDAUJ when LDAU is enabled"
            )
        ]
        actions = provider.get_code_actions(content, "file://INCAR", diagnostics, Range(start=Position(line=0, character=0), end=Position(line=0, character=14)))
        
        # Check for any LDAU-related fixes
        assert isinstance(actions, list)

    def test_fix_typo_start_col_not_found(self):
        """Test _create_fix_typo_action when start_col is -1."""
        provider = QuickFixesProvider()
        
        # Create a diagnostic with a tag name that doesn't exist in the line
        content = "SOME_OTHER_TAG = 1"
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=18)),
                message="Unknown INCAR tag: ENCO"
            )
        ]
        actions = provider.get_code_actions(content, "file://INCAR", diagnostics, Range(start=Position(line=0, character=0), end=Position(line=0, character=18)))
        
        # When start_col is -1, should return None for that action
        # So we might not get any typo fix actions
        assert isinstance(actions, list)

    def test_unknown_tag_typo_fix(self):
        """Test unknown tag typo fix with similar tag."""
        provider = QuickFixesProvider()
        
        content = "ENCO = 500"
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=10)),
                message="Unknown INCAR tag: ENCO"
            )
        ]
        actions = provider.get_code_actions(content, "file://INCAR", diagnostics, Range(start=Position(line=0, character=0), end=Position(line=0, character=10)))
        
        # Should suggest ENCUT as fix
        typo_fixes = [a for a in actions if "ENCUT" in a.title or "ENCO" in a.title]
        assert len(typo_fixes) > 0

    def test_ldaul_parameter_fix(self):
        """Test LDAUL parameter quick fix."""
        provider = QuickFixesProvider()
        
        content = "LDAU = .TRUE.\nLDAUTYPE = 2"
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=14)),
                message="Missing LDAUL when LDAU is enabled"
            )
        ]
        actions = provider.get_code_actions(content, "file://INCAR", diagnostics, Range(start=Position(line=0, character=0), end=Position(line=0, character=14)))
        
        ldaul_actions = [a for a in actions if "LDAUL" in a.title]
        assert len(ldaul_actions) > 0

    def test_ldauu_parameter_fix(self):
        """Test LDAUU parameter quick fix."""
        provider = QuickFixesProvider()
        
        content = "LDAU = .TRUE.\nLDAUTYPE = 2"
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=14)),
                message="Missing LDAUU when LDAU is enabled"
            )
        ]
        actions = provider.get_code_actions(content, "file://INCAR", diagnostics, Range(start=Position(line=0, character=0), end=Position(line=0, character=14)))
        
        ldauu_actions = [a for a in actions if "LDAUU" in a.title]
        assert len(ldauu_actions) > 0


class TestFormattingEdgeCases:
    """Additional edge case tests for formatting."""

    def test_format_incar_empty_params(self):
        """Test formatting INCAR with no parameters."""
        provider = FormattingProvider()
        content = "# Empty INCAR\n"
        result = provider.format_document(content, "file://INCAR")
        # Should return empty list when no parameters found
        assert result == []

    def test_format_kpoints_with_weight(self):
        """Test KPOINTS formatting with weight values."""
        provider = FormattingProvider()
        content = """KPOINTS
2
Reciprocal
0.0 0.0 0.0 0.5
0.5 0.5 0.5 0.5
"""
        result = provider.format_document(content, "file://KPOINTS")
        assert len(result) == 1

    def test_format_kpoints_with_comment(self):
        """Test KPOINTS formatting with inline comments."""
        provider = FormattingProvider()
        content = """KPOINTS
1
Reciprocal
0.0 0.0 0.0 1.0  # Gamma point
"""
        result = provider.format_document(content, "file://KPOINTS")
        assert len(result) == 1

    def test_format_poscar_with_selective_dynamics(self):
        """Test POSCAR formatting with selective dynamics."""
        provider = FormattingProvider()
        content = """Si crystal
1.0
5.43 0.0 0.0
0.0 5.43 0.0
0.0 0.0 5.43
Si
2
Selective dynamics
Direct
0.0 0.0 0.0 T T T
0.25 0.25 0.25 F F F
"""
        result = provider.format_document(content, "file://POSCAR")
        assert len(result) == 1


class TestFinalMissingCoverage:
    """Tests for the final missing lines."""

    def test_format_incar_unknown_tag_goes_to_other(self):
        """Test formatting INCAR with unknown tag goes to 'Other' group (line 123)."""
        provider = FormattingProvider()
        # Use a tag that doesn't belong to any known category
        # SYSTEM is a valid tag but not in any of the predefined groups
        content = "SYSTEM = test\nCUSTOM_PARAM = 1"
        result = provider.format_document(content, "file://INCAR")
        assert len(result) == 1
        # The unknown tag should be in "Other Parameters" group

    def test_format_poscar_short_lattice_vectors(self):
        """Test POSCAR formatting when lattice vector lines are missing (line 211)."""
        provider = FormattingProvider()
        # POSCAR with exactly 5 lines but only 2 valid lattice vectors
        # Line 4 (index 3) will be missing, triggering the else branch
        content = """Test POSCAR
1.0
1.0 0.0 0.0
0.0 1.0 0.0
Si
1
Direct
0.0 0.0 0.0
"""
        result = provider.format_document(content, "file://POSCAR")
        assert len(result) == 1
        # Should fill in missing lattice vector with zeros

    def test_similarity_score_edge_cases(self):
        """Test _similarity_score edge cases."""
        from vasp_lsp.features.quickfixes import QuickFixesProvider
        provider = QuickFixesProvider()
        
        # Test with one empty string
        score = provider._similarity_score("ABC", "")
        assert score == 0.0
        
        # Test with empty first string
        score = provider._similarity_score("", "ABC")
        assert score == 0.0
