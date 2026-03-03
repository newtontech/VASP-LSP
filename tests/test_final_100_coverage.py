"""Final tests to achieve 100% coverage."""

from unittest.mock import MagicMock

from lsprotocol.types import (
    CodeActionParams,
    Diagnostic,
    Position,
    Range,
    TextDocumentIdentifier,
)

from vasp_lsp.features.formatting import FormattingProvider
from vasp_lsp.features.quickfixes import QuickFixesProvider


class TestFinalFormattingCoverage:
    """Tests for remaining formatting.py coverage."""

    def test_incar_other_category(self):
        """Test INCAR with parameters in 'other' category (line 123)."""
        provider = FormattingProvider()
        # Use a tag that's not in any category
        content = "SOMEUNKNOWNTAG = value"
        result = provider._format_incar(content)
        assert len(result) == 1
        assert "Other Parameters" in result[0].new_text

    def test_poscar_missing_lattice_lines(self):
        """Test POSCAR with missing lattice lines (line 211)."""
        provider = FormattingProvider()
        # POSCAR with exactly 4 lines (shorter than 5)
        content = """Test POSCAR
   1.0
   1.0 0.0 0.0
   0.0 1.0 0.0"""
        result = provider._format_poscar(content)
        # Should return empty list if less than 5 lines
        assert result == []

    def test_kpoints_invalid_grid_parsing(self):
        """Test KPOINTS with invalid grid values (lines 346-348)."""
        provider = FormattingProvider()
        content = """KPOINTS
0
Gamma
not_a_number"""
        result = provider._format_kpoints(content)
        assert len(result) == 1

    def test_kpoints_invalid_six_value_grid(self):
        """Test KPOINTS with invalid six-value grid (lines 359-360)."""
        provider = FormattingProvider()
        content = """KPOINTS
0
Gamma
4 4 4 abc def"""
        result = provider._format_kpoints(content)
        assert len(result) == 1


class TestFinalQuickFixesCoverage:
    """Tests for remaining quickfixes.py coverage."""

    def test_similarity_score_both_empty(self):
        """Test _similarity_score with both empty strings (line 314)."""
        provider = QuickFixesProvider()
        # This should hit the early return for empty strings
        result = provider._similarity_score("", "")
        # Based on implementation: max_len=0, so it returns 0.0
        assert result == 0.0

    def test_fix_typo_returns_none_when_no_similar(self):
        """Test typo fix returns None when no similar tag (line 364)."""
        provider = QuickFixesProvider()
        lines = ["TOTALLYWRONGTAG = 123"]
        diagnostic = Diagnostic(
            range=Range(
                start=Position(line=0, character=0), end=Position(line=0, character=15)
            ),
            message="Unknown INCAR tag: TOTALLYWRONGTAG",
        )
        result = provider._create_fix_typo_action(
            lines, diagnostic, "TOTALLYWRONGTAG = 123"
        )
        # Should return None since no similar valid tag exists
        assert result is None


class TestFinalServerCoverage:
    """Tests for remaining server.py coverage."""

    def test_code_action_with_content(self):
        """Test code_action with valid content (lines 195-200)."""
        from vasp_lsp.server import code_action, server

        # Set up document content
        content = "ENCUT = 500\nISMEAR = 1"
        uri = "file:///test.INCAR"
        server.set_document_content(uri, content)
        server.set_document_diagnostics(uri, [])

        params = CodeActionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            range=Range(
                start=Position(line=0, character=0), end=Position(line=2, character=0)
            ),
            context=MagicMock(),
        )

        result = code_action(params)
        # Should return a list (possibly empty)
        assert result is not None or result == []
