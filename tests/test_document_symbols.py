"""Tests for the document symbols feature (navigation) in VASP-LSP.

TDD: write tests first, then implement the document symbols provider
to make them pass.
"""

from vasp_lsp.features.navigation import (
    DocumentSymbolsProvider,
    get_document_symbols,
)

# ---------------------------------------------------------------------------
# INCAR document symbols
# ---------------------------------------------------------------------------


class TestINCARDocumentSymbols:
    """Document symbols for INCAR files should expose every parameter."""

    def test_basic_incar_symbols(self):
        content = "ENCUT = 520\nISMEAR = 0\nSIGMA = 0.05\n"
        symbols = get_document_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 3
        names = [s.name for s in symbols]
        assert "ENCUT" in names
        assert "ISMEAR" in names
        assert "SIGMA" in names

    def test_incar_symbol_has_range(self):
        content = "ENCUT = 520\n"
        symbols = get_document_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 1
        sym = symbols[0]
        # Range should cover the line
        assert sym.range.start.line == 0
        assert sym.range.start.character == 0
        assert sym.range.end.line == 0

    def test_incar_skips_comments(self):
        content = "# comment\nENCUT = 520\n"
        symbols = get_document_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 1
        assert symbols[0].name == "ENCUT"

    def test_incar_skips_empty_lines(self):
        content = "\n\nENCUT = 520\n\n"
        symbols = get_document_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 1

    def test_incar_duplicate_parameter(self):
        """Duplicate parameters should each appear as symbols."""
        content = "ENCUT = 400\nENCUT = 520\n"
        symbols = get_document_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 2

    def test_incar_parameter_with_inline_comment(self):
        content = "ENCUT = 520  # cutoff energy\n"
        symbols = get_document_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 1
        assert symbols[0].name == "ENCUT"

    def test_incar_case_insensitive(self):
        content = "encut = 520\n"
        symbols = get_document_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 1
        assert symbols[0].name == "ENCUT"


# ---------------------------------------------------------------------------
# POSCAR document symbols
# ---------------------------------------------------------------------------


class TestPOSCARDocumentSymbols:
    """Document symbols for POSCAR files should expose structural sections."""

    def test_poscar_basic_symbols(self):
        content = (
            "Si bulk\n"
            "1.0\n"
            "5.43 0.0 0.0\n"
            "0.0 5.43 0.0\n"
            "0.0 0.0 5.43\n"
            "Si\n"
            "2\n"
            "Direct\n"
            "0.0 0.0 0.0\n"
            "0.25 0.25 0.25\n"
        )
        symbols = get_document_symbols(content, "file:///work/POSCAR")
        names = [s.name for s in symbols]
        assert "System Comment" in names
        assert "Scale Factor" in names
        assert "Lattice" in names or any("Lattice" in n for n in names)
        assert "Atom Types" in names
        assert "Coordinates" in names or any("Coord" in n for n in names)

    def test_poscar_scale_factor_symbol_range(self):
        content = "Si\n1.0\n0.0 0.0 0.0\n0.0 0.0 0.0\n0.0 0.0 0.0\n" "Si\n1\nDirect\n0.0 0.0 0.0\n"
        symbols = get_document_symbols(content, "file:///work/POSCAR")
        scale = next(s for s in symbols if s.name == "Scale Factor")
        assert scale.range.start.line == 1


# ---------------------------------------------------------------------------
# KPOINTS document symbols
# ---------------------------------------------------------------------------


class TestKPOINTSdocumentSymbols:
    """Document symbols for KPOINTS files."""

    def test_gamma_kpoints_symbols(self):
        content = "Gamma grid\n" "0\n" "Gamma\n" "4 4 4\n" "0 0 0\n"
        symbols = get_document_symbols(content, "file:///work/KPOINTS")
        names = [s.name for s in symbols]
        assert "Comment" in names
        assert "Mode" in names or "Generation" in names
        assert "Grid" in names

    def test_line_mode_symbols(self):
        content = (
            "Band structure\n"
            "Line-mode\n"
            "reciprocal\n"
            "20\n"
            "0.0 0.0 0.0 GAMMA\n"
            "0.5 0.0 0.5 X\n"
        )
        symbols = get_document_symbols(content, "file:///work/KPOINTS")
        assert len(symbols) >= 2


# ---------------------------------------------------------------------------
# Unknown file type
# ---------------------------------------------------------------------------


class TestUnknownFileTypeSymbols:
    def test_unknown_returns_empty(self):
        symbols = get_document_symbols("some content", "file:///work/unknown.txt")
        assert symbols == []


# ---------------------------------------------------------------------------
# Provider class
# ---------------------------------------------------------------------------


class TestDocumentSymbolsProvider:
    def test_provider_get_symbols_incar(self):
        provider = DocumentSymbolsProvider()
        content = "ENCUT = 520\nISMEAR = 0\n"
        symbols = provider.get_symbols(content, "file:///work/INCAR")
        assert len(symbols) == 2

    def test_provider_get_symbols_unknown(self):
        provider = DocumentSymbolsProvider()
        symbols = provider.get_symbols("text", "file:///work/unknown.txt")
        assert symbols == []

    def test_empty_string_returns_empty(self):
        provider = DocumentSymbolsProvider()
        symbols = provider.get_symbols("", "file:///work/INCAR")
        assert symbols == []

    def test_poscar_empty_returns_empty(self):
        provider = DocumentSymbolsProvider()
        symbols = provider.get_symbols("", "file:///work/POSCAR")
        assert symbols == []

    def test_poscar_minimal(self):
        """POSCAR with minimal lines (fewer than 5)."""
        provider = DocumentSymbolsProvider()
        content = "Si\n"
        symbols = provider.get_symbols(content, "file:///work/POSCAR")
        # Should return System Comment at minimum
        assert any(s.name == "System Comment" for s in symbols)

    def test_poscar_empty_comment_line(self):
        """POSCAR with empty first line."""
        provider = DocumentSymbolsProvider()
        content = "\n1.0\n1.0 0.0 0.0\n0.0 1.0 0.0\n0.0 0.0 1.0\n" "Si\n1\nDirect\n0.0 0.0 0.0\n"
        symbols = provider.get_symbols(content, "file:///work/POSCAR")
        # Should still produce symbols but not System Comment
        names = [s.name for s in symbols]
        assert "Scale Factor" in names

    def test_poscar_non_numeric_coordinate_break(self):
        """POSCAR where a coordinate line contains non-numeric data."""
        provider = DocumentSymbolsProvider()
        content = (
            "Si\n1.0\n1.0 0.0 0.0\n0.0 1.0 0.0\n0.0 0.0 1.0\n"
            "Si\n2\nDirect\n0.0 0.0 0.0\nnot_numeric\n"
        )
        symbols = provider.get_symbols(content, "file:///work/POSCAR")
        # Should still produce Coordinates symbol for first valid line
        names = [s.name for s in symbols]
        assert "Coordinates" in names

    def test_kpoints_empty_returns_empty(self):
        provider = DocumentSymbolsProvider()
        symbols = provider.get_symbols("", "file:///work/KPOINTS")
        assert symbols == []

    def test_kpoints_automatic_mode(self):
        """KPOINTS with 'A' on line 2 triggers 'Fully automatic' label."""
        provider = DocumentSymbolsProvider()
        content = "Auto\nA\n\n4 4 4\n"
        symbols = provider.get_symbols(content, "file:///work/KPOINTS")
        names = [s.name for s in symbols]
        assert "Mode" in names
        mode_sym = next(s for s in symbols if s.name == "Mode")
        assert "Fully automatic" in mode_sym.detail

    def test_kpoints_numeric_count(self):
        """KPOINTS with a numeric count (explicit mode)."""
        provider = DocumentSymbolsProvider()
        content = "Explicit\n4\nReciprocal\n0.0 0.0 0.0 0.25\n0.5 0.0 0.0 0.25\n0.0 0.5 0.0 0.25\n0.5 0.5 0.0 0.25\n"
        symbols = provider.get_symbols(content, "file:///work/KPOINTS")
        names = [s.name for s in symbols]
        assert "Mode" in names
        assert "K-points" in names

    def test_kpoints_non_numeric_kpoint_line(self):
        """KPOINTS where a k-point line has non-numeric data."""
        provider = DocumentSymbolsProvider()
        content = "test\n0\nGamma\n4 4 4\n0 0 0\nnot_numeric\n"
        symbols = provider.get_symbols(content, "file:///work/KPOINTS")
        # Should still produce symbols, just without the invalid line
        assert len(symbols) >= 3
