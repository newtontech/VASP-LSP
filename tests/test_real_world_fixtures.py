"""Real-world VASP fixture tests.

Tests using realistic VASP input examples covering:
- Standard DFT relaxation
- Static calculation (NSW=0)
- Band structure (ICHARG=11 + line-mode KPOINTS)
- Spin-polarized calculation
- DFT+U calculation
- Molecular dynamics
- Common errors and edge cases
"""

import pytest
from lsprotocol.types import (
    CompletionParams,
    HoverParams,
    Position,
    TextDocumentIdentifier,
)

from vasp_lsp.features.completion import CompletionProvider
from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.features.formatting import FormattingProvider
from vasp_lsp.features.hover import HoverProvider
from vasp_lsp.features.navigation import DocumentSymbolsProvider
from vasp_lsp.parsers.incar_parser import INCARParser
from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
from vasp_lsp.parsers.poscar_parser import POSCARParser

# ---------------------------------------------------------------------------
# Real-world INCAR fixtures
# ---------------------------------------------------------------------------

RELAXATION_INCAR = """\
# Si bulk relaxation
SYSTEM = Si bulk relaxation
ENCUT = 520
ISMEAR = 0
SIGMA = 0.05
EDIFF = 1E-6
NELM = 100
NELMIN = 4
ALGO = Normal
IBRION = 2
NSW = 100
ISIF = 3
EDIFFG = -1E-2
LWAVE = .FALSE.
LCHARG = .TRUE.
LORBIT = 11
NCORE = 4
"""

STATIC_INCAR = """\
SYSTEM = Si static
ENCUT = 520
ISMEAR = -5
EDIFF = 1E-8
ALGO = Normal
NSW = 0
LORBIT = 11
NEDOS = 2001
"""

SPIN_POLARIZED_INCAR = """\
SYSTEM = Fe bcc ferromagnetic
ENCUT = 400
ISPIN = 2
MAGMOM = 2.0
ISMEAR = 1
SIGMA = 0.2
EDIFF = 1E-6
IBRION = 2
NSW = 50
EDIFFG = -0.01
LORBIT = 11
"""

DFT_U_INCAR = """\
SYSTEM = NiO with DFT+U
ENCUT = 520
ISPIN = 2
MAGMOM = 0 0 0 0 2 2
LDAU = .TRUE.
LDAUTYPE = 2
LDAUL = -1 -1 2 2
LDAUU = 0 0 5 5
LDAUJ = 0 0 0 0
ISMEAR = 0
SIGMA = 0.05
EDIFF = 1E-6
IBRION = 2
NSW = 100
EDIFFG = -0.01
"""

MD_INCAR = """\
SYSTEM = Si MD
ENCUT = 400
ISMEAR = 0
SIGMA = 0.1
EDIFF = 1E-5
IBRION = 0
NSW = 1000
POTIM = 1.0
TEBEG = 300
TEEND = 300
LWAVE = .FALSE.
LCHARG = .FALSE.
"""

# ---------------------------------------------------------------------------
# Real-world POSCAR fixtures
# ---------------------------------------------------------------------------

SI_FCC_POSCAR = """\
Si fcc
   5.43
     0.5000000000000000    0.5000000000000000    0.0000000000000000
     0.0000000000000000    0.5000000000000000    0.5000000000000000
     0.5000000000000000    0.0000000000000000    0.5000000000000000
   Si
     2
Direct
  0.0000000000000000  0.0000000000000000  0.0000000000000000
  0.2500000000000000  0.2500000000000000  0.2500000000000000
"""

FE_BCC_POSCAR = """\
Fe bcc
2.87
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Fe
1
Cartesian
0.0 0.0 0.0
"""

NIO_POSCAR = """\
NiO rocksalt
1.0
4.17 0.0 0.0
0.0 4.17 0.0
0.0 0.0 4.17
Ni O
1 1
Direct
0.0 0.0 0.0
0.5 0.5 0.5
"""

SELECTIVE_DYNAMICS_POSCAR = """\
Si with selective dynamics
1.0
5.43 0.0 0.0
0.0 5.43 0.0
0.0 0.0 5.43
Si
4
Selective dynamics
Direct
0.0 0.0 0.0 T T T
0.25 0.25 0.25 F F F
0.5 0.5 0.0 T T F
0.75 0.75 0.25 T T T
"""

# ---------------------------------------------------------------------------
# Real-world KPOINTS fixtures
# ---------------------------------------------------------------------------

GAMMA_KPOINTS = """\
Gamma-centered 4x4x4
0
Gamma
4 4 4
0 0 0
"""

MONKHORST_KPOINTS = """\
Monkhorst-Pack 6x6x6
0
Monkhorst-Pack
6 6 6
0 0 0
"""

BAND_STRUCTURE_KPOINTS = """\
Band structure
20
Line-mode
reciprocal
   0.0000000000   0.0000000000   0.0000000000     GAMMA
   0.5000000000   0.0000000000   0.5000000000     X

   0.5000000000   0.0000000000   0.5000000000     X
   0.5000000000   0.2500000000   0.7500000000     W

   0.5000000000   0.2500000000   0.7500000000     W
   0.3750000000   0.3750000000   0.7500000000     K

   0.3750000000   0.3750000000   0.7500000000     K
   0.0000000000   0.0000000000   0.0000000000     GAMMA

   0.0000000000   0.0000000000   0.0000000000     GAMMA
   0.5000000000   0.5000000000   0.5000000000     L
"""

AUTOMATIC_KPOINTS = """\
Automatic mesh
A
4 4 4
0 0 0
"""

EXPLICIT_KPOINTS = """\
Explicit kpoints
4
Reciprocal
   0.0  0.0  0.0   0.25
   0.5  0.0  0.0   0.25
   0.0  0.5  0.0   0.25
   0.5  0.5  0.0   0.25
"""


# ---------------------------------------------------------------------------
# INCAR parser tests with real fixtures
# ---------------------------------------------------------------------------


class TestINCARRealFixtures:
    """Parse real-world INCAR files and verify parameter extraction."""

    def test_relaxation_incar_parsing(self):
        parser = INCARParser(RELAXATION_INCAR)
        params = parser.parse()
        assert parser.has_parameter("ENCUT")
        assert parser.has_parameter("ISMEAR")
        assert parser.has_parameter("SIGMA")
        assert parser.has_parameter("EDIFF")
        assert parser.has_parameter("IBRION")
        assert parser.has_parameter("NSW")
        assert parser.has_parameter("ISIF")
        assert parser.has_parameter("EDIFFG")
        assert parser.has_parameter("NCORE")
        assert params["ENCUT"].value == 520
        assert params["ISMEAR"].value == 0
        assert params["SIGMA"].value == pytest.approx(0.05)
        assert params["IBRION"].value == 2
        assert params["NSW"].value == 100
        assert params["LWAVE"].value is False
        assert params["LCHARG"].value is True
        assert len(params) == 16

    def test_static_incar_parsing(self):
        parser = INCARParser(STATIC_INCAR)
        params = parser.parse()
        assert params["NSW"].value == 0
        assert params["ISMEAR"].value == -5
        assert params["NEDOS"].value == 2001
        assert params["EDIFF"].value == pytest.approx(1e-8)

    def test_spin_polarized_incar_parsing(self):
        parser = INCARParser(SPIN_POLARIZED_INCAR)
        params = parser.parse()
        assert params["ISPIN"].value == 2
        assert params["MAGMOM"].value == pytest.approx(2.0)

    def test_dft_u_incar_parsing(self):
        parser = INCARParser(DFT_U_INCAR)
        params = parser.parse()
        assert params["LDAU"].value is True
        assert params["LDAUTYPE"].value == 2
        assert params["LDAUL"].value == [-1, -1, 2, 2]
        assert params["LDAUU"].value == [0, 0, 5, 5]

    def test_md_incar_parsing(self):
        parser = INCARParser(MD_INCAR)
        params = parser.parse()
        assert params["IBRION"].value == 0
        assert params["NSW"].value == 1000
        assert params["POTIM"].value == pytest.approx(1.0)
        assert params["TEBEG"].value == pytest.approx(300)
        assert params["LWAVE"].value is False

    def test_boolean_true_variants(self):
        """Test various boolean TRUE representations."""
        for true_val in [".TRUE.", "T", "TRUE"]:
            parser = INCARParser(f"TEST = {true_val}\n")
            params = parser.parse()
            assert params["TEST"].value is True, f"Failed for {true_val}"

    def test_boolean_false_variants(self):
        """Test various boolean FALSE representations."""
        for false_val in [".FALSE.", "F", "FALSE"]:
            parser = INCARParser(f"TEST = {false_val}\n")
            params = parser.parse()
            assert params["TEST"].value is False, f"Failed for {false_val}"


# ---------------------------------------------------------------------------
# POSCAR parser tests with real fixtures
# ---------------------------------------------------------------------------


class TestPOSCARRealFixtures:
    """Parse real-world POSCAR files."""

    def test_si_fcc_poscar(self):
        parser = POSCARParser(SI_FCC_POSCAR)
        result = parser.parse()
        assert result is not None
        assert result.system_comment == "Si fcc"
        assert result.scale_factor == pytest.approx(5.43)
        assert len(result.lattice_vectors) == 3
        assert result.atom_types == ["Si"]
        assert result.atom_counts == [2]
        assert result.coordinate_type == "Direct"
        assert len(result.coordinates) == 2

    def test_fe_bcc_poscar(self):
        parser = POSCARParser(FE_BCC_POSCAR)
        result = parser.parse()
        assert result is not None
        assert result.atom_types == ["Fe"]
        assert result.atom_counts == [1]
        assert result.coordinate_type == "Cartesian"
        assert len(result.coordinates) == 1

    def test_nio_poscar(self):
        parser = POSCARParser(NIO_POSCAR)
        result = parser.parse()
        assert result is not None
        assert result.atom_types == ["Ni", "O"]
        assert result.atom_counts == [1, 1]
        assert len(result.coordinates) == 2

    def test_selective_dynamics_poscar(self):
        parser = POSCARParser(SELECTIVE_DYNAMICS_POSCAR)
        result = parser.parse()
        assert result is not None
        assert result.has_selective_dynamics is True
        assert result.selective_dynamics is not None
        assert len(result.selective_dynamics) == 4
        # First atom: T T T
        assert result.selective_dynamics[0] == [True, True, True]
        # Second atom: F F F
        assert result.selective_dynamics[1] == [False, False, False]
        # Third atom: T T F
        assert result.selective_dynamics[2] == [True, True, False]


# ---------------------------------------------------------------------------
# KPOINTS parser tests with real fixtures
# ---------------------------------------------------------------------------


class TestKPOINTSRealFixtures:
    """Parse real-world KPOINTS files."""

    def test_gamma_kpoints(self):
        parser = KPOINTSParser(GAMMA_KPOINTS)
        result = parser.parse()
        assert result is not None
        assert result.grid == [4, 4, 4]
        assert result.shift == [0.0, 0.0, 0.0]

    def test_monkhorst_kpoints(self):
        parser = KPOINTSParser(MONKHORST_KPOINTS)
        result = parser.parse()
        assert result is not None
        assert result.grid == [6, 6, 6]

    def test_band_structure_kpoints(self):
        parser = KPOINTSParser(BAND_STRUCTURE_KPOINTS)
        result = parser.parse()
        assert result is not None
        from vasp_lsp.parsers.kpoints_parser import KPOINTSMode

        assert result.mode == KPOINTSMode.LINE_MODE
        assert result.line_density == 20
        assert len(result.kpoints) > 0

    def test_automatic_kpoints(self):
        parser = KPOINTSParser(AUTOMATIC_KPOINTS)
        result = parser.parse()
        assert result is not None
        from vasp_lsp.parsers.kpoints_parser import KPOINTSMode

        assert result.mode == KPOINTSMode.AUTOMATIC
        assert result.grid == [4, 4, 4]

    def test_explicit_kpoints(self):
        parser = KPOINTSParser(EXPLICIT_KPOINTS)
        result = parser.parse()
        assert result is not None
        from vasp_lsp.parsers.kpoints_parser import KPOINTSMode

        assert result.mode == KPOINTSMode.EXPLICIT
        assert len(result.kpoints) == 4
        assert result.weights is not None
        assert len(result.weights) == 4
        assert sum(result.weights) == pytest.approx(1.0)


# ---------------------------------------------------------------------------
# Invalid INCAR input tests
# ---------------------------------------------------------------------------


class TestINCARInvalidCases:
    """Test INCAR parser with various invalid inputs."""

    def test_unknown_tag(self):
        content = "UNKNOWN_TAG = 42\n"
        diag = DiagnosticsProvider()
        diags = diag.get_diagnostics(content, "file:///work/INCAR")
        messages = [d.message for d in diags]
        assert any("Unknown" in m for m in messages)

    def test_duplicate_parameter(self):
        content = "ENCUT = 400\nENCUT = 520\n"
        parser = INCARParser(content)
        parser.parse()
        errors = parser.get_errors()
        messages = [e["message"] for e in errors]
        assert any("Duplicate" in m for m in messages)

    def test_invalid_line(self):
        content = "this is not a valid line\n"
        parser = INCARParser(content)
        parser.parse()
        errors = parser.get_errors()
        assert len(errors) > 0

    def test_empty_content(self):
        content = ""
        parser = INCARParser(content)
        params = parser.parse()
        assert len(params) == 0

    def test_only_comments(self):
        content = "# just comments\n! another comment\n"
        parser = INCARParser(content)
        params = parser.parse()
        assert len(params) == 0

    def test_ismear_without_sigma(self):
        content = "ISMEAR = 0\n"
        diag = DiagnosticsProvider()
        diags = diag.get_diagnostics(content, "file:///work/INCAR")
        messages = [d.message for d in diags]
        assert any("SIGMA" in m for m in messages)

    def test_ncore_and_npar_conflict(self):
        content = "NCORE = 4\nNPAR = 2\n"
        diag = DiagnosticsProvider()
        diags = diag.get_diagnostics(content, "file:///work/INCAR")
        messages = [d.message for d in diags]
        assert any("NPAR" in m and "NCORE" in m for m in messages)

    def test_negative_encut(self):
        content = "ENCUT = -100\n"
        diag = DiagnosticsProvider()
        diags = diag.get_diagnostics(content, "file:///work/INCAR")
        messages = [d.message for d in diags]
        assert any(
            "ENCUT" in m
            and ("range" in m.lower() or "negative" in m.lower() or "below" in m.lower())
            for m in messages
        )

    def test_ldau_without_ldau_params(self):
        content = "LDAU = .TRUE.\n"
        diag = DiagnosticsProvider()
        diags = diag.get_diagnostics(content, "file:///work/INCAR")
        messages = [d.message for d in diags]
        assert any("LDAU" in m for m in messages)


# ---------------------------------------------------------------------------
# Invalid POSCAR input tests
# ---------------------------------------------------------------------------


class TestPOSCARInvalidCases:
    """Test POSCAR handling with various invalid inputs."""

    def test_too_few_lines(self):
        content = "Si\n"
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None

    def test_invalid_lattice_vector(self):
        content = (
            "Si\n1.0\n"
            "abc 0.0 0.0\n"
            "0.0 1.0 0.0\n"
            "0.0 0.0 1.0\n"
            "Si\n1\nDirect\n0.0 0.0 0.0\n"
        )
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None

    def test_negative_scale_factor(self):
        content = (
            "Si\n-1.0\n" "1.0 0.0 0.0\n0.0 1.0 0.0\n0.0 0.0 1.0\n" "Si\n1\nDirect\n0.0 0.0 0.0\n"
        )
        diag = DiagnosticsProvider()
        diags = diag.get_diagnostics(content, "file:///work/POSCAR")
        messages = [d.message for d in diags]
        assert any("negative" in m.lower() or "scale" in m.lower() for m in messages)

    def test_zero_volume_cell(self):
        """All-zero lattice vectors should be detected."""
        content = (
            "Si\n1.0\n" "0.0 0.0 0.0\n0.0 0.0 0.0\n0.0 0.0 0.0\n" "Si\n1\nDirect\n0.0 0.0 0.0\n"
        )
        parser = POSCARParser(content)
        result = parser.parse()
        if result is not None:
            diag = DiagnosticsProvider()
            diags = diag.get_diagnostics(content, "file:///work/POSCAR")
            messages = [d.message for d in diags]
            assert any("volume" in m.lower() or "linearly dependent" in m.lower() for m in messages)


# ---------------------------------------------------------------------------
# Invalid KPOINTS input tests
# ---------------------------------------------------------------------------


class TestKPOINTSInvalidCases:
    """Test KPOINTS handling with various invalid inputs."""

    def test_too_short_file(self):
        content = "kpoints\n"
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is None

    def test_invalid_grid_values(self):
        content = "grid\n0\nGamma\nabc def ghi\n0 0 0\n"
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is None

    def test_zero_grid_values(self):
        content = "grid\n0\nGamma\n0 0 0\n0 0 0\n"
        diag = DiagnosticsProvider()
        diags = diag.get_diagnostics(content, "file:///work/KPOINTS")
        messages = [d.message for d in diags]
        assert any("not positive" in m.lower() or "sparse" in m.lower() for m in messages)


# ---------------------------------------------------------------------------
# Formatting tests with real fixtures
# ---------------------------------------------------------------------------


class TestFormattingRealFixtures:
    """Test formatting with real-world files."""

    def test_format_relaxation_incar(self):
        formatter = FormattingProvider()
        edits = formatter.format_document(
            RELAXATION_INCAR, "file:///work/INCAR", {"tabSize": 2, "insertSpaces": True}
        )
        assert len(edits) > 0
        formatted = edits[0].new_text
        assert "ENCUT" in formatted
        assert "ISMEAR" in formatted

    def test_format_si_fcc_poscar(self):
        formatter = FormattingProvider()
        edits = formatter.format_document(
            SI_FCC_POSCAR, "file:///work/POSCAR", {"tabSize": 2, "insertSpaces": True}
        )
        assert len(edits) > 0
        formatted = edits[0].new_text
        assert "Direct" in formatted

    def test_format_gamma_kpoints(self):
        formatter = FormattingProvider()
        edits = formatter.format_document(
            GAMMA_KPOINTS, "file:///work/KPOINTS", {"tabSize": 2, "insertSpaces": True}
        )
        assert len(edits) > 0
        formatted = edits[0].new_text
        assert "Gamma" in formatted

    def test_format_empty_incar(self):
        formatter = FormattingProvider()
        edits = formatter.format_document("", "file:///work/INCAR", {})
        assert edits == []


# ---------------------------------------------------------------------------
# Hover tests with real fixtures
# ---------------------------------------------------------------------------


class TestHoverRealFixtures:
    """Test hover with real-world files."""

    def test_hover_incar_encut(self):
        provider = HoverProvider()
        hover = provider.get_hover(
            HoverParams(
                text_document=TextDocumentIdentifier(uri="file:///work/INCAR"),
                position=Position(line=2, character=2),
            ),
            RELAXATION_INCAR,
            "file:///work/INCAR",
        )
        assert hover is not None
        assert "ENCUT" in hover.contents.value

    def test_hover_poscar_scale(self):
        provider = HoverProvider()
        hover = provider.get_hover(
            HoverParams(
                text_document=TextDocumentIdentifier(uri="file:///work/POSCAR"),
                position=Position(line=1, character=0),
            ),
            SI_FCC_POSCAR,
            "file:///work/POSCAR",
        )
        assert hover is not None
        assert "Scale" in hover.contents.value

    def test_hover_kpoints_comment(self):
        provider = HoverProvider()
        hover = provider.get_hover(
            HoverParams(
                text_document=TextDocumentIdentifier(uri="file:///work/KPOINTS"),
                position=Position(line=0, character=0),
            ),
            GAMMA_KPOINTS,
            "file:///work/KPOINTS",
        )
        assert hover is not None


# ---------------------------------------------------------------------------
# Document Symbols with real fixtures
# ---------------------------------------------------------------------------


class TestDocumentSymbolsRealFixtures:
    """Test document symbols with real-world files."""

    def test_incar_relaxation_symbols(self):
        provider = DocumentSymbolsProvider()
        symbols = provider.get_symbols(RELAXATION_INCAR, "file:///work/INCAR")
        names = [s.name for s in symbols]
        assert "ENCUT" in names
        assert "ISMEAR" in names
        assert "SIGMA" in names
        assert "IBRION" in names

    def test_poscar_symbols(self):
        provider = DocumentSymbolsProvider()
        symbols = provider.get_symbols(SI_FCC_POSCAR, "file:///work/POSCAR")
        names = [s.name for s in symbols]
        assert "System Comment" in names
        assert "Scale Factor" in names
        assert "Coordinates" in names

    def test_kpoints_band_structure_symbols(self):
        provider = DocumentSymbolsProvider()
        symbols = provider.get_symbols(BAND_STRUCTURE_KPOINTS, "file:///work/KPOINTS")
        names = [s.name for s in symbols]
        assert "Comment" in names
        assert "Mode" in names


# ---------------------------------------------------------------------------
# Completion tests with real fixtures
# ---------------------------------------------------------------------------


class TestCompletionRealFixtures:
    """Test completion with real-world files."""

    def test_incar_tag_completion(self):
        provider = CompletionProvider()
        result = provider.get_completions(
            CompletionParams(
                text_document=TextDocumentIdentifier(uri="file:///work/INCAR"),
                position=Position(line=0, character=2),
            ),
            "EN\n",
            "file:///work/INCAR",
        )
        items = result.items
        labels = [i.label for i in items]
        assert "ENCUT" in labels

    def test_incar_value_completion_boolean(self):
        provider = CompletionProvider()
        result = provider.get_completions(
            CompletionParams(
                text_document=TextDocumentIdentifier(uri="file:///work/INCAR"),
                position=Position(line=0, character=12),
            ),
            "LWAVE = \n",
            "file:///work/INCAR",
        )
        items = result.items
        labels = [i.label for i in items]
        assert ".TRUE." in labels or ".FALSE." in labels

    def test_poscar_completion_line0(self):
        provider = CompletionProvider()
        result = provider.get_completions(
            CompletionParams(
                text_document=TextDocumentIdentifier(uri="file:///work/POSCAR"),
                position=Position(line=0, character=0),
            ),
            "",
            "file:///work/POSCAR",
        )
        assert len(result.items) > 0

    def test_kpoints_completion_line1(self):
        provider = CompletionProvider()
        result = provider.get_completions(
            CompletionParams(
                text_document=TextDocumentIdentifier(uri="file:///work/KPOINTS"),
                position=Position(line=1, character=0),
            ),
            "grid\n",
            "file:///work/KPOINTS",
        )
        assert len(result.items) > 0
