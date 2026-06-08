"""Enhanced diagnostics for realistic VASP input failures."""

from pathlib import Path
from typing import Dict

from lsprotocol.types import Diagnostic, DiagnosticSeverity, Position, Range

from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.features.quickfixes import QuickFixesProvider
from vasp_lsp.parsers.potcar_parser import POTCARParser


def write_calc(tmp_path: Path, files: Dict[str, str]) -> Dict[str, str]:
    """Write a VASP calculation fixture and return file URIs."""
    uris = {}
    for name, content in files.items():
        path = tmp_path / name
        path.write_text(content)
        uris[name] = path.as_uri()
    return uris


def test_potcar_parser_extracts_species_and_cutoffs():
    content = """
PAW_PBE Fe 06Sep2000
   ENMIN = 200.000; ENMAX = 267.883 eV
End of Dataset
PAW_PBE O 08Apr2002
   ENMAX = 400.000; ENMIN = 300.000 eV
End of Dataset
"""
    data = POTCARParser(content).parse()

    assert data is not None
    assert [entry.element for entry in data.entries] == ["Fe", "O"]
    assert data.entries[0].enmax == 267.883
    assert data.entries[1].enmin == 300.0


def test_poscar_reports_degenerate_lattice_and_bad_selective_flags():
    content = """Bad structure
1.0
1.0 0.0 0.0
2.0 0.0 0.0
0.0 0.0 1.0
Fe
1
Selective dynamics
Direct
0.0 0.0 0.0 yes T F
"""
    diagnostics = DiagnosticsProvider().get_diagnostics(content, "file:///tmp/POSCAR")
    messages = [diagnostic.message for diagnostic in diagnostics]

    assert any("cell volume is zero" in message.lower() for message in messages)
    assert any("selective dynamics flags" in message.lower() for message in messages)


def test_poscar_reports_extra_coordinate_rows():
    content = """Extra rows
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
H
1
Direct
0.0 0.0 0.0
0.5 0.5 0.5
"""
    diagnostics = DiagnosticsProvider().get_diagnostics(content, "file:///tmp/POSCAR")

    assert any("extra coordinate" in diagnostic.message.lower() for diagnostic in diagnostics)


def test_incar_reports_type_errors_for_scalar_tags():
    content = """ENCUT = high
LWAVE = maybe
NSW = 2.5
"""
    diagnostics = DiagnosticsProvider().get_diagnostics(content, "file:///tmp/INCAR")
    messages = [diagnostic.message for diagnostic in diagnostics]

    assert any("ENCUT expects a float" in message for message in messages)
    assert any("LWAVE expects a boolean" in message for message in messages)
    assert any("NSW expects an integer" in message for message in messages)


def test_workspace_diagnostics_compare_incar_poscar_kpoints_potcar(tmp_path):
    uris = write_calc(
        tmp_path,
        {
            "INCAR": """ENCUT = 250
ISPIN = 2
MAGMOM = 5 5
LDAU = .TRUE.
LDAUL = 2
LDAUU = 4.0
KSPACING = 0.25
ICHARG = 11
""",
            "POSCAR": """FeO
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Fe O
1 2
Direct
0.0 0.0 0.0
0.5 0.5 0.5
0.25 0.25 0.25
""",
            "KPOINTS": """Band path
10
Line-mode
Reciprocal
0.0 0.0 0.0
0.5 0.0 0.0
""",
            "POTCAR": """PAW_PBE O 08Apr2002
 ENMAX = 400.000; ENMIN = 300.000 eV
End of Dataset
PAW_PBE Fe 06Sep2000
 ENMAX = 267.883; ENMIN = 200.000 eV
End of Dataset
""",
        },
    )
    provider = DiagnosticsProvider()
    diagnostics = provider.get_diagnostics((tmp_path / "INCAR").read_text(), uris["INCAR"])
    messages = [diagnostic.message for diagnostic in diagnostics]

    assert any(
        "MAGMOM has 2 entries but POSCAR contains 3 atoms" in message for message in messages
    )
    assert any(
        "LDAUL has 1 entries but POSCAR defines 2 species" in message for message in messages
    )
    assert any(
        "LDAUU has 1 entries but POSCAR defines 2 species" in message for message in messages
    )
    assert any("KSPACING is set while a KPOINTS file exists" in message for message in messages)
    assert any("ENCUT=250 is below max POTCAR ENMAX=400" in message for message in messages)
    assert any(
        "POSCAR species order Fe, O differs from POTCAR order O, Fe" in message
        for message in messages
    )
    assert any("ICHARG=11 usually requires a precomputed CHGCAR" in message for message in messages)


def test_quickfixes_for_workspace_incar_diagnostics():
    content = """ENCUT = 250
MAGMOM = 5 5
KSPACING = 0.25
"""
    diagnostics = [
        Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=11)),
            message="ENCUT=250 is below max POTCAR ENMAX=400 eV.",
            severity=DiagnosticSeverity.Warning,
        ),
        Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=12)),
            message="MAGMOM has 2 entries but POSCAR contains 3 atoms.",
            severity=DiagnosticSeverity.Warning,
        ),
        Diagnostic(
            range=Range(start=Position(line=2, character=0), end=Position(line=2, character=15)),
            message="KSPACING is set while a KPOINTS file exists.",
            severity=DiagnosticSeverity.Warning,
        ),
    ]
    actions = QuickFixesProvider().get_code_actions(
        content,
        "file:///tmp/INCAR",
        diagnostics,
        Range(start=Position(line=0, character=0), end=Position(line=2, character=0)),
    )
    titles = [action.title for action in actions]

    assert "Set ENCUT to 400 eV" in titles
    assert "Replace MAGMOM with 3 default moments" in titles
    assert "Remove KSPACING (use KPOINTS instead)" in titles
