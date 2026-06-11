from pathlib import Path
from typing import Dict

from vasp_lsp.features.diagnostics import DiagnosticsProvider


def write_calc(tmp_path: Path, files: Dict[str, str]) -> Dict[str, str]:
    uris = {}
    for name, content in files.items():
        path = tmp_path / name
        path.write_text(content, encoding="utf-8")
        uris[name] = path.as_uri()
    return uris


POSCAR_ONE_ATOM = """Si
1.0
1.0 0.0 0.0
0.0 1.0 0.0
0.0 0.0 1.0
Si
1
Direct
0.0 0.0 0.0
"""

KPOINTS_444 = """Automatic mesh
0
Gamma
4 4 4
0 0 0
"""


def test_plan24_parallel_restart_preflight_reports_high_value_risks(tmp_path: Path) -> None:
    uris = write_calc(
        tmp_path,
        {
            "INCAR": """LHFCALC = .TRUE.
ALGO = VeryFast
NKREDX = 3
KPAR = 3
ISTART = 1
IBRION = 44
""",
            "POSCAR": POSCAR_ONE_ATOM,
            "KPOINTS": KPOINTS_444,
            "vasp-lsp.json": '{"mpi_ranks": 16}',
        },
    )

    diagnostics = DiagnosticsProvider().get_diagnostics(
        (tmp_path / "INCAR").read_text(encoding="utf-8"),
        uris["INCAR"],
        workspace_documents={
            uri: (tmp_path / name).read_text(encoding="utf-8") for name, uri in uris.items()
        },
    )
    messages = [diagnostic.message for diagnostic in diagnostics]

    assert any("LHFCALC=.TRUE. with ALGO=VeryFast" in message for message in messages)
    assert any(
        "NKREDX=3 does not divide KPOINTS mesh dimension 4" in message for message in messages
    )
    assert any("KPAR=3 does not divide mpi_ranks=16" in message for message in messages)
    assert any("ISTART=1 usually requires WAVECAR" in message for message in messages)
    assert any("IBRION=44 dimer mode requires" in message for message in messages)


def test_plan24_parallel_restart_preflight_allows_consistent_settings(tmp_path: Path) -> None:
    uris = write_calc(
        tmp_path,
        {
            "INCAR": """LHFCALC = .TRUE.
ALGO = Normal
NKREDX = 2
NKREDY = 2
NKREDZ = 2
KPAR = 4
NCORE = 2
ISTART = 0
IBRION = 2
""",
            "POSCAR": POSCAR_ONE_ATOM,
            "KPOINTS": KPOINTS_444,
            "vasp-lsp.json": '{"mpi_ranks": 16}',
        },
    )

    diagnostics = DiagnosticsProvider().get_diagnostics(
        (tmp_path / "INCAR").read_text(encoding="utf-8"),
        uris["INCAR"],
        workspace_documents={
            uri: (tmp_path / name).read_text(encoding="utf-8") for name, uri in uris.items()
        },
    )
    messages = [diagnostic.message for diagnostic in diagnostics]

    assert not any("LHFCALC=.TRUE. with ALGO=VeryFast" in message for message in messages)
    assert not any("does not divide KPOINTS mesh" in message for message in messages)
    assert not any("does not divide mpi_ranks" in message for message in messages)
    assert not any("usually requires WAVECAR" in message for message in messages)
    assert not any("dimer mode requires" in message for message in messages)
