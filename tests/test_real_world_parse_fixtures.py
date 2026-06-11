"""Regression tests for public real-world VASP input fixtures."""

from pathlib import Path

import pytest

from vasp_lsp.parsers.incar_parser import INCARParser
from vasp_lsp.parsers.kpoints_parser import KPOINTSMode, KPOINTSParser
from vasp_lsp.parsers.poscar_parser import POSCARParser

FIXTURE_DIR = Path(__file__).parent / "fixtures" / "real_world" / "pymatgen"


def read_fixture(name: str) -> str:
    return (FIXTURE_DIR / name).read_text(encoding="utf-8")


def test_real_world_incar_parse_succeeds_without_errors() -> None:
    parser = INCARParser(read_fixture("INCAR"))
    parameters = parser.parse()

    assert parameters
    assert {"ENCUT", "EDIFF", "MAGMOM"}.issubset(parameters)
    assert parser.get_errors() == []


@pytest.mark.parametrize(
    ("fixture_name", "expected_atom_count"),
    [
        ("POSCAR_Fe3O4", 14),
        ("POSCAR_bcc", 2),
    ],
)
def test_real_world_poscar_parse_succeeds_without_errors(
    fixture_name: str, expected_atom_count: int
) -> None:
    parser = POSCARParser(read_fixture(fixture_name))
    data = parser.parse()

    assert data is not None
    assert sum(data.atom_counts) == expected_atom_count
    assert len(data.coordinates) == expected_atom_count
    assert parser.get_errors() == []


@pytest.mark.parametrize(
    ("fixture_name", "expected_mode"),
    [
        ("KPOINTS_monkhorst", KPOINTSMode.GAMMA_MONKHORST),
        ("KPOINTS_explicit", KPOINTSMode.EXPLICIT),
        ("KPOINTS_gamma", KPOINTSMode.GAMMA_MONKHORST),
    ],
)
def test_real_world_kpoints_parse_succeeds_without_errors(
    fixture_name: str, expected_mode: KPOINTSMode
) -> None:
    parser = KPOINTSParser(read_fixture(fixture_name))
    data = parser.parse()

    assert data is not None
    assert data.mode == expected_mode
    assert parser.get_errors() == []
