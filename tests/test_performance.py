"""Performance benchmarks for VASP-LSP parsers and features.

Uses time.perf_counter() for high-resolution timing. Each test runs the
target operation multiple iterations and asserts that the average time per
call stays within the specified budget.
"""

import time
from typing import List

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
from vasp_lsp.parsers.incar_parser import INCARParser
from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
from vasp_lsp.parsers.poscar_parser import POSCARParser

# ---------------------------------------------------------------------------
# Fixture generators
# ---------------------------------------------------------------------------

# Known INCAR tags with representative values used for generating synthetic
# INCAR files of arbitrary size.  The list is longer than 50 so that the
# 50-tag benchmark gets a good mix of types (integer, float, boolean, string,
# array).
_TAG_VALUE_MAP: List[tuple] = [
    ("ENCUT", "520"),
    ("ISMEAR", "0"),
    ("SIGMA", "0.05"),
    ("EDIFF", "1E-6"),
    ("NELM", "100"),
    ("NELMIN", "4"),
    ("ALGO", "Normal"),
    ("ISPIN", "2"),
    ("MAGMOM", "5.0 5.0 5.0"),
    ("LORBIT", "11"),
    ("NEDOS", "601"),
    ("IBRION", "2"),
    ("NSW", "100"),
    ("EDIFFG", "-0.01"),
    ("POTIM", "0.5"),
    ("ISIF", "3"),
    ("KGAMMA", ".TRUE."),
    ("KSPACING", "0.3"),
    ("NCORE", "4"),
    ("NPAR", "4"),
    ("KPAR", "2"),
    ("LWAVE", ".FALSE."),
    ("LCHARG", ".TRUE."),
    ("LAECHG", ".TRUE."),
    ("LVHAR", ".FALSE."),
    ("LVTOT", ".FALSE."),
    ("LELF", ".FALSE."),
    ("LORBITALREAL", ".FALSE."),
    ("LHFCALC", ".TRUE."),
    ("HFSCREEN", "0.2"),
    ("PRECFOCK", "Fast"),
    ("IVDW", "12"),
    ("LDAU", ".TRUE."),
    ("LDAUTYPE", "2"),
    ("LDAUL", "2 2"),
    ("LDAUU", "4.0 4.0"),
    ("LDAUJ", "0.0 0.0"),
    ("LSORBIT", ".FALSE."),
    ("SAXIS", "0 0 1"),
    ("AMIX", "0.2"),
    ("BMIX", "0.0001"),
    ("AMIN", "0.01"),
    ("SYSTEM", "benchmark"),
    ("NWRITE", "2"),
    ("PREC", "Accurate"),
    ("ISTART", "1"),
    ("ICHARG", "0"),
    ("NBANDS", "256"),
    ("NELECT", "32.0"),
    ("LREAL", "Auto"),
    ("ROPT", "0.8 0.8"),
    ("EMIN", "-10.0"),
    ("EMAX", "10.0"),
]


def _generate_large_incar(n_tags: int) -> str:
    """Generate a synthetic INCAR file with *n_tags* parameter lines.

    Tags are cycled from the known tag list so that every type (int, float,
    bool, string, array) is represented.
    """
    lines: List[str] = ["# Performance benchmark INCAR", ""]
    for i in range(n_tags):
        tag, value = _TAG_VALUE_MAP[i % len(_TAG_VALUE_MAP)]
        lines.append(f"{tag} = {value}  ! benchmark tag {i}")
    return "\n".join(lines) + "\n"


def _generate_large_poscar(n_atoms: int) -> str:
    """Generate a synthetic POSCAR file with *n_atoms* Si atoms."""
    coords = "\n".join(
        f"  {i / n_atoms:.6f}  {((i * 7) % n_atoms) / n_atoms:.6f}  "
        f"{((i * 13) % n_atoms) / n_atoms:.6f}"
        for i in range(n_atoms)
    )
    return (
        f"Benchmark system ({n_atoms} atoms)\n"
        "1.0\n"
        "10.0 0.0 0.0\n"
        "0.0 10.0 0.0\n"
        "0.0 0.0 10.0\n"
        "Si\n"
        f"{n_atoms}\n"
        "Direct\n"
        f"{coords}\n"
    )


def _generate_large_kpoints(n_kpoints: int) -> str:
    """Generate an explicit-mode KPOINTS file with *n_kpoints* entries."""
    kpt_lines = "\n".join(
        f"  {i / n_kpoints:.6f}  {((i * 7) % n_kpoints) / n_kpoints:.6f}  "
        f"{((i * 13) % n_kpoints) / n_kpoints:.6f}  1.0"
        for i in range(n_kpoints)
    )
    return (
        "Benchmark KPOINTS\n"
        f"{n_kpoints}\n"
        "Reciprocal\n"
        f"{kpt_lines}\n"
    )


# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------

def _measure_avg_ms(func, iterations: int) -> float:
    """Run *func* for *iterations* times and return the average wall-clock
    time per call in milliseconds."""
    start = time.perf_counter()
    for _ in range(iterations):
        func()
    elapsed = time.perf_counter() - start
    return (elapsed / iterations) * 1000.0


# ---------------------------------------------------------------------------
# INCAR parser benchmarks
# ---------------------------------------------------------------------------

@pytest.mark.slow
class TestINCARParserPerformance:
    """Benchmarks for the INCAR file parser."""

    def test_parse_50_tags(self) -> None:
        """Parse a 50-tag INCAR; average must be < 10 ms."""
        content = _generate_large_incar(50)
        iterations = 100

        avg_ms = _measure_avg_ms(
            lambda: (INCARParser(content).parse()),
            iterations,
        )
        assert avg_ms < 10.0, f"INCAR 50-tag parse too slow: {avg_ms:.2f} ms (budget 10 ms)"


# ---------------------------------------------------------------------------
# POSCAR parser benchmarks
# ---------------------------------------------------------------------------

@pytest.mark.slow
class TestPOSCARParserPerformance:
    """Benchmarks for the POSCAR file parser."""

    def test_parse_100_atoms(self) -> None:
        """Parse a POSCAR with 100 atoms; average must be < 5 ms."""
        content = _generate_large_poscar(100)
        iterations = 100

        avg_ms = _measure_avg_ms(
            lambda: (POSCARParser(content).parse()),
            iterations,
        )
        assert avg_ms < 5.0, f"POSCAR 100-atom parse too slow: {avg_ms:.2f} ms (budget 5 ms)"

    def test_parse_1000_atoms(self) -> None:
        """Parse a POSCAR with 1000 atoms; average must be < 50 ms."""
        content = _generate_large_poscar(1000)
        iterations = 50

        avg_ms = _measure_avg_ms(
            lambda: (POSCARParser(content).parse()),
            iterations,
        )
        assert avg_ms < 50.0, f"POSCAR 1000-atom parse too slow: {avg_ms:.2f} ms (budget 50 ms)"


# ---------------------------------------------------------------------------
# KPOINTS parser benchmarks
# ---------------------------------------------------------------------------

@pytest.mark.slow
class TestKPOINTSParserPerformance:
    """Benchmarks for the KPOINTS file parser."""

    def test_parse_100_kpoints(self) -> None:
        """Parse an explicit KPOINTS with 100 entries; average must be < 5 ms."""
        content = _generate_large_kpoints(100)
        iterations = 100

        avg_ms = _measure_avg_ms(
            lambda: (KPOINTSParser(content).parse()),
            iterations,
        )
        assert avg_ms < 5.0, f"KPOINTS 100-kpt parse too slow: {avg_ms:.2f} ms (budget 5 ms)"

    def test_parse_1000_kpoints(self) -> None:
        """Parse an explicit KPOINTS with 1000 entries; average must be < 50 ms."""
        content = _generate_large_kpoints(1000)
        iterations = 50

        avg_ms = _measure_avg_ms(
            lambda: (KPOINTSParser(content).parse()),
            iterations,
        )
        assert avg_ms < 50.0, f"KPOINTS 1000-kpt parse too slow: {avg_ms:.2f} ms (budget 50 ms)"


# ---------------------------------------------------------------------------
# Diagnostics benchmarks
# ---------------------------------------------------------------------------

@pytest.mark.slow
class TestDiagnosticsPerformance:
    """Benchmarks for the diagnostics provider."""

    def test_incar_diagnostics_50_tags(self) -> None:
        """Run full INCAR diagnostics on a 50-tag file; average must be < 20 ms."""
        content = _generate_large_incar(50)
        provider = DiagnosticsProvider()
        uri = "file:///tmp/INCAR"
        iterations = 50

        avg_ms = _measure_avg_ms(
            lambda: provider.get_diagnostics(content, uri),
            iterations,
        )
        assert avg_ms < 20.0, f"INCAR diagnostics too slow: {avg_ms:.2f} ms (budget 20 ms)"


# ---------------------------------------------------------------------------
# Completion benchmarks
# ---------------------------------------------------------------------------

@pytest.mark.slow
class TestCompletionPerformance:
    """Benchmarks for the completion provider."""

    def test_incar_tag_completion(self) -> None:
        """Get INCAR tag completions at a position; average must be < 5 ms."""
        content = _generate_large_incar(50)
        provider = CompletionProvider()
        uri = "file:///tmp/INCAR"
        params = CompletionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=10, character=2),
        )
        iterations = 200

        avg_ms = _measure_avg_ms(
            lambda: provider.get_completions(params, content, uri),
            iterations,
        )
        assert avg_ms < 5.0, f"INCAR completion too slow: {avg_ms:.2f} ms (budget 5 ms)"

    def test_incar_value_completion(self) -> None:
        """Get INCAR value completions after '='; average must be < 5 ms."""
        content = _generate_large_incar(50)
        provider = CompletionProvider()
        uri = "file:///tmp/INCAR"
        # Position on a line that has "ALGO = " so we're after the '='
        params = CompletionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=6, character=8),
        )
        iterations = 200

        avg_ms = _measure_avg_ms(
            lambda: provider.get_completions(params, content, uri),
            iterations,
        )
        assert avg_ms < 5.0, f"INCAR value completion too slow: {avg_ms:.2f} ms (budget 5 ms)"


# ---------------------------------------------------------------------------
# Hover benchmarks
# ---------------------------------------------------------------------------

@pytest.mark.slow
class TestHoverPerformance:
    """Benchmarks for the hover documentation provider."""

    def test_incar_hover_known_tag(self) -> None:
        """Hover on a known INCAR tag; average must be < 5 ms."""
        content = _generate_large_incar(50)
        provider = HoverProvider()
        uri = "file:///tmp/INCAR"
        # Line 2 in the generated INCAR is "ENCUT = 520  ! benchmark tag 0"
        params = HoverParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=2, character=1),
        )
        iterations = 200

        avg_ms = _measure_avg_ms(
            lambda: provider.get_hover(params, content, uri),
            iterations,
        )
        assert avg_ms < 5.0, f"INCAR hover too slow: {avg_ms:.2f} ms (budget 5 ms)"

    def test_incar_hover_various_tags(self) -> None:
        """Hover on several different INCAR tags; each must be < 5 ms."""
        content = _generate_large_incar(50)
        provider = HoverProvider()
        uri = "file:///tmp/INCAR"
        iterations = 100

        # Spot-check a few different lines from the generated content.
        # Lines 2-6 map to the first five tags: ENCUT, ISMEAR, SIGMA, EDIFF, NELM.
        for line in (2, 3, 4, 5, 6):
            params = HoverParams(
                text_document=TextDocumentIdentifier(uri=uri),
                position=Position(line=line, character=1),
            )
            avg_ms = _measure_avg_ms(
                lambda p=params: provider.get_hover(p, content, uri),
                iterations,
            )
            assert avg_ms < 5.0, (
                f"INCAR hover (line {line}) too slow: {avg_ms:.2f} ms (budget 5 ms)"
            )


# ---------------------------------------------------------------------------
# Formatting benchmarks
# ---------------------------------------------------------------------------

@pytest.mark.slow
class TestFormattingPerformance:
    """Benchmarks for the document formatting provider."""

    def test_format_100_line_incar(self) -> None:
        """Format a ~100-line INCAR file; average must be < 20 ms."""
        # 50 tags + comments/header ≈ 53 lines; double for ~100.
        content = _generate_large_incar(100)
        provider = FormattingProvider()
        uri = "file:///tmp/INCAR"
        iterations = 100

        avg_ms = _measure_avg_ms(
            lambda: provider.format_document(content, uri),
            iterations,
        )
        assert avg_ms < 20.0, f"INCAR formatting too slow: {avg_ms:.2f} ms (budget 20 ms)"
