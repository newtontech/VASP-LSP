"""Validation accuracy tests using the structured test corpus.

These tests run the full validation corpus and verify:
  1. Each individual test case produces the expected outcome
  2. Category-level accuracy meets minimum thresholds
  3. Overall accuracy is acceptable

This framework does NOT fabricate scientific data. All test cases represent
known VASP configuration patterns with well-understood diagnostic expectations.
"""

import json
import os
import sys

import pytest
from lsprotocol.types import DiagnosticSeverity

# Add the validation directory to path for corpus import
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "validation"))

# Import from the validation package
from validation import (
    VALIDATION_CORPUS,
    ExpectedOutcome,
    ValidationAccuracyRunner,
    ValidationTestCase,
)

from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.parsers.incar_parser import INCARParser
from vasp_lsp.parsers.kpoints_parser import KPOINTSParser
from vasp_lsp.parsers.poscar_parser import POSCARParser
from vasp_lsp.parsers.potcar_parser import POTCARParser

# ---------------------------------------------------------------------------
# Individual test case parameterization
# ---------------------------------------------------------------------------


def _case_id(val):
    if isinstance(val, ValidationTestCase):
        return val.name
    return str(val)


@pytest.mark.parametrize("tc", VALIDATION_CORPUS, ids=_case_id)
class TestValidationCorpus:
    """Run each validation corpus test case as an individual test."""

    def test_case_outcome(self, tc: ValidationTestCase) -> None:
        """Each test case must produce the expected diagnostic outcome."""
        provider = DiagnosticsProvider()
        diags = provider.get_diagnostics(
            tc.content,
            f"file:///test/{tc.file_type}",
            workspace_documents=tc.workspace_files,
        )
        messages = [d.message.lower() for d in diags]

        if tc.expected_outcome == ExpectedOutcome.SHOULD_DIAGNOSE:
            for pat in tc.expected_patterns:
                assert any(
                    pat.lower() in msg for msg in messages
                ), f"Expected pattern '{pat}' not found in diagnostics: {messages}"
        else:  # SHOULD_BE_CLEAN
            # No error or warning diagnostics should be present
            error_warnings = [
                d
                for d in diags
                if d.severity in (DiagnosticSeverity.Error, DiagnosticSeverity.Warning)
            ]
            assert error_warnings == [], (
                f"Expected clean but got {len(error_warnings)} error/warning diagnostics: "
                f"{[d.message for d in error_warnings]}"
            )


# ---------------------------------------------------------------------------
# Category-level accuracy thresholds
# ---------------------------------------------------------------------------


class TestCategoryAccuracy:
    """Verify category-level accuracy meets minimum thresholds."""

    @pytest.fixture(autouse=True)
    def _run_corpus(self):
        self.runner = ValidationAccuracyRunner()
        self.report = self.runner.run()

    def test_overall_accuracy_above_90_percent(self) -> None:
        """Overall accuracy must be >= 90%."""
        total = self.report["total_cases"]
        passed = self.report["passed"]
        accuracy = passed / total if total > 0 else 0
        assert accuracy >= 0.90, (
            f"Overall accuracy {accuracy:.1%} is below 90%.\n"
            f"Report:\n{json.dumps(self.report, indent=2)}"
        )

    def test_incar_conflicts_accuracy(self) -> None:
        """INCAR conflicts category accuracy must be >= 90%."""
        cat = self.report["categories"].get("incar_conflicts")
        if cat:
            assert cat["accuracy"] >= 0.90, (
                f"INCAR conflicts accuracy {cat['accuracy']:.1%} below 90%.\n" f"Details: {cat}"
            )

    def test_poscar_structure_accuracy(self) -> None:
        """POSCAR structure category accuracy must be >= 90%."""
        cat = self.report["categories"].get("poscar_structure")
        if cat:
            assert cat["accuracy"] >= 0.90, (
                f"POSCAR structure accuracy {cat['accuracy']:.1%} below 90%.\n" f"Details: {cat}"
            )

    def test_kpoints_accuracy(self) -> None:
        """KPOINTS category accuracy must be >= 90%."""
        cat = self.report["categories"].get("kpoints")
        if cat:
            assert cat["accuracy"] >= 0.90, (
                f"KPOINTS accuracy {cat['accuracy']:.1%} below 90%.\n" f"Details: {cat}"
            )

    def test_cross_file_accuracy(self) -> None:
        """Cross-file category accuracy must be >= 90%."""
        cat = self.report["categories"].get("cross_file")
        if cat:
            assert cat["accuracy"] >= 0.90, (
                f"Cross-file accuracy {cat['accuracy']:.1%} below 90%.\n" f"Details: {cat}"
            )


# ---------------------------------------------------------------------------
# Corpus completeness checks
# ---------------------------------------------------------------------------


class TestCorpusCompleteness:
    """Verify the test corpus covers required categories and minimum case counts."""

    def test_corpus_has_minimum_cases(self) -> None:
        """Corpus must have at least 30 test cases."""
        assert (
            len(VALIDATION_CORPUS) >= 30
        ), f"Corpus has only {len(VALIDATION_CORPUS)} cases, minimum is 30"

    def test_corpus_covers_incar_conflicts(self) -> None:
        """Corpus must include INCAR conflict cases."""
        cats = {tc.category for tc in VALIDATION_CORPUS}
        assert "incar_conflicts" in cats

    def test_corpus_covers_poscar_structure(self) -> None:
        """Corpus must include POSCAR structure cases."""
        cats = {tc.category for tc in VALIDATION_CORPUS}
        assert "poscar_structure" in cats

    def test_corpus_covers_kpoints(self) -> None:
        """Corpus must include KPOINTS cases."""
        cats = {tc.category for tc in VALIDATION_CORPUS}
        assert "kpoints" in cats

    def test_corpus_covers_cross_file(self) -> None:
        """Corpus must include cross-file cases."""
        cats = {tc.category for tc in VALIDATION_CORPUS}
        assert "cross_file" in cats

    def test_corpus_has_positive_and_negative_cases(self) -> None:
        """Each category must have both positive and negative test cases."""
        by_category: dict = {}
        for tc in VALIDATION_CORPUS:
            by_category.setdefault(tc.category, []).append(tc)

        for cat, cases in by_category.items():
            outcomes = {tc.expected_outcome for tc in cases}
            assert (
                ExpectedOutcome.SHOULD_DIAGNOSE in outcomes
            ), f"Category '{cat}' has no positive (SHOULD_DIAGNOSE) test cases"
            assert (
                ExpectedOutcome.SHOULD_BE_CLEAN in outcomes
            ), f"Category '{cat}' has no negative (SHOULD_BE_CLEAN) test cases"

    def test_corpus_has_minimum_per_category(self) -> None:
        """Each category must have at least 5 test cases."""
        by_category: dict = {}
        for tc in VALIDATION_CORPUS:
            by_category.setdefault(tc.category, []).append(tc)

        for cat, cases in by_category.items():
            assert len(cases) >= 5, f"Category '{cat}' has only {len(cases)} cases, minimum is 5"


# ---------------------------------------------------------------------------
# Parser accuracy subset
# ---------------------------------------------------------------------------


class TestParserAccuracy:
    """Verify parser accuracy on known good and bad inputs."""

    def test_parse_valid_incar(self) -> None:
        """Valid INCAR parses without errors."""
        parser = INCARParser("SYSTEM = test\nISMEAR = 0\nSIGMA = 0.2\n")
        params = parser.parse()
        assert len(parser.get_errors()) == 0
        assert "SYSTEM" in params
        assert "ISMEAR" in params

    def test_parse_duplicate_incar_tag(self) -> None:
        """Duplicate INCAR tag produces a warning."""
        parser = INCARParser("ISMEAR = 0\nISMEAR = 1\n")
        parser.parse()
        errors = parser.get_errors()
        assert any("duplicate" in e["message"].lower() for e in errors)

    def test_parse_valid_poscar(self) -> None:
        """Valid POSCAR parses without errors."""
        content = """Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
2
Direct
0.0 0.0 0.0
0.5 0.5 0.5
"""
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is not None
        assert len(parser.get_errors()) == 0

    def test_parse_invalid_poscar_scale(self) -> None:
        """Invalid POSCAR scale factor produces an error."""
        content = """Test System
invalid
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
1
Direct
0.0 0.0 0.0
"""
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is None
        assert len(parser.get_errors()) > 0

    def test_parse_valid_kpoints(self) -> None:
        """Valid KPOINTS parses without errors."""
        content = """Automatic
0
Gamma
4 4 4
0 0 0
"""
        parser = KPOINTSParser(content)
        result = parser.parse()
        assert result is not None
        assert len(parser.get_errors()) == 0

    def test_parse_valid_potcar(self) -> None:
        """Valid POTCAR parses without errors."""
        content = """PAW_PBE Si 05Jan2001
   4.00000000
ENMAX =  245.345; ENMIN =  143.678
"""
        parser = POTCARParser(content)
        result = parser.parse()
        assert result is not None
        assert len(result.entries) == 1
        assert result.entries[0].element == "Si"
        assert result.entries[0].enmax == pytest.approx(245.345)

    def test_parse_potcar_multiple_species(self) -> None:
        """Multi-species POTCAR parses correctly."""
        content = """PAW_PBE Si 05Jan2001
   4.00000000
ENMAX =  245.345; ENMIN =  143.678
PAW_PBE O 08Apr2002
   3.50000000
ENMAX =  400.000; ENMIN =  225.000
"""
        parser = POTCARParser(content)
        result = parser.parse()
        assert result is not None
        assert len(result.entries) == 2
        assert result.entries[0].element == "Si"
        assert result.entries[1].element == "O"
