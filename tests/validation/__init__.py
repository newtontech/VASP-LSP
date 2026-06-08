"""Validation accuracy testing framework for VASP-LSP.

This framework provides:
  - A structured test corpus of VASP input files with expected diagnostic outcomes
  - Category-level accuracy metrics (precision, recall, F1)
  - A JSON report generator for CI integration

Each test case defines:
  - Input file content (INCAR, POSCAR, KPOINTS, POTCAR)
  - Expected diagnostics (list of expected messages or patterns)
  - Category label for grouping
  - Whether the case should produce diagnostics (positive) or not (negative)

This does NOT fabricate scientific data. All test cases represent known
VASP configuration patterns with well-understood diagnostic expectations.

Supported categories (as of this version):
  - incar_conflicts: NCORE/NPAR, LDAU completeness, ISPIN/MAGMOM, ISMEAR/SIGMA
  - poscar_structure: scale factor, atoms, coordinates, lattice, duplicates
  - kpoints: grid values, weights, coordinates, density
  - cross_file: MAGMOM/POSCAR, KSPACING/KPOINTS, ENCUT/POTCAR, species order
  - parser_accuracy: parser correctness on valid/invalid inputs
"""

import json
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional

from vasp_lsp.features.diagnostics import DiagnosticsProvider

# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------


class ExpectedOutcome(Enum):
    """Expected diagnostic outcome for a test case."""

    SHOULD_DIAGNOSE = "should_diagnose"  # Expect at least one matching diagnostic
    SHOULD_BE_CLEAN = "should_be_clean"  # Expect no error/warning diagnostics


@dataclass
class ValidationTestCase:
    """A single validation test case with expected outcome."""

    name: str
    category: str
    file_type: str  # "INCAR", "POSCAR", "KPOINTS"
    content: str
    expected_outcome: ExpectedOutcome
    expected_patterns: List[str]  # Substrings expected in diagnostic messages
    workspace_files: Optional[Dict[str, str]] = None
    description: str = ""


@dataclass
class CategoryMetrics:
    """Accuracy metrics for a single category."""

    category: str
    total: int = 0
    true_positives: int = 0  # Correctly detected issue
    true_negatives: int = 0  # Correctly reported clean
    false_positives: int = 0  # Incorrectly flagged clean input
    false_negatives: int = 0  # Missed an expected issue

    @property
    def precision(self) -> float:
        denom = self.true_positives + self.false_positives
        return self.true_positives / denom if denom > 0 else 0.0

    @property
    def recall(self) -> float:
        denom = self.true_positives + self.false_negatives
        return self.true_positives / denom if denom > 0 else 0.0

    @property
    def f1(self) -> float:
        denom = self.precision + self.recall
        return 2 * self.precision * self.recall / denom if denom > 0 else 0.0

    @property
    def accuracy(self) -> float:
        denom = self.total
        return (self.true_positives + self.true_negatives) / denom if denom > 0 else 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "category": self.category,
            "total": self.total,
            "true_positives": self.true_positives,
            "true_negatives": self.true_negatives,
            "false_positives": self.false_positives,
            "false_negatives": self.false_negatives,
            "precision": round(self.precision, 4),
            "recall": round(self.recall, 4),
            "f1": round(self.f1, 4),
            "accuracy": round(self.accuracy, 4),
        }


# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

POSCAR_VALID = """Test System
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

POTCAR_PBE_SI = """PAW_PBE Si 05Jan2001
   4.00000000
ENMAX =  245.345; ENMIN =  143.678
"""

POTCAR_PBE_SI_O = """PAW_PBE Si 05Jan2001
   4.00000000
ENMAX =  245.345; ENMIN =  143.678
PAW_PBE O 08Apr2002
   3.50000000
ENMAX =  400.000; ENMIN =  225.000
"""

KPOINTS_VALID = """Automatic
0
Gamma
4 4 4
0 0 0
"""


# ---------------------------------------------------------------------------
# Test corpus
#
# Only includes checks that are implemented on the current main branch.
# When new validations are added, extend the corpus in a follow-up commit.
# ---------------------------------------------------------------------------

VALIDATION_CORPUS: List[ValidationTestCase] = [
    # ===================================================================
    # INCAR parameter conflicts (existing on main)
    # ===================================================================
    ValidationTestCase(
        name="ismear_ge0_without_sigma",
        category="incar_conflicts",
        file_type="INCAR",
        content="ISMEAR = 1\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["SIGMA"],
        description="ISMEAR >= 0 without SIGMA set",
    ),
    ValidationTestCase(
        name="ismear_with_sigma_clean",
        category="incar_conflicts",
        file_type="INCAR",
        content="ISMEAR = 0\nSIGMA = 0.2\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        description="ISMEAR=0 with SIGMA: no conflict",
    ),
    ValidationTestCase(
        name="ncore_npar_conflict",
        category="incar_conflicts",
        file_type="INCAR",
        content="NCORE = 4\nNPAR = 4\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["NCORE", "NPAR"],
        description="NCORE and NPAR set together",
    ),
    ValidationTestCase(
        name="ncore_only_clean",
        category="incar_conflicts",
        file_type="INCAR",
        content="NCORE = 4\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        description="NCORE alone: no conflict",
    ),
    ValidationTestCase(
        name="ldau_missing_ldautype",
        category="incar_conflicts",
        file_type="INCAR",
        content="LDAU = .TRUE.\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["LDAUTYPE"],
        description="LDAU=.TRUE. without LDAUTYPE",
    ),
    ValidationTestCase(
        name="ldau_complete_clean",
        category="incar_conflicts",
        file_type="INCAR",
        content="LDAU = .TRUE.\nLDAUTYPE = 2\nLDAUL = 2\nLDAUU = 3.0\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        description="LDAU with all required tags: clean",
    ),
    ValidationTestCase(
        name="ispin2_without_magmom",
        category="incar_conflicts",
        file_type="INCAR",
        content="ISPIN = 2\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["MAGMOM"],
        description="ISPIN=2 without MAGMOM",
    ),
    ValidationTestCase(
        name="ispin1_clean",
        category="incar_conflicts",
        file_type="INCAR",
        content="ISPIN = 1\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        description="ISPIN=1: no MAGMOM needed",
    ),
    ValidationTestCase(
        name="incar_clean_basic",
        category="incar_conflicts",
        file_type="INCAR",
        content="SYSTEM = test\nISMEAR = 0\nSIGMA = 0.2\nENCUT = 400\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        description="Clean basic INCAR with no conflicts",
    ),
    # ===================================================================
    # POSCAR structure validation
    # ===================================================================
    ValidationTestCase(
        name="poscar_negative_scale",
        category="poscar_structure",
        file_type="POSCAR",
        content="""Test System
-1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
1
Direct
0.0 0.0 0.0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["negative scale factor"],
        description="Negative scale factor in POSCAR",
    ),
    ValidationTestCase(
        name="poscar_zero_atoms",
        category="poscar_structure",
        file_type="POSCAR",
        content="""Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
0
Direct
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["no atoms"],
        description="Zero atom count in POSCAR",
    ),
    ValidationTestCase(
        name="poscar_direct_out_of_range",
        category="poscar_structure",
        file_type="POSCAR",
        content="""Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
1
Direct
2.5 0.0 0.0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["outside typical range"],
        description="Direct coordinate outside [0, 1]",
    ),
    ValidationTestCase(
        name="poscar_duplicate_atoms",
        category="poscar_structure",
        file_type="POSCAR",
        content="""Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
2
Direct
0.25 0.25 0.25
0.25 0.25 0.25
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["identical coordinates"],
        description="Duplicate atom positions",
    ),
    ValidationTestCase(
        name="poscar_zero_lattice_vector",
        category="poscar_structure",
        file_type="POSCAR",
        content="""Test System
1.0
0.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
1
Direct
0.0 0.0 0.0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["lattice vector length is zero"],
        description="Zero-length lattice vector",
    ),
    ValidationTestCase(
        name="poscar_extreme_scale",
        category="poscar_structure",
        file_type="POSCAR",
        content="""Test System
200.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si
1
Direct
0.0 0.0 0.0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["extreme magnitude"],
        description="Extreme scale factor",
    ),
    ValidationTestCase(
        name="poscar_clean",
        category="poscar_structure",
        file_type="POSCAR",
        content=POSCAR_VALID,
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        description="Valid POSCAR with no issues",
    ),
    # ===================================================================
    # KPOINTS validation
    # ===================================================================
    ValidationTestCase(
        name="kpoints_zero_grid",
        category="kpoints",
        file_type="KPOINTS",
        content="""Automatic
0
Gamma
0 4 4
0 0 0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["not positive"],
        description="Zero grid value",
    ),
    ValidationTestCase(
        name="kpoints_sparse_grid",
        category="kpoints",
        file_type="KPOINTS",
        content="""Automatic
0
Gamma
1 1 1
0 0 0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["very sparse"],
        description="Very sparse k-point grid",
    ),
    ValidationTestCase(
        name="kpoints_dense_grid",
        category="kpoints",
        file_type="KPOINTS",
        content="""Dense grid
0
Gamma
60 60 60
0 0 0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["very dense"],
        description="Very dense k-point grid",
    ),
    ValidationTestCase(
        name="kpoints_zero_weight",
        category="kpoints",
        file_type="KPOINTS",
        content="""Explicit k-points
2
Reciprocal
0.0 0.0 0.0 0.0
0.5 0.5 0.5 1.0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["weight 0.0"],
        description="Zero-weight k-point",
    ),
    ValidationTestCase(
        name="kpoints_out_of_range",
        category="kpoints",
        file_type="KPOINTS",
        content="""Explicit k-points
1
Reciprocal
2.0 0.0 0.0 1.0
""",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["outside the typical reciprocal range"],
        description="K-point outside [-1, 1]",
    ),
    ValidationTestCase(
        name="kpoints_clean",
        category="kpoints",
        file_type="KPOINTS",
        content=KPOINTS_VALID,
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        description="Valid KPOINTS file",
    ),
    # ===================================================================
    # Cross-file: ENCUT / POTCAR
    # ===================================================================
    ValidationTestCase(
        name="encut_below_enmax",
        category="cross_file",
        file_type="INCAR",
        content="ENCUT = 200\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["ENMAX"],
        workspace_files={"file:///test/POTCAR": POTCAR_PBE_SI},
        description="ENCUT below POTCAR ENMAX",
    ),
    ValidationTestCase(
        name="encut_above_enmax_clean",
        category="cross_file",
        file_type="INCAR",
        content="ENCUT = 400\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        workspace_files={"file:///test/POTCAR": POTCAR_PBE_SI},
        description="ENCUT well above ENMAX",
    ),
    # ===================================================================
    # Cross-file: POSCAR / POTCAR species order
    # ===================================================================
    ValidationTestCase(
        name="species_order_mismatch",
        category="cross_file",
        file_type="INCAR",
        content="SYSTEM = test\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["species order"],
        workspace_files={
            "file:///test/POSCAR": """Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
O Si
1 1
Direct
0.0 0.0 0.0
0.5 0.5 0.5
""",
            "file:///test/POTCAR": POTCAR_PBE_SI_O,
        },
        description="POSCAR species order differs from POTCAR",
    ),
    ValidationTestCase(
        name="species_order_match",
        category="cross_file",
        file_type="INCAR",
        content="SYSTEM = test\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        workspace_files={
            "file:///test/POSCAR": """Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si O
1 1
Direct
0.0 0.0 0.0
0.5 0.5 0.5
""",
            "file:///test/POTCAR": POTCAR_PBE_SI_O,
        },
        description="POSCAR species order matches POTCAR",
    ),
    # ===================================================================
    # Cross-file: MAGMOM count
    # ===================================================================
    ValidationTestCase(
        name="magmom_count_mismatch",
        category="cross_file",
        file_type="INCAR",
        content="MAGMOM = 1.0 2.0 3.0\nISPIN = 2\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["MAGMOM", "entries"],
        workspace_files={"file:///test/POSCAR": POSCAR_VALID},
        description="MAGMOM count does not match POSCAR atom count",
    ),
    ValidationTestCase(
        name="magmom_count_match",
        category="cross_file",
        file_type="INCAR",
        content="MAGMOM = 1.0 2.0\nISPIN = 2\n",
        expected_outcome=ExpectedOutcome.SHOULD_BE_CLEAN,
        expected_patterns=[],
        workspace_files={"file:///test/POSCAR": POSCAR_VALID},
        description="MAGMOM count matches POSCAR atom count",
    ),
    # ===================================================================
    # Cross-file: KSPACING + KPOINTS conflict
    # ===================================================================
    ValidationTestCase(
        name="kspacing_with_kpoints",
        category="cross_file",
        file_type="INCAR",
        content="KSPACING = 0.5\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["KSPACING", "KPOINTS"],
        workspace_files={"file:///test/KPOINTS": KPOINTS_VALID},
        description="KSPACING set with KPOINTS file present",
    ),
    # ===================================================================
    # Cross-file: LDAU count mismatch
    # ===================================================================
    ValidationTestCase(
        name="ldau_count_mismatch",
        category="cross_file",
        file_type="INCAR",
        content="LDAU = .TRUE.\nLDAUU = 3.0\nLDAUTYPE = 2\nLDAUL = 2 -1\n",
        expected_outcome=ExpectedOutcome.SHOULD_DIAGNOSE,
        expected_patterns=["LDAUU", "entries"],
        workspace_files={"file:///test/POSCAR": """Test System
1.0
5.0 0.0 0.0
0.0 5.0 0.0
0.0 0.0 5.0
Si O
1 1
Direct
0.0 0.0 0.0
0.5 0.5 0.5
"""},
        description="LDAUU has 1 entry but POSCAR has 2 species",
    ),
]


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------


class ValidationAccuracyRunner:
    """Run the validation corpus and compute accuracy metrics."""

    def __init__(self, test_cases: Optional[List[ValidationTestCase]] = None):
        self.test_cases = test_cases or VALIDATION_CORPUS
        self.provider = DiagnosticsProvider()
        self.results: List[Dict[str, Any]] = []
        self.category_metrics: Dict[str, CategoryMetrics] = {}

    def run(self) -> Dict[str, Any]:
        """Run all test cases and compute metrics."""
        self.results = []
        self.category_metrics = {}

        for tc in self.test_cases:
            if tc.category not in self.category_metrics:
                self.category_metrics[tc.category] = CategoryMetrics(category=tc.category)

            metrics = self.category_metrics[tc.category]
            metrics.total += 1

            # Get diagnostics
            diags = self.provider.get_diagnostics(
                tc.content,
                f"file:///test/{tc.file_type}",
                workspace_documents=tc.workspace_files,
            )

            # Check if expected patterns are found
            messages = [d.message.lower() for d in diags]
            found = all(any(pat.lower() in msg for msg in messages) for pat in tc.expected_patterns)

            if tc.expected_outcome == ExpectedOutcome.SHOULD_DIAGNOSE:
                if found:
                    metrics.true_positives += 1
                    passed = True
                else:
                    metrics.false_negatives += 1
                    passed = False
            else:  # SHOULD_BE_CLEAN
                # For clean cases, check there are no error/warning diagnostics
                from lsprotocol.types import DiagnosticSeverity

                error_warnings = [
                    d
                    for d in diags
                    if d.severity in (DiagnosticSeverity.Error, DiagnosticSeverity.Warning)
                ]
                if not error_warnings:
                    metrics.true_negatives += 1
                    passed = True
                else:
                    metrics.false_positives += 1
                    passed = False

            self.results.append(
                {
                    "name": tc.name,
                    "category": tc.category,
                    "expected": tc.expected_outcome.value,
                    "passed": passed,
                    "diagnostic_count": len(diags),
                }
            )

        return self._build_report()

    def _build_report(self) -> Dict[str, Any]:
        """Build the accuracy report."""
        category_reports = {}
        for cat, metrics in sorted(self.category_metrics.items()):
            category_reports[cat] = metrics.to_dict()

        total_tp = sum(m.true_positives for m in self.category_metrics.values())
        total_tn = sum(m.true_negatives for m in self.category_metrics.values())
        total_fp = sum(m.false_positives for m in self.category_metrics.values())
        total_fn = sum(m.false_negatives for m in self.category_metrics.values())
        total = sum(m.total for m in self.category_metrics.values())

        precision = total_tp / (total_tp + total_fp) if (total_tp + total_fp) > 0 else 0.0
        recall = total_tp / (total_tp + total_fn) if (total_tp + total_fn) > 0 else 0.0
        f1 = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0.0

        return {
            "total_cases": total,
            "passed": total_tp + total_tn,
            "failed": total_fp + total_fn,
            "overall_precision": round(precision, 4),
            "overall_recall": round(recall, 4),
            "overall_f1": round(f1, 4),
            "categories": category_reports,
        }


def run_accuracy_report() -> str:
    """Run the validation corpus and return a JSON report."""
    runner = ValidationAccuracyRunner()
    report = runner.run()
    return json.dumps(report, indent=2)
