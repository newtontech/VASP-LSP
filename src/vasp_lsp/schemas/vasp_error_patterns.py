"""Runtime VASP error pattern registry.

This registry is intentionally stdlib-only so public CI can classify golden
runtime logs without installing VASP or a YAML parser.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Iterable, Sequence


@dataclass(frozen=True)
class RuntimeSuggestedAction:
    """A recovery action linked to a runtime pattern."""

    title: str
    safe_to_auto_apply: bool = True
    target_file: str = "INCAR"


@dataclass(frozen=True)
class VASPErrorPattern:
    """A structured VASP runtime error signature."""

    id: str
    patterns: tuple[str, ...]
    severity: str
    confidence: float
    category: str
    related_files: tuple[str, ...]
    suggested_actions: tuple[RuntimeSuggestedAction, ...]


@dataclass(frozen=True)
class RuntimePatternMatch:
    """A single line-level pattern match in a VASP runtime log."""

    pattern: VASPErrorPattern
    line_index: int
    line_text: str
    regex: str


DEFAULT_VASP_ERROR_PATTERNS: tuple[VASPErrorPattern, ...] = (
    VASPErrorPattern(
        id="vasp.runtime.edddav_zhegv",
        patterns=(
            r"\bEDDDAV\b.*\bZHEGV\b",
            r"\bDavidson algorithm has failed\b",
        ),
        severity="error",
        confidence=0.9,
        category="electronic_minimization",
        related_files=("INCAR", "POSCAR", "CHGCAR", "WAVECAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set ALGO = Normal"),
            RuntimeSuggestedAction(
                "Suggest removing CHGCAR/WAVECAR after confirmation",
                safe_to_auto_apply=False,
                target_file="CHGCAR/WAVECAR",
            ),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.eddrmm_zhegv",
        patterns=(r"\bEDDRMM\b.*\bZHEGV\b", r"\bRMM-DIIS\b.*\bfailed\b"),
        severity="error",
        confidence=0.88,
        category="electronic_minimization",
        related_files=("INCAR", "POSCAR", "CHGCAR", "WAVECAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set ALGO = Normal"),
            RuntimeSuggestedAction("Lower POTIM"),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.edwav_gradient",
        patterns=(r"\bEDWAV\b.*gradient is not orthogonal",),
        severity="warning",
        confidence=0.75,
        category="electronic_minimization",
        related_files=("INCAR", "POTCAR", "WAVECAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set ALGO = Normal"),
            RuntimeSuggestedAction(
                "Suggest removing WAVECAR after confirmation",
                safe_to_auto_apply=False,
                target_file="WAVECAR",
            ),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.fock_kpoints",
        patterns=(r"Internal error in FOCK ACC.*number of k-points incorrect",),
        severity="error",
        confidence=0.85,
        category="parallel_restart",
        related_files=("INCAR", "KPOINTS", "OUTCAR", "stdout"),
        suggested_actions=(RuntimeSuggestedAction("Check NKRED divisibility against KPOINTS"),),
    ),
    VASPErrorPattern(
        id="vasp.runtime.integer_divide_by_zero_parallel",
        patterns=(r"Integer divide by zero",),
        severity="error",
        confidence=0.7,
        category="parallel_restart",
        related_files=("INCAR", "job script", "OUTCAR", "stdout"),
        suggested_actions=(RuntimeSuggestedAction("Check KPAR/NCORE/NPAR against MPI ranks"),),
    ),
    VASPErrorPattern(
        id="vasp.runtime.invgrp_symmetry",
        patterns=(r"\bINVGRP\b.*inverse of rotation matrix was not found",),
        severity="error",
        confidence=0.9,
        category="symmetry",
        related_files=("INCAR", "POSCAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set ISYM = 0"),
            RuntimeSuggestedAction("Set SYMPREC = 1E-6"),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.mpi_collective",
        patterns=(r"\bMPI\b.*\b(Allreduce|Allgatherv)\b",),
        severity="error",
        confidence=0.72,
        category="parallel_restart",
        related_files=("job script", "INCAR", "OUTCAR", "stderr", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction(
                "Suggest checking MPI compatibility settings",
                safe_to_auto_apply=False,
                target_file="job script",
            ),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.nkred_kmesh",
        patterns=(r"\bNKRED(X|Y|Z)?\b.*does not divide",),
        severity="error",
        confidence=0.9,
        category="parallel_restart",
        related_files=("INCAR", "KPOINTS", "OUTCAR", "stdout"),
        suggested_actions=(RuntimeSuggestedAction("Remove or adjust NKRED/NKREDX/NKREDY/NKREDZ"),),
    ),
    VASPErrorPattern(
        id="vasp.runtime.no_initial_positions",
        patterns=(r"No initial positions read in",),
        severity="error",
        confidence=0.9,
        category="input_structure",
        related_files=("POSCAR", "CONTCAR", "OUTCAR", "stdout"),
        suggested_actions=(RuntimeSuggestedAction("Check POSCAR coordinate rows"),),
    ),
    VASPErrorPattern(
        id="vasp.runtime.pricel_symmetry",
        patterns=(r"\bPRICEL\b.*number of cells and vectors did not agree",),
        severity="error",
        confidence=0.88,
        category="symmetry",
        related_files=("INCAR", "POSCAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set SYMPREC = 1E-6"),
            RuntimeSuggestedAction("Set ISYM = 0"),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.pssyevx_eigenvalues",
        patterns=(r"\bPSSYEVX\b.*not enough eigenvalues found",),
        severity="error",
        confidence=0.86,
        category="electronic_minimization",
        related_files=("INCAR", "POSCAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set ALGO = Normal"),
            RuntimeSuggestedAction("Lower POTIM"),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.sgrcon_symmetry",
        patterns=(r"\bSGRCON\b.*non-?integer element in rotation matrix",),
        severity="error",
        confidence=0.88,
        category="symmetry",
        related_files=("INCAR", "POSCAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set SYMPREC = 1E-6"),
            RuntimeSuggestedAction("Set ISYM = 0"),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.sgrgen_symmetry",
        patterns=(r"\bSGRGEN\b",),
        severity="error",
        confidence=0.78,
        category="symmetry",
        related_files=("INCAR", "POSCAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set SYMPREC = 1E-6"),
            RuntimeSuggestedAction("Set ISYM = 0"),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.signal9_oom",
        patterns=(r"\bsignal\s+9\b", r"\bKilled\b.*\bmemory\b", r"\bOut Of Memory\b"),
        severity="error",
        confidence=0.7,
        category="parallel_restart",
        related_files=("job script", "INCAR", "OUTCAR", "stderr", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction(
                "Suggest reducing memory pressure or changing parallel layout",
                safe_to_auto_apply=False,
                target_file="job script",
            ),
        ),
    ),
    VASPErrorPattern(
        id="vasp.runtime.zpotrf_failed",
        patterns=(r"\bZPOTRF\b.*failed",),
        severity="error",
        confidence=0.82,
        category="electronic_minimization",
        related_files=("INCAR", "POSCAR", "WAVECAR", "OUTCAR", "stdout"),
        suggested_actions=(
            RuntimeSuggestedAction("Set ALGO = Normal"),
            RuntimeSuggestedAction(
                "Suggest removing WAVECAR after confirmation",
                safe_to_auto_apply=False,
                target_file="WAVECAR",
            ),
        ),
    ),
)


def validate_vasp_error_patterns(patterns: Sequence[VASPErrorPattern]) -> None:
    """Validate registry invariants used by runtime diagnostics."""
    seen: set[str] = set()
    for pattern in patterns:
        if pattern.id in seen:
            raise ValueError(f"Duplicate VASP error pattern id: {pattern.id}")
        seen.add(pattern.id)
        if not pattern.patterns:
            raise ValueError(f"VASP error pattern {pattern.id} has no regex patterns")
        if pattern.severity not in {"error", "warning", "information", "hint"}:
            raise ValueError(f"VASP error pattern {pattern.id} has invalid severity")
        if not 0.0 <= pattern.confidence <= 1.0:
            raise ValueError(f"VASP error pattern {pattern.id} has invalid confidence")
        for regex in pattern.patterns:
            re.compile(regex, re.IGNORECASE)


def match_vasp_error_patterns(
    content: str,
    patterns: Iterable[VASPErrorPattern] = DEFAULT_VASP_ERROR_PATTERNS,
) -> list[RuntimePatternMatch]:
    """Return deterministic line-level matches for VASP runtime log content."""
    pattern_list = tuple(patterns)
    validate_vasp_error_patterns(pattern_list)
    matches: list[RuntimePatternMatch] = []
    lines = content.splitlines()
    for line_index, line in enumerate(lines):
        for pattern in pattern_list:
            for regex in pattern.patterns:
                if re.search(regex, line, re.IGNORECASE):
                    matches.append(
                        RuntimePatternMatch(
                            pattern=pattern,
                            line_index=line_index,
                            line_text=line,
                            regex=regex,
                        )
                    )
                    break
    return sorted(
        matches,
        key=lambda match: (match.line_index, match.pattern.id, match.regex),
    )


validate_vasp_error_patterns(DEFAULT_VASP_ERROR_PATTERNS)
