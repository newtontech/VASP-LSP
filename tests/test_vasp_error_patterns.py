from vasp_lsp.schemas.vasp_error_patterns import (
    DEFAULT_VASP_ERROR_PATTERNS,
    RuntimeSuggestedAction,
    VASPErrorPattern,
    match_vasp_error_patterns,
    validate_vasp_error_patterns,
)


def test_default_patterns_have_unique_ids_and_required_metadata() -> None:
    validate_vasp_error_patterns(DEFAULT_VASP_ERROR_PATTERNS)

    ids = [pattern.id for pattern in DEFAULT_VASP_ERROR_PATTERNS]
    assert ids == sorted(ids)
    assert len(ids) == len(set(ids))
    assert "vasp.runtime.edddav_zhegv" in ids
    assert "vasp.runtime.invgrp_symmetry" in ids

    electronic = next(
        pattern
        for pattern in DEFAULT_VASP_ERROR_PATTERNS
        if pattern.id == "vasp.runtime.edddav_zhegv"
    )
    assert electronic.severity == "error"
    assert electronic.confidence >= 0.8
    assert "INCAR" in electronic.related_files
    assert any(action.safe_to_auto_apply for action in electronic.suggested_actions)
    assert any(not action.safe_to_auto_apply for action in electronic.suggested_actions)


def test_validate_patterns_rejects_duplicate_ids() -> None:
    pattern = VASPErrorPattern(
        id="vasp.runtime.duplicate",
        patterns=(r"duplicate",),
        severity="error",
        confidence=1.0,
        category="runtime",
        related_files=("OUTCAR",),
        suggested_actions=(RuntimeSuggestedAction("Inspect duplicate"),),
    )

    try:
        validate_vasp_error_patterns((pattern, pattern))
    except ValueError as exc:
        assert "Duplicate VASP error pattern id" in str(exc)
    else:
        raise AssertionError("duplicate IDs should fail validation")


def test_runtime_pattern_matching_is_line_based_and_deterministic() -> None:
    content = """running
Error EDDDAV: Call to ZHEGV failed. Returncode = 5
INVGRP: inverse of rotation matrix was not found
"""

    matches = match_vasp_error_patterns(content)

    assert [match.pattern.id for match in matches] == [
        "vasp.runtime.edddav_zhegv",
        "vasp.runtime.invgrp_symmetry",
    ]
    assert [match.line_index for match in matches] == [1, 2]
    assert matches[0].line_text.startswith("Error EDDDAV")


def test_runtime_pattern_matching_ignores_clean_logs() -> None:
    content = """DAV:  1  -0.12345
reached required accuracy - stopping structural energy minimisation
General timing and accounting informations for this job:
"""

    assert match_vasp_error_patterns(content) == []
