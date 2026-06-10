"""Tests targeting uncovered lines in diagnostics, quickfixes, and parsers.

These tests focus on the specific uncovered code paths identified by coverage
analysis, including:
- Diagnostics: _workspace_info with no param, _expanded_array_length with *-multiplier,
  _uri_to_path with non-file scheme, invalid element symbols, kpoint weight sum checks
- Quickfixes: extreme scale factor fix, duplicate atoms fix, zero weight fix, dense grid fix
- POSCAR parser: extra lines detection
- POTCAR parser: empty content, error detection
"""

import os
import tempfile

import pytest
from lsprotocol.types import Diagnostic, DiagnosticSeverity, Position, Range

from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.features.quickfixes import QuickFixesProvider
from vasp_lsp.parsers.poscar_parser import POSCARParser
from vasp_lsp.parsers.potcar_parser import POTCARParser

# ---------------------------------------------------------------------------
# Diagnostics: workspace info with no anchor param
# ---------------------------------------------------------------------------


class TestDiagnosticsWorkspaceInfoNoAnchor:
    """Cover _workspace_info / _workspace_diagnostic when param is None."""

    def test_line_mode_kpoints_without_icharg_no_params(self):
        """Line-mode KPOINTS + INCAR with no ICHARG and no params at all."""
        diag = DiagnosticsProvider()
        incar_content = "# empty INCAR\n"
        # VASP format: comment, number, "Line-mode", reciprocal, points...
        kpoints_content = (
            "Band structure\n20\nLine-mode\nreciprocal\n" "0.0 0.0 0.0 GAMMA\n0.5 0.0 0.5 X\n"
        )

        # Provide workspace documents with KPOINTS but INCAR has no params
        workspace_docs = {
            "file:///work/KPOINTS": kpoints_content,
        }
        diagnostics = diag.get_diagnostics(incar_content, "file:///work/INCAR", workspace_docs)
        # Should produce a diagnostic about ICHARG recommendation even with no anchor
        messages = [d.message for d in diagnostics]
        assert any("ICHARG" in m for m in messages)


class TestDiagnosticsExpandedArrayLength:
    """Cover _expanded_array_length with VASP-style * multiplier syntax."""

    def test_star_multiplier_in_magmom(self):
        diag = DiagnosticsProvider()
        poscar_content = (
            "Si\n1.0\n5.0 0.0 0.0\n0.0 5.0 0.0\n0.0 0.0 5.0\n"
            "Si\n4\nDirect\n"
            "0.0 0.0 0.0\n0.25 0.25 0.25\n0.5 0.5 0.5\n0.75 0.75 0.75\n"
        )
        incar_content = "ISPIN = 2\nMAGMOM = 4*1.0\n"
        workspace_docs = {
            "file:///work/POSCAR": poscar_content,
        }
        diagnostics = diag.get_diagnostics(incar_content, "file:///work/INCAR", workspace_docs)
        # MAGMOM expanded to 4 entries, POSCAR has 4 atoms -> no MAGMOM count mismatch
        messages = [d.message for d in diagnostics]
        assert not any("MAGMOM" in m and "mismatch" in m.lower() for m in messages)


class TestDiagnosticsUriToPath:
    """Cover _uri_to_path edge cases."""

    def test_non_file_scheme_returns_none(self):
        diag = DiagnosticsProvider()
        # Use a non-file URI, should return [] because no file type match
        # but more importantly test _read_neighbor via workspace
        result = diag._uri_to_path("http://example.com/file")
        assert result is None

    def test_file_scheme_returns_path(self):
        diag = DiagnosticsProvider()
        result = diag._uri_to_path("file:///path/to/INCAR")
        assert result == "/path/to/INCAR"


class TestDiagnosticsReadNeighbor:
    """Cover _read_neighbor with filesystem reads."""

    def test_read_neighbor_from_disk(self):
        """Test that _read_neighbor reads from the actual filesystem."""
        diag = DiagnosticsProvider()
        with tempfile.TemporaryDirectory() as tmpdir:
            incar_path = os.path.join(tmpdir, "INCAR")
            poscar_path = os.path.join(tmpdir, "POSCAR")
            with open(incar_path, "w") as f:
                f.write("ENCUT = 520\n")
            with open(poscar_path, "w") as f:
                f.write(
                    "Si\n1.0\n5.0 0.0 0.0\n0.0 5.0 0.0\n0.0 0.0 5.0\nSi\n1\nDirect\n0.0 0.0 0.0\n"
                )

            incar_uri = f"file://{incar_path}"
            result = diag._read_neighbor(incar_uri, "POSCAR", None)
            assert result is not None
            assert "Si" in result

    def test_read_neighbor_missing_file(self):
        diag = DiagnosticsProvider()
        with tempfile.TemporaryDirectory() as tmpdir:
            incar_path = os.path.join(tmpdir, "INCAR")
            with open(incar_path, "w") as f:
                f.write("ENCUT = 520\n")
            incar_uri = f"file://{incar_path}"
            result = diag._read_neighbor(incar_uri, "MISSING_FILE", None)
            assert result is None

    def test_read_neighbor_non_file_uri(self):
        diag = DiagnosticsProvider()
        result = diag._read_neighbor("http://example.com/INCAR", "POSCAR", None)
        assert result is None


class TestDiagnosticsInvalidElementSymbols:
    """Cover the invalid element symbols check in POSCAR."""

    def test_invalid_element_symbols(self):
        diag = DiagnosticsProvider()
        content = (
            "test\n1.0\n1.0 0.0 0.0\n0.0 1.0 0.0\n0.0 0.0 1.0\n"
            "XXXX YYY\n1 1\nDirect\n0.0 0.0 0.0\n0.5 0.5 0.5\n"
        )
        diagnostics = diag.get_diagnostics(content, "file:///work/POSCAR")
        messages = [d.message for d in diagnostics]
        assert any("Invalid element" in m for m in messages)


class TestDiagnosticsKpointWeightSum:
    """Cover kpoint weight sum check."""

    def test_explicit_kpoints_weights_sum_not_one(self):
        diag = DiagnosticsProvider()
        content = (
            "Explicit kpoints\n"
            "3\n"
            "Reciprocal\n"
            "0.0 0.0 0.0 0.5\n"
            "0.5 0.5 0.5 0.3\n"
            "0.25 0.25 0.25 0.5\n"
        )
        diagnostics = diag.get_diagnostics(content, "file:///work/KPOINTS")
        messages = [d.message for d in diagnostics]
        assert any("weights sum" in m.lower() for m in messages)


class TestDiagnosticsKpointCenteringMonkhorst:
    """Cover Monkhorst-Pack centering hint."""

    def test_monkhorst_centering_hint(self):
        diag = DiagnosticsProvider()
        content = "Monkhorst-Pack grid\n0\nMonkhorst-Pack\n4 4 4\n0 0 0\n"
        diagnostics = diag.get_diagnostics(content, "file:///work/KPOINTS")
        messages = [d.message for d in diagnostics]
        assert any("Monkhorst-Pack" in m for m in messages)


class TestDiagnosticsKpointExplicitZeroWeight:
    """Cover explicit kpoint zero-weight detection."""

    def test_explicit_zero_weight_kpoint(self):
        diag = DiagnosticsProvider()
        content = "Explicit kpoints\n" "2\n" "Reciprocal\n" "0.0 0.0 0.0 0.5\n" "0.5 0.5 0.5 0.0\n"
        diagnostics = diag.get_diagnostics(content, "file:///work/KPOINTS")
        messages = [d.message for d in diagnostics]
        assert any("weight 0.0" in m for m in messages)


class TestDiagnosticsExpandedArrayNone:
    """Cover _expanded_array_length with None value."""

    def test_none_value(self):
        diag = DiagnosticsProvider()
        result = diag._expanded_array_length(None)
        assert result == 0


class TestDiagnosticsCHGCARWarning:
    """Cover CHGCAR check with ICHARG=1 or 11."""

    def test_icharg_1_no_chgcar(self):
        diag = DiagnosticsProvider()
        incar = "ICHARG = 1\n"
        with tempfile.TemporaryDirectory() as tmpdir:
            incar_path = os.path.join(tmpdir, "INCAR")
            with open(incar_path, "w") as f:
                f.write(incar)
            diagnostics = diag.get_diagnostics(incar, f"file://{incar_path}", None)
            messages = [d.message for d in diagnostics]
            assert any("CHGCAR" in m for m in messages)


# ---------------------------------------------------------------------------
# POSCAR parser: extra lines detection (line 244-245)
# ---------------------------------------------------------------------------


class TestPOSCARParserExtraLines:
    """Cover extra coordinate detection in POSCAR."""

    def test_extra_coordinate_rows(self):
        content = (
            "Si\n1.0\n5.0 0.0 0.0\n0.0 5.0 0.0\n0.0 0.0 5.0\n"
            "Si\n1\nDirect\n0.0 0.0 0.0\n0.5 0.5 0.5\n0.75 0.75 0.75\n"
        )
        parser = POSCARParser(content)
        result = parser.parse()
        assert result is not None
        errors = parser.get_errors()
        messages = [e["message"] for e in errors]
        assert any("Extra coordinate" in m for m in messages)


class TestPOSCARParserNegativeCount:
    """Cover negative atom count detection."""

    def test_negative_atom_count(self):
        content = "Si\n1.0\n5.0 0.0 0.0\n0.0 5.0 0.0\n0.0 0.0 5.0\n" "Si\n-1\nDirect\n"
        parser = POSCARParser(content)
        parser.parse()
        errors = parser.get_errors()
        messages = [e["message"] for e in errors]
        assert any("non-negative" in m.lower() for m in messages)


# ---------------------------------------------------------------------------
# POTCAR parser: edge cases
# ---------------------------------------------------------------------------


class TestPOTCARParser:
    """Cover POTCAR parser edge cases."""

    def test_empty_content(self):
        parser = POTCARParser("")
        result = parser.parse()
        # Empty content has no entries, returns empty POTCARData (not None)
        assert result is None or (result and len(result.entries) == 0)

    def test_non_empty_no_datasets(self):
        parser = POTCARParser("some random text\nthat is not a POTCAR\n")
        result = parser.parse()
        assert result is None
        errors = parser.get_errors()
        assert len(errors) > 0

    def test_valid_potcar(self):
        content = (
            "PAW_PBE Si 05Jan2001\n"
            "   4.00000000000000\n"
            "parameters from PSCTR are:\n"
            "   VRHFIN =Si: s2p2\n"
            "   ENMAX  = 245.345; ENMIN  = 184.009 eV\n"
        )
        parser = POTCARParser(content)
        result = parser.parse()
        assert result is not None
        assert len(result.entries) == 1
        assert result.entries[0].element == "Si"
        assert result.entries[0].enmax == pytest.approx(245.345)
        assert result.entries[0].enmin == pytest.approx(184.009)

    def test_multiple_entries(self):
        content = (
            "PAW_PBE Si 05Jan2001\n"
            "   ENMAX  = 245.345; ENMIN  = 184.009 eV\n"
            "PAW_PBE O 08Apr2002\n"
            "   ENMAX  = 400.000; ENMIN  = 300.000 eV\n"
        )
        parser = POTCARParser(content)
        result = parser.parse()
        assert result is not None
        assert len(result.entries) == 2
        assert result.entries[0].element == "Si"
        assert result.entries[1].element == "O"

    def test_extract_element_edge_cases(self):
        parser = POTCARParser("title")
        # Test with single word - regex matches first letter
        result = parser._extract_element("PAW")
        assert result == "P"
        # Test with underscore - "PAW_PBE_Si_pv" -> token="Si", split on "_"
        result = parser._extract_element("PAW_PBE Si_pv")
        assert result == "Si"
        # Test with empty
        assert parser._extract_element("") == "Unknown"


# ---------------------------------------------------------------------------
# Quickfixes: extreme scale factor fix
# ---------------------------------------------------------------------------


class TestQuickFixesExtremeScale:
    """Cover _create_fix_extreme_scale_action."""

    def test_extreme_scale_quickfix(self):
        provider = QuickFixesProvider()
        content = (
            "test\n0.001\n1.0 0.0 0.0\n0.0 1.0 0.0\n0.0 0.0 1.0\n" "Si\n1\nDirect\n0.0 0.0 0.0\n"
        )
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=1, character=0), end=Position(line=1, character=5)),
                message="Scale factor 0.001 has extreme magnitude",
                severity=DiagnosticSeverity.Warning,
                source="vasp-lsp",
            )
        ]
        actions = provider.get_code_actions(
            content,
            "file:///work/POSCAR",
            diagnostics,
            Range(
                start=Position(line=1, character=0),
                end=Position(line=1, character=5),
            ),
        )
        assert any("scale" in a.title.lower() for a in actions)


class TestQuickFixesDuplicateAtoms:
    """Cover _create_remove_duplicate_atoms_action."""

    def test_duplicate_atoms_quickfix(self):
        provider = QuickFixesProvider()
        content = (
            "test\n1.0\n1.0 0.0 0.0\n0.0 1.0 0.0\n0.0 0.0 1.0\n"
            "Si\n2\nDirect\n0.0 0.0 0.0\n0.0 0.0 0.0\n"
        )
        diagnostics = [
            Diagnostic(
                range=Range(
                    start=Position(line=9, character=0), end=Position(line=9, character=20)
                ),
                message="Atoms 1 and 2 have identical coordinates.",
                severity=DiagnosticSeverity.Warning,
                source="vasp-lsp",
            )
        ]
        actions = provider.get_code_actions(
            content,
            "file:///work/POSCAR",
            diagnostics,
            Range(
                start=Position(line=9, character=0),
                end=Position(line=9, character=20),
            ),
        )
        assert any("duplicate" in a.title.lower() for a in actions)


class TestQuickFixesZeroWeightKpoints:
    """Cover _create_fix_zero_weights_action."""

    def test_zero_weight_quickfix(self):
        provider = QuickFixesProvider()
        content = "Explicit kpoints\n" "2\n" "Reciprocal\n" "0.0 0.0 0.0 0.5\n" "0.5 0.5 0.5 0.0\n"
        diagnostics = [
            Diagnostic(
                range=Range(
                    start=Position(line=4, character=0), end=Position(line=4, character=20)
                ),
                message="K-point 2 has weight 0.0.",
                severity=DiagnosticSeverity.Warning,
                source="vasp-lsp",
            )
        ]
        actions = provider.get_code_actions(
            content,
            "file:///work/KPOINTS",
            diagnostics,
            Range(
                start=Position(line=4, character=0),
                end=Position(line=4, character=20),
            ),
        )
        assert any("zero" in a.title.lower() for a in actions)


class TestQuickFixesDenseGrid:
    """Cover _create_reduce_dense_grid_action."""

    def test_dense_grid_quickfix(self):
        provider = QuickFixesProvider()
        content = "Dense grid\n" "0\n" "Gamma\n" "100 100 100\n" "0 0 0\n"
        diagnostics = [
            Diagnostic(
                range=Range(
                    start=Position(line=3, character=0), end=Position(line=3, character=15)
                ),
                message="K-point grid 100 100 100 is very dense (all values > 50). Computationally expensive.",
                severity=DiagnosticSeverity.Warning,
                source="vasp-lsp",
            )
        ]
        actions = provider.get_code_actions(
            content,
            "file:///work/KPOINTS",
            diagnostics,
            Range(
                start=Position(line=3, character=0),
                end=Position(line=3, character=15),
            ),
        )
        assert any("grid" in a.title.lower() for a in actions)


class TestQuickFixesMAGMOMCount:
    """Cover _create_replace_magmom_count_action."""

    def test_magmom_count_mismatch_quickfix(self):
        provider = QuickFixesProvider()
        content = "ISPIN = 2\nMAGMOM = 1.0 1.0\n"
        diagnostics = [
            Diagnostic(
                range=Range(
                    start=Position(line=1, character=0), end=Position(line=1, character=15)
                ),
                message="MAGMOM has 2 values but POSCAR contains 4 atoms",
                severity=DiagnosticSeverity.Warning,
                source="vasp-lsp",
            )
        ]
        actions = provider.get_code_actions(
            content,
            "file:///work/INCAR",
            diagnostics,
            Range(
                start=Position(line=1, character=0),
                end=Position(line=1, character=15),
            ),
        )
        assert any("MAGMOM" in a.title for a in actions)


class TestQuickFixesENCUTRaise:
    """Cover _create_raise_encut_action."""

    def test_raise_encut_quickfix(self):
        provider = QuickFixesProvider()
        content = "ENCUT = 200\n"
        diagnostics = [
            Diagnostic(
                range=Range(
                    start=Position(line=0, character=0), end=Position(line=0, character=12)
                ),
                message="ENCUT=200 is below max POTCAR ENMAX=520 eV.",
                severity=DiagnosticSeverity.Warning,
                source="vasp-lsp",
            )
        ]
        actions = provider.get_code_actions(
            content,
            "file:///work/INCAR",
            diagnostics,
            Range(
                start=Position(line=0, character=0),
                end=Position(line=0, character=12),
            ),
        )
        assert any("ENCUT" in a.title for a in actions)
