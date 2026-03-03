"""Tests for the quick fixes provider."""

import pytest
from lsprotocol.types import Diagnostic, Range, Position

from vasp_lsp.features.quickfixes import QuickFixesProvider


@pytest.fixture
def quickfixes():
    """Create quick fixes provider fixture."""
    return QuickFixesProvider()


class TestQuickFixesProvider:
    """Test the QuickFixesProvider class."""
    
    def test_get_file_type_incar(self, quickfixes):
        """Test file type detection for INCAR."""
        assert quickfixes._get_file_type("file:///path/INCAR") == "INCAR"
        assert quickfixes._get_file_type("file:///path/INCAR_test") == "INCAR"
    
    def test_get_file_type_poscar(self, quickfixes):
        """Test file type detection for POSCAR."""
        assert quickfixes._get_file_type("file:///path/POSCAR") == "POSCAR"
        assert quickfixes._get_file_type("file:///path/CONTCAR") == "POSCAR"
    
    def test_get_file_type_kpoints(self, quickfixes):
        """Test file type detection for KPOINTS."""
        assert quickfixes._get_file_type("file:///path/KPOINTS") == "KPOINTS"
    
    def test_get_file_type_unknown(self, quickfixes):
        """Test file type detection for unknown files."""
        assert quickfixes._get_file_type("file:///path/unknown.txt") == "UNKNOWN"
    
    def test_code_actions_unknown_file(self, quickfixes):
        """Test that unknown file types return empty actions."""
        result = quickfixes.get_code_actions(
            "content",
            "file:///unknown.txt",
            [],
            Range(start=Position(line=0, character=0), end=Position(line=0, character=1))
        )
        assert result == []


class TestINCARQuickFixes:
    """Test INCAR-specific quick fixes."""
    
    def test_add_sigma_action(self, quickfixes):
        """Test adding SIGMA when ISMEAR is set."""
        content = "ISMEAR = 0"
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=10)),
            message="ISMEAR >= 0 should have SIGMA set",
            source="vasp-lsp"
        )
        
        actions = quickfixes.get_code_actions(
            content, "file:///INCAR", [diagnostic],
            Range(start=Position(line=0, character=0), end=Position(line=0, character=10))
        )
        
        assert len(actions) > 0
        assert any("SIGMA" in action.title for action in actions)
    
    def test_add_magmom_action(self, quickfixes):
        """Test adding MAGMOM when ISPIN=2."""
        content = "ISPIN = 2"
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=9)),
            message="ISPIN=2 should have MAGMOM set",
            source="vasp-lsp"
        )
        
        actions = quickfixes.get_code_actions(
            content, "file:///INCAR", [diagnostic],
            Range(start=Position(line=0, character=0), end=Position(line=0, character=9))
        )
        
        assert len(actions) > 0
        assert any("MAGMOM" in action.title for action in actions)
    
    def test_add_ldau_params(self, quickfixes):
        """Test adding LDAU parameters."""
        content = "LDAU = .TRUE."
        
        diagnostics = [
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=13)),
                message="LDAU=.TRUE. requires LDAUTYPE to be set",
                source="vasp-lsp"
            ),
            Diagnostic(
                range=Range(start=Position(line=0, character=0), end=Position(line=0, character=13)),
                message="LDAU=.TRUE. requires LDAUL to be set",
                source="vasp-lsp"
            ),
        ]
        
        actions = quickfixes.get_code_actions(
            content, "file:///INCAR", diagnostics,
            Range(start=Position(line=0, character=0), end=Position(line=0, character=13))
        )
        
        assert len(actions) >= 2
        assert any("LDAUTYPE" in action.title for action in actions)
        assert any("LDAUL" in action.title for action in actions)
    
    def test_remove_npar_action(self, quickfixes):
        """Test removing NPAR when NCORE is set."""
        content = """NCORE = 4
NPAR = 2"""
        diagnostic = Diagnostic(
            range=Range(start=Position(line=1, character=0), end=Position(line=1, character=8)),
            message="NPAR and NCORE should not be set together",
            source="vasp-lsp"
        )
        
        actions = quickfixes.get_code_actions(
            content, "file:///INCAR", [diagnostic],
            Range(start=Position(line=1, character=0), end=Position(line=1, character=8))
        )
        
        assert len(actions) > 0
        assert any("Remove NPAR" in action.title for action in actions)
    
    def test_fix_typo_action(self, quickfixes):
        """Test fixing common typos."""
        content = "ENCUTT = 500"
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=6)),
            message="Unknown INCAR tag: ENCUTT",
            source="vasp-lsp"
        )
        
        actions = quickfixes.get_code_actions(
            content, "file:///INCAR", [diagnostic],
            Range(start=Position(line=0, character=0), end=Position(line=0, character=6))
        )
        
        assert len(actions) > 0
        assert any("ENCUT" in action.title for action in actions)
    
    def test_similarity_score(self, quickfixes):
        """Test string similarity calculation."""
        # Exact match
        assert quickfixes._similarity_score("ENCUT", "ENCUT") == 1.0
        
        # No match
        assert quickfixes._similarity_score("ABC", "XYZ") == 0.0
        
        # Partial match
        score = quickfixes._similarity_score("ENCUTT", "ENCUT")
        assert 0.7 < score < 1.0
    
    def test_find_similar_tag(self, quickfixes):
        """Test finding similar tags."""
        # Should find ENCUT for ENCUTT (common typo - 5/6 chars match = 83%)
        result = quickfixes._find_similar_tag("ENCUTT")
        assert result is not None
        assert result.upper() == "ENCUT"
        
        # Should return None for very different strings
        result = quickfixes._find_similar_tag("XYZ123")
        assert result is None
        
        # Test with exact match
        result = quickfixes._find_similar_tag("ENCUT")
        assert result == "ENCUT"


class TestPOSCARQuickFixes:
    """Test POSCAR quick fixes."""
    
    def test_poscar_returns_empty(self, quickfixes):
        """Test that POSCAR currently returns empty actions."""
        actions = quickfixes.get_code_actions(
            "POSCAR content",
            "file:///POSCAR",
            [],
            Range(start=Position(line=0, character=0), end=Position(line=0, character=1))
        )
        assert actions == []


class TestKPOINTSQuickFixes:
    """Test KPOINTS quick fixes."""
    
    def test_kpoints_returns_empty(self, quickfixes):
        """Test that KPOINTS currently returns empty actions."""
        actions = quickfixes.get_code_actions(
            "KPOINTS content",
            "file:///KPOINTS",
            [],
            Range(start=Position(line=0, character=0), end=Position(line=0, character=1))
        )
        assert actions == []
