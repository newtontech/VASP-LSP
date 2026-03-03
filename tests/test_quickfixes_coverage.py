"""Additional tests for 100% coverage of quickfixes.py."""

import pytest
from lsprotocol.types import Diagnostic, Range, Position
from vasp_lsp.features.quickfixes import QuickFixesProvider


class TestQuickFixesCoverage:
    """Tests for missing coverage in quickfixes.py."""
    
    def test_get_poscar_code_actions_empty(self):
        """Test POSCAR code actions returns empty list (lines 135-142)."""
        provider = QuickFixesProvider()
        diagnostics = []
        range_obj = Range(start=Position(line=0, character=0), end=Position(line=1, character=0))
        
        result = provider._get_poscar_code_actions("content", diagnostics, range_obj)
        assert result == []
    
    def test_get_kpoints_code_actions_empty(self):
        """Test KPOINTS code actions returns empty list (line 142)."""
        provider = QuickFixesProvider()
        diagnostics = []
        range_obj = Range(start=Position(line=0, character=0), end=Position(line=1, character=0))
        
        result = provider._get_kpoints_code_actions("content", diagnostics, range_obj)
        assert result == []
    
    def test_create_fix_typo_no_tag_in_line(self):
        """Test _create_fix_typo_action when tag not found in line (line 271)."""
        provider = QuickFixesProvider()
        lines = ["ENCUT = 500  # This line doesn't contain the unknown tag"]
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=10)),
            message="Unknown INCAR tag: UNKNOWNTAG"
        )
        
        result = provider._create_fix_typo_action(lines, diagnostic, "content")
        assert result is None
    
    def test_create_fix_typo_no_unknown_tag_in_message(self):
        """Test _create_fix_typo_action when message doesn't contain 'Unknown INCAR tag' (line 302)."""
        provider = QuickFixesProvider()
        lines = ["SOMESETTING = value"]
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=10)),
            message="Some other error message"
        )
        
        result = provider._create_fix_typo_action(lines, diagnostic, "content")
        assert result is None
    
    def test_find_similar_tag_no_match(self):
        """Test _find_similar_tag when no similar tag found (line 305)."""
        provider = QuickFixesProvider()
        # Use a very different string that won't match any valid tag
        result = provider._find_similar_tag("XYZ123ABC789")
        assert result is None
    
    def test_similarity_score_empty_strings(self):
        """Test _similarity_score with empty strings (line 314)."""
        provider = QuickFixesProvider()
        result = provider._similarity_score("", "test")
        assert result == 0.0
        
        result = provider._similarity_score("test", "")
        assert result == 0.0
        
        result = provider._similarity_score("", "")
        assert result == 0.0
    
    def test_create_fix_typo_no_valid_fix(self):
        """Test _create_fix_typo_action when no valid fix found (line 357)."""
        provider = QuickFixesProvider()
        # Create a line that matches the typo pattern but won't have a fix
        lines = ["VERYOBSCURETAG = value"]
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=14)),
            message="Unknown INCAR tag: VERYOBSCURETAG"
        )
        
        result = provider._create_fix_typo_action(lines, diagnostic, "content")
        # Should return None since no similar tag found
        assert result is None
    
    def test_create_fix_typo_same_as_original(self):
        """Test _create_fix_typo_action when fix is same as original (line 364)."""
        provider = QuickFixesProvider()
        # Use a valid tag but in different case - should find itself as match
        lines = ["encut = 500"]
        diagnostic = Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=5)),
            message="Unknown INCAR tag: ENCUT"
        )
        
        result = provider._create_fix_typo_action(lines, diagnostic, "content")
        # ENCUT is already valid, so fix would be same as original
        assert result is None
