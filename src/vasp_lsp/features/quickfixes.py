"""Quick fixes provider for VASP-LSP.

Provides code actions to automatically fix common VASP input file issues.
"""

from typing import List, Optional, Dict, Any
from lsprotocol.types import (
    CodeAction, CodeActionKind, Diagnostic, Range, Position, TextEdit, WorkspaceEdit
)

from ..parsers.incar_parser import INCARParser
from ..schemas.incar_tags import INCAR_TAGS, get_tag_info


class QuickFixesProvider:
    """Provides quick fixes (code actions) for VASP files."""
    
    def __init__(self):
        """Initialize quick fixes provider."""
        pass
    
    def get_code_actions(
        self,
        document_content: str,
        document_uri: str,
        diagnostics: List[Diagnostic],
        range: Range
    ) -> List[CodeAction]:
        """Get available code actions for the given diagnostics.
        
        Args:
            document_content: Full document content.
            document_uri: Document URI to determine file type.
            diagnostics: List of diagnostics in the document.
            range: The range for which code actions are requested.
            
        Returns:
            List of available code actions.
        """
        file_type = self._get_file_type(document_uri)
        
        if file_type == 'INCAR':
            return self._get_incar_code_actions(document_content, diagnostics, range)
        elif file_type == 'POSCAR':
            return self._get_poscar_code_actions(document_content, diagnostics, range)
        elif file_type == 'KPOINTS':
            return self._get_kpoints_code_actions(document_content, diagnostics, range)
            
        return []
    
    def _get_file_type(self, uri: str) -> str:
        """Determine file type from URI."""
        filename = uri.split('/')[-1].upper()
        
        if 'INCAR' in filename:
            return 'INCAR'
        if 'POSCAR' in filename or 'CONTCAR' in filename:
            return 'POSCAR'
        if 'KPOINTS' in filename:
            return 'KPOINTS'
            
        return 'UNKNOWN'
    
    def _get_incar_code_actions(
        self,
        content: str,
        diagnostics: List[Diagnostic],
        range: Range
    ) -> List[CodeAction]:
        """Get code actions for INCAR files."""
        actions = []
        parser = INCARParser(content)
        parser.parse()
        lines = content.split('\n')
        
        # Track which fixes we've added to avoid duplicates
        added_fixes = set()
        
        for diagnostic in diagnostics:
            message = diagnostic.message.lower()
            
            # Fix 1: Add missing SIGMA when ISMEAR >= 0
            if 'sigma' in message and 'ismear' in message:
                fix_key = 'add_sigma'
                if fix_key not in added_fixes:
                    action = self._create_add_sigma_action(lines, diagnostic)
                    if action:
                        actions.append(action)
                        added_fixes.add(fix_key)
            
            # Fix 2: Remove NPAR when NCORE is set
            if 'npar' in message and 'ncore' in message:
                fix_key = 'remove_npar'
                if fix_key not in added_fixes:
                    action = self._create_remove_line_action(
                        lines, diagnostic, 
                        "Remove NPAR (use NCORE instead)",
                        "Use NCORE instead of NPAR for parallelization control"
                    )
                    if action:
                        actions.append(action)
                        added_fixes.add(fix_key)
            
            # Fix 3: Add MAGMOM when ISPIN=2
            if 'magmom' in message and 'ispin' in message:
                fix_key = 'add_magmom'
                if fix_key not in added_fixes:
                    action = self._create_add_magmom_action(lines, diagnostic)
                    if action:
                        actions.append(action)
                        added_fixes.add(fix_key)
            
            # Fix 4: Add missing LDAU parameters
            if 'ldau' in message and 'ldautype' in message:
                fix_key = 'add_ldautype'
                if fix_key not in added_fixes:
                    action = self._create_add_ldau_param_action(
                        lines, diagnostic, 'LDAUTYPE', '2'
                    )
                    if action:
                        actions.append(action)
                        added_fixes.add(fix_key)
            
            if 'ldau' in message and 'ldaul' in message:
                fix_key = 'add_ldaul'
                if fix_key not in added_fixes:
                    action = self._create_add_ldau_param_action(
                        lines, diagnostic, 'LDAUL', '-1 -1 -1'
                    )
                    if action:
                        actions.append(action)
                        added_fixes.add(fix_key)
            
            if 'ldau' in message and 'ldauu' in message:
                fix_key = 'add_ldauu'
                if fix_key not in added_fixes:
                    action = self._create_add_ldau_param_action(
                        lines, diagnostic, 'LDAUU', '0 0 0'
                    )
                    if action:
                        actions.append(action)
                        added_fixes.add(fix_key)
            
            # Fix 5: Fix common typos in INCAR tags
            if 'unknown incar tag' in message:
                action = self._create_fix_typo_action(lines, diagnostic, content)
                if action:
                    actions.append(action)
        
        return actions
    
    def _create_add_sigma_action(self, lines: List[str], diagnostic: Diagnostic) -> Optional[CodeAction]:
        """Create action to add SIGMA parameter."""
        line_num = diagnostic.range.start.line
        
        # Find the end of ISMEAR line
        insert_line = line_num + 1
        
        return CodeAction(
            title="Add SIGMA=0.2",
            kind=CodeActionKind.QuickFix,
            diagnostics=[diagnostic],
            edit=WorkspaceEdit(
                changes={
                    "document": [
                        TextEdit(
                            range=Range(
                                start=Position(line=insert_line, character=0),
                                end=Position(line=insert_line, character=0)
                            ),
                            new_text="SIGMA = 0.2  # Default for ISMEAR >= 0\\n"
                        )
                    ]
                }
            )
        )
    
    def _create_remove_line_action(
        self, 
        lines: List[str], 
        diagnostic: Diagnostic,
        title: str,
        description: str
    ) -> Optional[CodeAction]:
        """Create action to remove a line."""
        line_num = diagnostic.range.start.line
        
        return CodeAction(
            title=title,
            kind=CodeActionKind.QuickFix,
            diagnostics=[diagnostic],
            edit=WorkspaceEdit(
                changes={
                    "document": [
                        TextEdit(
                            range=Range(
                                start=Position(line=line_num, character=0),
                                end=Position(line=line_num + 1, character=0)
                            ),
                            new_text=""
                        )
                    ]
                }
            )
        )
    
    def _create_add_magmom_action(self, lines: List[str], diagnostic: Diagnostic) -> Optional[CodeAction]:
        """Create action to add MAGMOM parameter."""
        line_num = diagnostic.range.start.line
        insert_line = line_num + 1
        
        return CodeAction(
            title="Add MAGMOM (default for all atoms)",
            kind=CodeActionKind.QuickFix,
            diagnostics=[diagnostic],
            edit=WorkspaceEdit(
                changes={
                    "document": [
                        TextEdit(
                            range=Range(
                                start=Position(line=insert_line, character=0),
                                end=Position(line=insert_line, character=0)
                            ),
                            new_text="MAGMOM = 1.0  # Default initial magnetic moment\\n"
                        )
                    ]
                }
            )
        )
    
    def _create_add_ldau_param_action(
        self, 
        lines: List[str], 
        diagnostic: Diagnostic,
        param_name: str,
        default_value: str
    ) -> Optional[CodeAction]:
        """Create action to add LDAU parameter."""
        line_num = diagnostic.range.start.line
        insert_line = line_num + 1
        
        return CodeAction(
            title=f"Add {param_name}={default_value}",
            kind=CodeActionKind.QuickFix,
            diagnostics=[diagnostic],
            edit=WorkspaceEdit(
                changes={
                    "document": [
                        TextEdit(
                            range=Range(
                                start=Position(line=insert_line, character=0),
                                end=Position(line=insert_line, character=0)
                            ),
                            new_text=f"{param_name} = {default_value}\\n"
                        )
                    ]
                }
            )
        )
    
    def _create_fix_typo_action(
        self, 
        lines: List[str], 
        diagnostic: Diagnostic,
        content: str
    ) -> Optional[CodeAction]:
        """Create action to fix common typos in INCAR tags."""
        # Extract the unknown tag name
        message = diagnostic.message
        if 'Unknown INCAR tag:' not in message:
            return None
        
        tag_name = message.split('Unknown INCAR tag:')[-1].strip()
        
        # Common typo mappings
        typo_fixes = {
            'ENCUTT': 'ENCUT',
            'ENCO': 'ENCUT',
            'ENCUTT': 'ENCUT',
            'ISMER': 'ISMEAR',
            'ISMAER': 'ISMEAR',
            'SIGM': 'SIGMA',
            'SIG': 'SIGMA',
            'PRECission': 'PREC',
            'ALGOO': 'ALGO',
            'IBRIO': 'IBRION',
            'IBRIONN': 'IBRION',
            'NSWW': 'NSW',
            'EDIF': 'EDIFF',
            'EDIFFG': 'EDIFFG',  # Valid but might be typo
            'LREAL': 'LREAL',
            'LWAV': 'LWAVE',
            'LCHARGG': 'LCHARG',
        }
        
        # Find closest match
        upper_tag = tag_name.upper()
        if upper_tag in typo_fixes:
            fixed_tag = typo_fixes[upper_tag]
        else:
            # Try to find similar tags using simple string similarity
            fixed_tag = self._find_similar_tag(tag_name)
        
        if not fixed_tag or fixed_tag.upper() == tag_name.upper():
            return None
        
        # Create the fix
        line_num = diagnostic.range.start.line
        line = lines[line_num]
        
        # Find the tag in the line
        start_col = line.upper().find(tag_name.upper())
        if start_col == -1:
            return None
        
        end_col = start_col + len(tag_name)
        
        return CodeAction(
            title=f"Change '{tag_name}' to '{fixed_tag}'",
            kind=CodeActionKind.QuickFix,
            diagnostics=[diagnostic],
            edit=WorkspaceEdit(
                changes={
                    "document": [
                        TextEdit(
                            range=Range(
                                start=Position(line=line_num, character=start_col),
                                end=Position(line=line_num, character=end_col)
                            ),
                            new_text=fixed_tag
                        )
                    ]
                }
            )
        )
    
    def _find_similar_tag(self, tag_name: str) -> Optional[str]:
        """Find a similar valid tag for a potentially misspelled one."""
        tag_name_upper = tag_name.upper()
        
        # Simple similarity check: count matching characters
        best_match = None
        best_score = 0
        
        for valid_tag in INCAR_TAGS.keys():
            # Calculate simple similarity
            score = self._similarity_score(tag_name_upper, valid_tag.upper())
            if score > best_score and score >= 0.7:  # 70% similarity threshold
                best_score = score
                best_match = valid_tag
        
        return best_match
    
    def _similarity_score(self, s1: str, s2: str) -> float:
        """Calculate similarity score between two strings."""
        if not s1 or not s2:
            return 0.0
        
        # Simple character-based similarity
        len1, len2 = len(s1), len(s2)
        max_len = max(len1, len2)
        matches = sum(c1 == c2 for c1, c2 in zip(s1, s2))
        
        return matches / max_len
    
    def _get_poscar_code_actions(
        self,
        content: str,
        diagnostics: List[Diagnostic],
        range: Range
    ) -> List[CodeAction]:
        """Get code actions for POSCAR files."""
        # TODO: Implement POSCAR quick fixes
        return []
    
    def _get_kpoints_code_actions(
        self,
        content: str,
        diagnostics: List[Diagnostic],
        range: Range
    ) -> List[CodeAction]:
        """Get code actions for KPOINTS files."""
        # TODO: Implement KPOINTS quick fixes
        return []
