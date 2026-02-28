"""Diagnostics provider for VASP-LSP."""

from typing import List
from lsprotocol.types import Diagnostic, DiagnosticSeverity, Range, Position

from ..parsers.incar_parser import INCARParser
from ..schemas.incar_tags import INCAR_TAGS, get_tag_info


class DiagnosticsProvider:
    """Provides diagnostics (error checking) for VASP files."""
    
    def __init__(self):
        """Initialize diagnostics provider."""
        pass
        
    def get_diagnostics(
        self,
        document_content: str,
        document_uri: str
    ) -> List[Diagnostic]:
        """Get diagnostics for the document.
        
        Args:
            document_content: Full document content.
            document_uri: Document URI to determine file type.
            
        Returns:
            List of diagnostic messages.
        """
        file_type = self._get_file_type(document_uri)
        
        if file_type == 'INCAR':
            return self._get_incar_diagnostics(document_content)
        elif file_type == 'POSCAR':
            return self._get_poscar_diagnostics(document_content)
        elif file_type == 'KPOINTS':
            return self._get_kpoints_diagnostics(document_content)
            
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
        
    def _get_incar_diagnostics(self, content: str) -> List[Diagnostic]:
        """Get diagnostics for INCAR files."""
        diagnostics = []
        
        # Parse the INCAR file
        parser = INCARParser(content)
        parser.parse()
        
        # Report parse errors
        for error in parser.get_errors():
            severity = (
                DiagnosticSeverity.Error if error['severity'] == 'error'
                else DiagnosticSeverity.Warning
            )
            
            diagnostic = Diagnostic(
                range=Range(
                    start=Position(line=error['line'] - 1, character=error.get('column', 0)),
                    end=Position(line=error['line'] - 1, character=error.get('column', 0) + 1)
                ),
                message=error['message'],
                severity=severity,
                source='vasp-lsp'
            )
            diagnostics.append(diagnostic)
            
        # Check for unknown tags
        for param_name, param in parser.get_all_parameters().items():
            if get_tag_info(param_name) is None:
                diagnostic = Diagnostic(
                    range=Range(
                        start=Position(line=param.line_number - 1, character=param.column_start),
                        end=Position(line=param.line_number - 1, character=param.column_end)
                    ),
                    message=f"Unknown INCAR tag: {param_name}",
                    severity=DiagnosticSeverity.Warning,
                    source='vasp-lsp'
                )
                diagnostics.append(diagnostic)
            else:
                # Validate parameter value
                tag = get_tag_info(param_name)
                value_diagnostics = self._validate_incar_value(tag, param, content)
                diagnostics.extend(value_diagnostics)
                
        # Check for parameter dependencies and conflicts
        diagnostics.extend(self._check_incar_dependencies(parser))
        
        return diagnostics
        
    def _validate_incar_value(self, tag, param, content) -> List[Diagnostic]:
        """Validate a single INCAR parameter value."""
        diagnostics = []
        value = param.value
        
        # Check enum values
        if tag.enum_values and value is not None:
            str_value = str(value).upper()
            valid_values = [v.upper() for v in tag.enum_values]
            if str_value not in valid_values:
                # Try to match as number
                try:
                    num_value = float(value)
                    if str(int(num_value)) not in valid_values:
                        diagnostics.append(Diagnostic(
                            range=Range(
                                start=Position(line=param.line_number - 1, character=param.column_start),
                                end=Position(line=param.line_number - 1, character=param.column_end)
                            ),
                            message=f"Invalid value for {tag.name}. Allowed: {', '.join(tag.enum_values)}",
                            severity=DiagnosticSeverity.Warning,
                            source='vasp-lsp'
                        ))
                except (ValueError, TypeError):
                    diagnostics.append(Diagnostic(
                        range=Range(
                            start=Position(line=param.line_number - 1, character=param.column_start),
                            end=Position(line=param.line_number - 1, character=param.column_end)
                        ),
                        message=f"Invalid value for {tag.name}. Allowed: {', '.join(tag.enum_values)}",
                        severity=DiagnosticSeverity.Warning,
                        source='vasp-lsp'
                    ))
                    
        # Check range for numeric values
        if tag.valid_range and isinstance(value, (int, float)):
            min_val, max_val = tag.valid_range
            if min_val is not None and value < min_val:
                diagnostics.append(Diagnostic(
                    range=Range(
                        start=Position(line=param.line_number - 1, character=param.column_start),
                        end=Position(line=param.line_number - 1, character=param.column_end)
                    ),
                    message=f"Value {value} is below minimum {min_val} for {tag.name}",
                    severity=DiagnosticSeverity.Warning,
                    source='vasp-lsp'
                ))
            if max_val is not None and value > max_val:
                diagnostics.append(Diagnostic(
                    range=Range(
                        start=Position(line=param.line_number - 1, character=param.column_start),
                        end=Position(line=param.line_number - 1, character=param.column_end)
                    ),
                    message=f"Value {value} is above maximum {max_val} for {tag.name}",
                    severity=DiagnosticSeverity.Warning,
                    source='vasp-lsp'
                ))
                
        return diagnostics
        
    def _check_incar_dependencies(self, parser: INCARParser) -> List[Diagnostic]:
        """Check for parameter dependencies and conflicts."""
        diagnostics = []
        
        # Check ISMEAR and SIGMA relationship
        ismear = parser.get_parameter('ISMEAR')
        sigma = parser.get_parameter('SIGMA')
        
        if ismear and not sigma:
            if isinstance(ismear.value, int) and ismear.value >= 0:
                diagnostics.append(Diagnostic(
                    range=Range(
                        start=Position(line=ismear.line_number - 1, character=0),
                        end=Position(line=ismear.line_number - 1, character=len(ismear.raw_line))
                    ),
                    message="ISMEAR >= 0 should have SIGMA set. Default SIGMA=0.2 may not be appropriate.",
                    severity=DiagnosticSeverity.Information,
                    source='vasp-lsp'
                ))
                
        # Check NCORE and NPAR conflict
        ncore = parser.get_parameter('NCORE')
        npar = parser.get_parameter('NPAR')
        
        if ncore and npar:
            diagnostics.append(Diagnostic(
                range=Range(
                    start=Position(line=npar.line_number - 1, character=0),
                    end=Position(line=npar.line_number - 1, character=len(npar.raw_line))
                ),
                message="NPAR and NCORE should not be set together. Prefer NCORE.",
                severity=DiagnosticSeverity.Warning,
                source='vasp-lsp'
            ))
            
        # Check LDAU requirements
        ldau = parser.get_parameter('LDAU')
        if ldau and ldau.value:
            if not parser.get_parameter('LDAUTYPE'):
                diagnostics.append(Diagnostic(
                    range=Range(
                        start=Position(line=ldau.line_number - 1, character=0),
                        end=Position(line=ldau.line_number - 1, character=len(ldau.raw_line))
                    ),
                    message="LDAU=.TRUE. requires LDAUTYPE to be set.",
                    severity=DiagnosticSeverity.Warning,
                    source='vasp-lsp'
                ))
            if not parser.get_parameter('LDAUL'):
                diagnostics.append(Diagnostic(
                    range=Range(
                        start=Position(line=ldau.line_number - 1, character=0),
                        end=Position(line=ldau.line_number - 1, character=len(ldau.raw_line))
                    ),
                    message="LDAU=.TRUE. requires LDAUL to be set.",
                    severity=DiagnosticSeverity.Warning,
                    source='vasp-lsp'
                ))
            if not parser.get_parameter('LDAUU'):
                diagnostics.append(Diagnostic(
                    range=Range(
                        start=Position(line=ldau.line_number - 1, character=0),
                        end=Position(line=ldau.line_number - 1, character=len(ldau.raw_line))
                    ),
                    message="LDAU=.TRUE. requires LDAUU to be set.",
                    severity=DiagnosticSeverity.Warning,
                    source='vasp-lsp'
                ))
                
        # Check hybrid functional requirements
        lhf = parser.get_parameter('LHFCALC')
        if lhf and lhf.value:
            if not parser.get_parameter('HFSCREEN'):
                pass  # Has default, so optional
            if not parser.get_parameter('PRECFOCK'):
                pass  # Has default
                
        # Check spin-polarization and MAGMOM
        ispin = parser.get_parameter('ISPIN')
        magmom = parser.get_parameter('MAGMOM')
        
        if ispin and isinstance(ispin.value, int) and ispin.value == 2 and not magmom:
            diagnostics.append(Diagnostic(
                range=Range(
                    start=Position(line=ispin.line_number - 1, character=0),
                    end=Position(line=ispin.line_number - 1, character=len(ispin.raw_line))
                ),
                message="ISPIN=2 (spin-polarized) should have MAGMOM set for initial magnetic moments.",
                severity=DiagnosticSeverity.Information,
                source='vasp-lsp'
            ))
            
        return diagnostics
        
    def _get_poscar_diagnostics(self, content: str) -> List[Diagnostic]:
        """Get diagnostics for POSCAR files."""
        # TODO: Implement POSCAR validation
        return []
        
    def _get_kpoints_diagnostics(self, content: str) -> List[Diagnostic]:
        """Get diagnostics for KPOINTS files."""
        # TODO: Implement KPOINTS validation
        return []
