"""Diagnostics provider for VASP-LSP."""

import os
import re
from typing import Dict, List, Optional
from urllib.parse import unquote, urlparse

from lsprotocol.types import Diagnostic, DiagnosticSeverity, Position, Range

from ..parsers.incar_parser import INCARParser
from ..parsers.kpoints_parser import KPOINTSMode, KPOINTSParser
from ..parsers.poscar_parser import POSCARParser
from ..parsers.potcar_parser import POTCARParser
from ..schemas.incar_tags import get_tag_info


class DiagnosticsProvider:
    """Provides diagnostics (error checking) for VASP files."""

    def __init__(self):
        """Initialize diagnostics provider."""
        pass

    def get_diagnostics(
        self,
        document_content: str,
        document_uri: str,
        workspace_documents: Optional[Dict[str, str]] = None,
    ) -> List[Diagnostic]:
        """Get diagnostics for the document.

        Args:
            document_content: Full document content.
            document_uri: Document URI to determine file type.

        Returns:
            List of diagnostic messages.
        """
        file_type = self._get_file_type(document_uri)

        if file_type == "INCAR":
            return self._get_incar_diagnostics(document_content, document_uri, workspace_documents)
        elif file_type == "POSCAR":
            return self._get_poscar_diagnostics(document_content)
        elif file_type == "KPOINTS":
            return self._get_kpoints_diagnostics(document_content)

        return []

    def _get_file_type(self, uri: str) -> str:
        """Determine file type from URI."""
        filename = uri.split("/")[-1].upper()

        if "INCAR" in filename:
            return "INCAR"
        if "POSCAR" in filename or "CONTCAR" in filename:
            return "POSCAR"
        if "KPOINTS" in filename:
            return "KPOINTS"

        return "UNKNOWN"

    def _get_incar_diagnostics(
        self,
        content: str,
        document_uri: str = "",
        workspace_documents: Optional[Dict[str, str]] = None,
    ) -> List[Diagnostic]:
        """Get diagnostics for INCAR files."""
        diagnostics = []

        # Parse the INCAR file
        parser = INCARParser(content)
        parser.parse()

        # Report parse errors
        for error in parser.get_errors():
            severity = (
                DiagnosticSeverity.Error
                if error["severity"] == "error"
                else DiagnosticSeverity.Warning
            )

            diagnostic = Diagnostic(
                range=Range(
                    start=Position(line=error["line"] - 1, character=error.get("column", 0)),
                    end=Position(line=error["line"] - 1, character=error.get("column", 0) + 1),
                ),
                message=error["message"],
                severity=severity,
                source="vasp-lsp",
            )
            diagnostics.append(diagnostic)

        # Check for unknown tags
        for param_name, param in parser.get_all_parameters().items():
            if get_tag_info(param_name) is None:
                diagnostic = Diagnostic(
                    range=Range(
                        start=Position(line=param.line_number - 1, character=param.column_start),
                        end=Position(line=param.line_number - 1, character=param.column_end),
                    ),
                    message=f"Unknown INCAR tag: {param_name}",
                    severity=DiagnosticSeverity.Warning,
                    source="vasp-lsp",
                )
                diagnostics.append(diagnostic)
            else:
                tag = get_tag_info(param_name)
                value_diagnostics = self._validate_incar_value(tag, param, content)
                diagnostics.extend(value_diagnostics)

        # Check for parameter dependencies and conflicts
        diagnostics.extend(self._check_incar_dependencies(parser))
        diagnostics.extend(
            self._check_workspace_consistency(parser, document_uri, workspace_documents)
        )

        return diagnostics

    def _validate_incar_value(self, tag, param, content) -> List[Diagnostic]:
        """Validate a single INCAR parameter value."""
        diagnostics = []
        value = param.value
        expected_type = tag.type

        if expected_type == "integer" and (not isinstance(value, int) or isinstance(value, bool)):
            diagnostics.append(
                self._value_type_diagnostic(param, f"{tag.name} expects an integer value.")
            )
        elif expected_type == "float" and (
            not isinstance(value, (int, float)) or isinstance(value, bool)
        ):
            diagnostics.append(
                self._value_type_diagnostic(param, f"{tag.name} expects a float value.")
            )
        elif expected_type == "boolean" and not isinstance(value, bool):
            diagnostics.append(
                self._value_type_diagnostic(param, f"{tag.name} expects a boolean value.")
            )
        # Array-valued INCAR tags may legitimately contain one scalar value when
        # the structure has one species or one atom, so their length is checked
        # with workspace context instead of treated as a scalar type error here.

        # Check enum values
        if tag.enum_values and value is not None:
            str_value = str(value).upper()
            valid_values = [v.upper() for v in tag.enum_values]
            if str_value not in valid_values:
                # Try to match as number
                try:
                    num_value = float(value)
                    if str(int(num_value)) not in valid_values:
                        diagnostics.append(
                            Diagnostic(
                                range=Range(
                                    start=Position(
                                        line=param.line_number - 1,
                                        character=param.column_start,
                                    ),
                                    end=Position(
                                        line=param.line_number - 1,
                                        character=param.column_end,
                                    ),
                                ),
                                message=f"Invalid value for {tag.name}. Allowed: {', '.join(tag.enum_values)}",
                                severity=DiagnosticSeverity.Warning,
                                source="vasp-lsp",
                            )
                        )
                except (ValueError, TypeError):
                    diagnostics.append(
                        Diagnostic(
                            range=Range(
                                start=Position(
                                    line=param.line_number - 1,
                                    character=param.column_start,
                                ),
                                end=Position(
                                    line=param.line_number - 1,
                                    character=param.column_end,
                                ),
                            ),
                            message=f"Invalid value for {tag.name}. Allowed: {', '.join(tag.enum_values)}",
                            severity=DiagnosticSeverity.Warning,
                            source="vasp-lsp",
                        )
                    )

        # Check range for numeric values
        if tag.valid_range and isinstance(value, (int, float)):
            min_val, max_val = tag.valid_range
            if min_val is not None and value < min_val:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(
                                line=param.line_number - 1, character=param.column_start
                            ),
                            end=Position(line=param.line_number - 1, character=param.column_end),
                        ),
                        message=f"Value {value} is below minimum {min_val} for {tag.name}",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )
            if max_val is not None and value > max_val:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(
                                line=param.line_number - 1, character=param.column_start
                            ),
                            end=Position(line=param.line_number - 1, character=param.column_end),
                        ),
                        message=f"Value {value} is above maximum {max_val} for {tag.name}",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )

        return diagnostics

    def _value_type_diagnostic(self, param, message: str) -> Diagnostic:
        return Diagnostic(
            range=Range(
                start=Position(line=param.line_number - 1, character=param.column_start),
                end=Position(line=param.line_number - 1, character=param.column_end),
            ),
            message=message,
            severity=DiagnosticSeverity.Error,
            source="vasp-lsp",
        )

    def _check_incar_dependencies(self, parser: INCARParser) -> List[Diagnostic]:
        """Check for parameter dependencies and conflicts."""
        diagnostics = []

        # Check ISMEAR and SIGMA relationship
        ismear = parser.get_parameter("ISMEAR")
        sigma = parser.get_parameter("SIGMA")

        if ismear and not sigma:
            if isinstance(ismear.value, int) and ismear.value >= 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=ismear.line_number - 1, character=0),
                            end=Position(
                                line=ismear.line_number - 1,
                                character=len(ismear.raw_line),
                            ),
                        ),
                        message="ISMEAR >= 0 should have SIGMA set. Default SIGMA=0.2 may not be appropriate.",
                        severity=DiagnosticSeverity.Information,
                        source="vasp-lsp",
                    )
                )

        # Check NCORE and NPAR conflict
        ncore = parser.get_parameter("NCORE")
        npar = parser.get_parameter("NPAR")

        if ncore and npar:
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=npar.line_number - 1, character=0),
                        end=Position(line=npar.line_number - 1, character=len(npar.raw_line)),
                    ),
                    message="NPAR and NCORE should not be set together. Prefer NCORE.",
                    severity=DiagnosticSeverity.Warning,
                    source="vasp-lsp",
                )
            )

        # Check LDAU requirements
        ldau = parser.get_parameter("LDAU")
        if ldau and ldau.value:
            if not parser.get_parameter("LDAUTYPE"):
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=ldau.line_number - 1, character=0),
                            end=Position(line=ldau.line_number - 1, character=len(ldau.raw_line)),
                        ),
                        message="LDAU=.TRUE. requires LDAUTYPE to be set.",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )
            if not parser.get_parameter("LDAUL"):
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=ldau.line_number - 1, character=0),
                            end=Position(line=ldau.line_number - 1, character=len(ldau.raw_line)),
                        ),
                        message="LDAU=.TRUE. requires LDAUL to be set.",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )
            if not parser.get_parameter("LDAUU"):
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=ldau.line_number - 1, character=0),
                            end=Position(line=ldau.line_number - 1, character=len(ldau.raw_line)),
                        ),
                        message="LDAU=.TRUE. requires LDAUU to be set.",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )

        # Check hybrid functional requirements
        lhf = parser.get_parameter("LHFCALC")
        if lhf and lhf.value:
            if not parser.get_parameter("HFSCREEN"):
                pass  # Has default, so optional
            if not parser.get_parameter("PRECFOCK"):
                pass  # Has default

        # Check spin-polarization and MAGMOM
        ispin = parser.get_parameter("ISPIN")
        magmom = parser.get_parameter("MAGMOM")

        if ispin and isinstance(ispin.value, int) and ispin.value == 2 and not magmom:
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=ispin.line_number - 1, character=0),
                        end=Position(line=ispin.line_number - 1, character=len(ispin.raw_line)),
                    ),
                    message="ISPIN=2 (spin-polarized) should have MAGMOM set for initial magnetic moments.",
                    severity=DiagnosticSeverity.Information,
                    source="vasp-lsp",
                )
            )

        return diagnostics

    def _check_workspace_consistency(
        self,
        parser: INCARParser,
        document_uri: str,
        workspace_documents: Optional[Dict[str, str]],
    ) -> List[Diagnostic]:
        """Check INCAR against POSCAR, KPOINTS, and POTCAR in the same calculation."""
        diagnostics: List[Diagnostic] = []
        poscar = self._parse_neighbor_poscar(document_uri, workspace_documents)
        kpoints = self._parse_neighbor_kpoints(document_uri, workspace_documents)
        potcar = self._parse_neighbor_potcar(document_uri, workspace_documents)

        poscar_data = poscar.parse() if poscar else None
        kpoints_data = kpoints.parse() if kpoints else None
        potcar_data = potcar.parse() if potcar else None

        if poscar_data:
            total_atoms = sum(poscar_data.atom_counts)
            species_count = len(poscar_data.atom_counts)

            magmom = parser.get_parameter("MAGMOM")
            if magmom:
                magmom_count = self._expanded_array_length(magmom.value)
                if magmom_count not in (0, total_atoms):
                    diagnostics.append(
                        self._parameter_warning(
                            magmom,
                            f"MAGMOM has {magmom_count} entries but POSCAR contains {total_atoms} atoms.",
                        )
                    )

            for name in ("LDAUL", "LDAUU", "LDAUJ"):
                param = parser.get_parameter(name)
                if param:
                    count = self._expanded_array_length(param.value)
                    if count not in (0, species_count):
                        diagnostics.append(
                            self._parameter_warning(
                                param,
                                f"{name} has {count} entries but POSCAR defines {species_count} species.",
                            )
                        )

        if parser.get_parameter("KSPACING") and kpoints_data:
            diagnostics.append(
                self._parameter_warning(
                    parser.get_parameter("KSPACING"),
                    "KSPACING is set while a KPOINTS file exists. VASP uses one k-point generation path; remove one source of k-point settings.",
                )
            )

        if potcar_data and poscar_data:
            potcar_species = [entry.element for entry in potcar_data.entries]
            if (
                poscar_data.atom_types
                and poscar_data.atom_types != potcar_species[: len(poscar_data.atom_types)]
            ):
                first_param = next(iter(parser.get_all_parameters().values()), None)
                diagnostics.append(
                    self._workspace_warning(
                        first_param,
                        "POSCAR species order "
                        f"{', '.join(poscar_data.atom_types)} differs from POTCAR order {', '.join(potcar_species)}.",
                    )
                )

        encut = parser.get_parameter("ENCUT")
        if encut and isinstance(encut.value, (int, float)) and potcar_data:
            enmax_values = [entry.enmax for entry in potcar_data.entries if entry.enmax is not None]
            if enmax_values:
                max_enmax = max(enmax_values)
                if float(encut.value) < max_enmax:
                    diagnostics.append(
                        self._parameter_warning(
                            encut,
                            f"ENCUT={encut.value:g} is below max POTCAR ENMAX={max_enmax:g} eV.",
                        )
                    )

        icharg = parser.get_parameter("ICHARG")
        if icharg and icharg.value in (1, 11):
            chgcar_content = self._read_neighbor(document_uri, "CHGCAR", workspace_documents)
            if chgcar_content is None:
                diagnostics.append(
                    self._parameter_info(
                        icharg,
                        f"ICHARG={icharg.value} usually requires a precomputed CHGCAR in the calculation directory.",
                    )
                )

        if kpoints_data and kpoints_data.mode == KPOINTSMode.LINE_MODE:
            icharg = parser.get_parameter("ICHARG")
            if not icharg or icharg.value not in (11, 12):
                anchor = icharg or next(iter(parser.get_all_parameters().values()), None)
                diagnostics.append(
                    self._workspace_info(
                        anchor,
                        "Line-mode KPOINTS is normally used for band structures with ICHARG=11 or 12 after a charge-density calculation.",
                    )
                )

        return diagnostics

    def _parameter_warning(self, param, message: str) -> Diagnostic:
        return self._parameter_diagnostic(param, message, DiagnosticSeverity.Warning)

    def _parameter_info(self, param, message: str) -> Diagnostic:
        return self._parameter_diagnostic(param, message, DiagnosticSeverity.Information)

    def _parameter_diagnostic(self, param, message: str, severity) -> Diagnostic:
        return Diagnostic(
            range=Range(
                start=Position(line=param.line_number - 1, character=0),
                end=Position(line=param.line_number - 1, character=len(param.raw_line)),
            ),
            message=message,
            severity=severity,
            source="vasp-lsp",
        )

    def _workspace_warning(self, param, message: str) -> Diagnostic:
        return self._workspace_diagnostic(param, message, DiagnosticSeverity.Warning)

    def _workspace_info(self, param, message: str) -> Diagnostic:
        return self._workspace_diagnostic(param, message, DiagnosticSeverity.Information)

    def _workspace_diagnostic(self, param, message: str, severity) -> Diagnostic:
        if param:
            return self._parameter_diagnostic(param, message, severity)
        return Diagnostic(
            range=Range(start=Position(line=0, character=0), end=Position(line=0, character=1)),
            message=message,
            severity=severity,
            source="vasp-lsp",
        )

    def _expanded_array_length(self, value) -> int:
        if value is None:
            return 0
        values = value if isinstance(value, list) else [value]
        count = 0
        for item in values:
            if isinstance(item, str):
                match = re.fullmatch(r"(\d+)\*[-+]?\d+(?:\.\d+)?", item.strip())
                if match:
                    count += int(match.group(1))
                    continue
            count += 1
        return count

    def _parse_neighbor_poscar(
        self, document_uri: str, workspace_documents: Optional[Dict[str, str]]
    ) -> Optional[POSCARParser]:
        content = self._read_neighbor(document_uri, "POSCAR", workspace_documents)
        if content is None:
            content = self._read_neighbor(document_uri, "CONTCAR", workspace_documents)
        return POSCARParser(content) if content is not None else None

    def _parse_neighbor_kpoints(
        self, document_uri: str, workspace_documents: Optional[Dict[str, str]]
    ) -> Optional[KPOINTSParser]:
        content = self._read_neighbor(document_uri, "KPOINTS", workspace_documents)
        return KPOINTSParser(content) if content is not None else None

    def _parse_neighbor_potcar(
        self, document_uri: str, workspace_documents: Optional[Dict[str, str]]
    ) -> Optional[POTCARParser]:
        content = self._read_neighbor(document_uri, "POTCAR", workspace_documents)
        return POTCARParser(content) if content is not None else None

    def _read_neighbor(
        self,
        document_uri: str,
        filename: str,
        workspace_documents: Optional[Dict[str, str]],
    ) -> Optional[str]:
        if workspace_documents:
            base_dir = os.path.dirname(self._uri_to_path(document_uri) or "")
            for uri, content in workspace_documents.items():
                path = self._uri_to_path(uri)
                if not path:
                    continue
                if os.path.dirname(path) == base_dir and os.path.basename(path).upper() == filename:
                    return content

        path = self._uri_to_path(document_uri)
        if not path:
            return None
        neighbor = os.path.join(os.path.dirname(path), filename)
        if not os.path.exists(neighbor):
            return None
        try:
            with open(neighbor, encoding="utf-8", errors="ignore") as handle:
                return handle.read()
        except OSError:
            return None

    def _uri_to_path(self, uri: str) -> Optional[str]:
        parsed = urlparse(uri)
        if parsed.scheme != "file":
            return None
        return unquote(parsed.path)

    def _get_poscar_diagnostics(self, content: str) -> List[Diagnostic]:
        """Get diagnostics for POSCAR files."""
        diagnostics = []
        parser = POSCARParser(content)
        result = parser.parse()

        # Report parse errors
        for error in parser.get_errors():
            severity = (
                DiagnosticSeverity.Error
                if error["severity"] == "error"
                else DiagnosticSeverity.Warning
            )
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=error["line"] - 1, character=error.get("column", 0)),
                        end=Position(line=error["line"] - 1, character=error.get("column", 0) + 1),
                    ),
                    message=error["message"],
                    severity=severity,
                    source="vasp-lsp",
                )
            )

        # Additional validation if parsing succeeded
        if result:
            diagnostics.extend(self._check_poscar_structure(result))

            # Check for negative scale factor
            if result.scale_factor < 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=1, character=0),
                            end=Position(line=1, character=20),
                        ),
                        message="Negative scale factor detected. This inverts the lattice.",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )

            # Check for zero atom counts
            total_atoms = sum(result.atom_counts)
            if total_atoms == 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=5, character=0),
                            end=Position(line=5, character=50),
                        ),
                        message="No atoms specified in POSCAR.",
                        severity=DiagnosticSeverity.Error,
                        source="vasp-lsp",
                    )
                )

            # Check coordinate ranges for direct coordinates
            if result.coordinate_type == "Direct":
                for i, coord in enumerate(result.coordinates):
                    for j, val in enumerate(coord):
                        if val < -0.5 or val > 1.5:
                            line_num = max(result.coordinate_start_line - 1 + i, 0)
                            diagnostics.append(
                                Diagnostic(
                                    range=Range(
                                        start=Position(line=line_num, character=0),
                                        end=Position(line=line_num, character=40),
                                    ),
                                    message=f"Direct coordinate {val:.3f} is outside typical range [0, 1].",
                                    severity=DiagnosticSeverity.Information,
                                    source="vasp-lsp",
                                )
                            )
                            break

        return diagnostics

    def _check_poscar_structure(self, result) -> List[Diagnostic]:
        diagnostics: List[Diagnostic] = []
        for index, vector in enumerate(result.lattice_vectors):
            length_sq = sum(component * component for component in vector)
            if length_sq == 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=2 + index, character=0),
                            end=Position(line=2 + index, character=40),
                        ),
                        message="Lattice vector length is zero.",
                        severity=DiagnosticSeverity.Error,
                        source="vasp-lsp",
                    )
                )

        volume = self._cell_volume(result.lattice_vectors) * (result.scale_factor**3)
        if abs(volume) < 1e-12:
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=2, character=0), end=Position(line=4, character=40)
                    ),
                    message="POSCAR cell volume is zero; lattice vectors are linearly dependent.",
                    severity=DiagnosticSeverity.Error,
                    source="vasp-lsp",
                )
            )

        invalid_elements = [
            element for element in result.atom_types if not re.fullmatch(r"[A-Z][a-z]?", element)
        ]
        if invalid_elements and not all(element.startswith("Type") for element in invalid_elements):
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=5, character=0), end=Position(line=5, character=80)
                    ),
                    message=f"Invalid element symbols in POSCAR: {', '.join(invalid_elements)}.",
                    severity=DiagnosticSeverity.Warning,
                    source="vasp-lsp",
                )
            )

        return diagnostics

    def _cell_volume(self, lattice_vectors: List[List[float]]) -> float:
        a, b, c = lattice_vectors
        return (
            a[0] * (b[1] * c[2] - b[2] * c[1])
            - a[1] * (b[0] * c[2] - b[2] * c[0])
            + a[2] * (b[0] * c[1] - b[1] * c[0])
        )

    def _get_kpoints_diagnostics(self, content: str) -> List[Diagnostic]:
        """Get diagnostics for KPOINTS files."""
        from ..parsers.kpoints_parser import KPOINTSParser

        diagnostics = []
        parser = KPOINTSParser(content)
        result = parser.parse()

        # Report parse errors
        for error in parser.get_errors():
            severity = (
                DiagnosticSeverity.Error
                if error["severity"] == "error"
                else DiagnosticSeverity.Warning
            )
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=error["line"] - 1, character=error.get("column", 0)),
                        end=Position(line=error["line"] - 1, character=error.get("column", 0) + 1),
                    ),
                    message=error["message"],
                    severity=severity,
                    source="vasp-lsp",
                )
            )

        # Additional validation if parsing succeeded
        if result and result.grid:
            # Check for zero or negative grid values
            for i, grid_val in enumerate(result.grid):
                if grid_val <= 0:
                    diagnostics.append(
                        Diagnostic(
                            range=Range(
                                start=Position(line=2, character=0),
                                end=Position(line=2, character=30),
                            ),
                            message=f"K-point grid value {grid_val} is not positive.",
                            severity=DiagnosticSeverity.Error,
                            source="vasp-lsp",
                        )
                    )
                    break

            # Check for very sparse grids
            if all(g < 2 for g in result.grid):
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=2, character=0),
                            end=Position(line=2, character=30),
                        ),
                        message="K-point grid is very sparse (all values < 2).",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )

        # Check explicit k-point weights sum
        if result and result.weights:
            total_weight = sum(result.weights)
            if abs(total_weight - 1.0) > 0.01 and total_weight > 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=3, character=0),
                            end=Position(line=3, character=30),
                        ),
                        message=f"K-point weights sum to {total_weight:.3f} (expected ~1.0).",
                        severity=DiagnosticSeverity.Information,
                        source="vasp-lsp",
                    )
                )

        return diagnostics
