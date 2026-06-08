"""Diagnostics provider for VASP-LSP."""

import math
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

            # Check for negative scale factor (use exact line from parser)
            if result.scale_factor < 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=result.scale_factor_line, character=0),
                            end=Position(
                                line=result.scale_factor_line,
                                character=len(str(result.scale_factor)),
                            ),
                        ),
                        message="Negative scale factor detected. This inverts the lattice.",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )

            # Check for zero atom counts (use exact line from parser)
            total_atoms = sum(result.atom_counts)
            if total_atoms == 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=result.atom_counts_line, character=0),
                            end=Position(
                                line=result.atom_counts_line,
                                character=len(" ".join(str(c) for c in result.atom_counts)),
                            ),
                        ),
                        message="No atoms specified in POSCAR.",
                        severity=DiagnosticSeverity.Error,
                        source="vasp-lsp",
                    )
                )

            # Check coordinate ranges for direct coordinates (use exact lines)
            if result.coordinate_type == "Direct":
                for i, coord in enumerate(result.coordinates):
                    for j, val in enumerate(coord):
                        if val < -0.5 or val > 1.5:
                            line_num = max(result.coordinate_start_line - 1 + i, 0)
                            coord_str = " ".join(f"{v:.6g}" for v in coord)
                            diagnostics.append(
                                Diagnostic(
                                    range=Range(
                                        start=Position(line=line_num, character=0),
                                        end=Position(line=line_num, character=len(coord_str)),
                                    ),
                                    message=f"Direct coordinate {val:.3f} is outside typical range [0, 1].",
                                    severity=DiagnosticSeverity.Information,
                                    source="vasp-lsp",
                                )
                            )
                            break

            # Check for extreme scale factor magnitude
            diagnostics.extend(self._check_scale_factor_magnitude(result))

            # Check lattice vector angles for nearly degenerate cell
            diagnostics.extend(self._check_lattice_angles(result))

            # Check coordinate count matches atom counts
            diagnostics.extend(self._check_coordinate_count(result))

            # Check for duplicate atom positions
            diagnostics.extend(self._check_duplicate_positions(result))

            # Check for very close atoms (potential overlap)
            if result.coordinate_type == "Cartesian":
                diagnostics.extend(self._check_close_atoms_cartesian(result))
            else:
                diagnostics.extend(self._check_close_atoms_direct(result))

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

    def _check_scale_factor_magnitude(self, result) -> List[Diagnostic]:
        """Warn if scale factor magnitude is too small or too large."""
        diagnostics: List[Diagnostic] = []
        abs_scale = abs(result.scale_factor)
        if abs_scale < 0.01 or abs_scale > 100:
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=1, character=0), end=Position(line=1, character=20)
                    ),
                    message=f"Scale factor {result.scale_factor:g} has extreme magnitude "
                    f"(|scale| = {abs_scale:g}). Consider using 1.0.",
                    severity=DiagnosticSeverity.Warning,
                    source="vasp-lsp",
                )
            )
        return diagnostics

    def _check_lattice_angles(self, result) -> List[Diagnostic]:
        """Warn if any angle between lattice vectors is < 10 or > 170 degrees."""
        diagnostics: List[Diagnostic] = []
        vecs = result.lattice_vectors
        pairs = [(0, 1, "a", "b"), (0, 2, "a", "c"), (1, 2, "b", "c")]
        for i, j, name_i, name_j in pairs:
            vi = vecs[i]
            vj = vecs[j]
            len_i = math.sqrt(sum(c * c for c in vi))
            len_j = math.sqrt(sum(c * c for c in vj))
            if len_i < 1e-12 or len_j < 1e-12:
                continue
            dot = sum(a * b for a, b in zip(vi, vj))
            cos_angle = max(-1.0, min(1.0, dot / (len_i * len_j)))
            angle_deg = math.degrees(math.acos(cos_angle))
            if angle_deg < 10 or angle_deg > 170:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=2 + i, character=0),
                            end=Position(line=2 + j, character=40),
                        ),
                        message=f"Angle between lattice vectors {name_i} and {name_j} is "
                        f"{angle_deg:.1f} degrees, indicating a nearly degenerate cell.",
                        severity=DiagnosticSeverity.Warning,
                        source="vasp-lsp",
                    )
                )
        return diagnostics

    def _check_coordinate_count(self, result) -> List[Diagnostic]:
        """Error if number of coordinate rows does not match sum of atom_counts."""
        diagnostics: List[Diagnostic] = []
        expected = sum(result.atom_counts)
        actual = len(result.coordinates)
        if actual != expected:
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=max(result.coordinate_start_line - 1, 0), character=0),
                        end=Position(
                            line=max(
                                result.coordinate_start_line - 1 + max(actual, expected) - 1, 0
                            ),
                            character=40,
                        ),
                    ),
                    message=f"Expected {expected} coordinate rows but found {actual}.",
                    severity=DiagnosticSeverity.Error,
                    source="vasp-lsp",
                )
            )
        return diagnostics

    def _check_duplicate_positions(self, result) -> List[Diagnostic]:
        """Warn when two atoms have identical coordinates (within tolerance 1e-6)."""
        diagnostics: List[Diagnostic] = []
        tolerance = 1e-6
        seen: List[int] = []
        for i, coord_i in enumerate(result.coordinates):
            if i in seen:
                continue
            for j in range(i + 1, len(result.coordinates)):
                if j in seen:
                    continue
                dist_sq = sum((a - b) ** 2 for a, b in zip(coord_i, result.coordinates[j]))
                if dist_sq < tolerance * tolerance:
                    line_j = max(result.coordinate_start_line - 1 + j, 0)
                    diagnostics.append(
                        Diagnostic(
                            range=Range(
                                start=Position(line=line_j, character=0),
                                end=Position(line=line_j, character=40),
                            ),
                            message=f"Atoms {i + 1} and {j + 1} have identical coordinates.",
                            severity=DiagnosticSeverity.Warning,
                            source="vasp-lsp",
                        )
                    )
                    seen.append(j)
                    break
        return diagnostics

    def _check_close_atoms_cartesian(self, result) -> List[Diagnostic]:
        """Warn if any two Cartesian atoms are closer than 0.5 Angstrom."""
        diagnostics: List[Diagnostic] = []
        min_dist = 0.5
        coords = result.coordinates
        for i in range(len(coords)):
            for j in range(i + 1, len(coords)):
                diff = [coords[i][k] - coords[j][k] for k in range(3)]
                dist = math.sqrt(sum(d * d for d in diff))
                if 0 < dist < min_dist:
                    line_j = max(result.coordinate_start_line - 1 + j, 0)
                    diagnostics.append(
                        Diagnostic(
                            range=Range(
                                start=Position(line=line_j, character=0),
                                end=Position(line=line_j, character=40),
                            ),
                            message=f"Atoms {i + 1} and {j + 1} are {dist:.3f} Angstrom apart "
                            f"(below minimum {min_dist} Angstrom).",
                            severity=DiagnosticSeverity.Warning,
                            source="vasp-lsp",
                        )
                    )
        return diagnostics

    def _check_close_atoms_direct(self, result) -> List[Diagnostic]:
        """Warn if any two Direct atoms are closer than 0.5 Angstrom."""
        diagnostics: List[Diagnostic] = []
        min_dist = 0.5
        coords = result.coordinates
        scale = result.scale_factor
        vecs = result.lattice_vectors
        for i in range(len(coords)):
            for j in range(i + 1, len(coords)):
                diff_frac = [coords[i][k] - coords[j][k] for k in range(3)]
                diff_cart = [
                    scale * sum(vecs[r][k] * diff_frac[r] for r in range(3)) for k in range(3)
                ]
                dist = math.sqrt(sum(d * d for d in diff_cart))
                if 0 < dist < min_dist:
                    line_j = max(result.coordinate_start_line - 1 + j, 0)
                    diagnostics.append(
                        Diagnostic(
                            range=Range(
                                start=Position(line=line_j, character=0),
                                end=Position(line=line_j, character=40),
                            ),
                            message=f"Atoms {i + 1} and {j + 1} are {dist:.3f} Angstrom apart "
                            f"(below minimum {min_dist} Angstrom).",
                            severity=DiagnosticSeverity.Warning,
                            source="vasp-lsp",
                        )
                    )
        return diagnostics

    def _get_kpoints_diagnostics(self, content: str) -> List[Diagnostic]:
        """Get diagnostics for KPOINTS files."""
        from ..parsers.kpoints_parser import KPOINTSMode, KPOINTSParser

        diagnostics: List[Diagnostic] = []
        parser = KPOINTSParser(content)
        result = parser.parse()
        lines = content.split("\n")

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

        if result is None:
            return diagnostics

        # Grid-based checks (automatic, gamma/monkhorst modes)
        if result.grid:
            diagnostics.extend(self._check_kpoints_grid(result, lines))

        # Gamma vs Monkhorst-Pack hint
        if result.mode == KPOINTSMode.GAMMA_MONKHORST:
            diagnostics.extend(self._check_kpoints_centering(result, lines))

        # Explicit k-point checks (weights, coordinates)
        if result.mode == KPOINTSMode.EXPLICIT:
            diagnostics.extend(self._check_kpoints_explicit(result, lines))

        # Line-mode density check
        if result.mode == KPOINTSMode.LINE_MODE:
            diagnostics.extend(self._check_kpoints_line_mode(result, lines))

        return diagnostics

    def _check_kpoints_grid(self, result, lines: List[str]) -> List[Diagnostic]:
        """Check k-point grid values for sparsity, density, and validity."""
        diagnostics: List[Diagnostic] = []

        # Use exact grid line from parser when available
        grid_line = result.grid_line_idx if result.grid_line_idx is not None else 2
        grid_text = lines[grid_line].strip() if grid_line < len(lines) else ""
        grid_end_char = len(grid_text)

        # Check for zero or negative grid values
        for grid_val in result.grid:
            if grid_val <= 0:
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=grid_line, character=0),
                            end=Position(line=grid_line, character=grid_end_char),
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
                        start=Position(line=grid_line, character=0),
                        end=Position(line=grid_line, character=grid_end_char),
                    ),
                    message="K-point grid is very sparse (all values < 2).",
                    severity=DiagnosticSeverity.Warning,
                    source="vasp-lsp",
                )
            )

        # Check for very dense grids
        if all(g > 50 for g in result.grid):
            grid_str = " ".join(str(g) for g in result.grid)
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=grid_line, character=0),
                        end=Position(line=grid_line, character=grid_end_char),
                    ),
                    message=(
                        f"K-point grid {grid_str} is very dense (all values > 50). "
                        "Computationally expensive; may not converge faster than a moderate grid."
                    ),
                    severity=DiagnosticSeverity.Warning,
                    source="vasp-lsp",
                )
            )

        # Check explicit k-point weights sum
        if result.weights:
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

    def _check_kpoints_centering(self, result, lines: List[str]) -> List[Diagnostic]:
        """Provide a hint about Gamma vs Monkhorst-Pack centering."""
        diagnostics: List[Diagnostic] = []

        # Line 3 (0-indexed) contains the generation type for Gamma/Monkhorst mode
        if len(lines) > 2:
            line3 = lines[2].strip().lower()
            if line3.startswith("g"):
                centering = "Gamma-centered"
            elif line3.startswith("m"):
                centering = "Monkhorst-Pack"
            else:
                return diagnostics

            grid_str = "x".join(str(g) for g in (result.grid or []))
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=2, character=0),
                        end=Position(line=2, character=30),
                    ),
                    message=(
                        f"Using {centering} mesh with grid {grid_str}. "
                        "Gamma-centered includes the Gamma point; "
                        "Monkhorst-Pack shifts the mesh off Gamma."
                    ),
                    severity=DiagnosticSeverity.Information,
                    source="vasp-lsp",
                )
            )

        return diagnostics

    def _check_kpoints_explicit(self, result, lines: List[str]) -> List[Diagnostic]:
        """Check explicit k-points for zero weights and unreasonable coordinates."""
        diagnostics: List[Diagnostic] = []

        if not result.weights or not result.kpoints:
            return diagnostics

        # Use exact start line from parser when available
        kpt_start = (
            result.kpoints_start_line_idx if result.kpoints_start_line_idx is not None else 3
        )

        # Check for zero-weight k-points
        zero_weight_indices = [i for i, w in enumerate(result.weights) if w == 0.0]
        if zero_weight_indices:
            first_idx = zero_weight_indices[0]
            kpoint_line = kpt_start + first_idx
            kpt_text = lines[kpoint_line].strip() if kpoint_line < len(lines) else ""
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=kpoint_line, character=0),
                        end=Position(line=kpoint_line, character=len(kpt_text)),
                    ),
                    message=(
                        f"K-point {first_idx + 1} has weight 0.0. "
                        "Zero-weight k-points are excluded from integration."
                    ),
                    severity=DiagnosticSeverity.Warning,
                    source="vasp-lsp",
                )
            )

        # Check for unreasonable k-point coordinates (outside [-1, 1] for reciprocal)
        for i, kpoint in enumerate(result.kpoints):
            for coord in kpoint:
                if abs(coord) > 1.0:
                    kpoint_line = kpt_start + i
                    coord_str = " ".join(f"{c:.4f}" for c in kpoint)
                    diagnostics.append(
                        Diagnostic(
                            range=Range(
                                start=Position(line=kpoint_line, character=0),
                                end=Position(line=kpoint_line, character=len(coord_str)),
                            ),
                            message=(
                                f"K-point coordinates ({coord_str}) are outside the "
                                "typical reciprocal range [-1, 1]. "
                                "Verify coordinates are in fractional reciprocal space."
                            ),
                            severity=DiagnosticSeverity.Error,
                            source="vasp-lsp",
                        )
                    )
                    break  # One diagnostic per k-point line is enough

        # Check weights sum (also applies to explicit mode)
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

    def _check_kpoints_line_mode(self, result, lines: List[str]) -> List[Diagnostic]:
        """Check line-mode k-point density."""
        diagnostics: List[Diagnostic] = []

        if result.line_density is not None and result.line_density < 10:
            diagnostics.append(
                Diagnostic(
                    range=Range(
                        start=Position(line=1, character=0),
                        end=Position(line=1, character=20),
                    ),
                    message=(
                        f"Line-mode density is {result.line_density} (< 10). "
                        "Low point density may miss band structure features."
                    ),
                    severity=DiagnosticSeverity.Information,
                    source="vasp-lsp",
                )
            )

        return diagnostics
