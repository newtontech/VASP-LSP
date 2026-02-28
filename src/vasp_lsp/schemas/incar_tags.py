"""VASP INCAR tag definitions with metadata.

This module contains structured metadata for VASP INCAR parameters,
enabling autocomplete, validation, and documentation features.
"""

from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass, field


@dataclass
class INCARTag:
    """Metadata for a single INCAR tag."""
    
    name: str
    type: str  # "integer", "float", "boolean", "string", "array"
    default: Any
    description: str
    category: str  # "electronic", "ionic", "parallel", "output", etc.
    valid_range: Optional[tuple] = None
    enum_values: Optional[List[str]] = None
    requires: Optional[List[str]] = None  # Related tags that should be set
    conflicts_with: Optional[List[str]] = None  # Tags that conflict with this one
    version_note: Optional[str] = None  # Version-specific notes
    
    def to_markdown(self) -> str:
        """Generate markdown documentation for this tag."""
        lines = [f"### {self.name}", ""]
        lines.append(f"**Type:** {self.type}")
        lines.append(f"**Default:** {self.default}")
        lines.append(f"**Category:** {self.category}")
        if self.valid_range:
            lines.append(f"**Range:** {self.valid_range[0]} to {self.valid_range[1]}")
        if self.enum_values:
            lines.append(f"**Allowed values:** {', '.join(self.enum_values)}")
        lines.append("")
        lines.append(self.description)
        if self.requires:
            lines.append("")
            lines.append(f"**Related tags:** {', '.join(self.requires)}")
        if self.conflicts_with:
            lines.append("")
            lines.append(f"**Conflicts with:** {', '.join(self.conflicts_with)}")
        return "\n".join(lines)


# INCAR tag definitions
INCAR_TAGS: Dict[str, INCARTag] = {
    # Electronic structure
    "ENCUT": INCARTag(
        name="ENCUT",
        type="float",
        default=None,
        description="Cutoff energy for the plane-wave basis set in eV. If not specified, VASP uses the maximum ENMAX from POTCAR files.",
        category="electronic",
        valid_range=(0.0, None),
    ),
    
    "ISMEAR": INCARTag(
        name="ISMEAR",
        type="integer",
        default=1,
        description="Determines how the partial occupancies are set for each orbital. -5: Tetrahedron method with Blöchl corrections, -4: Tetrahedron method, -3: Smearing not used (for RPA/GW), -2: Partial occupancies read from WAVECAR/INCAR, -1: Fermi smearing, 0: Gaussian smearing, 1: Methfessel-Paxton order 1, N>1: Methfessel-Paxton order N.",
        category="electronic",
        enum_values=["-5", "-4", "-3", "-2", "-1", "0", "1", "2"],
    ),
    
    "SIGMA": INCARTag(
        name="SIGMA",
        type="float",
        default=0.2,
        description="Width of the smearing in eV. Default depends on ISMEAR. For ISMEAR >= 0, SIGMA determines the width of the smearing.",
        category="electronic",
        valid_range=(0.0, None),
        requires=["ISMEAR"],
    ),
    
    "EDIFF": INCARTag(
        name="EDIFF",
        type="float",
        default=1e-4,
        description="Convergence criterion for electronic self-consistency. The relaxation stops when the total energy change between two steps is smaller than EDIFF.",
        category="electronic",
        valid_range=(0.0, None),
    ),
    
    "NELM": INCARTag(
        name="NELM",
        type="integer",
        default=60,
        description="Maximum number of electronic self-consistency steps.",
        category="electronic",
        valid_range=(1, None),
    ),
    
    "NELMIN": INCARTag(
        name="NELMIN",
        type="integer",
        default=2,
        description="Minimum number of electronic self-consistency steps.",
        category="electronic",
        valid_range=(1, None),
    ),
    
    "ALGO": INCARTag(
        name="ALGO",
        type="string",
        default="Normal",
        description="Algorithm for electronic minimization. Options: Normal (Davidson), Fast (RMM-DIIS), Very_Fast (RMM-DIIS with more aggressive settings), All (all algorithms sequentially), Damped (damped second-order Vaswani algorithm), Eigenval (optimization of one-electron energies), None (no optimization), Exact (exact diagonalization), CHI (response function calculation), G0W0R, G0W0, G0W0R, scGW0R, scGW0, scGW0R, G0W0R, BSE.",
        category="electronic",
        enum_values=["Normal", "Fast", "Very_Fast", "All", "Damped", "Eigenval", "None", 
                     "Exact", "CHI", "G0W0", "scGW0", "scGW", "BSE"],
    ),
    
    "ISPIN": INCARTag(
        name="ISPIN",
        type="integer",
        default=1,
        description="Spin polarization: 1 = non-spin polarized, 2 = spin-polarized (collinear).",
        category="electronic",
        enum_values=["1", "2"],
    ),
    
    "MAGMOM": INCARTag(
        name="MAGMOM",
        type="array",
        default=None,
        description="Initial magnetic moments for each atom. Format: MAGMOM = m1 m2 m3 ... (one per atom) or MAGMOM = m1*m2*m3 (for ISPIN=2, multiple values per atom).",
        category="electronic",
        requires=["ISPIN"],
    ),
    
    "LORBIT": INCARTag(
        name="LORBIT",
        type="integer",
        default=None,
        description="Determines whether the PROCAR or PROOUT files are written and the format of the file. 0: no output, 1: simple output, 2: detailed output, 5: simple output + phase, 10: time-dependent DFT, 11: like 1 + phase information, 12: like 2 + phase information.",
        category="output",
        enum_values=["0", "1", "2", "5", "10", "11", "12"],
    ),
    
    "NEDOS": INCARTag(
        name="NEDOS",
        type="integer",
        default=301,
        description="Number of grid points on which the DOS is calculated.",
        category="output",
        valid_range=(1, None),
    ),
    
    # Ionic relaxation
    "IBRION": INCARTag(
        name="IBRION",
        type="integer",
        default=0,
        description="Determines how the ions are updated and moved. -1: no update (fixed positions), 0: molecular dynamics, 1: quasi-Newton (RMM-DIIS), 2: conjugate gradient, 3: damped molecular dynamics, 5: finite differences (phonons), 6: finite differences with symmetry, 7: perturbation theory, 8: perturbation theory with symmetry.",
        category="ionic",
        enum_values=["-1", "0", "1", "2", "3", "5", "6", "7", "8"],
    ),
    
    "NSW": INCARTag(
        name="NSW",
        type="integer",
        default=0,
        description="Maximum number of ionic steps. For IBRION=0 (MD), this is the number of MD steps.",
        category="ionic",
        valid_range=(0, None),
    ),
    
    "EDIFFG": INCARTag(
        name="EDIFFG",
        type="float",
        default=1e-3,
        description="Convergence criterion for ionic relaxation. If EDIFFG < 0, relaxation stops when all forces are smaller than |EDIFFG| in eV/Å. If EDIFFG > 0, relaxation stops when the energy change is smaller than EDIFFG in eV.",
        category="ionic",
    ),
    
    "POTIM": INCARTag(
        name="POTIM",
        type="float",
        default=None,
        description="Scaling factor for the forces or time step for MD. For IBRION=2: trial step size for translation. For IBRION=1: step width scaling. For IBRION=0: time step in fs.",
        category="ionic",
        valid_range=(0.0, None),
    ),
    
    "ISIF": INCARTag(
        name="ISIF",
        type="integer",
        default=2,
        description="Controls whether the stress tensor is calculated and which degrees of freedom are allowed to change. 0: MD, no stress tensor, 1: ionic relaxation, stress tensor calculated, 2: ionic relaxation, stress tensor calculated, 3: ionic + cell shape relaxation, 4: ionic + cell shape + volume relaxation, 5: cell shape + volume relaxation (constant pressure), 6: cell shape relaxation (constant volume), 7: cell volume relaxation (constant shape).",
        category="ionic",
        enum_values=["0", "1", "2", "3", "4", "5", "6", "7"],
    ),
    
    # K-points
    "KGAMMA": INCARTag(
        name="KGAMMA",
        type="boolean",
        default=False,
        description="If .TRUE., the k-point grid includes the Gamma point. Only relevant for Monkhorst-Pack grids.",
        category="electronic",
    ),
    
    # Parallelization
    "NCORE": INCARTag(
        name="NCORE",
        type="integer",
        default=1,
        description="Number of cores per orbital that work on an individual orbital. NCORE determines the number of compute cores working on an individual orbital.",
        category="parallel",
        valid_range=(1, None),
    ),
    
    "NPAR": INCARTag(
        name="NPAR",
        type="integer",
        default=None,
        description="Determines the number of bands treated in parallel. NPAR determines the number of parallel band groups. If not set, VASP attempts to determine it automatically.",
        category="parallel",
        valid_range=(1, None),
        conflicts_with=["NCORE"],
    ),
    
    "KPAR": INCARTag(
        name="KPAR",
        type="integer",
        default=1,
        description="Parallelization over k-points. KPAR determines the number of k-point groups that are solved in parallel.",
        category="parallel",
        valid_range=(1, None),
    ),
    
    # Output control
    "LWAVE": INCARTag(
        name="LWAVE",
        type="boolean",
        default=True,
        description="Determines whether the wavefunctions are written to the WAVECAR file.",
        category="output",
    ),
    
    "LCHARG": INCARTag(
        name="LCHARG",
        type="boolean",
        default=True,
        description="Determines whether the charge density is written to the CHGCAR file.",
        category="output",
    ),
    
    "LAECHG": INCARTag(
        name="LAECHG",
        type="boolean",
        default=False,
        description="Determines whether the all-electron charge density is written to the AECCAR0/AECCAR2 files for Bader analysis.",
        category="output",
    ),
    
    "LVHAR": INCARTag(
        name="LVHAR",
        type="boolean",
        default=False,
        description="Determines whether the electrostatic potential (Hartree potential) is written to the LOCPOT file.",
        category="output",
    ),
    
    "LVTOT": INCARTag(
        name="LVTOT",
        type="boolean",
        default=False,
        description="Determines whether the total local potential is written to the LOCPOT file.",
        category="output",
    ),
    
    "LELF": INCARTag(
        name="LELF",
        type="boolean",
        default=False,
        description="Determines whether the electron localization function (ELF) is written to the ELFCAR file.",
        category="output",
    ),
    
    "LORBITALREAL": INCARTag(
        name="LORBITALREAL",
        type="boolean",
        default=False,
        description="Determines whether real-space projection operators are used.",
        category="electronic",
    ),
    
    # Hybrid functionals
    "LHFCALC": INCARTag(
        name="LHFCALC",
        type="boolean",
        default=False,
        description="Determines whether Hartree-Fock type calculations are performed. If set to .TRUE., a hybrid functional calculation is performed.",
        category="electronic",
    ),
    
    "HFSCREEN": INCARTag(
        name="HFSCREEN",
        type="float",
        default=0.2,
        description="Screening parameter for HSE06 functional in Å⁻¹. Default is 0.2 (HSE06). For HSE03 use 0.3.",
        category="electronic",
        valid_range=(0.0, None),
        requires=["LHFCALC"],
    ),
    
    "PRECFOCK": INCARTag(
        name="PRECFOCK",
        type="string",
        default="Normal",
        description="Determines the FFT grid used for the exact exchange (Hartree-Fock) calculations. Options: Low, Medium, Normal, Fast, Accurate.",
        category="electronic",
        enum_values=["Low", "Medium", "Normal", "Fast", "Accurate"],
        requires=["LHFCALC"],
    ),
    
    # Van der Waals corrections
    "IVDW": INCARTag(
        name="IVDW",
        type="integer",
        default=0,
        description="Determines the type of van der Waals correction. 0: no correction, 1: DFT-D2, 11: DFT-D3, 12: DFT-D3 with Becke-Johnson damping, 2: TS method, 21: TS with iterative Hirshfeld partitioning, 202: MBD@rsSCS.",
        category="electronic",
        enum_values=["0", "1", "11", "12", "2", "21", "202"],
    ),
    
    # DFT+U
    "LDAU": INCARTag(
        name="LDAU",
        type="boolean",
        default=False,
        description="Determines whether the DFT+U calculation is performed (LSDA+U or GGA+U).",
        category="electronic",
    ),
    
    "LDAUTYPE": INCARTag(
        name="LDAUTYPE",
        type="integer",
        default=2,
        description="Determines the type of DFT+U approach. 1: Liechtenstein, 2: Dudarev (simpler, only Ueff = U - J matters), 4: Liechtenstein with rotationally invariant formulation.",
        category="electronic",
        enum_values=["1", "2", "4"],
        requires=["LDAU"],
    ),
    
    "LDAUL": INCARTag(
        name="LDAUL",
        type="array",
        default=None,
        description="LDAUL specifies the l-quantum number on which the projection is applied for each species. -1: no U, 0: s, 1: p, 2: d, 3: f.",
        category="electronic",
        requires=["LDAU"],
    ),
    
    "LDAUU": INCARTag(
        name="LDAUU",
        type="array",
        default=None,
        description="The effective on-site Coulomb interaction parameter Ueff = U - J for each species (in eV).",
        category="electronic",
        requires=["LDAU"],
    ),
    
    # Magnetic calculations
    "LSORBIT": INCARTag(
        name="LSORBIT",
        type="boolean",
        default=False,
        description="Determines whether spin-orbit coupling is included. If set to .TRUE., a non-collinear calculation with spin-orbit coupling is performed.",
        category="electronic",
    ),
    
    "SAXIS": INCARTag(
        name="SAXIS",
        type="array",
        default=[0.0, 0.0, 1.0],
        description="Quantization axis for non-collinear spin calculations. SAXIS = sx sy sz defines the direction of the magnetization.",
        category="electronic",
        requires=["LSORBIT"],
    ),
    
    # Charge mixing
    "AMIX": INCARTag(
        name="AMIX",
        type="float",
        default=0.4,
        description="Linear mixing parameter for the charge density. AMIX determines the mixing amplitude for the charge density.",
        category="electronic",
        valid_range=(0.0, 1.0),
    ),
    
    "BMIX": INCARTag(
        name="BMIX",
        type="float",
        default=1.0,
        description="Cutoff wave vector for Kerker mixing scheme in Å⁻¹.",
        category="electronic",
        valid_range=(0.0, None),
    ),
    
    "AMIN": INCARTag(
        name="AMIN",
        type="float",
        default=0.1,
        description="Minimum mixing parameter for the charge density.",
        category="electronic",
        valid_range=(0.0, 1.0),
    ),
    
    # Other
    "SYSTEM": INCARTag(
        name="SYSTEM",
        type="string",
        default="Unknown",
        description="A description of the calculation. This string is written to the OUTCAR and OSZICAR files.",
        category="general",
    ),
    
    "NWRITE": INCARTag(
        name="NWRITE",
        type="integer",
        default=2,
        description="Determines the verbosity of the output. 0: minimal, 1: reduced, 2: normal, 3: detailed, 4: extensive.",
        category="output",
        enum_values=["0", "1", "2", "3", "4"],
    ),
    
    "PREC": INCARTag(
        name="PREC",
        type="string",
        default="Normal",
        description="Determines the precision mode. Options: Normal (default), Accurate (more accurate forces), Single (faster, less accurate), Fast (deprecated).",
        category="electronic",
        enum_values=["Normal", "Accurate", "Single", "Fast"],
    ),
    
    "ISTART": INCARTag(
        name="ISTART",
        type="integer",
        default=0,
        description="Determines whether WAVECAR is read. 0: start from scratch, 1: read WAVECAR and continue.",
        category="electronic",
        enum_values=["0", "1"],
    ),
    
    "ICHARG": INCARTag(
        name="ICHARG",
        type="integer",
        default=None,
        description="Determines how the initial charge density is constructed. 0: superposition of atomic charge densities, 1: read from CHGCAR, 2: constant charge density (for band structure), 11: constant charge density from CHGCAR, 12: constant charge density for non-selfconsistent calculations.",
        category="electronic",
        enum_values=["0", "1", "2", "11", "12"],
    ),
    
    "NBANDS": INCARTag(
        name="NBANDS",
        type="integer",
        default=None,
        description="Number of bands included in the calculation. Default is determined from the number of valence electrons.",
        category="electronic",
        valid_range=(1, None),
    ),
    
    "NELECT": INCARTag(
        name="NELECT",
        type="float",
        default=None,
        description="Total number of electrons in the system. Can be used to set a different number of electrons than determined from POTCAR.",
        category="electronic",
    ),
    
    "LREAL": INCARTag(
        name="LREAL",
        type="string",
        default=".FALSE.",
        description="Determines whether the projection operators are evaluated in real space. Options: .FALSE., .TRUE., On, Auto. Use Auto for large systems (>30 atoms).",
        category="electronic",
        enum_values=[".FALSE.", ".TRUE.", "On", "Auto"],
    ),
    
    "ROPT": INCARTag(
        name="ROPT",
        type="array",
        default=None,
        description="Optimization of the real-space projection operators. One value per species.",
        category="electronic",
        requires=["LREAL"],
    ),
    
    "EMIN": INCARTag(
        name="EMIN",
        type="float",
        default=None,
        description="Minimum energy for DOS calculation in eV.",
        category="output",
    ),
    
    "EMAX": INCARTag(
        name="EMAX",
        type="float",
        default=None,
        description="Maximum energy for DOS calculation in eV.",
        category="output",
    ),
}

# List of all tag names for quick reference
INCAR_TAG_LIST: List[str] = list(INCAR_TAGS.keys())


def get_tag_info(name: str) -> Optional[INCARTag]:
    """Get metadata for a specific INCAR tag.
    
    Args:
        name: The INCAR tag name (case-insensitive).
        
    Returns:
        INCARTag object if found, None otherwise.
    """
    return INCAR_TAGS.get(name.upper())


def search_tags(query: str) -> List[INCARTag]:
    """Search for INCAR tags matching a query string.
    
    Args:
        query: Search string to match against tag names and descriptions.
        
    Returns:
        List of matching INCARTag objects.
    """
    query_lower = query.lower()
    results = []
    for tag in INCAR_TAGS.values():
        if query_lower in tag.name.lower() or query_lower in tag.description.lower():
            results.append(tag)
    return results
