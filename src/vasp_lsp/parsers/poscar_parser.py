"""POSCAR file parser for VASP-LSP."""

import re
from typing import List, Optional, Tuple, Dict, Any
from dataclasses import dataclass


@dataclass
class POSCARData:
    """Represents parsed POSCAR data."""
    system_comment: str
    scale_factor: float
    lattice_vectors: List[List[float]]  # 3x3 matrix
    atom_types: List[str]
    atom_counts: List[int]
    coordinate_type: str  # "Direct" or "Cartesian"
    coordinates: List[List[float]]
    selective_dynamics: Optional[List[List[bool]]] = None


class POSCARParser:
    """Parser for VASP POSCAR/CONTCAR structure files."""
    
    def __init__(self, content: str):
        """Initialize parser with POSCAR file content.
        
        Args:
            content: The full content of the POSCAR file.
        """
        self.content = content
        self.lines = content.split('\n')
        self.data: Optional[POSCARData] = None
        self.errors: List[Dict[str, Any]] = []
        
    def parse(self) -> Optional[POSCARData]:
        """Parse the POSCAR file content.
        
        Returns:
            POSCARData object if successful, None otherwise.
        """
        self.errors = []
        
        try:
            line_idx = 0
            
            # Line 1: System comment
            system_comment = self.lines[line_idx].strip()
            line_idx += 1
            
            # Line 2: Scale factor
            try:
                scale_factor = float(self.lines[line_idx].strip())
            except (ValueError, IndexError) as e:
                self.errors.append({
                    'message': f"Invalid scale factor: {e}",
                    'line': line_idx + 1,
                    'severity': 'error'
                })
                return None
            line_idx += 1
            
            # Lines 3-5: Lattice vectors
            lattice_vectors = []
            for i in range(3):
                try:
                    parts = self.lines[line_idx].strip().split()
                    if len(parts) < 3:
                        raise ValueError(f"Expected 3 values, got {len(parts)}")
                    vector = [float(parts[0]), float(parts[1]), float(parts[2])]
                    lattice_vectors.append(vector)
                except (ValueError, IndexError) as e:
                    self.errors.append({
                        'message': f"Invalid lattice vector {i+1}: {e}",
                        'line': line_idx + 1,
                        'severity': 'error'
                    })
                    return None
                line_idx += 1
                
            # Line 6: Atom types (optional in VASP 5 format, required in VASP 4)
            atom_types_line = self.lines[line_idx].strip()
            # Check if it's atom symbols or atom counts
            if atom_types_line[0].isalpha():
                atom_types = atom_types_line.split()
                line_idx += 1
            else:
                atom_types = []
                
            # Line 6/7: Atom counts
            try:
                atom_counts = [int(x) for x in self.lines[line_idx].strip().split()]
                if atom_types and len(atom_types) != len(atom_counts):
                    self.errors.append({
                        'message': f"Mismatch between atom types ({len(atom_types)}) and counts ({len(atom_counts)})",
                        'line': line_idx + 1,
                        'severity': 'error'
                    })
            except (ValueError, IndexError) as e:
                self.errors.append({
                    'message': f"Invalid atom counts: {e}",
                    'line': line_idx + 1,
                    'severity': 'error'
                })
                return None
            line_idx += 1
            
            # If no atom types were specified, create generic ones
            if not atom_types:
                atom_types = [f"Type{i+1}" for i in range(len(atom_counts))]
                
            # Check for selective dynamics line
            selective_dynamics = None
            coord_type_line = self.lines[line_idx].strip().lower()
            if coord_type_line.startswith('s'):
                selective_dynamics = []
                line_idx += 1
                coord_type_line = self.lines[line_idx].strip().lower()
                
            # Coordinate type
            if coord_type_line.startswith('d'):
                coordinate_type = "Direct"
            elif coord_type_line.startswith('c') or coord_type_line.startswith('k'):
                coordinate_type = "Cartesian"
            else:
                self.errors.append({
                    'message': f"Unknown coordinate type: {coord_type_line}",
                    'line': line_idx + 1,
                    'severity': 'error'
                })
                return None
            line_idx += 1
            
            # Read coordinates
            total_atoms = sum(atom_counts)
            coordinates = []
            if selective_dynamics is not None:
                selective_dynamics = []
                
            for i in range(total_atoms):
                try:
                    parts = self.lines[line_idx].strip().split()
                    if len(parts) < 3:
                        raise ValueError(f"Expected at least 3 values, got {len(parts)}")
                    coord = [float(parts[0]), float(parts[1]), float(parts[2])]
                    coordinates.append(coord)
                    
                    # Parse selective dynamics flags if present
                    if selective_dynamics is not None:
                        if len(parts) >= 6:
                            flags = [
                                parts[3].upper() == 'T',
                                parts[4].upper() == 'T',
                                parts[5].upper() == 'T'
                            ]
                        else:
                            flags = [True, True, True]  # Default to movable
                        selective_dynamics.append(flags)
                        
                except (ValueError, IndexError) as e:
                    self.errors.append({
                        'message': f"Invalid coordinate for atom {i+1}: {e}",
                        'line': line_idx + 1,
                        'severity': 'error'
                    })
                    return None
                line_idx += 1
                
            self.data = POSCARData(
                system_comment=system_comment,
                scale_factor=scale_factor,
                lattice_vectors=lattice_vectors,
                atom_types=atom_types,
                atom_counts=atom_counts,
                coordinate_type=coordinate_type,
                coordinates=coordinates,
                selective_dynamics=selective_dynamics
            )
            
            return self.data
            
        except Exception as e:
            self.errors.append({
                'message': f"Unexpected parse error: {str(e)}",
                'line': 0,
                'severity': 'error'
            })
            return None
            
    def get_errors(self) -> List[Dict[str, Any]]:
        """Get all parse errors.
        
        Returns:
            List of error dictionaries.
        """
        return self.errors
