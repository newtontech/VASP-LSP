# Changelog

All notable changes to this project will be documented in this file.

## [0.1.0] - 2026-03-02

### Added
- Initial LSP server implementation for VASP input files
- INCAR file parser with support for:
  - Key-value parameter parsing
  - Boolean values (.TRUE., .FALSE., T, F)
  - Numeric values (integers, floats)
  - Array values
  - Comments (# and !)
  - Error detection for invalid formats
  - Duplicate parameter warnings

- POSCAR file parser with support for:
  - VASP 4 and VASP 5 formats
  - Lattice vectors parsing
  - Atom types and counts
  - Direct and Cartesian coordinates
  - Selective dynamics flags

- KPOINTS file parser with support for:
  - Automatic k-point mode
  - Gamma-centered and Monkhorst-Pack grids
  - Line mode for band structure
  - Explicit k-point lists

- LSP Features:
  - Autocomplete for INCAR tags and values
  - Hover documentation for INCAR parameters
  - Real-time diagnostics and error checking
  - Parameter dependency validation
  - Unknown tag warnings

- Test suite with 40 test cases
- Pre-commit hooks configuration
- Type hints throughout the codebase

### Changed
- Fixed test compatibility with lsprotocol types
- Updated pygls integration for initialize handling
- Improved error handling in parsers

### Documentation
- README with installation and usage instructions
- Inline documentation for all public APIs
