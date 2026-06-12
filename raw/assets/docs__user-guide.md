# VASP-LSP User Guide

Complete guide for using VASP-LSP Language Server Protocol implementation for VASP input files.

## Installation

### Python Package

```bash
pip install vasp-lsp
```

### Development Installation

```bash
git clone https://github.com/newtontech/VASP-LSP.git
cd VASP-LSP
pip install -e ".[dev]"
```

## Quick Start

### Running as Standalone Server

```bash
# Standard I/O mode
vasp-lsp --stdio

# TCP mode (for debugging)
vasp-lsp --tcp --host 127.0.0.1 --port 2087
```

### Neovim Configuration

```lua
require'lspconfig'.vasp_lsp.setup{}
```

## Supported File Types

- **INCAR**: Main input file with 100+ supported parameters
- **POSCAR**: Structure file with lattice and coordinates
- **KPOINTS**: K-point grid specification

## Features

### Autocomplete
- Parameter name suggestions
- Valid values for enums
- Boolean values (.TRUE./.FALSE.)

### Hover Documentation
- Parameter descriptions
- Valid values/ranges
- Default values

### Diagnostics
- Unknown parameter warnings
- Invalid value errors
- Missing required parameters
- Parameter conflicts

### Quick Fixes
- Add missing SIGMA when ISMEAR >= 0
- Add MAGMOM when ISPIN = 2
- Fix negative scale factors
- Wrap out-of-range coordinates

### Document Formatting
- Group INCAR parameters by category
- Align parameter values
- Normalize coordinate precision

## License

MIT License
