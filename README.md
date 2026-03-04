# VASP-LSP

A Language Server Protocol (LSP) implementation for VASP (Vienna Ab initio Simulation Package) input/output files.

## Overview

VASP-LSP provides intelligent code editing features for VASP calculation input files:
- **INCAR** - Input parameters with autocomplete and validation
- **POSCAR** - Structure file syntax highlighting  
- **KPOINTS** - K-point grid configuration

## Features

- 📝 **Autocomplete** - Smart completion for INCAR tags and values
- 📖 **Hover Documentation** - Instant access to VASP parameter documentation
- ⚠️ **Diagnostics** - Real-time error detection and warnings
- 🔍 **Go to Definition** - Navigate between related parameters
- ✨ **Document Formatting** - Format INCAR, POSCAR, and KPOINTS files
- 🔧 **Quick Fixes** - Automatic fixes for common issues (NEW)

## Installation

```bash
pip install vasp-lsp
```

### VSCode Extension

A VSCode extension is available in `editors/vscode/`. See the [extension README](editors/vscode/README.md) for installation instructions.

## Usage

### As a standalone server

```bash
vasp-lsp --stdio
```

### TCP mode (for debugging)

```bash
vasp-lsp --tcp --host 127.0.0.1 --port 2087
```

### Neovim (nvim-lspconfig)

```lua
require'lspconfig'.vasp_lsp.setup{}
```

## Features Details

### Autocomplete
Provides intelligent completions for:
- INCAR parameter names
- Parameter values (enums, booleans)
- Context-aware suggestions

### Hover Documentation
Hover over any INCAR parameter to see:
- Parameter description
- Valid values/range
- Default value
- Related parameters

### Diagnostics
Real-time validation including:
- Unknown parameter detection
- Value type checking
- Range validation
- Parameter dependency checks
- Common configuration warnings

### Document Formatting
Format your VASP input files:
- **INCAR**: Parameters grouped by category, aligned values
- **POSCAR**: Consistent coordinate precision, proper spacing
- **KPOINTS**: Normalized grid types, formatted k-point lists

### Quick Fixes (NEW)
Automatic fixes for common issues:
- Add missing SIGMA when ISMEAR >= 0
- Add missing MAGMOM when ISPIN = 2
- Add missing LDAU parameters
- Remove conflicting NPAR/NCORE
- Fix common tag typos

## Development

```bash
git clone https://github.com/newtontech/VASP-LSP.git
cd VASP-LSP
pip install -e ".[dev]"
```

## Testing

Run tests with:

```bash
pytest --cov=src/vasp_lsp --cov-report=term-missing
```

Current coverage: 100% (416 tests passing)


## Code Quality

The project maintains high code quality through:
- **100% test coverage** - All code paths are tested
- **Code cleanup** - Dead code and unreachable branches removed
- **Static analysis** - Linting with Ruff
- **Type hints** - Full type annotations for better IDE support

## License

MIT License

## Acknowledgments

- Inspired by [cp2k-language-server](https://github.com/cp2k/cp2k-input-tools)
- Built with [pygls](https://github.com/openlawlibrary/pygls)
