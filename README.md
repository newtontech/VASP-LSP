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
- ✨ **Document Formatting** - Format INCAR, POSCAR, and KPOINTS files (NEW)

## Installation

```bash
pip install vasp-lsp
```

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

## Formatting

The server supports document formatting via the LSP `textDocument/formatting` request:

- **INCAR**: Parameters are grouped by category (Electronic, Ionic, Mixing, Parallel, Output) and aligned
- **POSCAR**: Lattice vectors and coordinates are formatted with consistent precision
- **KPOINTS**: Grid types are normalized, k-point lists are properly formatted

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

Current coverage: 98% (327 tests passing, 100% for core modules)

## License

MIT License

## Acknowledgments

- Inspired by [cp2k-language-server](https://github.com/cp2k/cp2k-input-tools)
- Built with [pygls](https://github.com/openlawlibrary/pygls)
