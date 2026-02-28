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

## Installation

```bash
pip install vasp-lsp
```

## Usage

### As a standalone server

```bash
vasp-lsp --stdio
```

### Neovim (nvim-lspconfig)

```lua
require'lspconfig'.vasp_lsp.setup{}
```

## Development

```bash
git clone https://github.com/yourusername/VASP-LSP.git
cd VASP-LSP
pip install -e ".[dev]"
```

## License

MIT License

## Acknowledgments

- Inspired by [cp2k-language-server](https://github.com/cp2k/cp2k-input-tools)
- Built with [pygls](https://github.com/openlawlibrary/pygls)
