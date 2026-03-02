# Development Plan for vasp-lsp

## Overview
This is the development plan for vasp-lsp.

## Phase 1: Foundation (Week 1)
- [x] Setup project structure
- [x] Basic parser implementation (INCAR, POSCAR, KPOINTS)
- [x] Initial test suite (40 tests)

## Phase 2: Core Features (Week 2)
- [x] LSP server implementation
- [x] Diagnostics support
- [x] Completion provider

## Phase 3: Advanced Features (Week 3)
- [x] Hover documentation
- [ ] Code formatting
- [ ] Quick fixes

## Testing
- Run tests: `pytest tests/`
- Check coverage: `pytest --cov=vasp_lsp`

## Maintenance
- Nightly automated maintenance at random time
- See .maintenance/last-run.md for last check

## Recent Updates (2026-03-02)
- Fixed test compatibility with lsprotocol types
- Updated server initialization for pygls
- All 40 tests passing
- Added comprehensive INCAR tag schema with 50+ parameters
