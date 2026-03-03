# Development Plan for vasp-lsp

## Overview
This is the development plan for vasp-lsp - a Language Server Protocol implementation for VASP input files.

## Phase 1: Foundation ✅ (Completed)
- [x] Setup project structure
- [x] Basic parser implementation (INCAR, POSCAR, KPOINTS)
- [x] Initial test suite

## Phase 2: Core Features ✅ (Completed)
- [x] LSP server implementation
- [x] Diagnostics support
- [x] Completion provider
- [x] Hover documentation

## Phase 3: Advanced Features ✅ (Completed)
- [x] Hover documentation for INCAR tags
- [x] Improve test coverage to 100%
- [x] Code formatting
- [x] Quick fixes (NEW - 2026-03-03)

## Testing
- Run tests: `pytest tests/`
- Check coverage: `pytest --cov`
- Current status: 363 tests, 97% coverage

## Coverage Analysis (2026-03-03)
All core parsers have 100% coverage:
- `src/vasp_lsp/__init__.py`: 100%
- `src/vasp_lsp/features/__init__.py`: 100%
- `src/vasp_lsp/parsers/__init__.py`: 100%
- `src/vasp_lsp/schemas/__init__.py`: 100%
- `src/vasp_lsp/features/completion.py`: 100%
- `src/vasp_lsp/features/diagnostics.py`: 100%
- `src/vasp_lsp/features/hover.py`: 100%
- `src/vasp_lsp/parsers/incar_parser.py`: 100%
- `src/vasp_lsp/parsers/kpoints_parser.py`: 100%
- `src/vasp_lsp/parsers/poscar_parser.py`: 100%
- `src/vasp_lsp/schemas/incar_tags.py`: 100%
- `src/vasp_lsp/features/formatting.py`: 95%
- `src/vasp_lsp/features/quickfixes.py`: 91% (NEW)
- `src/vasp_lsp/server.py`: 92%

## Maintenance
- Nightly automated maintenance at random time
- See .maintenance/last-run.md for last check

## Recent Updates (2026-03-03)
- ✅ Added Quick Fixes feature (code actions)
- ✅ Added 14 quick fix tests
- ✅ Integrated quick fixes with LSP server
- ✅ Updated documentation
- ✅ All 363 tests passing
- ✅ 97% overall coverage
- ⚠️ GitHub issues disabled, no PRs pending

## Quick Fixes Feature Details
The new quick fixes feature provides automatic fixes for:
- **Missing SIGMA**: Add SIGMA=0.2 when ISMEAR >= 0
- **Missing MAGMOM**: Add default MAGMOM when ISPIN = 2
- **LDAU parameters**: Add missing LDAUTYPE, LDAUL, LDAUU
- **NPAR/NCORE conflict**: Remove NPAR when both are set
- **Tag typos**: Suggest correct tag names for unknown tags

## Next Steps
1. Create VSCode extension configuration
2. Add integration tests with real LSP clients
3. Improve quick fixes coverage to 100%
4. Add more POSCAR/KPOINTS diagnostics and quick fixes

## Version History
- v0.1.0: Initial release with core features
- v0.2.0: Added formatting support
- v0.3.0: Added quick fixes (planned)
