# Development Plan for vasp-lsp

## Overview
This is the development plan for vasp-lsp - a Language Server Protocol implementation for VASP input files.

## Phase 1: Foundation ✅ (Completed)
- [x] Setup project structure
- [x] Basic parser implementation (INCAR, POSCAR, KPOINTS)
- [x] Initial test suite (61 tests, 70% coverage)

## Phase 2: Core Features ✅ (Completed)
- [x] LSP server implementation
- [x] Diagnostics support
- [x] Completion provider
- [x] Hover documentation

## Phase 3: Advanced Features (In Progress)
- [x] Hover documentation for INCAR tags
- [ ] Code formatting (planned)
- [ ] Quick fixes (planned)
- [x] Improve test coverage to 100% (current: 83%)

## Testing
- Run tests: `pytest tests/`
- Check coverage: `pytest --cov`
- Current status: 97 tests, 83% coverage

## Coverage Analysis (2026-03-03)
Files with coverage:
- `src/vasp_lsp/__init__.py`: 100%
- `src/vasp_lsp/features/__init__.py`: 100%
- `src/vasp_lsp/parsers/__init__.py`: 100%
- `src/vasp_lsp/schemas/__init__.py`: 100%
- `src/vasp_lsp/features/completion.py`: 94%
- `src/vasp_lsp/features/diagnostics.py`: 88%
- `src/vasp_lsp/features/hover.py`: 92%
- `src/vasp_lsp/parsers/incar_parser.py`: 91%
- `src/vasp_lsp/parsers/kpoints_parser.py`: 61%
- `src/vasp_lsp/parsers/poscar_parser.py`: 92%
- `src/vasp_lsp/server.py`: 84%
- `src/vasp_lsp/schemas/incar_tags.py`: 74%

Remaining work:
- `src/vasp_lsp/parsers/kpoints_parser.py` - Add tests for explicit mode, reciprocal mode, line mode
- `src/vasp_lsp/server.py` - Add tests for TCP mode, error handling
- `src/vasp_lsp/schemas/incar_tags.py` - Add tests for helper functions
- `src/vasp_lsp/parsers/incar_parser.py` - Add tests for edge cases
- `src/vasp_lsp/parsers/poscar_parser.py` - Add tests for edge cases

## Maintenance
- Nightly automated maintenance at random time
- See .maintenance/last-run.md for last check

## Recent Updates (2026-03-03)
- ✅ Fixed TextDocumentSyncKind.FULL -> TextDocumentSyncKind.Full
- ✅ Added 36 additional tests for LSP handlers and features
- ✅ Improved coverage from 70% to 83%
- ✅ All 97 tests passing
- ⚠️ GitHub issues disabled, no PRs pending
- 📋 Need to reach 100% coverage for production readiness

## Next Steps
1. Add tests for KPOINTS parser explicit/reciprocal/line modes
2. Add tests for server TCP mode and main() function
3. Add tests for POSCAR/INCAR parser edge cases
4. Implement code formatting feature
5. Implement quick fixes feature
6. Update README with usage instructions
7. Create VSCode extension configuration
