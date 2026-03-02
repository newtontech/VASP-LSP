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
- [ ] Improve test coverage to 100% (current: 81%)

## Testing
- Run tests: `pytest tests/`
- Check coverage: `pytest --cov`
- Current coverage: 81% (61 tests passing)

## Coverage Analysis (2026-03-02)
Files needing additional tests:
- `src/vasp_lsp/features/completion.py` (51% coverage)
- `src/vasp_lsp/server.py` (47% coverage)
- `src/vasp_lsp/parsers/kpoints_parser.py` (61% coverage)
- `src/vasp_lsp/features/hover.py` (69% coverage)
- `src/vasp_lsp/features/diagnostics.py` (77% coverage)

## Maintenance
- Nightly automated maintenance at random time
- See .maintenance/last-run.md for last check

## Recent Updates (2026-03-02 22:41)
- ✅ All 61 tests passing
- ✅ Test coverage improved to 81%
- ✅ GitHub issues disabled, no PRs pending
- ⏳ Codex development session attempted (network issues encountered)
- 📋 Remaining tasks documented for next session

## Next Steps
1. Add tests for LSP handlers (initialize, did_open, did_change, etc.)
2. Add tests for completion edge cases
3. Add tests for hover edge cases
4. Add tests for kpoints parser error handling
5. Implement code formatting feature
6. Implement quick fixes feature
