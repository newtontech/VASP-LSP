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
- [x] Improve test coverage to 99%
- [x] Code formatting
- [x] Quick fixes

## Testing
- Run tests: \`pytest tests/\`
- Check coverage: \`pytest --cov\`
- Current status: 428 tests, 100% coverage (2026-03-04)

## Coverage Analysis (2026-03-04)
All modules at 100%:
- \`src/vasp_lsp/__init__.py\`: 100%
- \`src/vasp_lsp/features/__init__.py\`: 100%
- \`src/vasp_lsp/features/completion.py\`: 100%
- \`src/vasp_lsp/features/diagnostics.py\`: 100%
- \`src/vasp_lsp/features/formatting.py\`: 100%
- \`src/vasp_lsp/features/hover.py\`: 100%
- \`src/vasp_lsp/features/quickfixes.py\`: 100%
- \`src/vasp_lsp/parsers/__init__.py\`: 100%
- \`src/vasp_lsp/parsers/incar_parser.py\`: 100%
- \`src/vasp_lsp/parsers/kpoints_parser.py\`: 100%
- \`src/vasp_lsp/parsers/poscar_parser.py\`: 100%
- \`src/vasp_lsp/schemas/__init__.py\`: 100%
- \`src/vasp_lsp/schemas/incar_tags.py\`: 100%
- \`src/vasp_lsp/server.py\`: 100%

## Maintenance
- Nightly automated maintenance at random time
- See .maintenance/last-run.md for last check

## Recent Updates (2026-03-04)
- ✅ Fixed all Ruff lint errors (W293, F841, F601)
- ✅ 416 tests passing with 100% coverage maintained
- ✅ All Ruff checks now pass
- ✅ Updated pyproject.toml with proper lint configuration
- ✅ All changes committed and pushed to GitHub

## Code Quality Improvements (2026-03-04)
- Fixed W293: Removed whitespace from blank lines in docstrings
- Fixed F841: Removed unused variables in formatting.py and kpoints_parser.py
- Fixed F601: Removed duplicate 'ENCUTT' key in quickfixes.py
- Configured pyproject.toml to ignore E501 for long description strings
- All code now passes Ruff linting with zero warnings

## Code Quality Improvements (2026-03-04 - Cron Session)
- Fixed 461 Ruff lint errors across 40 files
- Fixed unused imports (F401) in all test files
- Fixed import order (I001) across all test files
- Fixed whitespace issues (W293) in test files
- Fixed unused variables (F841) in test files
- Fixed ambiguous variable names (E741) in test files
- Applied Ruff formatting to all source and test files
- 416 tests passing with 100% coverage maintained
- All Ruff checks now pass with zero warnings

## Next Steps
1. ✅ Create VSCode extension configuration (Completed 2026-03-04)
2. Add integration tests with real LSP clients
3. Add more POSCAR/KPOINTS diagnostics and quick fixes
4. Performance optimization and benchmarking

## Version History
- v0.1.0: Initial release with core features
- v0.2.0: Added formatting support
- v0.4.0: Achieved 100% test coverage, code cleanup
- v0.4.1: Fixed all Ruff lint errors, improved code quality
- v0.4.2: Added POSCAR and KPOINTS diagnostics (current)

## Version 0.4.2 (2026-03-04)

### New Features
- ✅ Implemented POSCAR file diagnostics
  - Parse error reporting
  - Negative scale factor detection
  - Zero atoms validation
  - Out-of-range coordinate warnings for Direct coordinates
- ✅ Implemented KPOINTS file diagnostics
  - Parse error reporting
  - Non-positive grid value detection
  - Sparse grid warnings
  - K-point weights sum validation

### Tests
- Added 12 new tests for POSCAR/KPOINTS diagnostics
- Total tests: 428 (up from 416)
- Coverage maintained at 100%

## VSCode Extension (2026-03-04)
- ✅ Created VSCode extension configuration in editors/vscode/
- ✅ Added syntax highlighting for INCAR, POSCAR, KPOINTS
- ✅ Added snippets for common INCAR templates
- ✅ Extension manifest with proper language definitions


## Version 0.4.3 (2026-03-04) - Cron Session

### New Features
- ✅ Implemented POSCAR file quick fixes
  - Fix negative scale factor (convert to positive)
  - Wrap out-of-range direct coordinates to [0, 1] range
  - Handle selective dynamics flags in coordinate fixes
- ✅ Implemented KPOINTS file quick fixes
  - Fix non-positive grid values (set to 1)
  - Normalize k-point weights to sum to 1.0

### Tests
- Added 18 new tests for POSCAR/KPOINTS quick fixes
- Total tests: 446 (up from 428)
- Coverage: 99% (1175/1178 statements covered)

### Code Changes
- Updated src/vasp_lsp/features/quickfixes.py
  - Implemented _get_poscar_code_actions method
  - Implemented _get_kpoints_code_actions method
  - Added _create_fix_negative_scale_action helper
  - Added _create_wrap_coordinates_action helper
  - Added _create_fix_grid_action helper
  - Added _create_normalize_weights_action helper
