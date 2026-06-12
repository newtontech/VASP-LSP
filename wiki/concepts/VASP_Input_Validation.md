# VASP_Input_Validation

> Type: concept
> Domain: VASP input preparation
> Sources: `raw/assets/examples__INCAR`, `raw/assets/examples__KPOINTS`, `raw/assets/examples__POSCAR`, `raw/assets/docs__DIAGNOSTIC_ENGINE_V1.md`

## Definition

VASP input validation checks the consistency and syntax of input files such as INCAR, KPOINTS, POSCAR, and related simulation metadata before a calculation is run.

## Agent Use

Agents should treat diagnostics as actionable feedback, apply minimal fixes, and rerun validation before considering an input ready.

## Related Pages

- [[VASP_LSP]]
- [[Agent_Workflow]]
