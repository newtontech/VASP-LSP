# VASP-LSP LLM Wiki Plan

> Project: VASP-LSP
> Created: 2026-06-12
> Status: initialized

## Objective

Maintain a Karpathy-style LLM Wiki for VASP input validation, agent-facing diagnostics, and editor integration.

## Structure

```text
raw/assets/              # source evidence copied from repository docs, examples, and metadata
wiki/entities/           # concrete VASP and LSP entities
wiki/concepts/           # reusable validation and simulation concepts
wiki/synthesis/          # agent workflows and API summaries
index.md                 # navigation hub
log.md                   # change log
```

## Initial Sources

- `raw/assets/README.md`
- `raw/assets/pyproject.toml`
- `raw/assets/docs__DIAGNOSTIC_ENGINE_V1.md`
- `raw/assets/examples__INCAR`
- `raw/assets/examples__KPOINTS`
- `raw/assets/examples__POSCAR`
