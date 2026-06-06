"""
Tests that release metadata and advertised server versions stay in sync.
"""

import json
import re
from pathlib import Path

from vasp_lsp import __version__
from vasp_lsp.server import VASPLanguageServer

ROOT = Path(__file__).resolve().parents[1]


def test_server_advertises_package_version():
    """The LSP server should advertise the package version to clients."""
    test_server = VASPLanguageServer()

    assert test_server.version == __version__
    assert test_server.lsp.server_info.version == __version__


def test_project_metadata_versions_match_package_version():
    """Keep Python package and VSCode extension versions aligned."""
    pyproject_text = (ROOT / "pyproject.toml").read_text(encoding="utf-8")
    package_json = json.loads((ROOT / "editors/vscode/package.json").read_text(encoding="utf-8"))

    pyproject_version = re.search(r'^version = "([^"]+)"$', pyproject_text, re.MULTILINE)

    assert pyproject_version is not None
    assert pyproject_version.group(1) == __version__
    assert package_json["version"] == __version__
