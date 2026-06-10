"""LSP integration tests for document symbols and rename features.

These tests exercise the full LSP server integration for the newly added
navigation features: document symbols and rename.
"""

from typing import Dict, List, Optional

import pytest
from lsprotocol.types import (
    Diagnostic,
    DocumentSymbolParams,
    Position,
    RenameParams,
    TextDocumentIdentifier,
)

from vasp_lsp.server import (
    VASPLanguageServer,
    document_symbol,
    rename,
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

INCAR_URI = "file:///workspace/INCAR"
POSCAR_URI = "file:///workspace/POSCAR"
KPOINTS_URI = "file:///workspace/KPOINTS"


class _CaptureServer(VASPLanguageServer):
    """Capture publish_diagnostics calls."""

    def __init__(self):
        super().__init__()
        self.published_diagnostics: Dict[str, List[Diagnostic]] = {}

    def publish_diagnostics(
        self, uri: str, diagnostics: Optional[List[Diagnostic]] = None, **kwargs: object
    ) -> None:
        self.published_diagnostics[uri] = diagnostics or []


@pytest.fixture()
def server():
    srv = _CaptureServer()
    return srv


@pytest.fixture(autouse=True)
def _patch_global(server, monkeypatch):
    import vasp_lsp.server as server_mod

    monkeypatch.setattr(server_mod, "server", server)


# ---------------------------------------------------------------------------
# Document Symbol Tests
# ---------------------------------------------------------------------------


class TestDocumentSymbolLSP:
    """Test the document_symbol handler through the LSP layer."""

    def test_incar_symbols(self, server):
        content = "ENCUT = 520\nISMEAR = 0\nSIGMA = 0.05\n"
        server.set_document_content(INCAR_URI, content)

        params = DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=INCAR_URI),
        )
        result = document_symbol(params)
        assert result is not None
        assert len(result) == 3
        names = [s.name for s in result]
        assert "ENCUT" in names
        assert "ISMEAR" in names
        assert "SIGMA" in names

    def test_poscar_symbols(self, server):
        content = (
            "Si bulk\n"
            "1.0\n"
            "5.43 0.0 0.0\n"
            "0.0 5.43 0.0\n"
            "0.0 0.0 5.43\n"
            "Si\n"
            "2\n"
            "Direct\n"
            "0.0 0.0 0.0\n"
            "0.25 0.25 0.25\n"
        )
        server.set_document_content(POSCAR_URI, content)

        params = DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=POSCAR_URI),
        )
        result = document_symbol(params)
        assert result is not None
        names = [s.name for s in result]
        assert "System Comment" in names
        assert "Scale Factor" in names

    def test_kpoints_symbols(self, server):
        content = "Gamma grid\n0\nGamma\n4 4 4\n0 0 0\n"
        server.set_document_content(KPOINTS_URI, content)

        params = DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=KPOINTS_URI),
        )
        result = document_symbol(params)
        assert result is not None
        assert len(result) >= 2

    def test_no_document_returns_empty(self, server):
        params = DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri="file:///workspace/INCAR"),
        )
        result = document_symbol(params)
        assert result == []

    def test_unknown_file_type(self, server):
        server.set_document_content("file:///workspace/unknown.txt", "content")
        params = DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri="file:///workspace/unknown.txt"),
        )
        result = document_symbol(params)
        assert result == []


# ---------------------------------------------------------------------------
# Rename Tests
# ---------------------------------------------------------------------------


class TestRenameLSP:
    """Test the rename handler through the LSP layer."""

    def test_rename_incar_tag(self, server):
        content = "ENCUT = 520\n"
        server.set_document_content(INCAR_URI, content)

        params = RenameParams(
            text_document=TextDocumentIdentifier(uri=INCAR_URI),
            position=Position(line=0, character=2),
            new_name="ENCUT_NEW",
        )
        result = rename(params)
        assert result is not None
        assert INCAR_URI in result.changes
        edits = result.changes[INCAR_URI]
        assert len(edits) == 1
        assert edits[0].new_text == "ENCUT_NEW"

    def test_rename_multiple_occurrences(self, server):
        content = "ENCUT = 520\nENCUT = 400\n"
        server.set_document_content(INCAR_URI, content)

        params = RenameParams(
            text_document=TextDocumentIdentifier(uri=INCAR_URI),
            position=Position(line=0, character=0),
            new_name="PLANEWAVE",
        )
        result = rename(params)
        assert result is not None
        edits = result.changes[INCAR_URI]
        assert len(edits) == 2

    def test_rename_not_on_tag_returns_none(self, server):
        content = "# comment\n"
        server.set_document_content(INCAR_URI, content)

        params = RenameParams(
            text_document=TextDocumentIdentifier(uri=INCAR_URI),
            position=Position(line=0, character=0),
            new_name="NEWNAME",
        )
        result = rename(params)
        assert result is None

    def test_rename_no_document_returns_none(self, server):
        params = RenameParams(
            text_document=TextDocumentIdentifier(uri=INCAR_URI),
            position=Position(line=0, character=0),
            new_name="NEWNAME",
        )
        result = rename(params)
        assert result is None

    def test_rename_non_incar_returns_none(self, server):
        content = "Si bulk\n1.0\n"
        server.set_document_content(POSCAR_URI, content)

        params = RenameParams(
            text_document=TextDocumentIdentifier(uri=POSCAR_URI),
            position=Position(line=0, character=0),
            new_name="NEWNAME",
        )
        result = rename(params)
        assert result is None

    def test_rename_invalid_identifier_returns_none(self, server):
        content = "ENCUT = 520\n"
        server.set_document_content(INCAR_URI, content)

        params = RenameParams(
            text_document=TextDocumentIdentifier(uri=INCAR_URI),
            position=Position(line=0, character=0),
            new_name="123invalid",
        )
        result = rename(params)
        assert result is None

    def test_rename_position_beyond_lines(self, server):
        content = "ENCUT = 520\n"
        server.set_document_content(INCAR_URI, content)

        params = RenameParams(
            text_document=TextDocumentIdentifier(uri=INCAR_URI),
            position=Position(line=999, character=0),
            new_name="NEWTAG",
        )
        result = rename(params)
        assert result is None
