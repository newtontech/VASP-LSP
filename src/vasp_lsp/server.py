"""Main LSP server for VASP input files.

This module implements the Language Server Protocol for VASP files,
providing features like autocomplete, hover documentation, and diagnostics.
"""

import argparse
import json
import logging
import re
from typing import Dict, List, Optional

from lsprotocol.types import (
    TEXT_DOCUMENT_CODE_ACTION,
    TEXT_DOCUMENT_COMPLETION,
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_DID_SAVE,
    TEXT_DOCUMENT_DOCUMENT_SYMBOL,
    TEXT_DOCUMENT_FORMATTING,
    TEXT_DOCUMENT_HOVER,
    TEXT_DOCUMENT_RENAME,
    CodeActionOptions,
    CodeActionParams,
    CompletionOptions,
    CompletionParams,
    Diagnostic,
    DidChangeTextDocumentParams,
    DidOpenTextDocumentParams,
    DidSaveTextDocumentParams,
    DocumentFormattingParams,
    DocumentSymbolParams,
    ExecuteCommandParams,
    HoverParams,
    InitializeParams,
    InitializeResult,
    Position,
    Range,
    RenameParams,
    ServerCapabilities,
    TextDocumentSyncKind,
    TextDocumentSyncOptions,
    TextEdit,
    WorkspaceEdit,
)
from pygls.server import LanguageServer

from . import __version__
from .features.completion import CompletionProvider
from .features.diagnostics import DiagnosticsProvider
from .features.formatting import FormattingProvider
from .features.hover import HoverProvider
from .features.navigation import DocumentSymbolsProvider
from .features.quickfixes import QuickFixesProvider

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class VASPLanguageServer(LanguageServer):
    """VASP Language Server implementation."""

    def __init__(self):
        super().__init__(name="vasp-lsp", version=__version__)
        self.completion_provider = CompletionProvider()
        self.hover_provider = HoverProvider()
        self.diagnostics_provider = DiagnosticsProvider()
        self.formatting_provider = FormattingProvider()
        self.quickfixes_provider = QuickFixesProvider()
        self.navigation_provider = DocumentSymbolsProvider()

        # Document cache
        self.documents: Dict[str, str] = {}
        self.document_diagnostics: Dict[str, List[Diagnostic]] = {}

    def get_document_content(self, uri: str) -> Optional[str]:
        """Get document content from cache or workspace."""
        return self.documents.get(uri)

    def set_document_content(self, uri: str, content: str) -> None:
        """Cache document content."""
        self.documents[uri] = content

    def set_document_diagnostics(self, uri: str, diagnostics: List[Diagnostic]) -> None:
        """Cache document diagnostics for code actions."""
        self.document_diagnostics[uri] = diagnostics

    def get_document_diagnostics(self, uri: str) -> List[Diagnostic]:
        """Get cached document diagnostics."""
        return self.document_diagnostics.get(uri, [])


# Create server instance
server = VASPLanguageServer()


@server.feature("initialize")
def initialize(params: InitializeParams) -> InitializeResult:
    """Handle server initialization."""
    logger.info("Initializing VASP-LSP v%s", __version__)
    logger.info(f"Client: {params.client_info.name if params.client_info else 'Unknown'}")

    capabilities = ServerCapabilities(
        text_document_sync=TextDocumentSyncOptions(
            open_close=True,
            change=TextDocumentSyncKind.Full,
        ),
        completion_provider=CompletionOptions(
            resolve_provider=False,
            trigger_characters=["=", " ", "."],
        ),
        hover_provider=True,
        document_formatting_provider=True,
        document_symbol_provider=True,
        rename_provider=True,
        code_action_provider=CodeActionOptions(
            code_action_kinds=[
                "quickfix",
                "source",
            ]
        ),
        execute_command_provider={
            "commands": ["vasp-lsp.diagnosticSnapshot"],
        },
    )

    return InitializeResult(capabilities=capabilities)


@server.feature(TEXT_DOCUMENT_DID_OPEN)
def text_document_did_open(params: DidOpenTextDocumentParams):
    """Handle document open."""
    uri = params.text_document.uri
    content = params.text_document.text
    server.set_document_content(uri, content)

    # Publish diagnostics
    _publish_diagnostics(uri, content)


@server.feature(TEXT_DOCUMENT_DID_CHANGE)
def text_document_did_change(params: DidChangeTextDocumentParams):
    """Handle document change."""
    uri = params.text_document.uri
    # Get the latest content
    if params.content_changes:
        content = params.content_changes[0].text
        server.set_document_content(uri, content)
        _publish_diagnostics(uri, content)


@server.feature(TEXT_DOCUMENT_DID_SAVE)
def text_document_did_save(params: DidSaveTextDocumentParams):
    """Handle document save."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)
    if content:
        _publish_diagnostics(uri, content)


@server.feature(TEXT_DOCUMENT_COMPLETION)
def completions(params: CompletionParams):
    """Handle completion request."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)

    if content is None:
        return None

    return server.completion_provider.get_completions(params, content, uri)


@server.feature(TEXT_DOCUMENT_HOVER)
def hover(params: HoverParams):
    """Handle hover request."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)

    if content is None:
        return None

    return server.hover_provider.get_hover(params, content, uri)


@server.feature(TEXT_DOCUMENT_FORMATTING)
def formatting(params: DocumentFormattingParams):
    """Handle document formatting request."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)

    if content is None:
        return None

    options = {
        "tabSize": params.options.tab_size,
        "insertSpaces": params.options.insert_spaces,
    }

    return server.formatting_provider.format_document(content, uri, options)


@server.feature(TEXT_DOCUMENT_CODE_ACTION)
def code_action(params: CodeActionParams):
    """Handle code action request."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)

    if content is None:
        return None

    # Get diagnostics for this document
    diagnostics = server.get_document_diagnostics(uri)

    # Filter diagnostics to those in the requested range
    range = params.range

    return server.quickfixes_provider.get_code_actions(content, uri, diagnostics, range)


@server.feature(TEXT_DOCUMENT_DOCUMENT_SYMBOL)
def document_symbol(params: DocumentSymbolParams):
    """Handle document symbol request."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)

    if content is None:
        return []

    return server.navigation_provider.get_symbols(content, uri)


@server.feature(TEXT_DOCUMENT_RENAME)
def rename(params: RenameParams):
    """Handle rename request for INCAR parameters."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)

    if content is None:
        return None

    file_type = _get_file_type(uri)
    if file_type != "INCAR":
        return None

    new_name = params.new_name.upper()
    position = params.position
    lines = content.split("\n")

    if position.line >= len(lines):
        return None

    line = lines[position.line]

    # Find the tag name under cursor
    match = re.match(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=", line)
    if not match:
        return None

    old_name = match.group(1)

    # Check that new_name is a valid INCAR tag or at least a valid identifier
    if not re.match(r"^[A-Za-z_][A-Za-z0-9_]*$", new_name):
        return None

    # Rename all occurrences of the tag in the document
    edits = []
    for line_idx, doc_line in enumerate(lines):
        tag_match = re.match(r"^(\s*)([A-Za-z_][A-Za-z0-9_]*)\s*=", doc_line)
        if tag_match and tag_match.group(2).upper() == old_name.upper():
            start_char = tag_match.start(2)
            end_char = tag_match.end(2)

            edits.append(
                TextEdit(
                    range=Range(
                        start=Position(line=line_idx, character=start_char),
                        end=Position(line=line_idx, character=end_char),
                    ),
                    new_text=new_name,
                )
            )

    if not edits:
        return None

    return WorkspaceEdit(changes={uri: edits})


# ---------------------------------------------------------------------------
# Execute command: diagnostic snapshot for agent feedback loops (#18)
# ---------------------------------------------------------------------------


@server.feature("workspace/executeCommand")
def execute_command(params: ExecuteCommandParams):
    """Handle execute command requests.

    Supports:
      - ``vasp-lsp.diagnosticSnapshot``: returns a JSON string with a structured
        diagnostic snapshot for the requested document URI.
    """
    command = params.command
    arguments = params.arguments or []

    if command == "vasp-lsp.diagnosticSnapshot":
        if not arguments:
            return None
        uri = arguments[0]
        content = server.get_document_content(uri)
        if content is None:
            return None
        snapshot = server.diagnostics_provider.get_diagnostics_snapshot(
            content, uri, server.documents
        )
        return json.dumps(snapshot, default=str)

    return None


def _publish_diagnostics(uri: str, content: str):
    """Publish diagnostics for a document."""
    diagnostics = server.diagnostics_provider.get_diagnostics(content, uri, server.documents)
    server.set_document_diagnostics(uri, diagnostics)
    server.publish_diagnostics(uri, diagnostics)


def _get_file_type(uri: str) -> str:
    """Determine file type from URI."""
    filename = uri.split("/")[-1].upper()
    if "INCAR" in filename:
        return "INCAR"
    if "POSCAR" in filename or "CONTCAR" in filename:
        return "POSCAR"
    if "KPOINTS" in filename:
        return "KPOINTS"
    return "UNKNOWN"


def main():
    """Main entry point for the VASP-LSP server."""
    parser = argparse.ArgumentParser(description="VASP Language Server")
    parser.add_argument(
        "--stdio",
        action="store_true",
        help="Use stdio for communication (default for LSP)",
    )
    parser.add_argument(
        "--tcp",
        action="store_true",
        help="Use TCP for communication",
    )
    parser.add_argument(
        "--host",
        default="127.0.0.1",
        help="Host address for TCP mode (default: 127.0.0.1)",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=2087,
        help="Port for TCP mode (default: 2087)",
    )
    parser.add_argument(
        "--version",
        action="version",
        version=f"%(prog)s {__version__}",
    )

    args = parser.parse_args()

    if args.tcp:
        logger.info(f"Starting VASP-LSP server on {args.host}:{args.port}")
        server.start_tcp(args.host, args.port)
    else:
        logger.info("Starting VASP-LSP server on stdio")
        server.start_io()


if __name__ == "__main__":
    main()
