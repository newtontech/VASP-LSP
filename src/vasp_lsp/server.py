"""Main LSP server for VASP input files.

This module implements the Language Server Protocol for VASP files,
providing features like autocomplete, hover documentation, and diagnostics.
"""

import argparse
import logging
from typing import Optional

from lsprotocol.types import (
    TEXT_DOCUMENT_COMPLETION,
    TEXT_DOCUMENT_HOVER,
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_SAVE,
    CompletionOptions,
    CompletionParams,
    HoverParams,
    DidOpenTextDocumentParams,
    DidChangeTextDocumentParams,
    DidSaveTextDocumentParams,
    InitializeParams,
    InitializeResult,
    ServerCapabilities,
    TextDocumentSyncKind,
    TextDocumentSyncOptions,
)
from pygls.server import LanguageServer

from .features.completion import CompletionProvider
from .features.hover import HoverProvider
from .features.diagnostics import DiagnosticsProvider

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class VASPLanguageServer(LanguageServer):
    """VASP Language Server implementation."""
    
    def __init__(self):
        super().__init__(name="vasp-lsp", version="0.1.0")
        self.completion_provider = CompletionProvider()
        self.hover_provider = HoverProvider()
        self.diagnostics_provider = DiagnosticsProvider()
        
        # Document cache
        self.documents: dict = {}
        
    def get_document_content(self, uri: str) -> Optional[str]:
        """Get document content from cache or workspace."""
        return self.documents.get(uri)
        
    def set_document_content(self, uri: str, content: str):
        """Cache document content."""
        self.documents[uri] = content
        

# Create server instance
server = VASPLanguageServer()


@server.feature(InitializeParams)
def initialize(params: InitializeParams) -> InitializeResult:
    """Handle server initialization."""
    logger.info(f"Initializing VASP-LSP v0.1.0")
    logger.info(f"Client: {params.client_info.name if params.client_info else 'Unknown'}")
    
    capabilities = ServerCapabilities(
        text_document_sync=TextDocumentSyncOptions(
            open_close=True,
            change=TextDocumentSyncKind.FULL,
        ),
        completion_provider=CompletionOptions(
            resolve_provider=False,
            trigger_characters=["=", " ", "."],
        ),
        hover_provider=True,
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
        
    return server.completion_provider.get_completions(
        params, content, uri
    )


@server.feature(TEXT_DOCUMENT_HOVER)
def hover(params: HoverParams):
    """Handle hover request."""
    uri = params.text_document.uri
    content = server.get_document_content(uri)
    
    if content is None:
        return None
        
    return server.hover_provider.get_hover(params, content, uri)


def _publish_diagnostics(uri: str, content: str):
    """Publish diagnostics for a document."""
    diagnostics = server.diagnostics_provider.get_diagnostics(content, uri)
    server.publish_diagnostics(uri, diagnostics)


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
        version="%(prog)s 0.1.0",
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
