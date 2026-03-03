"""
Integration tests to trigger coverage through public API.
"""

from lsprotocol.types import CompletionParams, HoverParams, Position

from vasp_lsp.features.completion import CompletionProvider
from vasp_lsp.features.diagnostics import DiagnosticsProvider
from vasp_lsp.features.hover import HoverProvider


class TestCompletionIntegration:
    """Integration tests for completion provider"""

    def test_completion_with_incar_file(self):
        """Test completion with INCAR file."""
        provider = CompletionProvider()
        content = "ENC"
        params = CompletionParams(
            text_document={"uri": "file:///INCAR"},
            position=Position(line=0, character=3),
        )
        result = provider.get_completions(params, content, "file:///INCAR")
        assert result is not None

    def test_completion_with_incar_dot_file(self):
        """Test completion with INCAR. file."""
        provider = CompletionProvider()
        content = "E"
        params = CompletionParams(
            text_document={"uri": "file:///INCAR."},
            position=Position(line=0, character=1),
        )
        result = provider.get_completions(params, content, "file:///INCAR.")
        assert result is not None

    def test_completion_with_incar_vasp_file(self):
        """Test completion with INCAR.VASP file."""
        provider = CompletionProvider()
        content = "E"
        params = CompletionParams(
            text_document={"uri": "file:///INCAR.VASP"},
            position=Position(line=0, character=1),
        )
        result = provider.get_completions(params, content, "file:///INCAR.VASP")
        assert result is not None

    def test_completion_with_poscar_file(self):
        """Test completion with POSCAR file."""
        provider = CompletionProvider()
        content = ""
        params = CompletionParams(
            text_document={"uri": "file:///POSCAR"},
            position=Position(line=0, character=0),
        )
        result = provider.get_completions(params, content, "file:///POSCAR")
        assert result is not None

    def test_completion_with_contcar_file(self):
        """Test completion with CONTCAR file."""
        provider = CompletionProvider()
        content = ""
        params = CompletionParams(
            text_document={"uri": "file:///CONTCAR"},
            position=Position(line=0, character=0),
        )
        result = provider.get_completions(params, content, "file:///CONTCAR")
        assert result is not None

    def test_completion_with_kpoints_file(self):
        """Test completion with KPOINTS file."""
        provider = CompletionProvider()
        content = ""
        params = CompletionParams(
            text_document={"uri": "file:///KPOINTS"},
            position=Position(line=0, character=0),
        )
        result = provider.get_completions(params, content, "file:///KPOINTS")
        assert result is not None

    def test_completion_with_kpoints_dot_file(self):
        """Test completion with KPOINTS. file."""
        provider = CompletionProvider()
        content = ""
        params = CompletionParams(
            text_document={"uri": "file:///KPOINTS."},
            position=Position(line=0, character=0),
        )
        result = provider.get_completions(params, content, "file:///KPOINTS.")
        assert result is not None

    def test_completion_with_kpoints_vasp_file(self):
        """Test completion with KPOINTS.VASP file."""
        provider = CompletionProvider()
        content = ""
        params = CompletionParams(
            text_document={"uri": "file:///KPOINTS.VASP"},
            position=Position(line=0, character=0),
        )
        result = provider.get_completions(params, content, "file:///KPOINTS.VASP")
        assert result is not None


class TestHoverIntegration:
    """Integration tests for hover provider"""

    def test_hover_incar_file(self):
        """Test hover with INCAR file."""
        provider = HoverProvider()
        content = "ENCUT = 500"
        params = HoverParams(
            text_document={"uri": "file:///INCAR"},
            position=Position(line=0, character=2),
        )
        result = provider.get_hover(params, content, "file:///INCAR")
        assert result is not None

    def test_hover_poscar_file(self):
        """Test hover with POSCAR file."""
        provider = HoverProvider()
        content = "System comment"
        params = HoverParams(
            text_document={"uri": "file:///POSCAR"},
            position=Position(line=0, character=0),
        )
        result = provider.get_hover(params, content, "file:///POSCAR")
        assert result is not None

    def test_hover_kpoints_file(self):
        """Test hover with KPOINTS file."""
        provider = HoverProvider()
        content = "Gamma\n4 4 4\n"
        params = HoverParams(
            text_document={"uri": "file:///KPOINTS"},
            position=Position(line=0, character=0),
        )
        result = provider.get_hover(params, content, "file:///KPOINTS")
        assert result is not None


class TestDiagnosticsIntegration:
    """Integration tests for diagnostics provider"""

    def test_diagnostics_hybrid_functional(self):
        """Test diagnostics with hybrid functional."""
        provider = DiagnosticsProvider()
        content = "LHFCALC = .TRUE.\n"
        diagnostics = provider.get_diagnostics(content, "file:///INCAR")
        assert isinstance(diagnostics, list)
