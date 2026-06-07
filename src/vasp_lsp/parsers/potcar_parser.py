"""POTCAR parser for cross-file VASP diagnostics."""

import re
from dataclasses import dataclass
from typing import Any, Dict, List, Optional


@dataclass
class POTCAREntry:
    """One pseudopotential dataset inside a POTCAR file."""

    title: str
    element: str
    enmax: Optional[float] = None
    enmin: Optional[float] = None


@dataclass
class POTCARData:
    """Parsed POTCAR data."""

    entries: List[POTCAREntry]


class POTCARParser:
    """Parse enough POTCAR metadata for static validation."""

    ENMAX_REGEX = re.compile(r"ENMAX\s*=\s*([-+]?\d+(?:\.\d+)?)", re.IGNORECASE)
    ENMIN_REGEX = re.compile(r"ENMIN\s*=\s*([-+]?\d+(?:\.\d+)?)", re.IGNORECASE)

    def __init__(self, content: str):
        """Initialize parser with POTCAR content."""
        self.content = content
        self.lines = content.splitlines()
        self.errors: List[Dict[str, Any]] = []

    def parse(self) -> Optional[POTCARData]:
        """Parse POTCAR dataset titles and cutoff metadata."""
        self.errors = []
        entries: List[POTCAREntry] = []
        current: Optional[POTCAREntry] = None

        for line_num, line in enumerate(self.lines, start=1):
            stripped = line.strip()
            if self._looks_like_title(stripped):
                if current:
                    entries.append(current)
                current = POTCAREntry(title=stripped, element=self._extract_element(stripped))
                continue

            if current:
                enmax_match = self.ENMAX_REGEX.search(line)
                if enmax_match:
                    current.enmax = float(enmax_match.group(1))

                enmin_match = self.ENMIN_REGEX.search(line)
                if enmin_match:
                    current.enmin = float(enmin_match.group(1))

        if current:
            entries.append(current)

        if not entries and self.content.strip():
            self.errors.append(
                {
                    "message": "No POTCAR datasets found",
                    "line": 1,
                    "severity": "error",
                }
            )
            return None

        return POTCARData(entries=entries)

    def _looks_like_title(self, line: str) -> bool:
        if not line:
            return False
        upper = line.upper()
        return upper.startswith(("PAW", "US", "LDA", "PBE")) and "ENMAX" not in upper

    def _extract_element(self, title: str) -> str:
        parts = title.split()
        if len(parts) >= 2:
            token = parts[1]
        elif parts:
            token = parts[0]
        else:
            return "Unknown"

        token = token.split("_")[0]
        match = re.match(r"([A-Z][a-z]?)", token)
        return match.group(1) if match else token

    def get_errors(self) -> List[Dict[str, Any]]:
        """Get parser errors."""
        return self.errors
