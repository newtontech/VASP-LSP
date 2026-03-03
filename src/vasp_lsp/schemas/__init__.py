"""VASP parameter schemas and metadata."""

from .incar_tags import INCAR_TAG_LIST, INCAR_TAGS, get_tag_info

__all__ = ["INCAR_TAGS", "INCAR_TAG_LIST", "get_tag_info"]
