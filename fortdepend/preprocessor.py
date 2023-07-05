"""Utilities for preprocessing Fortran"""

import io
import pcpp


class FortranPreprocessor(pcpp.Preprocessor):
    """Simple wrapper around `pcpp.Preprocessor` to write to string"""
    def __init__(self):
        super().__init__()
        self.add_path(".")

    def parse_to_string(self, text: str, source: str) -> str:
        """Preprocess ``text`` straight to string"""
        with io.StringIO() as f:
            self.parse(text, source=source)
            self.write(f)
            return f.getvalue()
