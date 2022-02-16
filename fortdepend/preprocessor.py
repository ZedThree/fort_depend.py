import io
import pcpp


class FortranPreprocessor(pcpp.Preprocessor):
    def __init__(self):
        super().__init__()
        self.add_path(".")

    def parse_to_string(self, text, source):
        with io.StringIO() as f:
            self.parse(text, source=source)
            self.write(f)
            return f.getvalue()
