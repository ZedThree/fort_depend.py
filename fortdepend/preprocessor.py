import io
import pcpp


class FortranPreprocessor(pcpp.Preprocessor):
    def __init__(self):
        super().__init__()
        self.add_path('.')

    def parse_to_string_lines(self, text, source):
        with io.StringIO() as f:
            self.parse(text, source=source)
            self.write(f)
            f.seek(0)
            result = f.readlines()
        return result
