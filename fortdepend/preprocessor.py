import io
import pcpp


class FortranPreprocessor(pcpp.Preprocessor):
    def __init__(self):
        super(pcpp.Preprocessor, self).__init__()

    def parse_to_string_lines(self, text):
        with io.StringIO() as f:
            self.parse(text)
            self.write(f)
            f.seek(0)
            result = f.readlines()
        return result
