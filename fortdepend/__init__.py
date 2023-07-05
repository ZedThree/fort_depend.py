try:
    from importlib.metadata import PackageNotFoundError, version
except ModuleNotFoundError:
    from importlib_metadata import PackageNotFoundError, version
try:
    __version__ = version(__name__)
except PackageNotFoundError:
    from setuptools_scm import get_version

    __version__ = get_version(root="..", relative_to=__file__)

from .fort_depend import FortranProject

__all__ = ["fort_depend", "units", "__version__", "FortranProject"]
