.. fortdepend documentation master file, created by
   sphinx-quickstart on Fri Nov  2 17:30:28 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

fortdepend: A Fortran Dependency Generator
==========================================

:py:mod:`fortdepend` is a python package to automatically generate
Fortran dependencies.

Given a set of files, :py:mod:`fortdepend` automatically constructs
the dependency graph for the programs and files and can write a
dependency file suitable for Makefiles. :py:mod:`fortdepend` now uses
pcpp_, a preprocessor written in Python, so it can determine which
modules will actually be used when you compile.

You can even use :py:mod:`fortdepend` to draw the graph of the module
dependencies (requires graphviz_)!

Original script by D. Dickinson

.. toctree::
   :maxdepth: 1
   :caption: Contents:

   basic_usage
   advanced_usage
   fortdepend

Limitations
-----------

- :py:mod:`fortdepend` requires Python 3.

- :py:mod:`fortdepend` works by looking for matching pairs of
  ``program <name>/end program <name>`` and ``module <name>/end module
  <name>``, and so will not work on Fortran 77-style files that just
  use ``end`` without the appropriate label.

Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


.. _pcpp: https://github.com/ned14/pcpp
.. _graphviz: https://github.com/xflr6/graphviz
