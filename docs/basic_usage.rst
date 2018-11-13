Installation
============

You can install fortdepend with pip:

.. code-block:: bash

    pip3 install --user fortdepend

Basic usage
===========

The quickest way to run :py:mod:`fortdepend` is like so:

.. code-block:: bash

    fortdepend -o Makefile.dep

This will look for all files ending in ``.f90`` or ``.F90`` in the
current directory and write the output to ``Makefile.dep``. The output
will something like this:

.. code-block:: make

    test :  \
            moduleA.o \
            moduleB.o \
            moduleC.o \
            moduleD.o \
            programTest.o

    moduleA.o :

    moduleB.o :  \
            moduleA.o

    moduleC.o :  \
            moduleA.o \
            moduleB.o

    moduleD.o :  \
            moduleC.o

You could then get a basic makefile working by putting the following
in ``Makefile``:

.. code-block:: make

    .f90.o:
        gfortran -c $<

    test:
        gfortran $^ -o $@

    include Makefile.dep

And ``make test`` will magically build everything in the correct order!
