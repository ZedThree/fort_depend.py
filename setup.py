from setuptools import setup

setup(name='fortdepend',
      version='0.1.0',
      description='Automatically generate Fortran dependencies',
      author='Peter Hill',
      author_email='peter@fusionplasma.co.uk',
      url='https://github.com/ZedThree/fort_depend.py/',
      download_url='https://github.com/ZedThree/fort_depend.py/tarball/0.1.0',
      license='MIT',
      classifiers=[
          'Development Status :: 3 - Alpha',
          'License :: OSI Approved :: MIT License',
          'Programming Language :: Python :: 2',
          'Programming Language :: Python :: 2.7',
          'Programming Language :: Python :: 3',
          'Programming Language :: Python :: 3.3',
          'Programming Language :: Python :: 3.4',
          'Programming Language :: Python :: 3.5',
          'Programming Language :: Python :: 3.6',
          'Programming Language :: Fortran',
      ],
      packages=['fortdepend'],
      install_requires=['colorama', 'pcpp'],
      keywords=['build', 'dependencies', 'fortran'],
      entry_points={
          'console_scripts': [
              'fortdepend = fortdepend.__main__:main',
          ],
      },
)
