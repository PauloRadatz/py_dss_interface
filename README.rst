========
Overview
========

.. start-badges

.. list-table::
    :stub-columns: 1

    * - docs
      - |docs|
    * - tests
      - | |travis| |appveyor| |requires|
        | |codecov|
    * - package
      - | |version| |wheel| |supported-versions| |supported-implementations|
        | |commits-since|
.. |docs| image:: https://readthedocs.org/projects/py_dss_interface/badge/?style=flat
    :target: https://readthedocs.org/projects/py_dss_interface
    :alt: Documentation Status

.. |travis| image:: https://api.travis-ci.org/PauloRadatz/py_dss_interface.svg?branch=master
    :alt: Travis-CI Build Status
    :target: https://travis-ci.org/PauloRadatz/py_dss_interface

.. |appveyor| image:: https://ci.appveyor.com/api/projects/status/github/PauloRadatz/py_dss_interface?branch=master&svg=true
    :alt: AppVeyor Build Status
    :target: https://ci.appveyor.com/project/PauloRadatz/py_dss_interface

.. |requires| image:: https://requires.io/github/PauloRadatz/py_dss_interface/requirements.svg?branch=master
    :alt: Requirements Status
    :target: https://requires.io/github/PauloRadatz/py_dss_interface/requirements/?branch=master

.. |codecov| image:: https://codecov.io/gh/PauloRadatz/py_dss_interface/branch/master/graphs/badge.svg?branch=master
    :alt: Coverage Status
    :target: https://codecov.io/github/PauloRadatz/py_dss_interface

.. |version| image:: https://img.shields.io/pypi/v/py-dss-interface.svg
    :alt: PyPI Package latest release
    :target: https://pypi.org/project/py-dss-interface

.. |wheel| image:: https://img.shields.io/pypi/wheel/py-dss-interface.svg
    :alt: PyPI Wheel
    :target: https://pypi.org/project/py-dss-interface

.. |supported-versions| image:: https://img.shields.io/pypi/pyversions/py-dss-interface.svg
    :alt: Supported versions
    :target: https://pypi.org/project/py-dss-interface

.. |supported-implementations| image:: https://img.shields.io/pypi/implementation/py-dss-interface.svg
    :alt: Supported implementations
    :target: https://pypi.org/project/py-dss-interface

.. |commits-since| image:: https://img.shields.io/github/commits-since/PauloRadatz/py_dss_interface/v0.0.0.svg
    :alt: Commits since latest release
    :target: https://github.com/PauloRadatz/py_dss_interface/compare/v0.0.0...master



.. end-badges

opendsspy is a Windows Python package providing access to OpenDSS direct dll version of OpenDSS.

* Free software: MIT license

Installation
============

::

    pip install py-dss-interface

You can also install the in-development version with::

    pip install https://github.com/PauloRadatz/py_dss_interface/archive/master.zip


Documentation
=============


https://py_dss_interface.readthedocs.io/


Development
===========

To run the all tests run::

    tox

Note, to combine the coverage data from all the tox environments run:

.. list-table::
    :widths: 10 90
    :stub-columns: 1

    - - Windows
      - ::

            set PYTEST_ADDOPTS=--cov-append
            tox

    - - Other
      - ::

            PYTEST_ADDOPTS=--cov-append tox
