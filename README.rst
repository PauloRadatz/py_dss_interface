========
Overview
========

.. start-badges

.. list-table::
    :stub-columns: 1

    * - docs
      - |docs|
    * - tests
      - | |appveyor|
        | |codecov|
    * - package
      - | |version| |wheel| |supported-versions| |supported-implementations|
        | |commits-since|
.. |docs| image:: https://readthedocs.org/projects/py_dss_interface/badge/?style=flat
    :target: https://readthedocs.org/projects/py_dss_interface
    :alt: Documentation Status

.. |appveyor| image:: https://ci.appveyor.com/api/projects/status/github/PauloRadatz/py_dss_interface?branch=master&svg=true
    :alt: AppVeyor Build Status
    :target: https://ci.appveyor.com/project/PauloRadatz/py_dss_interface

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

The py-dss-interface is a Python package that provides a Python interface to the official version of OpenDSS (Open Distribution System Simulator) software. OpenDSS is a free, open-source software for simulating and analyzing power distribution systems.

* Free software: MIT license

The current py-dss-interface works only with Windows. The reason is that EPRI provides only Windows versions of OpenDSS. The package will work for Linux when EPRI releases the Linux version of OpenDSS.

The py-dss-interface package allows users to interact with OpenDSS using Python code, which can be particularly useful for automating tasks, performing simulations, and analyzing results. The package provides a range of functionality, including:

* Creating and modifying OpenDSS circuit models

* Running simulations and analyzing results

* Accessing and manipulating data within the circuit model

* Plotting results

The package is available on the Python Package Index (PyPI) and can be installed using pip, the Python package installer. OpenDSS does not have to be installed on the user's system to use the package, as the py-dss-interface provides an OpenDSS version.

Overall, the py-dss-interface is a powerful tool for anyone working with power distribution systems who wants to use Python for simulation and analysis.


Disclaimer
============
This Python Package is purely responsibility of Paulo Radatz and not his employer. Use this package at your own risk.

Installation
============

::

    pip install py-dss-interface

Documentation
=============
You can access the documentation through:

1 - The Read the Docs.

https://py-dss-interface.readthedocs.io/en/latest/


Thanks
=============
I want to thank Ênio Viana and Rodolfo Pilar Londero for all their contribution to the new version of the tool.


