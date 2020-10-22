========
Overview
========

.. start-badges

.. list-table::
    :stub-columns: 1

    * - docs
      - |docs|
    * - tests
      - | |appveyor| |requires|
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

py-dss-interface is a Windows Python package providing access to OpenDSS direct dll version of OpenDSS - Version 9.1.0.1 (64-bit build); License Status: Open and Version 9.1.0.1 (32-bit build); License Status: Open.

* Free software: MIT license

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

1 - The Read the Docs (the only problem is that it could not generate the doc using the docstring from the DSSDLL class. This class uses ctypes in a way that just works with Windows, but the Read the Docs uses linux to create it - Any help would be great!)

https://py_dss_interface.readthedocs.io/

2 - Well, there is a solution (I hate it, but works), you can download the docs created offline and open that in your browser.

https://github.com/PauloRadatz/py_dss_interface/tree/master/docs/my_doc

3 - Another good resource is the OpenDSS_Direct_DLL.pdf doc created by Davis Montenegro. The package has been done based on this documentation.

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/Doc/OpenDSS_Direct_DLL.pdf


Thanks
=============
Thanks to Celso Rocha for starting the main script with me back in 2016/2017.


