
Changelog
=========

2.0.0 (2022-05-24)
------------------
* py-dss-interface version 2 or above does not have backward compatibility with version 1
* Methods replaced by Python property and setter. For example, instead of using dss.lines_read_r1() and dss.lines_write_r1() to read and write, respectively, we can use dss.lines.r1 to either read (variable = dss.lines.r1) or write (dss.lines.r1 = value) in the current version.
* Update OpenDSS version to: OpenDSS Version 9.6.1.1; License Status: Open

1.0.2 (2021-10-10)
------------------
* Code refactored
* PEP 8 in some methods

1.0.1
------------------
* Working only on Windows due EPRI offical OpenDSS is available only on Windows

1.0.0 (2021-01-21)
------------------

* Code refactored
* Works on Linux version of OpenDSS provided in the package. This version is from 2020.
* Tests included
* Methods renamed to satisfy PEP 8 -- Style Guide for Python Code

0.1.0 (2021-01-21)
------------------

* Update OpenDSS version to: OpenDSS Version 9.2.0.1; License Status: Open

0.0.9 (2021-01-21)
------------------

* text method returns string values (Jouni request)
* OpenDSS exe 64bits included

0.0.8 (2020-12-10)
------------------

* Allowing run multiple instances of DDLL without problems with memory - Gustavo asked it
* Allowing run multiple OpenDSS' DLLs
* Update OpenDSS version to: OpenDSS Version 9.1.3.3 (64-bit build); License Status: Open


0.0.7 (2020-10-22)
------------------

* PVsystems updated.
* Update OpenDSS version to: OpenDSS Version 9.1.0.1 (64-bit build); License Status: Open.
* Allowing to write values into Variant methods.


0.0.4 (2020-08-17)
------------------

* DSS class can receive OpenDSS folder in order to use the user own OpenDSS.

0.0.1 (2020-06-12)
------------------

* Integrating CI.


0.0.0 (2020-06-12)
------------------

* First release on PyPI.
