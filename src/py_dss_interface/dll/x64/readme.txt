The Open Distribution System Simulator, OpenDSS

Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
All rights reserved.

Version 9.5.1.1

Changes this version
====================

- Adds "Element" property to UPFC for triggering the reactive power compensation mode. Not needed in the past, but given the new control mode it requires it. 
- Adds DOC_P1Blocking property to relays. This enables/disables a positive sequence power blocking element according to IEEE Std C57.12.44-2014.
- Solved issue introduced with long-line correction changes of rev 3464. Single-phase lines without symmetrical component definition were being affected by change.
- Adding command for getting the line type list as string for reference.
- Includes new command for checking if there is a new version of OpenDSS available for download. The same command was implemented in the GUI's menu -> Help options.
- Solves an issue found in the LineTypeList container. It was generating an unexpected issue when clearing memory during parallel processing operations. It was also creating redundant constants between Line, LineCode and LineGeometry classes.
- Adding alternative method for accessing PCE state variables through the ActiveCktElement interface.
- First version of DynamicExp object released, still under test. 
- The ActiveCktelement interface includes new properties to allow users get/set state variables by name or index in the active circuit element. 
- Corrects a recent bug introduced while fixing pu values for VSource when working on Dynamics mode.
- Fixes version sync between exe, COM and DLL versions.
- Solves old bug located within the fmonitor variables (proposed by UCF). 
- Fixes DumpProperties procedures for conductor objects by utilizing the proper indexing. 
- Brings back GetPropertyValue functions for ConductorData and child classes that allows properties to be kept in synch. Fixed indexing that was preventing those functions from properly working
- Adds conditional compiles for debugging during Dynamic mode. Update comments in Vsource.
- Fix bug in Dynamics calc where PerUnit was not taken into account.
- Fixes issue when reporting Elements in class.
- Normalizes the progress form.
- Solves an issue detected when presenting the help form, the button at the bottom were missing.
- Solves a bug inserted when updating the sequence impedance entries through COM/DLL.
- COM interface help link added to Help option at the menu bar in EXE.
- UPFC device updated to operate at the control queue increasing the stability of the device when widely installed on a large-scale model.

The Version 8 was the first to be delivered with parallel processing capabilities in both 32-bit (X86) and 
64-bit (X64) versions. Version 9 is the latest update. The files are still listed under the 'Version8' folder on Sourceforge.net.
The OpenDSSInstaller download includes both X86 and X64 versions of Version 9, along 
with optional documentation and examples.  

If you have 64-bit Windows, you may install both the 64-bit and 32-bit 
versions.  The 32-bit version is required if you plan to automate OpenDSS 
from Excel or any other 32-bit program.  The 64-bit version is required to 
automate OpenDSS from 64-bit MatLab or other 64-bit apps on a 64-bit system.  

Installation
============

The installer will give you a choice to install the executables and 
optional files under a target directory of your choice, such as 
c:\opendss.  Files that are specific to the 32-bit version will be written 
to an x86 subdirectory, such as c:\opendss\x86.  Files that are specific 
to the 64-bit version will be written to an x64 subdirectory, such as 
c:\opendss\x64.  The EXE and DLL files should not be moved after 
installation, but may be updated in place with newer versions.
  
On a 64-bit system, you may install and use both the 32-bit and 64-bit 
versions with no conflict between them.  

Short-cuts to the program and manual are created under Start Menu/OpenDSS.
Please see the manual, OpenDSSManual.PDF, for an overview of the program. 
The most up-to-date reference information will always be found through the 
software's "Help / DSS Help" menu command.

If you have an earlier version of OpenDSS installed and registered, such as 7.4.3,
remove it completely. Otherwise, Windows may retain a registry entry to the
old 32-bit COM server when you start it up from a 32-bit program.

COM Automation
==============

The COM Server in OpenDSSEngine.DLL may be automated.  The installer will 
register either or both versions, depending on your selection.  Even 
though the file names and registration commands match, they are in 
separate locations and Windows will activate the correct version required 
by the calling program.  For example, 64-bit MatLab will call the 64-bit 
OpenDSSEngine.DLL and 32-bit Microsoft Excel will call the 32-bit version.  
(Note: The 64-bit version of Excel is rarely installed.) 

Background
==========

The OpenDSS is a simulator specifically designed to represent electric 
power distribution circuits.  OpenDSS is designed to support most types of 
power distribution planning analysis associated with the interconnection 
of distributed generation (DG) to utility systems.  It also supports many 
other types of frequency-domain circuit simulations commonly performed on 
utility electric power distribution systems.  It represents unbalanced 
conditions, stochastic processes, and other aspects of electrical power 
distribution systems and equipment in far greater detail than many other 
tools, including commercial products.  Through COM and scripting 
interfaces, other programs can drive OpenDSS in highly customized 
simulations, Monte Carlo analysis, etc.  Users can define their own models 
through dynamic linking, scripting, or automation.  

Electric Power Research Institute, Inc.  (http://www.epri.com) uses 
OpenDSS in its research and services work, and continues to enhance the 
software.  Earlier proprietary versions were used in dozens of studies for 
electric utility clients, and in a Web-based wind power simulator.  
There were several goals in making OpenDSS an open-source project in 2008: 

1 - Enhance the modeling capabilities available to government 
laboratories, universities, and other researchers engaged in grid 
modernization work.  

2 - Encourage interfaces between OpenDSS and complementary tools, such as 
communication system simulators or model compilers.  

3 - Encourage the adoption of items 1 and 2 into commercial products used 
by electric utilities.  

4 - Encourage collaborative efforts between industry, government, and 
university researchers in power distribution system analysis for grid 
modernization efforts.  

5 - Provide a capable testing platform for data and object modeling 
efforts currently underway in the electric utility industry.

Source Code
===========

The programming language for OpenDSS is Delphi 
(http://www.embarcadero.com), currently version Delphi 10.4 Sydney.  A free community version is available
to certain non-commercial users (see license requirements on Embarcadero site).
There is also a Free Pascal (Lazarus) version of the program.  Some of the supporting modules may 
require a C++ compiler to build from source.  OpenDSS source code is 
available from the following SVN repository: 

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/


Third-party Components
======================

KLUSolve.DLL is open source software, available from
www.sourceforge.net/projects/klusolve

Other convenient Sourceforge.net Links
======================================

OpenDSS Download Files:

http://sourceforge.net/projects/electricdss/files/

Getting Started

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=Getting_Started

Latest Tech Notes in Wiki

Selected Tech Notes are available in the Doc Folder

Questions and Answers

Selected Q&A files are available in the Doc Folder

OpenDSS Forum

http://sourceforge.net/p/electricdss/discussion/861976/

What is Unique About OpenDSS?

OpenDSS was derived from a family of power system harmonics solvers designed for analysis of distributions systems. This analysis requires very detailed models of the circuit topology. This gives the program the capability to represent nearly any circuit topology that might be encountered on a power distribution system. While the power flow solution is the most common application, the program is technically not a power flow program. The solution method and circuit modeling has more in common with harmonics and dynamics solvers.

The program was developed because users realized in 1996 that they were not getting the correct answer for distributed generation problems when using only the typical static power flow analysis used for distribution planning. OpenDSS was one of the first programs to implement an efficient quasi-static time series (QSTS) simulation for DER analysis. The capability was built into the program from the start.

It was recognized that it is not possible to anticipate everything that users will want to do in DER analysis and build a single user interface for this. This issue was addressed by building the program around a scripting interface that is user defined. Also, the COM interface was added so that users could drive the program from other software such as MATLAB and Python. This feature is popular among researchers and graduate students who want to do things not already supported in the program. Of course, EPRI uses this feature extensively in its research. A direct function call DLL interface (OpenDSSDirect.DLL) was also developed to allow this feature to be used on platforms and computer languages that do not support COM.



IEEE Test Cases

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/IEEETestCases/

EPRI Test Circuits
https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/EPRITestCircuits/

Source Code

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Source/

Top level of Distribution area (Releases)

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/

Examples

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/Examples/

License
=======

Use of this software is subject to a license. The terms are in:

 1 - A file called "license.txt" distributed with the software,
 2 - The user manual, and
 3 - The executable program's Help/About dialog box
