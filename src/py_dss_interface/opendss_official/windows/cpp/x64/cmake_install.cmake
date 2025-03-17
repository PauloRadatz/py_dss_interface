# Install script for directory: C:/OpenDSS_rep/VersionC

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "C:/OpenDSS_rep/VersionC/out/install/windows-default")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/openDSSC/bin/objects-Debug/OpenDSSC_Common" TYPE FILE FILES
    "Support/d2c_system.cpp.obj"
    "Support/d2c_syscurr.cpp.obj"
    "Support/d2c_sysstring.cpp.obj"
    "Support/d2c_sysmath.cpp.obj"
    "Support/d2c_sysdate.cpp.obj"
    "Support/d2c_sysexcept.cpp.obj"
    "Support/d2c_sysfile.cpp.obj"
    "Support/Sysutils.cpp.obj"
    "Support/d2c_structures.cpp.obj"
    "Shared/Arraydef.cpp.obj"
    "Shared/CktTree.cpp.obj"
    "Shared/Command.cpp.obj"
    "Shared/Dynamics.cpp.obj"
    "Shared/InvDynamics.cpp.obj"
    "Shared/HashList.cpp.obj"
    "Shared/IniRegSave.cpp.obj"
    "Shared/LineUnits.cpp.obj"
    "Shared/mathutil.cpp.obj"
    "Shared/PointerList.cpp.obj"
    "Shared/Pstcalc.cpp.obj"
    "Shared/StackDef.cpp.obj"
    "Shared/Ucmatrix.cpp.obj"
    "Shared/Ucomplex.cpp.obj"
    "Parser/RPN.cpp.obj"
    "Parser/ParserDel.cpp.obj"
    "Common/AutoAdd.cpp.obj"
    "Common/Bus.cpp.obj"
    "Common/Circuit.cpp.obj"
    "Common/CktElement.cpp.obj"
    "Common/Conductor.cpp.obj"
    "Common/CktElementClass.cpp.obj"
    "Common/ControlQueue.cpp.obj"
    "Common/Diakoptics.cpp.obj"
    "Common/DSSCallBackRoutines.cpp.obj"
    "Common/DSSClass.cpp.obj"
    "Common/DSSClassDefs.cpp.obj"
    "Common/DSSGlobals.cpp.obj"
    "Common/ExportCIMXML.cpp.obj"
    "Common/ExportResults.cpp.obj"
    "Common/Feeder.cpp.obj"
    "Common/MeTIS_Exec.cpp.obj"
    "Common/ShowResults.cpp.obj"
    "Common/Solution.cpp.obj"
    "Common/SolutionAlgs.cpp.obj"
    "Common/Sparse_Math.cpp.obj"
    "Common/Terminal.cpp.obj"
    "Common/Utilities.cpp.obj"
    "Common/YMatrix.cpp.obj"
    "Forms/CmdForms.cpp.obj"
    "GISCommands/GISCommands.cpp.obj"
    "Executive/ConnectOptions.cpp.obj"
    "Executive/ExecCommands.cpp.obj"
    "Executive/ExecHelper.cpp.obj"
    "Executive/ExecOptions.cpp.obj"
    "Executive/Executive.cpp.obj"
    "Executive/ExportOptions.cpp.obj"
    "Executive/ShowOptions.cpp.obj"
    "Executive/PlotOptions.cpp.obj"
    "Meters/MeterClass.cpp.obj"
    "Meters/EnergyMeter.cpp.obj"
    "Meters/fMonitor.cpp.obj"
    "Meters/MemoryMap_lib.cpp.obj"
    "Meters/MeterElement.cpp.obj"
    "Meters/Monitor.cpp.obj"
    "Meters/ReduceAlgs.cpp.obj"
    "Meters/Sensor.cpp.obj"
    "Meters/VLNodeVars.cpp.obj"
    "Controls/CapControl.cpp.obj"
    "Controls/CapControlVars.cpp.obj"
    "Controls/CapUserControl.cpp.obj"
    "Controls/ControlClass.cpp.obj"
    "Controls/ControlElem.cpp.obj"
    "Controls/ESPVLControl.cpp.obj"
    "Controls/ExpControl.cpp.obj"
    "Controls/GenDispatcher.cpp.obj"
    "Controls/InvControl.cpp.obj"
    "Controls/pyControl.cpp.obj"
    "Controls/Recloser.cpp.obj"
    "Controls/RegControl.cpp.obj"
    "Controls/Relay.cpp.obj"
    "Controls/StorageController.cpp.obj"
    "Controls/SwtControl.cpp.obj"
    "Controls/UPFCControl.cpp.obj"
    "PCElements/Equivalent.cpp.obj"
    "PCElements/generator.cpp.obj"
    "PCElements/GeneratorVars.cpp.obj"
    "PCElements/Generic5OrderMach.cpp.obj"
    "PCElements/GenUserModel.cpp.obj"
    "PCElements/GICLine.cpp.obj"
    "PCElements/GICsource.cpp.obj"
    "PCElements/IndMach012.cpp.obj"
    "PCElements/Isource.cpp.obj"
    "PCElements/Load.cpp.obj"
    "PCElements/PCClass.cpp.obj"
    "PCElements/PCElement.cpp.obj"
    "PCElements/PVsystem.cpp.obj"
    "PCElements/PVSystemUserModel.cpp.obj"
    "PCElements/Storage.cpp.obj"
    "PCElements/StorageVars.cpp.obj"
    "PCElements/StoreUserModel.cpp.obj"
    "PCElements/UPFC.cpp.obj"
    "PCElements/vccs.cpp.obj"
    "PCElements/VSConverter.cpp.obj"
    "PCElements/VSource.cpp.obj"
    "PCElements/WindGen.cpp.obj"
    "PCElements/WindGenUserModel.cpp.obj"
    "PCElements/WindGenVars.cpp.obj"
    "PCElements/WTG3_Model.cpp.obj"
    "PDElements/AutoTrans.cpp.obj"
    "PDElements/Capacitor.cpp.obj"
    "PDElements/Fault.cpp.obj"
    "PDElements/fuse.cpp.obj"
    "PDElements/GICTransformer.cpp.obj"
    "PDElements/Line.cpp.obj"
    "PDElements/PDClass.cpp.obj"
    "PDElements/PDElement.cpp.obj"
    "PDElements/Reactor.cpp.obj"
    "PDElements/Transformer.cpp.obj"
    "General/CableConstants.cpp.obj"
    "General/CableData.cpp.obj"
    "General/CNData.cpp.obj"
    "General/CNLineConstants.cpp.obj"
    "General/ConductorData.cpp.obj"
    "General/DSSObject.cpp.obj"
    "General/DynamicExp.cpp.obj"
    "General/GrowthShape.cpp.obj"
    "General/LineCode.cpp.obj"
    "General/LineConstants.cpp.obj"
    "General/LineGeometry.cpp.obj"
    "General/LineSpacing.cpp.obj"
    "General/LoadShape.cpp.obj"
    "General/NamedObject.cpp.obj"
    "General/OHLineConstants.cpp.obj"
    "General/PriceShape.cpp.obj"
    "General/Spectrum.cpp.obj"
    "General/TCC_Curve.cpp.obj"
    "General/TempShape.cpp.obj"
    "General/TSData.cpp.obj"
    "General/TSLineConstants.cpp.obj"
    "General/WireData.cpp.obj"
    "General/XfmrCode.cpp.obj"
    "General/XYcurve.cpp.obj"
    "General/CNTSLineConstants.cpp.obj"
    "MyOpenDSS/MyDSSClassDefs.cpp.obj"
    "Plot/DSSPlot.cpp.obj"
    "Plot/DSSGraph.cpp.obj"
    FILES_FROM_DIR "C:/OpenDSS_rep/VersionC/out/build/windows-default/CMakeFiles/OpenDSSC_Common.dir/./")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  include("C:/OpenDSS_rep/VersionC/out/build/windows-default/CMakeFiles/OpenDSSC_Common.dir/install-cxx-module-bmi-Debug.cmake" OPTIONAL)
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/openDSSC/bin" TYPE EXECUTABLE FILES "C:/OpenDSS_rep/VersionC/out/build/windows-default/OpenDSSCcmd.exe")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  include("C:/OpenDSS_rep/VersionC/out/build/windows-default/CMakeFiles/OpenDSSC_EXE.dir/install-cxx-module-bmi-Debug.cmake" OPTIONAL)
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/openDSSC/bin" TYPE STATIC_LIBRARY OPTIONAL FILES "C:/OpenDSS_rep/VersionC/out/build/windows-default/OpenDSSC.lib")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/openDSSC/bin" TYPE SHARED_LIBRARY FILES "C:/OpenDSS_rep/VersionC/out/build/windows-default/OpenDSSC.dll")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  include("C:/OpenDSS_rep/VersionC/out/build/windows-default/CMakeFiles/OpenDSSC_LIB.dir/install-cxx-module-bmi-Debug.cmake" OPTIONAL)
endif()

if(CMAKE_INSTALL_COMPONENT)
  if(CMAKE_INSTALL_COMPONENT MATCHES "^[a-zA-Z0-9_.+-]+$")
    set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
  else()
    string(MD5 CMAKE_INST_COMP_HASH "${CMAKE_INSTALL_COMPONENT}")
    set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INST_COMP_HASH}.txt")
    unset(CMAKE_INST_COMP_HASH)
  endif()
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
  file(WRITE "C:/OpenDSS_rep/VersionC/out/build/windows-default/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
