// OpenDSSX.cpp : Defines the entry point for the application.
//

#pragma hdrstop

#include <iostream>
#include "OpenDSSC.h"
#include <string>
#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include "Arraydef.h"
#include "Command.h"
#include "HashList.h"
#include "PointerList.h"
#include "RPN.h"
#include "ParserDel.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "CktElement.h"
#include "CktElementClass.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Bus.h"
#include "klusolve.h"
#include "PCClass.h"
#include "PCElement.h"
#include "PDClass.h"
#include "PDElement.h"
#include "CktTree.h"
#include "Pstcalc.h"
#include "StackDef.h"
#include "Terminal.h"
#include "mathutil.h"

#include "XYcurve.h"
#include "XfmrCode.h"
#include "WireData.h"
#include "TSLineConstants.h"
#include "CNData.h"
#include "TSData.h"
#include "TempShape.h"
#include "TCC_Curve.h"
#include "Spectrum.h"
#include "PriceShape.h"
#include "OHLineConstants.h"
#include "NamedObject.h"
#include "LoadShape.h"
#include "LineSpacing.h"
#include "LineGeometry.h"
#include "LineConstants.h"
#include "LineCode.h"
#include "GrowthShape.h"
#include "ConductorData.h"
#include "CNLineConstants.h"
#include "CableData.h"
#include "CableConstants.h"

#include "LineUnits.h"
#include "Conductor.h"
#include "Line.h"
#include "Transformer.h"
#include "Capacitor.h"
#include "Reactor.h"
#include "GICTransformer.h"
#include "fuse.h"
#include "Fault.h"
#include "AutoTrans.h"

#include "WindGenVars.h"
#include "WindGen.h"
#include "WindGenUserModel.h"
#include "VSource.h"
#include "VSConverter.h"
#include "vccs.h"
#include "UPFC.h"
#include "StorageVars.h"
#include "Storage.h"
#include "StoreUserModel.h"
#include "PVsystem.h"
#include "PVSystemUserModel.h"
#include "Load.h"
#include "Isource.h"
#include "IndMach012.h"
#include "GICsource.h"
#include "GICLine.h"
#include "GenUserModel.h"
#include "Generic5OrderMach.h"
#include "GeneratorVars.h"
#include "generator.h"
#include "Equivalent.h"

#include "UPFCControl.h"
#include "SwtControl.h"
#include "StorageController.h"
#include "Relay.h"
#include "RegControl.h"
#include "Recloser.h"
#include "InvControl.h"
#include "GenDispatcher.h"
#include "ExpControl.h"
#include "ESPVLControl.h"
#include "CapControlVars.h"
#include "CapControl.h"
#include "CapUserControl.h"

#include "VLNodeVars.h"
#include "Sensor.h"
#include "ReduceAlgs.h"
#include "Monitor.h"
#include "MeterClass.h"
#include "MeterElement.h"
#include "MemoryMap_lib.h"
#include "LD_fm_infos.h"
#include "fMonitor.h"
#include "EnergyMeter.h"

#include "AutoAdd.h"
#include "Solution.h"
#include "SolutionAlgs.h"
#include "Circuit.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "ControlQueue.h"
#include "DSSCallBackRoutines.h"

#include "Dynamics.h"
#include "InvDynamics.h"
#include "EventQueue.h"
#include "ExecCommands.h"
#include "ExecHelper.h"
#include "ExecOptions.h"
#include "Executive.h"
#include "ExportCIMXML.h"
#include "ExportOptions.h"
#include "ExportResults.h"
#include "Feeder.h"
#include "IniRegSave.h"
#include "MyDSSClassDefs.h"
#include "Notes.h"
#include "ShowOptions.h"
#include "ShowResults.h"
#include "TOPExport.h"
#include "Utilities.h"
#include "YMatrix.h"
#include "ConnectOptions.h"
#include "Diakoptics.h"
#include "Sparse_Math.h"
#include "MeTIS_Exec.h"
#include "GISCommands.h"
#include "djson.h"
#include "CmdForms.h"
#include "myCmdUtils.h"
#include "myCmdUtils.cpp"

#ifdef OPENDSSX_CPP_EXTRA_HEADER
// Include another file from a parent project.
// For details, see the note about PARENT PROJECTS at the end of this file.
#include OPENDSSX_CPP_EXTRA_HEADER
#endif


//#pragma resource "*.RES"
#define MAXIDX(x) (sizeof(x)/sizeof(x[0]))-1


//class TMyApplication;

using namespace std;


class TMyApplication  {

public:
    void DoRun(vector < string > myParam);
    TMyApplication();
    ~TMyApplication();
    virtual void WriteHelp();
    virtual void WriteLicensing();
    int GetOptionIdx(string myCMD);
    void Execute(string myCMD);
};



const string ExeName = "OpenDSSCMD";


bool UserFinished(string Cmd)
{
    bool result = false;
    result = false;
    Cmd = LowerCase(Cmd);
    if (Cmd == "exit")      result = true;
    else if (Cmd[0] == 'q') result = true;
    return result;
}


void TMyApplication::DoRun(vector <string> myParam)
{
    string ErrorMsg, Cmd;
//    Char* LNresult = NULL;
    int i = 0, CmdIdx = 0;
    NoFormsAllowed = true;
    ActiveActor = 1;
    IsDLL = false;
    DSSExecutive[ActiveActor] = new TExecutive();  // Make a DSS object
    DSSExecutive[ActiveActor]->CreateDefaultDSSItems();
    CoutLn("");
    CoutLn("Startup Directory: " + StartupDirectory);
    CoutLn("Data Directory: " + DataDirectory[ActiveActor]);
    CoutLn("Output Directory: " + OutputDirectory[ActiveActor]);
    CoutLn("GetCurrentDir: " + GetCurrentDir());
    CoutLn("---------------------");
    CoutLn("");
    CoutLn("Type '-help' for options");
    CoutLn("");
    CoutLn("******************************************************************");
    DataDirectory[ActiveActor] = StartupDirectory;
    OutputDirectory[ActiveActor] = StartupDirectory;
    SetCurrentDir(DataDirectory[ActiveActor]);
    NoFormsAllowed = false;  // messages will go to the console
    if (myParam.size() > 0)
    {
        for (int stop = (int) myParam.size(), i = 0; i < stop; i++)
        {
            Cmd = myParam[i];
            CoutLn(myParam[i]);
            Execute(Cmd);
        }
    }
    else
    {
        do
        {
            {  // this has no command history
                cout << CRLF;
                cout << ">>";
                getline(cin,Cmd);

                if (!Cmd.empty()) 
                    Execute(Cmd);
            }
        } while (!(UserFinished(Cmd)));
    }
}


void TMyApplication::Execute(string myCMD)
{
    int CmdIdx = 0;
    CmdIdx = GetOptionIdx(myCMD);
    if (CmdIdx == 0)
    {
        DSSExecutive[ActiveActor]->Set_Command(myCMD);
        if (DSSExecutive[ActiveActor]->Get_LastError() != "")
            CoutLn(DSSExecutive[ActiveActor]->Get_LastError());
        else
            CoutLn(GlobalResult);
    }
    else
    {
        switch (CmdIdx)
        {
        case 1:
            WriteHelp();
            break;
        case 2:
            cout << "OpenDSS console " + VersionString;
            break;
        case 3: case 4:
            cout << "Not implemented in Embarcadero version";
            break;
        case 5:
            WriteLicensing();
            break;
        case 6: case 7:
            cout << "Leaving the program";
            break;
        default:
            cout << "option not recognized";
        }
    }
}


TMyApplication::TMyApplication()
{
    // inherited::Create();
  //  StopOnException:=True;
}


TMyApplication::~TMyApplication()
{
    // todo check:  inherited::Destroy;
}


int TMyApplication::GetOptionIdx(string myCMD)
{
    int result = 0;
    myCMD = LowerCase(myCMD);
    result = 0;
    if ((myCMD == "-help") || (myCMD == "-h"))
        result = 1;
    if (myCMD == "-v")
        result = 2;
    if (myCMD == "-f")
        result = 3;
    if (myCMD == "-l")
        result = 4;
    if (myCMD == "-lic")
        result = 5;
    if (myCMD == "exit")
        result = 6;
    if (myCMD == "q")
        result = 7;
    return result;
}


void TMyApplication::WriteHelp()
{
    CoutLn("Usage: " + ExeName + " [-v | -h | -f | -l] [stop_time] [filename]");
    CoutLn(" [filename] -> optional DSS command file.");
    CoutLn("      If provided, runs this file and exits.");
    CoutLn("      If not provided, accepts user commands at the >> prompt.");
    CoutLn(" -h -> displays this message and exits");
    CoutLn(" -v -> displays the version and exits");
    CoutLn(" -Lic -> displays the license agreement");
    CoutLn(" exit, q -> leaves the program");
}

void TMyApplication::WriteLicensing()
{
    CoutLn("Copyright (c) 2008-2024, Electric Power Research Institute, Inc." 
        + CRLF + "All rights reserved." 
        + CRLF + "" 
        + CRLF + "Redistribution and use in source and binary forms, with or without" 
        + CRLF + "modification, are permitted provided that the following conditions are met:" 
        + CRLF + "    * Redistributions of source code must retain the above copyright"
        + CRLF + "      notice, this list of conditions and the following disclaimer." 
        + CRLF + "    * Redistributions in binary form must reproduce the above copyright" 
        + CRLF + "      notice, this list of conditions and the following disclaimer in the" 
        + CRLF + "      documentation and/or other materials provided with the distribution." 
        + CRLF + "    * Neither the name of the Electric Power Research Institute, Inc., nor"
        + CRLF + "      the names of its contributors may be used to endorse or promote" 
        + CRLF + "      products derived from this software without specific prior written" 
        + CRLF + "      permission." 
        + CRLF + "" 
        + CRLF + "THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., \"AS IS\""
        + CRLF + "AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE" 
        + CRLF + "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR" 
        + CRLF + "PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.," 
        + CRLF + "OR ANY OTHER ENTITY CONTRIBUTING TO OR INVOLVED IN THE PROVISION OF THE SOFTWARE," 
        + CRLF + "BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR"
        + CRLF + "CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF" 
        + CRLF + "SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS" 
        + CRLF + "INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN" 
        + CRLF + "CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)" 
        + CRLF + "ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE"
        + CRLF + "POSSIBILITY OF SUCH DAMAGE." 
        + CRLF);
}

int main(int argc, char* argv[])
{
    std::vector<std::string> myParam;

    //int i=1 is starting value, because argv[0] is the program's name.
    std::string mypath = "";

    //if (argc == 1)   // To force redirecting to default script when debugging and to not override external args
    //{
        //mypath = "compile D:\\Projects-D\\INDUCES\\Examples\\Kundur\\Dynamic_Kundur.dss";
        //argc = 2;
        //argv[1] = const_cast<char*>(mypath.c_str());
    //}

    for (int i = 1; i < argc; i++)
    {
        std::string myStr(argv[i]);
        myParam.push_back(myStr);
    }

    using namespace DSSGlobals;

    TMyApplication Application;

    CoutLn("**********************EPRI OpenDSS simulator**********************");
    CoutLn("* OpenDSS console " + VersionString + "          *");
    CoutLn("*Copyright (c) 2008-2024, Electric Power Research Institute, Inc.*");
    CoutLn("******************************************************************");

#ifdef OPENDSSX_CPP_MAIN_AFTER_INIT
    // Include additional code after our initialization:
    OPENDSSX_CPP_MAIN_AFTER_INIT
#endif
        
#ifndef OPENDSSX_CPP_MAIN_ALTERNATE

    // Added - DM 12/12/2022
//    myParam.resize(0); // for regular rebugging

    // This is the normal application event loop and return value:
    Application.DoRun(myParam);
    ExitCode = DSSExecutive[ActiveActor]->Get_ErrorResult();

    return ExitCode;
#else
    // Replace the normal application event loop and return value:
    OPENDSSX_CPP_MAIN_ALTERNATE
#endif
}

/* NOTE: PARENT PROJECTS
A note regarding these three #define's used in this source file:
    OPENDSSX_CPP_EXTRA_HEADER
    OPENDSSX_CPP_MAIN_AFTER_INIT
    OPENDSSX_CPP_MAIN_ALTERNATE

These names are optional and normally undefined to produce a standard build of
OpenDSS-X, but they permit any other project to act as a "parent" to import and
extend OpenDSS-X, with minimal (no?) changes to OpenDSS-X's code.

OPENDSSX_CPP_EXTRA_HEADER
    Specifies a header to #include at the top of this file.  This header
    will likely define either OPENDSSX_CPP_MAIN_AFTER_INIT or
    OPENDSSX_CPP_MAIN_ALTERNATE to modify the execution within int main().

    Example lines to go into this header:
        // Print out additional text after the startup banner:
        #define OPENDSSX_CPP_MAIN_AFTER_INIT std::cout << "Enhanced!\n";

    Alternatively, this header is free to make more drastic modifications,
    such as using a #define to rename our main() in order to replace it
    with the parent project's main() instead.

OPENDSSX_CPP_MAIN_AFTER_INIT
    Specifies additional code to include within our main() just after the
    normal initialization and command line parsing, but before the main
    application loop.

OPENDSSX_CPP_MAIN_ALTERNATE
    Specifies code to completely replace the main Application.DoRun loop.


Example usage:

For example if the parent project is also using CMake, then it may import and
extend OpenDSS-X by adding these lines to it's (the parent's) CMakeLists.txt
and not have to change OpenDSS-X's CMakeLists.txt file.

# Example commands for a parent project's CMakeLists.txt
add_subdirectory(opendss-x)
set_target_properties(OpenDSSX PROPERTIES
    # Rename the output executable binary:
    OUTPUT_NAME "OpenDSSX-with-extra-features"
    # Place this target with our other executables:
    RUNTIME_OUTPUT_DIRECTORY ".."
)
# Add more #include search directories:
target_include_directories(OpenDSSX PRIVATE
    other_include_dir
    another_include_dir
)
# Include a specific header in opendss-x/CMD/OpenDSSX.cpp:
target_compile_definitions(OpenDSSX PRIVATE
    OPENDSSX_CPP_EXTRA_HEADER="extend_opendssx_main.h"
    # Example text to go in this header to inject additional text after the normal startup banner:
    # #define OPENDSSX_CPP_MAIN_AFTER_INIT std::cout << "Extensions!\n";
)
# Add more sources to be compiled into the OpenDSS-X executable:
sources(OpenDSSX PRIVATE
    src-dirA/featureA.cpp
    src-dirB/featureB.cpp
)
*/
