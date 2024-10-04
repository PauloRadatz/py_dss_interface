

#pragma hdrstop

#include "ExecCommands.h"
#include "dirsep.h"

int ReadEfieldHDF(int a);
int ReadEfieldHDF(int a) {
	return a;
};

using namespace std;

namespace ExecCommands
{
	TCommandList CommandList;
	String dummy;
	String LastCmdLine;
	String RedirFile;
	string* ExecCommand;
	string* CommandHelp;

	void DefineCommands()
	{
		ExecCommand = new std::string[NumExecCommands];
		CommandHelp = new std::string[NumExecCommands];
		/*Main executive commands*/
		ExecCommand[1 - 1] = "New";
		ExecCommand[2 - 1] = "Edit";
		ExecCommand[3 - 1] = "More";
		ExecCommand[4 - 1] = "M";
		ExecCommand[5 - 1] = "~";
		ExecCommand[6 - 1] = "Select";
		ExecCommand[7 - 1] = "Save";
		ExecCommand[8 - 1] = "Show";
		ExecCommand[9 - 1] = "Solve";
		ExecCommand[10 - 1] = "Enable";
		ExecCommand[11 - 1] = "Disable";
		ExecCommand[12 - 1] = "Plot";
		ExecCommand[13 - 1] = "Reset";
		ExecCommand[14 - 1] = "Compile";
		ExecCommand[15 - 1] = "Set";   // Set DSS Options
		ExecCommand[16 - 1] = "Dump";   // Debug dump
		ExecCommand[17 - 1] = "Open";   // Open a device terminal conductor
		ExecCommand[18 - 1] = "Close";   // Close a device terminal conductor
		ExecCommand[19 - 1] = "//";       // Comment
		ExecCommand[20 - 1] = "Redirect";
		ExecCommand[21 - 1] = "Help";
		ExecCommand[22 - 1] = "Quit";
		ExecCommand[23 - 1] = "?";   // Property Value inquiry
		ExecCommand[24 - 1] = "Next";
		ExecCommand[25 - 1] = "Panel";
		ExecCommand[26 - 1] = "Sample";
		ExecCommand[27 - 1] = "Clear";
		ExecCommand[28 - 1] = "About";
		ExecCommand[29 - 1] = "Calcvoltagebases";  //  Computes voltage bases
		ExecCommand[30 - 1] = "SetkVBase";  //  Set kV Base at a Bus
		ExecCommand[31 - 1] = "BuildY";  //  forces Rebuild of Y matrix right now
		ExecCommand[32 - 1] = "Get";  //  returns values set WITH Set command
		ExecCommand[33 - 1] = "Init";
		ExecCommand[34 - 1] = "Export";
		ExecCommand[35 - 1] = "Fileedit";
		ExecCommand[36 - 1] = "Voltages";
		ExecCommand[37 - 1] = "Currents";
		ExecCommand[38 - 1] = "Powers";
		ExecCommand[39 - 1] = "Seqvoltages";
		ExecCommand[40 - 1] = "Seqcurrents";
		ExecCommand[41 - 1] = "Seqpowers";
		ExecCommand[42 - 1] = "Losses";
		ExecCommand[43 - 1] = "Phaselosses";
		ExecCommand[44 - 1] = "Cktlosses";
		ExecCommand[45 - 1] = "Allocateloads";
		ExecCommand[46 - 1] = "Formedit";
		ExecCommand[47 - 1] = "Totals";  // Total all energymeters
		ExecCommand[48 - 1] = "Capacity";  // Find upper kW limit of system for present year
		ExecCommand[49 - 1] = "Classes";  // List of intrinsic classes
		ExecCommand[50 - 1] = "Userclasses";  // List of user-defined classes
		ExecCommand[51 - 1] = "Zsc";
		ExecCommand[52 - 1] = "Zsc10";
		ExecCommand[53 - 1] = "ZscRefresh";
		ExecCommand[54 - 1] = "Ysc";
		ExecCommand[55 - 1] = "puvoltages";
		ExecCommand[56 - 1] = "VarValues";
		ExecCommand[57 - 1] = "Varnames";
		ExecCommand[58 - 1] = "Buscoords";
		ExecCommand[59 - 1] = "MakeBusList";
		ExecCommand[60 - 1] = "MakePosSeq";
		ExecCommand[61 - 1] = "Reduce";
		ExecCommand[62 - 1] = "Interpolate";
		ExecCommand[63 - 1] = "AlignFile";
		ExecCommand[64 - 1] = "TOP";
		ExecCommand[65 - 1] = "Rotate";
		ExecCommand[66 - 1] = "Vdiff";
		ExecCommand[67 - 1] = "Summary";
		ExecCommand[68 - 1] = "Distribute";
		ExecCommand[69 - 1] = "DI_plot";
		ExecCommand[70 - 1] = "Comparecases";
		ExecCommand[71 - 1] = "YearlyCurves";
		ExecCommand[72 - 1] = "CD";
		ExecCommand[73 - 1] = "Visualize";
		ExecCommand[74 - 1] = "CloseDI";
		ExecCommand[75 - 1] = "DOScmd";
		ExecCommand[76 - 1] = "Estimate";
		ExecCommand[77 - 1] = "Reconductor";
		ExecCommand[78 - 1] = "_InitSnap";
		ExecCommand[79 - 1] = "_SolveNoControl";
		ExecCommand[80 - 1] = "_SampleControls";
		ExecCommand[81 - 1] = "_DoControlActions";
		ExecCommand[82 - 1] = "_ShowControlQueue";
		ExecCommand[83 - 1] = "_SolveDirect";
		ExecCommand[84 - 1] = "_SolvePFlow";
		ExecCommand[85 - 1] = "AddBusMarker";
		ExecCommand[86 - 1] = "Uuids";
		ExecCommand[87 - 1] = "SetLoadAndGenKV";
		ExecCommand[88 - 1] = "CvrtLoadshapes";
		ExecCommand[89 - 1] = "NodeDiff";
		ExecCommand[90 - 1] = "Rephase";
		ExecCommand[91 - 1] = "SetBusXY";
		ExecCommand[92 - 1] = "UpdateStorage";
		ExecCommand[93 - 1] = "Obfuscate";
		ExecCommand[94 - 1] = "LatLongCoords";
		ExecCommand[95 - 1] = "BatchEdit";
		ExecCommand[96 - 1] = "Pstcalc";
		ExecCommand[97 - 1] = "Variable";
		ExecCommand[98 - 1] = "ReprocessBuses";
		ExecCommand[99 - 1] = "ClearBusMarkers";
		ExecCommand[100 - 1] = "RelCalc";
		ExecCommand[101 - 1] = "var";
		ExecCommand[102 - 1] = "Cleanup";
		ExecCommand[103 - 1] = "FinishTimeStep";
		ExecCommand[104 - 1] = "NodeList";
		ExecCommand[105 - 1] = "NewActor";
		ExecCommand[106 - 1] = "ClearAll";
		ExecCommand[107 - 1] = "Wait";
		ExecCommand[108 - 1] = "SolveAll";
		ExecCommand[109 - 1] = "CalcIncMatrix";
		ExecCommand[110 - 1] = "CalcIncMatrix_O";
		ExecCommand[111 - 1] = "Tear_Circuit";
		ExecCommand[112 - 1] = "Connect";
		ExecCommand[113 - 1] = "Disconnect";
		ExecCommand[114 - 1] = "Refine_BusLevels";
		ExecCommand[115 - 1] = "Remove";
		ExecCommand[116 - 1] = "Abort";
		ExecCommand[117 - 1] = "CalcLaplacian";
		ExecCommand[118 - 1] = "Clone";
		ExecCommand[119 - 1] = "FNCSPublish";
		//  ExecCommand[119] := 'UpdateStorage2';
		ExecCommand[120 - 1] = "ExportOverloads";
		ExecCommand[121 - 1] = "ExportVViolations";
		ExecCommand[122 - 1] = "Zsc012";
		ExecCommand[123 - 1] = "AggregateProfiles";
		ExecCommand[124 - 1] = "AllPCEatBus";
		ExecCommand[125 - 1] = "AllPDEatBus";
		ExecCommand[126 - 1] = "TotalPowers";
		ExecCommand[127 - 1] = "COMHelp";
		ExecCommand[128 - 1] = "GIS";
		ExecCommand[129 - 1] = "GISCoords";
		//	Start list of INDUCES commands
		ExecCommand[130 - 1] = "ReadEfieldHDF";

		//	Ends list of INDUCES commands
		


		// Command help descriptions
		CommandHelp[1 - 1] = String("Create a new object within the DSS. Object becomes the " "active object") + CRLF
			+ "Example: New Line.line1 ...";
		CommandHelp[2 - 1] = String("Edit an object. The object is selected and it then becomes the active object.") + CRLF
			+ CRLF
			+ "Note that Edit is the default command.  You many change a property value simply by "
			+ "giving the full property name and the new value, for example:"
			+ CRLF
			+ CRLF
			+ "line.line1.r1=.04"
			+ CRLF
			+ "vsource.source.kvll=230";
		CommandHelp[3 - 1] = "Continuation of editing on the active object.";
		CommandHelp[4 - 1] = "Continuation of editing on the active object. An abbreviation for More";
		CommandHelp[5 - 1] = String("Continuation of editing on the active object. An abbreviation.") + CRLF
			+ CRLF
			+ "Example:"
			+ CRLF
			+ "New Line.Line1 Bus1=aaa  bus2=bbb"
			+ CRLF
			+ "~ R1=.058"
			+ CRLF
			+ "~ X1=.1121";
		CommandHelp[6 - 1] = String("Selects an element and makes it the active element.  You can also specify the " "active terminal (default = 1).") + CRLF
			+ CRLF
			+ "Syntax:"
			+ CRLF
			+ "Select [-1]elementname  [terminal= - 1]terminalnumber "
			+ CRLF
			+ CRLF
			+ "Example:"
			+ CRLF
			+ "Select Line.Line1 "
			+ CRLF
			+ "~ R1=.1"
			+ CRLF
			+ "(continue editing)"
			+ CRLF
			+ CRLF
			+ "Select Line.Line1 2 "
			+ CRLF
			+ "Voltages  (returns voltages at terminal 2 in Result)";
		CommandHelp[7 - 1] = String("{Save [class= - 1]{Meters | Circuit | Voltages | (classname)} [file= - 1]filename [dir= - 1]directory ") + CRLF
			+ CRLF
			+ "Default class = Meters, which saves the present values in both monitors and energy meters in the active circuit. "
			+ CRLF
			+ CRLF
			+ "\"Save Circuit\" saves the present enabled circuit elements to the specified subdirectory in standard DSS form "
			+ "with a Master.txt file and separate files for each class of data. "
			+ CRLF
			+ CRLF
			+ "If Dir= not specified a unique name based on the circuit name is created automatically. "
			+ CRLF
			+ CRLF
			+ "If Dir= is specified, any existing files are overwritten. "
			+ CRLF
			+ CRLF
			+ "\"Save Voltages\" saves the present solution in a simple CSV format in a file called DSS_SavedVoltages. "
			+ "Used for VDIFF command."
			+ CRLF
			+ CRLF
			+ "Any class can be saved to a file.  If no filename specified, the classname is used.";
		CommandHelp[8 - 1] = String("Writes selected results to a text file and brings " "up the default text editor (see Set Editor=....) with the file for you to browse.") + CRLF
			+ CRLF
			+ "See separate help on Show command. "
			+ CRLF
			+ CRLF
			+ "Default is \"show voltages LN Seq\".  ";
		CommandHelp[9 - 1] = "Perform the solution of the present solution mode. You can set any option "
			"that you can set with the Set command (see Set). "
			"The Solve command is virtually synonymous with the Set command except that "
			"a solution is performed after the options are processed.";
		CommandHelp[10 - 1] = String("Enables a circuit element or entire class.  Example:") + CRLF
			+ "Enable load.loadxxx"
			+ CRLF
			+ "Enable generator.*  (enables all generators)";
		CommandHelp[11 - 1] = String("Disables a circuit element or entire class.  Example:") + CRLF
			+ "Disable load.loadxxx"
			+ CRLF
			+ "Disable generator.*  (Disables all generators)"
			+ CRLF
			+ CRLF
			+ "The item remains defined, but is not included in the solution.";
		CommandHelp[12 - 1] = "Plots circuits and results in a variety of manners.  See separate Plot command help.";
		CommandHelp[13 - 1] = "{MOnitors | MEters | Faults | Controls | Eventlog | Keeplist |(no argument) } Resets all Monitors, Energymeters, etc. "
			"If no argument specified, resets all options listed.";
		CommandHelp[14 - 1] = String("Reads the designated file name containing DSS commands " "and processes them as if they were entered directly into the command line. " "The file is said to be \"compiled.\" " "Similar to \"redirect\" except changes the default directory to the path of the specified file.") + CRLF
			+ CRLF
			+ "Syntax:"
			+ CRLF
			+ "Compile filename";
		CommandHelp[15 - 1] = "Used to set various DSS solution modes and options.  You may also set the options with the Solve command. "
			"See \"Options\" for help.";
		CommandHelp[16 - 1] = String("Display the properties of either a specific DSS object or a complete dump " "on all variables in the problem (Warning! Could be very large!)." " Brings up the default text editor with the text file written by this command.") + CRLF
			+ " Syntax: dump [-1] [debug - 1]"
			+ CRLF
			+ " Examples:"
			+ CRLF
			+ CRLF
			+ " Dump line.line1 "
			+ CRLF
			+ " Dump solution  (dumps all solution vars) "
			+ CRLF
			+ " Dump commands  (dumps all commands to a text file) "
			+ CRLF
			+ " Dump transformer.*  (dumps all transformers)"
			+ CRLF
			+ " Dump ALLOCationfactors  (load allocation factors)"
			+ CRLF
			+ " Dump Buslist    (bus name hash list)"
			+ CRLF
			+ " Dump Devicelist    (Device name hash list)"
			+ CRLF
			+ " Dump      (dumps all objects in circuit) ";
		//' Dump debug';   // Debug dump
		CommandHelp[17 - 1] = String("Opens the specified terminal and conductor of the specified circuit element. " "If the conductor is not specified, all phase conductors of the terminal are opened.") + CRLF
			+ CRLF
			+ "Examples:"
			+ CRLF
			+ "Open line.line1 2 "
			+ CRLF
			+ "(opens all phases of terminal 2)"
			+ CRLF
			+ CRLF
			+ "Open line.line1 2 3"
			+ CRLF
			+ "(opens the 3rd conductor of terminal 2)";
		CommandHelp[18 - 1] = "Opposite of the Open command.";   // Close a device terminal conductor
		CommandHelp[19 - 1] = "Comment.  Command line is ignored.";       // Comment
		CommandHelp[20 - 1] = String("Reads the designated file name containing DSS commands " "and processes them as if they were entered directly into the command line. " "Similar to \"Compile\", but leaves current directory where it was when Redirect command is invoked." "Can temporarily change to subdirectories if nested Redirect commands require.") + CRLF
			+ CRLF
			+ "ex:  redirect filename";
		CommandHelp[21 - 1] = "Gives this display.";
		CommandHelp[22 - 1] = "Shuts down DSS unless this is the DLL version.  Then it does nothing;  DLL parent is responsible for shutting down the DLL.";
		CommandHelp[23 - 1] = String("Inquiry for property value.  Result is put into GlobalReault and can be seen in the Result Window. " "Specify the full property name.") + CRLF
			+ CRLF
			+ "Example: ? Line.Line1.R1"
			+ CRLF
			+ CRLF
			+ "Note you can set this property merely by saying:"
			+ CRLF
			+ "Line.line1.r1=.058";   // Property Value inquiry
		CommandHelp[24 - 1] = "{Year | Hour | t}  Increments year, hour, or time as specified.  If \"t\" is "
			"specified, then increments time by current step size.";
		CommandHelp[25 - 1] = "Displays main control panel window.";
		CommandHelp[26 - 1] = "Force all monitors and meters to take a sample for the most recent solution. Keep in mind that meters will perform integration.";
		CommandHelp[27 - 1] = "Clear all circuits currently in memory.";
		CommandHelp[28 - 1] = "Display \"About Box\".  (Result string set to Version string.)";
		CommandHelp[29 - 1] = "Calculates voltagebase for buses based on voltage bases defined "
			"with Set voltagebases=... command.";
		CommandHelp[30 - 1] = String("Command to explicitly set the base voltage for a bus. " "Bus must be previously defined. Parameters in order are:") + CRLF
			+ "Bus = {bus name}"
			+ CRLF
			+ "kVLL = (line-to-line base kV)"
			+ CRLF
			+ "kVLN = (line-to-neutral base kV)"
			+ CRLF
			+ CRLF
			+ "kV base is normally given in line-to-line kV (phase-phase). "
			+ "However, it may also be specified by line-to-neutral kV."
			+ CRLF
			+ "The following exampes are equivalent:"
			+ CRLF
			+ CRLF
			+ "setkvbase Bus=B9654 kVLL=13.2"
			+ CRLF
			+ "setkvbase B9654 13.2"
			+ CRLF
			+ "setkvbase B9654 kvln=7.62";
		CommandHelp[31 - 1] = "Forces rebuild of Y matrix upon next Solve command regardless of need. "
			"The usual reason for doing this would be to reset the matrix for another "
			"load level when using LoadModel=PowerFlow (the default) when the system is difficult to "
			"solve when the load is far from its base value.  Works by invalidating the Y primitive "
			"matrices for all the Power Conversion elements.";
		CommandHelp[32 - 1] = String("Returns DSS property values set using the Set command. " "Result is returne in Result property of the Text interface. ") + CRLF
			+ CRLF
			+ "VBA Example:"
			+ CRLF
			+ CRLF
			+ "DSSText.Command = \"Get mode\""
			+ CRLF
			+ "Answer = DSSText.Result"
			+ CRLF
			+ CRLF
			+ "Multiple properties may be requested on one get.  The results are appended "
			+ "and the individual values separated by commas."
			+ CRLF
			+ CRLF
			+ "See help on Set command for property names.";
		CommandHelp[33 - 1] = "This command forces reinitialization of the solution for the next Solve command. "
			"To minimize iterations, most solutions start with the previous solution unless there "
			"has been a circuit change.  However, if the previous solution is bad, it may be necessary "
			"to re-initialize.  In most cases, a re-initiallization results in a zero-load power flow "
			"solution with only the series power delivery elements considered.";
		CommandHelp[34 - 1] = String("Export various solution values to CSV (or XML) files for import into other programs. " "Creates a new file except for Energymeter and Generator objects, for which " "the results for each device of this class are APPENDED to the CSV File. You may export to " "a specific file by specifying the file name as the LAST parameter on the line. For example:") + CRLF
			+ CRLF
			+ "  Export Voltage Myvoltagefile.CSV"
			+ CRLF
			+ CRLF
			+ "Otherwise, the default file names shown in the Export help are used. "
			+ "For Energymeter and Generator, specifying the switch \"/multiple\" (or /m) for the file name will cause "
			+ "a separate file to be written for each meter or generator. "
			+ "The default is for a single file containing all elements."
			+ CRLF
			+ CRLF
			+ "May be abreviated Export V, Export C, etc.  Default is \"V\" for voltages."
			+ " If Set ShowExport=Yes, the output file will be automatically displayed in the default editor. "
			+ "Otherwise, you must open the file separately. The name appears in the Result window.";
		CommandHelp[35 - 1] = String("Edit specified file in default text file editor (see Set Editor= option).") + CRLF
			+ CRLF
			+ "Fileedit EXP_METERS.CSV (brings up the meters export file)"
			+ CRLF
			+ CRLF
			+ "\"FileEdit\" may be abbreviated to a unique character string.";
		CommandHelp[36 - 1] = "Returns the voltages for the ACTIVE BUS in the Result string. "
			"For setting the active Bus, use the Select command or the Set Bus= option. "
			"Returned as magnitude and angle quantities, comma separated, one set per conductor of the terminal.";
		CommandHelp[37 - 1] = "Returns the currents for each conductor of ALL terminals of the active circuit element in the Result string. "
			"(See Select command.)"
			"Returned as comma-separated magnitude and angle.";
		CommandHelp[38 - 1] = "Returns the powers (complex) going into each conductors of ALL terminals of the active circuit element in the Result string. "
			"(See Select command.)"
			"Returned as comma-separated kW and kvar.";
		CommandHelp[39 - 1] = "Returns the sequence voltages at all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values."
			"Order of returned values: 0, 1, 2  (for each terminal).";
		CommandHelp[40 - 1] = "Returns the sequence currents into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values."
			"Order of returned values: 0, 1, 2  (for each terminal).";
		CommandHelp[41 - 1] = "Returns the sequence powers into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated kw, kvar pairs."
			"Order of returned values: 0, 1, 2  (for each terminal).";
		CommandHelp[42 - 1] = "Returns the total losses for the active circuit element (see Select command) "
			"in the Result string in kW, kvar.";
		CommandHelp[43 - 1] = "Returns the losses for the active circuit element (see Select command) "
			"for each PHASE in the Result string in comma-separated kW, kvar pairs.";
		CommandHelp[44 - 1] = "Returns the total losses for the active circuit in the Result string in kW, kvar.";
		CommandHelp[45 - 1] = "Estimates the allocation factors for loads that are defined using the XFKVA property. "
			"Requires that energymeter objects be defined with the PEAKCURRENT property set. "
			"Loads that are not in the zone of an energymeter cannot be allocated.";
		CommandHelp[46 - 1] = "FormEdit [class.object - 1].  Brings up form editor on active DSS object.";
		CommandHelp[47 - 1] = "Totals all EnergyMeter objects in the circuit and reports register totals in the result string.";
		CommandHelp[48 - 1] = String("Find the maximum load the active circuit can serve in the PRESENT YEAR. Uses the EnergyMeter objects with the registers " "set with the SET UEREGS= (..) command for the AutoAdd functions.  Syntax (defaults shown):") + CRLF
			+ CRLF
			+ "capacity [-1]0.9 [increment= - 1]0.005"
			+ CRLF
			+ CRLF
			+ "Returns the metered kW (load + losses - generation) and per unit load multiplier for the loading level at which something in the system reports an overload or undervoltage. "
			+ "If no violations, then it returns the metered kW for peak load for the year (1.0 multiplier). "
			+ "Aborts and returns 0 if no energymeters.";
		CommandHelp[49 - 1] = "List of intrinsic DSS Classes. Returns comma-separated list in Result variable.";
		CommandHelp[50 - 1] = "List of user-defined DSS Classes. Returns comma-separated list in Result variable.";
		CommandHelp[51 - 1] = "Returns full Zsc matrix for the ACTIVE BUS in comma-separated complex number form.";
		CommandHelp[52 - 1] = "Returns symmetrical component impedances, Z1, Z0 for the ACTIVE BUS in comma-separated R+jX form.";
		CommandHelp[53 - 1] = "Refreshes Zsc matrix for the ACTIVE BUS.";
		CommandHelp[54 - 1] = "Returns full Ysc matrix for the ACTIVE BUS in comma-separated complex number form G + jB.";
		CommandHelp[55 - 1] = "Just like the Voltages command, except the voltages are in per unit if the kVbase at the bus is defined.";
		CommandHelp[56 - 1] = "Returns variable values for active element if PC element. Otherwise, returns null.";
		CommandHelp[57 - 1] = "Returns variable names for active element if PC element. Otherwise, returns null.";
		CommandHelp[58 - 1] = String("Define x,y coordinates for buses.  Execute after Solve or MakeBusList command is executed so that bus lists are defined." "Reads coordinates from a CSV file with records of the form: busname, x, y.") + CRLF
			+ CRLF
			+ "Example: BusCoords [-1]xxxx.csv";
		CommandHelp[59 - 1] = "Updates the buslist, if needed, using the currently enabled circuit elements.  (This happens automatically for Solve command.)"
			" See ReprocessBuses";
		CommandHelp[60 - 1] = "Attempts to convert present circuit model to a positive sequence equivalent. "
			"It is recommended to Save the circuit after this and edit the saved version to correct possible misinterpretations.";
		CommandHelp[61 - 1] = "{All | MeterName}  Default is \"All\".  Reduce the circuit according to reduction options. "
			"See \"Set ReduceOptions\" and \"Set Keeplist\" options."
			"Energymeter objects actually perform the reduction.  \"All\" causes all meters to reduce their zones.";
		CommandHelp[62 - 1] = "{All | MeterName}  Default is \"All\". Interpolates coordinates for missing bus coordinates in meter zone";
		CommandHelp[63 - 1] = "Alignfile [file= - 1]filename.  Aligns DSS script files in columns for easier reading.";
		CommandHelp[64 - 1] = "[class= - 1]{Loadshape | Tshape | Monitor  } [object= - 1]{ALL (Loadshapes only) | objectname}. "
			"Send specified object to TOP.  Loadshapes and TShapes must be hourly fixed interval. ";
		CommandHelp[65 - 1] = "Usage: Rotate [angle= - 1]nnn.  Rotate circuit plotting coordinates by specified angle (degrees). ";
		CommandHelp[66 - 1] = "Displays the difference between the present solution and the last on saved using the SAVE VOLTAGES command.";
		CommandHelp[67 - 1] = "Returns a power flow summary of the most recent solution in the global result string.";
		CommandHelp[68 - 1] = String("kw=nn how={Proportional* | Uniform |Random | Skip} skip=nn PF=nn file=filename MW=nn What=[Generator*|Load - 1]") + CRLF
			+ CRLF
			+ "Creates a DSS script file to distribute Generator or Load objects on the system in the manner specified by \"how\"."
			+ CRLF
			+ "kW = total generation to be distributed (default=1000) "
			+ CRLF
			+ "how= process name as indicated (default=proportional to load)"
			+ CRLF
			+ "skip = no. of buses to skip for \"How=Skip\" (default=1)"
			+ CRLF
			+ "PF = power factor for new generators (default=1.0)"
			+ CRLF
			+ "file = name of file to save (default=distgenerators.dss or distloads.dss)"
			+ CRLF
			+ "MW = alternate way to specify kW (default = 1)"
			+ CRLF
			+ "What = what type of device to add, Generator (default) or Load";
		CommandHelp[69 - 1] = String("[case= - 1]casename [year= - 1]yr [registers= - 1](reg1, reg2,...)  [peak= - 1]y/n  [meter= - 1]metername") + CRLF
			+ "Plots demand interval (DI) results from yearly simulation cases.  "
			+ "Plots selected registers from selected meter file (default = DI_Totals.CSV).  "
			+ "Peak defaults to NO.  If YES, only daily peak of specified registers "
			+ "is plotted. Example:"
			+ CRLF
			+ CRLF
			+ " DI_Plot basecase year=5 registers=(9,11) no";
		CommandHelp[70 - 1] = String("[Case1= - 1]casename [case2= - 1]casename [register= - 1](register number) [meter= - 1]{Totals* | SystemMeter | metername}. ") + CRLF
			+ "Compares yearly simulations of two specified cases with respect to the quantity in the designated register "
			+ "from the designated meter file. "
			+ "Defaults: Register=9 meter=Totals.  Example:"
			+ CRLF
			+ CRLF
			+ "Comparecases base pvgens 10";
		CommandHelp[71 - 1] = String("[cases= - 1](case1, case2, ...) [registers= - 1](reg1, reg2, ...)  [meter= - 1]{Totals* | SystemMeter | metername}" "Plots yearly curves for specified cases and registers. ") + CRLF
			+ "Default: meter=Totals. Example: "
			+ CRLF
			+ CRLF
			+ "yearlycurves cases=(basecase, pvgens) registers=9";
		CommandHelp[72 - 1] = String("Change default directory to specified directory") + CRLF
			+ CRLF
			+ "CD dirname";
		CommandHelp[73 - 1] = "[What= - 1] one of {Currents* | Voltages | Powers} [element= - 1]full_element_name  (class.name). "
			"Shows the selected quantity for selected element on a multiphase line drawing in phasor values.";
		CommandHelp[74 - 1] = "Close all DI files ... useful at end of yearly solution where DI files are left open. "
			"(Reset and Set Year=nnn will also close the DI files)";
#ifdef windows
		CommandHelp[75 - 1] = String("Do a DOS command. Sends the command \"cmd ... \" to Windows. Execute the \"cmd /?\" command " "in a DOS window to see the options. To do a DOS command and automatically exit, do ") + CRLF
			+ CRLF
			+ "DOScmd /c ...command string ..."
			+ CRLF
			+ CRLF
			+ "To keep the DOS window open, use /k switch.";
#else
		CommandHelp[74] = String("Execute a shell command. Sends the command \"cmd ... \" to /bin/sh");
#endif
		CommandHelp[76 - 1] = "Execute state estimator on present circuit given present sensor values.";
		CommandHelp[77 - 1] = String("Reconductor a line section. Must be in an EnergyMeter zone. ") + CRLF
			+ "Syntax: Reconductor Line1=... Line2=... {LineCode= | Geometry = } EditString=\"...\" NPhases=#"
			+ CRLF
			+ "Line1 and Line2 may be given in any order. All lines in the path between the two are redefined "
			+ "with either the LineCode or Geometry (not both). You may also add an optional string the alter any other line properties. "
			+ "The edit string should be enclosed in quotes or parens or brackets."
			+ CRLF
			+ "Nphases is an optional filter on the number of phases in line segments to change.";
		CommandHelp[78 - 1] = "For step control of solution process: Intialize iteration counters, etc. that normally occurs at the "
			"start of a snapshot solution process.";
		CommandHelp[79 - 1] = "For step control of solution process: Solves the circuit in present state but does not check for control actions.";
		CommandHelp[80 - 1] = "For step control of solution process: Sample the control elements, which push control action requests onto the control queue.";
		CommandHelp[81 - 1] = "For step control of solution process: Pops control actions off the control queue according to the present control mode rules. "
			"Dispatches contol actions to proper control element \"DoPendingAction\" handlers.";
		CommandHelp[82 - 1] = "For step control of solution process: Show the present control queue contents.";
		CommandHelp[83 - 1] = "For step control of solution process: Invoke direct solution function in DSS. Non-iterative solution of Y matrix and active sources only.";
		CommandHelp[84 - 1] = "For step control of solution process: Invoke iterative power flow solution function of DSS directly.";
		CommandHelp[85 - 1] = String("Add a marker to a bus in a circuit plot. Markers must be added before issuing the Plot command. Effect is persistent until circuit is cleared. " "See also ClearBusMarkers command. Example: ") + CRLF
			+ CRLF
			+ "ClearBusMarkers    !...Clears any previous bus markers"
			+ CRLF
			+ "AddBusMarker Bus=Mybusname code=5 color=Red size=3"
			+ CRLF
			+ CRLF
			+ "You can use any of the standard color names  or RGB numbers. See Help on C1 property in Plot command.";
		CommandHelp[86 - 1] = "Read UUIDs (v4) for class names and other CIM objects. Tab or comma-delimited file with full object name (or key) and UUID. Side effect is to start a new UUID list for the Export CIM100 command; the UUID list is freed after the Export UUIDs command.";
		CommandHelp[87 - 1] = "Set load and generator object kv to agree with the bus they are connected to using the bus voltage base and connection type.";
		CommandHelp[88 - 1] = String("Convert all Loadshapes presently loaded into either files of single or files of double. " "Usually files of singles are adequate precision for loadshapes.  Syntax:") + CRLF
			+ CRLF
			+ "cvrtloadshapes type=sng  (this is the default)"
			+ CRLF
			+ "cvrtloadshapes type=dbl"
			+ CRLF
			+ CRLF
			+ "A DSS script for loading the loadshapes from the created files is produced and displayed in the default editor. ";
		CommandHelp[89 - 1] = String("Global result is set to voltage difference, volts and degrees, (Node1 - Node2) between any two nodes. Syntax:") + CRLF
			+ CRLF
			+ "   NodeDiff Node1=MyBus.1 Node2=MyOtherBus.1";
		CommandHelp[90 - 1] = String("Generates a script to change the phase designation of all lines downstream from a start in line. Useful for such things as moving a single-phase " "lateral from one phase to another and keep the phase designation consistent for reporting functions that need it to be " "(not required for simply solving). ") + CRLF
			+ CRLF
			+ "StartLine=... PhaseDesignation=\"...\"  EditString=\"...\" ScriptFileName=... StopAtTransformers=Y/N/T/F"
			+ CRLF
			+ CRLF
			+ "Enclose the PhaseDesignation in quotes since it contains periods (dots)."
			+ CRLF
			+ "You may add and optional EditString to edit any other line properties."
			+ CRLF
			+ CRLF
			+ "Rephase StartLine=Line.L100  PhaseDesignation=\".2\"  EditString=\"phases=1\" ScriptFile=Myphasechangefile.DSS  Stop=No";
		CommandHelp[91 - 1] = "Bus=...  X=...  Y=... Set the X, Y coordinates for a single bus. Prerequisite: Bus must exist as a result of a Solve, CalcVoltageBases, or MakeBusList command.";
		CommandHelp[92 - 1] = "Update Storage elements based on present solution and time interval. ";
		CommandHelp[93 - 1] = "Change Bus and circuit element names to generic values to remove identifying names. Generally, "
			"you will follow this command immediately by a \"Save Circuit Dir=MyDirName\" command.";
		CommandHelp[94 - 1] = String("Define x,y coordinates for buses using Latitude and Longitude values (decimal numbers).  Similar to BusCoords command. " "Execute after Solve command or MakeBusList command is executed so that bus lists are defined." "Reads coordinates from a CSV file with records of the form: busname, Latitude, Longitude.") + CRLF
			+ CRLF
			+ "Example: LatLongCoords [-1]xxxx.csv"
			+ CRLF
			+ CRLF
			+ "Note: Longitude is mapped to x coordinate and Latitude is mapped to y coordinate.";
		CommandHelp[95 - 1] = String("Batch edit objects in the same class. Example: BatchEdit Load..* duty=duty_shape") + CRLF
			+ "In place of the object name, supply a PERL regular expression. .* matches all names."
			+ CRLF
			+ "The subsequent parameter string is applied to each object selected.";
		CommandHelp[96 - 1] = String("Pst calculation. PstCalc Npts=nnn Voltages=[array - 1] dt=nnn freq=nn lamp=120 or 230.") + CRLF
			+ "Set Npts to a big enough value to hold the incoming voltage array. "
			+ CRLF
			+ "dt = time increment in seconds. default is 1"
			+ CRLF
			+ "freq = base frequency in Hz 50 or 60. Default is default base frequency"
			+ CRLF
			+ "Lamp= 120 for North America; 230 for Europe. Default is 120"
			+ CRLF
			+ CRLF
			+ "PSTCalc Npts=1900 V=[-1] dt=1 freq=60 lamp=120";
		CommandHelp[97 - 1] = String("[name= - 1] MyVariableName  [Index= - 1] IndexofMyVariable ") + CRLF
			+ CRLF
			+ "Returns the value of the specified state variable of the active circuit element, if a PCelement. "
			+ "Returns the value as a string in the Result window or the Text.Result interface if using the COM server. "
			+ CRLF
			+ CRLF
			+ "You may specify the variable by name or by its index. You can determine the index using the VarNames command. "
			+ "If any part of the request is invalid, the Result is null.";
		CommandHelp[98 - 1] = "Forces reprocessing of bus definitions whether there has been a change or not. Use for rebuilding meter zone lists "
			"when a line length changes, for example or some other event that would not normally trigger an update to the bus list.";
		CommandHelp[99 - 1] = "Clear all bus markers created with the AddBusMarker command.";
		CommandHelp[100 - 1] = String("[restore=Y/N - 1]Perform reliability calcs: Failure rates and number of interruptions. ") + CRLF
			+ CRLF
			+ "Optional parameter:"
			+ CRLF
			+ CRLF
			+ "If restore=y automatic restoration of unfaulted section is assumed.";
		CommandHelp[101 - 1] = String("Define and view script variables.  Variable names begin with \"@\"") + CRLF
			+ CRLF
			+ "Usage:"
			+ CRLF
			+ CRLF
			+ "var @varname1=values  @varname2=value2    ..."
			+ CRLF
			+ "var @varname1  (shows the value of @varname1)"
			+ CRLF
			+ "var            (displays all variabiles and values)"
			+ CRLF
			+ CRLF
			+ "Example of using a variable:"
			+ CRLF
			+ CRLF
			+ "FileEdit @LastFile";
		CommandHelp[102 - 1] = "Force execution of the end-of-time-step cleanup functions that samples/saves meters and updates selected state variables such as storage level";
		CommandHelp[103 - 1] = "Do Cleanup, sample monitors, and increment time.";
		CommandHelp[104 - 1] = String("[Circuit element name - 1] (Optional) Returns a list of node numbers for all conductors of all terminals of the active circuit element in the Result window or interface." "If the optional circuit element name is supplied, the program makes it the active element. Usage:") + CRLF
			+ CRLF
			+ "NodeList"
			+ CRLF
			+ "NodeList Line.Myline";
		CommandHelp[105 - 1] = "This command creates a new actor (OpenDSS Instance) and sets the new actor as the active actor. "
			"There can be only 1 circuit per actor. The NewActor command will increment the variable NumOfActors;"
			" however, if the number of actors is the same as the number of available CPUs the new actor will not be created "
			"generating an error message. This instruction will deliver the ID of the active actor. This command does not requires a precedent command.";
		CommandHelp[106 - 1] = "Clears all the circuits and all the actors, after this instruction there will be only 1 actor (actor 1) and will be the active actor";
		CommandHelp[107 - 1] = "Pauses the scripting thread until all the active actors are Ready to receive new commands (have finished all their tasks and are ready to receive new simulation orders).";
		CommandHelp[108 - 1] = "Solves all the circuits (Actors) loaded into memory by the user";
		CommandHelp[109 - 1] = "Calculates the incidence matrix of the Active Circuit";
		CommandHelp[110 - 1] = "Calculates the incidence matrix of the Active Circuit. However, in this case the matrix will be calculated considering its hierarchical order,"
			"listing the buses starting from the substation to the farthest load in the mode";
		CommandHelp[111 - 1] = "Estimates the buses for tearing the system in many parts as CPUs - 1 are in the local computer, is used for tearing the interconnected circuit into a"
			" balanced (same number of nodes) collection of subsystems for the A-Diakoptics algorithm";
		CommandHelp[112 - 1] = "Request to create a TCP/IP socket to communicate data with external modules. This function requires the host address and TCP port to connect.";
		CommandHelp[113 - 1] = "Request to terminate a TCP/IP socket. This function requires the host address and TCP port to disconnect.";
		CommandHelp[114 - 1] = "This function takes the bus levels array and traces all the possible paths considering the longest paths from the substation to the farthest branches"
			" within the circuit. Then, the new paths are filled with 0 to complement the oroginal levels proposed by the calcincmatrix_o command.";
		CommandHelp[115 - 1] = String("{ElementName=} [KeepLoad=Y*/N - 1] [EditString=\"...\" - 1] " "Remove (disable) all branches downline from the PDelement named by \"ElementName\" property. Circuit must have an Energymeter on this branch. " "If KeepLoad=Y (default) a new Load element is defined and kW, kvar set to " "present power flow solution for the first element eliminated. " "The EditString is applied to each new Load element defined. ") + CRLF
			+ "If KeepLoad=N, all downline elements are disabled. Examples: "
			+ CRLF
			+ CRLF
			+ "Remove Line.Lin3021"
			+ CRLF
			+ "Remove Line.L22 Editstring=\"Daily=Dailycurve Duty=SolarShape"
			+ CRLF
			+ "Remove Line.L333 KeepLoad=No";
		CommandHelp[116 - 1] = "Aborts all the simulations running";
		CommandHelp[117 - 1] = "Calculate the laplacian matrix using the incidence matrix "
			"previously calculated. Before calling this command "
			"the incidence matrix needs to be calculated using calcincmatrix/calcincmatrix_o.";
		CommandHelp[118 - 1] = "Clones the active circuit. This command creates as many copies of the active cirucit as indicated in the argument "
			"if the number of requested clones does not overpasses the number of local CPUs. The form of this command is clone X where"
			"X is the number of clones to be created";
		CommandHelp[119 - 1] = "Read FNCS publication topics from a JSON file";
		//CommandHelp[119] := 'Update Storage2 elements based on present solution and time interval. ';
		CommandHelp[120 - 1] = "Exports the overloads report with the content avaiable at the moment of the call. It only affects the overloads report for the active actor.";
		CommandHelp[121 - 1] = "Exports the voltage violations1 report with the content avaiable at the moment of the call. It only affects the voltage violations report for the active actor.";
		CommandHelp[122 - 1] = "Returns symmetrical component short circuit impedances Z0, Z1, and Z2 for the ACTIVE 3-PHASE BUS. Determined from Zsc matrix.";
		CommandHelp[123 - 1] = String("Aggregates the load shapes in the model using the number of zones given in the argument.") + CRLF
			+ "Use this command when the number of load shapes is considerably big, this algorithm will simplify"
			+ CRLF
			+ "the amount of load shapes in order to make the memory consumption lower for the model."
			+ CRLF
			+ "The output of this algorithm is a script describing the new load shapes and their application into loads across the model."
			+ CRLF
			+ "The argument on this command can be: Actual/pu to define the units in which the load profiles are."
			+ CRLF
			+ "Check the OpenDSS user manual for details";
		CommandHelp[124 - 1] = String("Brings back the names of all PCE connected to the bus specified in the argument.") + CRLF
			+ "The command goes as follows:"
			+ CRLF
			+ CRLF
			+ "AllPCEatBus myBus"
			+ CRLF
			+ CRLF
			+ "Where \"myBus\" is the name of the bus of interest";
		CommandHelp[125 - 1] = String("Brings back the names of all PDE connected to the bus specified in the argument.") + CRLF
			+ "The command goes as follows:"
			+ CRLF
			+ CRLF
			+ "AllPDEatBus myBus"
			+ CRLF
			+ CRLF
			+ "Where \"myBus\" is the name of the bus of interest";
		CommandHelp[126 - 1] = "Returns the total powers (complex) at ALL terminals of the active circuit element in the Result string. "
			"(See Select command.)"
			"Returned as comma-separated kW and kvar.";
#ifdef windows
		CommandHelp[127 - 1] = "Shows the documentation file for the COM interface."
#else
		CommandHelp[126] = "This command is not supported on this platform, but on MS Windows, this command would show the documentation file for the COM interface."
#endif
			"This file provides guidance on the properties and methods included in the COM interface as well as examples and tips. Use this file to learn more about the COM interface and its different interfaces or just as a reference guide.";
		CommandHelp[128 - 1] = "Executes GIS options working with OpenDSS-GIS. See GIS command help.";
		CommandHelp[129 - 1] = String("Define x,y coordinates for buses using real GIS Latitude and Longitude values (decimal numbers).  Similar to BusCoords command. " "Execute after Solve command or MakeBusList command is executed so that bus lists are defined." "Reads coordinates from a CSV file with records of the form: busname, Latitude, Longitude.") + CRLF
			+ CRLF
			+ "Example: GISCoords [-1]xxxx.csv"
			+ CRLF
			+ CRLF
			+ "Note: For using only if OpenDSS-GIS is locally installed.";
		//	Start list of INDUCES commands
		CommandHelp[130 - 1] = "Reads an HDF file with non uniform, time varying geoelectric fields used in EMP simulations";
		//	Ends list of INDUCES commands
	}

	//----------------------------------------------------------------------------

	void ProcessCommand(const string CmdLine)
	{
		int ParamPointer = 0;
		int Temp_Int = 0;
		int Iter = 0;
		int i = 0;
		String ParamName;
		String Param;
		String ObjName;
		String PropName;
		TEnergyMeterObj* MeterElem = nullptr;
		try
		{
			CmdResult = 0;
			ErrorNumber = 0;  // Reset Error number
			LastErrorMessage = "";
			GlobalResult = "";

	/*Load up the parser and process the first parameter only*/
			LastCmdLine = CmdLine;
			Parser[ActiveActor]->SetCmdString(LastCmdLine);  // Load up command parser
			LastCommandWasCompile = false;
			ParamPointer = 0;
			ParamName = Parser[ActiveActor]->GetNextParam();
			Param = Parser[ActiveActor]->MakeString_();
			if(Param.size() == 0)
				return;  // Skip blank line

	   // Check for Command verb or Property Value
	   // Commands do not have equal signs so ParamName must be zero
			if(ParamName.size() == 0)
				ParamPointer = CommandList.Getcommand(Param);

	   // Check first for Compile or Redirect and get outta here
			switch(ParamPointer)
			{
				case 	14:
				{
					/*# with DSSExecutive[ActiveActor] do */
					{
					
						if(DSSExecutive[ActiveActor]->get_FRecorderOn())
							DSSExecutive[ActiveActor]->Write_to_RecorderFile(CRLF + "!*********" + CmdLine);
					}
					CmdResult = DoRedirect(true);
					return;
				}//'Compile';
				case 	20:
				{
					/*# with DSSExecutive[ActiveActor] do */
					{
					
						if(DSSExecutive[ActiveActor]->get_FRecorderOn())
							DSSExecutive[ActiveActor]->Write_to_RecorderFile(CRLF + "!*********" + CmdLine);
					}
					CmdResult = DoRedirect(false);
					return;
				} //'Redirect';
	   // Write everything direct to recorder, if ON
				default:
				/*# with DSSExecutive[ActiveActor] do */
				{
				
					if(DSSExecutive[ActiveActor]->get_FRecorderOn())
						DSSExecutive[ActiveActor]->Write_to_RecorderFile(CmdLine);
				}
				break;
			}

	   // Things that are OK to do before a circuit is defined
			switch(ParamPointer)
			{
				case 	1:
				CmdResult = DoNewCmd();
				break; // new
				case 	15:
				if(!ASSIGNED(ActiveCircuit[ActiveActor]))
				{
					DoSetCmd_NoCircuit(); // can only call this if no circuit active
					return;
				}
				break; /*Do Nothing - comment*/
				case 	19:
				;
				break;
				case 	21:
				CmdResult = DoHelpCmd();
				break;
				case 	22:
				if(!IsDLL)
					ExitControlPanel();
				break;  // Quit in Stand alone version
				case 	25:
				ShowControlPanel();
				break; // DSSForms
				case 	27:
				DoClearCmd();
				break;
				case 	28:
				DoAboutBox();
				break;
				case 	32:
				if(!ASSIGNED(ActiveCircuit[ActiveActor]))
				{
					DoGetCmd_NoCircuit(); // can only call this if no circuit active
					return;
				}
				break;
				case 	35:
				CmdResult = DoFileEditCmd();
				break;
				case 	49:
				CmdResult = DoClassesCmd();
				break;
				case 	50:
				CmdResult = DoUserClassesCmd();
				break;
				case 	63:
				CmdResult = DoAlignFileCmd();
				break;
				case 	69:
				CmdResult = DoDI_PlotCmd();
				break;
				case 	70:
				CmdResult = DoCompareCasesCmd();
				break;
				case 	71:
				CmdResult = DoYearlyCurvesCmd();
				break;
				case 	72:
				{
					ParamName = Parser[ActiveActor]->GetNextParam();
					Param = Parser[ActiveActor]->MakeString_();
					if(SetCurrentDir(Param))
					{
						CmdResult = 0;
						SetDataPath(Param);  // change datadirectory
					}
					else
					DoSimpleMsg(String("Directory \"") + Param + "\" not found.", 282);
				}
				break;
				case 	75:
				DoADOScmd();
				break;
				case 	88:
				DoCvrtLoadshapesCmd();
				break;
				case 	101:
				DoVarCmd();
				break;
				case 	105:
				{
					New_Actor_Slot();
				}
				break;
				case 	106:
				DoClearAllCmd();
				break;
				case 	107:
				{
					if(Parallel_enabled)
						Wait4Actors(0);
				}
				break;
				  //  Added to avoid crashes when in A-Diakoptics mode but the user
				  //  uses the SolveAll command
				case 	108:
				{
					int stop = 0;
					if(ADiakoptics)
						Iter = 1;
					else
						Iter = NumOfActors;
				  //  Execution area
					for(stop = Iter, i = 1; i <= stop; i++)
					{
						ActiveActor = i;
						CmdResult = DoSetCmd(1);
					}
				}
				break;
				case 	109:
				{
					ActiveCircuit[ActiveActor]->Solution->Calc_Inc_Matrix(ActiveActor);
				}
				break;
				case 	110:
				{
					ActiveCircuit[ActiveActor]->Solution->Calc_Inc_Matrix_Org(ActiveActor);
				}
				break;
				case 	111:
				{
					ADiakoptics_Tearing(false);
				}
				break;
				case 	114:
				{
					ActiveCircuit[ActiveActor]->Get_paths_4_Coverage();
					Temp_Int =(ActiveCircuit[ActiveActor]->Path_Idx.size()) - 1;
					GlobalResult = IntToStr(Temp_Int) + " new paths detected";
				}
				break;
				case 	117:
				{
					/*# with ActiveCircuit[ActiveActor]->Solution do */
					{
						auto with3 = ActiveCircuit[ActiveActor]->Solution;
						with3->Laplacian = with3->IncMat.Transpose();          // Transposes the Incidence Matrix
						with3->Laplacian = with3->Laplacian.multiply( &(with3->IncMat) );  // IncMatT*IncMat
					}
				}
				break;
				case 	127:
				{
#ifdef windows
					Show_COM_Help();
#endif
				}
				break;
//#ifndef windows								//	By making these calls dependent on the OS, we should be able to compile OpenDSS-X by itself 
				case 130:
				{
					int a = 0;
					int ret = ReadEfieldHDF(a);		//	Read Efield HDF file and populate internal data structures with file contents
					if (!NoFormsAllowed)
						cout << CmdLine;
				}									
				break;
//#endif	
				default:
				if(ActiveCircuit[ActiveActor] == nullptr)
				{
					DoSimpleMsg("You must create a new circuit object first: \"new circuit.mycktname\" to execute this command.", 301);
					return;
				}
				break;
			}

	   // Now check to see if this is a command or a property reference
			if(ParamPointer == 0)
		 /*If not a command or the command is unknown, THEN it could be a property of a circuit element*/

		   /*If a command or no text beFORe the = sign, THEN error*/
			{
				if((ParamName.size() == 0) || (CompareText(ParamName, "command") == 0))
				{
					DoSimpleMsg(String("Unknown Command: \"") + Param
				   + "\" "
				   + CRLF
				   + Parser[ActiveActor]->get_CmdBuffer(), 302);
					CmdResult = 1;
				}
				else
				{
					ParseObjName(ParamName, ObjName, PropName);
					if(ObjName.size() > 0)
						SetObject(ObjName);  // Set active element
					if(ActiveDSSObject[ActiveActor] != nullptr)
				 // rebuild command line and pass to editor
				 // use quotes to ensure first parameter is interpreted OK after rebuild
					{
						Parser[ActiveActor]->SetCmdString(PropName + "=\"" + Param + "\" " + Parser[ActiveActor]->Get_Remainder());
						ActiveDSSClass[ActiveActor]->Edit(ActiveActor);
					}
				}
				return;
			}

	   // Process the rest of the commands
			switch(ParamPointer)
			{
				case 	2:
				CmdResult = DoEditCmd();
				break; // edit
				case 3: case 4: case 5:
				CmdResult = DoMoreCmd();
				break; // more , m, ~
				case 	6:
				CmdResult = DoSelectCmd();
				break;
				case 	7:
				CmdResult = DoSaveCmd();
				break; //'save';
				case 	8:
				CmdResult = DoShowCmd();
				break; //'show';
				case 	9:
				{
					if(ADiakoptics)
						ActiveActor = 1;   // Just in case
					CmdResult = DoSetCmd(1);  // changed from DoSolveCmd; //'solve';
				}
				break;
				case 	10:
				CmdResult = DoEnableCmd();
				break;
				case 	11:
				CmdResult = DoDisableCmd();
				break;
				case 	12:
				CmdResult = DoPlotCmd();
				break; //'plot';
				case 	13:
				CmdResult = DoResetCmd(ActiveActor);
				break; //'resetmonitors';
				case 	15:
				CmdResult = DoSetCmd(0);
				break;  //'set WITH no solve'
				case 	16:
				CmdResult = DoPropertyDump();
				break;
				case 	17:
				CmdResult = DoOpenCmd();
				break;
				case 	18:
				CmdResult = DoCloseCmd();
				break;
				case 	23:
				CmdResult = DoQueryCmd();
				break;
				case 	24:
				CmdResult = DoNextCmd();
				break;  // Advances time
					   /*25: ControlPanel.Show -- see above */
				case 	26:
				CmdResult = DoSampleCmd(ActiveActor);
				break;
		   /*27: Begin ClearAllCircuits; DisposeDSSClasses; CreateDSSClasses; End;*/
		   /*28: DoAboutBox; */
				case 	29:
				CmdResult = DoSetVoltageBases(ActiveActor);
				break;
				case 	30:
				CmdResult = DoSetkVBase();
				break;
				case 	31:
				ActiveCircuit[ActiveActor]->InvalidateAllPCElements();
				break;  // FORce rebuilding of Y
				case 	32:
				CmdResult = DoGetCmd();
				break;
				case 	33:
				ActiveCircuit[ActiveActor]->Solution->SolutionInitialized = false;
				break;
				case 	34:
				CmdResult = DoExportCmd();
				break;
		   /*35: CmdResult := DoFileEditCmd;*/
				case 	36:
				CmdResult = DovoltagesCmd(false);
				break;
				case 	37:
				CmdResult = DocurrentsCmd();
				break;
				case 	38:
				CmdResult = DopowersCmd(0);
				break;
				case 	39:
				CmdResult = DoseqvoltagesCmd();
				break;
				case 	40:
				CmdResult = DoseqcurrentsCmd();
				break;
				case 	41:
				CmdResult = DoseqpowersCmd();
				break;
				case 	42:
				CmdResult = DolossesCmd();
				break;
				case 	43:
				CmdResult = DophaselossesCmd();
				break;
				case 	44:
				CmdResult = DocktlossesCmd();
				break;
				case 	45:
				CmdResult = DoAllocateLoadsCmd(ActiveActor);
				break;
				case 	46:
				CmdResult = DoFormEditCmd();
				break;
				case 	47:
				CmdResult = DoMeterTotals();
				break;
				case 	48:
				CmdResult = DoCapacityCmd();
				break;
	//       49: CmdResult := DoClassesCmd;
	//       50: CmdResult := DoUserClassesCmd;
				case 	51:
				CmdResult = DoZscCmd(true);
				break;
				case 	52:
				CmdResult = DoZsc10Cmd();
				break;
				case 	53:
				CmdResult = DoZscRefresh(ActiveActor);
				break;
				case 	54:
				CmdResult = DoZscCmd(false);
				break;
				case 	55:
				CmdResult = DovoltagesCmd(true);
				break;
				case 	56:
				CmdResult = DoVarValuesCmd();
				break;
				case 	57:
				CmdResult = DoVarNamesCmd();
				break;
				case 	58:
				CmdResult = DoBusCoordsCmd(false, 0);
				break;
				case 	59:
				/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with40 = ActiveCircuit[ActiveActor];
					if(with40->get_FBusNameRedefined())
						with40->ReProcessBusDefs(ActiveActor);
				}
				break;
				case 	60:
				CmdResult = DoMakePosSeq();
				break;
				case 	61:
				CmdResult = DoReduceCmd();
				break;
				case 	62:
				CmdResult = DoInterpolateCmd();
				break;
				case 	64:
				CmdResult = DoTOPCmd();
				break;
				case 	65:
				CmdResult = DoRotateCmd();
				break;
				case 	66:
				CmdResult = DoVDiffCmd();
				break;
				case 	67:
				CmdResult = DoSummaryCmd();
				break;
				case 	68:
				CmdResult = DoDistributeCmd();
				break;
	//      69;
	//      70;
	//      71;
	//      72;
				case 	73:
				CmdResult = DoVisualizeCmd();
				break;
				case 	74:
				CmdResult = DoCloseDICmd();
				break;
				case 	76:
				CmdResult = DoEstimateCmd();
				break;
				case 	77:
				CmdResult = DoReconductorCmd();
				break;
		   /*Step solution commands*/
				case 	78:
				ActiveCircuit[ActiveActor]->Solution->SnapShotInit(ActiveActor);
				break;
				case 	79:
				{
					ActiveCircuit[ActiveActor]->Solution->SolveCircuit(ActiveActor);
				}
				break;
				case 	80:
				ActiveCircuit[ActiveActor]->Solution->SampleControlDevices(ActiveActor);
				break;
				case 	81:
				ActiveCircuit[ActiveActor]->Solution->DoControlActions(ActiveActor);
				break;
				case 	82:
				ActiveCircuit[ActiveActor]->ControlQueue.ShowQueue(DSSDirectory + CircuitName_[ActiveActor] + "ControlQueue.csv");
				break;
				case 	83:
				{
					ActiveCircuit[ActiveActor]->Solution->SolveDirect(ActiveActor);
				}
				break;
				case 	84:
				{
					ActiveCircuit[ActiveActor]->Solution->DoPFLOWsolution(ActiveActor);
				}
				break;
				case 	85:
				CmdResult = DoAddMarkerCmd();
				break;
				case 	86:
				CmdResult = DoUuidsCmd();
				break;
				case 	87:
				CmdResult = DoSetLoadAndGenKVCmd();
				break;
	//       88:;
				case 	89:
				CmdResult = DoNodeDiffCmd();
				break;
				case 	90:
				CmdResult = DoRephaseCmd();
				break;
				case 	91:
				CmdResult = DoSetBusXYCmd();
				break;
				case 	92:
				CmdResult = DoUpdateStorageCmd();
				break;
				case 	93:
				Obfuscate();
				break;
				case 	94:
				CmdResult = DoBusCoordsCmd(true, 0);
				break;   // swaps X and Y
				case 	95:
				CmdResult = DoBatchEditCmd();
				break;
				case 	96:
				CmdResult = DoPstCalc();
				break;
				case 	97:
				CmdResult = DoValVarCmd();
				break;
				case 	98:
				ActiveCircuit[ActiveActor]->ReProcessBusDefs(ActiveActor);
				break;
				case 	99:
				ActiveCircuit[ActiveActor]->ClearBusMarkers();
				break;
				case 	100:
				CmdResult = DoLambdaCalcs();
				break;   // Option: Assume Restoration
				case 	102:
				EndOfTimeStepCleanup(ActiveActor);
				break;
				case 	103:
				FinishTimeStep(ActiveActor);
				break;
				case 	104:
				CmdResult = DoNodeListCmd();
				break;
				case 	112:
				CmdResult = DoConnectCmd();
				break; //'TCP/IP connect';
				case 	113:
				CmdResult = DoDisConnectCmd();
				break; //'TCP/IP disconnect';';
				case 	115:
				DoRemoveCmd();
				break;
				case 	116:
				SolutionAbort = true;
				break;
				case 	118:
				DoClone();
				break;
				case 	119:
				DoFNCSPubCmd();
				break;
	 //   120: CmdResult := DoUpDateStorage2Cmd;
				case 	120:
				{
					if(OV_MHandle[ActiveActor] != nullptr)
						CloseMHandler(OV_MHandle[ActiveActor], EnergyMeterClass[ActiveActor]->DI_Dir + DIRSEP_STR "DI_Overloads_" + IntToStr(ActiveActor) + ".CSV", OV_Append[ActiveActor]);
				}
				break;
				case 	121:
				{
					if(VR_MHandle[ActiveActor] != nullptr)
						CloseMHandler(VR_MHandle[ActiveActor], EnergyMeterClass[ActiveActor]->DI_Dir + DIRSEP_STR "DI_VoltExceptions_" + IntToStr(ActiveActor) + ".CSV", VR_Append[ActiveActor]);
				}
				break;
				case 	122:
				CmdResult = DoZsc012Cmd();
				break; // Get full symmetrical component transformation of Zsc
				case 	123:
				{
					dummy = Parser[ActiveActor]->GetNextParam();
					ActiveCircuit[ActiveActor]->AggregateProfiles(Parser[ActiveActor]->MakeString_());
				}
				break;
				case 	124:
				{
					dummy = Parser[ActiveActor]->GetNextParam();
					GlobalResult = ActiveCircuit[ActiveActor]->ReportPCEatBus(Parser[ActiveActor]->MakeString_());
				}
				break;
				case 	125:
				{
					dummy = Parser[ActiveActor]->GetNextParam();
					GlobalResult = ActiveCircuit[ActiveActor]->ReportPDEatBus(Parser[ActiveActor]->MakeString_());
				}
				break;
				case 	126:
				CmdResult = DopowersCmd(1);
				break;
				case 	128:
				GlobalResult = DoGISCmd();
				break;
				case 	129:
				CmdResult = DoBusCoordsCmd(false, 1);
				break;   // GIS coordinates

		   // Ignore excess parameters
				default:
				  ;
				break;
			}
		}
		catch (std::exception &e)
		{
			DoErrorMsg((String(String("ProcessCommand") + CRLF + "Exception Raised While Processing DSS Command:") + CRLF + Parser[ActiveActor]->get_CmdBuffer()), (std::string) e.what(), "Error in command string or circuit data.", 303);
		}

	//  if ActorStatus[ActiveActor] = 1 then
	//    ParserVars.Add('@result', GlobalResult)
	}

	void DisposeStrings()
	{
		delete[] ExecCommand;
		delete[] CommandHelp;
	}


	void ExecCommands_initialization()
	{
		DefineCommands();
	}

	void ExecCommands_finalization()
	{
		DisposeStrings();
	}

	class 		ExecCommands_unit
	{
	public:
	ExecCommands_unit()
	{
		//AssertSystemInitialization();
		ExecCommands_initialization();
	}
	~		ExecCommands_unit(){ExecCommands_finalization(); }
	};
	ExecCommands_unit _ExecCommands_unit;

}  // namespace ExecCommands




