

#pragma hdrstop

#include "ExecOptions.h"
#include "Utilities.h"



using namespace std;
using namespace Command;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace EnergyMeter;
using namespace ExecHelper;
using namespace Executive;
using namespace LoadShape;
using namespace ParserDel;
using namespace Solution;
using namespace System;
using namespace Utilities;

namespace ExecOptions
{

	TCommandList* OptionList = nullptr;
	std::string* ExecOption;
	std::string* OptionHelp;

	void DefineOptions()
	{
		ExecOption = new string[NumExecOptions];
		OptionHelp = new string [NumExecOptions];

		ExecOption[1 - 1] = "type";
		ExecOption[2 - 1] = "element";
		ExecOption[3 - 1] = "hour";
		ExecOption[4 - 1] = "sec";
		ExecOption[5 - 1] = "year";
		ExecOption[6 - 1] = "frequency";
		ExecOption[7 - 1] = "stepsize";
		ExecOption[8 - 1] = "mode";
		ExecOption[9 - 1] = "random";
		ExecOption[10 - 1] = "number";
		ExecOption[11 - 1] = "time";
		ExecOption[12 - 1] = "class";
		ExecOption[13 - 1] = "object";
		ExecOption[14 - 1] = "circuit";
		ExecOption[15 - 1] = "editor";
		ExecOption[16 - 1] = "tolerance";
		ExecOption[17 - 1] = "maxiterations";
		ExecOption[18 - 1] = "h";
		ExecOption[19 - 1] = "Loadmodel";
		ExecOption[20 - 1] = "Loadmult";
		ExecOption[21 - 1] = "normvminpu";
		ExecOption[22 - 1] = "normvmaxpu";
		ExecOption[23 - 1] = "emergvminpu";
		ExecOption[24 - 1] = "emergvmaxpu";
		ExecOption[25 - 1] = "%mean";
		ExecOption[26 - 1] = "%stddev";
		ExecOption[27 - 1] = "LDCurve";  // Load Duration Curve
		ExecOption[28 - 1] = "%growth";  // default growth rate
		ExecOption[29 - 1] = "Genkw";
		ExecOption[30 - 1] = "Genpf";
		ExecOption[31 - 1] = "CapkVAR";
		ExecOption[32 - 1] = "Addtype";
		ExecOption[33 - 1] = "Allowduplicates";
		ExecOption[34 - 1] = "Zonelock";
		ExecOption[35 - 1] = "UEweight";
		ExecOption[36 - 1] = "Lossweight";
		ExecOption[37 - 1] = "UEregs";
		ExecOption[38 - 1] = "Lossregs";
		ExecOption[39 - 1] = "Voltagebases";  //  changes the default voltage base rules
		ExecOption[40 - 1] = "Algorithm";  //  changes the default voltage base rules
		ExecOption[41 - 1] = "Trapezoidal";
		ExecOption[42 - 1] = "Autobuslist";  // array of bus names to include in auto& add solutions
		ExecOption[43 - 1] = "Controlmode";
		ExecOption[44 - 1] = "Tracecontrol";
		ExecOption[45 - 1] = "Genmult";
		ExecOption[46 - 1] = "Defaultdaily";
		ExecOption[47 - 1] = "Defaultyearly";
		ExecOption[48 - 1] = "Allocationfactors";
		ExecOption[49 - 1] = "Cktmodel";
		ExecOption[50 - 1] = "Pricesignal";
		ExecOption[51 - 1] = "Pricecurve";
		ExecOption[52 - 1] = "Terminal";
		ExecOption[53 - 1] = "Basefrequency";
		ExecOption[54 - 1] = "Harmonics";
		ExecOption[55 - 1] = "Maxcontroliter";
		ExecOption[56 - 1] = "Bus";
		ExecOption[57 - 1] = "Datapath";
		ExecOption[58 - 1] = "KeepList";
		ExecOption[59 - 1] = "ReduceOption";
		ExecOption[60 - 1] = "DemandInterval";
		ExecOption[61 - 1] = "%Normal";
		ExecOption[62 - 1] = "DIVerbose";
		ExecOption[63 - 1] = "Casename";
		ExecOption[64 - 1] = "Markercode";
		ExecOption[65 - 1] = "Nodewidth";
		ExecOption[66 - 1] = "Log";
		ExecOption[67 - 1] = "Recorder";
		ExecOption[68 - 1] = "Overloadreport";
		ExecOption[69 - 1] = "Voltexceptionreport";
		ExecOption[70 - 1] = "Cfactors";
		ExecOption[71 - 1] = "Showexport";
		ExecOption[72 - 1] = "Numallociterations";
		ExecOption[73 - 1] = "DefaultBaseFrequency";
		ExecOption[74 - 1] = "Markswitches";
		ExecOption[75 - 1] = "Switchmarkercode";
		ExecOption[76 - 1] = "Daisysize";
		ExecOption[77 - 1] = "Marktransformers";
		ExecOption[78 - 1] = "TransMarkerCode";
		ExecOption[79 - 1] = "TransMarkerSize";
		ExecOption[80 - 1] = "LoadShapeClass";
		ExecOption[81 - 1] = "EarthModel";
		ExecOption[82 - 1] = "QueryLog";
		ExecOption[83 - 1] = "MarkCapacitors";
		ExecOption[84 - 1] = "MarkRegulators";
		ExecOption[85 - 1] = "MarkPVSystems";
		ExecOption[86 - 1] = "MarkStorage";
		ExecOption[87 - 1] = "CapMarkerCode";
		ExecOption[88 - 1] = "RegMarkerCode";
		ExecOption[89 - 1] = "PVMarkerCode";
		ExecOption[90 - 1] = "StoreMarkerCode";
		ExecOption[91 - 1] = "CapMarkerSize";
		ExecOption[92 - 1] = "RegMarkerSize";
		ExecOption[93 - 1] = "PVMarkerSize";
		ExecOption[94 - 1] = "StoreMarkerSize";
		ExecOption[95 - 1] = "NeglectLoadY";
		ExecOption[96 - 1] = "MarkFuses";
		ExecOption[97 - 1] = "FuseMarkerCode";
		ExecOption[98 - 1] = "FuseMarkerSize";
		ExecOption[99 - 1] = "MarkReclosers";
		ExecOption[100 - 1] = "RecloserMarkerCode";
		ExecOption[101 - 1] = "RecloserMarkerSize";
		ExecOption[102 - 1] = "RegistryUpdate";
		ExecOption[103 - 1] = "MarkRelays";
		ExecOption[104 - 1] = "RelayMarkerCode";
		ExecOption[105 - 1] = "RelayMarkerSize";
		ExecOption[106 - 1] = "ProcessTime";
		ExecOption[107 - 1] = "TotalTime";
		ExecOption[108 - 1] = "StepTime";
		ExecOption[109 - 1] = "NumCPUs";
		ExecOption[110 - 1] = "NumCores";
		ExecOption[111 - 1] = "NumActors";
		ExecOption[112 - 1] = "ActiveActor";
		ExecOption[113 - 1] = "CPU";
		ExecOption[114 - 1] = "ActorProgress";
		ExecOption[115 - 1] = "Parallel";
		ExecOption[116 - 1] = "ConcatenateReports";
		ExecOption[117 - 1] = "OpenDSSViewer";
		ExecOption[118 - 1] = "DSSVInstalled";
		ExecOption[119 - 1] = "Coverage";
		ExecOption[120 - 1] = "Num_SubCircuits";
		ExecOption[121 - 1] = "SampleEnergyMeters";
		ExecOption[122 - 1] = "ADiakoptics";
		ExecOption[123 - 1] = "MinIterations"; // default is 2
		ExecOption[124 - 1] = "LinkBranches";
		ExecOption[125 - 1] = "KeepLoad";
		ExecOption[126 - 1] = "Zmag";
		ExecOption[127 - 1] = "SeasonRating";
		ExecOption[128 - 1] = "SeasonSignal";
		ExecOption[129 - 1] = "NUMANodes";
		ExecOption[130 - 1] = "GISInstalled";
		ExecOption[131 - 1] = "GISCoords";
		ExecOption[132 - 1] = "GISColor";
		ExecOption[133 - 1] = "GISThickness";
		ExecOption[134 - 1] = "UseMyLinkBranches";
		ExecOption[135 - 1] = "LineTypes";
		ExecOption[136 - 1] = "EventLogDefault";
		ExecOption[137 - 1] = "LongLineCorrection";
		ExecOption[138 - 1] = "ShowReports";
		ExecOption[139 - 1] = "IgnoreGenQLimits";
		ExecOption[140 - 1] = "NCIMQGain";
        ExecOption[141 - 1] = "StateVar";

		 /*Deprecated
		  ExecOption[130 - 1] := 'MarkPVSystems2';
		  ExecOption[132 - 1] := 'MarkStorage2';
		 */
		OptionHelp[1 - 1] = "Sets the active DSS class type.  Same as Class=...";
		OptionHelp[2 - 1] = "Sets the active DSS element by name. You can use "
			"the complete object spec (class.name) or just the "
			"name.  if full name is specifed, class becomes the active "
			"class, also.";
		OptionHelp[3 - 1] = "Sets the hour used for the start time of the solution.";
		OptionHelp[4 - 1] = "Sets the seconds from the hour for the start time of the solution.";
		OptionHelp[5 - 1] = "Sets the Year (integer number) to be used for the solution. "
			"for certain solution types, this determines the growth multiplier.";
		OptionHelp[6 - 1] = "Sets the frequency for the solution of the active circuit.";
		OptionHelp[7 - 1] = String("Sets the time step size for the active circuit.  Default units are s. " "May also be specified in minutes or hours by appending \"m\" or \"h\" to the value. For example:") + CRLF
			+ CRLF
			+ "   stepsize=.25h "
			+ CRLF
			+ "  stepsize=15m"
			+ CRLF
			+ "  stepsize=900s";
		OptionHelp[8 - 1] = String("Set the solution Mode: One of") + CRLF
			+ "  Snapshot,"
			+ CRLF
			+ "  Daily,"
			+ CRLF
			+ "  Yearly (follow Yearly curve),"
			+ CRLF
			+ "  DIrect,"
			+ CRLF
			+ "  DUtycycle,"
			+ CRLF
			+ "  Time, ( see LoadShapeClass option)"
			+ CRLF
			+ "  DYnamic,  ( see LoadShapeClass option)"
			+ CRLF
			+ "  Harmonic,"
			+ CRLF
			+ "  HarmonicT,  (sequential Harmonic Mode)"
			+ CRLF
			+ "  M1 (Monte Carlo 1),"
			+ CRLF
			+ "  M2 (Monte Carlo 2),"
			+ CRLF
			+ "  M3 (Monte Carlo 3),"
			+ CRLF
			+ "  Faultstudy,"
			+ CRLF
			+ "  MF (monte carlo fault study)"
			+ CRLF
			+ "  Peakday,"
			+ CRLF
			+ "  LD1 (load-duration 1)"
			+ CRLF
			+ "  LD2 (load-duration 2)"
			+ CRLF
			+ "  AutoAdd (see AddType)"
			+ CRLF
			+ "  YearlyVQ (Yearly Vector Quantiaztion)"
			+ CRLF
			+ "  DutyVQ (Duty Vector Quantiaztion)"
			+ CRLF
			+ CRLF
			+ "Side effect: setting the Mode propergy resets all monitors and energy meters. It also "
			+ "resets the time step, etc. to defaults for each mode.  After the initial reset, the user "
			+ "must explicitly reset the monitors and/or meters until another Set Mode= command.";
		OptionHelp[9 - 1] = "One of [Uniform | Gaussian | Lognormal | None  - 1] for Monte Carlo Variables.";
		OptionHelp[10 - 1] = String("Number of solutions or time steps to perform for each Solve command. Defaults for selected modes: ") + CRLF
			+ CRLF
			+ "Daily = 24"
			+ CRLF
			+ "Yearly = 8760"
			+ CRLF
			+ "Duty = 100";
		OptionHelp[11 - 1] = String("Specify the solution start time as an array:") + CRLF
			+ "time=(hour, secs)";
		OptionHelp[12 - 1] = "Synonym for Type=. (See above)";
		OptionHelp[13 - 1] = "Synonym for Element=. (See above)";
		OptionHelp[14 - 1] = "Set the active circuit by name.";
		OptionHelp[15 - 1] = "Set the command string required to start up the editor preferred by the user. Does not require a circuit defined.";
		OptionHelp[16 - 1] = "Sets the solution tolerance.  Default is 0.0001.";
		OptionHelp[17 - 1] = "Sets the maximum allowable iterations for power flow solutions. Default is 15.";
		OptionHelp[18 - 1] = "Alternate name for time step size.";
		OptionHelp[19 - 1] = "{Powerflow | Admittance} depending on the type of solution you wish to perform. "
			"If admittance, a non-iterative, direct solution is done with all loads and generators modeled by their "
			"equivalent admittance.";
		OptionHelp[20 - 1] = "Global load multiplier for this circuit.  Does not affect loads "
			"designated to be \"fixed\".  All other base kW values are multiplied by this number. "
			"Defaults to 1.0 when the circuit is created. As with other values, it always stays "
			"at the last value to which it was set until changed again.";
		OptionHelp[21 - 1] = "Minimum permissible per unit voltage for normal conditions. Default is 0.95.";
		OptionHelp[22 - 1] = "Maximum permissible per unit voltage for normal conditions. Default is 1.05.";
		OptionHelp[23 - 1] = "Minimum permissible per unit voltage for emergency (contingency) conditions. Default is 0.90.";
		OptionHelp[24 - 1] = "Maximum permissible per unit voltage for emergency (contingency) conditions. Default is 1.08.";
		OptionHelp[25 - 1] = "Percent mean to use for global load multiplier. Default is 65%.";
		OptionHelp[26 - 1] = "Percent Standard deviation to use for global load multiplier. Default is 9%.";
		OptionHelp[27 - 16] = "Set Load-Duration Curve. Global load multiplier is defined by this curve for LD1 and LD2 solution modes. Default is Nil.";
		OptionHelp[28 - 1] = "Set default annual growth rate, percent, for loads with no growth curve specified. Default is 2.5.";
		OptionHelp[29 - 1] = "Size of generator, kW, to automatically add to system. Default is 1000.0";
		OptionHelp[30] = "Power factor of generator to assume for automatic addition. Default is 1.0.";
		OptionHelp[31 - 1] = "Size of capacitor, kVAR, to automatically add to system.  Default is 600.0.";
		OptionHelp[32 - 1] = "{Generator | Capacitor} Default is Generator. Type of device for AutoAdd Mode.";
		OptionHelp[33 - 1] = "{YES/TRUE | NO/FALSE}   Default is No. Flag to indicate if it is OK to have devices of same name in the same class. "
			"If No, then a New command is treated as an Edit command. "
			"If Yes, then a New command will always result in a device being added.";
		OptionHelp[34 - 1] = "{YES/TRUE | NO/FALSE}  Default is No. if No, then meter zones are recomputed each time there is a change in the circuit. "
			"If Yes, then meter zones are not recomputed unless they have not yet been computed. "
			"Meter zones are normally recomputed on Solve command following a circuit change.";
		OptionHelp[35 - 1] = String("Weighting factor for UE/EEN in AutoAdd functions.  Defaults to 1.0.") + CRLF
			+ CRLF
			+ "Autoadd mode minimizes"
			+ CRLF
			+ CRLF
			+ "(Lossweight * Losses + UEweight * UE). "
			+ CRLF
			+ CRLF
			+ "If you wish to ignore UE, set to 0. "
			+ "This applies only when there are EnergyMeter objects. "
			+ "Otherwise, AutoAdd mode minimizes total system losses.";
		OptionHelp[36 - 1] = String("Weighting factor for Losses in AutoAdd functions.  Defaults to 1.0.") + CRLF
			+ CRLF
			+ "Autoadd mode minimizes"
			+ CRLF
			+ CRLF
			+ "(Lossweight * Losses + UEweight * UE). "
			+ CRLF
			+ CRLF
			+ "If you wish to ignore Losses, set to 0. "
			+ "This applies only when there are EnergyMeter objects. "
			+ "Otherwise, AutoAdd mode minimizes total system losses.";
		OptionHelp[37 - 1] = String("Which EnergyMeter register(s) to use for UE in AutoAdd Mode. " "May be one or more registers.  if more than one, register values are summed together. " "Array of integer values > 0.  Defaults to 11 (for Load EEN). ") + CRLF
			+ CRLF
			+ "for a list of EnergyMeter register numbers, do the \"Show Meters\" command after defining a circuit.";
		OptionHelp[38 - 1] = String("Which EnergyMeter register(s) to use for Losses in AutoAdd Mode. " "May be one or more registers.  if more than one, register values are summed together. " "Array of integer values > 0.  Defaults to 13 (for Zone kWh Losses). ") + CRLF
			+ CRLF
			+ "for a list of EnergyMeter register numbers, do the \"Show Meters\" command after defining a circuit.";
		OptionHelp[39 - 1] = String("Define legal bus voltage bases for this circuit.  Enter an array " "of the legal voltage bases, in phase-to-phase voltages, for example:") + CRLF
			+ CRLF
			+ "set voltagebases=\".208, .480, 12.47, 24.9, 34.5, 115.0, 230.0\" "
			+ CRLF
			+ CRLF
			+ "When the CalcVoltageBases command is issued, a snapshot solution is performed "
			+ "with no load injections and the bus base voltage is set to the nearest legal voltage base. "
			+ "The defaults are as shown in the example above.";
		OptionHelp[40 - 1] = "{Normal* | Newton | TCIM}  Solution algorithm type. "
			+ CRLF + CRLF +
			"Normal (default) is a fixed point iteration solution method "
			"that is a little quicker than the Newton iteration.  Normal is adequate for most radial "
			"distribution circuits.  " 
			+ CRLF + CRLF +
			"Newton is more robust for circuits that are difficult to solve."
			+ CRLF + CRLF +
			"NCIM or the N Conductor Current Injection Method is a Newton-Raphson based method to be used for Transmission like cases.";
		OptionHelp[41 - 1] = String("{YES/TRUE | NO/FALSE}  Default is \"No/False\". Specifies whether to use trapezoidal integration for accumulating energy meter registers. " "Applies to EnergyMeter and Generator objects.  Default method simply multiplies the " "present value of the registers times the width of the interval (Euler). " "Trapezoidal is more accurate when there are sharp changes in a load shape or unequal intervals. " "Trapezoidal is automatically used for " "some load-duration curve simulations where the interval size varies considerably. " "Keep in mind that for Trapezoidal, you have to solve one more point than the number of intervals. " "That is, to do a Daily simulation on a 24-hr load shape, you would set Number=25 to force a solution " "at the first point again to establish the last (24th) interval.") + CRLF
			+ CRLF
			+ "Note: Set Mode= resets Trapezoidal to No/False. Set this to Yes/True AFTER setting the Mode option.";
		OptionHelp[42 - 1] = String("Array of bus names to include in AutoAdd searches. Or, you can specify a text file holding the names, one to a line, " "by using the syntax (file=filename) instead of the actual array elements. " "Default is null, which results in the program " "using either the buses in the EnergyMeter object zones or, if no EnergyMeters, all the buses, which can " "make for lengthy solution times. ") + CRLF
			+ CRLF
			+ "Examples:"
			+ CRLF
			+ CRLF
			+ "Set autobuslist=(bus1, bus2, bus3, ... )"
			+ CRLF
			+ "Set autobuslist=(file=buslist.txt)";
		OptionHelp[43 - 1] = String("{OFF | STATIC |EVENT | TIME}  Default is \"STATIC\".  Control mode for the solution. " "Set to OFF to prevent controls from changing.") + CRLF
			+ "STATIC = Time does not advance.  Control actions are executed in order of shortest time to act "
			+ "until all actions are cleared from the control queue.  Use this mode for power flow solutions which may require several "
			+ "regulator tap changes per solution."
			+ CRLF
			+ CRLF
			+ "EVENT = solution is event driven.  Only the control actions nearest in time "
			+ "are executed and the time is advanced automatically to the time of the event. "
			+ CRLF
			+ CRLF
			+ "TIME = solution is time driven.  Control actions are executed when the time for the pending "
			+ "action is reached or surpassed."
			+ CRLF
			+ CRLF
			+ "Controls may reset and may choose not to act when it comes their time. "
			+ CRLF
			+ "Use TIME mode when modeling a control externally to the DSS and a solution mode such as "
			+ "DAILY or DUTYCYCLE that advances time, or set the time (hour and sec) explicitly from the external program. ";
		OptionHelp[44 - 1] = "{YES/TRUE | NO/FALSE}  Set to YES to trace the actions taken in the control queue.  "
			"Creates a file named TRACE_CONTROLQUEUE.CSV in the default directory. "
			"The names of all circuit elements taking an action are logged.";
		OptionHelp[45 - 1] = "Global multiplier for the kW output of every generator in the circuit. Default is 1.0. "
			"Applies to all but Autoadd solution modes. "
			"Ignored for generators designated as Status=Fixed.";
		OptionHelp[46 - 1] = "Default daily load shape name. Default value is \"default\", which is a 24-hour curve defined when the DSS is started.";
		OptionHelp[47 - 1] = "Default yearly load shape name. Default value is \"default\", which is a 24-hour curve defined when the DSS is started.";
		OptionHelp[48 - 1] = "Sets the connected kVA allocation factors for all loads in the active circuit to the value given.";
		OptionHelp[49 - 1] = "{Multiphase | Positive}  Default = Multiphase.  Designates whether circuit model is to interpreted as a normal multi-phase "
			"model or a positive-sequence only mode";
		OptionHelp[50 - 1] = "Sets the present price signal ($/MWh) for the circuit.  Default value is 25.";
		OptionHelp[51 - 1] = "Sets the PRICESHAPE object to use to obtain for price signal. Default is none (null string). If none, "
			"price signal either remains constant or is set by an external process using Set Price= option. "
			"Curve is defined as a PRICESHAPE  in actual values (not normalized) and should be defined to correspond to "
			"the type of analysis being performed (daily, yearly, etc.).";
		OptionHelp[52 - 1] = "Set the active terminal of the active circuit element. May also be done with Select command.";
		OptionHelp[53 - 12] = "Default = 60. Set the fundamental frequency for harmonic solution and the default base frequency for all impedance quantities. "
			"Side effect: also changes the value of the solution frequency. Saved as default for next circuit.";
		OptionHelp[54 - 1] = String("{ALL | (list of harmonics) }  Default = ALL. Array of harmonics for which to perform a solution in Harmonics mode. " "If ALL, then solution is performed for all harmonics defined in spectra currently being used. " "Otherwise, specify a more limited list such as: ") + CRLF
			+ CRLF
			+ "   Set Harmonics=(1 5 7 11 13)";
		OptionHelp[55 - 1] = "Max control iterations per solution.  Default is 10.";
		OptionHelp[56 - 1] = "Set Active Bus by name.  Can also be done with Select and SetkVBase commands and the \"Set Terminal=\"  option. "
			"The bus connected to the active terminal becomes the active bus. See Zsc and Zsc012 commands.";
		OptionHelp[57 - 1] = String("Set the data path for files written or read by the DSS.") + CRLF
			+ "Defaults to the user documents folder."
			+ CRLF
			+ "If the DataPath is not writable, output files will be written to the user application data folder."
			+ CRLF
			+ "May be Null.  Executes a CHDIR to this path if non-null."
			+ CRLF
			+ "Does not require a circuit defined.";
		OptionHelp[58 - 1] = String("Array of bus names to keep when performing circuit reductions. You can specify a text file holding the names, one to a line, " "by using the syntax (file=filename) instead of the actual array elements. " "Command is cumulative (reset keeplist first). " "Reduction algorithm may keep other buses automatically. ") + CRLF
			+ CRLF
			+ "Examples:"
			+ CRLF
			+ CRLF
			+ "Reset Keeplist (sets all buses to FALSE (no keep))"
			+ CRLF
			+ "Set KeepList=(bus1, bus2, bus3, ... )"
			+ CRLF
			+ "Set KeepList=(file=buslist.txt)";
		OptionHelp[59 - 1] = String("{ Default or [null - 1] | Shortlines [Zmag=nnn - 1] | MergeParallel | BreakLoops | Switches | Ends | Laterals}  Strategy for reducing feeders. " "Default is to eliminate all dangling end buses and buses without load, caps, or taps. ") + CRLF
			+ "\"Shortlines [-1]\" merges short branches with impedance less than Zmag (default = 0.02 ohms) "
			+ CRLF
			+ "\"MergeParallel\" merges lines that have been found to be in parallel "
			+ CRLF
			+ "\"Breakloops\" disables one of the lines at the head of a loop. "
			+ CRLF
			+ "\"Ends\" eliminates dangling ends only."
			+ CRLF
			+ "\"Switches\" merges switches with downline lines and eliminates dangling switches."
			+ CRLF
			+ "\"Laterals [-1]\" uses the Remove command to eliminate 1-phase laterals and optionally lump the load back to the 2- or 3-phase feeder (default behavior). "
			+ CRLF
			+ CRLF
			+ "Marking buses with \"Keeplist\" will prevent their elimination.";
		OptionHelp[60 - 1] = "{YES/TRUE | NO/FALSE} Default = no. Set for keeping demand interval data for daily, yearly, etc, simulations. "
			"Side Effect:  Resets all meters!!!";
		OptionHelp[61 - 1] = "Sets the Normal rating of all lines to a specified percent of the emergency rating.  Note: This action takes place immediately. "
			"Only the in-memory value is changed for the duration of the run.";
		OptionHelp[62 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE.  Set to Yes/True if you wish a separate demand interval (DI) file written "
			"for each meter.  Otherwise, only the totalizing meters are written.";
		OptionHelp[63 - 1] = String("Name of case for yearly simulations with demand interval data. " "Becomes the name of the subdirectory under which all the year data are stored. " "Default = circuit name ") + CRLF
			+ CRLF
			+ "Side Effect: Sets the prefix for output files";
		OptionHelp[64 - 1] = "Number code for node marker on circuit plots. Number from 0 to 47. Default is 16 (open circle). 24 is solid circle. Try other values for other symbols. See also Nodewidth";
		OptionHelp[65 - 1] = "Width of node marker. Default=1. See MarkerCode";
		OptionHelp[66 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE.  Significant solution events are added to the Event Log, primarily for debugging.";
		OptionHelp[67 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE. Opens DSSRecorder.DSS in DSS install folder and enables recording of all commands that come through "
			"the text command interface. Closed by either setting to NO/FALSE or exiting the program. "
			"When closed by this command, the file name can be found in the Result. Does not require a circuit defined.";
		OptionHelp[68 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE. For yearly solution mode, sets overload reporting on/off. DemandInterval must be set to true for this to have effect.";
		OptionHelp[69 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE. For yearly solution mode, sets voltage exception reporting on/off. DemandInterval must be set to true for this to have effect.";
		OptionHelp[70 - 1] = "Sets the CFactors for for all loads in the active circuit to the value given.";
		OptionHelp[71 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE. If YES/TRUE will automatically show the results of an Export Command after it is written.";
		OptionHelp[72 - 1] = "Default is 2. Maximum number of iterations for load allocations for each time the AllocateLoads or Estimate command is given.";
		OptionHelp[73 - 1] = "Set Default Base Frequency, Hz. Side effect: Sets solution Frequency and default Circuit Base Frequency. This value is saved when the DSS closes down.";
		OptionHelp[74 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark lines that are switches or are isolated with a symbol. See SwitchMarkerCode.";
		OptionHelp[75 - 1] = "Numeric marker code for lines with switches or are isolated from the circuit. Default is 4. See markswitches option.";
		OptionHelp[76 - 1] = "Default is 1.0. Relative size (a multiplier applied to default size) of daisy circles on daisy plot.";
		OptionHelp[77 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark transformer locations with a symbol. See TransMarkerCode. "
			"The coordinate of one of the buses for winding 1 or 2 must be defined for the symbol to show";
		OptionHelp[78 - 1] = "Numeric marker code (0..47 see Users Manual) for transformers. Default is 35. See markstransformers option.";
		OptionHelp[79 - 1] = "Size of transformer marker. Default is 1.";
		OptionHelp[80 - 1] = "={Daily | Yearly | Duty | None*} Default loadshape class to use for mode=time and mode=dynamic simulations. Loads and generators, etc., will follow "
			"this shape as time is advanced. Default value is None. That is, Load will not vary with time.";
		OptionHelp[81 - 1] = "One of {Carson | FullCarson | Deri*}.  Default is Deri, which is"
			"a  fit to the Full Carson that works well into high frequencies. "
			"\"Carson\" is the simplified Carson method that is typically used for 50/60 Hz power flow programs. "
			"Applies only to Line objects that use LineGeometry objects to compute impedances.";
		OptionHelp[82 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE. When set to TRUE/YES, clears the query log file and thereafter appends "
			"the time-stamped Result string contents to the log file after a query command, ?. ";
		OptionHelp[83 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark Capacitor locations with a symbol. See CapMarkerCode. ";
		OptionHelp[84 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark Regulator locations with a symbol. See RegMarkerCode. ";
		OptionHelp[85 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark PVSystem locations with a symbol. See PVMarkerCode and PVMarkerSize. ";
		OptionHelp[86 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark Storage locations with a symbol. See StoreMarkerCode and StoreMarkerSize. ";
		OptionHelp[87 - 1] = "Numeric marker code (0..47 -- see Users Manual) for Capacitors. Default is 38.";
		OptionHelp[88 - 1] = "Numeric marker code (0..47 see Users Manual) for Regulators. Default is 17. (red)";
		OptionHelp[89 - 1] = "Numeric marker code (0..47 see Users Manual) for PVSystems and PVSystem. Default is 15.";
		OptionHelp[90 - 1] = "Numeric marker code (0..47 see Users Manual) for Storage elements. Default is 9.";
		OptionHelp[91 - 1] = "Size of Capacitor marker. Default is 3.";
		OptionHelp[92 - 1] = "Size of Regulator marker. Default is 5.";
		OptionHelp[93 - 1] = "Size of PVsystem and PVSystem markers. Default is 1.";
		OptionHelp[94 - 1] = "Size of Storage marker. Default is 1.";
		OptionHelp[95 - 1] = String("{YES/TRUE | NO/FALSE}  Default is NO. For Harmonic solution, neglect the Load shunt admittance branch that can siphon off some of the Load injection current. ") + CRLF
			+ CRLF
			+ "If YES, the current injected from the LOAD at harmonic frequencies will be nearly ideal.";
		OptionHelp[96 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark Fuse locations with a symbol. See FuseMarkerCode and FuseMarkerSize. ";
		OptionHelp[97 - 1] = "Numeric marker code (0..47 see Users Manual) for Fuse elements. Default is 25.";
		OptionHelp[98 - 1] = "Size of Fuse marker. Default is 1.";
		OptionHelp[99 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark Recloser locations with a symbol. See RecloserMarkerCode and RecloserMarkerSize. ";
		OptionHelp[100 - 1] = "Numeric marker code (0..47 see Users Manual) for Recloser elements. Default is 17. (color=Lime)";
		OptionHelp[101 - 1] = "Size of Recloser marker. Default is 5.";
		OptionHelp[102 - 1] = "{YES/TRUE | NO/FALSE}  Default is Yes. Update Windows Registry values upon exiting.  You might want to turn this off if you temporarily "
			"change fonts or DefaultBaseFrequency, for example. ";
		OptionHelp[103 - 1] = "{YES/TRUE | NO/FALSE}  Default is NO. Mark Relay locations with a symbol. See RelayMarkerCode and RelayMarkerSize. ";
		OptionHelp[104 - 1] = "Numeric marker code (0..47 see Users Manual) for Relay elements. Default is 17. (Color=Lime)";
		OptionHelp[105 - 1] = "Size of Relay marker. Default is 5.";
		OptionHelp[106 - 1] = "The time in microseconds to execute the solve process in the most recent time step or solution (read only)";
		OptionHelp[107 - 1] = "The accumulated time in microseconds to solve the circuit since the last reset. Set this value to reset the accumulator.";
		OptionHelp[108 - 1] = "Process time + meter sampling time in microseconds for most recent time step - (read only)";
		OptionHelp[109 - 1] = "Delivers the number of threads (CPUs) available on the machine (read Only)";
		OptionHelp[110 - 1] = "Delivers the number of physical processors (Cores) available on the computer. If your computers processor has less than 64 cores, this number should be equal to the half of the available CPUs, otherise the number should  be the same (Read Only)";
		OptionHelp[111 - 1] = "Delivers the number of Actors created by the user, 1 is the default";
		OptionHelp[112 - 1] = "Gets/Sets the number of the active actor, if the value is * (set active actor=*), the commands send after this instruction will be aplied to all the actors.";
		OptionHelp[113 - 1] = String("(default -1)Gets/Sets the CPU to be used by the active actor. If negative (-1) means that the actor affinity is to all the CPUs and will be executed in the") + CRLF
			+ "first available CPU and will be realocated into another CPU dynamically if the operating system requires it. By setting a CPU number for an actor will force"
			+ CRLF
			+ "the actor to be executed only on the specific CPU.";
		OptionHelp[114 - 1] = "Gets progress (%) for all the actors when performing a task";
		OptionHelp[115 - 1] = "Activates/Deactivates the parallel machine in OpenDSS, if deactivated OpenDSS will behave sequentially";
		OptionHelp[116 - 1] = "Activates/Deactivates the option for concatenate the reports generated by the existing actors, if Yes, everytime the user"
			"a show/export monitor command the report will include the data generated by all the actors, otherwise the report will contain"
			"The data generated by the active actor";
		OptionHelp[117 - 1] = "Activates/Deactivates the extended version of the plot command for figures with the OpenDSS Viewer.";
		OptionHelp[118 - 1] = "Returns Yes/No if the OpenDSS Viewer installation is detected in the local machine (Read Only)";
		OptionHelp[119 - 1] = String("Percentage of coverage expected when estimating the longest paths on the circuit for tearing, the default coverage") + CRLF
			+ "is the 90% (0.9), this value cannot exceed 1.0. When used with the \"Set\" command is used for the algorithm for estimating the paths within the circuit"
			+ CRLF
			+ "but when the \"get\" command is used after executing the tear_circuit command it will deliver the actual coverage after running the algorithm";
		OptionHelp[120 - 1] = "This is the number of subcircuits in which the circuit will be torn when executing the tear_circuit command, by default is the number of local CPUs - 1";
		OptionHelp[121 - 1] = "{YES/TRUE | NO/FALSE} Overrides default value for sampling EnergyMeter objects at the end of the solution loop. "
			"Normally Time and Duty modes do not automatically sample EnergyMeters whereas Daily, Yearly, M1, M2, M3, LD1 and LD2 modes do. "
			"Use this Option to turn sampling on or off";
		OptionHelp[122 - 1] = String("{YES/TRUE | NO/FALSE} Activates the A-Diakoptics solution algorithm for using spatial parallelization on the feeder.") + CRLF
			+ "This parameter only affects Actor 1, no matter from which actor is called. When activated (True), OpenDSS will start the "
			+ CRLF
			+ "initialization routine for the A-Diakoptics solution mode";
		OptionHelp[123 - 1] = "Minimum number of iterations required for a solution. Default is 2.";
		OptionHelp[124 - 1] = String("Get/set the names of the link branches used for tearing the circuit after initializing using set ADiakoptics = True. Using this instruction will set the Active Actor = 1") + CRLF
			+ "If ADiakoptics is not initialized, this instruction will return an error message";
		OptionHelp[125 - 1] = "Keeploads = Y/N option for ReduceOption Laterals option";
		OptionHelp[126 - 1] = "Sets the Zmag option (in Ohms) for ReduceOption Shortlines option. Lines have less line mode impedance are reduced.";
		OptionHelp[127 - 1] = "{YES/TRUE | NO/FALSE} Default = FALSE. Enables/disables the seasonal selection of the rating for determining if an element is overloaded. When enabled, the energy meter wil"
			"look for the rating (NormAmps) using the SeasonSignal  to evaluate if the PDElement is overloaded";
		OptionHelp[128 - 1] = "It is the name of the XY curve defining the ratings seasonal change for the PDElements in the model when performing QSTS simulations. The seasonal ratings need to be defined"
			"at the PDElement or at the general object definition such as linecodes, lineGeometry, etc.";
		OptionHelp[129 - 1] = "Delivers the number of Non-uniform memory access nodes (NUMA Nodes) available on the machine (read Only). This information is vital when working"
			"with processor clusters (HPC). It will help you know the number of processors in the cluster";
		OptionHelp[130 - 1] = "Returns Yes/No if the OpenDSS GIS installation is detected in the local machine (Read Only)";
		OptionHelp[131 - 1] = "[Coords - 1] : An array of doubles defining the longitud and latitude for an area to be used as refrence for the OpenDSS-GIS related commands, long1, lat1, long2, lat2";
		OptionHelp[132 - 1] = "Color    : A Hex string defining 24 bit color in RGB format, e.g. , red = FF0000";
		OptionHelp[133 - 1] = "Thickness: An integer defining the thickness (default = 3)";
		OptionHelp[134 - 1] = "{YES/TRUE | NO/FALSE*} Set/get the boolean flag for indicating to the tearing algorithm the source of the link branches for tearing the model into sub-circuits."
			" If FALSE, OpenDSS will use METIS for estimating the link branches to be used based on the number of sub-circuits given by the user through the command \"set Num_SubCircuits\"."
			"Otherwise, OpenDSS will use the list of link branches given by the user with the command \"set LinkBranches\".";
		OptionHelp[135 - 1] = "(Read only) Returns the list of line types available in the code for reference.These line types apply to lines, line codes, and line geometry objects.";
		OptionHelp[136 - 1] = "{YES / TRUE | NO / FALSE*} Sets / gets the default for the eventlog.After changing this flags the model needs to be recompiled to take effect.";
		OptionHelp[137 - 1] = "{YES / TRUE | NO / FALSE*} Defines whether the long - line correctlion is applied or not.Long - line correction only affects lines modelled with sequence components.";
		OptionHelp[138 - 1] = "{YES / TRUE | NO / FALSE} Default = TRUE.If YES / TRUE will automatically show the results of a Show Command after it is written.";
		OptionHelp[139 - 1] = "{YES/TRUE | NO/FALSE*} Use this flag to indicate if you want to ignore the Q limits for generators during an NCIM solution. The default is NO/FALSE, signaling that generators will always respect their Q generation/absorbtion limits.";
		OptionHelp[140 - 1] = "{1.0*} Use this option to set a gain for the reactive power compensation provided by PV buses (generator model 3) when using the NCIM solution algorithm. The default value is 1.0.";
        OptionHelp[141 - 1] = "Reads or Writes the value of the given state variable for the given PCE. Depending on the access mode (read/write) the syntax may vary. For writing the variable use the following syntax:" + CRLF + 
							CRLF + 
							"set StateVar = myObjName myVarName myValue" + CRLF + 
							CRLF + 
							"Where myObjName corresponds to the class and object name, for example, if you want to refer to generator Gen1, then myObjName will be Generator.Gen1. myVarName is the name of the state variable and myValue is the value to assign." + CRLF + 
							CRLF + " For reading the state variable use the following syntax : " + CRLF + 
							CRLF + " get StateVar myObjName myVarName " + CRLF + 
							CRLF + " The reading structure will return the value in the results tab.";


	}
	//----------------------------------------------------------------------------

	bool DoSetCmd_NoCircuit()
	{
		bool result = false;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		result = true;
		 // Continue parsing command line
		ParamPointer = 0;
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
		while(Param.size() > 0)
		{
			if(ParamName.size() == 0)
				++ParamPointer;
			else
				ParamPointer = OptionList->Getcommand(ParamName);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for Set Command ", 130);
				break;
				case 	15:
				DefaultEditor = Param;
				break;     // 'Editor='
				case 	57:
				SetDataPath(Param);
				break;  // Set a legal data path
				case 	67:
				DSSExecutive[ActiveActor]->Set_RecorderOn(InterpretYesNo(Param));
				break;
				case 	73:
				DefaultBaseFreq = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	102:
				UpdateRegistry = InterpretYesNo(Param);
				break;
				case 	112:
				{
					if(Parser[ActiveActor]->MakeString_() == "*")
					{
						AllActors = true;
						ActiveActor = 1;
					}
					else
					{
						if(Parser[ActiveActor]->MakeInteger_() <= NumOfActors)
						{
							ActiveActor = Parser[ActiveActor]->MakeInteger_();
							AllActors = false;
						}
						else
						{
							DoSimpleMsg("The actor does not exists", 7002);
						}
					}
				}
				break;
				case 	113:
				{
					if(Parser[ActiveActor]->MakeInteger_() < CPU_Cores)
					{
						ActorCPU[ActiveActor] = Parser[ActiveActor]->MakeInteger_();
						if(ActorHandle[ActiveActor] != nullptr)
						{
							//ActorHandle[ActiveActor]->CPU = ActorCPU[ActiveActor];
							//ActorHandle[ActiveActor]->Priority = tpTimeCritical;
						}
					}
					else
					{
						DoSimpleMsg("The CPU does not exists", 7003);
					}
				}
				break;
				case 	115:
				{
					Parallel_enabled = InterpretYesNo(Param);
				}
				break;
				case 	116:
				{
					ConcatenateReports = InterpretYesNo(Param);
				}
				break;
				case 	117:
				{
					DSS_Viz_enable = InterpretYesNo(Param);
				}
				break;
				case    136:  
				{
					EventLogDefault = InterpretYesNo(Param);
				}
				break;
				default:
				DoSimpleMsg("You must create a new circuit object first: \"new circuit.mycktname\" to execute this Set command.", 301);
				result = false;  // Indicate that we could not process all set command
				return result;
				break;
			}
			ParamName = Parser[ActiveActor]->GetNextParam();
			Param = Parser[ActiveActor]->MakeString_();
		} /*WHILE*/
		return result;
	}  // Set Commands that do not require a circuit
	//----------------------------------------------------------------------------

	// This is for setting global options that do not require an active circuit


	//----------------------------------------------------------------------------

	int DoSetCmd(int SolveOption)
	{
		int result = 0;
		int i = 0;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		TLoadShapeObj* TestLoadShapeObj = nullptr;
		TStringList* myList;
		result = 0;
		 // Continue parsing command line
		ParamPointer = 0;
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
		while(Param.size() > 0)
		{
			if(ParamName.size() == 0)
				++ParamPointer;
			else
				ParamPointer = OptionList->Getcommand(ParamName);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for Set Command ", 130);
				break;
				case 	1:
				 case 12:
				SetObjectClass(Param);
				break;
				case 	2:
				 case 13:
				SetObject(Param);
				break;
				case 	3:
				ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	4:
				ActiveCircuit[ActiveActor]->Solution->DynaVars.T = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	5:
				/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with0 = ActiveCircuit[ActiveActor];
					with0->Solution->Set_Year(Parser[ActiveActor]->MakeInteger_());
					with0->DefaultGrowthFactor = pow(with0->DefaultGrowthRate, (with0->Solution->get_Fyear() - 1));
				}
				break;
				case 	6:
				ActiveCircuit[ActiveActor]->Solution->Set_Frequency(Parser[ActiveActor]->MakeDouble_());
				break;
				case 	7:
				 case 18:
				/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with0 = ActiveCircuit[ActiveActor];
					with0->Solution->DynaVars.h = InterpretTimeStepSize(Param);
					with0->Solution->IntervalHrs = double(with0->Solution->DynaVars.h) / 3600.0;
				}
				break;
				case 	8:
				{
					ActiveCircuit[ActiveActor]->Solution->Set_Mode(InterpretSolveMode(Param));  // see DSSGlobals
					if(ADiakoptics)
					{
						int stop = 0;
						for(stop = NumOfActors, i = 2; i <= stop; i++)
						{
							ActiveCircuit[i]->Solution->Set_Mode(ActiveCircuit[1]->Solution->Get_SolMode());
						}
					}
				}
				break;
				case 	9:
				ActiveCircuit[ActiveActor]->Solution->RandomType = InterpretRandom(Param);
				break;
				case 	10:
				ActiveCircuit[ActiveActor]->Solution->NumberOfTimes = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	11:
				Set_Time();
				break;
				case 	14:
				SetActiveCircuit(Param);
				break;
				case 	15:
				DefaultEditor = Param;
				break;     // 'Editor='
				case 	16:
				ActiveCircuit[ActiveActor]->Solution->ConvergenceTolerance = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	17:
				ActiveCircuit[ActiveActor]->Solution->MaxIterations = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	19:
				/*# with ActiveCircuit[ActiveActor]->Solution do */
				{
					auto with2 = ActiveCircuit[ActiveActor]->Solution;
					with2->DefaultLoadModel = InterpretLoadModel(Param); // for reverting to last on specified
					with2->LoadModel = with2->DefaultLoadModel;
				}
				break;
				case 	20:
				ActiveCircuit[ActiveActor]->Set_LoadMultiplier(Parser[ActiveActor]->MakeDouble_());
				break;  // Set using LoadMultiplier property
				case 	21:
				ActiveCircuit[ActiveActor]->NormalMinVolts = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	22:
				ActiveCircuit[ActiveActor]->NormalMaxVolts = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	23:
				ActiveCircuit[ActiveActor]->EmergMinVolts = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	24:
				ActiveCircuit[ActiveActor]->EmergMaxVolts = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	25:
				ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->Set_Mean( (Parser[ActiveActor]->MakeDouble_()) / 100.0);
				break;
				case 	26:
				ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->Set_StdDev( (Parser[ActiveActor]->MakeDouble_()) / 100.0);
				break;
				case 	27:
				/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with30 = ActiveCircuit[ActiveActor];
					with30->LoadDurCurve = Param;
					with30->LoadDurCurveObj = (TLoadShapeObj*) LoadShapeClass[ActiveActor]->Find(Param);
					if(with30->LoadDurCurveObj == nullptr)
						DoSimpleMsg("Load-Duration Curve not found.", 131);
				}
				break;
				case 	28:
				/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with30 = ActiveCircuit[ActiveActor];
					with30->DefaultGrowthRate = 1.0 + Parser[ActiveActor]->MakeDouble_() / 100.0;
					with30->DefaultGrowthFactor = pow(with30->DefaultGrowthRate, (with30->Solution->get_Fyear() - 1));
				}
				break;
				case 	29:
				ActiveCircuit[ActiveActor]->AutoAddObj.GenkW = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	30:
				ActiveCircuit[ActiveActor]->AutoAddObj.GenPF = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	31:
				ActiveCircuit[ActiveActor]->AutoAddObj.Capkvar = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	32:
				ActiveCircuit[ActiveActor]->AutoAddObj.AddType = InterpretAddType(Param);
				break;
				case 	33:
				ActiveCircuit[ActiveActor]->DuplicatesAllowed = InterpretYesNo(Param);
				break;
				case 	34:
				ActiveCircuit[ActiveActor]->ZonesLocked = InterpretYesNo(Param);
				break;
				case 	35:
				ActiveCircuit[ActiveActor]->UEWeight = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	36:
				ActiveCircuit[ActiveActor]->LossWeight = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	37:
				ParseIntArray(ActiveCircuit[ActiveActor]->UEregs, ActiveCircuit[ActiveActor]->NumUEregs, Param);
				break;
				case 	38:
				ParseIntArray(ActiveCircuit[ActiveActor]->LossRegs, ActiveCircuit[ActiveActor]->NumLossRegs, Param);
				break;
				case 	39:
				DoLegalVoltageBases();
				break;
				case 	40:
					ActiveCircuit[ActiveActor]->Solution->Algorithm = InterpretSolveAlg(Param);
					if (ActiveCircuit[ActiveActor]->Solution->Algorithm == NCIMSOLVE)
						ActiveCircuit[ActiveActor]->Solution->NCIMRdy = false;
					break;
				case 	41:
				ActiveCircuit[ActiveActor]->TrapezoidalIntegration = InterpretYesNo(Param);
				break;
				case 	42:
				DoAutoAddBusList(Param);
				break;
				case 	43:
				/*# with ActiveCircuit[ActiveActor]->Solution do */
				{
					auto with5 = ActiveCircuit[ActiveActor]->Solution;
					with5->ControlMode = InterpretControlMode(Param);
					with5->DefaultControlMode = with5->ControlMode;  // always revert to last one specified in a script
	//				if(ADiakoptics && (ActiveActor == 1))
	//					with5->SendCmd2Actors(with5->GETCTRLMODE);
				}
				break;
				case 	44:
				ActiveCircuit[ActiveActor]->ControlQueue.Set_Trace(InterpretYesNo(Param));
				break;
				case 	45:
				ActiveCircuit[ActiveActor]->GenMultiplier = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	46:
				{
					TestLoadShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActiveActor]->Find(Param));
					if(TestLoadShapeObj != nullptr)
						ActiveCircuit[ActiveActor]->DefaultDailyShapeObj = TestLoadShapeObj;
				}
				break;
				case 	47:
				{
					TestLoadShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActiveActor]->Find(Param));
					if(TestLoadShapeObj != nullptr)
						ActiveCircuit[ActiveActor]->DefaultYearlyShapeObj = TestLoadShapeObj;
				}
				break;
				case 	48:
				DoSetAllocationFactors(Parser[ActiveActor]->MakeDouble_());
				break;
				case 	49:
				ActiveCircuit[ActiveActor]->PositiveSequence = InterpretCktModel(Param);
				break;
				case 	50:
				ActiveCircuit[ActiveActor]->PriceSignal = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	51:
				/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with31 = ActiveCircuit[ActiveActor];
					with31->PriceCurve = Param;
					with31->PriceCurveObj = (TPriceShapeObj*) PriceShapeClass[ActiveActor]->Find(Param);
					if(with31->PriceCurveObj == nullptr)
						DoSimpleMsg(String("Priceshape.") + Param + " not found.", 132);
				}
				break;
				case 	52:
				/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with31 = ActiveCircuit[ActiveActor];
					if(with31->get_FActiveCktElement() != nullptr)
						/*# with ActiveCktElement do */
						{
							auto with8 = with31->get_FActiveCktElement();
							with8->Set_ActiveTerminal(Parser[ActiveActor]->MakeInteger_());
							SetActiveBus(StripExtension(with8->GetBus(with8->get_FActiveTerminal())));   // bus connected to terminal
						}
				}
				break;
				case 	53:
				{
					ActiveCircuit[ActiveActor]->Fundamental = Parser[ActiveActor]->MakeDouble_();     // Set Base Frequency for system (used henceforth)
					ActiveCircuit[ActiveActor]->Solution->Set_Frequency(Parser[ActiveActor]->MakeDouble_());
				}
				break;
				case 	54:
				DoHarmonicsList(Param);
				break;
				case 	55:
				{
					ActiveCircuit[ActiveActor]->Solution->MaxControlIterations = Parser[ActiveActor]->MakeInteger_();
					if(ADiakoptics && (ActiveActor == 1))
						ActiveCircuit[ActiveActor]->Solution->SendCmd2Actors(GETCTRLMODE);
				}
				break;
				case 	56:
				result = SetActiveBus(Param);
				break;   // See DSSGlobals
				case 	57:
				SetDataPath(Param);
				break;  // Set a legal data path
				case 	58:
				DoKeeperBusList(Param);
				break;
				case 	59:
				DoSetReduceStrategy(Param);
				break;
				case 	60:
				EnergyMeterClass[ActiveActor]->Set_SaveDemandInterval(ActiveActor, InterpretYesNo(Param));
				break;
				case 	61:
				{
					ActiveCircuit[ActiveActor]->PctNormalFactor = Parser[ActiveActor]->MakeDouble_();
					DoSetNormal(ActiveCircuit[ActiveActor]->PctNormalFactor);
				}
				break;
				case 	62:
				EnergyMeterClass[ActiveActor]->Set_DI_Verbose(ActiveActor, InterpretYesNo(Param));
				break;
				case 	63:
				ActiveCircuit[ActiveActor]->Set_CaseName(Parser[ActiveActor]->MakeString_());
				break;
				case 	64:
				ActiveCircuit[ActiveActor]->NodeMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	65:
				ActiveCircuit[ActiveActor]->NodeMarkerWidth = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	66:
				ActiveCircuit[ActiveActor]->LogEvents = InterpretYesNo(Param);
				break;
				case 	67:
				DSSExecutive[ActiveActor]->Set_RecorderOn(InterpretYesNo(Param));
				break;
				case 	68:
				EnergyMeterClass[ActiveActor]->Do_OverloadReport = InterpretYesNo(Param);
				break;
				case 	69:
				EnergyMeterClass[ActiveActor]->Do_VoltageExceptionReport = InterpretYesNo(Param);
				break;
				case 	70:
				DoSetCFactors(Parser[ActiveActor]->MakeDouble_());
				break;
				case 	71:
				AutoShowExport = InterpretYesNo(Param);
				break;
				case 	72:
				MaxAllocationIterations = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	73:
				{
					DefaultBaseFreq = Parser[ActiveActor]->MakeDouble_();
					ActiveCircuit[ActiveActor]->Fundamental = Parser[ActiveActor]->MakeDouble_();     // Set Base Frequency for system (used henceforth)
					ActiveCircuit[ActiveActor]->Solution->Set_Frequency(Parser[ActiveActor]->MakeDouble_());
				}
				break;
				case 	74:
				ActiveCircuit[ActiveActor]->MarkSwitches = InterpretYesNo(Param);
				break;
				case 	75:
				ActiveCircuit[ActiveActor]->SwitchMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	76:
				DaisySize = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	77:
				ActiveCircuit[ActiveActor]->MarkTransformers = InterpretYesNo(Param);
				break;
				case 	78:
				ActiveCircuit[ActiveActor]->TransMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	79:
				ActiveCircuit[ActiveActor]->TransMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	80:
				ActiveCircuit[ActiveActor]->ActiveLoadShapeClass = InterpretLoadShapeClass(Param);
				break;
				case 	81:
				DefaultEarthModel = InterpretEarthModel(Param);
				break;
				case 	82:
				{
					LogQueries = InterpretYesNo(Param);
					if(LogQueries)
						ResetQueryLogFile();
				}
				break;
				case 	83:
				ActiveCircuit[ActiveActor]->MarkCapacitors = InterpretYesNo(Param);
				break;
				case 	84:
				ActiveCircuit[ActiveActor]->MarkRegulators = InterpretYesNo(Param);
				break;
				case 	85:
				ActiveCircuit[ActiveActor]->MarkPVSystems = InterpretYesNo(Param);
				break;
				case 	86:
				ActiveCircuit[ActiveActor]->MarkStorage = InterpretYesNo(Param);
				break;
				case 	87:
				ActiveCircuit[ActiveActor]->CapMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	88:
				ActiveCircuit[ActiveActor]->RegMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	89:
				ActiveCircuit[ActiveActor]->PVMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	90:
				ActiveCircuit[ActiveActor]->StoreMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	91:
				ActiveCircuit[ActiveActor]->CapMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	92:
				ActiveCircuit[ActiveActor]->RegMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	93:
				ActiveCircuit[ActiveActor]->PVMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	94:
				ActiveCircuit[ActiveActor]->StoreMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	95:
				ActiveCircuit[ActiveActor]->NeglectLoadY = InterpretYesNo(Param);
				break;
				case 	96:
				ActiveCircuit[ActiveActor]->MarkFuses = InterpretYesNo(Param);
				break;
				case 	97:
				ActiveCircuit[ActiveActor]->FuseMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	98:
				ActiveCircuit[ActiveActor]->FuseMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	99:
				ActiveCircuit[ActiveActor]->MarkReclosers = InterpretYesNo(Param);
				break;
				case 	100:
				ActiveCircuit[ActiveActor]->RecloserMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	101:
				ActiveCircuit[ActiveActor]->RecloserMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	102:
				UpdateRegistry = InterpretYesNo(Param);
				break;
				case 	103:
				ActiveCircuit[ActiveActor]->MarkRelays = InterpretYesNo(Param);
				break;
				case 	104:
				ActiveCircuit[ActiveActor]->RelayMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	105:
				ActiveCircuit[ActiveActor]->RelayMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	107:
				ActiveCircuit[ActiveActor]->Solution->Set_Total_Time(Parser[ActiveActor]->MakeDouble_());
				break;
				case 	112:
				{
					if(Parser[ActiveActor]->MakeString_() == "*")
					{
						AllActors = true;
						ActiveActor = 1;
					}
					else
					{
						if(Parser[ActiveActor]->MakeInteger_() <= NumOfActors)
						{
							ActiveActor = Parser[ActiveActor]->MakeInteger_();
							AllActors = false;
						}
						else
						{
							DoSimpleMsg("The actor does not exists", 7002);
						}
					}
				}
				break;
				case 	113:
				{
					if(Parser[ActiveActor]->MakeInteger_() < CPU_Cores)
					{
						ActorCPU[ActiveActor] = Parser[ActiveActor]->MakeInteger_();
						if(ActorHandle[ActiveActor] != nullptr)
						{
							//ActorHandle[ActiveActor]->CPU = ActorCPU[ActiveActor];
							//ActorHandle[ActiveActor]->Priority = tpTimeCritical;
						}
					}
					else
					{
						DoSimpleMsg("The CPU does not exists", 7003);
					}
				}
				break;
				case 	115:
				{
					Parallel_enabled = InterpretYesNo(Param);
				}
				break;
				case 	116:
				{
					ConcatenateReports = InterpretYesNo(Param);
				}
				break;
				case 	117:
				{
					DSS_Viz_enable = InterpretYesNo(Param);
				}
				break;
				case 	119:
				{
					ActiveCircuit[ActiveActor]->Coverage = Parser[ActiveActor]->MakeDouble_();
				}
				break;
				case 	120:
				{
					ActiveCircuit[ActiveActor]->Num_SubCkts = Parser[ActiveActor]->MakeInteger_();
				}
				break;
				case 	121:
				ActiveCircuit[ActiveActor]->Solution->SampleTheMeters = InterpretYesNo(Param);
				break;
				case 	122:
				{
					if(InterpretYesNo(Param))
					{
						if(!ADiakoptics)  // Initalizes the parallel environment if enabled
							ADiakopticsInit();
						else
							DoSimpleMsg("A-Diakoptics is already active, please use ClearAll and recompile the source mode"
				   " before activating A-Diakoptics again.", 7010);
					}
					else
					ADiakoptics = false;
				}
				break;
				case 	123:
				ActiveCircuit[ActiveActor]->Solution->MinIterations = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	124:
				{
					myList = new TStringList();
					InterpretTStringListArray(Param, *myList);
					if(myList->size() <= (CPU_Cores - 3))
					{
						int stop = 0;
						ActiveCircuit[ActiveActor]->Link_Branches.resize(myList->size() + 1);
						for(stop = myList->size(), i = 1; i <= stop; i++)
						{
							ActiveCircuit[ActiveActor]->Link_Branches[i] = (*myList)[i];
						}
					}
					else
					{
						DoSimpleMsg("The number of link branches exceeds the number of available CPUs for circuit tearing", 7009);
						ActiveCircuit[ActiveActor]->Link_Branches.clear();
					}
					delete myList;
				}
				break;
				case 	125:
				ActiveCircuit[ActiveActor]->ReduceLateralsKeepLoad = InterpretYesNo(Param);
				break;
				case 	126:
				ActiveCircuit[ActiveActor]->ReductionZmag = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	127:
				SeasonalRating = InterpretYesNo(Param);
				break;
				case 	128:
				SeasonSignal = Param;
				break;
	 // deprecated         130: ActiveCircuit[ActiveActor]->MarkPVSystems2   := InterpretYesNo(Param);
	  // deprecated           132: ActiveCircuit[ActiveActor]->MarkStorage2     := InterpretYesNo(Param);
				case 	131:
				{
					Parser[ActiveActor]->ParseAsVector(4, GISCoords);
				}
				break;
				case 	132:
				{
					GISColor = Parser[ActiveActor]->MakeString_();
				}
				break;
				case 	133:
				{
					GISThickness = Parser[ActiveActor]->MakeString_();
				}
				break;
				case 	134:
				{
					UseUserLinks = InterpretYesNo(Param);
				}
				break;
				case    136:  
				{
					EventLogDefault = InterpretYesNo(Param);
				}
				break;
				case    137:  
				{
					ActiveCircuit[ActiveActor]->LongLineCorrection = InterpretYesNo(Param);
				}
				break;
				case	138: 
				{
					AutoDisplayShowReport = InterpretYesNo(Param);
				}
				break;
				case 	139:
				{
					ActiveCircuit[ActiveActor]->Solution->IgnoreQLimit = InterpretYesNo(Param);
				}
				break;
				case 	140:
					ActiveCircuit[ActiveActor]->Solution->GenGainNCIM = Parser[ActiveActor]->MakeDouble_();
				break;
				case 141:
				{
                    auto with3 = ActiveCircuit[ActiveActor];
                    string TmpStr = Parser[ActiveActor]->MakeString_();
                    string VarPCE[3] = { "generator", "windgen", "storage" };
                    int i = with3->SetElementActive(TmpStr);
                    if (i == 0)
                        DoSimpleMsg("Object " + TmpStr + " not found", 7100);
                    else
                    {
                        TmpStr = LowerCase(StripExtension(TmpStr));
                        bool ValidObj = false;
                        for (i = 0; i < 3; i++)
                        {
                            ValidObj = ValidObj || (VarPCE[i] == TmpStr);
                        }

						if (ValidObj)
						{
                            Parser[ActiveActor]->GetNextParam();
                            TmpStr = LowerCase(Parser[ActiveActor]->MakeString_());
                            // Search for the variable within the object
                            ValidObj = false;
                            for (i = 1; i <= ((TPCElement*)with3->FActiveCktElement)->NumStateVars; i++)
                            {
                                if (LowerCase(((TPCElement*)with3->FActiveCktElement)->VariableName(i)) == TmpStr)
								{
                                    ValidObj = true;
                                    break;
								}
                            }

							if (ValidObj)
                            {
								// Once found, modifies the value
                                Parser[ActiveActor]->GetNextParam();
                                ((TPCElement*)with3->FActiveCktElement)->Set_Variable(i, Parser[ActiveActor]->MakeDouble_());
                            }
                            else
								DoSimpleMsg("State variable " + TmpStr + " not found", 7102);
						}
                        else
							DoSimpleMsg("Object " + TmpStr + " is not a valid element for this command. Only generators, storage and WindGen.", 7101);
                    }
				}
                break;
			   // Ignore excess parameters
				default:
				  ;
				break;
			}
			switch(ParamPointer)
			{
				case 	3:
				 case 4:
				ActiveCircuit[ActiveActor]->Solution->Update_dblHour();
				break;
				case	137:
				{
					TLineObj* LineObj = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
					while (LineObj != nullptr)
					{
						if (LineObj->FEnabled && LineObj->SymComponentsModel)
							LineObj->Set_YprimInvalid(ActiveActor, true);
					}
				}
				break;

				 case 135:
				 {
					 // Do nothing
				 }
				default:
				  ;
				break;
			}
			ParamName = Parser[ActiveActor]->GetNextParam();
			Param = Parser[ActiveActor]->MakeString_();
		} /*WHILE*/
		if(SolveOption == 1)
			DoSolveCmd();
		return result;
	}
	//----------------------------------------------------------------------------

	// Set DSS Options
	// Solve Command is re-routed here first to set options beFORe solving



	//----------------------------------------------------------------------------

	int DoGetCmd()
	{
		int result = 0;
		int ParamPointer = 0;
		int i = 0;
		String TempString;
		String ParamName;
		String Param;
		//	TScriptEdit* ScriptEd = nullptr;
		result = 0;
		try
		{
			GlobalResult = "";  //initialize for appending

			// Continue parsing command line
			ParamName = Parser[ActiveActor]->GetNextParam();
			Param = Parser[ActiveActor]->MakeString_();
			// there will be no named paramters in this command and the params
			// themselves will be the parameter name to return
			while (Param.size() > 0)
			{
				ParamPointer = OptionList->Getcommand(Param);
				switch (ParamPointer)
				{
				case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for Get Command ", 133);
					break;
				case 	1: case 12:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Get_myPName());
					break;
				case 	2: case 13:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->get_FActiveCktElement()->get_Name());
					break;
				case 	3:
					AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour));
					break;
				case 	4:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->DynaVars.T));
					break;
				case 	5:
					AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor]->Solution->get_Fyear()));
					break;
				case 	6:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->get_FFrequency()));
					break;
				case 	7: case 18:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->DynaVars.h));
					break;
				case 	8:
					AppendGlobalResult(GetSolutionModeID());
					break;
				case 	9:
					AppendGlobalResult(GetRandomModeID());
					break;
				case 	10:
					AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor]->Solution->NumberOfTimes));
					break;
				case 	11:
					AppendGlobalResult(Format("[ %d, %-g ] !... %-g (hours)", ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour, ActiveCircuit[ActiveActor]->Solution->DynaVars.T, ActiveCircuit[ActiveActor]->Solution->DynaVars.dblHour));
					break;
				case 	14:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->Get_Name());
					break;
				case 	15:
					AppendGlobalResult(DefaultEditor);
					break;
				case 	16:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->ConvergenceTolerance));
					break;
				case 	17:
					AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor]->Solution->MaxIterations));
					break;
				case 	19:
					AppendGlobalResult(GetLoadModel());
					break;
				case 	20:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->get_FLoadMultiplier()));
					break;
				case 	21:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->NormalMinVolts));
					break;
				case 	22:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->NormalMaxVolts));
					break;
				case 	23:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->EmergMinVolts));
					break;
				case 	24:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->EmergMaxVolts));
					break;
				case 	25:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->Get_Mean() * 100.0));
					break;
				case 	26:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->Get_StdDev() * 100.0));
					break;
				case 	27:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->LoadDurCurve);
					break;
				case 	28:
					AppendGlobalResult(Format("%-g", (ActiveCircuit[ActiveActor]->DefaultGrowthRate - 1.0) * 100.0));
					break;
				case 	29:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->AutoAddObj.GenkW));
					break;
				case 	30:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->AutoAddObj.GenPF));
					break;
				case 	31:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->AutoAddObj.Capkvar));
					break;
				case 	32:
					switch (ActiveCircuit[ActiveActor]->AutoAddObj.AddType)
					{
					case 	GENADD:
						AppendGlobalResult("generator");
						break;
					case 	CAPADD:
						AppendGlobalResult("capacitor");
						break;
					default:
						;
						break;
					}
					break;
				case 	33:
					if (ActiveCircuit[ActiveActor]->DuplicatesAllowed)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	34:
					if (ActiveCircuit[ActiveActor]->ZonesLocked)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	35:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->UEWeight));
					break;
				case 	36:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->LossWeight));
					break;
				case 	37:
					AppendGlobalResult(IntArrayToString(ActiveCircuit[ActiveActor]->UEregs, ActiveCircuit[ActiveActor]->NumUEregs));
					break;
				case 	38:
					AppendGlobalResult(IntArrayToString(ActiveCircuit[ActiveActor]->LossRegs, ActiveCircuit[ActiveActor]->NumLossRegs));
					break;
				case 	39:
					/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with32 = ActiveCircuit[ActiveActor];
					i = 0;
					GlobalResult = "(";
					while ((with32->LegalVoltageBases)[i] > 0.0)
					{
						GlobalResult = GlobalResult + Format("%-g, ", (with32->LegalVoltageBases)[i]);
						++i;
					}
					GlobalResult = GlobalResult + ")";
				}
				break;
				case 	40:
					switch (ActiveCircuit[ActiveActor]->Solution->Algorithm)
					{
					case 	NORMALSOLVE:
						AppendGlobalResult("normal");
						break;
					case 	NEWTONSOLVE:
						AppendGlobalResult("newton");
						break;
					case 	NCIMSOLVE:
						AppendGlobalResult("ncim");
						break;
					default:
						;
						break;
					}
					break;
				case 	41:
					if (ActiveCircuit[ActiveActor]->TrapezoidalIntegration)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	42:
					/*# with ActiveCircuit[ActiveActor]->AutoAddBusList do */
				{
					auto& with1 = ActiveCircuit[ActiveActor]->AutoAddBusList;
					int stop = 0;
					for (stop = with1.Get_NumElements(), i = 1; i <= stop; i++)
					{
						AppendGlobalResult(with1.Get(i));
					}
				}
				break;
				case 	43:
					AppendGlobalResult(GetControlModeID());
					break;
				case 	44:
					if (ActiveCircuit[ActiveActor]->ControlQueue.get_DebugTrace())
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	45:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->GenMultiplier));
					break;
				case 	46:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->get_Name());
					break;
				case 	47:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->DefaultYearlyShapeObj->get_Name());
					break;
				case 	48:
					AppendGlobalResult("Get function not applicable.");
					break;
				case 	49:
					if (ActiveCircuit[ActiveActor]->PositiveSequence)
						AppendGlobalResult("positive");
					else
						AppendGlobalResult("multiphase");
					break;
				case 	50:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->PriceSignal));
					break;
				case 	51:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->PriceCurve);
					break;
				case 	52:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->get_FActiveCktElement()->get_FActiveTerminal()));
					break;
				case 	53:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Fundamental));
					break;
				case 	54:
					/*# with ActiveCircuit[ActiveActor]->Solution do */
				{
					auto with2 = ActiveCircuit[ActiveActor]->Solution;
					if (with2->DoAllHarmonics)
						AppendGlobalResult("ALL");
					else
					{
						int stop = 0;
						for (stop = with2->HarmonicListSize, i = 1; i <= stop; i++)
						{
							AppendGlobalResult(Format("%-g", with2->HarmonicList[i]));
						}
					}
				}
				break;
				case 	55:
					AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor]->Solution->MaxControlIterations));
					break;
				case 	56:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->BusList.Get(ActiveCircuit[ActiveActor]->ActiveBusIndex + 1));
					break;
				case 	57:
					AppendGlobalResult(DataDirectory[ActiveActor]);
					break; // NOTE - not necessarily output directory
				case 	58:
					/*# with ActiveCircuit[ActiveActor] do */
				{
					auto with33 = ActiveCircuit[ActiveActor];
					int stop = 0;
					for (stop = with33->NumBuses, i = 1; i <= stop; i++)
					{
						if (with33->Buses[i - 1]->Keep)
							AppendGlobalResult(with33->BusList.Get(i));
					}
				}
				break;
				case 	59:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->ReductionStrategyString);
					break;
				case 	60:
					if (EnergyMeterClass[ActiveActor]->Get_SaveDemandInterval(ActiveActor))
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	61:
					AppendGlobalResult(Format("%-.g", ActiveCircuit[ActiveActor]->PctNormalFactor));
					break;
				case 	62:
					if (EnergyMeterClass[ActiveActor]->Get_DI_Verbose(ActiveActor))
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	63:
					AppendGlobalResult(ActiveCircuit[ActiveActor]->get_FCaseName());
					break;
				case 	64:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->NodeMarkerCode));
					break;
				case 	65:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->NodeMarkerWidth));
					break;
				case 	66:
					if (ActiveCircuit[ActiveActor]->LogEvents)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	67:
					if (DSSExecutive[ActiveActor]->get_FRecorderOn())
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	68:
					if (EnergyMeterClass[ActiveActor]->Do_OverloadReport)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	69:
					if (EnergyMeterClass[ActiveActor]->Do_VoltageExceptionReport)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	70:
					AppendGlobalResult("Get function not applicable.");
					break;
				case 	71:
					if (AutoShowExport)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	72:
					AppendGlobalResult(Format("%d", MaxAllocationIterations));
					break;
				case 	73:
					AppendGlobalResult(Format("%d", Round(DefaultBaseFreq)));
					break;
				case 	74:
					if (ActiveCircuit[ActiveActor]->MarkSwitches)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	75:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->SwitchMarkerCode));
					break;
				case 	76:
					AppendGlobalResult(Format("%-.6g", DaisySize));
					break;
				case 	77:
					if (ActiveCircuit[ActiveActor]->MarkTransformers)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	78:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->TransMarkerCode));
					break;
				case 	79:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->TransMarkerSize));
					break;
				case 	80:
					AppendGlobalResult(GetActiveLoadShapeClass());
					break;
				case 	81:
					AppendGlobalResult(GetEarthModel(DefaultEarthModel));
					break;
				case 	82:
					if (LogQueries)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	83:
					if (ActiveCircuit[ActiveActor]->MarkCapacitors)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	84:
					if (ActiveCircuit[ActiveActor]->MarkRegulators)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	85:
					if (ActiveCircuit[ActiveActor]->MarkPVSystems)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	86:
					if (ActiveCircuit[ActiveActor]->MarkStorage)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	87:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->CapMarkerCode));
					break;
				case 	88:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->RegMarkerCode));
					break;
				case 	89:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->PVMarkerCode));
					break;
				case 	90:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->StoreMarkerCode));
					break;
				case 	91:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->CapMarkerSize));
					break;
				case 	92:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->RegMarkerSize));
					break;
				case 	93:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->PVMarkerSize));
					break;
				case 	94:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->StoreMarkerSize));
					break;
				case 	95:
					if (ActiveCircuit[ActiveActor]->NeglectLoadY)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	96:
					if (ActiveCircuit[ActiveActor]->MarkFuses)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	97:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->FuseMarkerCode));
					break;
				case 	98:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->FuseMarkerSize));
					break;
				case 	99:
					if (ActiveCircuit[ActiveActor]->MarkReclosers)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	100:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->RecloserMarkerCode));
					break;
				case 	101:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->RecloserMarkerSize));
					break;
				case 	102:
					UpdateRegistry = InterpretYesNo(Param);
					break;
				case 	103:
					if (ActiveCircuit[ActiveActor]->MarkRelays)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	104:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->RelayMarkerCode));
					break;
				case 	105:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->RelayMarkerSize));
					break;
				case 	106:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->get_Solve_Time_Elapsed()));
					break;
				case 	107:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->get_Total_Time_Elapsed()));
					break;
				case 	108:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->get_Step_Time_Elapsed()));
					break;
				case 	109:
					AppendGlobalResult(Format("%d", CPU_Cores));
					break;
				case 	110:
					AppendGlobalResult(Format("%d", CPU_Physical));
					break;
				case 	111:
					AppendGlobalResult(Format("%d", NumOfActors));
					break;
				case 	112:
				{
					if (AllActors)
						AppendGlobalResult("All");
					else
						AppendGlobalResult(Format("%d", ActiveActor));
				}
				break;
				case 	113:
					AppendGlobalResult(Format("%d", ActorCPU[ActiveActor]));
					break;
				case 	114:
				{
					std::string myProgress = "";
					for (int idx = 1; idx <= NumOfActors; idx++)
						myProgress = myProgress + to_string(ActorPctProgress[idx]) + ", ";
					CoutLn(myProgress);
				}
				//ScriptEd->UpdateProgressSummary();
				break;
				case 	115:
					if (Parallel_enabled)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	116:
					if (ConcatenateReports)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	117:
					if (DSS_Viz_enable)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	118:
					if (DSS_Viz_installed)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	119:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Actual_Coverage));
					break;
				case 	120:
					AppendGlobalResult(Format("%d", ActiveCircuit[ActiveActor]->Num_SubCkts));
					break;
				case 	121:
					if (ActiveCircuit[ActiveActor]->Solution->SampleTheMeters)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	122:
					if (ADiakoptics)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	123:
					AppendGlobalResult(IntToStr(ActiveCircuit[ActiveActor]->Solution->MinIterations));
					break;
				case 	124:
				{
					int stop = 0;
					ActiveActor = 1;
					for (stop = (ActiveCircuit[ActiveActor]->Link_Branches.size() - 1), i = 0; i <= stop; i++)
					{
						AppendGlobalResult(ActiveCircuit[ActiveActor]->Link_Branches[i]);
					}
				}
				break;
				case 	125:
					if (ActiveCircuit[ActiveActor]->ReduceLateralsKeepLoad)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	126:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->ReductionZmag));
					break;
				case 	127:
					if (SeasonalRating)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	128:
					AppendGlobalResult(SeasonSignal);
					break;
				case 	129:
					//AppendGlobalResult(Format("%d", NumNUMA))));
					break;
				case 	130:
					if (DSS_GIS_installed)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	134:
					if (UseUserLinks)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case	135: 
					GlobalResult = GetLineTypes();
					break;
				case	136: 
				{
					if (EventLogDefault)
						AppendGlobalResult("Yes");
					else 
						AppendGlobalResult("No");
				}
				break;
				case	137: 
				{
					if (ActiveCircuit[ActiveActor]->LongLineCorrection)
						AppendGlobalResult("Yes");
					else 
						AppendGlobalResult("No");
				}
				break;
				case	138: 
				{
					if (AutoDisplayShowReport)
						AppendGlobalResult("Yes");
					else 
						AppendGlobalResult("No");
				}
				break;
				case 	139:
					if (ActiveCircuit[ActiveActor]->Solution->IgnoreQLimit)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
				case 	140:
					AppendGlobalResult(Format("%-g", ActiveCircuit[ActiveActor]->Solution->GenGainNCIM));
					break;
                case 141:
					{
						auto with4 = ActiveCircuit[ActiveActor];
						Parser[ActiveActor]->GetNextParam();
                        string TmpStr = Parser[ActiveActor]->MakeString_();
                        string VarPCE[3] = { "generator", "storage", "windgen" };
                        bool ValidObj = false;
                        int i = 0;

						TmpStr = LowerCase(StripExtension(TmpStr));
                        for (i = 0; i < 3; i++)
                            ValidObj = ValidObj || (VarPCE[i] == TmpStr);

						if (ValidObj)
                        {
                            Parser[ActiveActor]->GetNextParam();
                            TmpStr = LowerCase(Parser[ActiveActor]->MakeString_());
                            // Search for the variable within the object
                            ValidObj = false;
                            for (i = 1; i <= ((TPCElement*)with4->FActiveCktElement)->NumStateVars; i++)
                            {
                                if (LowerCase(((TPCElement*)with4->FActiveCktElement)->VariableName(i)) == TmpStr)
								{
                                    ValidObj = true;
                                    break;
								}
                            }
                            if (ValidObj)
                                AppendGlobalResult(Format("%-g", ((TPCElement*)with4->FActiveCktElement)->VariableName(i)));
                            else
                                DoSimpleMsg("State variable " + TmpStr + " not found", 7102);
                        }
                        else
                            DoSimpleMsg("Object " + TmpStr + " is not a valid element for this command. Only generators, storage and WindGen.", 7101);
					}
					break;
					// Ignore excess parameters
				default:
					;
					break;
				}
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = Parser[ActiveActor]->MakeString_();
			} /*WHILE*/
		}
		catch (...)
		{
			AppendGlobalResult("***Error***");
		}
		return result;
	}

	// Get DSS Options Reguest and put it in Global Result string
	// may be retrieved by Result property of the DSSText interface


	//----------------------------------------------------------------------------

	bool DoGetCmd_NoCircuit()
	{
		bool result = false;
		int ParamPointer = 0;
		int i = 0;
		String ParamName;
		String Param;
		result = false;
		try
		{
			GlobalResult = "";  //initialize for appending

		 // Continue parsing command line
			ParamName = Parser[ActiveActor]->GetNextParam();
			Param = Parser[ActiveActor]->MakeString_();
		 // there will be no named paramters in this command and the params
		 // themselves will be the parameter name to return
			while(Param.size() > 0)
			{
				ParamPointer = OptionList->Getcommand(Param);
				switch(ParamPointer)
				{
					case 	109:
					AppendGlobalResult(to_string(CPU_Cores));
					break;
					case 	110:
					{
						if(CPU_Cores < 64)
							AppendGlobalResult(to_string(CPU_Cores / 2));
						else
							AppendGlobalResult(to_string(CPU_Cores));
					}
					break;
					case 	111:
					AppendGlobalResult(to_string(NumOfActors));
					break;
					case 	112:
					{
						if(AllActors)
							AppendGlobalResult("All");
						else
							AppendGlobalResult(to_string(ActiveActor));
					}
					break;
					case 	113:
					AppendGlobalResult(to_string(ActorCPU[ActiveActor]));
					break;
					case 	115:
					if(Parallel_enabled)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
					case 	116:
					if(ConcatenateReports)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
					case 	117:
					if(DSS_Viz_enable)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
					case 	118:
					if(DSS_Viz_installed)
						AppendGlobalResult("Yes");
					else
						AppendGlobalResult("No");
					break;
					default:
					DoSimpleMsg("You must create a new circuit object first: \"new circuit.mycktname\" to execute this Set command.", 301);
					result = false;  // Indicate that we could not process all set command
					return result;
					break;
				}
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = Parser[ActiveActor]->MakeString_();
			} /*WHILE*/
		}
		catch(...)
		{
			AppendGlobalResult("***Error***");
		}
		return result;
	}

	// Get DSS Options Reguest and put it in Global Result string
	// may be retrieved by Result property of the DSSText interface

	void DisposeStrings()
	{
		delete[] ExecOption;
		delete[] OptionHelp;
	}


	void ExecOptions_initialization()
	{
		DefineOptions();
	}

	void ExecOptions_finalization()
	{
		DisposeStrings();
	}

	class 		ExecOptions_unit
	{
	public:
	ExecOptions_unit()
	{
		//AssertSystemInitialization();
		ExecOptions_initialization();
	}
	~		ExecOptions_unit(){ExecOptions_finalization(); }
	};
	ExecOptions_unit _ExecOptions_unit;

}  // namespace ExecOptions




