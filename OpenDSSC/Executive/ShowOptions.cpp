

#pragma hdrstop

#include "ShowOptions.h"



using namespace std;
using namespace Command;
using namespace DSSGlobals;
using namespace LineUnits;
using namespace Monitor;
using namespace ParserDel;
using namespace System;
using namespace Utilities;

namespace ShowOptions
{

	TCommandList* ShowCommands = nullptr;
	std::string* ShowOption;
	std::string* ShowHelp;

	void DefineOptions()
	{
		ShowOption = new string[NumShowOptions];
		ShowHelp = new string[NumShowOptions];

		ShowOption[1 - 1] = "autoadded";
		ShowOption[2 - 1] = "buses";
		ShowOption[3 - 1] = "currents";
		ShowOption[4 - 1] = "convergence";
		ShowOption[5 - 1] = "elements";
		ShowOption[6 - 1] = "faults";
		ShowOption[7 - 1] = "isolated";
		ShowOption[8 - 1] = "generators";
		ShowOption[9 - 1] = "meters";
		ShowOption[10 - 1] = "monitor";
		ShowOption[11 - 1] = "pane";
		ShowOption[12 - 1] = "powers";
		ShowOption[13 - 1] = "voltages";
		ShowOption[14 - 1] = "zone";
		ShowOption[15 - 1] = "taps";
		ShowOption[16 - 1] = "overloads";
		ShowOption[17 - 1] = "unserved";
		ShowOption[18 - 1] = "eventlog";
		ShowOption[19 - 1] = "variables";
		ShowOption[20 - 1] = "ratings";
		ShowOption[21 - 1] = "loops";
		ShowOption[22 - 1] = "losses";
		ShowOption[23 - 1] = "busflow";
		ShowOption[24 - 1] = "lineconstants";
		ShowOption[25 - 1] = "yprim";
		ShowOption[26 - 1] = "y";
		ShowOption[27 - 1] = "controlqueue";
		ShowOption[28 - 1] = "topology";
		ShowOption[29 - 1] = "mismatch";
		ShowOption[30 - 1] = "kvbasemismatch";
		ShowOption[31 - 1] = "deltaV";
		ShowOption[32 - 1] = "QueryLog";
		ShowOption[33 - 1] = "Controlled";
		ShowOption[34 - 1] = "Result";
		ShowOption[35 - 1] = "PV2PQ_Conversions";

		ShowHelp[1 - 1] = "Shows auto added capacitors or generators. See AutoAdd solution mode.";
		ShowHelp[2 - 1] = "Report showing all buses and nodes currently defined.";
		ShowHelp[3 - 1] = String("Report showing currents from most recent solution. syntax: ") + CRLF
				   + CRLF
				   + "Show Currents  [[residual= - 1]yes|no* - 1] [Seq* | Elements - 1]"
				   + CRLF
				   + CRLF
				   + "If \"residual\" flag is yes, the sum of currents in all conductors is reported. "
				   + "Default is to report Sequence currents; otherwise currents in all conductors are reported.";
		ShowHelp[4 - 1] = "Report on the convergence of each node voltage.";
		ShowHelp[5 - 1] = String("Shows names of all elements in circuit or all elements of a specified class. Syntax: ") + CRLF
				   + CRLF
				   + "Show ELements [Classname - 1] "
				   + CRLF
				   + CRLF
				   + "Useful for creating scripts that act on selected classes of elements. ";
		ShowHelp[6 - 1] = "After fault study solution, shows fault currents.";
		ShowHelp[7 - 1] = "Report showing buses and elements that are isolated from the main source.";
		ShowHelp[8 - 1] = String("Report showing generator elements currently defined and the values of the energy meters ") + CRLF
				   + "associated with each generator.";
		ShowHelp[9 - 1] = "Shows the present values of the registers in the EnergyMeter elements.";
		ShowHelp[10 - 1] = String("Shows the contents of a selected monitor. Syntax: ") + CRLF
				   + CRLF
				   + " Show Monitor  monitorname";
		ShowHelp[11 - 1] = "Shows control panel. (not necessary for standalone version)";
		ShowHelp[12 - 1] = String("Report on powers flowing in circuit from most recent solution. ") + CRLF
				   + "Powers may be reported in kVA or MVA and in sequence quantities or in every "
				   + "conductor of each element. Syntax:"
				   + CRLF
				   + CRLF
				   + "Show Powers [MVA|kVA* - 1] [Seq* | Elements - 1]"
				   + CRLF
				   + CRLF
				   + "Sequence powers in kVA is the default. Examples:"
				   + CRLF
				   + CRLF
				   + "Show powers"
				   + CRLF
				   + "Show power kva element"
				   + CRLF
				   + "Show power mva elem";
		ShowHelp[13 - 1] = String("Reports voltages from most recent solution. Voltages are reported with respect to ") + CRLF
				   + "system reference (Node 0) by default (LN option), but may also be reported Line-Line (LL option)."
				   + CRLF
				   + "The voltages are normally reported by bus/node, but may also be reported by circuit element. Syntax:"
				   + CRLF
				   + CRLF
				   + "Show Voltages [LL |LN* - 1]  [Seq* | Nodes | Elements - 1]"
				   + CRLF
				   + CRLF
				   + "Show Voltages"
				   + CRLF
				   + "Show Voltage LN Nodes"
				   + CRLF
				   + "Show Voltages LL Nodes"
				   + CRLF
				   + "Show Voltage LN Elem";
		ShowHelp[14 - 1] = String("Shows the zone for a selected EnergyMeter element. Shows zone either in " "a text file or in a graphical tree view.") + CRLF
				   + CRLF
				   + "Show Zone  energymetername [Treeview - 1]";
		ShowHelp[15 - 1] = "Shows the regulator/LTC taps from the most recent solution.";
		ShowHelp[16 - 1] = "Shows overloaded power delivery elements.";
		ShowHelp[17 - 1] = String("Shows loads that are \"unserved\". That is, loads for which the voltage is too low, " "or a branch on the source side is overloaded. If UEonly is specified, shows only those loads " "in which the emergency rating has been exceeded. Syntax:") + CRLF
				   + CRLF
				   + "Show Unserved [UEonly - 1] (unserved loads)";
		ShowHelp[18 - 1] = "Shows the present event log. (Regulator tap changes, capacitor switching, etc.)";
		ShowHelp[19 - 1] = "Shows internal state variables of devices (Power conversion elements) that report them.";
		ShowHelp[20 - 1] = "Shows ratings of power delivery elements.";
		ShowHelp[21 - 1] = "Shows closed loops detected by EnergyMeter elements that are possibly unwanted. Otherwise, loops are OK.";
		ShowHelp[22 - 1] = "Reports losses in each element and in the entire circuit.";
		ShowHelp[23 - 1] = String("Creates a report showing power and current flows as well as voltages around a selected bus. Syntax:") + CRLF
				   + CRLF
				   + "Show BUSFlow busname [MVA|kVA* - 1] [Seq* | Elements - 1]"
				   + CRLF
				   + CRLF
				   + "Show busflow busxxx kVA elem"
				   + CRLF
				   + "Show busflow busxxx MVA seq"
				   + CRLF
				   + CRLF
				   + "NOTE: The Show menu will prompt you for these values.";
		ShowHelp[24 - 1] = String("Creates two report files for the line constants (impedances) of every LINEGEOMETRY element currently defined. " "One file shows the main report with the matrices. The other file contains corresponding LINECODE " "definitions that you may use in subsequent simulations.  Syntax:") + CRLF
				   + CRLF
				   + "Show LIneConstants [frequency - 1] [none|mi|km|kft|m|me|ft|in|cm - 1] [rho - 1]"
				   + CRLF
				   + CRLF
				   + "Specify the frequency, length units and earth resistivity (meter-ohms). Examples:"
				   + CRLF
				   + CRLF
				   + "Show Lineconstants 60 kft 100"
				   + CRLF
				   + "Show Linecon 50 km 1000";
		ShowHelp[25 - 1] = "Show the primitive admittance (y) matrix for the active element.";
		ShowHelp[26 - 1] = "Show the system Y matrix. Could be a large file!";
		ShowHelp[27 - 1] = "Shows the present contents of the control queue.";
		ShowHelp[28 - 1] = "Shows the topology as seen by the SwtControl elements.";
		ShowHelp[29 - 1] = "Shows the current mismatches at each node in amperes and percent of max currents at node.";
		ShowHelp[30 - 1] = "Creates a report of Load and Generator elements for which the base voltage does not match the Bus base voltage. "
				   "Scripts for correcting the voltage base are suggested.";
		ShowHelp[31 - 1] = "Show voltages ACROSS each 2-terminal element, phase-by-phase. ";
		ShowHelp[32 - 1] = "Show Query Log file. ";
		ShowHelp[33 - 1] = "Show Controlled elements and the names of the controls connected to them in CSV format.";
		ShowHelp[34 - 1] = "Show last result (in @result variable).";
		ShowHelp[35 - 1] = "Show the list of generators converted from PV bus to PQ during the last soltuion step (NCIM).";
	}


	//----------------------------------------------------------------------------

	int DoShowCmd()
	{
		int result = 0;
		String ParamName;
		String Param;
		String FilName;
		int ParamPointer = 0;
		TMonitorObj* pMon = nullptr;
		int MVAopt = 0;
		bool LLopt = false;
		bool ShowResid = false;
		int ShowOptionCode = 0;
		String Busname;
		double Freq = 0.0;
		int Units = 0;
		double Rho_line = 0.0;
		int InitP = 0;
		int FinalP = 0;
		int idxP = 0;  // Variables added to concatenate the results in OpenDSS-PM
		result = 0;
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = LowerCase(Parser[ActiveActor]->MakeString_());
		ParamPointer = ShowCommands->Getcommand(Param);
		if(ParamPointer == 0)
		{
			DoSimpleMsg(String("Error: Unknown Show Command:\"") + Param + "\"", 24700);
			return result;
		}

	   /*Check commands requiring a solution and abort if no solution or circuit*/
		switch(ParamPointer)
		{
			case 	4:
			 case 6:
			 case 8: case 9: case 10:
			 case 12:
			 case 13: case 14: case 15: case 16: case 17:
			 case 19: case 20: case 21: case 22: case 23:
			 case 29: case 30: case 31:
			{
				if(!ASSIGNED(ActiveCircuit[ActiveActor]))
				{
					DoSimpleMsg("No circuit created.", 24701);
					return result;
				}
				if(!ASSIGNED(ActiveCircuit[ActiveActor]->Solution) || (ActiveCircuit[ActiveActor]->Solution->NodeV.empty()))
				{
					DoSimpleMsg("The circuit must be solved before you can do this.", 24702);
					return result;
				}
			}
			break;
			default:
			  ;
			break;
		}
		InShowResults = true;
		switch(ParamPointer)
		{
			case 	1: /*Autoadded*/
			{
				FireOffEditor(GetOutputDirectory() + CircuitName_[ActiveActor] + "AutoAddedGenerators.Txt");
				FireOffEditor(GetOutputDirectory() + CircuitName_[ActiveActor] + "AutoAddedCapacitors.Txt");
			}
			break;
			case 	2:
			ShowBuses(GetOutputDirectory() + CircuitName_[ActiveActor] + "Buses.Txt");
			break;
			case 	3:
			{
				ShowOptionCode = 0;
				ShowResid = false;
				ParamName = Parser[ActiveActor]->GetNextParam();   // Look for residual
				Param = UpperCase(Parser[ActiveActor]->MakeString_());
			   // logic handles show curr y|n|T elements or show curr elements
				if(Param.size() > 0)
					switch(Param[0])
					{
						case 	L'Y':
						 case L'T':
						ShowResid = true;
						break;
						case 	L'N':
						ShowResid = false;
						break;
						case 	L'E':
						ShowOptionCode = 1;
						break;
						default:
						  ;
						break;
					}
				ParamName = Parser[ActiveActor]->GetNextParam();   // Look for another param
				Param = UpperCase(Parser[ActiveActor]->MakeString_());
				if(Param.size() > 0)
					switch(Param[0])
					{
						case 	L'E':
						ShowOptionCode = 1;
						break;
						default:
						  ;
						break;
					}
				switch(ShowOptionCode)
				{
					case 	0:
					FilName = "Curr_Seq";
					break;
					case 	1:
					FilName = "Curr_Elem";
					break;
					default:
					  ;
					break;
				}
				ShowCurrents(GetOutputDirectory() + CircuitName_[ActiveActor] + FilName + ".Txt", ShowResid, ShowOptionCode);
			}
			break;
			case 	4:
			ActiveCircuit[ActiveActor]->Solution->WriteConvergenceReport(GetOutputDirectory() + CircuitName_[ActiveActor] + "Convergence.TXT");
			break;
			case 	5:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();   // Look for another param
				Param = LowerCase(Parser[ActiveActor]->MakeString_());
				ShowElements(GetOutputDirectory() + CircuitName_[ActiveActor] + "Elements.Txt", Param);
			}
			break;
			case 	6:
			ShowFaultStudy(GetOutputDirectory() + CircuitName_[ActiveActor] + "FaultStudy.Txt");
			break;
			case 	7:
			ShowIsolated(GetOutputDirectory() + CircuitName_[ActiveActor] + "Isolated.Txt");
			break;
			case 	8:
			ShowGenMeters(GetOutputDirectory() + CircuitName_[ActiveActor] + "GenMeterOut.Txt");
			break;
			case 	9:
			ShowMeters(GetOutputDirectory() + CircuitName_[ActiveActor] + "EMout.Txt");
			break;     // Show Monitor
			case 	10:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = Parser[ActiveActor]->MakeString_();
				if(Param.size() > 0)
				{
					int stop = 0;
					if(ConcatenateReports) // In case of being activated, the export will be made for all actors
					{
						InitP = 1;
						FinalP = NumOfActors;
					}
					else
						  // Otherwise just for the active actor monitor
					{
						InitP = ActiveActor;
						FinalP = ActiveActor;
					}
					for(stop = FinalP, idxP = InitP; idxP <= stop; idxP++)
					{
						pMon = (TMonitorObj*) MonitorClass[idxP]->Find(Param);
						if(pMon != nullptr)
							pMon->TranslateToCSV((idxP == FinalP), idxP);
						else
							DoSimpleMsg(String("Monitor \"") + Param
				   + "\" not found."
				   + CRLF
				   + Parser[ActiveActor]->get_CmdBuffer(), 248);
					}
				}
				else
				DoSimpleMsg(String("Monitor Name Not Specified.") + CRLF
				   + Parser[ActiveActor]->get_CmdBuffer(), 249);
			}
			break;
			case 	11:
			ShowControlPanel();
			break;
			case 	12:
			{
				ShowOptionCode = 0;
				MVAopt = 0;
				FilName = "Power";
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = LowerCase(Parser[ActiveActor]->MakeString_());
				if(Param.size() > 0)
					switch(Param[0])
					{
						case 	L'm':
						MVAopt = 1;
						break;
						case 	L'e':
						ShowOptionCode = 1;
						break;
						default:
						  ;
						break;
					}
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = LowerCase(Parser[ActiveActor]->MakeString_());
				if(Param.size() > 0)
				{
					if(Param[0] == L'e')
						ShowOptionCode = 1;
				}
				if(ShowOptionCode == 1)
					FilName = FilName + "_elem";
				else
					FilName = FilName + "_seq";
				if(MVAopt == 1)
					FilName = FilName + "_MVA";
				else
					FilName = FilName + "_kVA";
				ShowPowers(GetOutputDirectory() + CircuitName_[ActiveActor] + FilName + ".txt", MVAopt, ShowOptionCode);
			}
			break;
			case 	13:
			{
				LLopt = false;      // Line-Line voltage option
				ShowOptionCode = 0;
				/*Check for LL or LN option*/
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = Parser[ActiveActor]->MakeString_();
				FilName = "VLN";
				if(Param.size() > 0)
				{
					if(CompareText(Param, "ll") == 0)
					{
						LLopt = true;
						FilName = "VLL";
					}
				/*Check for Seq | nodes | elements*/
				}
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = UpperCase(Parser[ActiveActor]->MakeString_());
				if(Param.size() > 0)
					switch(Param[0])
					{
						case 	L'N':
						{
							ShowOptionCode = 1;
							FilName = FilName + "_Node";
						}
						break;
						case 	L'E':
						{
							ShowOptionCode = 2;
							FilName = FilName + "_elem";
						}
						break;
						default:
						FilName = FilName + "_seq";
						break;
					}
				ShowVoltages(GetOutputDirectory() + CircuitName_[ActiveActor] + FilName + ".Txt", LLopt, ShowOptionCode);
			}
			break;
			case 	14:
			ShowMeterZone(GetOutputDirectory() + CircuitName_[ActiveActor] + "ZoneOut.Txt");
			break;
			case 	15:
			ShowRegulatorTaps(GetOutputDirectory() + CircuitName_[ActiveActor] + "RegTaps.Txt");
			break;
			case 	16:
			ShowOverloads(GetOutputDirectory() + CircuitName_[ActiveActor] + "Overload.Txt");
			break;
			case 	17:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = Parser[ActiveActor]->MakeString_();
				if(Param.size() > 0)
					ShowUnserved(GetOutputDirectory() + CircuitName_[ActiveActor] + "Unserved.Txt", true);
				else
					ShowUnserved(GetOutputDirectory() + CircuitName_[ActiveActor] + "Unserved.Txt", false);
			}
			break;
			case 	18:
			ShowEventLog(GetOutputDirectory() + CircuitName_[ActiveActor] + "EventLog.Txt");
			break;// ShowMessageForm(EventStrings);
			case 	19:
			ShowVariables(GetOutputDirectory() + CircuitName_[ActiveActor] + "Variables.Txt");
			break;
			case 	20:
			ShowRatings(GetOutputDirectory() + CircuitName_[ActiveActor] + "RatingsOut.Txt");
			break;
			case 	21:
			ShowLoops(GetOutputDirectory() + CircuitName_[ActiveActor] + "Loops.Txt");
			break;
			case 	22:
			ShowLosses(GetOutputDirectory() + CircuitName_[ActiveActor] + "Losses.Txt");
			break;  // Show Bus Power Report
			case 	23:
			{
				ShowOptionCode = 0;
				MVAopt = 0;
				ParamName = Parser[ActiveActor]->GetNextParam(); // Get busname
				Busname = Parser[ActiveActor]->MakeString_();
				if(Busname.size() > 0)
					FilName = Busname;
				else
					FilName = "BusPower";
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = LowerCase(Parser[ActiveActor]->MakeString_());
				if(Param.size() > 0)
					switch(Param[0])
					{
						case 	L'm':
						MVAopt = 1;
						break;
						case 	L'e':
						ShowOptionCode = 1;
						break;
						default:
						  ;
						break;
					}
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = LowerCase(Parser[ActiveActor]->MakeString_());
				if(Param.size() > 0)
				{
					if(Param[0] == L'e')
						ShowOptionCode = 1;
				}
				if(ShowOptionCode == 1)
					FilName = FilName + "_elem";
				else
					FilName = FilName + "_seq";
				if(MVAopt == 1)
					FilName = FilName + "_MVA";
				else
					FilName = FilName + "_kVA";
				ShowBusPowers(GetOutputDirectory() + CircuitName_[ActiveActor] + FilName + ".txt", Busname, MVAopt, ShowOptionCode);
			}
			break; /*ShowLineConstants  Show Lineconstants 60 mi*/
			case 	24:
			{
				Freq = DefaultBaseFreq;  // Default
				Units = UNITS_KFT; // 'kft'; // default
				Rho_line = 100.0;
				ParamName = Parser[ActiveActor]->GetNextParam();
				if(Parser[ActiveActor]->MakeString_().size() > 0)
					Freq = Parser[ActiveActor]->MakeDouble_();
				ParamName = Parser[ActiveActor]->GetNextParam();
				if(Parser[ActiveActor]->MakeString_().size() > 0)
					Units = GetUnitsCode(Parser[ActiveActor]->MakeString_());
				ParamName = Parser[ActiveActor]->GetNextParam();
				if(Parser[ActiveActor]->MakeString_().size() > 0)
					Rho_line = Parser[ActiveActor]->MakeDouble_();
				ShowLineConstants(GetOutputDirectory() + CircuitName_[ActiveActor] + "LineConstants.txt", Freq, Units, Rho_line);
			}
			break;
			case 	25:
			if(ActiveCircuit[ActiveActor] != nullptr)  /*Yprim*/
			{
				/*# with ActiveCircuit[ActiveActor].ActiveCktElement do */
				{
					auto with0 = (TDSSObject*) ActiveCircuit[ActiveActor]->get_FActiveCktElement();
					ShowYPrim(GetOutputDirectory() + with0->ParentClass->get_myClass_name() + "_" + with0->get_Name() + "_Yprim.txt");
				}
			}
			break;   /*Y*/
			case 	26:
			{
				ShowY(GetOutputDirectory() + CircuitName_[ActiveActor] + "SystemY.txt");
			}
			break;
			case 	27:
			if(ActiveCircuit[ActiveActor] != nullptr)
				ActiveCircuit[ActiveActor]->ControlQueue.ShowQueue(GetOutputDirectory() + CircuitName_[ActiveActor] + "ControlQueue" + RepTermination);
			break;
			case 	28:
			ShowTopology(GetOutputDirectory() + CircuitName_[ActiveActor]);
			break;
			case 	29:
			ShowNodeCurrentSum(GetOutputDirectory() + CircuitName_[ActiveActor] + "NodeMismatch.Txt");
			break;
			case 	30:
			ShowkVBaseMismatch(GetOutputDirectory() + CircuitName_[ActiveActor] + "kVBaseMismatch.Txt");
			break;
			case 	31:
			ShowDeltaV(GetOutputDirectory() + CircuitName_[ActiveActor] + "DeltaV.Txt");
			break;
			case 	32:
			FireOffEditor(QueryLogFileName);
			break;
			case 	33:
			ShowControlledElements(GetOutputDirectory() + CircuitName_[ActiveActor] + "ControlledElements" + RepTermination);
			break;
			case 	34:
			ShowResult(GetOutputDirectory() + CircuitName_[ActiveActor] + "Result" + RepTermination);
			break;
			case	35:
				ShowPV2PQGen(GetOutputDirectory() + CircuitName_[ActiveActor] + "PV2PQ_Generators.Txt");
			break;
			default:
			  ;
			break;
		}
		InShowResults = false;
		return result;
	}

	void DisposeStrings()
	{
		delete[] ShowOption;
		delete[] ShowHelp;
	}


	void ShowOptions_initialization()
	{
		DefineOptions();
		ShowCommands = new TCommandList(ShowOption, NumShowOptions);
		ShowCommands->set_AbbrevAllowed(true);
	}

	void ShowOptions_finalization()
	{
		DisposeStrings();
		delete ShowCommands;
	}

		class 		ShowOptions_unit
		{
		public:
		ShowOptions_unit()
		{
			//AssertSystemInitialization();
			ShowOptions_initialization();
		}
		~		ShowOptions_unit(){ShowOptions_finalization(); }
		};
		ShowOptions_unit _ShowOptions_unit;

}  // namespace ShowOptions




