

#pragma hdrstop

#include "ExportOptions.h"



using namespace std;
using namespace Command;
using namespace DSSGlobals;
using namespace EnergyMeter;
using namespace Monitor;
using namespace NamedObject;
using namespace ParserDel;
using namespace System;
using namespace Utilities;

namespace ExportOptions
{

	TCommandList* ExportCommands = nullptr;
	std::string* ExportOption;
	std::string* ExportHelp;

	TUuid AssignNewUUID(String Val)
	{
		TUuid result = {};
		if(Pos("{", Val) < 1)
			Val = String("{") + Val + "}";
		result = StringToUUID(Val);
		return result;
	}

	void DefineOptions()
	{
		ExportOption = new std::string[NumExportOptions];
		ExportHelp = new std::string[NumExportOptions];

		ExportOption[1 - 1] = "Voltages";
		ExportOption[2 - 1] = "SeqVoltages";
		ExportOption[3 - 1] = "Currents";
		ExportOption[4 - 1] = "SeqCurrents";
		ExportOption[5 - 1] = "Estimation";
		ExportOption[6 - 1] = "Capacity";
		ExportOption[7 - 1] = "Overloads";
		ExportOption[8 - 1] = "Unserved";
		ExportOption[9 - 1] = "Powers";
		ExportOption[10 - 1] = "SeqPowers";
		ExportOption[11 - 1] = "Faultstudy";
		ExportOption[12 - 1] = "Generators";
		ExportOption[13 - 1] = "Loads";
		ExportOption[14 - 1] = "Meters";
		ExportOption[15 - 1] = "Monitors";
		ExportOption[16 - 1] = "Yprims";
		ExportOption[17 - 1] = "Y";
		ExportOption[18 - 1] = "seqz";
		ExportOption[19 - 1] = "P_byphase";
		ExportOption[20 - 1] = "CIM100Fragments";
		ExportOption[21 - 1] = "CIM100";
		ExportOption[22 - 1] = "CDPSMAsset";
		ExportOption[23 - 1] = "Buscoords";
		ExportOption[24 - 1] = "Losses";
		ExportOption[25 - 1] = "Uuids";
		ExportOption[26 - 1] = "Counts";
		ExportOption[27 - 1] = "Summary";
		ExportOption[28 - 1] = "CDPSMElec";
		ExportOption[29 - 1] = "CDPSMGeo";
		ExportOption[30 - 1] = "CDPSMTopo";
		ExportOption[31 - 1] = "CDPSMStateVar";
		ExportOption[32 - 1] = "Profile";
		ExportOption[33 - 1] = "EventLog";
		ExportOption[34 - 1] = "AllocationFactors";
		ExportOption[35 - 1] = "VoltagesElements";
		ExportOption[36 - 1] = "GICMvars";
		ExportOption[37 - 1] = "BusReliability";
		ExportOption[38 - 1] = "BranchReliability";
		ExportOption[39 - 1] = "NodeNames";
		ExportOption[40 - 1] = "Taps";
		ExportOption[41 - 1] = "NodeOrder";
		ExportOption[42 - 1] = "ElemCurrents";
		ExportOption[43 - 1] = "ElemVoltages";
		ExportOption[44 - 1] = "ElemPowers";
		ExportOption[45 - 1] = "Result";
		ExportOption[46 - 1] = "YNodeList";
		ExportOption[47 - 1] = "YVoltages";
		ExportOption[48 - 1] = "YCurrents";
		ExportOption[49 - 1] = "PVSystem_Meters";
		ExportOption[50 - 1] = "Storage_Meters";
		ExportOption[51 - 1] = "Sections";
		ExportOption[52 - 1] = "ErrorLog";
		ExportOption[53 - 1] = "IncMatrix";
		ExportOption[54 - 1] = "IncMatrixRows";
		ExportOption[55 - 1] = "IncMatrixCols";
		ExportOption[56 - 1] = "BusLevels";
		ExportOption[57 - 1] = "Laplacian";
		ExportOption[58 - 1] = "ZLL";
		ExportOption[59 - 1] = "ZCC";
		ExportOption[60 - 1] = "Contours";
		ExportOption[61 - 1] = "Y4";
		ExportOption[62 - 1] = "Jacobian";
		ExportOption[63 - 1] = "deltaF";
		ExportOption[64 - 1] = "deltaZ";

		ExportHelp[1 - 1] = "(Default file = EXP_VOLTAGES.CSV) Voltages to ground by bus/node.";
		ExportHelp[2 - 1] = "(Default file = EXP_SEQVOLTAGES.CSV) Sequence voltages.";
		ExportHelp[3 - 1] = "(Default file = EXP_CURRENTS.CSV) Currents in each conductor of each element.";
		ExportHelp[4 - 1] = "(Default file = EXP_SEQCURRENTS.CSV) Sequence currents in each terminal of 3-phase elements.";
		ExportHelp[5 - 1] = "(Default file = EXP_ESTIMATION.CSV) Results of last estimation.";
		ExportHelp[6 - 1] = "(Default file = EXP_CAPACITY.CSV) Capacity report.";
		ExportHelp[7 - 1] = "(Default file = EXP_OVERLOADS.CSV) Overloaded elements report.";
		ExportHelp[8 - 1] = "(Default file = EXP_UNSERVED.CSV) [UEonly - 1] [Filename - 1] Report on elements that are unserved due to violation of ratings.";
		ExportHelp[9 - 1] = "(Default file = EXP_POWERS.CSV) [MVA - 1] [Filename - 1] Powers (kVA by default) into each terminal of each element.";
		ExportHelp[10 - 1] = "(Default file = EXP_SEQPOWERS.CSV) Sequence powers into each terminal of 3-phase elements.";
		ExportHelp[11 - 1] = "(Default file = EXP_FAULTS.CSV) results of a fault study.";
		ExportHelp[12 - 1] = "(Default file = EXP_GENMETERS.CSV) Present values of generator meters. Adding the switch \"/multiple\" or \"/m\" will "
				   " cause a separate file to be written for each generator.";
		ExportHelp[13 - 1] = "(Default file = EXP_LOADS.CSV) Report on loads from most recent solution.";
		ExportHelp[14 - 1] = "(Default file = EXP_METERS.CSV) Energy meter exports. Adding the switch \"/multiple\" or \"/m\" will "
				   " cause a separate file to be written for each meter.";
		ExportHelp[15 - 1] = String("(file name is assigned by Monitor export) Monitor values. The argument is the name of the monitor (e.g. Export Monitor XYZ, XYZ is the name of the monitor).") + CRLF
				   + "The argument can be ALL, which means that all the monitors will be exported";
		ExportHelp[16 - 1] = "(Default file = EXP_YPRIMS.CSV) All primitive Y matrices.";
		ExportHelp[17 - 1] = "(Default file = EXP_Y.CSV) [triplets - 1] [Filename - 1] System Y matrix, defaults to non-sparse format.";
		ExportHelp[18 - 1] = "(Default file = EXP_SEQZ.CSV) Equivalent sequence Z1, Z0 to each bus.";
		ExportHelp[19 - 1] = "(Default file = EXP_P_BYPHASE.CSV) [MVA - 1] [Filename - 1] Power by phase. Default is kVA.";
		ExportHelp[20 - 1] = String("(Default file ROOT = CIM100) (IEC 61968-13, CIM100 for unbalanced load flow profile)") + CRLF + 
			" produces 6 separate files ROOT_FUN.XML for Functional profile,' + CRLF + ' ROOT_EP.XML for Electrical Properties profile," + CRLF + 
			" ROOT_TOPO.XML for Topology profile," + CRLF + 
			" ROOT_CAT.XML for Asset Catalog profile," + CRLF + 
			" ROOT_GEO.XML for Geographical profile and" + CRLF + 
			" ROOT_SSH.XML for Steady State Hypothesis profile" + CRLF + 
			" [File=fileroot fid=_uuidstring Substation=subname sid=_uuidstring" + CRLF + 
			" SubGeographicRegion=subgeoname sgrid=_uuidstring GeographicRegion=geoname rgnid=_uuidstring]";
		ExportHelp[21 - 1] = String("(Default file = CIM100x.XML) (IEC 61968-13, combined CIM100 for unbalanced load flow profile)") + CRLF +
			" [File=filename fid=_uuidstring Substation=subname sid=_uuidstring" + CRLF +
			" SubGeographicRegion=subgeoname sgrid=_uuidstring GeographicRegion=geoname rgnid=_uuidstring]";
		ExportHelp[22 - 1] = "** Deprecated ** (IEC 61968-13, CDPSM Asset profile)";
		ExportHelp[23 - 1] = "[Default file = EXP_BUSCOORDS.CSV - 1] Bus coordinates in csv form.";
		ExportHelp[24 - 1] = "[Default file = EXP_LOSSES.CSV - 1] Losses for each element.";
		ExportHelp[25 - 1] = "[Default file = EXP_UUIDS.CSV - 1] Uuids for each element. This frees the UUID list after export.";
		ExportHelp[26 - 1] = "[Default file = EXP_Counts.CSV - 1] (instance counts for each class)";
		ExportHelp[27 - 1] = "[Default file = EXP_Summary.CSV - 1] Solution summary.";
		ExportHelp[28 - 1] = "** Deprecated ** (IEC 61968-13, CDPSM Electrical Properties profile)";
		ExportHelp[29 - 1] = "** Deprecated ** (IEC 61968-13, CDPSM Geographical profile)";
		ExportHelp[30 - 1] = "** Deprecated ** (IEC 61968-13, CDPSM Topology profile)";
		ExportHelp[31 - 1] = "** Deprecated ** (IEC 61968-13, CDPSM State Variables profile)";
		ExportHelp[32 - 1] = String("[Default file = EXP_Profile.CSV - 1] Coordinates, color of each line section in Profile plot. Same options as Plot Profile Phases property.") + CRLF
				   + CRLF
				   + "Example:  Export Profile Phases=All [optional file name - 1]";
		ExportHelp[33 - 1] = "(Default file = EXP_EventLog.CSV) All entries in the present event log.";
		ExportHelp[34 - 1] = "Exports load allocation factors. File name is assigned.";
		ExportHelp[35 - 1] = "(Default file = EXP_VOLTAGES_ELEM.CSV) Voltages to ground by circuit element.";
		ExportHelp[36 - 1] = "(Default file = EXP_GIC_Mvar.CSV) Mvar for each GICtransformer object by bus for export to power flow programs ";
		ExportHelp[37 - 1] = "(Default file = EXP_BusReliability.CSV) Failure rate, number of interruptions and other reliability data at each bus.";
		ExportHelp[38 - 1] = "(Default file = EXP_BranchReliability.CSV) Failure rate, number of interruptions and other reliability data for each PD element.";
		ExportHelp[39 - 1] = "(Default file = EXP_NodeNames.CSV) Exports Single-column file of all node names in the active circuit. Useful for making scripts.";
		ExportHelp[40 - 1] = "(Default file = EXP_Taps.CSV)  Exports the regulator tap report similar to Show Taps.";
		ExportHelp[41 - 1] = "(Default file = EXP_NodeOrder.CSV)  Exports the present node order for all conductors of all circuit elements";
		ExportHelp[42 - 1] = "(Default file = EXP_ElemCurrents.CSV)  Exports the current into all conductors of all circuit elements";
		ExportHelp[43 - 1] = "(Default file = EXP_ElemVoltages.CSV)  Exports the voltages to ground at all conductors of all circuit elements";
		ExportHelp[44 - 1] = "(Default file = EXP_elemPowers.CSV)  Exports the powers into all conductors of all circuit elements";
		ExportHelp[45 - 1] = "(Default file = EXP_Result.CSV)  Exports the result of the most recent command.";
		ExportHelp[46 - 1] = "(Default file = EXP_YNodeList.CSV)  Exports a list of nodes in the same order as the System Y matrix.";
		ExportHelp[47 - 1] = "(Default file = EXP_YVoltages.CSV)  Exports the present solution complex Voltage array in same order as YNodeList.";
		ExportHelp[48 - 1] = "(Default file = EXP_YCurrents.CSV)  Exports the present solution complex Current array in same order as YNodeList. This is generally the injection current array";
		ExportHelp[49 - 1] = "(Default file = EXP_PVMETERS.CSV) Present values of PVSystem meters. Adding the switch \"/multiple\" or \"/m\" will "
				   " cause a separate file to be written for each PVSystem.";
		ExportHelp[50 - 1] = "(Default file = EXP_STORAGEMETERS.CSV) Present values of Storage meters. Adding the switch \"/multiple\" or \"/m\" will "
				   " cause a separate file to be written for each Storage device.";
		ExportHelp[51 - 1] = String("(Default file = EXP_SECTIONS.CSV) Data for each section between overcurrent protection devices. ") + CRLF
				   + CRLF
				   + "Examples: "
				   + CRLF
				   + "  Export Sections [optional filename - 1]"
				   + CRLF
				   + "Export Sections meter=M1 [optional filename - 1]";
		ExportHelp[52 - 1] = "(Default file = EXP_ErrorLog.TXT) All entries in the present Error log.";
		ExportHelp[53 - 1] = "Exports the Branch-to-Node Incidence matrix calculated for the circuit in compressed coordianted format (Row,Col,Value)";
		ExportHelp[54 - 1] = "Exports the names of the rows (PDElements) used for calculating the Branch-to-Node Incidence matrix for the active circuit";
		ExportHelp[55 - 1] = "Exports the names of the Cols (Buses) used for calculating the Branch-to-Node Incidence matrix for the active circuit";
		ExportHelp[56 - 1] = "Exports the names and the level of each Bus inside the Circuit based on its topology information. The level value defines"
				   "how far or close is the bus from the circuits backbone (0 means that the bus is at the backbone)";
		ExportHelp[57 - 1] = "Exports the Laplacian matrix calculated using the branch-to-node Incidence matrix in compressed coordinated format (Row,Col,Value)";
		ExportHelp[58 - 1] = "Exports the Link branches matrix (ZLL) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are complex conjugates. If A-Diakoptics is not initialized this command does nothing";
		ExportHelp[59 - 1] = "Exports the connectivity matrix (ZCC) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are complex conjugates.  If A-Diakoptics is not initialized this command does nothing";
		ExportHelp[60 - 1] = "Exports the Contours matrix (C) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are integers.  If A-Diakoptics is not initialized this command does nothing";
		ExportHelp[61 - 1] = "Exports the inverse of Z4 (ZCC) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are complex conjugates.  If A-Diakoptics is not initialized this command does nothing";
		ExportHelp[62 - 1] = "Exports the Jacobian matrix, this matrix is calculated when using the NCIM solution algorithm. The matrix is exported as triplets.";
		ExportHelp[63 - 1] = "Exports the coeficients of the vector deltaF, which is used for storing the injection current estimation when using the NCIM solution algorithm.";
		ExportHelp[64 - 1] = "Exports the coeficients of the vector deltaZ, which is used for storing the voltage delta estimation when using the NCIM solution algorithm.";
	}

	//----------------------------------------------------------------------------

	int DoExportCmd()
	{
		int result = 0;
		String ParamName;
		String Parm1;
		String Parm2;
		String FileName;
		int i = 0;
		int MVAopt = 0;
		bool UEonlyOpt = false;
		bool TripletOpt = false;
		TMonitorObj* pMon = nullptr;
		TEnergyMeterObj* pMeter = nullptr;
		int ParamPointer = 0;
		int PhasesToPlot = 0;
		bool AbortExport = false;
		String SUBSTATION;
		String GeographicRegion;
		String SubGeographicRegion; // for CIM export
		TUuid FdrUUID = {};
		TUuid SubUUID = {};
		TUuid SubGeoUUID = {};
		TUuid RgnUUID = {};              // for CIM export
		int InitP = 0;
		int FinalP = 0;
		int idxP = 0; // Variables created for concatenating options
		result = 0;
		AbortExport = false;
		ParamName = Parser[ActiveActor]->GetNextParam();
		Parm1 = LowerCase(Parser[ActiveActor]->MakeString_());
		ParamPointer = ExportCommands->Getcommand(Parm1);

	   /*Check commands requiring a solution and abort if no solution or circuit*/
		switch(ParamPointer)
		{
			case 1: case 2: case 3: case 4: case 5: case 6: case 7: case 8: case 9: case 10:
			 case 11: case 12: case 13: case 14: case 15: case 16: case 17: case 18: case 19: case 20:
			 case 21: case 22: case 23: case 24:
			 case 28: case 29: case 30: case 31: case 32:
			 case 35:
			 case 46: case 47: case 48: case 49: case 50: case 51:
			{
				if(!ASSIGNED(ActiveCircuit[ActiveActor]))
				{
					DoSimpleMsg("No circuit created.", 24711);
					return result;
				}
				if(!ASSIGNED(ActiveCircuit[ActiveActor]->Solution) || (ActiveCircuit[ActiveActor]->Solution->NodeV.empty()))
				{
					DoSimpleMsg("The circuit must be solved before you can do this.", 24712);
					return result;
				}
			}
			break;
			default:
			  ;
			break;
		}
		MVAopt = 0;
		UEonlyOpt = false;
		TripletOpt = false;
		PhasesToPlot = PROFILE3PH;  // init this to get rid of compiler warning
		pMeter = nullptr;
		SUBSTATION = String(ActiveCircuit[ActiveActor]->Get_Name()) + "_Substation";
		SubGeographicRegion = String(ActiveCircuit[ActiveActor]->Get_Name()) + "_SubRegion";
		GeographicRegion = String(ActiveCircuit[ActiveActor]->Get_Name()) + "_Region";
		DefaultCircuitUUIDs(FdrUUID, SubUUID, RgnUUID, SubGeoUUID);
		switch(ParamPointer)
		{
			case 	9:
			 case 19: /* Trap export powers command and look for MVA/kVA option */
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Parm2 = LowerCase(Parser[ActiveActor]->MakeString_());
				MVAopt = 0;
				if(Parm2.size() > 0)
				{
					if(Parm2[0] == L'm')
						MVAopt = 1;
				}
			}
			break; /* Trap UE only flag  */
			case 	8:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Parm2 = LowerCase(Parser[ActiveActor]->MakeString_());
				UEonlyOpt = false;
				if(Parm2.size() > 0)
				{
					if(Parm2[0] == L'u')
						UEonlyOpt = true;
				}
			}
			break; /*Get monitor name for export monitors command*/
			case 	15:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Parm2 = Parser[ActiveActor]->MakeString_();
			}
			break; /* Trap Sparse Triplet flag  */
			case 	17:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Parm2 = LowerCase(Parser[ActiveActor]->MakeString_());
				TripletOpt = false;
				if(Parm2.size() > 0)
				{
					if(Parm2[0] == L't')
						TripletOpt = true;
				}
			}
			break; /*user-supplied substation and regions*/
			case 	20:
			 case 21:
			{
				ParamName = LowerCase(Parser[ActiveActor]->GetNextParam());
				Parm2 = Parser[ActiveActor]->MakeString_();
				while(ParamName.size() > 0)
				{
					if(CompareTextShortest(ParamName, "subs") == 0)
						SUBSTATION = Parm2;
					else
					{
						if(CompareTextShortest(ParamName, "subg") == 0)
							SubGeographicRegion = Parm2;
						else
						{
							if(CompareTextShortest(ParamName, "g") == 0)
								GeographicRegion = Parm2;
							else
							{
								if(CompareTextShortest(ParamName, "fil") == 0)
									FileName = Parm2;
								else
								{
									if(CompareTextShortest(ParamName, "fid") == 0)
										FdrUUID = AssignNewUUID(Parm2);
									else
									{
										if(CompareTextShortest(ParamName, "sid") == 0)
											SubUUID = AssignNewUUID(Parm2);
										else
										{
											if(CompareTextShortest(ParamName, "sg") == 0)
												SubGeoUUID = AssignNewUUID(Parm2);
											else
											{
												if(CompareTextShortest(ParamName, "rg") == 0)
													RgnUUID = AssignNewUUID(Parm2);
											}
										}
									}
								}
							}
						}
					}
					ParamName = LowerCase(Parser[ActiveActor]->GetNextParam());
					Parm2 = Parser[ActiveActor]->MakeString_();
				}
			}
			break; /*Get phases to plot*/
			case 	32:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Parm2 = Parser[ActiveActor]->MakeString_();
				PhasesToPlot = PROFILE3PH; // the default
				if(CompareTextShortest(Parm2, "default") == 0)
					PhasesToPlot = PROFILE3PH;
				else
				{
					if(CompareTextShortest(Parm2, "all") == 0)
						PhasesToPlot = PROFILEALL;
					else
					{
						if(CompareTextShortest(Parm2, "primary") == 0)
							PhasesToPlot = PROFILEALLPRI;
						else
						{
							if(CompareTextShortest(Parm2, "ll3ph") == 0)
								PhasesToPlot = PROFILELL;
							else
							{
								if(CompareTextShortest(Parm2, "llall") == 0)
									PhasesToPlot = PROFILELLALL;
								else
								{
									if(CompareTextShortest(Parm2, "llprimary") == 0)
										PhasesToPlot = PROFILELLPRI;
									else
									{
										if(Parm2.size() == 1)
											PhasesToPlot = Parser[ActiveActor]->MakeInteger_();
									}
								}
							}
						}
					}
				}
			}
			break; /*Sections*/
			case 	51:
			{
				ParamName = Parser[ActiveActor]->GetNextParam();
				Parm2 = Parser[ActiveActor]->MakeString_();
				if(CompareTextShortest(ParamName, "meter") == 0)
					pMeter = ((TEnergyMeterObj*) EnergyMeterClass[ActiveActor]->Find(Parm2));
			}
			break;
			default:
			  ;
			break;
		}

	   /*Pick up next parameter on line, alternate file name, if any*/
		if(FileName.size() == 0)
		{
			ParamName = Parser[ActiveActor]->GetNextParam();
			FileName = LowerCase(Parser[ActiveActor]->MakeString_());    // should be full path name to work universally
		}
		InShowResults = true;

	   /*Assign default file name if alternate not specified*/
		if(FileName.size() == 0)
		{
			switch(ParamPointer)
			{
				case 	1:
				FileName = "EXP_VOLTAGES" + RepTermination;
				break;
				case 	2:
				FileName = "EXP_SEQVOLTAGES" + RepTermination;
				break;
				case 	3:
				FileName = "EXP_CURRENTS" + RepTermination;
				break;
				case 	4:
				FileName = "EXP_SEQCURRENTS" + RepTermination;
				break;
				case 	5:
				FileName = "EXP_ESTIMATION" + RepTermination;
				break;   // Estimation error
				case 	6:
				FileName = "EXP_CAPACITY" + RepTermination;
				break;
				case 	7:
				FileName = "EXP_OVERLOADS" + RepTermination;
				break;
				case 	8:
				FileName = "EXP_UNSERVED" + RepTermination;
				break;
				case 	9:
				FileName = "EXP_POWERS" + RepTermination;
				break;
				case 	10:
				FileName = "EXP_SEQPOWERS" + RepTermination;
				break;
				case 	11:
				FileName = "EXP_FAULTS" + RepTermination;
				break;
				case 	12:
				FileName = "EXP_GENMETERS" + RepTermination;
				break;
				case 	13:
				FileName = "EXP_LOADS" + RepTermination;
				break;
				case 	14:
				FileName = "EXP_METERS" + RepTermination;
				break;
			 /*15: Filename is assigned*/
				case 	16:
				FileName = "EXP_YPRIM" + RepTermination;
				break;
				case 	17:
				FileName = "EXP_Y" + RepTermination;
				break;
				case 	18:
				FileName = "EXP_SEQZ" + RepTermination;
				break;
				case 	19:
				FileName = "EXP_P_BYPHASE" + RepTermination;
				break;
				case 	20:
				FileName = "CIM100";
				break;
				case 	21:
				FileName = "CIM100x.XML";
				break;
				case 	22:
				FileName = "";
				break;
				case 	23:
				FileName = "EXP_BUSCOORDS" + RepTermination;
				break;
				case 	24:
				FileName = "EXP_LOSSES" + RepTermination;
				break;
				case 	25:
				FileName = "EXP_UUIDS" + RepTermination;
				break;
				case 	26:
				FileName = "EXP_Counts" + RepTermination;
				break;
				case 	27:
				FileName = "EXP_Summary" + RepTermination;
				break;
				case 	28:
				FileName = "";
				break;
				case 	29:
				FileName = "";
				break;
				case 	30:
				FileName = "";
				break;
				case 	31:
				FileName = "";
				break;
				case 	32:
				FileName = "EXP_Profile" + RepTermination;
				break;
				case 	33:
				FileName = "EXP_EventLog" + RepTermination;
				break;
				case 	34:
				FileName = "AllocationFactors.Txt";
				break;
				case 	35:
				FileName = "EXP_VOLTAGES_ELEM" + RepTermination;
				break;
				case 	36:
				FileName = "EXP_GIC_Mvar" + RepTermination;
				break;
				case 	37:
				FileName = "EXP_BusReliability" + RepTermination;
				break;
				case 	38:
				FileName = "EXP_BranchReliability" + RepTermination;
				break;
				case 	39:
				FileName = "EXP_NodeNames" + RepTermination;
				break;
				case 	40:
				FileName = "EXP_Taps" + RepTermination;
				break;
				case 	41:
				FileName = "EXP_NodeOrder" + RepTermination;
				break;
				case 	42:
				FileName = "EXP_ElemCurrents" + RepTermination;
				break;
				case 	43:
				FileName = "EXP_ElemVoltages" + RepTermination;
				break;
				case 	44:
				FileName = "EXP_ElemPowers" + RepTermination;
				break;
				case 	45:
				FileName = "EXP_Result" + RepTermination;
				break;
				case 	46:
				FileName = "EXP_YNodeList" + RepTermination;
				break;
				case 	47:
				FileName = "EXP_YVoltages" + RepTermination;
				break;
				case 	48:
				FileName = "EXP_YCurrents" + RepTermination;
				break;
				case 	49:
				FileName = "EXP_PVMeters" + RepTermination;
				break;
				case 	50:
				FileName = "EXP_STORAGEMeters" + RepTermination;
				break;
				case 	51:
				FileName = "EXP_SECTIONS" + RepTermination;
				break;
				case 	52:
				FileName = "EXP_ErrorLog.txt";
				break;
				case 	53:
				FileName = "Inc_Matrix" + RepTermination;
				break;
				case 	54:
				FileName = "Inc_Matrix_Rows" + RepTermination;
				break;
				case 	55:
				FileName = "Inc_Matrix_Cols" + RepTermination;
				break;
				case 	56:
				FileName = "Bus_Levels" + RepTermination;
				break;
				case 	57:
				FileName = "Laplacian" + RepTermination;
				break;
				case 	58:
				FileName = "ZLL" + RepTermination;
				break;
				case 	59:
				FileName = "ZCC" + RepTermination;
				break;
				case 	60:
				FileName = "C" + RepTermination;
				break;
				case 	61:
				FileName = "Y4" + RepTermination;
				break;
				case 	62:
					FileName = "Jacobian" + RepTermination;
					break;
				case 	63:
					FileName = "deltaF" + RepTermination;
					break;
				case 	64:
					FileName = "deltaZ" + RepTermination;
					break;
				default:
				FileName = "EXP_VOLTAGES" + RepTermination;    // default
				break;
			}
			FileName = GetOutputDirectory() + CircuitName_[ActiveActor] + FileName;  // Explicitly define directory
		}
		switch(ParamPointer)
		{
			case 	1:
			ExportVoltages(FileName);
			break;
			case 	2:
			ExportSeqVoltages(FileName);
			break;
			case 	3:
			ExportCurrents(FileName);
			break;
			case 	4:
			ExportSeqCurrents(FileName);
			break;
			case 	5:
			ExportEstimation(FileName);
			break;   // Estimation error
			case 	6:
			ExportCapacity(FileName);
			break;
			case 	7:
			ExportOverloads(FileName);
			break;
			case 	8:
			ExportUnserved(FileName, UEonlyOpt);
			break;
			case 	9:
			ExportPowers(FileName, MVAopt);
			break;
			case 	10:
			ExportSeqPowers(FileName, MVAopt);
			break;
			case 	11:
			ExportFaultStudy(FileName);
			break;
			case 	12:
			ExportGenMeters(FileName);
			break;
			case 	13:
			ExportLoads(FileName);
			break;
			case 	14:
			ExportMeters(FileName);
			break;
			case 	15:
			if(Parm2.size() > 0)
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
					if(Parm2 == "all")
					{
						pMon = (TMonitorObj*) ActiveCircuit[idxP]->Monitors.Get_First();
						while(pMon != nullptr)
						{
							if(pMon != nullptr)
							{
								pMon->TranslateToCSV(false, idxP);
								FileName = GlobalResult;
							}
							pMon = (TMonitorObj*) ActiveCircuit[idxP]->Monitors.Get_Next();
						}
					}
					else
					{
						pMon = (TMonitorObj*) MonitorClass[idxP]->Find(Parm2);
						if(pMon != nullptr)
						{
							pMon->TranslateToCSV(false, idxP);
							FileName = GlobalResult;
						}
						else
						DoSimpleMsg(String("Monitor \"") + Parm2
				   + "\" not found."
				   + CRLF
				   + Parser[ActiveActor]->get_CmdBuffer(), 250);
					}
				}
			}
			else
			DoSimpleMsg(String("Monitor Name Not Specified.") + CRLF
				   + Parser[ActiveActor]->get_CmdBuffer(), 251);
			break;
			case 	16:
			ExportYprim(FileName);
			break;
			case 	17:
			ExportY(FileName, TripletOpt);
			break;
			case 	18:
			ExportSeqZ(FileName);
			break;
			case 	19:
			ExportPbyphase(FileName, MVAopt);
			break;
			case 	20:
			ExportCDPSM(FileName, SUBSTATION, SubGeographicRegion, GeographicRegion, FdrUUID, SubUUID, SubGeoUUID, RgnUUID, false);
			break;
			case 	21:
			ExportCDPSM(FileName, SUBSTATION, SubGeographicRegion, GeographicRegion, FdrUUID, SubUUID, SubGeoUUID, RgnUUID, true);
			break;
			case 	22:
			DoSimpleMsg("Asset export no longer supported; use Export CIM100", 252);
			break;
			case 	23:
			ExportBusCoords(FileName);
			break;
			case 	24:
			ExportLosses(FileName);
			break;
			case 	25:
			ExportUuids(FileName);
			break;
			case 	26:
			ExportCounts(FileName);
			break;
			case 	27:
			ExportSummary(FileName);
			break;
			case 	28:
			DoSimpleMsg("ElectricalProperties export no longer supported; use Export CIM100", 252);
			break;
			case 	29:
			DoSimpleMsg("Geographical export no longer supported; use Export CIM100", 252);
			break;
			case 	30:
			DoSimpleMsg("Topology export no longer supported; use Export CIM100", 252);
			break;
			case 	31:
			DoSimpleMsg("StateVariables export no longer supported; use Export CIM100", 252);
			break;
			case 	32:
			ExportProfile(FileName, PhasesToPlot);
			break;
			case 	33:
			ExportEventLog(FileName);
			break;
			case 	34:
			DumpAllocationFactors(FileName);
			break;
			case 	35:
			ExportVoltagesElements(FileName);
			break;
			case 	36:
			ExportGICMvar(FileName);
			break;
			case 	37:
			ExportBusReliability(FileName);
			break;
			case 	38:
			ExportBranchReliability(FileName);
			break;
			case 	39:
			ExportNodeNames(FileName);
			break;
			case 	40:
			ExportTaps(FileName);
			break;
			case 	41:
			ExportNodeOrder(FileName);
			break;
			case 	42:
			ExportElemCurrents(FileName);
			break;
			case 	43:
			ExportElemVoltages(FileName);
			break;
			case 	44:
			ExportElemPowers(FileName);
			break;
			case 	45:
			ExportResult(FileName);
			break;
			case 	46:
			ExportYNodeList(FileName);
			break;
			case 	47:
			ExportYVoltages(FileName);
			break;
			case 	48:
			ExportYCurrents(FileName);
			break;
			case 	49:
			ExportPVSystemMeters(FileName);
			break;
			case 	50:
			ExportStorageMeters(FileName);
			break;
			case 	51:
			ExportSections(FileName, *pMeter);
			break;
			case 	52:
			ExportErrorLog(FileName);
			break;
			case 	53:
			ExportIncMatrix(FileName);
			break;
			case 	54:
			ExportIncMatrixRows(FileName);
			break;
			case 	55:
			ExportIncMatrixCols(FileName);
			break;
			case 	56:
			ExportBusLevels(FileName);
			break;
			case 	57:
			ExportLaplacian(FileName);
			break;
			case 	58:
			ExportZLL(FileName);
			break;
			case 	59:
			ExportZCC(FileName);
			break;
			case 	60:
			ExportC(FileName);
			break;
			case 	61:
			ExportY4(FileName);
			break;
			case 	62:
				ExportJacobian(FileName);
				break;
			case 	63:
				ExportdeltaF(FileName);
				break;
			case 	64:
				ExportdeltaZ(FileName);
				break;
			// ExportVoltages(FileName);    // default
			default:
			DoSimpleMsg(String("Error: Unknown Export command: \"") + Parm1 + "\"", 24713);
			AbortExport = true;
			break;
		}
		result = 0;
		InShowResults = false;
		if(!AbortExport)
		{
			SetLastResultFile(FileName);
			ParserVars->Add("@lastexportfile", FileName);
			if(AutoShowExport)
				FireOffEditor(FileName);
		}
		return result;
	}

	void DisposeStrings()
	{
		delete[] ExportOption;
		delete[] ExportHelp;
	}


	void ExportOptions_initialization()
	{
		DefineOptions();
		ExportCommands = new TCommandList(ExportOption, NumExportOptions);
		ExportCommands->set_AbbrevAllowed(true);
	}

	void ExportOptions_finalization()
	{
		DisposeStrings();
		delete ExportCommands;
	}

		class 		ExportOptions_unit
		{
		public:
		ExportOptions_unit()
		{
			//AssertSystemInitialization();
			ExportOptions_initialization();
		}
		~		ExportOptions_unit(){ExportOptions_finalization(); }
		};
		ExportOptions_unit _ExportOptions_unit;

}  // namespace ExportOptions




