
#pragma hdrstop

#include "PlotOptions.h"

using namespace std;
using namespace Command;
using namespace DSSGlobals;
using namespace DSSPlot;
using namespace ParserDel;
using namespace System;

namespace PlotOptions
{


String* PlotOption;
String* PlotHelp;
TCommandList* PlotCommands = nullptr;

void DefineOptions()
{
	PlotOption = new string[NumPlotOptions + 1];
	PlotHelp = new string[NumPlotOptions + 1];

	PlotOption[1 - 1] = "type";
	PlotOption[2 - 1] = "quantity";
	PlotOption[3 - 1] = "max";
	PlotOption[4 - 1] = "dots";
	PlotOption[5 - 1] = "labels";
	PlotOption[6 - 1] = "object";
	PlotOption[7 - 1] = "showloops";
	PlotOption[8 - 1] = "r3";
	PlotOption[9 - 1] = "r2";
	PlotOption[10 - 1] = "c1";
	PlotOption[11 - 1] = "c2";
	PlotOption[12 - 1] = "c3";
	PlotOption[13 - 1] = "channels";
	PlotOption[14 - 1] = "bases";
	PlotOption[15 - 1] = "subs";
	PlotOption[16 - 1] = "thickness";
	PlotOption[17 - 1] = "buslist";
	PlotOption[18 - 1] = "min";
	PlotOption[19 - 1] = "3phLinestyle";
	PlotOption[20 - 1] = "1phLinestyle";
	PlotOption[21 - 1] = "phases";
	PlotOption[22 - 1] = "profilescale";
	PlotOption[23 - 1] = "PlotID";
	PlotHelp[1 - 1] = "One of {Circuit | Monitor | Daisy | Zones | AutoAdd | General (bus data) | Loadshape | Tshape | Priceshape |Profile} " + CRLF 
	           + "A \"Daisy\" plot is a special circuit plot that places a marker at each Generator location "
	           + "or at buses in the BusList property, if defined. "
	           + "A Zones plot shows the meter zones (see help on Object). "
	           + "Autoadd shows the autoadded generators. General plot shows quantities associated with buses "
	           + "using gradient colors between C1 and C2. Values are read from a file (see Object). "
	           + "Loadshape plots the specified loadshape. Examples:"
	           + CRLF 
	           + CRLF 
	           + "Plot type=circuit quantity=power"
	           + CRLF 
	           + "Plot Circuit Losses 1phlinestyle=3"
	           + CRLF 
	           + "Plot Circuit quantity=3 object=mybranchdata.csv"
	           + CRLF 
	           + "Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]"
	           + CRLF 
	           + "Plot General quantity=1 object=mybusdata.csv"
	           + CRLF 
	           + "Plot Loadshape object=myloadshape"
	           + CRLF 
	           + "Plot Tshape object=mytemperatureshape"
	           + CRLF 
	           + "Plot Priceshape object=mypriceshape"
	           + CRLF 
	           + "Plot Profile"
	           + CRLF 
	           + "Plot Profile Phases=Primary"
	           + CRLF 
	           + CRLF 
	           + "Additional plots with the OpenDSS Viewer (These plots are enabled with the \"OpenDSSViewer\" option):"
	           + CRLF 
	           + "- Plot evolution  ! Probabilistic density evolution plot with the line-to-ground magnitude of all load voltages in per unit base."
	           + CRLF 
	           + "- Plot energymeter object=system  ! System energy meter plot. The \"DemandInterval\" option is required."
	           + CRLF 
	           + "- Plot energymeter object=Totals  ! Totals energy meter plot. The \"DemandInterval\" option is required."
	           + CRLF 
	           + "- Plot energymeter object=voltexception  ! Voltage exception plot. The \"DemandInterval\" and \"VoltExceptionReport\" options are required."
	           + CRLF 
	           + "- Plot energymeter object=overloads  ! Overload report plot. The \"DemandInterval\" and \"OverloadReport\" options are required."
	           + CRLF 
	           + "- Plot energymeter object=myMeter  ! Energy meter plot. The \"DemandInterval\" and \"DIVerbose\" options are required."
	           + CRLF 
	           + "- Plot loadshape object=myLoadshape  ! Loadshapes with the OpenDSS Viewer functionalities."
	           + CRLF 
	           + "- Plot matrix incidence  ! Incidence matrix plot (Requires: CalcIncMatrix or CalcIncMatrix_O)."
	           + CRLF 
	           + "- Plot matrix laplacian  ! Laplacian matrix plot (Requires: CalcLaplacian)."
	           + CRLF 
	           + "- Plot monitor object=myMonitor  ! Monitors with the OpenDSS Viewer functionalities. All channels are included in this plot."
	           + CRLF 
	           + "- Plot phasevoltage object=myMeter  ! Phase voltage plot associated to an energy meter. The \"DemandInterval\", \"DIVerbose\" options and the \"PhaseVoltageReport\" parameter are required."
	           + CRLF 
	           + "- Plot profile  ! 3D and 2D versions of the voltage profile."
	           + CRLF 
	           + "- Plot scatter  ! Scatter plot with geovisualization of line-to-ground bus voltage magnitudes in per unit.";
	PlotHelp[2 - 1] = "One of {Voltage | Current | Power | Losses | Capacity | (Value Index for General, AutoAdd, or Circuit[w/ file]) }";
	PlotHelp[3 - 1] = "Enter 0 (the default value) or the value corresponding to max scale or line thickness in the circuit plots. "
	           "Power and Losses in kW. Also, use this to specify the max value corresponding to color C2 in General plots.";
	PlotHelp[4 - 1] = "Yes or No*. Places a marker on the circuit plot at the bus location. See Set Markercode under options.";
	PlotHelp[5 - 1] = "Yes or No*. If yes, bus labels (abbreviated) are printed on the circuit plot.";
	PlotHelp[6 - 1] = "Object to be plotted. One of [Meter Name (zones plot) | Monitor Name | LoadShape Name | File Name for General bus data | File Name Circuit branch data]";
	PlotHelp[7 - 1] = "{Yes | No*} Shows loops on Circuit plot. Requires an EnergyMeter to be defined.";
	PlotHelp[8 - 1] = "pu value for tri-color plot max range [default=.85 of max scale]. Corresponds to color C3.";
	PlotHelp[9 - 1] = "pu value for tri-color plot mid range [default=.50 of max scale]. Corresponds to color C2.";
	PlotHelp[10 - 1] = String("RGB color number or standard color name for color C1. This is the default color for circuit plots. Default is blue. See options in the Plot menu.") + CRLF
	           + CRLF 
	           + "Standard color names are: "
	           + CRLF 
	           + CRLF 
	           + " Black  "
	           + CRLF 
	           + " Maroon "
	           + CRLF 
	           + " Green  "
	           + CRLF 
	           + " Olive  "
	           + CRLF 
	           + " Navy   "
	           + CRLF 
	           + " Purple "
	           + CRLF 
	           + " Teal   "
	           + CRLF 
	           + " Gray   "
	           + CRLF 
	           + " Silver "
	           + CRLF 
	           + " Red    "
	           + CRLF 
	           + " Lime   "
	           + CRLF 
	           + " Yellow "
	           + CRLF 
	           + " Blue   "
	           + CRLF 
	           + " Fuchsia"
	           + CRLF 
	           + " Aqua   "
	           + CRLF 
	           + " LtGray "
	           + CRLF 
	           + " DkGray "
	           + CRLF 
	           + " White  ";
	PlotHelp[11 - 1] = String("RGB color number or standard color name for color C2. Used for gradients and tricolor plots such as circuit voltage.") + CRLF 
	           + CRLF 
	           + "See Help on C1 for list of standard color names.";
	PlotHelp[12 - 1] = String("RGB color number or standard color name for color C3. Used for gradients and tricolor plots such a circuit voltage.") + CRLF
	           + CRLF
	           + "See Help on C1 for list of standard color names.";
	PlotHelp[13 - 1] = String("Array of channel numbers for monitor plot. Example") + CRLF
	           + CRLF
	           + "Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5]"
	           + CRLF
	           + CRLF
	           + "Do \"Show Monitor MyMonitor\" to see channel definitions.";
	PlotHelp[14 - 1] = String("Array of base values for each channel for monitor plot. Useful for creating per unit plots. Default is 1.0 for each channel.  Set Base= property after defining channels.") + CRLF
	           + CRLF
	           + "Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5] Bases=[2400 2400 2400]"
	           + CRLF
	           + CRLF
	           + "Do \"Show Monitor MyMonitor\" to see channel range and definitions.";
	PlotHelp[15 - 1] = "{Yes | No*} Displays a marker at each transformer declared to be a substation. "
	           "At least one bus coordinate must be defined for the transformer. "
	           "See MarkTransformer and TransMarkerCode options.";
	PlotHelp[16 - 1] = "Max thickness allowed for lines in circuit plots (default=7).";
	PlotHelp[17 - 1] = String("{Array of Bus Names | File=filename } This is for the Daisy plot. ") + CRLF
	           + CRLF
	           + "Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]"
	           + CRLF
	           + CRLF
	           + "A \"daisy\" marker is plotted for "
	           + "each bus in the list. Bus name may be repeated, which results in multiple markers distributed around the bus location. "
	           + "This gives the appearance of a daisy if there are several symbols at a bus. Not needed for plotting active generators.";
	PlotHelp[18 - 1] = "Enter 0 (the default value) or the value corresponding to min value corresponding to color C1 in General bus data plots.";
	PlotHelp[19 - 1] = "Line style for drawing 3-phase lines. A number in the range of [1..7].Default is 1 (solid). Use 3 for dotted; 2 for dashed.";
	PlotHelp[20 - 1] = "Line style for drawing 1-phase lines. A number in the range of [1..7].Default is 1 (solid). Use 3 for dotted; 2 for dashed.";
	PlotHelp[21 - 1] = String("{default* | ALL | PRIMARY | LL3ph | LLALL | LLPRIMARY | (phase number)} For Profile plot. Specify which phases you want plotted.") + CRLF
	           + CRLF
	           + "default = plot only nodes 1-3 at 3-phase buses (default)"
	           + CRLF
	           + "ALL = plot all nodes"
	           + CRLF
	           + "PRIMARY = plot all nodes -- primary only (voltage > 1kV)"
	           + CRLF
	           + "LL3ph = 3-ph buses only -- L-L voltages)"
	           + CRLF
	           + "LLALL = plot all nodes -- L-L voltages)"
	           + CRLF
	           + "LLPRIMARY = plot all nodes -- L-L voltages primary only)"
	           + CRLF
	           + "(phase number) = plot all nodes on selected phase"
	           + CRLF
	           + CRLF
	           + "Note: Only nodes downline from an energy meter are plotted.";
	PlotHelp[22 - 1] = String("PUKM | 120KFT, default is PUKM") + CRLF
	           + "PUKM = per-unit voltage vs. distance in km"
	           + CRLF
	           + "120KFT = voltage on 120-V base vs. distance in kft.";
	PlotHelp[23 - 1] = String("Plot identifier for dynamic updates of \"profile\" and \"scatter\" plots in the OpenDSS Viewer (See \"plot type\" for more details)." "When multiple \"plot\" commands are executed with the same PlotID, the same figure will be updated with the most recent simulation results.") + CRLF
	           + CRLF
	           + "This identifier could be declared as an integer number or a string without spaces."
	           + CRLF
	           + CRLF
	           + "Example:"
	           + CRLF
	           + CRLF
	           + "set OpenDSSViewer=true ! OpenDSS Viewer enabled"
	           + CRLF
	           + "solve"
	           + CRLF
	           + "plot scatter PlotID=plotA  !Generates a new scatter plot"
	           + CRLF
	           + "solve"
	           + CRLF
	           + "plot scatter PlotID=plotB  !Generates a new scatter plot"
	           + CRLF
	           + "solve"
	           + CRLF
	           + "plot scatter PlotID=plotA  !Updates the data in plotA"
	           + CRLF
	           + "solve"
	           + CRLF
	           + "plot scatter PlotID=plotA  !Updates the data in plotA"
	           + CRLF;
}


//----------------------------------------------------------------------------

int DoPlotCmd()
{
	int		result			= 0,
			ParamPointer	= 0,
			i				= 0,
			NumChannels		= 0;
	String	ParamName		= "",
			Param			= "",
			OrgParam;

	double DblBuffer[51];

	if(NoFormsAllowed)
	{
		result = 1;
		return result;
	}
	if(!ASSIGNED(DSSPlotObj))
		DSSPlotObj = new TDSSPlot();
	DSSPlotObj->SetDefaults();

    /*Get next parameter on command line*/
	ParamPointer = 0;
	ParamName = UpperCase(Parser[ActiveActor]->GetNextParam());
	OrgParam = Parser[ActiveActor]->MakeString_();
	Param = UpperCase(OrgParam);
	while(Param.size() > 0)

      /*Interpret Parameter*/
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = PlotCommands->Getcommand(ParamName);

      /*Check options requiring a solution and abort if no solution or circuit*/
		switch(ParamPointer)
		{
			case 	1:
				switch(Param[0])
				{
					case	L'A':
					case L'C':
					case L'D':
					case L'G':
					case L'M':
					case L'P':
					case L'Z':
						if(!(CompareTextShortest("pri", Param) == 0))   // allow Price shape
						{
							if(!ASSIGNED(ActiveCircuit[ActiveActor]))
							{
								DoSimpleMsg("No circuit created.", 24731);
								return result;
							}
							if(!ASSIGNED(ActiveCircuit[ActiveActor]->Solution) || (ActiveCircuit[ActiveActor]->Solution->NodeV.empty()))
							{
								DoSimpleMsg("The circuit must be solved before you can do this.", 24732);
								return result;
							}
						}
						break;
					default:
					  ;
					break;
				}
				break;
			default:
				break;
		}
		/*# with DSSPlotObj do */
		{
			auto with0 = DSSPlotObj;
			switch(ParamPointer)
			{
				case 	1:
				switch(Param[0])
				{
					case 	L'A':
					{
						with0->PlotType = ptAutoAddLogPlot;
						with0->ObjectName = CircuitName_[ActiveActor] + "AutoAddLog.CSV";
						with0->ValueIndex = 2;
					}
					break;
					case 	L'C':
						with0->PlotType = ptCircuitplot;
					break;
					case 	L'E':
						if(CompareTextShortest("ener", Param) == 0)
							with0->PlotType = ptEnergyPlot;
						else
							with0->PlotType = ptEvolutionPlot;
					break;
					case 	L'G':
						with0->PlotType = ptGeneralDataPlot;
					break;
					case 	L'L':
						with0->PlotType = ptLoadShape;
					break;
					case 	L'M':
						if(CompareTextShortest("mon", Param) == 0)
							with0->PlotType = ptmonitorplot;
						else
							with0->PlotType = ptMatrixplot;
					break;
					case 	L'P':
						if(CompareTextShortest("pro", Param) == 0)
							with0->PlotType = ptProfile;
						else
						{
							if(CompareTextShortest("phas", Param) == 0)
								with0->PlotType = ptPhaseVoltage;
							else
								with0->PlotType = ptPriceShape;
						}
					break;
					case 	L'S':
						with0->PlotType = ptScatterPlot;
					break;
					case 	L'T':
						with0->PlotType = ptTShape;
					break;
					case 	L'D':
					{
						with0->PlotType = ptdaisyplot;
						with0->DaisyBusList.clear();
					}
					break;
					case 	L'Z':
						with0->PlotType = ptMeterZones;
					break;
					default:
					  ;
					break;
				}
				break;
				case 	2:
				switch(Param[0])
				{
					case 	L'V':
					with0->Quantity = pqVoltage;
					break;
					case 	L'C':
						switch(Param[1])
						{
							case 	L'A':
							with0->Quantity = pqCapacity;
							break;
							case 	L'U':
							with0->Quantity = pqCurrent;
							break;
							default:
							  ;
							break;
						}
					break;
					case 	L'P':
						with0->Quantity = pqPower;
					break;
					case 	L'L':
						if(CompareTextShortest("los", Param) == 0)
							with0->Quantity = pqLosses;
						else
							with0->MatrixType = pLaplacian;
					break;
					case 	L'I':
						with0->MatrixType = pIncMatrix;
					break;
					default:
						with0->Quantity = pqNone;
						with0->ValueIndex = Parser[ActiveActor]->MakeInteger_();
					break;
				}
				break;
				case 	3:
				{
					with0->MaxScale = Parser[ActiveActor]->MakeDouble_();
					if(with0->MaxScale > 0.0)    // Indicate the user wants a particular value
						with0->MaxScaleIsSpecified = true;
					else
						with0->MaxScaleIsSpecified = false;
				}
				break;
				case 	4:
				with0->Dots = InterpretYesNo(Param);
				break;
				case 	5:
				with0->Labels = InterpretYesNo(Param);
				break;
				case 	6:
				with0->ObjectName = Parser[ActiveActor]->MakeString_();
				break;
				case 	7:
				{
					with0->ShowLoops = InterpretYesNo(Param);
					if(with0->ShowLoops)
						with0->PlotType = ptMeterZones;
				}
				break;
				case 	8:
				with0->TriColorMax = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	9:
				with0->TriColorMid = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	10:
				with0->Color1 = InterpretColorName(Param);
				break;
				case 	11:
				with0->Color2 = InterpretColorName(Param);
				break;
				case 	12:
				with0->Color3 = InterpretColorName(Param);
				break;    /*Channel definitions for Plot Monitor*/
				case 	13:
				{
					NumChannels = Parser[ActiveActor]->ParseAsVector(51, DblBuffer);  // allow up to 50 channels
					if(NumChannels > 0)   // Else take the defaults
					{
						int stop = 0;
						with0->Channels.resize(NumChannels);
						for(stop = NumChannels - 1, i = 0; i <= stop; i++)
						{
							with0->Channels[i] = (unsigned int) Round(DblBuffer[i]);
						}
						with0->Bases.resize(NumChannels);
						for(stop = NumChannels - 1, i = 0; i <= stop; i++)
						{
							with0->Bases[i] = 1.0;
						}
					}
				}
				break;
				case 	14:
				{
					NumChannels = Parser[ActiveActor]->ParseAsVector(51, DblBuffer);  // allow up to 50 channels
					if(NumChannels > 0)
					{
						int stop = 0;
						with0->Bases.resize(NumChannels);
						for(stop = NumChannels - 1, i = 0; i <= stop; i++)
						{
							with0->Bases[i] = DblBuffer[i];
						}
					}
				}
				break;
				case 	15:
				with0->ShowSubs = InterpretYesNo(Param);
				break;
				case 	16:
				with0->FMaxLineThickness = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	17:
				InterpretTStringListArray(OrgParam, with0->DaisyBusList);
				break;  /*read in Bus list*/
				case 	18:
				{
					with0->MinScale = Parser[ActiveActor]->MakeDouble_();
					with0->MinScaleIsSpecified = true;    // Indicate the user wants a particular value
				}
				break;
				case 	19:
				ThreePhLineStyle = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	20:
				SinglePhLineStyle = Parser[ActiveActor]->MakeInteger_();
				break;  // Parse off phase(s) to plot
				case 	21:
				{
					with0->PhasesToPlot = PROFILE3PH; // the default
					if(CompareTextShortest(Param, "default") == 0)
						with0->PhasesToPlot = PROFILE3PH;
					else
					{
						if(CompareTextShortest(Param, "all") == 0)
							with0->PhasesToPlot = PROFILEALL;
						else
						{
							if(CompareTextShortest(Param, "primary") == 0)
								with0->PhasesToPlot = PROFILEALLPRI;
							else
							{
								if(CompareTextShortest(Param, "ll3ph") == 0)
									with0->PhasesToPlot = PROFILELL;
								else
								{
									if(CompareTextShortest(Param, "llall") == 0)
										with0->PhasesToPlot = PROFILELLALL;
									else
									{
										if(CompareTextShortest(Param, "llprimary") == 0)
											with0->PhasesToPlot = PROFILELLPRI;
										else
										{
											if(Param.size() == 1)
												with0->PhasesToPlot = Parser[ActiveActor]->MakeInteger_();
										}
									}
								}
							}
						}
					}
				}
				break;
				case 	22:
				{
					with0->ProfileScale = PROFILEPUKM;
					if(CompareTextShortest(Param, "120KFT") == 0)
						with0->ProfileScale = PROFILE120KFT;
				}
				break;
				case 	23:
					with0->PlotID = Parser[ActiveActor]->MakeString_();
				break;
				default:
				  ;
				break;
			}
		}
		ParamName = UpperCase(Parser[ActiveActor]->GetNextParam());
		OrgParam = Parser[ActiveActor]->MakeString_();
		Param = UpperCase(OrgParam);
	}
	if(!ActiveCircuit[ActiveActor]->Issolved)
		DSSPlotObj->Quantity = pqNone;
	/*# with DSSPlotObj do */
	{
		auto with1 = DSSPlotObj;
		if(DSS_Viz_enable)
		{
			if(DSS_Viz_installed && ((with1->PlotType == ptmonitorplot) || (with1->PlotType == ptLoadShape) || (with1->PlotType == ptProfile) || (with1->PlotType == ptScatterPlot) || (with1->PlotType == ptEvolutionPlot) || (with1->PlotType == ptEnergyPlot) || (with1->PlotType == ptPhaseVoltage) || (with1->PlotType == ptMatrixplot)))
				with1->DSSVizPlot(); // OpenDSS Viewer
		}
		else
		{
			if((with1->PlotType == ptScatterPlot) || (with1->PlotType == ptEvolutionPlot) || (with1->PlotType == ptMatrixplot))
				DoSimpleMsg("The OpenDSS Viewer is disabled (Check the OpenDSSViewer option in the help).", 0);
			else
				with1->Execute();   // makes a new plot based on these options
		}
	}
	return result;
}

/*
  Produce a plot with the DSSGraphX object
*/

void DisposeStrings()
{
	delete[] PlotOption;
	delete[] PlotHelp;
}


void PlotOptions_initialization()
{
	DefineOptions();
	PlotCommands = new TCommandList(PlotOption, NumPlotOptions);
	PlotCommands->set_AbbrevAllowed(true);
}

void PlotOptions_finalization()
{
	DisposeStrings();
	delete PlotCommands;
}

		class 		PlotOptions_unit
		{
		public:
		PlotOptions_unit()
		{
			//AssertSystemInitialization();
			PlotOptions_initialization();
		}
		~		PlotOptions_unit(){PlotOptions_finalization(); }
		};
		PlotOptions_unit _PlotOptions_unit;

}  // namespace PlotOptions




