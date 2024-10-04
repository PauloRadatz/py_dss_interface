
#pragma hdrstop

#include "XfmrCode.h"

#include "DSSGlobals.h"
#include "Utilities.h"

using namespace std;

namespace XfmrCode
{

TXfmrCodeObj::TXfmrCodeObj(TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TXfmrCodeObj::TXfmrCodeObj(String ClassName) : inherited(ClassName) {}
TXfmrCodeObj::TXfmrCodeObj() {}


TXfmrCodeObj* ActiveXfmrCodeObj = nullptr;
const int NumPropsThisClass = 39;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TXfmrCode::TXfmrCode()
{
	;
	Class_Name = "XfmrCode";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TXfmrCode::~TXfmrCode()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TXfmrCode::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();
	(PropertyName)[1 - 1] = "phases";
	(PropertyName)[2 - 1] = "windings";

   // Winding Definition
	(PropertyName)[3 - 1] = "wdg";
	(PropertyName)[4 - 1] = "conn";
	(PropertyName)[5 - 1] = "kV"; // FOR 2-and 3- always kVLL ELSE actual winding KV
	(PropertyName)[6 - 1] = "kVA";
	(PropertyName)[7 - 1] = "tap";
	(PropertyName)[8 - 1] = "%R";
	(PropertyName)[9 - 1] = "Rneut";
	(PropertyName)[10 - 1] = "Xneut";

   // General Data
	(PropertyName)[11 - 1] = "conns";
	(PropertyName)[12 - 1] = "kVs";
	(PropertyName)[13 - 1] = "kVAs";
	(PropertyName)[14 - 1] = "taps";
	(PropertyName)[15 - 1] = "Xhl";
	(PropertyName)[16 - 1] = "Xht";
	(PropertyName)[17 - 1] = "Xlt";
	(PropertyName)[18 - 1] = "Xscarray";  // x12 13 14... 23 24.. 34 ..
	(PropertyName)[19 - 1] = "thermal";
	(PropertyName)[20 - 1] = "n";
	(PropertyName)[21 - 1] = "m";
	(PropertyName)[22 - 1] = "flrise";
	(PropertyName)[23 - 1] = "hsrise";
	(PropertyName)[24 - 1] = "%loadloss";
	(PropertyName)[25 - 1] = "%noloadloss";
	(PropertyName)[26 - 1] = "normhkVA";
	(PropertyName)[27 - 1] = "emerghkVA";
	(PropertyName)[28 - 1] = "MaxTap";
	(PropertyName)[29 - 1] = "MinTap";
	(PropertyName)[30 - 1] = "NumTaps";
	(PropertyName)[31 - 1] = "%imag";
	(PropertyName)[32 - 1] = "ppm_antifloat";
	(PropertyName)[33 - 1] = "%Rs";
	(PropertyName)[34 - 1] = "X12";
	(PropertyName)[35 - 1] = "X13";
	(PropertyName)[36 - 1] = "X23";
	(PropertyName)[37 - 1] = "RdcOhms";
	(PropertyName)[38 - 1] = "Seasons";
	(PropertyName)[39 - 1] = "Ratings";

     // define Property help values
	(PropertyHelp)[1 - 1] = "Number of phases this transformer. Default is 3.";
	(PropertyHelp)[2 - 1] = "Number of windings, this transformers. (Also is the number of terminals) "
	           "Default is 2. This property triggers memory allocation for the Transformer and will cause other properties to revert to default values.";
   // Winding Definition
	(PropertyHelp)[3 - 1] = "Set this = to the number of the winding you wish to define.  Then set "
	           "the values for this winding.  Repeat for each winding.  Alternatively, use "
	           "the array collections (buses, kvas, etc.) to define the windings.  Note: "
	           "reactances are BETWEEN pairs of windings; they are not the property of a single winding.";
	(PropertyHelp)[4 - 1] = "Connection of this winding. Default is \"wye\" with the neutral solidly grounded.";
	(PropertyHelp)[5 - 1] = "For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding";
	(PropertyHelp)[6 - 1] = "Base kVA rating of the winding. Side effect: forces change of max normal and emerg kva ratings."
	           "If 2-winding transformer, forces other winding to same value. "
	           "When winding 1 is defined, all other windings are defaulted to the same rating "
	           "and the first two winding resistances are defaulted to the %loadloss value.";
	(PropertyHelp)[7 - 1] = "Per unit tap that this winding is normally on.";
	(PropertyHelp)[8 - 1] = "Percent resistance this winding.  (half of total for a 2-winding).";
	(PropertyHelp)[9 - 1] = "Default = -1. Neutral resistance of wye (star)-connected winding in actual ohms."
	           "If entered as a negative value, the neutral is assumed to be open, or floating.";
	(PropertyHelp)[10 - 1] = "Neutral reactance of wye(star)-connected winding in actual ohms.  May be + or -.";

   // General Data
	(PropertyHelp)[11 - 1] = String("Use this to specify all the Winding connections at once using an array. Example:") + CRLF
	           + CRLF
	           + "New Transformer.T1 buses=\"Hibus, lowbus\" "
	           + "~ conns=(delta, wye)";
	(PropertyHelp)[12 - 1] = String("Use this to specify the kV ratings of all windings at once using an array. Example:") + CRLF
	           + CRLF
	           + "New Transformer.T1 buses=\"Hibus, lowbus\" "
	           + CRLF
	           + "~ conns=(delta, wye)"
	           + CRLF
	           + "~ kvs=(115, 12.47)"
	           + CRLF
	           + CRLF
	           + "See kV= property for voltage rules.";
	(PropertyHelp)[13 - 1] = "Use this to specify the kVA ratings of all windings at once using an array.";
	(PropertyHelp)[14 - 1] = "Use this to specify the normal p.u. tap of all windings at once using an array.";
	(PropertyHelp)[15 - 1] = "Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use "
	           "for 2- or 3-winding transformers. On the kva base of winding 1.";
	(PropertyHelp)[16 - 1] = "Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use "
	           "for 3-winding transformers only. On the kVA base of winding 1.";
	(PropertyHelp)[17 - 1] = "Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use "
	           "for 3-winding transformers only. On the kVA base of winding 1.";
	(PropertyHelp)[18 - 1] = String("Use this to specify the percent reactance between all pairs of windings as an array. " "All values are on the kVA base of winding 1.  The order of the values is as follows:") + CRLF
	           + CRLF
	           + "(x12 13 14... 23 24.. 34 ..)  "
	           + CRLF
	           + CRLF
	           + "There will be n(n-1)/2 values, where n=number of windings.";
	(PropertyHelp)[19 - 1] = "Thermal time constant of the transformer in hours.  Typically about 2.";
	(PropertyHelp)[20 - 1] = "n Exponent for thermal properties in IEEE C57.  Typically 0.8.";
	(PropertyHelp)[21 - 1] = "m Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0";
	(PropertyHelp)[22 - 1] = "Temperature rise, deg C, for full load.  Default is 65.";
	(PropertyHelp)[23 - 1] = "Hot spot temperature rise, deg C.  Default is 15.";
	(PropertyHelp)[24 - 1] = "Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading.";
	(PropertyHelp)[25 - 1] = "Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding.";
	(PropertyHelp)[26 - 1] = "Normal maximum kVA rating of H winding (winding 1).  Usually 100% - 110% of"
	           "maximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1.";
	(PropertyHelp)[27 - 1] = "Emergency (contingency)  kVA rating of H winding (winding 1).  Usually 140% - 150% of"
	           "maximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1.";
	(PropertyHelp)[28 - 1] = "Max per unit tap for the active winding.  Default is 1.10";
	(PropertyHelp)[29 - 1] = "Min per unit tap for the active winding.  Default is 0.90";
	(PropertyHelp)[30 - 1] = "Total number of taps between min and max tap.  Default is 32.";
	(PropertyHelp)[31 - 1] = "Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see \"ppm_antifloat\".";
	(PropertyHelp)[32 - 1] = "Default=1 ppm.  Parts per million of transformer winding VA rating connected to ground to protect against accidentally floating a winding without a reference. "
	           "If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor.";
	(PropertyHelp)[33 - 1] = String("Use this property to specify all the winding %resistances using an array. Example:") + CRLF
	           + CRLF
	           + "New Transformer.T1 buses=\"Hibus, lowbus\" "
	           + "~ %Rs=(0.2  0.3)";
	(PropertyHelp)[34 - 1] = "Alternative to XHL for specifying the percent reactance from winding 1 to winding 2.  Use "
	           "for 2- or 3-winding transformers. Percent on the kVA base of winding 1. ";
	(PropertyHelp)[35 - 1] = "Alternative to XHT for specifying the percent reactance from winding 1 to winding 3.  Use "
	           "for 3-winding transformers only. Percent on the kVA base of winding 1. ";
	(PropertyHelp)[36 - 1] = "Alternative to XLT for specifying the percent reactance from winding 2 to winding 3.Use "
	           "for 3-winding transformers only. Percent on the kVA base of winding 1.  ";
	(PropertyHelp)[37 - 1] = "Winding dc resistance in OHMS. Useful for GIC analysis. From transformer test report. "
	           "Defaults to 85% of %R property";
	(PropertyHelp)[38 - 1] = "Defines the number of ratings to be defined for the transfomer, to be used only when defining seasonal ratings using the \"Ratings\" property.";
	(PropertyHelp)[39 - 1] = String("An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert") + CRLF
	           + "multiple ratings to change during a QSTS simulation to evaluate different ratings in transformers.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TXfmrCode::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TXfmrCodeObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

void TXfmrCodeObj::SetNumWindings(int n)
{
	int i = 0;
	int OldWdgSize = 0;
	int NewWdgSize = 0;
	if(n > 1)
	{
		int stop = 0;
		WINDING_.clear(); // Free old WINDING_ objects
		OldWdgSize = ((NumWindings - 1) * ((double) NumWindings / 2)); // this is needed to emulate the beavior in delphi, do not remove the (double)!
		NumWindings = n;
		MaxWindings = n;
		NewWdgSize = ((NumWindings - 1) * ((double) NumWindings / 2));
		WINDING_.resize(MaxWindings);  // Reallocate collector array
		for(stop = MaxWindings, i = 1; i <= stop; i++)
		{
			WINDING_[i - 1] = TWinding();
		}
		XSC.resize(NewWdgSize);
		for(stop = NewWdgSize, i = OldWdgSize + 1; i <= stop; i++)
		{
			XSC[i - 1] = 0.30;   // default to something
		}
	}
	else
	DoSimpleMsg(String("Invalid number of windings: (") + IntToStr(n)
	           + ") for Transformer "
	           + ActiveTransfObj->get_Name(), 111);
}

void TXfmrCode::SetActiveWinding(int W)
{
	/*# with ActiveXfmrCodeObj do */
	{
		auto with0 = ActiveXfmrCodeObj;
		if((W > 0) && (W <= with0->NumWindings))
			with0->ActiveWinding = W;
		else
			DoSimpleMsg(String("Wdg parameter invalid for \"") + ActiveXfmrCodeObj->get_Name()
	           + "\"", 112);
	}
}

void TXfmrCode::InterpretWindings(const String s, WdgParmChoice Which)
{
	String Str;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);
	/*# with ActiveXfmrCodeObj do */
	{
		auto with0 = ActiveXfmrCodeObj;
		int stop = 0;
		for(stop = with0->NumWindings, i = 1; i <= stop; i++)
		{
			with0->ActiveWinding = i;
			String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			Str = AuxParser[ActiveActor]->MakeString_();
			if(Str.length() > 0)
				switch(Which)
				{
					case 	Conn:
					with0->WINDING_[with0->ActiveWinding - 1].Connection = InterpretConnection(Str);
					break;
					case 	kV:
					with0->WINDING_[with0->ActiveWinding - 1].kVLL = AuxParser[ActiveActor]->MakeDouble_();
					break;
					case 	kVA:
					with0->WINDING_[with0->ActiveWinding - 1].kVA = AuxParser[ActiveActor]->MakeDouble_();
					break;
					case 	R:
					with0->WINDING_[with0->ActiveWinding - 1].Rpu = 0.01 * AuxParser[ActiveActor]->MakeDouble_();
					break;
					case 	Tap:
					with0->WINDING_[with0->ActiveWinding - 1].puTap = AuxParser[ActiveActor]->MakeDouble_();
					break;
					default:
					  ;
					break;
				}
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TXfmrCode::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	int i = 0;
	String ParamName;  /*For parsing property names*/
	String Param;
	bool UpdateXsc = false;
	ActiveXfmrCodeObj = ((TXfmrCodeObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveXfmrCodeObj;
	UpdateXsc = false;
	/*# with ActiveXfmrCodeObj do */
	{
		auto with0 = ActiveXfmrCodeObj;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.length() > 0)
		{
			if(ParamName.length() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->Set_PropertyValue(ParamPointer,Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \"XfmrCode."
	           + with0->get_Name()
	           + "\"", 110);
				break;
				case 	1:
				with0->Fnphases = Parser[ActorID]->MakeInteger_();
				break;
				case 	2:
				with0->SetNumWindings(Parser[ActorID]->MakeInteger_());
				break; // Reallocate stuff if bigger
				case 	3:
				SetActiveWinding(Parser[ActorID]->MakeInteger_());
				break;
				case 	4:
				with0->WINDING_[with0->ActiveWinding - 1].Connection = InterpretConnection(Param);
				break;
				case 	5:
				with0->WINDING_[with0->ActiveWinding - 1].kVLL = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				with0->WINDING_[with0->ActiveWinding - 1].kVA = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				with0->WINDING_[with0->ActiveWinding - 1].puTap = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				with0->WINDING_[with0->ActiveWinding - 1].Rpu = Parser[ActorID]->MakeDouble_() * 0.01;
				break;  // %R
				case 	9:
				with0->WINDING_[with0->ActiveWinding - 1].Rneut = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				with0->WINDING_[with0->ActiveWinding - 1].Xneut = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				InterpretWindings(Param, Conn);
				break;
				case 	12:
				InterpretWindings(Param, kV);
				break;
				case 	13:
				InterpretWindings(Param, kVA);
				break;
				case 	14:
				InterpretWindings(Param, Tap);
				break;
				case 	15:
				with0->XHL = Parser[ActorID]->MakeDouble_() * 0.01;
				break;
				case 	16:
				with0->XHT = Parser[ActorID]->MakeDouble_() * 0.01;
				break;
				case 	17:
				with0->XLT = Parser[ActorID]->MakeDouble_() * 0.01;
				break;
				case 	18:
				Parser[ActorID]->ParseAsVector(((with0->NumWindings - 1) * (with0->NumWindings / 2)), (pDoubleArray) & (with0->XSC));
				break;
				case 	19:
				with0->ThermalTimeConst = Parser[ActorID]->MakeDouble_();
				break;
				case 	20:
				with0->n_thermal = Parser[ActorID]->MakeDouble_();
				break;
				case 	21:
				with0->m_thermal = Parser[ActorID]->MakeDouble_();
				break;
				case 	22:
				with0->FLrise = Parser[ActorID]->MakeDouble_();
				break;
				case 	23:
				with0->HSrise = Parser[ActorID]->MakeDouble_();
				break;
				case 	24:
				with0->pctLoadLoss = Parser[ActorID]->MakeDouble_();
				break;
				case 	25:
				with0->pctNoLoadLoss = Parser[ActorID]->MakeDouble_();
				break;
				case 	26:
				with0->NormMaxHkVA = Parser[ActorID]->MakeDouble_();
				break;
				case 	27:
				with0->EmergMaxHkVA = Parser[ActorID]->MakeDouble_();
				break;
				case 	28:
				with0->WINDING_[with0->ActiveWinding - 1].MaxTap = Parser[ActorID]->MakeDouble_();
				break;
				case 	29:
				with0->WINDING_[with0->ActiveWinding - 1].MinTap = Parser[ActorID]->MakeDouble_();
				break;
				case 	30:
				with0->WINDING_[with0->ActiveWinding - 1].NumTaps = Parser[ActorID]->MakeInteger_();
				break;
				case 	31:
				with0->pctImag = Parser[ActorID]->MakeDouble_();
				break;
				case 	32:
				with0->ppm_FloatFactor = Parser[ActorID]->MakeDouble_() * 1.0e-6;
				break;
				case 	33:
				InterpretWindings(Param, R);
				break;
				case 	34:
				with0->XHL = Parser[ActorID]->MakeDouble_() * 0.01;
				break;
				case 	35:
				with0->XHT = Parser[ActorID]->MakeDouble_() * 0.01;
				break;
				case 	36:
				with0->XLT = Parser[ActorID]->MakeDouble_() * 0.01;
				break;
				case 	37:
				with0->WINDING_[with0->ActiveWinding - 1].RdcOhms = Parser[ActorID]->MakeDouble_();
				break;
				case 	38:
				{
					with0->NumAmpRatings = Parser[ActorID]->MakeInteger_();
					with0->AmpRatings.resize(with0->NumAmpRatings);
				}
				break;
				case 	39:
				{
					with0->AmpRatings.resize(with0->NumAmpRatings);
					Param = Parser[ActiveActor]->MakeString_();
					with0->NumAmpRatings = InterpretDblArray(Param, with0->NumAmpRatings, ((pDoubleArray) &(with0->AmpRatings[0])));
				}
				break;
				default:
				ClassEdit(ActiveXfmrCodeObj, ParamPointer - NumPropsThisClass);
				break;
			}

         /*Take care of properties that require some additional work,*/
			switch(ParamPointer)
			{
				case 	6:
				if(with0->ActiveWinding == 1)
          // default all winding kvas to first winding so latter Donot have to be specified
				{
					int stop = 0;
					for(stop = with0->NumWindings, i = 2; i <= stop; i++)
					{
						with0->WINDING_[i - 1].kVA = with0->WINDING_[1 - 1].kVA;
					}
					with0->NormMaxHkVA = 1.1 * with0->WINDING_[1 - 1].kVA;    // Defaults for new winding rating.
					with0->EmergMaxHkVA = 1.5 * with0->WINDING_[1 - 1].kVA;
				}
				else
				{
					if(with0->NumWindings == 2)
					{
						with0->WINDING_[1 - 1].kVA = with0->WINDING_[2 - 1].kVA;  // For 2-winding, force both kVAs to be same
					}
				}
				break;
           // Update LoadLosskW if winding %r changed. Using only windings 1 and 2
				case 	8:
				with0->pctLoadLoss = (with0->WINDING_[1 - 1].Rpu + with0->WINDING_[2 - 1].Rpu) * 100.0;
				break;
				case 	13:
				{
					with0->NormMaxHkVA = 1.1 * with0->WINDING_[1 - 1].kVA;    // Defaults for new winding rating.
					with0->EmergMaxHkVA = 1.5 * with0->WINDING_[1 - 1].kVA;
				}
				break;
				case 15: case 16: case 17:
				{
					UpdateXsc = true;
				}
				break;
				case 	18:
				for(int stop = ((with0->NumWindings - 1) * (with0->NumWindings / 2)), i = 1; i <= stop; i++)
				{
					(with0->XSC)[i - 1] = (with0->XSC)[i - 1] * 0.01;
				}
				break;  // Convert to per unit
    // Assume load loss is split evenly  between windings 1 and 2
				case 	24:
				{
					with0->WINDING_[1 - 1].Rpu = with0->pctLoadLoss / 2.0 / 100.0;
					with0->WINDING_[2 - 1].Rpu = with0->WINDING_[1 - 1].Rpu;
				}
				break;
				case 	33:
				with0->pctLoadLoss = (with0->WINDING_[1 - 1].Rpu + with0->WINDING_[2 - 1].Rpu) * 100.0;
				break; // Keep this up to date
				case 34: case 35: case 36:
				UpdateXsc = true;
				break;
				case 	37:
				with0->WINDING_[with0->ActiveWinding].RdcSpecified = true;
				break;
				default:
				  ;
				break;
			}

         /*Advance to next property on input line*/
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		if(UpdateXsc)
		{
			if(with0->NumWindings <= 3)
			{
				int stop = 0;
				for(stop = (with0->NumWindings * (with0->NumWindings - 1) / 2), i = 1; i <= stop; i++)
				{
					switch(i)
					{
						case 	1:
						(with0->XSC)[1 - 1] = with0->XHL;
						break;
						case 	2:
						(with0->XSC)[2 - 1] = with0->XHT;
						break;
						case 	3:
						(with0->XSC)[3 - 1] = with0->XLT;
						break;
						default:
						  ;
						break;
					}
				}
			}
		}
	}
	result = 0;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TXfmrCode::MakeLike(const String Name)
{
	int result = 0;
	TXfmrCodeObj* Other = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this ode in the present collection*/
	Other = ((TXfmrCodeObj*) Find(Name));
	if(Other != nullptr)
		/*# with ActiveXfmrCodeObj do */
		{
			auto with0 = ActiveXfmrCodeObj;
			int stop = 0;
			with0->Fnphases = Other->Fnphases;
			with0->SetNumWindings(Other->NumWindings);
			for(stop = with0->NumWindings, i = 1; i <= stop; i++)
			{
				/*# with with0->WINDING^[i] do */
				{
					
					with0->WINDING_[i - 1].Connection = Other->WINDING_[i - 1].Connection;
					with0->WINDING_[i - 1].kVLL = Other->WINDING_[i - 1].kVLL;
					with0->WINDING_[i - 1].VBase = Other->WINDING_[i - 1].VBase;
					with0->WINDING_[i - 1].kVA = Other->WINDING_[i - 1].kVA;
					with0->WINDING_[i - 1].puTap = Other->WINDING_[i - 1].puTap;
					with0->WINDING_[i - 1].Rpu = Other->WINDING_[i - 1].Rpu;
					with0->WINDING_[i - 1].RdcOhms = Other->WINDING_[i - 1].RdcOhms;
					with0->WINDING_[i - 1].RdcSpecified = Other->WINDING_[i - 1].RdcSpecified;
					with0->WINDING_[i - 1].Rneut = Other->WINDING_[i - 1].Rneut;
					with0->WINDING_[i - 1].Xneut = Other->WINDING_[i - 1].Xneut;
					with0->WINDING_[i - 1].TapIncrement = Other->WINDING_[i - 1].TapIncrement;
					with0->WINDING_[i - 1].MinTap = Other->WINDING_[i - 1].MinTap;
					with0->WINDING_[i - 1].MaxTap = Other->WINDING_[i - 1].MaxTap;
					with0->WINDING_[i - 1].NumTaps = Other->WINDING_[i - 1].NumTaps;
				}
			}
			with0->XHL = Other->XHL;
			with0->XHT = Other->XHT;
			with0->XLT = Other->XLT;
			for(stop = (with0->NumWindings * (with0->NumWindings - 1) / 2), i = 1; i <= stop; i++)
			{
				(with0->XSC)[i - 1] = (Other->XSC)[i - 1];
			}
			with0->ThermalTimeConst = Other->ThermalTimeConst;
			with0->n_thermal = Other->n_thermal;
			with0->m_thermal = Other->m_thermal;
			with0->FLrise = Other->FLrise;
			with0->HSrise = Other->HSrise;
			with0->pctLoadLoss = Other->pctLoadLoss;
			with0->pctNoLoadLoss = Other->pctNoLoadLoss;
			with0->NormMaxHkVA = Other->NormMaxHkVA;
			with0->EmergMaxHkVA = Other->EmergMaxHkVA;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,Other->Get_PropertyValue(i));
			}
			with0->NumAmpRatings = Other->NumAmpRatings;
			with0->AmpRatings.resize(with0->NumAmpRatings);
			for(stop = (sizeof(with0->AmpRatings) - 1), i = 0; i <= stop; i++)
			{
				with0->AmpRatings[i] = Other->AmpRatings[i];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in XfmrCode MakeLike: \"") + Name
	           + "\" Not Found.", 102);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TXfmrCode::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TXfmrCode.Init", -1);
	result = 0;
	return result;
}  // Returns active line code string

String TXfmrCode::Get_Code()
{
	String result;
	result = ((TXfmrCodeObj*) ElementList.Get_Active())->get_Name();
	return result;
}  // sets the  active XfmrCode

void TXfmrCode::Set_Code(const String Value)
{
	TXfmrCodeObj* XfmrCodeObj = nullptr;
	ActiveXfmrCodeObj = nullptr;
	XfmrCodeObj = ((TXfmrCodeObj*) ElementList.Get_First());
	while(XfmrCodeObj != nullptr)
	{
		if(CompareText(XfmrCodeObj->get_Name(), Value) == 0)
		{
			ActiveXfmrCodeObj = XfmrCodeObj;
			return;
		}
		XfmrCodeObj = ((TXfmrCodeObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("XfmrCode: \"") + Value + "\" not Found.", 103);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TXfmrCode Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TXfmrCodeObj::TXfmrCodeObj(TDSSClass* ParClass, const String XfmrCodeName)
 : inherited(ParClass),
			Fnphases(3),
			ActiveWinding(1),
			NumWindings(0),
			MaxWindings(0),
			XHL(0.0),
			XHT(0.0),
			XLT(0.0),
			VABase(0.0),
			NormMaxHkVA(0.0),
			EmergMaxHkVA(0.0),
			ThermalTimeConst(0.0),
			n_thermal(0.0),
			m_thermal(0.0),
			FLrise(0.0),
			HSrise(0.0),
			pctLoadLoss(0.0),
			pctNoLoadLoss(0.0),
			ppm_FloatFactor(0.0),
			pctImag(0.0),
			NumAmpRatings(0)
{
	int i = 0;
	int stop = 0;
	XSC.clear();
	Set_Name(LowerCase(XfmrCodeName));
	DSSObjType = ParClass->DSSClassType;

  // default values and sizes
	NumWindings = 2;
	MaxWindings = 2;
	WINDING_.resize(MaxWindings);
	for(stop = MaxWindings, i = 1; i <= stop; i++)
	{
		WINDING_[i - 1] = TWinding();
	}
	XHL = 0.07;
	XHT = 0.35;
	XLT = 0.30;
	XSC.resize((NumWindings - 1) * (NumWindings / 2));
	VABase = WINDING_[1 - 1].kVA * 1000.0;
	ThermalTimeConst = 2.0;
	n_thermal = 0.8;
	m_thermal = 0.8;
	FLrise = 65.0;
	HSrise = 15.0;  // Hot spot rise
	NormMaxHkVA = 1.1 * WINDING_[1 - 1].kVA;
	EmergMaxHkVA = 1.5 * WINDING_[1 - 1].kVA;
	pctLoadLoss = 2.0 * WINDING_[1 - 1].Rpu * 100.0; //  assume two windings
	ppm_FloatFactor = 0.000001;
  /*Compute antifloat added for each winding    */
	for(stop = NumWindings, i = 1; i <= stop; i++)
	{
		WINDING_[i - 1].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / Fnphases);
	}
	pctNoLoadLoss = 0.0;
	pctImag = 0.0;
	NumAmpRatings = 1;
	AmpRatings.resize(NumAmpRatings);
	AmpRatings[0] = 600;
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


TXfmrCodeObj::~TXfmrCodeObj()
{
	XSC.clear();
	// inherited::Destroy();
}


void TXfmrCodeObj::PullFromTransformer(TTransfObj* Obj)
{
	int i = 0;
	int stop = 0;
	SetNumWindings(Obj->get_NumWindings());
	Fnphases = Obj->Get_NPhases();
	XHL = Obj->get_XHL();
	XHT = Obj->get_XHT();
	XLT = Obj->get_XLT();
	VABase = Obj->get_VABase();
	NormMaxHkVA = Obj->get_NormMaxHkVA();
	EmergMaxHkVA = Obj->get_EmergMaxHkVA();
	ThermalTimeConst = Obj->get_ThermalTimeConst();
	n_thermal = Obj->get_n_thermal();
	m_thermal = Obj->get_m_thermal();
	FLrise = Obj->get_FLrise();
	HSrise = Obj->get_HSrise();
	pctLoadLoss = Obj->get_pctLoadLoss();
	pctNoLoadLoss = Obj->get_pctNoLoadLoss();
	ppm_FloatFactor = Obj->get_ppm_FloatFactor();
	pctImag = Obj->get_pctImag();
	for(stop = (NumWindings - 1) * (NumWindings / 2), i = 1; i <= stop; i++)
	{
		XSC[i - 1] = Obj->Get_Xsc(i);
	}
	for(stop = NumWindings, i = 1; i <= stop; i++)
	{
		WINDING_[i - 1].Connection = Obj->Get_WdgConnection(i - 1);
		WINDING_[i - 1].kVLL = Obj->Get_BasekVLL(i - 1);
		WINDING_[i - 1].VBase = Obj->Get_BaseVoltage(i - 1);
		WINDING_[i - 1].kVA = Obj->Get_WdgkVA(i - 1);
		WINDING_[i - 1].puTap = Obj->Get_PresentTap(i - 1, ActiveActor - 1);
		WINDING_[i - 1].Rpu = Obj->Get_WdgResistance(i - 1);
		WINDING_[i - 1].Rneut = Obj->Get_WdgRneutral(i - 1);
		WINDING_[i - 1].Xneut = Obj->Get_WdgXneutral(i - 1);
		WINDING_[i - 1].Y_PPM = Obj->Get_WdgYPPM(i - 1);
		WINDING_[i - 1].TapIncrement = Obj->Get_TapIncrement(i - 1);
		WINDING_[i - 1].MinTap = Obj->Get_MinTap(i - 1);
		WINDING_[i - 1].MaxTap = Obj->Get_MaxTap(i - 1);
		WINDING_[i - 1].NumTaps = Obj->Get_NumTaps(i - 1);
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TXfmrCodeObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	int stop = 0;
	inherited::DumpProperties(f, Complete);

    /*Basic Property Dump*/
	{ System::Write(f, "~ "); Write(f, "NumWindings="); WriteLn(f, NumWindings, 0); }
	{ System::Write(f, "~ "); Write(f, "phases="); WriteLn(f, Fnphases, 0); }
	for(stop = NumWindings, i = 1; i <= stop; i++)
	{
		/*# with WINDING^[i] do */
		{
			
			if(i == 1)
				{
				System::Write(f, "~ "); System::Write(f, "Wdg="); WriteLn(f, i, 0); }
			else
				{
				System::Write(f, "~ "); System::Write(f, "Wdg="); WriteLn(f, i, 0); }
			switch(WINDING_[i - 1].Connection)
			{
				case 	0:
					System::WriteLn(f, "~ conn=wye");
				break;
				case 	1:
					System::WriteLn(f, "~ conn=delta");
				break;
				default:
				  ;
				break;
			}
			{ System::Write(f, "~ kV="); System::WriteLn(f, WINDING_[i - 1].kVLL, 0, 2); }
			{ System::Write(f, "~ kVA="); System::WriteLn(f, WINDING_[i - 1].kVA, 0, 1); }
			{ System::Write(f, "~ tap="); System::WriteLn(f, WINDING_[i - 1].puTap, 0, 3); }
			{ System::Write(f, "~ %R="); System::WriteLn(f, (WINDING_[i - 1].Rpu * 100.0), 0, 2); }
			System::WriteLn(f, Format("~ RdcOhms=%.7g", WINDING_[i - 1].RdcOhms) );
			{ System::Write(f, "~ rneut="); System::WriteLn(f, WINDING_[i - 1].Rneut, 0, 3); }
			{ System::Write(f, "~ xneut="); System::WriteLn(f, WINDING_[i - 1].Xneut, 0, 3); }
		}
	}
	{ System::Write(f, "~ "); System::Write(f, "XHL="); System::WriteLn(f, XHL * 100.0, 0, 3); }
	{ System::Write(f, "~ "); System::Write(f, "XHT="); System::WriteLn(f, XHT * 100.0, 0, 3); }
	{ System::Write(f, "~ "); System::Write(f, "XLT="); System::WriteLn(f, XLT * 100.0, 0, 3); }
	{ System::Write(f, "~ "); System::Write(f, "X12="); System::WriteLn(f, XHL * 100.0, 0, 3); }
	{ System::Write(f, "~ "); System::Write(f, "X13="); System::WriteLn(f, XHT * 100.0, 0, 3); }
	{ System::Write(f, "~ "); System::Write(f, "X23="); System::WriteLn(f, XLT * 100.0, 0, 3); }
	System::Write(f, "~ Xscmatrix= \"");
	for(stop = (NumWindings - 1) * (NumWindings / 2), i = 1; i <= stop; i++)
	{
		{ System::Write(f, XSC[i - 1] * 100.0, 0, 2); System::Write(f, L' '); }
	}
	System::WriteLn(f, L'\"');
	{ System::Write(f, "~ "); System::Write(f, "NormMAxHkVA="); System::WriteLn(f, NormMaxHkVA, 0, 0); }
	{ System::Write(f, "~ "); System::Write(f, "EmergMAxHkVA="); System::WriteLn(f, EmergMaxHkVA, 0, 0); }
	{ System::Write(f, "~ "); System::Write(f, "thermal="); System::WriteLn(f, ThermalTimeConst, 0, 1); }
	{ System::Write(f, "~ "); System::Write(f, "n="); System::WriteLn(f, n_thermal, 0, 1); }
	{ System::Write(f, "~ "); System::Write(f, "m="); System::WriteLn(f, m_thermal, 0, 1); }
	{ System::Write(f, "~ "); System::Write(f, "flrise="); System::WriteLn(f, FLrise, 0, 0); }
	{ System::Write(f, "~ "); System::Write(f, "hsrise="); System::WriteLn(f, HSrise, 0, 0); }
	{ System::Write(f, "~ "); System::Write(f, "%loadloss="); System::WriteLn(f, pctLoadLoss, 0, 0); }
	{ System::Write(f, "~ "); System::Write(f, "%noloadloss="); System::WriteLn(f, pctNoLoadLoss, 0, 0); }
	for(stop = NumPropsThisClass, i = 28; i <= stop; i++)
	{
		{ System::Write(f, "~ "); System::Write(f, (*ParentClass->PropertyName)[i]); System::Write(f, L'='); System::WriteLn(f, Get_PropertyValue(i)); }
	}
	/*# with ParentClass do */
	{
		auto with1 = ParentClass;
		int stop = 0;
		for(stop = with1->NumProperties, i = NumPropsThisClass + 1; i <= stop; i++)
		{
			{ System::Write(f, "~ "); System::Write(f, (*with1->PropertyName)[i]); System::Write(f, L'='); System::WriteLn(f, Get_PropertyValue(i)); }
		}
	}
}

/* gets the property for the active winding ; Set the active winding before calling*/

String TXfmrCodeObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	int k = 0;
	String TempStr;
	switch(Index)
	{
		case 11: case 12: case 13: case 14:
		 case 18:
		 case 33:
		result = "[";
		break;
		default:
		result = "";
		break;
	}
	switch(Index)
	{
		case 	3:
		result = IntToStr(ActiveWinding);
		break;  // return active winding
		case 	4:
		switch(WINDING_[ActiveWinding - 1].Connection)
		{
			case 	0:
			result = "wye ";
			break;
			case 	1:
			result = "delta ";
			break;
			default:
			  ;
			break;
		}
		break;
		case 	5:
		result = Format("%.7g", WINDING_[ActiveWinding- 1].kVLL);
		break;
		case 	6:
		result = Format("%.7g", WINDING_[ActiveWinding- 1].kVA);
		break;
		case 	7:
		result = Format("%.7g", WINDING_[ActiveWinding- 1].puTap);
		break;
		case 	8:
		result = Format("%.7g", WINDING_[ActiveWinding- 1].Rpu * 100.0);
		break;   // %R
		case 	9:
		result = Format("%.7g", WINDING_[ActiveWinding- 1].Rneut);
		break;
		case 	10:
		result = Format("%.7g", WINDING_[ActiveWinding- 1].Xneut);
		break;
		case 	11:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			switch(WINDING_[i - 1].Connection)
			{
				case 	0:
				result = result + "wye, ";
				break;
				case 	1:
				result = result + "delta, ";
				break;
				default:
				  ;
				break;
			}
		}
		break;
		case 	12:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", WINDING_[i- 1].kVLL);
		}
		break;
		case 	13:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", WINDING_[i- 1].kVA);
		}
		break;
		case 	14:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", WINDING_[i- 1].puTap);
		}
		break;
		case 	18:
		for(int stop = (NumWindings - 1) * (NumWindings / 2), i = 1; i <= stop; i++)
		{
			result = result + Format("%-g, ", XSC[i- 1] * 100.0);
		}
		break;
		case 	24:
		result = Format("%.7g", pctLoadLoss);
		break;
		case 	25:
		result = Format("%.7g", pctNoLoadLoss);
		break;
		case 	28:
		result = Format("%.7g", WINDING_[ActiveWinding - 1].MaxTap);
		break;
		case 	29:
		result = Format("%.7g", WINDING_[ActiveWinding - 1].MinTap);
		break;
		case 	30:
		result = Format("%-d", WINDING_[ActiveWinding - 1].NumTaps);
		break;
		case 	33:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", WINDING_[i - 1].Rpu * 100.0);
		}
		break;
		case 	37:
		result = Format("%.7g", WINDING_[ActiveWinding - 1].RdcOhms);
		break;
		case 	38:
		result = IntToStr(NumAmpRatings);
		break;
		case 	39:
		{
			int stop = 0;
			TempStr = "[";
			for(stop = NumAmpRatings, k = 1; k <= stop; k++)
			{
				TempStr = TempStr + FloatToStrF(AmpRatings[k - 1], ffGeneral, 8, 4) + ",";
			}
			TempStr = TempStr + "]";
			result = TempStr;
		}
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	switch(Index)
	{
		case 11: case 12: case 13: case 14:
		 case 18:
		 case 33:
		result = result + "]";
		break;
		default:
		  ;
		break;
	}
	return result;
}

void TXfmrCodeObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"3"); //'phases';
	Set_PropertyValue(2,"2"); //'windings';
	Set_PropertyValue(3,"1"); //'wdg';
	Set_PropertyValue(4,"wye"); // 'conn';
	Set_PropertyValue(5,"12.47"); // IF 2or 3-phase:  phase-phase    ELSE actual winding
	Set_PropertyValue(6,"1000");
	Set_PropertyValue(7,"1.0");
	Set_PropertyValue(8,"0.2");
	Set_PropertyValue(9,"-1");
	Set_PropertyValue(10,"0");
	Set_PropertyValue(11,"");
	Set_PropertyValue(12,""); // IF 1-phase: actual winding rating; ELSE phase-phase
	Set_PropertyValue(13,""); // IF 1-phase: actual winding rating; ELSE phase-phase
	Set_PropertyValue(14,"");
	Set_PropertyValue(15,"7");
	Set_PropertyValue(16,"35");
	Set_PropertyValue(17,"30");
	Set_PropertyValue(18,"");  // x12 13 14... 23 24.. 34 ..
	Set_PropertyValue(19,"2");
	Set_PropertyValue(20,".8");
	Set_PropertyValue(21,".8");
	Set_PropertyValue(22,"65");
	Set_PropertyValue(23,"15");
	Set_PropertyValue(24,"0");
	Set_PropertyValue(25,"0");
	Set_PropertyValue(26,"");
	Set_PropertyValue(27,"");
	Set_PropertyValue(28,"1.10");
	Set_PropertyValue(29,"0.90");
	Set_PropertyValue(30,"32");
	Set_PropertyValue(31,"0");
	Set_PropertyValue(32,"1");
	Set_PropertyValue(33,"");
	Set_PropertyValue(34,"7");
	Set_PropertyValue(35,"35");
	Set_PropertyValue(36,"30");
	Set_PropertyValue(37,"0.1");
	Set_PropertyValue(38,"1");
	Set_PropertyValue(39,"[600]");
	inherited::InitPropertyValues(NumPropsThisClass);
}

}  // namespace XfmrCode





