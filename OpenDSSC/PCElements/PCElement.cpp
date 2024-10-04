
#pragma hdrstop

#include "PCElement.h"
#include "DSSGlobals.h"
#include "MeterElement.h"

using namespace DynamicExp;
using namespace std;


namespace PCElement
{

//TPCElement::TPCElement(TDSSClass* ParClass) : inherited(ParClass) {}
TPCElement::TPCElement(String ClassName) : inherited(ClassName) {}
TPCElement::TPCElement() {}



TPCElement::TPCElement(TDSSClass* ParClass)
 : inherited(ParClass),
			FIterminalUpdated(false),
			SpectrumObj(nullptr),
			MeterObj(nullptr),
			SensorObj(nullptr),
			FMonObj(nullptr),
			Cluster_num(0),
			NdNumInCluster(0),
			nVLeaders(0),
			FMonObj2(nullptr),
			cluster_num2(0),
			NdNumInCluster2(0),
			InjCurrent(nullptr)
{
	Spectrum		= "default";
	SpectrumObj		= nullptr;  // have to allocate later because not guaranteed there will be one now.
	SensorObj		= nullptr;
	MeterObj		= nullptr;
	InjCurrent		= nullptr;
	DSSObjType		= PC_ELEMENT;
	DynamicEq		= "";
	DynamicEqObj	= NULL;
	NumStateVars	= 0;
	DynamicEqPair.resize(0);
	DynamicEqVals.resize(0);
	DynOut.resize(0);
}

TPCElement::~TPCElement()
{
	if(InjCurrent != NULL)
		free(InjCurrent);
	// inherited::Destroy();
}


// Add injection currents into System currents array

int TPCElement::InjCurrents(int ActorID)
{
	int result = 0;
	int i = 0;
	result = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;

		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			caccum(with0->Currents[NodeRef[i - 1]], InjCurrent[i - 1]);
		}

	}
	return result;
}

void TPCElement::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	DoErrorMsg("PCElement.InjCurrents", (String("Improper call to GetInjCurrents for Element: ") + get_Name() + "."), "Called PCELEMENT class virtual function instead of actual.", 640);
}

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


// This is called only if we need to compute the terminal currents from the inj currents
// Such as for Harmonic model

void TPCElement::GetTerminalCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	if(FIterminalUpdated)   // Just copy iTerminal unless iTerminal=Curr
	{
		if(Curr != &(Iterminal[0]))
		{
			int stop = 0;
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				(Curr)[i - 1] = (Iterminal)[i - 1];
			}
		}
	}
	else
	{
		int stop = 0;
		YPrim->MVmult(Curr, &(Vterminal[0]));
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			caccum((Curr)[i - 1], cnegate((InjCurrent)[i - 1]));
		}
		set_ITerminalUpdated(true, ActorID);
	}
	IterminalSolutionCount[ActorID] = ActiveCircuit[ActorID]->Solution->SolutionCount;
}

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


/*Gets total Currents going INTO a devices terminals*/

void TPCElement::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			if(Get_Enabled())
			{
				if((with0->LastSolutionWasDirect) && (!(with0->IsDynamicModel || with0->IsHarmonicModel)))
       
           // Take a short cut and get Currents from YPrim only
           // For case where model is entirely in Y matrix
				{
					CalcYPrimContribution(Curr, ActorID);
				}
				else
				{
					GetTerminalCurrents(Curr, ActorID);
				} /*IF*/
			}
			else
   // not enabled
			{
				int stop = 0;
				for(stop = Yorder, i = 1; i <= stop; i++)
				{
					(Curr)[i - 1] = CZero;
				}
			}
		}  /*With*/
	}
	catch(exception &e)
	{
		DoErrorMsg((String("GetCurrents for Element: ") + get_Name() + "."), (string)e.what(), "Inadequate storage allotted for circuit element.", 641);
	}
}
//----------------------------------------------------------------------------
// Evaluates if the value provided corresponds to a constant value or to an operand
// for calculating the value using the simulation results
int TPCElement::CheckIfDynVar(string myVar, int ActorID)
{
	int myOp, Result = 0;
	string myValue = "";

	Result = -1;
	if ( ASSIGNED( DynamicEqObj ) )
	{
		Result = DynamicEqObj->Get_Var_Idx(myVar);
		if ((Result >= 0) && (Result < 50000))
		{
			myValue = Parser[ActorID]->MakeString_();
			if (DynamicEqObj->Check_If_CalcValue(myValue, myOp))
			{
				// Adss the pair (var index + operand index)
				DynamicEqPair.push_back(Result);
				DynamicEqPair.push_back(myOp);
			}
			else
			{
				// Otherwise, move the value to the values array
				DynamicEqVals[Result][0] = Parser[ActorID]->MakeDouble_();
			}
		}
		else
			Result = -1;
	}

	return Result;
}

//----------------------------------------------------------------------------
//{Returns the names of the variables to be used as outputs for the dynamic expression}
std::string TPCElement::GetDynOutputStr()
{
	int idx				= 0;
	std::string Result	= "[";

	if (ASSIGNED(DynamicEqObj))
	{
		for (idx = 0; idx < DynOut.size(); idx++)
		{
			Result = Result + DynamicEqObj->Get_VarName(DynOut[idx]) + ",";
		}
	}

	Result = Result + "]";

	return Result;
}

//----------------------------------------------------------------------------
//{Obtains the indexes of the given variables to use them as reference for setting
//the dynamic output for the generator}
void TPCElement::SetDynOutput(string myVar)
{
	int idx					= 0;
	int VarIdx				= 0;
	TStringList myStrArray;

	if (ASSIGNED(DynamicEqObj))
	{
		DynOut.resize(2);
		myStrArray.resize(0);
		InterpretTStringListArray(myVar, myStrArray);
		for (idx = 0; idx < myStrArray.size(); idx++)
		{
			myStrArray[idx] = LowerCase(myStrArray[idx]);
			VarIdx = DynamicEqObj->Get_Out_Idx(myStrArray[idx]);
			if (VarIdx < 0)
				// Being here means that the given name doesn't exist or is a constant
				DoSimpleMsg("DynamicExp variable " + myStrArray[idx] + " not found or not defined as an output.", 50008);
			else
				DynOut[idx] = VarIdx;
		}
		myStrArray.resize(0);
	}
	else
		DoSimpleMsg("A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [" + myVar + "]", 50007);
}

void TPCElement::CalcYPrimContribution(pComplexArray Curr, int ActorID)
{
	ComputeVterminal(ActorID);
      // Apply these voltages to Yprim
	YPrim->MVmult(Curr, &(Vterminal[0]));
}

void TPCElement::InitHarmonics(int ActorID)
{

  // By default do nothing in the base class
}

void TPCElement::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(ArrayOffset + 1,Spectrum);
	inherited::InitPropertyValues(ArrayOffset + 1);
}

void TPCElement::InitStateVars(int ActorID)
{

    // By default do nothing
}

void TPCElement::IntegrateStates(int ActorID)
{

 // inherited;
 // By default do nothing
}

int TPCElement::InitializeStates(int ActorID)
{

	// inherited;
	// By default do nothing
	return 0;
}

void TPCElement::CalculateRate(int ActorID)
{

	// inherited;
	// By default do nothing
}

void TPCElement::StateIntegration(int ActorID)
{
	// inherited;
	// By default do nothing

}

void TPCElement::StateIntegration_correction(int ActorID)
{
	// inherited;
	// By default do nothing

}

void TPCElement::GetAllVariables(pDoubleArray States)
{

     /*Do Nothing*/
}

bool TPCElement::get_FITerminalUpdated(int ActorID, int value)
{
	return FIterminalUpdated;
}

int TPCElement::NumVariables()
{
	int result = 0;
	result = 0;
	return result;
}

String TPCElement::VariableName(int i)
{
	String result;
   /*Do Nothing*/
	result = "";
	return result;
}

/*Search through variable name list and return index if found*/
/*Compare up to length of S*/

int TPCElement::LookupVariable(const String s)
{
	int result = 0;
	int i = 0;
	int TestLength = 0;
	int stop = 0;
	result = -1;   // Returns -1 for error not found
	TestLength = (int) s.size();
	for(stop = NumVariables(), i = 1; i <= stop; i++)
	{
		if(CompareText(VariableName(i).substr(0, TestLength), s) == 0)
		{
			result = i;
			break;
		}
	}
	return result;
}

bool TPCElement::CheckForGeneratorModel(void)
{
	//	
	return false;
}

bool TPCElement::IsGenerator()
{
	//	Will get here only for PCelemets that are not generators
	return false;
}

void TPCElement::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	if(Complete)
	{
		int stop = 0;
		WriteLn(f, L"! VARIABLES");
		for(stop = NumVariables(), i = 1; i <= stop; i++)
		{
			{ Write(f, "! "); Write(f, i, 2); Write(f, ": "); Write(f, VariableName(i)); Write(f, " = "); WriteLn(f, Format("%-.5g", Get_Variable(i))); }
		}
	}
}

double TPCElement::Get_Variable(int i)
{
	double result = 0.0;
   /*do Nothing here -- up to override function*/
	result = -9999.99;
	return result;
}

void TPCElement::Set_Variable(int i, double Value)
{
	if (i < 0)
	{
		//Do Nothing
	}
	else
	{
		//return Value;
	}
  /*Do Nothing*/
}

void TPCElement::ComputeIterminal(int ActorID)
{
	if(IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount)
	{
		GetCurrents(&(Iterminal[0]), ActorID);
		IterminalSolutionCount[ActorID] = ActiveCircuit[ActorID]->Solution->SolutionCount;
	}
}

void TPCElement::ZeroInjCurrent()
{
	int i = 0;
	int stop = 0;
	for(stop = Yorder, i = 1; i <= stop; i++)
	{
		(InjCurrent)[i - 1] = CZero;
	}
}

void TPCElement::set_ITerminalUpdated(bool Value, int ActorID)
{
	FIterminalUpdated = Value;
	if(Value)
		IterminalSolutionCount[ActorID] = ActiveCircuit[ActorID]->Solution->SolutionCount;
}




}  // namespace PCElement




