
#pragma hdrstop

#include "Executive.h"

#include "DSSGlobals.h"
#include "Utilities.h"
#include "Solution.h"
#include "CmdForms.h"
#include "klusolve.h"
#include "ExecCommands.h"
#include "ExecOptions.h"

using namespace std;


namespace Executive
{


     /*ExecHelper,*/


//----------------------------------------------------------------------------

TExecutive::TExecutive()
 : FRecorderOn(false),
			FRecorderFile("")
{
	;


     // Exec Commands
	CommandList = TCommandList(ExecCommand, NumExecCommands);

     // Exec options
	OptionList = new TCommandList(ExecOption, NumExecOptions);
     /*Instantiate All DSS Classe Definitions, Intrinsic and User-defined*/
	CreateDSSClasses();     // in DSSGlobals
	Circuits = TPointerList(2);   // default buffer for 2 active circuits
//     ActiveCircuit[ActiveActor] := nil;
//     Parser := TParser.Create;  // Create global parser object (in DSS globals)
	LastCmdLine = "";
	RedirFile = "";
	ReadDSS_Registry();

     /*Override Locale defaults so that CSV files get written properly*/
//	FormatSettings.DecimalSeparator = L'.';
//	FormatSettings.ThousandSeparator = L',';
}


//----------------------------------------------------------------------------

 TExecutive::~TExecutive()
{
	int i = 0;

    /*Write some global Variables to Registry*/
	WriteDSS_Registry();
	ClearAllCircuits();
	delete OptionList;
	DisposeDSSClasses(true);
	delete Parser[ActiveActor];
	Parser[ActiveActor] = nullptr;
	// inherited::Destroy();
}


//----------------------------------------------------------------------------

String TExecutive::Get_LastError()
{
	String result;
	result = LastErrorMessage;
	return result;
}

//----------------------------------------------------------------------------

int TExecutive::Get_ErrorResult()
{
	int result = 0;
	result = ErrorNumber;
	return result;
}


//----------------------------------------------------------------------------


/*Create default loadshapes, growthshapes, and other general DSS objects
 used by all circuits.
*/

void TExecutive::CreateDefaultDSSItems()
{


/* this load shape used for generator dispatching, etc.   Loads may refer to it, also.*/
	Set_Command( "new loadshape.default npts=24 1.0 mult=(.677 .6256 .6087 .5833 .58028 .6025 .657 .7477 .832 .88 .94 .989 .985 .98 .9898 .999 1 .958 .936 .913 .876 .876 .828 .756)");
	if(CmdResult == 0)
	{
		Set_Command( "new growthshape.default 2 year=\"1 20\" mult=(1.025 1.025)");  // 20 years at 2.5%
		Set_Command( "new spectrum.default 7  Harmonic=(1 3 5 7 9 11 13)  %mag=(100 33 20 14 11 9 7) Angle=(0 0 0 0 0 0 0)");
		Set_Command( "new spectrum.defaultload 7  Harmonic=(1 3 5 7 9 11 13)  %mag=(100 1.5 20 14 1 9 7) Angle=(0 180 180 180 180 180 180)");
		Set_Command( "new spectrum.defaultgen 7  Harmonic=(1 3 5 7 9 11 13)  %mag=(100 5 3 1.5 1 .7 .5) Angle=(0 0 0 0 0 0 0)");
		Set_Command( "new spectrum.defaultvsource 1  Harmonic=(1 )  %mag=(100 ) Angle=(0 ) ");
		Set_Command( "new spectrum.linear 1  Harmonic=(1 )  %mag=(100 ) Angle=(0 ) ");
		Set_Command( "new spectrum.pwm6 13  Harmonic=(1 3 5 7 9 11 13 15 17 19 21 23 25) %mag=(100 4.4 76.5 62.7 2.9 24.8 12.7 0.5 7.1 8.4 0.9 4.4 3.3) Angle=(-103 -5 28 -180 -33 -59 79 36 -253 -124 3 -30 86)");
		Set_Command( "new spectrum.dc6 10  Harmonic=(1 3 5 7 9 11 13 15 17 19)  %mag=(100 1.2 33.6 1.6 0.4 8.7  1.2  0.3  4.5 1.3) Angle=(-75 28 156 29 -91 49 54 148 -57 -46)");
		Set_Command( "New TCC_Curve.A 5 c_array=(1, 2.5, 4.5, 8.0, 14.)  t_array=(0.15 0.07 .05 .045 .045) ");
		Set_Command( "New TCC_Curve.D 5 c_array=(1, 2.5, 4.5, 8.0, 14.)  t_array=(6 0.7 .2 .06 .02)");
		Set_Command( "New TCC_Curve.TLink 7 c_array=(2 2.1 3 4 6 22 50)  t_array=(300 100 10.1 4.0 1.4 0.1  0.02)");
		Set_Command( "New TCC_Curve.KLink 6 c_array=(2 2.2 3 4 6 30)    t_array=(300 20 4 1.3 0.41 0.02)");
		Set_Command( "New \"TCC_Curve.uv1547\" npts=2 C_array=(0.5, 0.9, ) T_array=(0.166, 2, )");
		Set_Command( "New \"TCC_Curve.ov1547\" npts=2 C_array=(1.1, 1.2, ) T_array=(2, 0.166, )");
		Set_Command( "New \"TCC_Curve.mod_inv\" npts=15 C_array=(1.1, 1.3, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100, ) T_array=(27.1053, 9.9029, 6.439, 3.8032, 2.4322, 1.9458, 1.6883, 1.5255, 1.4117, 1.3267, 1.2604, 1.2068, 0.9481, 0.7468, 0.6478, )");
		Set_Command( "New \"TCC_Curve.very_inv\" npts=15 C_array=(1.1, 1.3, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100, ) T_array=(93.872, 28.9113, 16.179, 7.0277, 2.9423, 1.7983, 1.3081, 1.0513, 0.8995, 0.8023, 0.7361, 0.6891, 0.5401, 0.4988, 0.493, )");
		Set_Command( "New \"TCC_Curve.ext_inv\" npts=15 C_array=(1.1, 1.3, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100, ) T_array=(134.4074, 40.9913, 22.6817, 9.5217, 3.6467, 2.0017, 1.2967, 0.9274, 0.7092, 0.5693, 0.4742, 0.4065, 0.1924, 0.133, 0.1245, )");
		Set_Command( "New \"TCC_Curve.definite\" npts=3 C_array=(1, 1.001, 100, ) T_array=(300, 1, 1, )");
	}
}

String TExecutive::Get_Command()
{
	String result;
	result = LastCmdLine;
	return result;
}

void TExecutive::Set_Command(String Value)
{
	int Idx = 0;
	if(AllActors)    // Applies the same command to all the actors
	{
		int stop = 0;
		for(stop = NumOfActors, Idx = 1; Idx <= stop; Idx++)
		{
			if(AllActors)
				ActiveActor = Idx;
			ProcessCommand(Value);
		}
	}
	else
	{
		ProcessCommand(Value);                  // Applies the command to the active actor
	}
}

void TExecutive::Clear()
{
	int i = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
          /*First get rid of all existing stuff*/
	{
		Circuits = TPointerList(2);         // Make a new list of circuits
		DisposeDSSClasses(false);
		ActiveCircuit[ActiveActor]->NumCircuits = 0; // <<<< added
		delete ActiveCircuit[ActiveActor]; // ActiveCircuit[ActiveActor]->~TDSSCircuit();             // <<<< added
		ActiveCircuit[ActiveActor] = nullptr;             // <<<< added
           // In case the actor hasn't been destroyed
		if(ActorHandle[ActiveActor] != nullptr)
		{
			ActorHandle[ActiveActor]->Send_Message(EXIT_ACTOR);
			//ActorHandle[ActiveActor]->WaitFor();
			delete ActorHandle[ActiveActor]; // ActorHandle[ActiveActor]->~TSolver();
			ActorHandle[ActiveActor] = nullptr;
		}

            /*Now, Start over*/
		CreateDSSClasses();
		CreateDefaultDSSItems();
		RebuildHelpForm = true; // because class strings have changed
	}
//	if(!IsDLL)
//		ControlPanel.UpdateElementBox;
	DefaultEarthModel = DERI;
	LogQueries = false;
	MaxAllocationIterations = 2;

       /*Prepare for new variables*/
	delete ParserVars;
	ParserVars = new TParserVar(100);  // start with space for 100 variables
}

void TExecutive::ClearAll()
{
	int i = 0;
       /*First get rid of all existing stuff*/
	ClearAllCircuits();
	DisposeDSSClasses(true);
       /*Now, Start over*/
	ActiveActor = 1;
	CreateDSSClasses();
	if (Parser[ActiveActor] != NULL)
		delete Parser[ActiveActor];
	Parser[ActiveActor] = new ParserDel::TParser();
	if (AuxParser[ActiveActor] != NULL)
		delete AuxParser[ActiveActor];
	AuxParser[ActiveActor] = new ParserDel::TParser();
	CreateDefaultDSSItems();
	RebuildHelpForm = true; // because class strings have changed
//	if(!isDLL)
//		ControlPanel.UpdateElementBox; 
       /*Prepare for new variables*/
	delete ParserVars;
	ParserVars = new TParserVar(100);  // start with space for 100 variables
	ActiveActor = 1;
	NumOfActors = 1;
}

void TExecutive::Set_RecorderOn(bool Value)
{
	if(Value)
	{
		if(!FRecorderOn)
		{
			FRecorderFile = GetOutputDirectory() + "DSSRecorder.DSS";
			AssignFile(RecorderFile, FRecorderFile);
		}
		Rewrite(RecorderFile);
		IOResultToException();
	}
	else
	{
		if(FRecorderOn)
		{
			CloseFile(RecorderFile);
		}
	}
	GlobalResult = FRecorderFile;
	FRecorderOn = Value;
}

bool TExecutive::get_FRecorderOn()
{
	return FRecorderOn;
}

void TExecutive::Write_to_RecorderFile(const String s)
{
	WriteLn(RecorderFile, s);
}

//WriteDLLDebugFile('Executive');




}  // namespace Executive




