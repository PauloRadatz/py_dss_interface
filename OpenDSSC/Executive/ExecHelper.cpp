#pragma hdrstop

#include "ExecHelper.h"
#include "dirsep.h"
#include <gencls.h>
#include <genrou.h>
#include <ExcSexs.h>
#include <tgov.h>



using namespace std;
using namespace Arraydef;
using namespace Bus;
using namespace Capacitor;
using namespace Circuit;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace EnergyMeter;
using namespace ExecCommands;
using namespace Executive;
using namespace Generator;
using namespace Line;
using namespace LineUnits;
using namespace Load;
using namespace LoadShape;
using namespace Monitor;
using namespace NamedObject;
using namespace PCElement;
using namespace PDELement;
using namespace ParserDel;
using namespace Pstcalc;
using namespace Reactor;
using namespace ReduceAlgs;
using namespace Sensor;
using namespace Solution;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace ExecHelper
{
#define ExecHelper__0 (TPerlRegExOptions() << preCaseLess)

 /*ShowResults, ExportResults,*/
TCommandList* SaveCommands = nullptr;
TCommandList* DistributeCommands = nullptr;
TCommandList* DI_PlotCommands = nullptr;
TCommandList* ReconductorCommands = nullptr;
TCommandList* RephaseCommands = nullptr;
TCommandList* AddMarkerCommands = nullptr;
TCommandList* SetBusXYCommands = nullptr;
TCommandList* PstCalcCommands = nullptr;
TCommandList* RemoveCommands = nullptr;
TCommandList* FNCSPubCommands = nullptr;


//----------------------------------------------------------------------------

void GetObjClassAndName(String& ObjClass, String& ObjName)
{
	String ParamName;
	String Param;

/*
  We're looking for Object Definition:

      ParamName = 'object' IF given
     and the name of the object

     Object=Capacitor.C1
    or just Capacitor.C1

  If no dot, last class is assumed
*/
	ObjClass = "";
	ObjName = "";
	ParamName = LowerCase(Parser[ActiveActor]->GetNextParam());
	Param = Parser[ActiveActor]->MakeString_();
	if(ParamName.size() > 0)   // IF specified, must be object or an abbreviation
	{
		if(CompareTextShortest(ParamName, "object") != 0)
		{
			DoSimpleMsg(String("object=Class.Name expected as first parameter in command.") + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 240);
			return;
		}
	}
	ParseObjectClassandName(Param, ObjClass, ObjName);     // see DSSGlobals
}


//----------------------------------------------------------------------------

int DoNewCmd()
{
	int result = 0;
	String ObjClass;
	String ObjName;
	int Handle = 0;
	result = 0;
	Handle = 0;
	GetObjClassAndName(ObjClass, ObjName);
	if(CompareText(ObjClass, "solution") == 0)
	{
		DoSimpleMsg("You cannot create new Solution objects through the command interface.", 241);
		return result;
	}
	if(CompareText(ObjClass, "circuit") == 0)
	{
		MakeNewCircuit(ObjName);  // Make a new circuit
		ClearEventLog();      // Start the event log in the current directory
		ClearErrorLog();
	}
	else
    // Everything else must be a circuit element or DSS Object
	{
		Handle = AddObject(ObjClass, ObjName);
	}
	if(Handle == 0)
		result = 1;
	return result;
}

// Process the New Command
// new type=xxxx name=xxxx  editstring

// IF the device being added already exists, the default behavior is to
// treat the New command as an Edit command.  This may be overridden
// by setting the DuplicatesAllowed VARiable to true, in which CASE,
// the New command always results in a new device being added.


//----------------------------------------------------------------------------

int DoEditCmd()
{
	int		result = 0;
	String	ObjType = "",
			ObjName = "";

	GetObjClassAndName(ObjType, ObjName);
	if(CompareText(ObjType, "circuit") == 0)
		;
	else

                 // Do nothing


        // Everything ELSE must be a circuit element
	{
		result = EditObject(ObjType, ObjName);
	}
	return result;
}

// edit type=xxxx name=xxxx  editstring


//----------------------------------------------------------------------------

int DoBatchEditCmd()
{
	int result = 0;
	String ObjType;
	String Pattern;
	TPerlRegEx* RegEx1 = nullptr;
	TDSSObject* pObj = nullptr;
	int Params = 0;
	int iElement = 0;
	result = 0;
	GetObjClassAndName(ObjType, Pattern);
	if(CompareText(ObjType, "circuit") == 0)
		;
	else

    // Do nothing
	{
		LastClassReferenced[ActiveActor] = ClassNames[ActiveActor].Find(ObjType);
		switch(LastClassReferenced[ActiveActor])
		{
			case 	0:
			{
				DoSimpleMsg(String("BatchEdit Command: Object Type \"") + ObjType
	           + "\" not found."
	           + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 267);
				return result;
			}/*Error*/
			default:
			Params = Parser[ActiveActor]->get_position();
			ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
			RegEx1 = new TPerlRegEx();
//			RegEx1->Options = ExecHelper__0;
			RegEx1->set_FRegEx(Pattern); // AnsiString(Pattern);
			if(ActiveDSSClass[ActiveActor]->Get_First() > 0)
				pObj = (TDSSObject*) ActiveDSSObject[ActiveActor];
			else
				pObj = nullptr;
			while(pObj != nullptr)
			{
				RegEx1->set_mySubject(pObj->get_Name()); //(pObj.Name);
				if(RegEx1->Match())
				{
					Parser[ActiveActor]->set_position(Params);
					ActiveDSSClass[ActiveActor]->Edit(ActiveActor);
				}
				if(ActiveDSSClass[ActiveActor]->Get_Next() > 0)
					pObj = (TDSSObject*) ActiveDSSObject[ActiveActor];
				else
					pObj = nullptr;
			}
			break;
		}
	}
	return result;
}
// batchedit type=xxxx name=pattern  editstring
 

//----------------------------------------------------------------------------

int DoRedirect(bool IsCompile)
{
	int result = 0;
	TTextRec Fin = {};
	String ParamName;
	String InputLine;
	String CurrDir;
	String SaveDir;
	String LocalCompFileName;
	bool InBlockComment = false;
	result = 0;
	InBlockComment = false;  // Discareded off stack upon return
    // Therefore extent of block comment does not extend beyond a file
    // Going back up the redirect stack

    // Get next parm and try to interpret as a file name
	ParamName = Parser[ActiveActor]->GetNextParam();
	RedirFile = ExpandFileName(Parser[ActiveActor]->MakeString_());
	if(RedirFile.length())
	{
		SaveDir = GetCurrentDir();
		try
		{
			AssignFile(Fin, RedirFile);
			Reset(Fin);
			IOResultToException();
			if(IsCompile)
			{
				LastFileCompiled = RedirFile;
				LocalCompFileName = RedirFile;
			}

         // Couldn't find file  Try appending a '.dss' to the file name
         // If it doesn't already have an extension
		}
		catch(...)
		{
			if(Pos(".", RedirFile) == 0)
			{
				RedirFile = RedirFile + ".dss";
				try
				{
					AssignFile(Fin, RedirFile);
					Reset(Fin);
					IOResultToException();
				}
				catch(...)
				{
					DoSimpleMsg("Redirect File: \"" + RedirFile + "\" Not Found.", 242);
					SolutionAbort = true;
					return result;
				}
			}
			else
			{
				DoSimpleMsg("Redirect File: \"" + RedirFile + "\" Not Found.", 243);
				SolutionAbort = true;
				return result;
			}
		}

    // OK, we finally got one open, so we're going to continue
		try
		{
			try

             // Change Directory to path specified by file in CASE that
             // loads in more files
			{
				CurrDir = ExtractFileDir(RedirFile);
				SetCurrentDir(CurrDir);
				if(IsCompile)
					SetDataPath(CurrDir);  // change datadirectory
				Redirect_Abort = false;
				In_Redirect = true;
				while(!((Eof(Fin)) || (Redirect_Abort)))
				{
					ReadLn(Fin, InputLine);
					if(InputLine.size() > 0)
					{
						if(!InBlockComment)
							switch(InputLine[0])
							{     // look for '/*'  at baginning of line
								case 	L'/':
								if((InputLine.size() > 1) && (InputLine[1] == L'*'))
									InBlockComment = true;
								break;
								default: {}
							}
						if(!InBlockComment)
						{
							if (!SolutionAbort)   // process the command line
							{
								int NumOfTimes = 1;
								if (AllActors)
									NumOfTimes = NumOfActors;
								for (int idx = 1; idx <= NumOfTimes; idx++)
								{
									if (AllActors) ActiveActor = idx;
									ProcessCommand(InputLine);
								}
								
							}
							else
								Redirect_Abort = true;  // Abort file if solution was aborted

                      // in block comment ... look for */   and cancel block comment (whole line)
						}
						if(InBlockComment)
						{
							if(Pos("*/", InputLine) > 0)
								InBlockComment = false;
						}
					}
				}
				if(ActiveCircuit[ActiveActor] != nullptr)
					ActiveCircuit[ActiveActor]->CurrentDirectory = CurrDir + DIRSEP_STR;
			}
			catch (std::exception &e)
			{
				DoErrorMsg(String("DoRedirect") + CRLF + "Error Processing Input Stream in Compile/Redirect.", (std::string) e.what(), String("Error in File: \"") + RedirFile + "\" or Filename itself.", 244);
			}
/* }
		__finally
		{*/
			CloseFile(Fin);
			In_Redirect = false;
			ParserVars->Add("@lastfile", RedirFile);
			if(IsCompile)
			{
				SetDataPath(CurrDir); // change datadirectory
				LastCommandWasCompile = true;
				ParserVars->Add("@lastcompilefile", LocalCompFileName); // will be last one off the stack - had to remove the '@', it was killing the process
			}
			else
			{
				SetCurrentDir(SaveDir);    // set back to where we were for redirect, but not compile
				ParserVars->Add("@lastredirectfile", RedirFile);
			}
		}
		catch (...)
		{
			//
		}
	}  // ELSE ignore altogether IF null filename
	return result;
}

//  This routine should be recursive
//  So you can redirect input an arbitrary number of times

// If Compile, makes directory of the file the new home directory
// If not Compile (is simple redirect), return to where we started


//----------------------------------------------------------------------------

int DoSelectCmd()
{
	int result = 0;
	String ObjClass;
	String ObjName;
	String ParamName;
	String Param;
	result = 1;
	GetObjClassAndName(ObjClass, ObjName);  // Parse Object class and name
	if((ObjClass.size() == 0) && (ObjName.size() == 0))
		return result;  // select active obj if any
	if(CompareText(ObjClass, "circuit") == 0)
	{
		SetActiveCircuit(ObjName);
	}
	else


        // Everything else must be a circuit element
	{
		if(ObjClass.size() > 0)
			SetObjectClass(ObjClass);
		ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
		if(ActiveDSSClass[ActiveActor] != nullptr)
		{
			if(!ActiveDSSClass[ActiveActor]->SetActive(ObjName)) // scroll through list of objects untill a match
			{
				DoSimpleMsg(String("Error! Object \"") + ObjName
	           + "\" not found."
	           + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 245);
				result = 0;
			}
			else
			/*# with ActiveCircuit[ActiveActor] do */
			{
				
				switch( ( (TDSSObject*) ActiveDSSObject[ActiveActor] )->DSSObjType )
				{
					case 	DSS_OBJECT:
					;
					break;  // do nothing for general DSS object
					default:
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(((TDSSCktElement*) ActiveDSSClass[ActiveActor]->GetActiveObj()));
                   // Now check for active terminal designation
					ParamName = LowerCase(Parser[ActiveActor]->GetNextParam());
					Param = Parser[ActiveActor]->MakeString_();
					if(Param.size() > 0)
						ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_ActiveTerminal(Parser[ActiveActor]->MakeInteger_());
					else
						ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_ActiveTerminal(1);  /*default to 1*/
					/*# with ActiveCktElement do */
					{
						auto with1 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
						SetActiveBus(StripExtension(with1->GetBus(with1->get_FActiveTerminal())));
					}
					break;
				}
			}
		}
		else
		{
			DoSimpleMsg("Error! Active object type/class is not set.", 246);
			result = 0;
		}
	}
	return result;
}

// select active object
// select element=elementname terminal=terminalnumber


//----------------------------------------------------------------------------

int DoMoreCmd()
{
	int result = 0;
	if(ActiveDSSClass[ActiveActor] != nullptr)
		result = ActiveDSSClass[ActiveActor]->Edit(ActiveActor);
	else
		result = 0;
	return result;
}

// more editstring  (assumes active circuit element)



//----------------------------------------------------------------------------

int DoSaveCmd()
{
	int result = 0;
	TMonitorObj* pMon = nullptr;
	TEnergyMeterObj* pMtr = nullptr;
	int i = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	String ObjClass;
	String SaveDir;
	String saveFile;
	TDSSClass* DSSClass = nullptr;
	result = 0;
	ObjClass = "";
	SaveDir = "";
	saveFile = "";
	ParamPointer = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while (Param.size() > 0)
	{
		if (ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = SaveCommands->Getcommand(ParamName);
		switch (ParamPointer)
		{
		case 	1:
			ObjClass = Parser[ActiveActor]->MakeString_();
			break;
		case 	2:
			saveFile = Parser[ActiveActor]->MakeString_();
			break;   // File name for saving  a class
		case 	3:
			SaveDir = Parser[ActiveActor]->MakeString_();
			break;
		default:
			;
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
	InShowResults = true;
	if ((ObjClass.size() == 0) || (CompareTextShortest(ObjClass, "meters") == 0))
		// Save monitors and Meters
	{
		/*# with ActiveCircuit[ActiveActor].Monitors do */
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Monitors;
			int stop = 0;
			for (stop = with0.get_myNumList(), i = 1; i <= stop; i++)
			{
				pMon = (TMonitorObj*)with0.Get(i);
				pMon->Save();
			}
		}
		/*# with ActiveCircuit[ActiveActor].EnergyMeters do */
		{
			auto& with1 = ActiveCircuit[ActiveActor]->EnergyMeters;
			int stop = 0;
			for (stop = with1.get_myNumList(), i = 1; i <= stop; i++)
			{
				pMtr = (TEnergyMeterObj*)with1.Get(i);
				pMtr->SaveRegisters(ActiveActor);
			}
		}
		return result;
	}
	if (CompareTextShortest(ObjClass, "circuit") == 0)
	{
		if (!ActiveCircuit[ActiveActor]->Save(SaveDir))
			result = 1;
		return result;
	}
	if (CompareTextShortest(ObjClass, "voltages") == 0)
	{
		ActiveCircuit[ActiveActor]->Solution->SaveVoltages();
		return result;
	}
	else
		return 0;

	/*Assume that we have a class name for a DSS Class*/
	DSSClass = (TDSSClass*)GetDSSClassPtr(ObjClass);
	if (DSSClass != nullptr)
	{
		if (saveFile.size() == 0)
			saveFile = ObjClass;
		if (SaveDir.size() > 0)
		{
			if (!DirectoryExists(SaveDir))
				try
			{
				MkDir(SaveDir);
			}
			catch (std::exception &e)
			{
				DoSimpleMsg(String("Error making Directory: \"") + SaveDir + "\". " + (std::string)e.what(), 247);
			}
			saveFile = SaveDir + DIRSEP_STR + saveFile;
		}
		WriteClassFile((*DSSClass), saveFile, false); // just write the class with no checks
	}
	SetLastResultFile(saveFile);
	GlobalResult = saveFile;
	return result;
}

// Save current values in both monitors and Meters



//----------------------------------------------------------------------------

int DoClearCmd()
{
	int result = 0;
	DSSExecutive[ActiveActor]->Clear();
	result = 0;
	return result;
}
//----------------------------------------------------------------------------

int DoClearAllCmd()
{
	int result = 0;
	DSSExecutive[ActiveActor]->ClearAll();
	result = 0;
	return result;
}
//----------------------------------------------------------------------------

int DoHelpCmd()
{
	int result = 0;
//	ShowHelpForm(); // DSSForms Unit
	result = 0;
	return result;
}


//----------------------------------------------------------------------------

int DoSampleCmd(int ActorID)
{
	int result = 0;
	MonitorClass[ActorID]->SampleAll(ActorID);
	EnergyMeterClass[ActorID]->SampleAll(ActorID);  // gets generators too
	result = 0;
	return result;
}

// FORce all monitors and meters in active circuit to take a sample



//----------------------------------------------------------------------------

int DoSolveCmd()
{
	int result = 0;
   // just invoke solution obj's editor to pick up parsing and execute rest of command
	ActiveSolutionObj = ActiveCircuit[ActiveActor]->Solution;
	result = SolutionClass[ActiveActor]->Edit(ActiveActor);
	return result;
}


//----------------------------------------------------------------------------

int SetActiveCktElement()
{
	int result = 0;
	String ObjType;
	String ObjName;
	result = 0;
	GetObjClassAndName(ObjType, ObjName);
	if(CompareText(ObjType, "circuit") == 0)
		;
	else

                 // Do nothing
	{
		if(CompareText(ObjType, ActiveDSSClass[ActiveActor]->get_myClass_name()) != 0)
			LastClassReferenced[ActiveActor] = ClassNames[ActiveActor].Find(ObjType);
		switch(LastClassReferenced[ActiveActor])
		{
			case 	0:
			{
				DoSimpleMsg(String("Object Type \"") + ObjType
	           + "\" not found."
	           + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 253);
				result = 0;
				return result;
			}/*Error*/

        // intrinsic and user Defined models
			default:
			ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
			if(ActiveDSSClass[ActiveActor]->SetActive(ObjName))
				/*# with ActiveCircuit[ActiveActor] do */
				{
					 // scroll through list of objects until a match
					switch( ( (TDSSObject*) ActiveDSSObject[ActiveActor] )->DSSObjType)
					{
						case 	DSS_OBJECT:
						DoSimpleMsg(String("Error in SetActiveCktElement: Object not a circuit Element.") + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 254);
						break;
						default:
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(((TDSSCktElement*) ActiveDSSClass[ActiveActor]->GetActiveObj()));
						result = 1;
						break;
					}
				}
			break;
		}
	}
	return result;
}

// Parses the object off the line and sets it active as a circuitelement.



//----------------------------------------------------------------------------

int DoEnableCmd()
{
	int result = 0;
	String ObjType;
	String ObjName;
	TDSSClass* ClassPtr = nullptr;
	TDSSCktElement* CktElem = nullptr;
	int i = 0;

  //   Result := SetActiveCktElement;
  //  IF Result>0 THEN ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := True;
	result = 0;
	GetObjClassAndName(ObjType, ObjName);
	if(CompareText(ObjType, "circuit") == 0)
		;
	else
	{
		if(ObjType.size() > 0)
                 // Do nothing

      // only applies to CktElementClass objects
		{
			ClassPtr = (TDSSClass*) GetDSSClassPtr(ObjType);
			if(ClassPtr != nullptr)
			{
				if((ClassPtr->DSSClassType & BaseClassMask) > 0)
              // Everything else must be a circuit element
				{
					if(CompareText(ObjName, "*") == 0)
               // Enable all elements of this class
					{
						int stop = 0;
						for(stop = ClassPtr->Get_ElementCount(), i = 1; i <= stop; i++)
						{
							CktElem = (TDSSCktElement*) ClassPtr->ElementList.Get(i);
							CktElem->Set_Enabled(true);
						}
					}
					else


              // just load up the parser and call the edit routine for the object in question
					{
						Parser[ActiveActor]->SetCmdString("Enabled=true");  // Will only work for CktElements
						result = EditObject(ObjType, ObjName);
					}
				}
			}
		}
	}
	return result;
}

//----------------------------------------------------------------------------

int DoDisableCmd()
{
	int result = 0;
	String ObjType;
	String ObjName;
	TDSSClass* ClassPtr = nullptr;
	TDSSCktElement* CktElem = nullptr;
	int i = 0;
	result = 0;
	GetObjClassAndName(ObjType, ObjName);
	if(CompareText(ObjType, "circuit") == 0)
		;
	else
	{
		if(ObjType.size() > 0)
                 // Do nothing

      // only applies to CktElementClass objects
		{
			ClassPtr = (TDSSClass*) GetDSSClassPtr(ObjType);
			if(ClassPtr != nullptr)
			{
				if((ClassPtr->DSSClassType & BaseClassMask) > 0)
              // Everything else must be a circuit element
				{
					if(CompareText(ObjName, "*") == 0)
               // Disable all elements of this class
					{
						int stop = 0;
						for(stop = ClassPtr->Get_ElementCount(), i = 1; i <= stop; i++)
						{
							CktElem = (TDSSCktElement*) ClassPtr->ElementList.Get(i);
							CktElem->Set_Enabled(false);
						}
					}
					else


              // just load up the parser and call the edit routine for the object in question
					{
						Parser[ActiveActor]->SetCmdString("Enabled=false");  // Will only work for CktElements
						result = EditObject(ObjType, ObjName);
					}
				}
			}
		}

//     Result := SetActiveCktElement;
//     IF Result>0 THEN ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := False;
	}
	return result;
}

//----------------------------------------------------------------------------

int DoPropertyDump()
{
	int result = 0;
	TDSSObject* PObject = nullptr;
	TTextRec f = {};
	bool SingleObject = false;
	bool DebugDump = false;
	bool IsSolution = false;
	int i = 0;
	String FileName;
	String ParamName;
	String Param;
	String Param2;
	String ObjClass;
	String ObjName;
	result = 0;
	SingleObject = false;
	IsSolution = false;
	DebugDump = false;
	ObjClass = " ";  // make sure these have at least one character
	ObjName = " ";
 
 // Continue parsing command line - check for object name
 	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	if(Param.size() > 0)
	{
		if(CompareText(Param, "commands") == 0)
		{
			if(!NoFormsAllowed)
			{
				DumpAllDSSCommands(FileName);
				FireOffEditor(FileName);
				return result;
			}

    /*dump bus names hash list*/
		}
		if(CompareText(Param, "buslist") == 0)
		{
			if(!NoFormsAllowed)
			{
				FileName = GetOutputDirectory() + "Bus_Hash_List.Txt";
				ActiveCircuit[ActiveActor]->BusList.DumpToFile(FileName);
				FireOffEditor(FileName);
				return result;
			}

    /*dump device names hash list*/
		}
		if(CompareText(Param, "devicelist") == 0)
		{
			if(!NoFormsAllowed)
			{
				FileName = GetOutputDirectory() + "Device_Hash_List.Txt";
				ActiveCircuit[ActiveActor]->DeviceList.DumpToFile(FileName);
				FireOffEditor(FileName);
				return result;
			}
		}
		if(CompareText(LowerCase(Param).substr(0, 5), "alloc") == 0)
		{
			FileName = GetOutputDirectory() + "AllocationFactors.Txt";
			DumpAllocationFactors(FileName);
			FireOffEditor(FileName);
			return result;
		}
		if(CompareText(Param, "debug") == 0)
			DebugDump = true;
		else
		{
			if(CompareText(Param, "solution") == 0)
          // Assume active circuit solution IF not qualified
			{
				ActiveDSSClass[ActiveActor] = SolutionClass[ActiveActor];
				ActiveDSSObject[ActiveActor] = ActiveCircuit[ActiveActor]->Solution;
				IsSolution = true;
			}
			else
			{
				SingleObject = true;
           // Check to see IF we want a debugdump on this object
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param2 = Parser[ActiveActor]->MakeString_();
				if(CompareText(Param2, "debug") == 0)
					DebugDump = true;
            // Set active Element to be value in Param
				Parser[ActiveActor]->SetCmdString(String("\"") + Param + "\"");  // put param back into parser
				GetObjClassAndName(ObjClass, ObjName);
            // IF DoSelectCmd=0 THEN Exit;  8-17-00
				if(SetObjectClass(ObjClass))
				{
					ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
					if(ActiveDSSClass[ActiveActor] == nullptr)
						return result;
				}
				else
				return result;
			}
		}
	}
	try
	{
		AssignFile(f, GetOutputDirectory() + CircuitName_[ActiveActor] + "PropertyDump.Txt");
		Rewrite(f);
		IOResultToException();
	}
	catch (std::exception &e)
	{
		{
			DoErrorMsg(String("DoPropertyDump - opening ") + GetOutputDirectory()
	           + " DSS_PropertyDump.txt for writing in "
	           + GetCurrentDir(), (std::string) e.what(), "Disk protected or other file error", 255);
			return result;
		}
	}
	try
	{
		if(SingleObject)

        /*IF ObjName='*' then we dump all objects of this class*/
		{
			switch(ObjName[0])
			{
				case 	L'*':
				{
					int stop = 0;
					for(stop = ActiveDSSClass[ActiveActor]->Get_ElementCount(), i = 1; i <= stop; i++)
					{
						ActiveDSSClass[ActiveActor]->Set_Active(i);
						( (TDSSCktElement*) ActiveDSSObject[ActiveActor])->DumpProperties(f, DebugDump);
					}
				}
				break;
				default:
				if(!ActiveDSSClass[ActiveActor]->SetActive(ObjName))
				{
					DoSimpleMsg(String("Error! Object \"") + ObjName + "\" not found.", 256);
					return result;
				}
				else
				( (TDSSCktElement*) ActiveDSSObject[ActiveActor] )->DumpProperties(f, DebugDump);  // Dump only properties of active circuit element
				break;
			}
		}
		else
		{
			if(IsSolution)
			{
				((TDSSCktElement*)ActiveDSSObject[ActiveActor])->DumpProperties(f, DebugDump);
			}
			else


        // Dump general Circuit stuff
			{
				if(DebugDump)
					ActiveCircuit[ActiveActor]->DebugDump(f);
        // Dump circuit objects
				try
				{
					PObject = (TDSSObject*) ActiveCircuit[ActiveActor]->CktElements.Get_First();
					while(PObject != nullptr)
					{
						PObject->DumpProperties(f, DebugDump);
						PObject = (TDSSObject*) ActiveCircuit[ActiveActor]->CktElements.Get_Next();
					}
					PObject = (TDSSObject*) DSSObjs[ActiveActor].Get_First();
					while(PObject != nullptr)
					{
						PObject->DumpProperties(f, DebugDump);
						PObject = (TDSSObject*) DSSObjs[ActiveActor].Get_Next();
					}
				}
				catch (std::exception &e)
				{
					DoErrorMsg("DoPropertyDump - Problem writing file.", (std::string) e.what(), "File may be read only, in use, or disk full?", 257);
				}
				ActiveCircuit[ActiveActor]->Solution->DumpProperties(f, DebugDump);
			}
		}
/* }
	__finally
	{*/
		CloseFile(f);
	}  /*TRY*/
	catch (...)
	{
		//
	}
	FireOffEditor(GetOutputDirectory() + CircuitName_[ActiveActor] + "PropertyDump.Txt");
	return result;
}


//----------------------------------------------------------------------------

void Set_Time()
{
	double TimeArray[2/*# range 1..2*/]{};
	Parser[ActiveActor]->ParseAsVector(2, (pDoubleArray) TimeArray);
	/*# with ActiveCircuit[ActiveActor].Solution do */
	{
		auto with0 = ActiveCircuit[ActiveActor]->Solution;
		with0->DynaVars.intHour = (int) Round(TimeArray[1 - 1]);
		with0->DynaVars.T = TimeArray[2 - 1];
		with0->Update_dblHour();
	}
}

// for interpreting time specified as an array "hour, sec"


//----------------------------------------------------------------------------

void SetActiveCircuit(const String cktname)
{
	TDSSCircuit* pCkt = nullptr;
	pCkt = (TDSSCircuit*) Circuits.Get_First();
	while(pCkt != nullptr)
	{
		if(CompareText(pCkt->Get_Name(), cktname) == 0)
		{
			ActiveCircuit[ActiveActor] = pCkt;
			return;
		}
		pCkt = (TDSSCircuit*) Circuits.Get_Next();
	}

   // IF none is found, just leave as is after giving error
	DoSimpleMsg(String("Error! No circuit named \"") + cktname
	           + "\" found."
	           + CRLF
	           + "Active circuit not changed.", 258);
}

/*-------------------------------------------*/

void DoLegalVoltageBases()
{
	pDoubleArray Dummy;
	int i = 0;
	int Num = 0;
	Dummy = (pDoubleArray)malloc(sizeof(double) * 200); // Big Buffer
	Num = Parser[ActiveActor]->ParseAsVector(200, Dummy);
	/*Parsing zero-fills the array*/

	/*LegalVoltageBases is a zero-terminated array, so we have to allocate
	 one more than the number of actual values*/
	 /*# with ActiveCircuit[ActiveActor] do */
	{

		int stop = 0;
		ActiveCircuit[ActiveActor]->LegalVoltageBases.resize(static_cast<std::vector<double, std::allocator<double>>::size_type>(Num) + 1);
		for (stop = Num + 1, i = 1; i <= stop; i++)
		{
			ActiveCircuit[ActiveActor]->LegalVoltageBases[static_cast<std::vector<double, std::allocator<double>>::size_type>(i) - 1] = Dummy[i - 1];
		}
	}
	free(Dummy);
}



//----------------------------------------------------------------------------

int DoOpenCmd()
{
	int result = 0;
	int RetVal = 0;
	int Terminal = 0;
	int Conductor = 0;
	String ParamName;

// syntax:  "Open class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened.
	RetVal = SetActiveCktElement();
	if(RetVal > 0)
	{
		ParamName = Parser[ActiveActor]->GetNextParam();
		Terminal = Parser[ActiveActor]->MakeInteger_();
		ParamName = Parser[ActiveActor]->GetNextParam();
		Conductor = Parser[ActiveActor]->MakeInteger_();
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_ActiveTerminal(Terminal);
			ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_ConductorClosed(Conductor, ActiveActor, false);
			/*# with ActiveCktElement do */
			{
				auto with1 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
				SetActiveBus(StripExtension(with1->GetBus(with1->get_FActiveTerminal())));
			}
		}
	}
	else
	{
		DoSimpleMsg(String("Error in Open Command: Circuit Element Not Found.") + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 259);
	}
	result = 0;
	return result;
}
// Opens a terminal and conductor of a ckt Element




//----------------------------------------------------------------------------

int DoCloseCmd()
{
	int result = 0;
	int RetVal = 0;
	int Terminal = 0;
	int Conductor = 0;
	String ParamName;

// syntax:  "Close class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened
	RetVal = SetActiveCktElement();
	if(RetVal > 0)
	{
		ParamName = Parser[ActiveActor]->GetNextParam();
		Terminal = Parser[ActiveActor]->MakeInteger_();
		ParamName = Parser[ActiveActor]->GetNextParam();
		Conductor = Parser[ActiveActor]->MakeInteger_();
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_ActiveTerminal(Terminal);
			ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_ConductorClosed(Conductor, ActiveActor, true);
			/*# with ActiveCktElement do */
			{
				auto with1 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
				SetActiveBus(StripExtension(with1->GetBus(with1->get_FActiveTerminal())));
			}
		}
	}
	else
	{
		DoSimpleMsg(String("Error in Close Command: Circuit Element Not Found.") + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 260);
	}
	result = 0;
	return result;
}
// Closes a terminal and conductor of a ckt Element


//----------------------------------------------------------------------------

int DoResetCmd(int ActorID)
{
	int result = 0;
	String ParamName;
	String Param;
	result = 0;

	// Obtener el siguiente parámetro e intentar interpretarlo como un nombre de archivo
	ParamName = Parser[ActorID]->GetNextParam();
	Param = UpperCase(Parser[ActorID]->MakeString_());
	if (Param.size() == 0)
	{
		DoResetMonitors(ActorID);
		DoResetMeters(ActorID);
		DoResetFaults();
		DoResetControls();
		ClearEventLog();
		ClearErrorLog();
		DoResetKeepList();
	}
	else
	{
		switch (Param[0])
		{
		case L'M':
			switch (Param[1])
			{
			case L'O': /* Monitor */
				DoResetMonitors(ActorID);
				break;
			case L'E': /* Meter */
				DoResetMeters(ActorID);
				break;
			default:
				// Código de caso desconocido
				break;
			}
			break;
		case L'F': /* Faults */
			DoResetFaults();
			break;
		case L'C': /* Controls */
			DoResetControls();
			break;
		case L'E': /* EventLog and ErrorLog */
			ClearEventLog();
			ClearErrorLog();
			break;
		case L'K':
			DoResetKeepList();
			break;
		default:
			DoSimpleMsg(String("Unknown argument to Reset Command: \"") + Param + "\"", 261);
			break;
		}
	}

	return result;
}


void MarkCapandReactorBuses()
{
	TDSSClass* pClass = nullptr;
	TCapacitorObj* pCapElement = nullptr;
	TReactorObj* pReacElement = nullptr;
	int ObjRef = 0;
/*Mark all buses as keepers if there are capacitors or reactors on them*/
	pClass = (TDSSClass*) GetDSSClassPtr("capacitor");
	if(pClass != nullptr)
	{
		ObjRef = pClass->Get_First();
		while(ObjRef > 0)
		{
			pCapElement = ((TCapacitorObj*) ActiveDSSObject[ActiveActor]);
			if(pCapElement->IsShunt)
			{
				if(pCapElement->Get_Enabled())
					ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(pCapElement->Terminals[0].BusRef) - 1]->Keep = true;
			}
			ObjRef = pClass->Get_Next();
		}
	}

    /*Now Get the Reactors*/
	pClass = (TDSSClass*)GetDSSClassPtr("reactor");
	if(pClass != nullptr)
	{
		ObjRef = pClass->Get_First();
		while(ObjRef > 0)
		{
			pReacElement = ((TReactorObj*) ActiveDSSObject[ActiveActor]);
			if(pReacElement->IsShunt)
				try
				{
					if(pReacElement->Get_Enabled())
						ActiveCircuit[ActiveActor]->Buses[static_cast<std::vector<Bus::TDSSBus*, std::allocator<Bus::TDSSBus*>>::size_type>(pReacElement->Terminals[0].BusRef) - 1]->Keep = true;
				}
				catch (std::exception &e)
				{
					{
						DoSimpleMsg(Format("%s %s Reactor=%s Bus No.=%d ", 
							e.what(), CRLF.c_str(), pReacElement->get_Name().c_str(), (int)(pReacElement->NodeRef)[0]), 9999);
						break;
					}
				}
			ObjRef = pClass->Get_Next();
		}
	}
}

//----------------------------------------------------------------------------

int DoReduceCmd()
{
	int					result = 0;
	TEnergyMeterObj*	MetObj = nullptr;
	TEnergyMeter*		MeterClass = nullptr;
	String				ParamName = "",
						Param = "";
	int					DevClassIndex = 0;
	
    // Get next parm and try to interpret as a file name
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = UpperCase(Parser[ActiveActor]->MakeString_());

    /*Mark Capacitor and Reactor buses as Keep so we don't lose them*/
	MarkCapandReactorBuses();
	if(Param.size() == 0)
		Param = "A";
	switch(Param[0])
	{
		case 	L'A':
		{
			MetObj = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
			while(MetObj != nullptr)
			{
				MetObj->ReduceZone(ActiveActor);
				MetObj = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_Next();
			}
		}
		break;
       /*Reduce a specific meter*/
		default:
		DevClassIndex = ClassNames[ActiveActor].Find("energymeter");
		if(DevClassIndex > 0)
		{
			MeterClass = (TEnergyMeter*) DSSClassList[ActiveActor].Get(DevClassIndex);
			if( ( (TDSSClass*) MeterClass )->SetActive(Param))   // Try to set it active
			{
				MetObj = ((TEnergyMeterObj*) ((TDSSClass*)MeterClass)->GetActiveObj());
				MetObj->ReduceZone(ActiveActor);
			}
			else
			DoSimpleMsg(String("EnergyMeter \"") + Param + "\" not found.", 262);
		}
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

int DoResetMonitors(int ActorID)
{
	int result = 0;
	TMonitorObj* pMon = nullptr;
	/*# with ActiveCircuit[ActorID] do */
	{
		
		pMon = (TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_First();
		while(pMon != nullptr)
		{
			pMon->ResetIt(ActorID);
			pMon = (TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_Next();
		}
		result = 0;
	}
	return result;
}

//----------------------------------------------------------------------------

int DoFileEditCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	result = 0;

    // Get next parm and try to interpret as a file name
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	if(FileExists(Param))
		FireOffEditor(Param);
	else
	{
		GlobalResult = String("File \"") + Param + "\" does not exist.";
		result = 1;
	}
	return result;
}

//----------------------------------------------------------------------------

void ParseObjName(const String FullName, String& ObjName, String& PropName)
{
	int DotPos1 = 0;
	int DotPos2 = 0;
	DotPos1 = Pos(".", FullName);
	switch(DotPos1)
	{
		case 	0:
		{
			ObjName = "";
			PropName = FullName;
		}
		break;
		default:
		PropName = FullName.substr(DotPos1, (FullName.size() - DotPos1));
		DotPos2 = Pos(".", PropName);
		switch(DotPos2)
		{
			case 	0:
			{
				ObjName = FullName.substr(0, static_cast<size_t>(DotPos1) - 1);
			}
			break;
			default:
			ObjName = FullName.substr(0, static_cast<size_t>(DotPos1) + DotPos2 - 1);
			PropName = PropName.substr(DotPos2, (PropName.size() - DotPos2));
			break;
		}
		break;
	}
}

/* Parse strings such as

    1. Classname.Objectname,Property    (full name)
    2. Objectname.Property   (classname omitted)
    3. Property           (classname and objectname omitted
*/

int DoQueryCmd()
{
	int result			= 0,
		propIndex		= 0;
	String	ParamName	= "",
			Param		= "",
			ObjName		= "",
			PropName	= "";
	

	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	ParseObjName(Param, ObjName, PropName);
	if(CompareText(ObjName, "solution") == 0)  // special for solution
	{
		ActiveDSSClass[ActiveActor] = SolutionClass[ActiveActor];
		ActiveDSSObject[ActiveActor] = ActiveCircuit[ActiveActor]->Solution;
	}
	else

         // Set Object Active
	{
		Parser[ActiveActor]->SetCmdString(String("\"") + ObjName + "\"");
		DoSelectCmd();
	}

     // Put property value in global VARiable
	propIndex = ActiveDSSClass[ActiveActor]->PropertyIndex(PropName);
	if(propIndex > 0)
		GlobalPropertyValue = ( (TDSSObject*) ActiveDSSObject[ActiveActor] )->GetPropertyValue(propIndex);
	else
		GlobalPropertyValue = "Property Unknown";
	GlobalResult = GlobalPropertyValue;
	if(LogQueries)
		WriteQueryLogFile(Param, GlobalResult); // write time-stamped query
	return result;
}
/* ? Command */
/* Syntax:  ? Line.Line1.R1*/

//----------------------------------------------------------------------------

int DoResetMeters(int ActorID)
{
	int result = 0;
	result = 0;
	EnergyMeterClass[ActorID]->ResetAll(ActorID);
	return result;
}


//----------------------------------------------------------------------------

int DoNextCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	result = 0;

    // Get next parm and try to interpret as a file name
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	/*# with ActiveCircuit[ActiveActor].Solution do */
	{
		auto with0 = ActiveCircuit[ActiveActor]->Solution;
		switch(UpCase(Param[1]))
		{
			case 	L'Y':/*Year*/
			with0->Set_Year(with0->get_Fyear() + 1);
			break;/*Hour*/
			case 	L'H':
			++with0->DynaVars.intHour;
			break;/*Time*/
			case 	L'T':
			with0->Increment_time();
			break;
			default:
			  ;
			break;
		}
	}
	return result;
}

//----------------------------------------------------------------------------

void DoAboutBox()
{
	if(NoFormsAllowed)
		return;
	ShowAboutBox();
}

//----------------------------------------------------------------------------

int DoSetVoltageBases(int ActorID)
{
	int result = 0;
	result = 0;
	ActiveCircuit[ActiveActor]->Solution->SetVoltageBases(ActorID);
	return result;
}



//----------------------------------------------------------------------------

int AddObject(const String ObjType, const String Name)
{
	int result = 0;
	result = 0;

   // Search for class IF not already active
   // IF nothing specified, LastClassReferenced remains
	if(CompareText(ObjType, ActiveDSSClass[ActiveActor]->get_myClass_name()) != 0)
		LastClassReferenced[ActiveActor] = ClassNames[ActiveActor].Find(ObjType);
	switch(LastClassReferenced[ActiveActor])
	{
		case 	0:
		{
			DoSimpleMsg(String("New Command: Object Type \"") + ObjType
	           + "\" not found."
	           + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 263);
			result = 0;
			return result;
		}/*Error*/

     // intrinsic and user Defined models
     // Make a new circuit element
		default:
		ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);

      // Name must be supplied
		if(Name.size() == 0)
		{
			DoSimpleMsg(String("Object Name Missing") + CRLF + Parser[ActiveActor]->get_CmdBuffer(), 264);
			return result;
		}


   // now let's make a new object or set an existing one active, whatever the case
		switch(ActiveDSSClass[ActiveActor]->DSSClassType)
		{
			case 	DSS_OBJECT:
			if(!ActiveDSSClass[ActiveActor]->SetActive(Name))
            // These can be added WITHout having an active circuit
            // Duplicates not allowed in general DSS objects;
            // If the name is the same, Edit is executed instead of New
			{
				result = ActiveDSSClass[ActiveActor]->NewObject(Name); 
				DSSObjs[ActiveActor].Add(ActiveDSSObject[ActiveActor]);  // Stick in pointer list to keep track of it
			}
			break;
            // These are circuit elements
			default:
			if(ActiveActor == 0)
			{
				DoSimpleMsg("You Must Create a circuit first: \"new circuit.yourcktname\"", 265);
				return result;
			}

          // IF Object already exists.  Treat as an Edit IF dulicates not allowed
			if(ActiveCircuit[ActiveActor]->DuplicatesAllowed)
			{
				result = ActiveDSSClass[ActiveActor]->NewObject(Name); // Returns index into this class
				ActiveCircuit[ActiveActor]->AddCktElement(result);   // Adds active object to active circuit
			}
			else
      // Check to see if we can set it active first
			{
				if(!ActiveDSSClass[ActiveActor]->SetActive(Name))
				{
					result = ActiveDSSClass[ActiveActor]->NewObject(Name);   // Returns index into this class
					ActiveCircuit[ActiveActor]->AddCktElement(result);   // Adds active object to active circuit
				}
				else
				{
					DoSimpleMsg(String("Warning: Duplicate new element definition: \"") + ActiveDSSClass[ActiveActor]->get_myClass_name()
	           + "."
	           + Name
	           + "\""
	           + CRLF
	           + "Element being redefined.", 266);
				}
			}
			break;
		}

        // ActiveDSSObject now points to the object just added
        // IF a circuit element, ActiveCktElement in ActiveCircuit[ActiveActor] is also set
		if(result > 0)
			( (TDSSObject*) ActiveDSSObject[ActiveActor] )->ClassIndex = result;
		ActiveDSSClass[ActiveActor]->Edit(ActiveActor); // Process remaining instructions on the command line
		break;
	}
	return result;
}


//----------------------------------------------------------------------------

int EditObject(const String ObjType, const String Name)
{
	int result = 0;
	result = 0;
	LastClassReferenced[ActiveActor] = ClassNames[ActiveActor].Find(ObjType);
	switch(LastClassReferenced[ActiveActor])
	{
		case 	0:
		{
			DoSimpleMsg(String("Edit Command: Object Type \"") + ObjType
	           + "\" not found."
	           + CRLF
	           + Parser[ActiveActor]->get_CmdBuffer(), 267);
			result = 0;
			return result;
		}/*Error*/

   // intrinsic and user Defined models
   // Edit the DSS object
		default:
		ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
		if(ActiveDSSClass[ActiveActor]->SetActive(Name))
		{
			result = ActiveDSSClass[ActiveActor]->Edit(ActiveActor);   // Edit the active object
		}
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

int DoSetkVBase()
{
	int result = 0;
	String ParamName;
	String Busname;
	double kVValue = 0.0;

// Parse off next two items on line
	ParamName = Parser[ActiveActor]->GetNextParam();
	Busname = LowerCase(Parser[ActiveActor]->MakeString_());
	ParamName = Parser[ActiveActor]->GetNextParam();
	kVValue = Parser[ActiveActor]->MakeDouble_();

   // Now find the bus and set the value
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->ActiveBusIndex = ActiveCircuit[ActiveActor]->BusList.Find(Busname);
		if(ActiveCircuit[ActiveActor]->ActiveBusIndex > 0)
		{
			if(CompareText(ParamName, "kvln") == 0)
				ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(ActiveCircuit[ActiveActor]->ActiveBusIndex) - 1]->kVBase = kVValue;
			else
				ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(ActiveCircuit[ActiveActor]->ActiveBusIndex) - 1]->kVBase = kVValue / SQRT3;
			result = 0;
			ActiveCircuit[ActiveActor]->Solution->VoltageBaseChanged = true;
           // Solution.SolutionInitialized := FALSE;  // Force reinitialization
		}
		else
		{
			result = 1;
			AppendGlobalResult(String("Bus ") + Busname + " Not Found.");
		}
	}
	return result;
}



//----------------------------------------------------------------------------

void DoAutoAddBusList(const String s)
{
	String dummy;
	String Parmname;
	String Param;
	String S2;
	TTextRec f = {};
	ActiveCircuit[ActiveActor]->AutoAddBusList.Clear();

     // Load up auxiliary parser to reparse the array list or file name
	AuxParser[ActiveActor]->SetCmdString(s);
	Parmname = AuxParser[ActiveActor]->GetNextParam();
	Param = AuxParser[ActiveActor]->MakeString_();

     /*Syntax can be either a list of bus names or a file specification:  File= ...*/
	if(CompareText(Parmname, "file") == 0)
         // load the list from a file
	{
		try
		{
			AssignFile(f, Param);
			Reset(f);
			IOResultToException();
			while(!Eof(f))
         // Fixed 7/8/01 to handle all sorts of bus names
			{
				ReadLn(f, S2);
				AuxParser[ActiveActor]->SetCmdString(S2);
				Parmname = AuxParser[ActiveActor]->GetNextParam();
				Param = AuxParser[ActiveActor]->MakeString_();
				if(Param.size() > 0)
					ActiveCircuit[ActiveActor]->AutoAddBusList.Add(Param);
			}
			CloseFile(f);
		}
		catch (std::exception &e)
		{
			DoSimpleMsg(String("Error trying to read bus list file. Error is: ") + (std::string) e.what(), 268);
		}
	}
	else


       // Parse bus names off of array list
	{
		while(Param.size() > 0)
		{
			ActiveCircuit[ActiveActor]->AutoAddBusList.Add(Param);
			dummy = AuxParser[ActiveActor]->GetNextParam();
			Param = AuxParser[ActiveActor]->MakeString_();
		}
	}
}

//----------------------------------------------------------------------------

void DoKeeperBusList(const String s)
{
	String Parmname;
	String Param;
	String S2;
	TTextRec f = {};
	int iBus = 0;

     // Load up auxiliary parser to reparse the array list or file name
	AuxParser[ActiveActor]->SetCmdString(s);
	Parmname = AuxParser[ActiveActor]->GetNextParam();
	Param = AuxParser[ActiveActor]->MakeString_();

     /*Syntax can be either a list of bus names or a file specification:  File= ...*/
	if(CompareText(Parmname, "file") == 0)
         // load the list from a file
	{
		try
		{
			AssignFile(f, Param);
			Reset(f);
			IOResultToException();
			while(!Eof(f))
         // Fixed 7/8/01 to handle all sorts of bus names
			{
				ReadLn(f, S2);
				AuxParser[ActiveActor]->SetCmdString(S2);
				Parmname = AuxParser[ActiveActor]->GetNextParam();
				Param = AuxParser[ActiveActor]->MakeString_();
				if(Param.size() > 0)
					/*# with ActiveCircuit[ActiveActor] do */
					{
						
						iBus = ActiveCircuit[ActiveActor]->BusList.Find(Param);
						if(iBus > 0)
							ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iBus) - 1]->Keep = true;
					}
			}
			CloseFile(f);
		}
		catch (std::exception &e)
		{
			DoSimpleMsg(String("Error trying to read bus list file \"+param+\". Error is: ") + (std::string) e.what(), 269);
		}
	}
	else


       // Parse bus names off of array list
	{
		while(Param.size() > 0)
		{
			/*# with ActiveCircuit[ActiveActor] do */
			{
				
				iBus = ActiveCircuit[ActiveActor]->BusList.Find(Param);
				if(iBus > 0)
					ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iBus) - 1]->Keep = true;
			}
			String dummy = AuxParser[ActiveActor]->GetNextParam();
			Param = AuxParser[ActiveActor]->MakeString_();
		}
	}
}


// Created 4/25/03

/*Set Keep flag on buses found in list so they aren't eliminated by some reduction
 algorithm.  This command is cumulative. To clear flag, use Reset Keeplist*/

//----------------------------------------------------------------------------

int DocktlossesCmd()
{
	int result = 0;
	complex LossValue = {};
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
	{
		GlobalResult = "";
		LossValue = ActiveCircuit[ActiveActor]->Get_Losses(ActiveActor);
		GlobalResult = Format("%10.5g, %10.5g", LossValue.re * 0.001, LossValue.im * 0.001);
	}
	else
	GlobalResult = "No Active Circuit.";
	return result;
}

int DocurrentsCmd()
{
	int result = 0;
	pComplexArray cBuffer = nullptr; // Inicializar a nullptr para evitar basura en el puntero
	int NValues = 0;
	int i = 0;
	result = 0;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
		int stop = 0;
		NValues = with0->Get_NConds() * with0->Get_NTerms();
		GlobalResult = "";
		cBuffer = (pComplexArray)malloc(sizeof(complex) * NValues);
		if (cBuffer != nullptr) // Verificar si malloc pudo asignar memoria
		{
			with0->GetCurrents(cBuffer, ActiveActor);
			for (stop = NValues, i = 1; i <= stop; i++)
			{
				GlobalResult = GlobalResult + Format("%10.5g, %6.1f,", cabs(cBuffer[i - 1]), cdang(cBuffer[i - 1]));
			}
			free(cBuffer); // Liberar la memoria asignada por malloc
		}
		else
		{
			GlobalResult = "Memory allocation failed."; // Si malloc no pudo asignar memoria
		}
	}
	else
	{
		GlobalResult = "No Active Circuit.";
	}
	return result;
}


int DoNodeListCmd()
{
	int result = 0;
	int NValues = 0;
	int i = 0;
	String CktElementName;
	String s;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
	{
		s = Parser[ActiveActor]->GetNextParam();
		CktElementName = Parser[ActiveActor]->MakeString_();
		if(CktElementName.size() > 0)
			SetObject(CktElementName);
		if(ASSIGNED(ActiveCircuit[ActiveActor]->get_FActiveCktElement()))
			/*# with ActiveCircuit[ActiveActor].ActiveCktElement do */
			{
				auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
				int stop = 0;
				NValues = with0->Get_NConds() * with0->Get_NTerms();
				GlobalResult = "";
				for(stop = NValues, i = 1; i <= stop; i++)
				{
					GlobalResult = GlobalResult + Format("%d, ", GetNodeNum(with0->NodeRef[static_cast<size_t>(i) - 1]));
				}
			}
		else
			GlobalResult = "No Active Circuit.";
	}
	return result;
}

int DolossesCmd()
{
	int result = 0;
	complex LossValue = {};
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->get_FActiveCktElement() != nullptr)
			{
				GlobalResult = "";
				LossValue = ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Get_Losses(ActiveActor);
				GlobalResult = Format("%10.5g, %10.5g", LossValue.re * 0.001, LossValue.im * 0.001);
			}
		}
	else
		GlobalResult = "No Active Circuit.";
	return result;
}

int DophaselossesCmd()
{
	int result = 0;
	pComplexArray cBuffer = nullptr;
	int NValues = 0;
	int i = 0;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor].ActiveCktElement do */
		{
			auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
			int stop = 0;
			NValues = with0->Get_NPhases();
			cBuffer = (pComplexArray) malloc(sizeof(complex) * NValues);
			GlobalResult = "";
			with0->GetPhaseLosses(NValues, cBuffer, ActiveActor);
			for(stop = NValues, i = 1; i <= stop; i++)
			{
				GlobalResult = GlobalResult
	           + Format("%10.5g, %10.5g,",  cBuffer[i - 1].re * 0.001,  cBuffer[i - 1].im * 0.001);
			}			
		}
	else
		GlobalResult = "No Active Circuit.";

	free(cBuffer);

	return result;
}

// Returns Phase losses in kW, kVar

int DopowersCmd(int Total)
{
	int result = 0;
	pComplexArray cBuffer;
	int NValues = 0;
	int myInit = 0;
	int myEnd = 0;
	int j = 0;
	int i = 0;
	std::vector <complex> myBuffer;
  // If Total = 0, returns the powers per phase
  // If Total = 1, returns the power sum at each terminal
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor].ActiveCktElement do */
		{
			auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
			NValues = with0->Get_NConds() * with0->Get_NTerms();
			GlobalResult = "";
			cBuffer = (pComplexArray) malloc(sizeof(complex) * NValues);
			with0->GetPhasePower(cBuffer, ActiveActor);
			if(Total == 0)
			{
				int stop = 0;
				for(stop = NValues, i = 1; i <= stop; i++)
				{
					GlobalResult = GlobalResult
	           + Format("%10.5g, %10.5g,",  cBuffer[i - 1].re * 0.001,  cBuffer[i - 1].im * 0.001);
				}
			}
			else
			{
				int stop = 0;
				myBuffer.resize( with0->Get_NTerms() );
				for(stop = with0->Get_NTerms(), j = 1; j <= stop; j++)
				{
					int stop1 = 0;
					myBuffer[static_cast<size_t>(j) - 1] = cmplx(0.0, 0.0);
					myInit = (j - 1) * with0->Get_NConds() + 1;
					myEnd = with0->Get_NConds() * j;
					for(stop1 = myEnd, i = myInit; i <= stop1; i++)
					{
						myBuffer[static_cast<size_t>(j) - 1] = cadd(myBuffer[static_cast<size_t>(j) - 1], cBuffer[i - 1]);
					}
					GlobalResult = GlobalResult
	           + Format("%10.5g, %10.5g,", myBuffer[static_cast<size_t>(j) - 1].re * 0.001, myBuffer[static_cast<size_t>(j) - 1].im * 0.001);
				}
			}
			free(cBuffer);
		}
	else
		GlobalResult = "No Active Circuit";
	return result;
}

int DoseqcurrentsCmd()
{
	int result = 0;
	int NValues = 0;
	int i = 0;
	int j = 0;
	int k = 0;
	complex IPh[4]	= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };
	complex I012[4] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };
	pComplexArray cBuffer;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->get_FActiveCktElement() != nullptr)
				/*# with ActiveCktElement do */
				{
					auto with1 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
					GlobalResult = "";
					if(with1->Get_NPhases() < 3)
					{
						int stop = 0;
						for(stop = 3 * with1->Get_NTerms() - 1, i = 0; i <= stop; i++)
						{  // Signify n/A
							GlobalResult = GlobalResult + " -1.0,";
						}
					}
					else
					{
						int stop = 0;
						NValues = with1->Get_NConds() * with1->Get_NTerms();
						cBuffer = (pComplexArray)malloc(sizeof(complex) * NValues);
						with1->GetCurrents(cBuffer, ActiveActor);
						for(stop = with1->Get_NTerms(), j = 1; j <= stop; j++)
						{
							int stop1 = 0;
							k = (j - 1) * with1->Get_NConds();
							for(stop1 = 3, i = 1; i <= stop1; i++)
							{
								IPh[i - 1] = cBuffer[k + i - 1];
							}
							Phase2SymComp(&IPh[0], &I012[0]);
							for(stop1 = 3, i = 1; i <= stop1; i++)
							{
								GlobalResult = GlobalResult + Format("%10.5g, ", cabs(I012[i - 1]));
							}
						}
						free(cBuffer);
					} /*ELSE*/
				} /*WITH ActiveCktElement*/
		}   /*IF/WITH ActiveCircuit[ActiveActor]*/
	else
		GlobalResult = "No Active Circuit";
	return result;
}
// All sequence currents of active ciruit element
// returns magnitude only.

int DoseqpowersCmd()
{
	int result = 0;
	int NValues = 0;
	int i = 0;
	int j = 0;
	int k = 0;
	complex s = cmplx(0,0);
	complex Vph[4]	= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) },
			V012[4] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) },
			IPh[4]	= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) },
			I012[4] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };
	vector <complex> cBuffer;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->get_FActiveCktElement() != nullptr)
				/*# with ActiveCktElement do */
				{
					auto with1 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
					GlobalResult = "";
					if(with1->Get_NPhases() < 3)
					{
						int stop = 0;
						for(stop = 2 * 3 * with1->Get_NTerms() - 1, i = 0; i <= stop; i++)
						{  // Signify n/A
							GlobalResult = GlobalResult + "-1.0, ";
						}
					}
					else
					{
						int stop = 0;
						NValues = with1->Get_NConds() * with1->Get_NTerms();
						cBuffer.resize(NValues + 1);
						with1->GetCurrents(&cBuffer[0], ActiveActor);
						for(stop = with1->Get_NTerms(), j = 1; j <= stop; j++)
						{
							int stop1 = 0;
							k = (j - 1) * with1->Get_NConds();
							for(stop1 = 3, i = 1; i <= stop1; i++)
							{
								Vph[i - 1] = ActiveCircuit[ActiveActor]->Solution->NodeV[(with1->Terminals[static_cast<size_t>(j) - 1].TermNodeRef)[i - 1]];
							}
							for(stop1 = 3, i = 1; i <= stop1; i++)
							{
								IPh[i - 1] = cBuffer[(k + i) - 1];
							}
							Phase2SymComp(&IPh[0], &I012[0]);
							Phase2SymComp(&Vph[0], &V012[0]);
							for(stop1 = 3, i = 1; i <= stop1; i++)
							{
								s = cmul(V012[i - 1], conjg(I012[i - 1]));
								GlobalResult = GlobalResult
	           + Format("%10.5g, %10.5g,", s.re * 0.003, s.im * 0.003); // 3-phase kW conversion
							}
						}
					}
					cBuffer.resize(0);
				}
		}
	else
		GlobalResult = "No Active Circuit";
	return result;
}
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

int DoseqvoltagesCmd()
{
	int result = 0;
	int NValues = 0;
	int i = 0;
	int j = 0;
	int k = 0;
	int n = 0;
	complex Vph[4]	= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };
	complex V012[4] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };
	String s;
	result = 0;
	NValues = -1; // unassigned, for exception message
	n = -1; // unassigned, for exception message
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->get_FActiveCktElement() != nullptr)
				/*# with ActiveCktElement do */
				{
					auto with1 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
					if(with1->Get_Enabled())
					{
						try
						{
							NValues = with1->Get_NPhases();
							GlobalResult = "";
							if(NValues < 3)
							{
								int stop = 0;
								for(stop = 3 * with1->Get_NTerms(), i = 1; i <= stop; i++)
								{  // Signify n/A
									GlobalResult = GlobalResult + "-1.0, ";
								}
							}
							else
							{
								int stop = 0;
								for(stop = with1->Get_NTerms(), j = 1; j <= stop; j++)
								{
									int stop1 = 0;
									k = (j - 1) * with1->Get_NConds();
									for(stop1 = 3, i = 1; i <= stop1; i++)
									{
										Vph[i - 1] = (ActiveCircuit[ActiveActor]->Solution->NodeV)[(with1->NodeRef)[static_cast<size_t>(i) + k - 1]];
									}
									Phase2SymComp(&Vph[0], &V012[0]);   // Compute Symmetrical components
									for(stop1 = 3, i = 1; i <= stop1; i++)
									{  // Stuff it in the result
										GlobalResult = GlobalResult + Format("%10.5g, ", cabs(V012[i - 1]));
									}
								}
							}
						}
						catch (std::exception &e)
						{
							{
								s = (std::string) e.what()
	           + CRLF
	           + "Element="
	           + ( (TDSSObject*) ActiveCircuit[ActiveActor]->get_FActiveCktElement() )->get_Name()
	           + CRLF
	           + "Nvalues="
	           + IntToStr(NValues)
	           + CRLF
	           + "Nterms="
	           + IntToStr(with1->Get_NTerms())
	           + CRLF
	           + "NConds ="
	           + IntToStr(with1->Get_NConds())
	           + CRLF
	           + "noderef="
	           + IntToStr(n);
								DoSimpleMsg(s, 270);
							}
						}
					}
					else
					GlobalResult = "Element Disabled";
				}  // Disabled
		}
	else
		GlobalResult = "No Active Circuit";
	return result;
}

// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal


//----------------------------------------------------------------------------

int DovoltagesCmd(bool PerUnit)
{
	int result = 0;
	int i = 0;
	complex Volts = {};
	TDSSBus* ActiveBus = nullptr;
	double Vmag = 0.0;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->ActiveBusIndex != 0)
			{
				int stop = 0;
				ActiveBus = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(ActiveCircuit[ActiveActor]->ActiveBusIndex) - 1];
				GlobalResult = "";
				for(stop = ActiveBus->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
				{
					Volts = (ActiveCircuit[ActiveActor]->Solution->NodeV)[ActiveBus->GetRef(i)];
					Vmag = cabs(Volts);
					if(PerUnit && (ActiveBus->kVBase > 0.0))
					{
						Vmag = Vmag * 0.001 / ActiveBus->kVBase;
						GlobalResult = GlobalResult
	           + Format("%10.5g, %6.1f, ", Vmag, cdang(Volts));
					}
					else
					GlobalResult = GlobalResult
	           + Format("%10.5g, %6.1f, ", Vmag, cdang(Volts));
				}
			}
			else
			GlobalResult = "No Active Bus.";
		}
	else
		GlobalResult = "No Active Circuit.";
	return result;
}
// Bus Voltages at active terminal


//----------------------------------------------------------------------------

int DoZscCmd(bool Zmatrix)
{
	int result = 0;
	int i = 0;
	int j = 0;
	TDSSBus* ActiveBus = nullptr;
	complex Z = {};
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->ActiveBusIndex != 0)
			{
				ActiveBus = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(ActiveCircuit[ActiveActor]->ActiveBusIndex) - 1];
				GlobalResult = "";
				if(ActiveBus->Zsc.Norder == 0)
					return result;
				/*# with ActiveBus do */
				{
					auto with1 = ActiveBus;
					int stop = 0;
					for(stop = with1->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
					{
						int stop1 = 0;
						for(stop1 = with1->get_FNumNodesThisBus(), j = 1; j <= stop1; j++)
						{
							if(Zmatrix)
								Z = with1->Zsc.GetElement(i, j);
							else
								Z = with1->Ysc.GetElement(i, j);
							GlobalResult = GlobalResult + Format("%-.5g, %-.5g,   ", Z.re, Z.im);
						}
					}
				}
			}
			else
			GlobalResult = "No Active Bus.";
		}
	else
		GlobalResult = "No Active Circuit.";
	return result;
}
// Bus Short Circuit matrix



//----------------------------------------------------------------------------

int DoZsc012Cmd()
{
	int result = 0;
	int i = 0;
	TDSSBus* ActiveBus = nullptr;
	complex Z0 = {};
	complex Z1 = {};
	complex Z2 = {};
	pComplexArray Temp1 = nullptr;
	pComplexArray temp2 = nullptr;
	TcMatrix* Zsc012Temp = nullptr;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->ActiveBusIndex != 0)
			{
				ActiveBus = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(ActiveCircuit[ActiveActor]->ActiveBusIndex) - 1];
				GlobalResult = "";
				if (ActiveBus->Zsc.Norder == 0)
					return result;
				/*# with ActiveBus do */
				{
					auto with1 = ActiveBus;
					if(with1->get_FNumNodesThisBus() == 3)

        // Compute ZSC012 for 3-phase buses else leave it zeros
        // ZSC012 = Ap2s Zsc As2p
					{
						Zsc012Temp = with1->Zsc.MtrxMult(As2p);  // temp for intermediate result
						with1->Zsc012 = *(Ap2s->MtrxMult(Zsc012Temp));
                  // Cleanup
						delete Zsc012Temp;


              /*Just return diagonal elements only*/
						Z0 = with1->Zsc012.GetElement(1, 1);
						Z1 = with1->Zsc012.GetElement(2, 2);
						Z2 = with1->Zsc012.GetElement(3, 3);
						GlobalResult = GlobalResult
	           + Format("Z0, (%-.5g, +j %-.5g), ", Z0.re, Z0.im)
	           + CRLF;
						GlobalResult = GlobalResult
	           + Format("Z1, (%-.5g, +j %-.5g), ", Z1.re, Z1.im)
	           + CRLF;
						GlobalResult = GlobalResult
	           + Format("Z2, (%-.5g, +j %-.5g), ", Z2.re, Z2.im);
					}
					else
					GlobalResult = "Not a 3-phase bus. Cannot compute Symmetrical Component matrix.";
				}
			}
			else
			GlobalResult = "No Active Bus.";
		}
	else
		GlobalResult = "No Active Circuit.";
	return result;
}
// Bus Short Circuit matrix


//----------------------------------------------------------------------------

int DoZsc10Cmd()
{
	int result = 0;
	TDSSBus* ActiveBus = nullptr;
	complex Z = {};
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(ActiveCircuit[ActiveActor]->ActiveBusIndex != 0)
			{
				ActiveBus = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(ActiveCircuit[ActiveActor]->ActiveBusIndex) - 1];
				GlobalResult = "";
				if (ActiveBus->Zsc.Norder == 0)
					return result;
				/*# with ActiveBus do */
				{
					auto with1 = ActiveBus;
					Z = with1->Get_Zsc1();
					GlobalResult = GlobalResult
	           + Format("Z1, %-.5g, %-.5g, ", Z.re, Z.im)
	           + CRLF;
					Z = with1->Get_Zsc0();
					GlobalResult = GlobalResult + Format("Z0, %-.5g, %-.5g, ", Z.re, Z.im);
				}
			}
			else
			GlobalResult = "No Active Bus.";
		}
	else
		GlobalResult = "No Active Circuit.";
	return result;
}
// Bus Short Circuit matrix



//----------------------------------------------------------------------------

int DoAllocateLoadsCmd(int ActorID)
{
	int			result = 0,
				iterCount = 0;
	TEnergyMeterObj*	pMeter = nullptr;
	TSensorObj*			pSensor = nullptr;

	/*# with ActiveCircuit[ActorID] do */
	{
		
		int stop = 0;
		auto with0 = ActiveCircuit[ActorID];
		with0->Set_LoadMultiplier(1.0);   // Property .. has side effects
		/*# with Solution do */
		{
			auto with1 = with0->Solution;
			if(with1->Get_SolMode() != SNAPSHOT)
				with1->Set_Mode(SNAPSHOT);   // Resets meters, etc. if not in snapshot mode
			with1->Solve(ActorID);  /*Make guess based on present allocationfactors*/
		}

         /*Allocation loop -- make MaxAllocationIterations iterations*/
		for(stop = MaxAllocationIterations, iterCount = 1; iterCount <= stop; iterCount++)
		{

           /*Do EnergyMeters*/
			pMeter = (TEnergyMeterObj*) with0->EnergyMeters.Get_First();
			while(pMeter != nullptr)
			{
				pMeter->CalcAllocationFactors(ActorID);
				pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Next();
			}

           /*Now do other Sensors*/
			pSensor = (TSensorObj*)with0->Sensors.Get_First();
			while(pSensor != nullptr)
			{
				pSensor->CalcAllocationFactors(ActorID);
				pSensor = (TSensorObj*)with0->Sensors.Get_Next();
			}

           /*Now let the EnergyMeters run down the circuit setting the loads*/
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_First();
			while(pMeter != nullptr)
			{
				pMeter->AllocateLoad(ActorID);
				pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Next();
			}
			with0->Solution->Solve(ActorID);  /*Update the solution*/
		}
	}
	return result;
}

/* Requires an EnergyMeter Object at the head of the feeder
  Adjusts loads defined by connected kVA or kWh billing
*/

//----------------------------------------------------------------------------

void DoSetAllocationFactors(double X)
{
	TLoadObj* pLoad = nullptr;
	if(X <= 0.0)
		DoSimpleMsg("Allocation Factor must be greater than zero.", 271);
	else
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
			while(pLoad != nullptr)
			{
				pLoad->Set_kVAAllocationFactor(X);
				pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
			}
		}
}

void DoSetCFactors(double X)
{
	TLoadObj* pLoad = nullptr;
	if(X <= 0.0)
		DoSimpleMsg("CFactor must be greater than zero.", 271);
	else
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
			while(pLoad != nullptr)
			{
				pLoad->Set_CFactor(X);
				pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
			}
		}
}

//----------------------------------------------------------------------------

int DoHarmonicsList(const String s)
{
	int result = 0;
	pDoubleArray Dummy;
	int i = 0;
	int Num = 0;
	result = 0;
	/*# with ActiveCircuit[ActiveActor].Solution do */
	{
		auto with0 = ActiveCircuit[ActiveActor]->Solution;
		if(CompareText(s, "ALL") == 0)
			with0->DoAllHarmonics = true;
		else
		{
			int stop = 0;
			with0->DoAllHarmonics = false;
			Dummy = (pDoubleArray)malloc(sizeof(complex) * 100); 
			Num = Parser[ActiveActor]->ParseAsVector(100, Dummy);
       /*Parsing zero-fills the array*/
			with0->HarmonicListSize = Num;
			with0->HarmonicList = new double[ with0->HarmonicListSize ];
			for(stop = with0->HarmonicListSize, i = 1; i <= stop; i++)
			{
				(with0->HarmonicList)[i - 1] = (Dummy)[i - 1];
			}
			free(Dummy);
		}
	}
	return result;
}


//----------------------------------------------------------------------------

int DoFormEditCmd()
{
	int result = 0;
	result = 0;
	if(NoFormsAllowed)
		return result;
	DoSelectCmd();  // Select ActiveObject
	if(ActiveDSSObject[ActiveActor] != nullptr)
	{
		ShowPropEditForm();
	}
	else
	{
		DoSimpleMsg("Element Not Found.", 272);
		result = 1;
	}
	return result;
}


//----------------------------------------------------------------------------

int DoMeterTotals()
{
	int result = 0;
	int i = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
	{
		ActiveCircuit[ActiveActor]->TotalizeMeters();
        // Now export to global result
		for (i = 0; i < NumEMRegisters; i++)
		{
			AppendGlobalResult(Format("%-.6g", ActiveCircuit[ActiveActor]->RegisterTotals[i]));
		}
	}
	return result;
}

//----------------------------------------------------------------------------

int DoCapacityCmd()
{
	int result = 0;
	int ParamPointer = 0;
	String Param;
	String ParamName;
	result = 0;
	ParamPointer = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			switch(ParamName[1])
			{
				case 	L's':
				ParamPointer = 1;
				break;
				case 	L'i':
				ParamPointer = 2;
				break;
				default:
				ParamPointer = 0;
				break;
			}
		switch(ParamPointer)
		{
			case 	0:
			DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for Capacity Command", 273);
			break;
			case 	1:
			ActiveCircuit[ActiveActor]->CapacityStart = Parser[ActiveActor]->MakeDouble_();
			break;
			case 	2:
			ActiveCircuit[ActiveActor]->CapacityIncrement = Parser[ActiveActor]->MakeDouble_();
			break;
			default:
			  ;
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		if(ActiveCircuit[ActiveActor]->ComputeCapacity(ActiveActor))   // Totalizes EnergyMeters at End
		{
			GlobalResult = Format("%-.6g", (ActiveCircuit[ActiveActor]->RegisterTotals[3 - 1] + ActiveCircuit[ActiveActor]->RegisterTotals[19 - 1]));  // Peak KW in Meters
			AppendGlobalResult(Format("%-.6g", ActiveCircuit[ActiveActor]->get_FLoadMultiplier()));
		}
	}
	return result;
}

//----------------------------------------------------------------------------

int DoClassesCmd()
{
	int result = 0;
	int i = 0;
	int stop = 0;
	for(stop = NumIntrinsicClasses, i = 1; i <= stop; i++)
	{
		AppendGlobalResult(((TDSSClass*) DSSClassList[ActiveActor].Get(i))->get_myClass_name());
	}
	result = 0;
	return result;
}

//----------------------------------------------------------------------------

int DoUserClassesCmd()
{
	int result = 0;
	int i = 0;
	result = 0;
	if(NumUserClasses == 0)
	{
		AppendGlobalResult("No User Classes Defined.");
	}
	else
	{
		int stop = 0;
		for(stop = DSSClassList[ActiveActor].get_myNumList(), i = NumIntrinsicClasses + 1; i <= stop; i++)
		{
			AppendGlobalResult(((TDSSClass*) DSSClassList[ActiveActor].Get(i))->get_myClass_name());
		}
	}
	return result;
}

//----------------------------------------------------------------------------

int DoZscRefresh(int ActorID)
{
	int result = 0;
	int j = 0;
	result = 1;
	try
	{
		/*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do */
		{
			
			auto with1 = ActiveCircuit[ActiveActor]->Solution;
			int stop = 0;
			for(stop = ActiveCircuit[ActiveActor]->NumNodes, j = 1; j <= stop; j++)
			{
				(with1->Currents)[j - 1] = CZero;
			}  // Clear Currents array
			if((ActiveCircuit[ActiveActor]->ActiveBusIndex >= 0) && (ActiveCircuit[ActiveActor]->ActiveBusIndex < ActiveCircuit[ActiveActor]->NumBuses))
			{
				if(ActiveCircuit[ActiveActor]->Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex]->Zsc.Norder == 0)
					ActiveCircuit[ActiveActor]->Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex]->AllocateBusQuantities();
				ComputeYsc(ActiveCircuit[ActiveActor]->ActiveBusIndex, ActorID);      // Compute YSC for active Bus
				result = 0;
			}
		}
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("ZscRefresh Error: ") + (std::string) e.what() + CRLF, 274);
	}
	return result;
}

int DoVarValuesCmd()
{
	int result = 0;
	int i = 0;
  // PcElem:TPCElement;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
         /*Check if PCElement*/
			switch(( ( (TDSSObject*) ActiveCircuit[ActiveActor]->get_FActiveCktElement() )->DSSObjType & BaseClassMask))
			{
				case 	PC_ELEMENT:
				/*# with ActiveCktElement as TPCElement do */
				{
					auto with1 = (TPCElement*) ActiveCircuit[ActiveActor]->get_FActiveCktElement();
					int stop = 0;
					for(stop = with1->NumVariables(), i = 1; i <= stop; i++)
					{
						AppendGlobalResult(Format("%-.6g", with1->Get_Variable(i)));
					}
				}
				break;
				default:
				AppendGlobalResult("Null");
				break;
			}
		}
	return result;
}

int DoValVarCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	int VarIndex = 0;
	int propIndex = 0;
	TPCElement* pcElem = nullptr;
	result = 0;

    /*Check to make sure this is a PC Element. If not, return null string in global result*/
	if( ( ( (TDSSObject*) ActiveCircuit[ActiveActor]->get_FActiveCktElement() )->DSSObjType & BaseClassMask) != PC_ELEMENT )
		GlobalResult = "";
	else
	{
		pcElem = (TPCElement*) ActiveCircuit[ActiveActor]->get_FActiveCktElement();

        /*Get next parameter on command line*/
		ParamName = UpperCase(Parser[ActiveActor]->GetNextParam());
		Param = Parser[ActiveActor]->MakeString_();
		propIndex = 1;
		if(ParamName.size() > 0)
			switch(ParamName[1])
			{
				case 	L'N':
				propIndex = 1;
				break;
				case 	L'I':
				propIndex = 2;
				break;
				default:
				  ;
				break;
			}
		VarIndex = 0;
		switch(propIndex)
		{
			case 	1:
			VarIndex = pcElem->LookupVariable(Param);
			break;  // Look up property index
			case 	2:
			VarIndex = Parser[ActiveActor]->MakeInteger_();
			break;
			default:
			  ;
			break;
		}
		if((VarIndex > 0) && (VarIndex <= pcElem->NumVariables()))
			GlobalResult = Format("%.8g", pcElem->Get_Variable(VarIndex));
		else
			GlobalResult = "";   /*Invalid var name or index*/
	}
	return result;
}

/*Geg value of specified variable by name of index,*/

int DoVarNamesCmd()
{
	int result = 0;
	int i = 0;
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
         /*Check if PCElement*/
			switch( ( ( (TDSSObject*) ActiveCircuit[ActiveActor]->get_FActiveCktElement() )->DSSObjType & BaseClassMask))
			{
				case 	PC_ELEMENT:
				/*# with (ActiveCktElement as TPCElement) do */
				{
					auto with1 = ((TPCElement*) ActiveCircuit[ActiveActor]->get_FActiveCktElement());
					int stop = 0;
					for(stop = with1->NumVariables(), i = 1; i <= stop; i++)
					{
						AppendGlobalResult(with1->VariableName(i));
					}
				}
				break;
				default:
				AppendGlobalResult("Null");
				break;
			}
		}
	return result;
}

int DoBusCoordsCmd(bool SwapXY, int CoordType)
{
	int result = 0;
	TTextRec f = {};
	String dummy;
	String ParamName;
	String Param;
	String s;
	String Busname;
	int iB = 0;
	int iLine = 0;
	result = 0;

    /*Get next parameter on command line*/
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	try
	{
		iLine = -1;
		try
		{
			AssignFile(f, Param);
			Reset(f);
			IOResultToException();
			iLine = 0;
			while(!Eof(f))
			{
				++iLine;
				ReadLn(f, s);      // Read line in from file
				/*# with AuxParser[ActiveActor] do */
				{
					      // User Auxparser to parse line
					auto with35 = AuxParser[ActiveActor];
					with35->SetCmdString(s);
					dummy = with35->GetNextParam();
					Busname = with35->MakeString_();
					iB = ActiveCircuit[ActiveActor]->BusList.Find(Busname);
					if(iB > 0)
					{
						/*# with ActiveCircuit[ActiveActor]->Buses^[iB] do */
						{
							     // Returns TBus object
							if(CoordType == 0)                                   // Standard buscoords
							{
								dummy = with35->GetNextParam();
								if(SwapXY)
									ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->y = with35->MakeDouble_();
								else
									ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->x = with35->MakeDouble_();
								dummy = with35->GetNextParam();
								if(SwapXY)
									ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->x = with35->MakeDouble_();
								else
									ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->y = with35->MakeDouble_();
								ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->CoordDefined = true;
							}
							else
                                                   // GIS coords
							{
								dummy = with35->GetNextParam();
								ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->lat = with35->MakeDouble_();
								dummy = with35->GetNextParam();
								ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->longitude = with35->MakeDouble_();
								ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->GISCoordDefined = true;
							}
						}
					}
				}
              /*Else just ignore a bus that's not in the circuit*/
			}
      /***CHANGE THIS ERROR MESSAGE***/
		}
		catch (std::exception &e)
		{
			{
				if(iLine ==  - 1)
					DoSimpleMsg(String("Bus Coordinate file: \"") + Param
	           + "\" not found; "
	           + (std::string) e.what(), 275);
				else
					DoSimpleMsg(String("Bus Coordinate file: Error Reading Line ") + IntToStr(iLine)
	           + "; "
	           + (std::string) e.what(), 275);
			}
		}
/* }
	__finally
	{*/
		CloseFile(f);
	}
	catch (...)
	{
		//
	}
	return result;
}

/*
 Format of File should be

   Busname, x, y

   (x, y are real values)

   If SwapXY is true, x and y values are swapped

*/

int DoMakePosSeq()
{
	int result = 0;
	TDSSCktElement* CktElem = nullptr;
	result = 0;
	ActiveCircuit[ActiveActor]->PositiveSequence = true;
	CktElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_First();
	while(CktElem != nullptr)
	{
		CktElem->MakePosSequence(ActiveActor);
		CktElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_Next();
	}
	return result;
}

void DoSetReduceStrategy(const String s)
{

	auto AtLeast = [&](int i, int j) -> int 
	{
		int result = 0;
		if(j < i)
			result = i;
		else
			result = j;
		return result;
	};
	ActiveCircuit[ActiveActor]->ReductionStrategyString = s;
	ActiveCircuit[ActiveActor]->ReductionStrategy = rsDefault;
	if(s.size() == 0)
		return;  /*No option given*/
	switch(UpperCase(s)[0])
	{
		case 	L'B':
		ActiveCircuit[ActiveActor]->ReductionStrategy = rsBreakLoop;
		break;
		case 	L'D':
		ActiveCircuit[ActiveActor]->ReductionStrategy = rsDefault;
		break;  /*Default*/
		case 	L'E':
		ActiveCircuit[ActiveActor]->ReductionStrategy = rsDangling;
		break;  /*Ends*/ /*Laterals*/
		case 	L'L':
		{
			ActiveCircuit[ActiveActor]->ReductionStrategy = rsLaterals;
		}
		break;
		case 	L'M':
		ActiveCircuit[ActiveActor]->ReductionStrategy = rsMergeParallel;
		break;
        /**
       'T': Begin
              ActiveCircuit[ActiveActor].ReductionStrategy := rsTapEnds;
              ActiveCircuit[ActiveActor].ReductionMaxAngle := 15.0;
              If Length(param2) > 0 Then  ActiveCircuit[ActiveActor].ReductionMaxAngle := Auxparser.MakeDouble_();
            End;
            **/  /*Shortlines or Switch*/
		case 	L'S':
		{
			if(CompareTextShortest(s, "SWITCH") == 0)
			{
				ActiveCircuit[ActiveActor]->ReductionStrategy = rsSwitches;
			}
			else

                 /* ActiveCircuit.ReductionZmag is now set in main ExecOptions     */
			{
				ActiveCircuit[ActiveActor]->ReductionStrategy = rsShortlines;
			}
		}
		break;
		default:
		DoSimpleMsg(String("Unknown Reduction Strategy: \"") + s + "\".", 276);
		break;
	}
}

int DoInterpolateCmd()
{
	int result = 0;
	TEnergyMeterObj* MetObj = nullptr;
	TEnergyMeter* MeterClass = nullptr;
	String ParamName;
	String Param;
	int DevClassIndex = 0;
	TDSSCktElement* CktElem = nullptr;
	result = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = UpperCase(Parser[ActiveActor]->MakeString_());

    // initialize the Checked Flag FOR all circuit Elements
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		CktElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_First();
		while((CktElem != nullptr))
		{
			CktElem->Checked = false;
			CktElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_Next();
		}
	}
	if(Param.size() == 0)
		Param = "A";
	switch(Param[0])
	{
		case 	'A':
		{
			MetObj = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
			while(MetObj != nullptr)
			{
				MetObj->InterpolateCoordinates();
				MetObj = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_Next();
			}
		}
		break;
       /*Interpolate a specific meter*/
		default:
		DevClassIndex = ClassNames[ActiveActor].Find("energymeter");
		if(DevClassIndex > 0)
		{
			MeterClass = (TEnergyMeter*)DSSClassList[ActiveActor].Get(DevClassIndex);
			if( ( (TDSSClass*) MeterClass )->SetActive(Param))   // Try to set it active
			{
				MetObj = ((TEnergyMeterObj*) ( (TDSSClass*) MeterClass )->GetActiveObj());
				MetObj->InterpolateCoordinates();
			}
			else
			DoSimpleMsg(String("EnergyMeter \"") + Param + "\" not found.", 277);
		}
		break;
	}
	return result;
}

/*Interpolate bus coordinates in meter zones*/

int DoAlignFileCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	result = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	if(FileExists(Param))
	{
		if(!RewriteAlignedFile(Param))
			result = 1;
	}
	else
	{
		DoSimpleMsg(String("File \"") + Param + "\" does not exist.", 278);
		result = 1;
	}
	if(result == 0)
		FireOffEditor(GlobalResult);
	return result;
}
/*Rewrites designated file, aligning the fields into columns*/ /*DoAlignfileCmd*/

int DoTOPCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	String ObjName;
	result = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = UpperCase(Parser[ActiveActor]->MakeString_());
	ParamName = Parser[ActiveActor]->GetNextParam();
	ObjName = UpperCase(Parser[ActiveActor]->MakeString_());
	if(ObjName.size() == 0)
		ObjName = "ALL";
	switch(Param[1])
	{
		case 	L'L':
		LoadShapeClass[ActiveActor]->TOPExport(ObjName);
		break;
		case 	L'T':
		TShapeClass[ActiveActor]->TOPExport(ObjName);
		break;
        /*
          'G': GrowthShapeClass.TOPExportAll;
          'T': TCC_CurveClass.TOPExportAll;
        */
		default:
		MonitorClass[ActiveActor]->TOPExport(ObjName);
		break;
	}
	return result;
}
/* Sends Monitors, Loadshapes, GrowthShapes, or TCC Curves to TOP as an STO file*/

void DoSetNormal(double pctNormal)
{
	int i = 0;
	TLineObj* pLine = nullptr;
	if(ActiveCircuit[ActiveActor] != nullptr)
	{
		int stop = 0;
		pctNormal = pctNormal * 0.01;  // local copy only
		for(stop = ActiveCircuit[ActiveActor]->Lines.get_myNumList(), i = 1; i <= stop; i++)
		{
			pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get(i);
			pLine->NormAmps = pctNormal * pLine->EmergAmps;
		}
	}
}

int DoRotateCmd()
{
	int result = 0;
	int i = 0;
	double Angle = 0.0;
	double xmin = 0.0;
	double xmax = 0.0;
	double ymin = 0.0;
	double ymax = 0.0;
	double xc = 0.0;
	double YC = 0.0;
	String ParamName;
	complex A = {};
	complex Vector = {};
	result = 0;
	if(ActiveCircuit[ActiveActor] != nullptr)
	{
		ParamName = Parser[ActiveActor]->GetNextParam();
		Angle = Parser[ActiveActor]->MakeDouble_() * DSSGlobals::PI / 180.0;   // Deg to rad
		A = cmplx(cos(Angle), sin(Angle));
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			int stop = 0;
			xmin = 1.0e50;
			xmax = -1.0e50;
			ymin = 1.0e50;
			ymax = -1.0e50;
			for(stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
			{
				if( ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(i) - 1]->CoordDefined)
				{
					/*# with ActiveCircuit[ActiveActor]->Buses^[i] do */
					{
						
						xmax = max(xmax, ActiveCircuit[ActiveActor]->Buses[i - 1]->x);
						xmin = min(xmin, ActiveCircuit[ActiveActor]->Buses[i - 1]->x);
						ymax = max(ymax, ActiveCircuit[ActiveActor]->Buses[i - 1]->y);
						ymin = min(ymin, ActiveCircuit[ActiveActor]->Buses[i - 1]->y);
					}
				}
			}
			xc = (xmax + xmin) / 2.0;
			YC = (ymax + ymin) / 2.0;
			for(stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
			{
				if(ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(i) - 1]->CoordDefined)
				{
					/*# with ActiveCircuit[ActiveActor]->Buses^[i] do */
					{
						
						Vector = cmplx(ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(i) - 1]->x - xc, ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(i) - 1]->y - YC);
						Vector = cmul(Vector, A);
						ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(i) - 1]->x = xc + Vector.re;
						ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(i) - 1]->y = YC + Vector.im;
					}
				}
			}
		}
	}
	return result;
}

/*rotate about the center of the coordinates*/

int DoVDiffCmd()
{
	int result = 0;
	TTextRec Fin = {};
	TTextRec Fout = {};
	String Busname;
	String dummy;
	String Line;
	int i = 0;
	int Node = 0;
	int BusIndex = 0;
	double Vmag = 0.0;
	double Diff = 0.0;
	result = 0;
	if(FileExists(CircuitName_[ActiveActor] + "SavedVoltages.Txt"))
	{
		try
		{
			try
			{
				AssignFile(Fin, CircuitName_[ActiveActor] + "SavedVoltages.Txt");
				Reset(Fin);
				IOResultToException();
				AssignFile(Fout, CircuitName_[ActiveActor] + "VDIFF.txt");
				Rewrite(Fout);
				IOResultToException();
				while(!Eof(Fin))
				{
					ReadLn(Fin, Line);
					AuxParser[ActiveActor]->SetCmdString(Line);
					dummy = AuxParser[ActiveActor]->GetNextParam();
					Busname = AuxParser[ActiveActor]->MakeString_();
					if(Busname.size() > 0)
					{
						BusIndex = ActiveCircuit[ActiveActor]->BusList.Find(Busname);
						if(BusIndex > 0)
						{
							dummy = AuxParser[ActiveActor]->GetNextParam();
							Node = AuxParser[ActiveActor]->MakeInteger_();
							/*# with ActiveCircuit[ActiveActor]->Buses^[BusIndex] do */
							{
								
								int stop = 0;
								for(stop = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(BusIndex) - 1]->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
								{
									if( ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(BusIndex) - 1]->GetNum(i) == Node )
									{
										dummy = AuxParser[ActiveActor]->GetNextParam();
										Vmag = AuxParser[ActiveActor]->MakeDouble_();
										Diff = cabs((ActiveCircuit[ActiveActor]->Solution->NodeV)[ ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(BusIndex) - 1]->GetRef(i)] ) - Vmag;
										if(Vmag != 0.0)
										{
											{ Write(Fout, Busname); Write(Fout, L'.'); Write(Fout, Node); Write(Fout, ", "); Write(Fout, (Diff / Vmag * 100.0), 7, 2); WriteLn(Fout, ", %"); }
										}
										else
										{ Write(Fout, Busname); Write(Fout, L'.'); Write(Fout, Node); Write(Fout, ", "); Write(Fout, Format("%-.5g", Diff)); WriteLn(Fout, ", Volts"); }
									}
								}
							}
						}
					}
				}
			}
			catch (std::exception &e)
			{
				{
					DoSimpleMsg(String("Error opening Saved Voltages or VDIFF File: ") + (std::string) e.what(), 280);
					return result;
				}
			}
/* }
		__finally
		{*/
			CloseFile(Fin);
			CloseFile(Fout);
			FireOffEditor(CircuitName_[ActiveActor] + "VDIFF.txt");
		}
		catch (...)
		{
			//
		}
	}
	else
	DoSimpleMsg("Error: No Saved Voltages.", 281);
	return result;
}

int DoSummaryCmd()
{
	int result = 0;
	String s;
	complex cLosses = {};
	complex cPower = {};
	result = 0;
	s = "";
	if(ActiveCircuit[ActiveActor]->Issolved)
		s = s + "Status = SOLVED" + CRLF;
	else
	{
		s = s + "Status = NOT Solved" + CRLF;
	}
	s = s + "Solution Mode = " + GetSolutionModeID() + CRLF;
	s = s + "Number = " + IntToStr(ActiveCircuit[ActiveActor]->Solution->NumberOfTimes) + CRLF;
	s = s
	           + "Load Mult = "
	           + Format("%5.3f", ActiveCircuit[ActiveActor]->get_FLoadMultiplier())
	           + CRLF;
	s = s + "Devices = " + Format("%d", ActiveCircuit[ActiveActor]->NumDevices) + CRLF;
	s = s + "Buses = " + Format("%d", ActiveCircuit[ActiveActor]->NumBuses) + CRLF;
	s = s + "Nodes = " + Format("%d", ActiveCircuit[ActiveActor]->NumNodes) + CRLF;
	s = s + "Control Mode =" + GetControlModeID() + CRLF;
	s = s + "Total Iterations = " + IntToStr(ActiveCircuit[ActiveActor]->Solution->Iteration) + CRLF;
	s = s
	           + "Control Iterations = "
	           + IntToStr(ActiveCircuit[ActiveActor]->Solution->ControlIteration)
	           + CRLF;
	s = s + "Max Sol Iter = " + IntToStr(ActiveCircuit[ActiveActor]->Solution->MostIterationsDone) + CRLF;
	s = s + " " + CRLF;
	s = s + " - Circuit Summary -" + CRLF;
	s = s + " " + CRLF;
	if(ActiveCircuit[ActiveActor] != nullptr)
	{
		s = s + Format("Year = %d ", ActiveCircuit[ActiveActor]->Solution->get_Fyear()) + CRLF;
		s = s
	           + Format("Hour = %d ", ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour)
	           + CRLF;
		s = s
	           + "Max pu. voltage = "
	           + Format("%-.5g ", GetMaxPUVoltage())
	           + CRLF;
		s = s
	           + "Min pu. voltage = "
	           + Format("%-.5g ", GetMinPUVoltage(true))
	           + CRLF;
		cPower = cmulreal(GetTotalPowerFromSources(ActiveActor), 0.000001);  // MVA
		s = s + Format("Total Active Power:   %-.6g MW", cPower.re) + CRLF;
		s = s + Format("Total Reactive Power: %-.6g Mvar", cPower.im) + CRLF;
		cLosses = cmulreal(ActiveCircuit[ActiveActor]->Get_Losses(ActiveActor), 0.000001);
		if(cPower.re != 0.0)
			s = s
	           + Format("Total Active Losses:   %-.6g MW, (%-.4g %%)", cLosses.re, (cLosses.re / cPower.re * 100.0))
	           + CRLF;
		else
			s = s + "Total Active Losses:   ****** MW, (**** %%)" + CRLF;
		s = s + Format("Total Reactive Losses: %-.6g Mvar", cLosses.im) + CRLF;
		s = s
	           + Format("Frequency = %-g Hz", ActiveCircuit[ActiveActor]->Solution->get_FFrequency())
	           + CRLF;
		s = s + "Mode = " + GetSolutionModeID() + CRLF;
		s = s + "Control Mode = " + GetControlModeID() + CRLF;
		s = s + "Load Model = " + GetLoadModel() + CRLF;
	}
	GlobalResult = s;
	CoutLn(s); // publishes the info
	return result;
}

// Returns summary in global result String

int DoDistributeCmd()
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	bool DoGenerators = false;
	double kW = 0.0;
	double PF = 0.0;
	int Skip = 0;
	String how;
	String FilName;
	result = 0;
	ParamPointer = 0;
     /*Defaults*/
	kW = 1000.0;
	how = "Proportional";
	Skip = 1;
	PF = 1.0;
	FilName = "DistGenerators.dss";
	DoGenerators = true;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = DistributeCommands->Getcommand(ParamName);
		switch(ParamPointer)
		{
			case 	1:
			kW = Parser[ActiveActor]->MakeDouble_();
			break;
			case 	2:
			how = Parser[ActiveActor]->MakeString_();
			break;
			case 	3:
			Skip = Parser[ActiveActor]->MakeInteger_();
			break;
			case 	4:
			PF = Parser[ActiveActor]->MakeDouble_();
			break;
			case 	5:
			FilName = Parser[ActiveActor]->MakeString_();
			break;
			case 	6:
			kW = Parser[ActiveActor]->MakeDouble_() * 1000.0;
			break;
			case 	7:
			if(UpperCase(Param)[1] == L'L')
				DoGenerators = false;
			else
				DoGenerators = true;
			break;  // Load or Generator

             // ignore unnamed and extra parms
			default:
			  ;
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
	if(!DoGenerators)
		FilName = "DistLoads.dss";
	MakeDistributedGenerators(kW, PF, how, Skip, FilName, DoGenerators);  // in Utilities
	return result;
}

int DoDI_PlotCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	int ParamPointer = 0;
	int i = 0;
	String CaseName;
	String MeterName;
	int CaseYear = 0;
	double dRegisters[231/*# range 1..NumEMRegisters*/]{};
	std::vector <int> iRegisters;
	int NumRegs = 0;
	bool PEAKDAY = false;
	if(DIFilesAreOpen[ActiveActor])
		EnergyMeterClass[ActiveActor]->CloseAllDIFiles(ActiveActor);
//	if(!ASSIGNED(DSSPlotObj))
//		DSSPlotObj = new TDSSPlot();
     /*Defaults*/
	NumRegs = 1;
	iRegisters.resize( NumRegs );
	iRegisters[0] = 9;
	PEAKDAY = false;
	CaseYear = 1;
	CaseName = "";
	MeterName = "DI_Totals";
	ParamPointer = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = DI_PlotCommands->Getcommand(ParamName);
		switch(ParamPointer)
		{
			case 	1:
			CaseName = Param;
			break;
			case 	2:
			CaseYear = Parser[ActiveActor]->MakeInteger_();
			break;
			case 	3:
			{
				int stop = 0;
				NumRegs = Parser[ActiveActor]->ParseAsVector(NumEMRegisters, (pDoubleArray) dRegisters);
				iRegisters.resize( NumRegs );
				for(stop = NumRegs, i = 1; i <= stop; i++)
				{
					iRegisters[static_cast<size_t>(i) - 1] = (int) Round(dRegisters[i - 1]);
				}
			}
			break;
			case 	4:
			PEAKDAY = InterpretYesNo(Param);
			break;
			case 	5:
			MeterName = Param;
			break;
             // ignore unnamed and extra parms
			default:
			  ;
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
//	DSSPlotObj->DoDI_Plot(CaseName, CaseYear, &iRegisters[0], iRegisters.High, PEAKDAY, MeterName);
	iRegisters.clear();
	result = 0;
	return result;
}

int DoCompareCasesCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	int ParamPointer = 0;
	bool unknown = false;
	int reg = 0;
	String CaseName1;
	String CaseName2;
	String WhichFile;
	if(DIFilesAreOpen[ActiveActor])
		EnergyMeterClass[ActiveActor]->CloseAllDIFiles(ActiveActor);
//	if(!ASSIGNED(DSSPlotObj))
//		DSSPlotObj = new TDSSPlot();
	CaseName1 = "base";
	CaseName2 = "";
	reg = 9;    // Overload EEN
	WhichFile = "Totals";
	ParamPointer = 0;
	ParamName = UpperCase(Parser[ActiveActor]->GetNextParam());
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		unknown = false;
		if(Param.size() == 0)
			++ParamPointer;
		else
		{
			if(CompareTextShortest(ParamName, "CASE1") == 0){
				ParamPointer = 1;
				++ParamPointer;
			}
			else
			{
				if(CompareTextShortest(ParamName, "CASE2") == 0){
					ParamPointer = 2;
					++ParamPointer;
				}
				else
				{
					if(CompareTextShortest(ParamName, "REGISTER") == 0) {
						ParamPointer = 3;
						++ParamPointer;
					}
					else
					{
						if(CompareTextShortest(ParamName, "METER") == 0) {
							ParamPointer = 4;
							++ParamPointer;
						}
						else
							unknown = true;
					}
				}
			}
		}
		if(!unknown)
			switch(ParamPointer)
			{
				case 	1:
				CaseName1 = Param;
				break;
				case 	2:
				CaseName2 = Param;
				break;
				case 	3:
				reg = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	4:
				WhichFile = Param;
				break;
             // ignore unnamed and extra parms
				default:
				  ;
				break;
			}
		ParamName = UpperCase(Parser[ActiveActor]->GetNextParam());
		Param = Parser[ActiveActor]->MakeString_();
	}
//	DSSPlotObj->DoCompareCases(CaseName1, CaseName2, WhichFile, reg);
	result = 0;
	return result;
}

int DoYearlyCurvesCmd()
{
	int result = 0;
	String dummy;
	String ParamName;
	String Param;
	int ParamPointer = 0;
	int i = 0;
	bool unknown = false;
	TStringList* CaseNames = nullptr;
	double dRegisters[231/*# range 1..NumEMRegisters*/]{};
	std::vector<int> iRegisters;
	int Nregs = 0;
	String WhichFile;
	if(DIFilesAreOpen[ActiveActor])
		EnergyMeterClass[ActiveActor]->CloseAllDIFiles(ActiveActor);
//	if(!ASSIGNED(DSSPlotObj))
//		DSSPlotObj = new TDSSPlot();
	Nregs = 1;
	iRegisters.resize( Nregs );
	WhichFile = "Totals";
	ParamPointer = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		unknown = false;
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			switch(UpperCase(ParamName)[1])
			{
				case 	L'C':
				ParamPointer = 1;
				break;
				case 	L'R':
				ParamPointer = 2;
				break;
				case 	L'M':
				ParamPointer = 3;
				break; /*meter=*/
				default:
				unknown = true;
				break;
			}
		if(!unknown)
			switch(ParamPointer)
			{
				case 	1:  // List of case names
				{
					AuxParser[ActiveActor]->SetCmdString(Param);
					dummy = AuxParser[ActiveActor]->GetNextParam();
					Param = AuxParser[ActiveActor]->MakeString_();
					while(Param.size() > 0)
					{
						CaseNames->push_back(Param);
						dummy = AuxParser[ActiveActor]->GetNextParam();
						Param = AuxParser[ActiveActor]->MakeString_();
					}
				}
				break;
				case 	2:
				{
					int stop = 0;
					Nregs = Parser[ActiveActor]->ParseAsVector(NumEMRegisters, (pDoubleArray) dRegisters);
					iRegisters.resize( Nregs );
					for(stop = Nregs, i = 1; i <= stop; i++)
					{
						iRegisters[static_cast<size_t>(i) - 1] = (int) Round(dRegisters[i - 1]);
					}
				}
				break;
				case 	3:
				WhichFile = Param;
				break;
             // ignore unnamed and extra parms
				default:
				  ;
				break;
			}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
//	DSSPlotObj->DoYearlyCurvePlot(&CaseNames, WhichFile, &iRegisters[0], iRegisters.High);
	iRegisters.clear();
	result = 0;
	return result;
}

int DoVisualizeCmd()
{
	int result = 0;
	int DevIndex = 0;
	String Param;
	String ParamName;
	int ParamPointer = 0;
	bool unknown = false;
	int Quantity = 0;
	String ElemName;
	TDSSObject* PElem = nullptr;
	result = 0; 
     // Abort if no circuit or solution
	if(!ASSIGNED(ActiveCircuit[ActiveActor]))
	{
		DoSimpleMsg("No circuit created.", 24721);
		return result;
	}
	if(!ASSIGNED(ActiveCircuit[ActiveActor]->Solution) || (ActiveCircuit[ActiveActor]->Solution->NodeV.empty()))
	{
		DoSimpleMsg("The circuit must be solved before you can do this.", 24722);
		return result;
	}
//	Quantity = vizCURRENT;
	Quantity = 1;
	ElemName = "";
        //Parse rest of command line
	/*
	ParamPointer = 0;
	ParamName = UpperCase(Parser[ActiveActor]->GetNextParam());
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		unknown = false;
		if(ParamName.size() == 0)
			++ParamPointer;
		else
		{
			if(CompareTextShortest(ParamName, "WHAT") == 0)
				ParamPointer = 1;
			else
			{
				if(CompareTextShortest(ParamName, "ELEMENT") == 0)
					ParamPointer = 2;
				else
					unknown = true;
			}
		}
		if(!unknown)
			switch(ParamPointer)
			{
				case 	1:
				switch(LowerCase(Param)[0])
				{
					case 	L'c':
					Quantity = vizCURRENT;
					break;
					case 	L'v':
					Quantity = vizVOLTAGE;
					break;
					case 	L'p':
					Quantity = vizPOWER;
					break;
					default:
					  ;
					break;
				}
				break;
				case 	2:
				ElemName = Param;
				break;
             // ignore unnamed and extra parms
				default:
				  ;
				break;
			}
		ParamName = UpperCase(Parser[ActiveActor]->GetNextParam());
		Param = Parser[ActiveActor]->MakeString_();
	} //WHILE

     //--------------------------------------------------------------
	DevIndex = GetCktElementIndex(ElemName); // Global function
	if(DevIndex > 0)  //  element must already exist
	{
		PElem = ActiveCircuit[ActiveActor]->CktElements.Get(DevIndex);
		if(ObjectIs(PElem, TDSSCktElement*))
		{
			DSSPlotObj->DoVisualizationPlot(((TDSSCktElement*) PElem), Quantity);
		}
		else
		{
			DoSimpleMsg(PElem->get_Name() + " must be a circuit element type!", 282);   // Wrong type
		}
	}
	else
	{
		DoSimpleMsg(String("Requested Circuit Element: \"") + ElemName
	           + "\" Not Found.", 282); // Did not find it ..
	}
	*/
	return result;
}

int DoCloseDICmd()
{
	int result = 0;
	result = 0;
	EnergyMeterClass[ActiveActor]->CloseAllDIFiles(ActiveActor);
	return result;
}

int DoADOScmd()
{
	int result = 0;
	result = 0;
	DoDOSCmd(Parser[ActiveActor]->Get_Remainder());
	return result;
}

int DoEstimateCmd()
{
	int result = 0;
	result = 0;

    /*Load current Estimation is driven by Energy Meters at head of feeders.*/
	DoAllocateLoadsCmd(ActiveActor);

    /*Let's look to see how well we did*/
	if(!AutoShowExport)
		DSSExecutive[ActiveActor]->Set_Command("Set showexport=yes");
	DSSExecutive[ActiveActor]->Set_Command("Export Estimation");
	return result;
}

int DoReconductorCmd()
{
	int result = 0;
	String Param;
	String ParamName;
	int ParamPointer = 0;
	String Line1;
	String Line2;
	String LineCode;
	String Geometry;
	String EditString;
	String MyEditString;
	bool LineCodeSpecified = false;
	bool GeometrySpecified = false;
	TLineObj* pLine1 = nullptr;
	TLineObj* pLine2 = nullptr;
	TLine* LineClass = nullptr;
	int TraceDirection = 0;
	int NPhases = 0;
	result = 0;
	ParamPointer = 0;
	LineCodeSpecified = false;
	GeometrySpecified = false;
	Line1 = "";
	Line2 = "";
	MyEditString = "";
	NPhases = 0; // no filtering by number of phases
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = ReconductorCommands->Getcommand(ParamName);
		switch(ParamPointer)
		{
			case 	1:
			Line1 = Param;
			break;
			case 	2:
			Line2 = Param;
			break;
			case 	3:
			{
				LineCode = Param;
				LineCodeSpecified = true;
				GeometrySpecified = false;
			}
			break;
			case 	4:
			{
				Geometry = Param;
				LineCodeSpecified = false;
				GeometrySpecified = true;
			}
			break;
			case 	5:
			MyEditString = Param;
			break;
			case 	6:
			NPhases = Parser[ActiveActor]->MakeInteger_();
			break;
			default:
			DoSimpleMsg(String("Error: Unknown Parameter on command line: ") + Param, 28701);
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}

     /*Check for Errors*/

     /*If user specified full line name, get rid of "line."*/
	Line1 = StripClassName(Line1);
	Line2 = StripClassName(Line2);
	if((Line1.size() == 0) || (Line2.size() == 0))
	{
		DoSimpleMsg("Both Line1 and Line2 must be specified!", 28702);
		return result;
	}
	if((!LineCodeSpecified) && (!GeometrySpecified))
	{
		DoSimpleMsg("Either a new LineCode or a Geometry must be specified!", 28703);
		return result;
	}
	LineClass = (TLine*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("Line"));
	pLine1 = ((TLineObj*) ( (TDSSClass*) LineClass )->Find(Line1));
	pLine2 = ((TLineObj*) ( (TDSSClass*) LineClass )->Find(Line2));
	if((pLine1 == nullptr) || (pLine2 == nullptr))
	{
		if(pLine1 == nullptr)
			DoSimpleMsg(String("Line.") + Line1 + " not found.", 28704);
		else
		{
			if(pLine2 == nullptr)
				DoSimpleMsg(String("Line.") + Line2 + " not found.", 28704);
		}
		return result;
	}

     /*Now check to make sure they are in the same meter's zone*/
	if((pLine1->MeterObj == nullptr) || (pLine2->MeterObj == nullptr))
	{
		DoSimpleMsg("Error: Both Lines must be in the same EnergyMeter zone. One or both are not in any meter zone.", 28705);
		return result;
	}
	if(pLine1->MeterObj != pLine2->MeterObj)
	{
		DoSimpleMsg(String("Error: Line1 is in EnergyMeter.") + ( (TDSSObject*) pLine1->MeterObj )->get_Name()
	           + " zone while Line2 is in EnergyMeter."
	           + ( (TDSSObject*) pLine2->MeterObj )->get_Name()
	           + " zone. Both must be in the same Zone.", 28706);
		return result;
	}

     /*Since the lines can be given in either order, Have to check to see which direction they are specified and find the path between them*/
	TraceDirection = 0;
	if(IsPathBetween(pLine1, pLine2))
		TraceDirection = 1;
	if(IsPathBetween(pLine2, pLine1))
		TraceDirection = 2;
	if(LineCodeSpecified)
		EditString = String("Linecode=") + LineCode;
	else
		EditString = String("Geometry=") + Geometry;

     // Append MyEditString onto the end of the edit string to change the linecode  or geometry
	EditString = EditString + " " + MyEditString;
	switch(TraceDirection)
	{
		case 	1:
		TraceAndEdit(pLine1, pLine2, NPhases, EditString);
		break;
		case 	2:
		TraceAndEdit(pLine2, pLine1, NPhases, EditString);
		break;
		default:
		DoSimpleMsg("Traceback path not found between Line1 and Line2.", 28707);
		return result;
		break;
	}
	return result;
}

int DoAddMarkerCmd()
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	TBusMarker* BusMarker = nullptr;
	result = 0;
	ParamPointer = 0;
	BusMarker = new TBusMarker();
	ActiveCircuit[ActiveActor]->BusMarkerList.push_back(BusMarker);
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = AddMarkerCommands->Getcommand(ParamName);
		/*# with BusMarker do */
		{
			auto with0 = BusMarker;
			switch(ParamPointer)
			{
				case 	1:
				with0->BusName = Param;
				break;
				case 	2:
				with0->AddMarkerCode = Parser[ActiveActor]->MakeInteger_();
				break;
				case 	3:
				with0->AddMarkerColor = InterpretColorName(Param);
				break;
				case 	4:
				with0->AddMarkerSize = Parser[ActiveActor]->MakeInteger_();
				break;
             // ignore unnamed and extra parms
				default:
				  ;
				break;
			}
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
	return result;
}

int DoSetLoadAndGenKVCmd()
{
	int result = 0;
	TLoadObj* pLoad = nullptr;
	TGeneratorObj* pGen = nullptr;
	TDSSBus* pBus = nullptr;
	String sBus;
	int iBus = 0;
	int i = 0;
	double kvln = 0.0;
	int stop = 0;
	result = 0;
	pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
	while(pLoad != nullptr)
	{
		ActiveLoadObj = pLoad; // for UpdateVoltageBases to work
		sBus = StripExtension( ( (TDSSCktElement*) pLoad )->GetBus(1));
		iBus = ActiveCircuit[ActiveActor]->BusList.Find(sBus);
		pBus = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iBus) - 1];
		kvln = pBus->kVBase;
		if((pLoad->Connection == 1) || ( ( (TDSSCktElement*) pLoad )->Get_NPhases() == 3))
			pLoad->kVLoadBase = kvln * sqrt(3.0L);
		else
			pLoad->kVLoadBase = kvln;
		pLoad->UpdateVoltageBases();
		pLoad->RecalcElementData(ActiveActor);
		pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
	}
	for(stop = ActiveCircuit[ActiveActor]->Generators.get_myNumList(), i = 1; i <= stop; i++)
	{
		pGen = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get(i);
		sBus = StripExtension(pGen->GetBus(1));
		iBus = ActiveCircuit[ActiveActor]->BusList.Find(sBus);
		pBus = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iBus) - 1];
		kvln = pBus->kVBase;
		if((pGen->Connection == 1) || (pGen->Get_NPhases() > 1))
			pGen->Set_PresentkV(kvln * sqrt(3.0L));
		else
			pGen->Set_PresentkV(kvln);
		pGen->RecalcElementData(ActiveActor);
	}
	return result;
}

int DoUuidsCmd()
{
	int result = 0;
	TTextRec f = {};
	String dummy;
	String ParamName;
	String Param;
	String s;
	String NameVal;
	String UuidVal;
	String DevClass;
	String devName;
	TNamedObject* PName = nullptr;
	int Idx = 0;
	StartUuidList(ActiveCircuit[ActiveActor]->NumBuses + 2 * ActiveCircuit[ActiveActor]->NumDevices);
	result = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	try
	{
		AssignFile(f, Param);
		Reset(f);
		IOResultToException();
		AuxParser[ActiveActor]->set_delimchars(",");
		while(!Eof(f))
		{
			ReadLn(f, s);
			/*# with AuxParser[ActiveActor] do */
			{
				auto with36 = AuxParser[ActiveActor];
				PName = nullptr;
				with36->SetCmdString(s);
				dummy = with36->GetNextParam();
				NameVal = with36->MakeString_();
				dummy= with36->GetNextParam();
				UuidVal = with36->MakeString_();
        // format the UUID properly
				if(Pos("{", UuidVal) < 1)
					UuidVal = String("{") + UuidVal + "}";
				if(Pos("=", NameVal) > 0)  // it's a non-identified object in OpenDSS
				{
					AddHashedUUID(NameVal, UuidVal);
				}
				else
  // find this as a descendant of TNamedObject
				{
					PName = nullptr;
					ParseObjectClassandName(NameVal, DevClass, devName);
					if(CompareText(DevClass, "circuit") == 0)
					{
						PName = ActiveCircuit[ActiveActor];
					}
					else
					{
						if(CompareText(DevClass, "Bus") == 0)
						{
							Idx = ActiveCircuit[ActiveActor]->BusList.Find(devName);
							PName = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(Idx) - 1];
						}
						else
						{
							LastClassReferenced[ActiveActor] = ClassNames[ActiveActor].Find(DevClass);
							ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
							if(ActiveDSSClass[ActiveActor] != nullptr)
							{
								if(ActiveDSSClass[ActiveActor]->SetActive(devName))
									PName = ((TNamedObject*) ActiveDSSClass[ActiveActor]->GetActiveObj());
							}
						}
          // re-assign its UUID
					}
					if(PName != nullptr)
						PName->Set_UUID(StringToUUID(UuidVal));
				}
			}
		}
/* }
	__finally
	{*/
		AuxParser[ActiveActor]->ResetDelims();
		CloseFile(f);
	}
	catch (...)
	{
		//
	}
	return result;
}

int DoCvrtLoadshapesCmd()
{
	int result = 0;
	TLoadShapeObj* pLoadshape = nullptr;
	int iLoadshape = 0;
	TLoadShape* LoadShapeClass = nullptr;
	String ParamName;
	String Param;
	String Action;
	TTextRec f = {};
	String FName;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	if(Param.size() == 0)
		Param = "s";

    /*Double file or Single file?*/
	switch(LowerCase(Param)[0])
	{
		case 	L'd':
		Action = "action=dblsave";
		break;
		default:
		Action = "action=sngsave";   // default
		break;
	}
	LoadShapeClass = (TLoadShape*) GetDSSClassPtr("loadshape");
	FName = "ReloadLoadshapes.DSS";
	AssignFile(f, FName);
	Rewrite(f);
	IOResultToException();
	iLoadshape = LoadShapeClass->Get_First();
	while(iLoadshape > 0)
	{
		pLoadshape = ((TLoadShapeObj*) LoadShapeClass->GetActiveObj());
		Parser[ActiveActor]->SetCmdString(Action);
		pLoadshape->Edit(ActiveActor);
		WriteLn(f, "New Loadshape." + pLoadshape->get_Name() + Format("Npts = % d Interval = % .8g ", pLoadshape->get_FNumPoints(), pLoadshape->Interval) + GlobalResult);
		iLoadshape = LoadShapeClass->Get_Next();
	}
	CloseFile(f);
	FireOffEditor(FName);
	result = 0;
	return result;
}

int DoNodeDiffCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	String sNode1;
	String sNode2;
	String sBusName;
	complex V1 = {};
	complex V2 = {};
	complex VNodeDiff = {};
	int iBusidx = 0;
	int B1ref = 0;
	int B2ref = 0;
	int NumNodes = 0;
	int NodeBuffer[50/*# range 1..50*/]{};
	result = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	sNode1 = Param;
	if(Pos("2", ParamName) > 0)
		sNode2 = Param;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	sNode2 = Param;
	if(Pos("1", ParamName) > 0)
		sNode1 = Param;

    // Get first node voltage
	AuxParser[ActiveActor]->set_Token(sNode1);
	NodeBuffer[1 - 1] = 1;
	sBusName = AuxParser[ActiveActor]->ParseAsBusName(NumNodes, (pIntegerArray) NodeBuffer, ActiveActor);
	iBusidx = ActiveCircuit[ActiveActor]->BusList.Find(sBusName);
	if(iBusidx > 0)
	{
		B1ref = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iBusidx) - 1]->Find(NodeBuffer[1 - 1]);
	}
	else
	{
		DoSimpleMsg("Bus %s not found." + sBusName, 28709);
		return result;
	}
	V1 = (ActiveCircuit[ActiveActor]->Solution->NodeV)[B1ref];

    // Get 2nd node voltage
	AuxParser[ActiveActor]->set_Token(sNode2);
	NodeBuffer[1 - 1] = 1;
	sBusName = AuxParser[ActiveActor]->ParseAsBusName(NumNodes, (pIntegerArray) NodeBuffer, ActiveActor);
	iBusidx = ActiveCircuit[ActiveActor]->BusList.Find(sBusName);
	if(iBusidx > 0)
	{
		B2ref = ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iBusidx) - 1]->Find(NodeBuffer[1 - 1]);
	}
	else
	{
		DoSimpleMsg("Bus %s not found." + sBusName, 28710);
		return result;
	}
	V2 = (ActiveCircuit[ActiveActor]->Solution->NodeV)[B2ref];
	VNodeDiff = csub(V1, V2);
	GlobalResult = Format("%.7g, V,    %.7g, deg  ", cabs(VNodeDiff), cdang(VNodeDiff));
	return result;
}

int DoRephaseCmd()
{
	int result = 0;
	String Param;
	String ParamName;
	int ParamPointer = 0;
	String StartLine;
	String NewPhases;
	String MyEditString;
	String ScriptFileName;
	TLineObj* pStartLine = nullptr;
	TLine* LineClass = nullptr;
	bool TransfStop = false;
	result = 0;
	ParamPointer = 0;
	MyEditString = "";
	ScriptFileName = "RephaseEditScript.DSS";
	TransfStop = true;  // Stop at Transformers
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = RephaseCommands->Getcommand(ParamName);
		switch(ParamPointer)
		{
			case 	1:
			StartLine = Param;
			break;
			case 	2:
			NewPhases = Param;
			break;
			case 	3:
			MyEditString = Param;
			break;
			case 	4:
			ScriptFileName = Param;
			break;
			case 	5:
			TransfStop = InterpretYesNo(Param);
			break;
			default:
			DoSimpleMsg(String("Error: Unknown Parameter on command line: ") + Param, 28711);
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
	LineClass = (TLine*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("Line"));
	pStartLine = ((TLineObj*) ( (TDSSClass*) LineClass )->Find(StripClassName(StartLine)));
	if(pStartLine == nullptr)
	{
		DoSimpleMsg(String("Starting Line (") + StartLine + ") not found.", 28712);
		return result;
	}
     /*Check for some error conditions and abort if necessary*/
	if(pStartLine->MeterObj == nullptr)
	{
		DoSimpleMsg("Starting Line must be in an EnergyMeter zone.", 28713);
		return result;
	}
	if(!(dynamic_cast<TEnergyMeterObj*>(pStartLine->MeterObj)))
	{
		DoSimpleMsg("Starting Line must be in an EnergyMeter zone.", 28714);
		return result;
	}
	GoForwardAndRephase(pStartLine, NewPhases, MyEditString, ScriptFileName, TransfStop);
	return result;
}

int DoSetBusXYCmd()
{
	int result = 0;
	String Param;
	String ParamName;
	int ParamPointer = 0;
	String Busname;
	double Xval = 0.0;
	double Yval = 0.0;
	int iB = 0;
	result = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	ParamPointer = 0;
	Xval = 0.0;
	Yval = 0.0;
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = SetBusXYCommands->Getcommand(ParamName);
		switch(ParamPointer)
		{
			case 	1:
			Busname = Param;
			break;
			case 	2:
			Xval = Parser[ActiveActor]->MakeDouble_();
			break;
			case 	3:
			Yval = Parser[ActiveActor]->MakeDouble_();
			break;
			default:
			DoSimpleMsg(String("Error: Unknown Parameter on command line: ") + Param, 28721);
			break;
		}
		iB = ActiveCircuit[ActiveActor]->BusList.Find(Busname);
		if(iB > 0)
		{
			/*# with ActiveCircuit[ActiveActor]->Buses^[iB] do */
			{
				     // Returns TBus object
				ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->x = Xval;
				ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->y = Yval;
				ActiveCircuit[ActiveActor]->Buses[static_cast<size_t>(iB) - 1]->CoordDefined = true;
			}
		}
		else
		{
			DoSimpleMsg(String("Error: Bus \"") + Busname + "\" Not Found.", 28722);
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
	return result;
}

int DoFNCSPubCmd()
{
	int result = 0;
	DoSimpleMsg("Error: FNCS only supported in the Free Pascal version", 28728);
	return result;
}

int DoUpdateStorageCmd()
{
	int result = 0;
	StorageClass[ActiveActor]->UpdateAll(ActiveActor);
	result = 0;
	return result;
}

/*
FUNCTION DoUpdateStorage2Cmd:Integer;

Begin
       StorageClass[ActiveActor].UpdateAll(ActiveActor);
       Result := 0;
End;
*/

int DoPstCalc()
{
	int result = 0;
	String Param;
	String ParamName;
	int ParamPointer = 0;
	int npts = 0;
	pDoubleArray VArray;
	int CyclesPerSample = 0;
	int Lamp = 0;
	pDoubleArray PstArray;
	int nPst = 0;
	int i = 0;
	String s;
	double Freq = 0.0;
	result = 0;
	VArray = NULL;
	PstArray = NULL;
	npts = 0;
	Lamp = 120;  // 120 or 230
	CyclesPerSample = 60;
	Freq = DefaultBaseFreq;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	ParamPointer = 0;
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = PstCalcCommands->Getcommand(ParamName);
         // 'Npts', 'Voltages', 'cycles', 'lamp'
		switch(ParamPointer)
		{
			case 	1:
			{
				npts = Parser[ActiveActor]->MakeInteger_();
				VArray = (pDoubleArray) malloc(sizeof(double) * npts);
			}
			break;
			case 	2:
			npts = InterpretDblArray(Param, npts, VArray);
			break;
			case 	3:
			CyclesPerSample = (int) Round(ActiveCircuit[ActiveActor]->Solution->get_FFrequency() * Parser[ActiveActor]->MakeDouble_());
			break;
			case 	4:
			Freq = Parser[ActiveActor]->MakeDouble_();
			break;
			case 	5:
			Lamp = Parser[ActiveActor]->MakeInteger_();
			break;
			default:
			DoSimpleMsg(String("Error: Unknown Parameter on command line: ") + Param, 28722);
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}
	if(npts > 10)
	{
		int stop = 0;
		nPst = PstRMS(PstArray, VArray, Freq, CyclesPerSample, npts, Lamp);
         // put resulting pst array in the result string
		s = "";
		for(stop = nPst, i = 1; i <= stop; i++)
		{
			s = s + Format("%.8g, ",  PstArray[i - 1]);
		}
		GlobalResult = s;
	}
	else
	DoSimpleMsg("Insuffient number of points for Pst Calculation.", 28723);
	free(VArray);   // discard temp arrays
	free(PstArray);
	return result;
}
int DoLambdaCalcs()
{
	int		result = 0,
			i = 0;
	TEnergyMeterObj* pMeter = nullptr;

	String	ParamName = "",
			Param = "";
	bool AssumeRestoration = false;

	auto with0 = ActiveCircuit[ActiveActor];
	// Do for each Energymeter object in active circuit
	pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_First();
	if (pMeter == nullptr)
	{
		DoSimpleMsg("No EnergyMeter Objects Defined. EnergyMeter objects required for this function.", 28724);
		return result;
	}
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	if (Param.size() > 0)
		AssumeRestoration = InterpretYesNo(Param);
	else
		AssumeRestoration = false;

	// initialize bus quantities

 /*# with ActiveCircuit[ActiveActor] do */
	{
		int stop = 0;
		for (stop = with0->NumBuses, i = 1; i <= stop; i++)
		{
			/*# with ActiveCircuit[ActiveActor]->Buses^[i] do */
			{

				with0->Buses[i - 1]->BusFltRate = 0.0;
				with0->Buses[i - 1]->Bus_Num_Interrupt = 0.0;
			}
		}
	}
	while (pMeter != nullptr)
	{
		pMeter->CalcReliabilityIndices(AssumeRestoration, ActiveActor);
		pMeter = (TEnergyMeterObj*) with0->EnergyMeters.Get_Next();
	}
	return result;
}
/*Execute fault rate and bus number of interruptions calc*/

int DoVarCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	String Str;
	int iVar = 0;
	TStringList* MsgStrings = nullptr;
	result = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	if(Param.size() == 0)  // show all vars
	{
		int stop = 0;
		if(NoFormsAllowed)
			return result;
          /*
          MsgStrings := TStringList.Create;
          MsgStrings.Add('Variable, Value');
          for iVar := 1 to ParserVars.NumVariables  do
              MsgStrings.Add(ParserVars.VarString[iVar] );
          ShowMessageForm(MsgStrings);
          MsgStrings.Free;*/
		Str = String("Variable, Value") + CRLF;
		for(stop = ParserVars->NumVariables, iVar = 1; iVar <= stop; iVar++)
		{
			Str = Str + String(ParserVars->Get_VarString((unsigned int) iVar)) + CRLF;
		}
		DoSimpleMsg(Str, 999345);
	}
	else
	{
		if(ParamName.size() == 0)   // show value of this var
		{
			GlobalResult = Param;  // Parser substitutes @var with value
		}
		else
		{
			while(ParamName.size() > 0)
			{
				switch(ParamName[0])
				{
					case 	L'@':
					ParserVars->Add(ParamName, Param);
					break;
					default:
					DoSimpleMsg(String("Illegal Variable Name: ") + ParamName
	           + "; Must begin with \"@\"", 28725);
					return result;
					break;
				}
				ParamName = Parser[ActiveActor]->GetNextParam();
				Param = Parser[ActiveActor]->MakeString_();
			}
		}
	}
	return result;
}
/*Process Script variables*/

int DoRemoveCmd()
{
	int result = 0;
	String ParamName;
	String Param;
	String Str;
	int i = 0;
	int ParamPointer = 0;
	int DeviceIndex = 0;
	String FElementName;
	bool ElmFound = false;
	bool FKeepLoad = false;
	String FEditString;
	TPDElement* pPDElem = nullptr;
	TEnergyMeterObj* pMeter = nullptr;
	String FMeterName;
	result = 0;
	FKeepLoad = true;
	ParamPointer = 0;
	ParamName = Parser[ActiveActor]->GetNextParam();
	Param = Parser[ActiveActor]->MakeString_();
	while(Param.size() > 0)
	{
		if(ParamName.size() == 0)
			++ParamPointer;
		else
			ParamPointer = RemoveCommands->Getcommand(ParamName);
		switch(ParamPointer)
		{
			case 	1:
			FElementName = Param;
			break; /*ElementName*/
			case 	2:
			FKeepLoad = InterpretYesNo(Param);
			break; /*KeepLoad*/
			case 	3:
			FEditString = Param;
			break; /*EditString*/
			default:
			  ;
			break;
		}
		ParamName = Parser[ActiveActor]->GetNextParam();
		Param = Parser[ActiveActor]->MakeString_();
	}

     // Check for existence of FelementName
	DeviceIndex = GetCktElementIndex(FElementName);
	if(DeviceIndex == 0)
	{
		DoSimpleMsg(String("Error: Element ") + FElementName + " does not exist in this circuit.", 28726);
	}
	else
 // Element exists  GO!
     // first, checks if the element is not linked to an energy meter, if it does, abort (added 01/06/2020 -DM)
	{
		ElmFound = false;
		if(ActiveCircuit[ActiveActor] != nullptr)
			/*# with ActiveCircuit[ActiveActor] do */
			{
				
				int stop = 0;
				pMeter = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
				for(stop = ActiveCircuit[ActiveActor]->EnergyMeters.get_myNumList(), i = 1; i <= stop; i++)
				{
					if(AnsiLowerCase(pMeter->ElementName) == AnsiLowerCase(FElementName))
					{
						ElmFound = true;
						break;
					}
					else
					pMeter = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_Next();
				}
			}
		if(!ElmFound)
        // Set CktElement active
		{
			SetObject(FElementName);

        // Get Energymeter associated with this element.
			if(dynamic_cast<TPDElement*>(ActiveCircuit[ActiveActor]->get_FActiveCktElement()))
			{
				pPDElem = (TPDElement*) ActiveCircuit[ActiveActor]->get_FActiveCktElement();
				if(pPDElem->SensorObj == nullptr)
					DoSimpleMsg("Element " + ((TDSSObject*)pPDElem)->ParentClass->get_myClass_name() + 
						"." + ((TDSSObject*)pPDElem)->get_Name() + " is not in a meter zone!Add an Energymeter. ", 287261);
				else
				{
					FMeterName = ((TDSSObject*)pPDElem->SensorObj)->ParentClass->get_myClass_name() + "." +( (TDSSObject*) pPDElem->SensorObj )->get_Name();
					SetObject(FMeterName);
					if(dynamic_cast<TEnergyMeterObj*>(ActiveCircuit[ActiveActor]->get_FActiveCktElement()))
					{
						pMeter = (TEnergyMeterObj*) (void*) ActiveCircuit[ActiveActor]->get_FActiveCktElement();
                  // in ReduceAlgs
						DoRemoveBranches(pMeter->BranchList, pPDElem, FKeepLoad, FEditString);
					}
					else
					DoSimpleMsg(String("Error: The Sensor Object for ") + FElementName
	           + " is not an EnergyMeter object", 28727);
				}
			}
			else
			DoSimpleMsg(String("Error: Element ") + FElementName
	           + " is not a power delivery element (PDElement)", 28728);
		}
		else
		DoSimpleMsg(String("Error: Element ") + FElementName + " is tied to an Energy Meter.", 28800);
	}
	return result;
}

/*Initialize Command lists*/


void ExecHelper_initialization()
{
	std::string temp[4] = { "class", "file", "dir", "keepdisabled" };
	SaveCommands = new TCommandList(temp, 4);
	SaveCommands->set_AbbrevAllowed(true);

	std::string temp1[5] = { "case", "year", "registers", "peak", "meter" };
	DI_PlotCommands = new TCommandList(temp1, 5);

	std::string temp2[7] = { "kW", "how", "skip", "pf", "file", "MW", "what" };
	DistributeCommands = new TCommandList(temp2, 7);
	DistributeCommands->set_AbbrevAllowed(true);

	std::string temp3[6] = { "Line1", "Line2", "LineCode", "Geometry", "EditString", "Nphases" };
	ReconductorCommands = new TCommandList(temp3, 6);
	ReconductorCommands->set_AbbrevAllowed(true);

	std::string temp4[5] = { "StartLine", "PhaseDesignation", "EditString", "ScriptFileName", "StopAtTransformers" };
	RephaseCommands = new TCommandList(temp4, 5);
	RephaseCommands->set_AbbrevAllowed(true);

	std::string temp5[4] = { "Bus", "code", "color", "size" };
	AddMarkerCommands = new TCommandList(temp5, 4);
	AddMarkerCommands->set_AbbrevAllowed(true);

	std::string temp6[3] = { "Bus", "x", "y" };
	SetBusXYCommands = new TCommandList(temp6, 3);
	SetBusXYCommands->set_AbbrevAllowed(true);

	std::string temp7[5] = { "Npts", "Voltages", "dt", "Frequency", "lamp" };
	PstCalcCommands = new TCommandList(temp7, 5);
	PstCalcCommands->set_AbbrevAllowed(true);

	std::string temp8[1] = { "Fname" };
	FNCSPubCommands = new TCommandList(temp8, 1);
	FNCSPubCommands->set_AbbrevAllowed(true);

	std::string temp9[3] = { "ElementName", "KeepLoad", "Editstring" };
	RemoveCommands = new TCommandList(temp9, 3);
	RemoveCommands->set_AbbrevAllowed(true);
}

void ExecHelper_finalization()
{
	delete DistributeCommands;
	delete DI_PlotCommands;
	delete SaveCommands;
	delete AddMarkerCommands;
	delete ReconductorCommands;
	delete RephaseCommands;
	delete SetBusXYCommands;
	delete PstCalcCommands;
	delete FNCSPubCommands;
	delete RemoveCommands;
}

		class 		ExecHelper_unit
		{
		public:
		ExecHelper_unit()
		{
			//AssertSystemInitialization();
			ExecHelper_initialization();
		}
		~		ExecHelper_unit(){ExecHelper_finalization(); }
		};
		ExecHelper_unit _ExecHelper_unit;

}  // namespace ExecHelper




