
#pragma hdrstop

#ifndef _WIN32
//#include <sys/stat.h>
//#include <fcntl.h>
//#include <unistd.h>
#include <sys/mman.h>
#endif

#include "LoadShape.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "mathutil.h"
#include "Utilities.h"
//#include "../Common/TOPExport.h"
#include <math.h>
#include "PointerList.h"


using namespace std;
using namespace Arraydef;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace PointerList;
using namespace System;
//using namespace TOPExport;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace LoadShape
{

TLoadShapeObj::TLoadShapeObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TLoadShapeObj::TLoadShapeObj(string ClassName) : inherited(ClassName) {}
TLoadShapeObj::TLoadShapeObj() {}


TLoadShapeObj* ActiveLoadShapeObj = nullptr;
const int NumPropsThisClass = 22;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TLoadShape::TLoadShape()
{
	;
	Class_Name = "LoadShape";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLoadShape::~TLoadShape()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLoadShape::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "npts";           // Number of points to expect
	PropertyName[2 - 1] = "interval";       // default = 1.0;
	PropertyName[3 - 1] = "mult";           // vector of power multiplier values
	PropertyName[4 - 1] = "hour";           // vextor of hour values
	PropertyName[5 - 1] = "mean";           // set the mean (otherwise computed)
	PropertyName[6 - 1] = "stddev";         // set the std dev (otherwise computed)
	PropertyName[7 - 1] = "csvfile";        // Switch input to a csvfile
	PropertyName[8 - 1] = "sngfile";        // switch input to a binary file of singles
	PropertyName[9 - 1] = "dblfile";        // switch input to a binary file of singles
	PropertyName[10 - 1] = "action";        // actions  Normalize
	PropertyName[11 - 1] = "qmult";         // Q multiplier
	PropertyName[12 - 1] = "UseActual";     // Flag to signify to use actual value
	PropertyName[13 - 1] = "Pmax";          // MaxP value
	PropertyName[14 - 1] = "Qmax";          // MaxQ
	PropertyName[15 - 1] = "sinterval";     // Interval in seconds
	PropertyName[16 - 1] = "minterval";     // Interval in minutes
	PropertyName[17 - 1] = "Pbase";         // for normalization, use peak if 0
	PropertyName[18 - 1] = "Qbase";         // for normalization, use peak if 0
	PropertyName[19 - 1] = "Pmult";         // synonym for Mult
	PropertyName[20 - 1] = "PQCSVFile";     // Redirect to a file with p, q pairs
	PropertyName[21 - 1] = "MemoryMapping"; // Enable/disable using Memory mapping for this shape
	PropertyName[22 - 1] = "mode";
     // define Property help values
	PropertyHelp[1 - 1] = "Max number of points to expect in load shape vectors. This gets reset to the number of multiplier values found (in files only) if less than specified.";     // Number of points to expect
	PropertyHelp[2 - 1] = String("Time interval for fixed interval data, hrs. Default = 1. " "If Interval = 0 then time data (in hours) may be at either regular or  irregular intervals and time value must be specified using either the Hour property or input files. " "Then values are interpolated when Interval=0, but not for fixed interval data.  ") + CRLF
	           + CRLF
	           + "See also \"sinterval\" and \"minterval\"."; // default = 1.0;
	PropertyHelp[3 - 1] = String("Array of multiplier values for active power (P) or other key value (such as pu V for Vsource). ") + CRLF
	           + CRLF
	           + "You can also use the syntax: "
	           + CRLF
	           + CRLF
	           + "mult = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "mult = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "mult = (sngfile=filename)  !for packed file of singles "
	           + CRLF
	           + "mult = (file=MyCSVFile.CSV, col=3, header=yes)  !for multicolumn CSV files "
	           + CRLF
	           + CRLF
	           + "Note: this property will reset Npts if the  number of values in the files are fewer."
	           + CRLF
	           + CRLF
	           + "Same as Pmult";     // vector of power multiplier values
	PropertyHelp[4 - 1] = String("Array of hour values. Only necessary to define for variable interval data (Interval=0)." " If you set Interval>0 to denote fixed interval data, DO NOT USE THIS PROPERTY. " "You can also use the syntax: ") + CRLF
	           + "hour = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "hour = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "hour = (sngfile=filename)  !for packed file of singles ";     // vextor of hour values
	PropertyHelp[5 - 1] = "Mean of the active power multipliers.  This is computed on demand the first time a "
	           "value is needed.  However, you may set it to another value independently. "
	           "Used for Monte Carlo load simulations.";     // set the mean (otherwise computed)
	PropertyHelp[6 - 1] = String("Standard deviation of active power multipliers.  This is computed on demand the first time a " "value is needed.  However, you may set it to another value independently." "Is overwritten if you subsequently read in a curve") + CRLF
	           + CRLF
	           + "Used for Monte Carlo load simulations.";   // set the std dev (otherwise computed)
	PropertyHelp[7 - 1] = "Switch input of active power load curve data to a CSV text file "
	           "containing (hour, mult) points, or simply (mult) values for fixed time interval data, one per line. "
	           "NOTE: This action may reset the number of points to a lower value.";   // Switch input to a csvfile
	PropertyHelp[8 - 1] = "Switch input of active power load curve data to a binary file of singles "
	           "containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. "
	           "NOTE: This action may reset the number of points to a lower value.";  // switch input to a binary file of singles
	PropertyHelp[9 - 1] = "Switch input of active power load curve data to a binary file of doubles "
	           "containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. "
	           "NOTE: This action may reset the number of points to a lower value.";   // switch input to a binary file of singles
	PropertyHelp[10 - 1] = String("{NORMALIZE | DblSave | SngSave} After defining load curve data, setting action=normalize " "will modify the multipliers so that the peak is 1.0. " "The mean and std deviation are recomputed.") + CRLF
	           + CRLF
	           + "Setting action=DblSave or SngSave will cause the present mult and qmult values to be written to "
	           + "either a packed file of double or single. The filename is the loadshape name. The mult array will have a "
	           + "\"_P\" appended on the file name and the qmult array, if it exists, will have \"_Q\" appended."; // Action
     // vector of qmultiplier values
	PropertyHelp[11 - 1] = String("Array of multiplier values for reactive power (Q).  You can also use the syntax: ") + CRLF
	           + "qmult = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "qmult = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "qmult = (sngfile=filename)  !for packed file of singles "
	           + CRLF
	           + "qmult = (file=MyCSVFile.CSV, col=4, header=yes)  !for multicolumn CSV files ";
	PropertyHelp[12 - 1] = "{Yes | No* | True | False*} If true, signifies to Load, Generator, Vsource, or other objects to "
	           "use the return value as the actual kW, kvar, kV, or other value rather than a multiplier. "
	           "Nominally for AMI Load data but may be used for other functions.";
	PropertyHelp[13 - 1] = "kW value at the time of max power. Is automatically set upon reading in a loadshape. "
	           "Use this property to override the value automatically computed or to retrieve the value computed.";
	PropertyHelp[14 - 1] = "kvar value at the time of max kW power. Is automatically set upon reading in a loadshape. "
	           "Use this property to override the value automatically computed or to retrieve the value computed.";
	PropertyHelp[15 - 1] = "Specify fixed interval in SECONDS. Alternate way to specify Interval property.";
	PropertyHelp[16 - 1] = "Specify fixed interval in MINUTES. Alternate way to specify Interval property.";
	PropertyHelp[17 - 1] = "Base P value for normalization. Default is zero, meaning the peak will be used.";
	PropertyHelp[18 - 1] = "Base Q value for normalization. Default is zero, meaning the peak will be used.";
	PropertyHelp[19 - 1] = "Synonym for \"mult\".";
	PropertyHelp[20 - 1] = String("Switch input to a CSV text file containing (active, reactive) power (P, Q) multiplier pairs, one per row. ") + CRLF
	           + "If the interval=0, there should be 3 items on each line: (hour, Pmult, Qmult)";
	PropertyHelp[21 - 1] = String("{Yes | No* | True | False*} Enables the memory mapping functionality for dealing with large amounts of load shapes. ") + CRLF
	           + "By defaul is False. Use it to accelerate the model loading when the containing a large number of load shapes.";
	PropertyHelp[22 - 1] = String("{carryover | default} carryover will initialize generator dispatch from latest Pgen/Qgen in memory.  default will use kWbase ") + CRLF
	           + "and kvarbase to initialize generator dispatch.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLoadShape::NewObject(const String ObjName)
{
	int result = 0;
  // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TLoadShapeObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


/********************************************************************************
*      Loads the mapped file features into local variables for further use     *
*********************************************************************************/
void TLoadShapeObj::LoadFileFeatures(int ShapeType)
{
	int LocalCol = 0;
	int myType = 0;
	String Parmname;
	String Param;
	AuxParser[ActiveActor]->SetCmdString(myFileCmd);
	Parmname = AuxParser[ActiveActor]->GetNextParam();
	Param = AuxParser[ActiveActor]->MakeString_();
	LocalCol = 1;
	if(CompareText(Parmname, "file") == 0)
     /*Default values*/
	{
		myType = 0;
     // Default options

    // Look for other options  (may be in either order)
		Parmname = AuxParser[ActiveActor]->GetNextParam();
		Param = AuxParser[ActiveActor]->MakeString_();
		while(Param.length() > 0)
		{
			if(CompareTextShortest(Parmname, "column") == 0)
				LocalCol = AuxParser[ActiveActor]->MakeInteger_();
			Parmname = AuxParser[ActiveActor]->GetNextParam();
			Param = AuxParser[ActiveActor]->MakeString_();
		}
	}
	else
	{
		if(CompareText(Parmname, "dblfile") == 0)
			myType = 1;
		else
		{
			if(CompareText(Parmname, "sngfile") == 0)
				myType = 2;
		}
	}
	if(ShapeType == 0) // P
	{
		myFileType = myType;
		myColumn = LocalCol;
	}
	else
	{
		myFileTypeQ = myType;
		myColumnQ = LocalCol;
	}
}

/********************************************************************************
*         Uploads the active MMF view into memory for further use              *
*********************************************************************************/
// Destination
//  0   : P
//  1   : Q

void TLoadShapeObj::LoadMMFView(const String Parmname, int Destination)
{
	int FirstPos = 0;
	unsigned char myLastCh = 0;
  // processes the view depending on the file type
	FirstPos = 1;
	if(Destination == 0)
	{
		if(CompareText(Parmname, "file") == 0) // starndard csv file
		{
			myLastCh = myView[FirstPos];
			while(myLastCh != 0x0A)
			{
				++FirstPos;
				myLastCh = myView[FirstPos];
			}
			myLineLen = FirstPos + 1;
    // DBL file
		}
		else
		{
			if((Parmname.length() > 0) && (CompareTextShortest(Parmname, "dblfile") == 0))
    // SGL file
				myLineLen = sizeof(double);
			else
			{
				if((Parmname.length() > 0) && (CompareTextShortest(Parmname, "sngfile") == 0))
					myLineLen = sizeof(float);
			}
		}
	}
	else
	{
		if(CompareText(Parmname, "file") == 0) // starndard csv file
		{
			myLastCh = myViewQ[FirstPos];
			while(myLastCh != 0x0A)
			{
				++FirstPos;
				myLastCh = myViewQ[FirstPos];
			}
			myLineLenQ = FirstPos + 1;
    // DBL file
		}
		else
		{
			if((Parmname.length() > 0) && (CompareTextShortest(Parmname, "dblfile") == 0))
    // SGL file
				myLineLenQ = sizeof(double);
			else
			{
				if((Parmname.length() > 0) && (CompareTextShortest(Parmname, "sngfile") == 0))
					myLineLenQ = sizeof(float);
			}
		}
	}
}

/********************************************************************************
*   Creates the Memory mapping for the file specified, Destination is used to  *
*   Indicate the destinaton (0 = P, 1 = Q)                                     *
*********************************************************************************/

int TLoadShape::CreateMMF(const String s, int Destination)
{
	int result = 0;
	String Parmname;
	String Param;
	unsigned char myLastCh = 0;
	int i = 0;
	/*# with ActiveLoadShapeObj do */
	{
		auto with0 = ActiveLoadShapeObj;
		try
		{
			AuxParser[ActiveActor]->SetCmdString(s);
			Parmname = AuxParser[ActiveActor]->GetNextParam();
			Param = AuxParser[ActiveActor]->MakeString_();
			if(FileExists(Param))
			{
				if(Destination == 0)
          // Creating mapping for P
          // Opens the file for this instance
				{
#ifdef _WIN32
					with0->myFile = CreateFile(Param.c_str(), GENERIC_READ, (DWORD) FILE_SHARE_READ, nullptr, (DWORD) OPEN_EXISTING, (DWORD) FILE_ATTRIBUTE_NORMAL, 0);
          // Creates the memory map for the file
					with0->myMMF = CreateFileMapping(with0->myFile, nullptr, (DWORD) PAGE_READONLY, 0, 0, nullptr);
					with0->myFileSize = GetFileSize(with0->myFile, nullptr);
					with0->MyViewLen = (int) with0->myFileSize;
					with0->myView = ((PByte) MapViewOfFile(with0->myMMF, (DWORD) FILE_MAP_READ, 0, 0, (SIZE_T) with0->MyViewLen));
#else
					with0->myFile = open(Param.c_str(), O_RDONLY, 0);
					with0->myFileSize = lseek(with0->myFile, 0, SEEK_END);
					(void) lseek(with0->myFile, 0, SEEK_SET);
					with0->MyViewLen = (int) with0->myFileSize;
					with0->myView = (PByte) mmap(nullptr, with0->myFileSize, PROT_READ, MAP_SHARED, with0->myFile, 0);
#endif
					with0->myFileCmd = s;
				}
				else

          // Creating mapping for Q
          // Opens the file for this instance
				{
#ifdef _WIN32					
					with0->myQFile = CreateFile(Param.c_str(), GENERIC_READ, (DWORD) FILE_SHARE_READ, nullptr, (DWORD) OPEN_EXISTING, (DWORD) FILE_ATTRIBUTE_NORMAL, 0);
          // Creates the memory map for the file
					with0->myQMMF = CreateFileMapping(with0->myQFile, nullptr, (DWORD) PAGE_READONLY, 0, 0, nullptr);
					with0->myFileSizeQ = GetFileSize(with0->myFile, nullptr);
					with0->MyViewLenQ = (int) with0->myFileSizeQ;
					with0->myViewQ = ((PByte) MapViewOfFile(with0->myQMMF, (DWORD) FILE_MAP_READ, 0, 0, (SIZE_T) with0->MyViewLenQ));
#else
					with0->myQFile = open(Param.c_str(), O_RDONLY, 0);
					with0->myFileSizeQ = lseek(with0->myQFile, 0, SEEK_END);
					(void) lseek(with0->myQFile, 0, SEEK_SET);
					with0->MyViewLenQ = (int) with0->myFileSizeQ;
					with0->myViewQ = (PByte) mmap(nullptr, with0->myFileSizeQ, PROT_READ, MAP_SHARED, with0->myQFile, 0);
#endif		
					with0->myFileCmdQ = s;
				}
				with0->LoadMMFView(Parmname, Destination);
				result = 0;
			}
			else
			{
				DoSimpleMsg("The file "+ Param + " does not exist. Process cancelled.", 800002);
				result = -1;
			}
		}
		catch(...)
		{
			DoSimpleMsg("There was a proble mapping file " + Param + ". Process cancelled.", 800001);
			result = -1;
		}
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLoadShape::Edit(int ActorID)
{
	int result = 0;
	int MMFError = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveLoadShapeObj = ((TLoadShapeObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveLoadShapeObj;
	/*# with ActiveLoadShapeObj do */
	{
		auto with0 = ActiveLoadShapeObj;
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
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 610);
				break;
				case 	1:
				with0->Set_NumPoints( Parser[ActorID]->MakeInteger_());
				break;
				case 	2:
				with0->Interval = Parser[ActorID]->MakeDouble_();
				break;
				case 	3:
				{
					if(with0->UseMMF)      // A different procedure if the user is working with MMF
					{
						MMFError = CreateMMF(Param, 0); // Creates MMF for P
						if(MMFError == 0)
						{
							with0->LoadFileFeatures(0);
							with0->myDataSize = with0->get_FNumPoints();
							with0->PMultipliers.resize(2);
						}
					}
					else
               // Otherwise, follow the traditional technique for loading up load shapes
					{
						with0->PMultipliers.resize(with0->get_FNumPoints());
                   // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
						with0->Set_NumPoints( InterpretDblArray(Param, with0->get_FNumPoints(), &(with0->PMultipliers[0])));   // Parser.ParseAsVector(Npts, Multipliers);
					}
				}
				break;
				case 	4:
				{
					with0->Hours.resize(with0->get_FNumPoints());
					with0->Set_NumPoints( InterpretDblArray(Param, with0->get_FNumPoints(), &(with0->Hours[0])));   // Parser.ParseAsVector(Npts, Hours);
					with0->Interval = 0.0;
				}
				break;
				case 	5:
				with0->Set_Mean(Parser[ActorID]->MakeDouble_());
				break;
				case 	6:
				with0->Set_StdDev(Parser[ActorID]->MakeDouble_());
				break;
				case 	7:
				DoCSVFile(Param);
				break;
				case 	8:
				DoSngFile(Param);
				break;
				case 	9:
				DoDblFile(Param);
				break;
				case 	10:
				switch(LowerCase(Param)[0])
				{
					case 	L'n':
					with0->Normalize();
					break;
					case 	L'd':
					with0->SaveToDblFile();
					break;
					case 	L's':
					with0->SaveToSngFile();
					break;
					default:
					  ;
					break;
				}
				break;
				case 	11:
				{
					if(with0->UseMMF)      // A different procedure if the user is working with MMF
					{
						MMFError = CreateMMF(Param, 1);  // Creates MMF for Q
						if(MMFError == 0)
						{
							with0->LoadFileFeatures(1);
							if(!with0->PMultipliers.empty())
								with0->myDataSizeQ = with0->myDataSize;
							else
								with0->myDataSizeQ = with0->get_FNumPoints();
							with0->QMultipliers.resize(2);
						}
					}
					else
               // Otherwise, follow the traditional technique for loading up load shapes
					{
						with0->QMultipliers.resize(with0->get_FNumPoints());
						with0->Set_NumPoints( InterpretDblArray(Param, with0->get_FNumPoints(), &(with0->QMultipliers[0])));   // Parser.ParseAsVector(Npts, Multipliers);
					}
				}
				break;
				case 	12:
				with0->UseActual = InterpretYesNo(Param);
				break;
				case 	13:
				with0->MaxP = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->MaxQ = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->Interval = Parser[ActorID]->MakeDouble_() / 3600.0;
				break;  // Convert seconds to hr
				case 	16:
				with0->Interval = Parser[ActorID]->MakeDouble_() / 60.0;
				break;  // Convert minutes to hr
				case 	17:
				with0->BaseP = Parser[ActorID]->MakeDouble_();
				break;
				case 	18:
				with0->BaseQ = Parser[ActorID]->MakeDouble_();
				break;   // same as mult
				case 	19:
				{
					if(with0->UseMMF)      // A different procedure if the user is working with MMF
					{
						MMFError = CreateMMF(Param, 0); // Creates MMF for P
						if(MMFError == 0)
						{
							with0->LoadFileFeatures(0);
							with0->myDataSize = with0->get_FNumPoints();
							with0->PMultipliers.resize(2);
						}
					}
					else
               // Otherwise, follow the traditional technique for loading up load shapes
					{
						with0->PMultipliers.resize(with0->get_FNumPoints());
                   // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
						with0->Set_NumPoints( InterpretDblArray(Param, with0->get_FNumPoints(), &(with0->PMultipliers[0])));   // Parser.ParseAsVector(Npts, Multipliers);
					}
				}
				break;
				case 	20:
				Do2ColCSVFile(Param);
				break;
				case 	21:
				with0->UseMMF = InterpretYesNo(Param);
				break;
				case 22:
				with0->mode = Parser[ActorID]->MakeString_();
				break;


           // Inherited parameters
				default:
				ClassEdit(ActiveLoadShapeObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	3: case 7: case 8: case 9: case 11:
				{
					with0->FStdDevCalculated = false;   // now calculated on demand
					with0->ArrayPropertyIndex = ParamPointer;
					with0->Set_NumPoints(with0->FNumPoints);  // Keep Properties in order for save command
				}
				break;
				case 	14:
				with0->MaxQSpecified = true;
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		} /*WHILE*/
		if(!with0->PMultipliers.empty())
			with0->SetMaxPandQ();
	} /*WITH*/
	return result;
}

void* TLoadShape::Find(const String ObjName)
{
	void* result = nullptr;
	if((ObjName.length() == 0) || (CompareText(ObjName, "none") == 0))
		result = nullptr;
	else
		result = inherited::Find(ObjName);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLoadShape::MakeLike(const String ShapeName)
{
	int result = 0;
	TLoadShapeObj* OtherLoadShape = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherLoadShape = ((TLoadShapeObj*) Find(ShapeName));
	if(OtherLoadShape != nullptr)
		/*# with ActiveLoadShapeObj do */
		{
			auto with0 = ActiveLoadShapeObj;
			int stop = 0;
			with0->Set_NumPoints(OtherLoadShape->get_FNumPoints());
			with0->Interval = OtherLoadShape->Interval;
			with0->PMultipliers.resize(with0->get_FNumPoints());
			for(stop = with0->get_FNumPoints(), i = 1; i <= stop; i++)
			{
				(with0->PMultipliers)[i - 1] = (OtherLoadShape->PMultipliers)[i - 1];
			}
			if(!OtherLoadShape->QMultipliers.empty())
			{
				int stop = 0;
				with0->QMultipliers.resize(with0->get_FNumPoints());
				for(stop = with0->get_FNumPoints(), i = 1; i <= stop; i++)
				{
					(with0->QMultipliers)[i - 1] = (OtherLoadShape->QMultipliers)[i - 1];
				}
			}
			if(with0->Interval > 0.0)
				with0->Hours.clear();
			else
			{
				int stop = 0;
				with0->Hours.resize(with0->get_FNumPoints());
				for(stop = with0->get_FNumPoints(), i = 1; i <= stop; i++)
				{
					(with0->Hours)[i - 1] = (OtherLoadShape->Hours)[i - 1];
				}
			}
			with0->SetMaxPandQ();
			with0->UseActual = OtherLoadShape->UseActual;
			with0->UseMMF = OtherLoadShape->UseMMF;
			with0->BaseP = OtherLoadShape->BaseP;
			with0->BaseQ = OtherLoadShape->BaseQ;


       /* MaxP :=  OtherLoadShape.MaxP;
        MaxQ :=  OtherLoadShape.MaxQ;
        Mean :=  OtherLoadShape.Mean;
        StdDev := OtherLoadShape.StdDev;
       */
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherLoadShape->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in LoadShape MakeLike: \"") + ShapeName
	           + "\" Not Found.", 611);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLoadShape::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TLoadShape.Init", -1);
	result = 0;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Returns active line code string

String TLoadShape::Get_Code()
{
	String result;
	TLoadShapeObj* LoadShapeObj = nullptr;
	LoadShapeObj = ((TLoadShapeObj*) ElementList.Get_Active());
	result = LoadShapeObj->get_Name();
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // sets the  active LoadShape

void TLoadShape::Set_Code(const String Value)
{
	TLoadShapeObj* LoadShapeObj = nullptr;
	ActiveLoadShapeObj = nullptr;
	LoadShapeObj = ((TLoadShapeObj*) ElementList.Get_First());
	while(LoadShapeObj != nullptr)
	{
		if(CompareText(LoadShapeObj->get_Name(), Value) == 0)
		{
			ActiveLoadShapeObj = LoadShapeObj;
			return;
		}
		LoadShapeObj = ((TLoadShapeObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("LoadShape: \"") + Value + "\" not Found.", 612);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/*
   Process 2-column CSV file (3-col if time expected)
*/

void TLoadShape::Do2ColCSVFile(const String FileName)
{
	System::TTextRec f = {};
	int MMFError = 0;
	int i = 0;
	String dummy, s;
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 613);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	/*# with ActiveLoadShapeObj do */
	{
		auto with0 = ActiveLoadShapeObj;
		if(with0->UseMMF)      // A different procedure if the user is working with MMF
		{
			System::CloseFile(f);
			with0->myDataSize = with0->get_FNumPoints();
			with0->myFileCmd = String("file=") + FileName + " column=1";      // Command for P
			MMFError = CreateMMF(with0->myFileCmd, 0);               // Creates MMF for the whole file
			with0->myViewQ = with0->myView;
			if(MMFError == 0)
			{
				with0->LoadFileFeatures(0);                                             // Features for P
				with0->myFileCmd = String("file=") + FileName + " column=2";      // Command for Q
				with0->LoadFileFeatures(1);                                             // Features for Q
				with0->myDataSize = with0->get_FNumPoints();
				with0->myLineLenQ = with0->myLineLen;
				with0->PMultipliers.resize(2);
				with0->QMultipliers.resize(2);
			}
		}
		else
		{
			try

        // Allocate both P and Q multipliers
			{
				with0->PMultipliers.resize(with0->get_FNumPoints());
				with0->QMultipliers.resize(with0->get_FNumPoints());
				if(with0->Interval == 0.0)
					with0->Hours.resize(with0->get_FNumPoints());
				i = 0;
				while((!Eof(f)) && (i < with0->FNumPoints))
				{
					++i;
					System::ReadLn(f, s); // read entire line  and parse with AuxParser
          /*AuxParser allows commas or white space*/
					/*# with AuxParser[ActiveActor] do */
					{
						auto with1 = AuxParser[ActiveActor];
						with1->SetCmdString(s);
						if(with0->Interval == 0.0)
						{
							dummy = with1->GetNextParam();
							(with0->Hours)[i - 1] = with1->MakeDouble_();
						}
						dummy = with1->GetNextParam();
						(with0->PMultipliers)[i - 1] = with1->MakeDouble_();  // first parm
						dummy = with1->GetNextParam();
						(with0->QMultipliers)[i - 1] = with1->MakeDouble_();  // second parm
					}
				}
				System::CloseFile(f);
				if(i != with0->get_FNumPoints())
					with0->Set_NumPoints(i);
			}
			catch(exception& e)
			{
				{
					DoSimpleMsg(String("Error Processing CSV File: \"") + FileName
	           + ". "
	           + (string) e.what(), 614);
					System::CloseFile(f); System::InOutRes = 0;
					return;
				}
			}
		}
	}
}

void TLoadShape::DoCSVFile(const String FileName)
{
	int MMFError = 0;
	int i = 0;
	String dummy, s;
	System::TTextRec f = {};
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 613);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	/*# with ActiveLoadShapeObj do */
	{
		auto with0 = ActiveLoadShapeObj;
		if(with0->UseMMF)      // A different procedure if the user is working with MMF
		{
			CloseFile(f);
			s = String("file=") + FileName;
			MMFError = CreateMMF(s, 0); // Creates MMF for P
			if(MMFError == 0)
			{
				with0->LoadFileFeatures(0);
				with0->myDataSize = with0->get_FNumPoints();
				with0->PMultipliers.resize(2);
			}
		}
		else
		{
			try
			{
				with0->PMultipliers.resize(with0->get_FNumPoints());
				if(with0->Interval == 0.0)
					with0->Hours.resize(with0->get_FNumPoints());
				i = 0;
				while((!Eof(f)) && (i < with0->FNumPoints))
				{
					++i;
					System::ReadLn(f, s); // read entire line  and parse with AuxParser
          /*AuxParser allows commas or white space*/
					/*# with AuxParser[ActiveActor] do */
					{
						auto with1 = AuxParser[ActiveActor];
						with1->SetCmdString(s);
						if(with0->Interval == 0.0)
						{
							dummy = with1->GetNextParam();
							(with0->Hours)[i - 1] = with1->MakeDouble_();
						}
						dummy = with1->GetNextParam();
						(with0->PMultipliers)[i - 1] = with1->MakeDouble_();
					}
				}
				System::CloseFile(f);
				if(i != with0->get_FNumPoints())
					with0->Set_NumPoints(i);
			}
			catch(exception& e)
			{
				{
					DoSimpleMsg(String("Error Processing CSV File: \"") + FileName
	           + ". "
	           + (string) e.what(), 614);
					System::CloseFile(f); System::InOutRes = 0;
					return;
				}
			}
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLoadShape::DoSngFile(const String FileName)
{
	String s;
	System::TTypedFile<float> f;
	float hr = 0.0F;
	float m = 0.0F;
	int MMFError = 0;
	int i = 0;
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 615);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	/*# with ActiveLoadShapeObj do */
	{
		auto with0 = ActiveLoadShapeObj;
		if(with0->UseMMF)      // A different procedure if the user is working with MMF
		{
			System::CloseFile(f);
			s = String("sngfile=") + FileName;
			MMFError = CreateMMF(s, 0); // Creates MMF for P
			if(MMFError == 0)
			{
				with0->LoadFileFeatures(0);
				with0->myDataSize = with0->get_FNumPoints();
				with0->PMultipliers.resize(2);
			}
		}
		else
		{
			try
			{
				with0->PMultipliers.resize(with0->get_FNumPoints());
				if(with0->Interval == 0.0)
					with0->Hours.resize(with0->get_FNumPoints());
				i = 0;
				while((!Eof(f)) && (i < with0->FNumPoints))
				{
					++i;
					if(with0->Interval == 0.0)
					{
						System::Read(f, &hr);
						(with0->Hours)[i - 1] = hr;
					}
					System::Read(f, &m);
					(with0->PMultipliers)[i - 1] = m;
				}
				System::CloseFile(f);
				if(i != with0->FNumPoints)
					with0->Set_NumPoints(i);
			}
			catch(...)
			{
				DoSimpleMsg(String("Error Processing LoadShape File: \"") + FileName, 616);
				System::CloseFile(f); System::InOutRes = 0;
				return;
			}
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLoadShape::DoDblFile(const String FileName)
{
	String s;
	System::TTypedFile<double> f;
	int MMFError = 0;
	int i = 0;
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 617);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	/*# with ActiveLoadShapeObj do */
	{
		auto with0 = ActiveLoadShapeObj;
		if(with0->UseMMF)      // A different procedure if the user is working with MMF
		{
			System::CloseFile(f);
			s = String("dblfile=") + FileName;
			MMFError = CreateMMF(s, 0); // Creates MMF for P
			if(MMFError == 0)
			{
				with0->LoadFileFeatures(0);
				with0->myDataSize = with0->get_FNumPoints();
				with0->PMultipliers.resize(2);
			}
		}
		else
		{
			try
			{
				with0->PMultipliers.resize(with0->get_FNumPoints());
				if(with0->Interval == 0.0)
					with0->Hours.resize(with0->get_FNumPoints());
				i = 0;
				while((!Eof(f)) && (i < with0->FNumPoints))
				{
					++i;
					if(with0->Interval == 0.0)
						System::Read(f, &(with0->Hours)[i - 1]);
					System::Read(f, &(with0->PMultipliers)[i - 1]);
				}
				System::CloseFile(f);
				if(i != with0->FNumPoints)
					with0->Set_NumPoints(i);
			}
			catch(...)
			{
				DoSimpleMsg(String("Error Processing LoadShape File: \"") + FileName, 618);
				System::CloseFile(f); System::InOutRes = 0;
				return;
			}
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLoadShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLoadShapeObj::TLoadShapeObj(TDSSClass* ParClass, const String LoadShapeName)
 : inherited(ParClass),
			LastValueAccessed(1),
			FNumPoints(0),
			ArrayPropertyIndex(0),
			iMaxP(0),
			FStdDevCalculated(false),
			MaxQSpecified(false),
			FMean(0.0),
			FStdDev(0.0),
			Interval(0.0),
			MaxP(0.0),
			MaxQ(0.0),
			BaseP(0.0),
			BaseQ(0.0),
			Enabled(false),
			UseActual(false),
			UseMMF(false),
#ifdef _WIN32
			myMMF(0),
			myQMMF(0),
#endif			
			myFile(0),
			myQFile(0),
			myFileSizeQ(0),
			myFileSize(0),
			myViewQ(nullptr),
			myView(nullptr),
			myFileType(0),
			myFileTypeQ(0),
			myColumn(0),
			myColumnQ(0),
			myLineLen(0),
			myLineLenQ(0),
			myDataSize(0),
			myDataSizeQ(0),
			MyViewLenQ(0),
			MyViewLen(0)
{
	Set_Name(LowerCase(LoadShapeName));
	DSSObjType = ParClass->DSSClassType;
	Interval = 1.0;  // hr
	Hours.clear();
	PMultipliers.clear();
	QMultipliers.clear();
	MaxP = 1.0;
	MaxQ = 0.0;
	BaseP = 0.0;
	BaseQ = 0.0;
	UseActual = false;
	UseMMF = false;  // No memory mapping by default
	MaxQSpecified = false;
	FStdDevCalculated = false;  // calculate on demand
	Enabled = true;
	MyViewLen = 1000;   // 1kB by default, it may change for not missing a row
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLoadShapeObj::~TLoadShapeObj()
{
	if( !Hours.empty() )
		Hours.clear();
	if( !PMultipliers.empty() )
		PMultipliers.clear();
	if( !QMultipliers.empty() )
		QMultipliers.clear();

	if(UseMMF)
	{
#ifdef _WIN32
		UnmapViewOfFile(myView);
		CloseHandle(myMMF);
		CloseHandle(myFile);
		UnmapViewOfFile(myViewQ);
		CloseHandle(myQMMF);
		CloseHandle(myQFile);
#else
        if ((myViewQ != nullptr) && (myViewQ != myView))
            munmap(myViewQ, myFileSizeQ);
        if (myView != nullptr)
            munmap(myView, myFileSize);

        if ((myQFile != 0) && (myQFile != myFile))
            close(myQFile);
        if (myFile != 0)
            close(myQFile);
#endif		
	}

	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This function returns a multiplier for the given hour.
// If no points exist in the curve, the result is  1.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.

complex TLoadShapeObj::GetMult(double hr)
{
	complex	result = CZero;
	String	FileType = "";
	bool	MMFound = false;
	int		LocalPage = 0,
			UpLimit = 0,
			LowLimit = 0,
			Index = 0,
			j = 0,
			i = 0;

	auto Set_Result_im = [&](double RealPart) -> double 
	{
		double result = 0.0;
		if(UseActual)       // if actual, assume zero
			result = 0.0;
		else
			result = RealPart; // same as real otherwise
		return result;
	};
  /*Set imaginary part of Result when Qmultipliers not defined*/
	result.re = 1.0;
	result.im = 1.0;    // default return value if no points in curve
	if(FNumPoints > 0)
	{
		if(FNumPoints == 1)         // Handle Exceptional cases
		{
			result.re = PMultipliers[1 - 1];
			if( !QMultipliers.empty())
				result.im = QMultipliers[1 - 1];
			else
				result.im = Set_Result_im(result.re);
		}
		else
		{
			if(Interval > 0.0)                                      // Using Interval
			{
				Index = Round(hr / Interval);
				if(UseMMF)
				{
					if(Index > myDataSize)
						Index = Index % myDataSize;  // Wrap around using remainder
					if(Index == 0)
						Index = myDataSize;
					result.re = InterpretDblArrayMMF(myView, myFileType, myColumn, Index, myLineLen);
					if( !QMultipliers.empty() )
						result.im = InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, Index, myLineLenQ);
					else
						result.im = Set_Result_im(result.re);
				}
				else
				{
					if(Index > FNumPoints)
						Index = Index % FNumPoints;  // Wrap around using remainder
					if(Index == 0)
						Index = FNumPoints;
					result.re = PMultipliers[Index - 1];
					if(!QMultipliers.empty())
						result.im = QMultipliers[Index - 1];
					else
						result.im = Set_Result_im(result.re);
				}
			}
			else

      // For random interval
			        /* Start with previous value accessed under the assumption that most
          of the time, this function will be called sequentially*/
        /*Normalize Hr to max hour in curve to get wraparound*/
			{
				int stop = 0;
				if(hr > (Hours)[FNumPoints - 1])
					hr = hr - Trunc(hr / (Hours)[FNumPoints - 1]) * (Hours)[FNumPoints - 1];
				if((Hours)[LastValueAccessed - 1] > hr)
					LastValueAccessed = 1;  // Start over from beginning
				for(stop = FNumPoints, i = LastValueAccessed + 1; i <= stop; i++)
				{
					if(Abs(((Hours)[i - 1] - hr)) < 0.00001)  // If close to an actual point, just use it.
					{
						if(UseMMF)
						{
							result.re = InterpretDblArrayMMF(myView, myFileType, myColumn, Index, myLineLen);
							if(!QMultipliers.empty())
								result.im = InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, Index, myLineLenQ);
							else
								result.im = Set_Result_im(result.re);
						}
						else
						{
							result.re = PMultipliers[i - 1];
							if(!QMultipliers.empty())
								result.im = QMultipliers[i - 1];
							else
								result.im = Set_Result_im(result.re);
						}
						LastValueAccessed = i;
						return result;
					}
					else
					{
						if((Hours)[i - 1] > hr)      // Interpolate for multiplier
						{
							LastValueAccessed = i - 1;
							if(UseMMF)
							{
								result.re = InterpretDblArrayMMF(myView, myFileType, myColumn, LastValueAccessed, myLineLen) + (hr - (Hours)[LastValueAccessed - 1]) / ((Hours)[i - 1] - (Hours)[LastValueAccessed - 1]) * (InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen) - InterpretDblArrayMMF(myView, myFileType, myColumn, LastValueAccessed, myLineLen));
								if(!QMultipliers.empty())
									result.im = InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, LastValueAccessed, myLineLenQ) + (hr - (Hours)[LastValueAccessed - 1]) / ((Hours)[i - 1] - (Hours)[LastValueAccessed - 1]) * (InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ) - InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, LastValueAccessed, myLineLenQ));
								else
									result.im = Set_Result_im(result.re);
							}
							else
							{
								result.re = PMultipliers[LastValueAccessed - 1] + (hr - (Hours)[LastValueAccessed - 1]) / ((Hours)[i - 1] - (Hours)[LastValueAccessed - 1]) * (PMultipliers[i - 1] - PMultipliers[LastValueAccessed - 1]);
								if(!QMultipliers.empty())
									result.im = QMultipliers[LastValueAccessed - 1] + (hr - (Hours)[LastValueAccessed - 1]) / ((Hours)[i - 1] - (Hours)[LastValueAccessed - 1]) * (QMultipliers[i - 1] - QMultipliers[LastValueAccessed - 1]);
								else
									result.im = Set_Result_im(result.re);
							}
							return result;
						}
					}
				}
        // If we fall through the loop, just use last value
				LastValueAccessed = FNumPoints - 1;
				result.re = PMultipliers[FNumPoints - 1];
				if(!QMultipliers.empty())
					result.im = QMultipliers[FNumPoints - 1];
				else
					result.im = Set_Result_im(result.re);
			}
		}
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// normalize this load shape

void TLoadShapeObj::Normalize()
{
	double MaxMult = 0.0;

	auto DoNormalize = [&](pDoubleArray Multipliers) -> void 
	{
		int i = 0;
		if(FNumPoints > 0)
		{
			int stop = 0;
			if(MaxMult <= 0.0)
			{
				int stop = 0;
				MaxMult = Abs((Multipliers)[1 - 1]);
				for(stop = FNumPoints, i = 2; i <= stop; i++)
				{
					MaxMult = max(MaxMult, Abs((Multipliers)[i - 1]));
				}
			}
			if(MaxMult == 0.0)
				MaxMult = 1.0; // Avoid divide by zero
			for(stop = FNumPoints, i = 1; i <= stop; i++)
			{
				(Multipliers)[i - 1] = (Multipliers)[i - 1] / MaxMult;
			}
		}
	};
	if(UseMMF)
		DoSimpleMsg("Normalize is not possible when working in memory mapping mode\"", 2000001);
	else
	{
		MaxMult = BaseP;
		DoNormalize(&(PMultipliers[0]));
		if(!QMultipliers.empty())
		{
			MaxMult = BaseQ;
			DoNormalize(&(QMultipliers[0]));
		}
		UseActual = false;  // not likely that you would want to use the actual if you normalized it.
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLoadShapeObj::CalcMeanandStdDev()
{
	if(FNumPoints > 0)
	{
		if(Interval > 0.0)
			RCDMeanAndStdDev(&(PMultipliers[0]), FNumPoints, FMean, FStdDev);
		else
			CurveMeanAndStdDev(&(PMultipliers[0]), &(Hours[0]), FNumPoints, FMean, FStdDev);
	}
	Set_PropertyValue(5,Format("%.8g", FMean));
	Set_PropertyValue(6,Format("%.8g", FStdDev));
	FStdDevCalculated = true;
   /* No Action is taken on Q multipliers*/
}

/*
Function TLoadShapeObj.Get_FirstMult:Double;
Begin

  If Npts>0 Then Begin
     Result :=  Multipliers^[1];
     LastValueAccessed := 1;
  End
  Else
      Result := 0.0;

End;

Function TLoadShapeObj.Get_NextMult :Double;
Begin

  If Npts>0 Then Begin
     Inc(LastValueAccessed);
     If LastValueAccessed>Npts Then Begin
         Result := 0.0;
         Dec(LastValueAccessed);
     End
     Else Begin
          Result :=  Multipliers^[LastValueAccessed];
     End;
  End Else
      Result := 0.0;

End;
*/

double TLoadShapeObj::Get_Interval()
{
	double result = 0.0;
	if(Interval > 0.0)
		result = Interval;
	else
	{
		if(LastValueAccessed > 1)
			result = (Hours)[LastValueAccessed - 1] - (Hours)[LastValueAccessed - 1 - 1];
		else
			result = 0.0;
	}
	return result;
}

double TLoadShapeObj::Get_Mean()
{
	double result = 0.0;
	if(!FStdDevCalculated)
		CalcMeanandStdDev();
	result = FMean;
	return result;
}

double TLoadShapeObj::Get_StdDev()
{
	double result = 0.0;
	if(!FStdDevCalculated)
		CalcMeanandStdDev();
	result = FStdDev;
	return result;
}

double TLoadShapeObj::Mult(int i)
{
	double result = 0.0;
	if((i <= FNumPoints) && (i > 0))
	{
		if(UseMMF)
			result = InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen);
		else
			result = PMultipliers[i - 1];
		LastValueAccessed = i;
	}
	else
	result = 0.0;
	return result;
}

double TLoadShapeObj::Hour(int i)
{
	double result = 0.0;
	if(Interval == 0)
	{
		if((i <= FNumPoints) && (i > 0))
		{
			result = (Hours)[i - 1];
			LastValueAccessed = i;
		}
		else
		result = 0.0;
	}
	else
	{
		result = (Hours)[i - 1] * Interval;
		LastValueAccessed = i;
	}
	return result;
}

void TLoadShapeObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ 
				System::Write(f, "~ "); 
				System::Write(f, with0->PropertyName[i - 1]); 
				System::Write(f, "="); 
				System::WriteLn(f, Get_PropertyValue(i)); }
		}
	}
}

String TLoadShapeObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	2:
		result = Format("%.8g", Interval);
		break;
		case 	3:
		{
			if(UseMMF)
				result = String("(") + myFileCmd + ")";
			else
				result = GetDSSArray_Real(FNumPoints, &(PMultipliers[0]));
		}
		break;
		case 	4:
		if(!Hours.empty())
			result = GetDSSArray_Real(FNumPoints, &(Hours[0]));
		break;
		case 	5:
		result = Format("%.8g", Get_Mean());
		break;
		case 	6:
		result = Format("%.8g", Get_StdDev());
		break;
		case 	11:
		{
			if(!QMultipliers.empty())
			{
				if(UseMMF)
					result = String("(") + myFileCmdQ + ")";
				else
					result = GetDSSArray_Real(FNumPoints, &(QMultipliers[0]));
			}
		}
		break;
		case 	12:
		if(UseActual)
			result = "Yes";
		else
			result = "No";
		break;
		case 	13:
		result = Format("%.8g", MaxP);
		break;
		case 	14:
		result = Format("%.8g", MaxQ);
		break;
		case 	15:
		result = Format("%.8g", Interval * 3600.0);
		break;
		case 	16:
		result = Format("%.8g", Interval * 60.0);
		break;
		case 	17:
			result = Format("%.8g", BaseP);
		break;
		case 	18:
		result = Format("%.8g", BaseQ);
		break;
		case 	19:
		{
			if(UseMMF)
				result = String("(") + myFileCmd + ")";
			else
				result = GetDSSArray_Real(FNumPoints, &(PMultipliers[0]));
		}
		break;
		case 	21:
		if(UseMMF)
			result = "Yes";
		else
			result = "No";
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TLoadShapeObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1, "0");     // Number of points to expect
	Set_PropertyValue(2, "1"); // default = 1.0 hr;
	Set_PropertyValue(3, "");     // vector of multiplier values
	Set_PropertyValue(4, "");     // vextor of hour values
	Set_PropertyValue(5, "0");     // set the mean (otherwise computed)
	Set_PropertyValue(6, "0");   // set the std dev (otherwise computed)
	Set_PropertyValue(7, "");   // Switch input to a csvfile
	Set_PropertyValue(8, "");  // switch input to a binary file of singles
	Set_PropertyValue(9, "");   // switch input to a binary file of singles
	Set_PropertyValue(10, ""); // action option .
	Set_PropertyValue(11, ""); // Qmult.
	Set_PropertyValue(12, "No");
	Set_PropertyValue(13, "0");
	Set_PropertyValue(14, "0");
	Set_PropertyValue(15, "3600");   // seconds
	Set_PropertyValue(16, "60");     // minutes
	Set_PropertyValue(17, "0");
	Set_PropertyValue(18, "0");
	Set_PropertyValue(19, "");   // same as 3
	Set_PropertyValue(20, "");  // switch input to csv file of P, Q pairs
	Set_PropertyValue(21, "No");  // memory mapped load shape
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TLoadShape::TOPExport(String ObjName)
{
	TStringList* NameList = nullptr;
	TStringList* cNames = nullptr;
	pDoubleArray Vbuf = nullptr;
	pDoubleArray CBuf = nullptr;
	TLoadShapeObj* Obj = nullptr;
	int MaxPts = 0;
	int i = 0;
	int j = 0;
	double MaxTime = 0.0;
	double MinInterval = 0.0;
	double Hr_Time = 0.0;
	PointerList::TPointerList* ObjList = nullptr;
	/*
	TOPTransferFile->FileName = GetOutputDirectory() + "TOP_LoadShape.STO";
	try
	{
		TOPTransferFile->Open();
	}
	catch(Exception* e)
	{
		{
			DoSimpleMsg(String("TOP Transfer File Error: ") + e->Message, 619);
			try
			{
				TOPTransferFile->Close();
              //OK if Error
			}
			catch(...)
			{
				;
			}
			return;
		}
	}

     //Send only fixed interval data
	ObjList = new PointerList::TPointerList(10);
	NameList = new TStringList();
	cNames = new TStringList();

     //Make a List of fixed interval data where the interval is greater than 1 minute
	if(CompareText(ObjName, "ALL") == 0)
	{
		Obj = ((TLoadShapeObj*) ElementList.Get_First());
		while(Obj != nullptr)
		{
			if(Obj->Interval > (1.0 / 60.0))
				ObjList->Add(Obj);
			Obj = ((TLoadShapeObj*) ElementList.Get_Next());
		}
	}
	else
	{
		Obj = ((TLoadShapeObj*) Find(ObjName));
		if(Obj != nullptr)
		{
			if(Obj->Interval > (1.0 / 60.0))
				ObjList->Add(Obj);
			else
				DoSimpleMsg(String("Loadshape.") + ObjName + " is not hourly fixed interval.", 620);
		}
		else
		{
			DoSimpleMsg(String("Loadshape.") + ObjName + " not found.", 621);
		}
	}

     //If none found, exit
	if(ObjList->get_myNumList() > 0)

       //Find Max number of points
	{
		int stop = 0;
		MaxTime = 0.0;
		MinInterval = 1.0;
		Obj = ((TLoadShapeObj*) ObjList->Get_First());
		while(Obj != nullptr)
		{
			MaxTime = Max(MaxTime, Obj->get_FNumPoints() * Obj->Interval);
			MinInterval = Min(MinInterval, Obj->Interval);
			NameList->Add(Obj->get_Name());
			Obj = ((TLoadShapeObj*) ObjList->Get_Next());
		}
      // SetLength(Xarray, maxPts);
		MaxPts = (int) Round(MaxTime / MinInterval);
		TOPTransferFile->WriteHeader(0.0, MaxTime, MinInterval, ObjList->get_myNumList(), 0, 16, "OpenDSS(TM), EPRI (R)");
		TOPTransferFile->WriteNames(NameList, cNames);
		Hr_Time = 0.0;
		Vbuf = ((pDoubleArray) AllocMem(sizeof((*Vbuf)[1 - 1]) * ObjList->get_myNumList()));
		CBuf = ((pDoubleArray) AllocMem(sizeof((*Vbuf)[1 - 1]) * 1));   // just a dummy -- Cbuf is ignored here
		for(stop = MaxPts, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = ObjList->get_myNumList(), j = 1; j <= stop1; j++)
			{
				Obj = ((TLoadShapeObj*) ObjList->Get(j));
				(*Vbuf)[j - 1] = Obj->GetMult(Hr_Time).re;
			}
			TOPTransferFile->WriteData(Hr_Time, Vbuf, CBuf);
			Hr_Time = Hr_Time + MinInterval;
		}
		TOPTransferFile->Close();
		TOPTransferFile->SendToTop();
		Vbuf = (pDoubleArray) realloc(Vbuf, 0);
		CBuf = (pDoubleArray) realloc(CBuf, 0);
	}
	delete ObjList;
	delete NameList;
	delete cNames;
	*/
}

void TLoadShapeObj::SaveToDblFile()
{
	double myDBL = 0.0;
	System::TTypedFile<double> f;
	int i = 0;
	String FName;
	if (!PMultipliers.empty())
	{
		try
		{
			FName = get_Name() + "_P.dbl";
			System::AssignFile(f, FName);
			System::Rewrite(f);
			IOResultToException();
			if (UseMMF)
			{
				int stop = 0;
				for (stop = get_FNumPoints(), i = 1; i <= stop; i++)
				{
					myDBL = InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen);
					System::Write(f, &myDBL);
				}
			}
			else
			{
				int stop = 0;
				for (stop = get_FNumPoints(), i = 1; i <= stop; i++)
				{
					System::Write(f, &PMultipliers[i - 1]);
				}
			}
			GlobalResult = String("mult=[dblfile=") + FName + "]";
			//		}
			//		__finally
			//		{
			System::CloseFile(f);
		}
		catch (...)
		{
			// Added to match Delphi (try)
		}
		if(!QMultipliers.empty())
		{
			try
			{
				FName = get_Name() + "_Q.dbl";
				System::AssignFile(f, FName);
				System::Rewrite(f);
				IOResultToException();
				if(UseMMF)
				{
					int stop = 0;
					for(stop = get_FNumPoints(), i = 1; i <= stop; i++)
					{
						myDBL = InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ);
						System::Write(f, &myDBL);
					}
				}
				else
				{
					int stop = 0;
					for(stop = get_FNumPoints(), i = 1; i <= stop; i++)
					{
						System::Write(f, &QMultipliers[i - 1]);
					}
				}
				AppendGlobalResult(String(" Qmult=[dblfile=") + FName + "]");
//			}
//			__finally
//			{
				System::CloseFile(f);
			}
			catch (...)
			{
				// added to match Delphi (try)
			}
		}
	}
	else
	DoSimpleMsg(String("Loadshape.") + get_Name() + " P multipliers not defined.", 622);
}

//--------------------------------------------------------------------------------------------

int TLoadShapeObj::get_FNumPoints()
{
	return FNumPoints;
}

//--------------------------------------------------------------------------------------------

void TLoadShapeObj::SaveToSngFile()
{
	System::TTypedFile<float> f;
	int i = 0;
	String FName;
	float Temp = 0.0F;
	if(!PMultipliers.empty())
	{
		try
		{
			int stop = 0;
			FName = get_Name() + "_P.sng";
			System::AssignFile(f, FName);
			System::Rewrite(f);
			IOResultToException();
			for(stop = get_FNumPoints(), i = 1; i <= stop; i++)
			{
				if(UseMMF)
					Temp = (float) InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen);
				else
					Temp = (float) PMultipliers[i - 1];
				System::Write(f, &Temp);
			}
			GlobalResult = String("mult=[sngfile=") + FName + "]";
//		}
//		__finally
//		{
			CloseFile(f);
		}
		catch (...)
		{
			// added to match with Delphi (try)
		}
		if(!QMultipliers.empty())
		{
			try
			{
				int stop = 0;
				FName = get_Name() + "_Q.sng";
				System::AssignFile(f, FName);
				System::Rewrite(f);
				IOResultToException();
				for(stop = get_FNumPoints(), i = 1; i <= stop; i++)
				{
					if(UseMMF)
						Temp = (float) InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ);
					else
						Temp = (float) QMultipliers[i - 1];
					System::Write(f, &Temp);
				}
				AppendGlobalResult(String(" Qmult=[sngfile=") + FName + "]");
//			}
//			__finally
//			{
				CloseFile(f);
			}
			catch (...)
			{
				// Added to match Delphi (try)
			}
		}
	}
	else
	DoSimpleMsg(String("Loadshape.") + get_Name() + " P multipliers not defined.", 623);
}

void TLoadShapeObj::SetMaxPandQ()
{
	if(!UseMMF)
	{
		iMaxP = iMaxAbsdblArrayValue(get_FNumPoints(), &(PMultipliers[0]));
		if(iMaxP > 0)
		{
			MaxP = PMultipliers[iMaxP - 1];
			if(!MaxQSpecified)
			{
				if(!QMultipliers.empty())
					MaxQ = QMultipliers[iMaxP - 1];
				else
					MaxQ = 0.0;
			}
		}
	}
}

void TLoadShapeObj::Set_Mean(double Value)
{
	FStdDevCalculated = true;
	FMean = Value;
}

void TLoadShapeObj::Set_NumPoints(int Value)
{
	Set_PropertyValue(1,IntToStr(Value));   // Update property list variable

        // Reset array property values to keep them in proper order in Save
	if(ArrayPropertyIndex > 0)
		Set_PropertyValue(ArrayPropertyIndex,Get_PropertyValue(ArrayPropertyIndex));
	if(!QMultipliers.empty())
		Set_PropertyValue(11,Get_PropertyValue(11));
	FNumPoints = Value;   // Now assign the value
}

void TLoadShapeObj::Set_StdDev(double Value)
{
	FStdDevCalculated = true;
	FStdDev = Value;
}

}  // namespace LoadShape





