#ifndef ExecutiveH
#define ExecutiveH

#include "System.h"
#include "Sysutils.h"

#include "PointerList.h"
#include "Command.h"
#include "d2c_structures.h"

#include "DSSClassDefs.h"
#include "ParserDel.h"
#include "DSSClass.h"
#include "IniRegSave.h"

#include <locale>


namespace Executive
{


	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/

	/*  Change Log

	  8/12/99  Added Show Zone Help string

	  10/11/99 Added Dump Commands option.  Moved ExecCommand into Public area.
	  10/12/99 ADded new AutoAdd options and revised behavior of New Command.
	  10/14/99 Added UE weighting option
			   Fixed Redirect/Compile to change default directory.
	  11/2/99  Added message in Open and Close cmd for ckt element not found.
	  12/3/99  Fixed bug in command parser - needed quotes when rebuilding command line
	  12/6/99  Merged Set and Solve commands
	  1-14-00 Added Get Command
			  Added LossWeight, UEreg, lossreg properties
	  2-20-00 Revised Helpform so that help strings won't go away after Clear
	  3-2-00  Repaired some places where re-parsing would mess up on names with blanks
	  3-10-00 Added FileEdit and Export commands
	  3-20-00 Added DefaultDaily and DefaultYearly Options
	  4-17-00 Moved bulk of functions to ExecHelper
			  Added AllocateLoads Command and AllocationFactors option
	  8-23-00 Added Price Signal Option
	  9-18-00 Fixed Dump Command Help
	  9-20-00 Added Dynamic Mode
	  10-3-00 Removed test for comment since '//' is now done in the Parser
	  5/22/01 Changed behavior of Compile and Redirect with respect to directory changes.
	  5/30/01 Add Set maxControlIterations
	  7/19/01 Added Totals command, Capacity Command
	  8/1/01  Revise the way the Capacity Command works
	  9/12/02 Added Classes and UserClasses
	  2/4/03  Added Set Bus=
			  Added Zsc, Zsc012.
			  Changed way Voltages command works

	*/

	class TExecutive : public TObject
	{
	public:
		typedef TObject inherited;	
	//private:
		bool FRecorderOn;
		string FRecorderFile;
		string Get_LastError();
		int Get_ErrorResult();
		string Get_Command();
		void Set_Command(String Value);
		void Set_RecorderOn(bool Value);
		bool get_FRecorderOn();
	public:
		System::TTextRec RecorderFile;
		TExecutive();
		virtual ~TExecutive();
		void CreateDefaultDSSItems();
		void Write_to_RecorderFile(const String s);
		void Clear();
		void ClearAll();
	};

}  // namespace Executive

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Executive;
#endif

#endif // ExecutiveH




