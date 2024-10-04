#ifndef ExecOptionsH
#define ExecOptionsH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "d2c_structures.h"

#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "ParserDel.h"
#include <math.h>
#include "Executive.h"
#include "ExecHelper.h"
#include "LoadShape.h"
#include "Utilities.h"
#include "Solution.h"
#include "EnergyMeter.h"
#include "Diakoptics.h"

namespace ExecOptions
{

	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2024, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/
	const int NumExecOptions = 141;
	extern std::string* ExecOption;
	extern std::string* OptionHelp;
	extern Command::TCommandList* OptionList;
	int DoGetCmd();
	int DoSetCmd(int SolveOption);
	bool DoSetCmd_NoCircuit();  // Set Commands that do not require a circuit
	bool DoGetCmd_NoCircuit();  // Get Commands that do not require a circuit


}  // namespace ExecOptions

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ExecOptions;
#endif

#endif // ExecOptionsH




