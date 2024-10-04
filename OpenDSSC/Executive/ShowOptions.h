#ifndef ShowOptionsH
#define ShowOptionsH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "d2c_structures.h"

#include "ShowResults.h"
#include "ParserDel.h"
#include "Monitor.h"
#include "Utilities.h"
#include "DSSGlobals.h"
#include "CmdForms.h"
#include "LineUnits.h"

namespace ShowOptions
{

	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/
	const int NumShowOptions = 35;
	int DoShowCmd();
	extern std::string* ShowOption;
	extern std::string* ShowHelp;
	extern Command::TCommandList* ShowCommands;


}  // namespace ShowOptions

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ShowOptions;
#endif

#endif // ShowOptionsH




