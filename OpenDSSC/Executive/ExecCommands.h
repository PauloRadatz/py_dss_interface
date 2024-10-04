#ifndef ExecCommandsH
#define ExecCommandsH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "d2c_structures.h"

#include "DSSGlobals.h"
#include "ExecHelper.h"
#include "Executive.h"
#include "ExecOptions.h"
#include "ShowOptions.h"
#include "ExportOptions.h"
#include "ParserDel.h"
#include "LoadShape.h"
#include "Utilities.h"
#include "SolutionAlgs.h"
#include "DSSClassDefs.h"
#include "Arraydef.h"
#include "CmdForms.h"
#include "ConnectOptions.h"
#include "Diakoptics.h"
#include "Sparse_Math.h"
#include "MemoryMap_lib.h"
#include "EnergyMeter.h"
#include "GISCommands.h"
#include "PlotOptions.h"

namespace ExecCommands
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
	const int NumExecCommands = 130;		// This number has to change if we add more commands to OpenDSS...this will need to change for induces
	extern string* ExecCommand;
	extern string* CommandHelp;
	extern TCommandList CommandList;
	extern string LastCmdLine;   // always has last command processed
	extern string RedirFile;
	void ProcessCommand(const string CmdLine);


}  // namespace ExecCommands

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ExecCommands;
#endif

#endif // ExecCommandsH




