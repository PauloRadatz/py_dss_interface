#ifndef PlotOptionsH
#define PlotOptionsH

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
#include "DSSPlot.h"

namespace PlotOptions
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
	const int NumPlotOptions = 23;
	int DoPlotCmd();
	extern String* PlotOption;
	extern String* PlotHelp;
	extern Command::TCommandList* PlotCommands;


}  // namespace PlotOptions

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PlotOptions;
#endif

#endif // PlotOptionsH




