#ifndef ExportOptionsH
#define ExportOptionsH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "d2c_structures.h"

#include "ExportResults.h"
#include "Monitor.h"
#include "EnergyMeter.h"
#include "ParserDel.h"
#include "DSSGlobals.h"
#include "ExportCIMXML.h"
#include "Utilities.h"
#include "NamedObject.h"

namespace ExportOptions
{


	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/
	const int NumExportOptions = 64;
	int DoExportCmd();
	extern std::string* ExportOption;
	extern std::string* ExportHelp;
	extern Command::TCommandList* ExportCommands;


}  // namespace ExportOptions

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ExportOptions;
#endif

#endif // ExportOptionsH




