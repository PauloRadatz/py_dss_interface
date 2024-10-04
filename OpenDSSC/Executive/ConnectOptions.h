#ifndef ConnectOptionsH
#define ConnectOptionsH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "d2c_structures.h"

namespace ConnectOptions
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
const int NumConnectOptions = 2;
int DoConnectCmd();
int DoDisConnectCmd();
extern String* ConnectOption;
extern String* ConnectHelp;
extern Command::TCommandList* ConnectCommands;


}  // namespace ConnectOptions

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ConnectOptions;
#endif

#endif // ConnectOptionsH




