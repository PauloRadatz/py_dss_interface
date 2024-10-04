#ifndef DSSCallBackRoutinesH
#define DSSCallBackRoutinesH
/*
    ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/


#include "System.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "DSSCallBackStructDef.h"



typedef unsigned int unsignedint;


extern TDSSCallBacks CallBackRoutines;
void __stdcall DoSimpleMsgCallback( char* S, unsignedint maxlen ); // Call back for user-written models

#endif //  DSSCallBackRoutinesH








