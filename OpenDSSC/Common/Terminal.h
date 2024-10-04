#ifndef TerminalH
#define TerminalH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/* Definition of classes for all terminals of a DSS element*/


//#include <System.hpp>

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"
#include "Conductor.h"



//class TPowerTerminal;




class TPowerTerminal: public TObject {
  typedef TObject inherited;
  private:
  int FNumCond;
  int ActiveConductor;
  int get_ActiveConductor();
  void Set_ActiveConductor( int Value );
  public:
  int BusRef;
  std::vector <longInt> TermNodeRef;   // Need to get to this fast
  pTConductorArray Conductors;
  bool Checked;
  TPowerTerminal( int Ncond );
  virtual ~TPowerTerminal( );
  TPowerTerminal();
};


typedef std::vector <TPowerTerminal> pTerminalList;
//typedef TPowerTerminal* TerminalList [ 3/*# range 1..3*/ ];

  /*
   Control Terminal is managed by override functions in classes that are derived from this class
  */

#endif //  TerminalH








