

#pragma hdrstop

#include "Terminal.h"



/*TPowerTerminal*/

TPowerTerminal::TPowerTerminal()
    : FNumCond(1),
    ActiveConductor(0),
    BusRef(-1),
    Checked(false)
{
    int i = 0;
    // inherited::Create();
    TermNodeRef.resize( 2 );
    Conductors.resize( 2 );
    for (int stop = 1, i = 1; i <= stop; i++)
        Conductors[i - 1] = TConductor();
    ActiveConductor = 1;
}

TPowerTerminal::TPowerTerminal( int Ncond )
 : FNumCond(Ncond),
   ActiveConductor(0),
   BusRef(- 1),
   Checked(false)
{
  int i = 0;
  // inherited::Create();
  TermNodeRef.resize( FNumCond + 1 );
  Conductors.resize( FNumCond + 1 );
  for ( int stop = FNumCond, i = 1; i <= stop; i++)
    Conductors[i - 1] = TConductor();
  ActiveConductor = 1;
}


TPowerTerminal::~TPowerTerminal( )
{
  int i = 0;
  Conductors.clear();
  if(!TermNodeRef.empty())
    TermNodeRef.clear();
  // todo check:  inherited::Destroy;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



void TPowerTerminal::Set_ActiveConductor( int Value )
{
  if ( ( Value > 0 ) && ( Value <= FNumCond ) )
    ActiveConductor = Value;
}

int TPowerTerminal::get_ActiveConductor()
{
    return ActiveConductor;
}









