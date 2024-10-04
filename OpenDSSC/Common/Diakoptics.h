#ifndef DiakopticsH
#define DiakopticsH
/*
   ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*

*/


#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"


int Solve_Diakoptics( );
int ADiakoptics_Tearing( bool AddISrc );
void ADiakopticsInit( );
int Calc_C_Matrix( PString PLinks, int NLinks );
int Calc_ZLL( PString PLinks, int NLinks );
void Calc_ZCC( int Links );
void Calc_Y4( );
void SendIdx2Actors( );
String get_Statistics( );

#endif //  DiakopticsH








