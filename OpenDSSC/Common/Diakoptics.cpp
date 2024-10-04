

#pragma hdrstop

#include "Diakoptics.h"

#include "Circuit.h"
#include "Solution.h"
#include "DSSGlobals.h"
#include "DSSClassDefs.h"
#include "EnergyMeter.h"
#include "SolutionAlgs.h"
#include "Line.h"
#include "CmdForms.h"

#include "ExecHelper.h"
#include "Executive.h"
#include "ParserDel.h"
#include "YMatrix.h"
#include "klusolve.h"
#include "Ucomplex.h"
#include "Sparse_Math.h"
#include "Ucmatrix.h"
#include <math.h>
#include "dirsep.h"




/********************************************************************************
*              This is the A-Diakoptics algorithm executed by the              *
*                        Coordinator (Actor = 1)                               *
********************************************************************************/


int Solve_Diakoptics( )
{
  int result = 0;
  int i = 0, myRow = 0;
  TSparse_Complex Vpartial;
  /*Space left empty to implement the simplified Diakoptics algorithm*/
  /*# with ActiveCircuit[1], ActiveCircuit[1]->Solution do */
  {
    auto with0 = ActiveCircuit[1];
    auto with1 = ActiveCircuit[1]->Solution;
    {

    // Solves the partial systems to find the voltages at the edges of the sub-systems
      with1->SendCmd2Actors( SOLVE_AD1 );
      Vpartial = TSparse_Complex();
      Vpartial.sparse_matrix_Cmplx( with0->Contours.NCols(), 1);
    // Does the voltage diff calculation using the partial results
      myRow = 0;
      for ( int stop = (with0->Contours.NCols() - 1), i = 0; i <= stop; i++)
      {
        Vpartial.Insert( i, 0, csub( with1->NodeV[with0->Contours.CData[myRow].Row + 1], with1->NodeV[with0->Contours.CData[myRow + 1].Row + 1] ) );
        myRow = myRow + 2;
      }
    // Loads the partial solution considering the previous iteration
      Vpartial = with0->Y4.multiply( &Vpartial );
      with0->Ic = with0->Contours.multiply( &Vpartial );  // Calculates the new Injecting Currents

    // Commands the actors to complement the solution
      with1->SendCmd2Actors( SOLVE_AD2 );
    }
  }
  ActiveCircuit[1]->Issolved = true;
  ActiveCircuit[1]->Set_BusNameRedefined(false);
  if ( SolutionAbort )
    ActiveCircuit[1]->Issolved = false;
  ActiveActor = 1;    // Returns the control to Actor 1
  result = 0;
  //free(Vpartial);
  return result;
}

/********************************************************************************
*              Returns a string with the partitioning statistics               *
*                  It only works if the partitioning was succesful             *
********************************************************************************/


String get_Statistics( )
{
  String result;
  std::vector <float> unbalance;
  std::vector <float> ASize;
  int idx = 0;
  double GReduct = 0.0, MaxImbal = 0.0, AvgImbal = 0.0;
  ASize.clear();
  for ( int stop = NumOfActors, idx = 2; idx <= stop; idx++)
  {
    ASize.push_back( ActiveCircuit[idx]->NumNodes );
  }
  GReduct = ( 1 - ( double( MaxValue( &ASize ) ) / ActiveCircuit[1]->NumNodes ) ) * 100;   // The biggest actor
  unbalance.resize( ASize.size() );
  for ( int stop = ASize.size() - 1, idx = 0; idx <= stop; idx++)
    unbalance[idx] = ( 1 - ( ASize[idx] / MaxValue( &ASize ) ) ) * 100; // All the unbalances
  MaxImbal = MaxValue( &unbalance );                              // Max imbalance
  AvgImbal = mean( &unbalance );                                  // Average
  // publishes the results
  result = CRLF + "Circuit reduction    (%): " + FloatToStrF( GReduct, ffGeneral, 4, 2 ) + CRLF +
      "Max imbalance       (%): " + FloatToStrF( MaxImbal, ffGeneral, 4, 2 ) + CRLF + 
      "Average imbalance(%): " + FloatToStrF( AvgImbal, ffGeneral, 4, 2 ) + CRLF;
  return result;
}

/********************************************************************************
*              Sets the memory index for each actor so they can write          *
*                   directly into the coordinator's Voltage vector             *
********************************************************************************/


void SendIdx2Actors( )
{
  int i = 0, j = 0, k = 0;
  String BusName;
  std::vector <String> AllNNames;
// Gets the names of the nodes in the interconnected system
  AllNNames.resize( 0 );
  /*# with ActiveCircuit[1] do */
  {
    auto with0 = ActiveCircuit[1];
    {
      for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
      {
        BusName = with0->BusList.Get( i );
        for ( int stop = with0->Buses[i - 1]->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
        {
          AllNNames.push_back( BusName + "." + IntToStr(with0->Buses[i - 1]->GetNum( j ) ) );
        }
      }
    }
  }
// Sets the index for each actor
// The feeder head first
  ActiveCircuit[2]->VIndex = 0;
// Then checks the rest of the actors
  for ( int stop = NumOfActors, i = 3; i <= stop; i++)
  {
    BusName = ActiveCircuit[i]->BusList.Get( 1 ) + ".1";
    // Looks for the node within all the Node Names in the interconnected model
    for ( int stop = AllNNames.size() - 1, j = 0; j <= stop; j++)
      if ( BusName == AllNNames[j] )
        break;
    ActiveCircuit[i]->VIndex = j;
  }
  // Initializes the Ic vector with zeros
  ActiveCircuit[1]->Ic.sparse_matrix_Cmplx( AllNNames.size(), 1 );
  ActiveCircuit[1]->V_0.sparse_matrix_Cmplx( AllNNames.size(), 1 );
  for ( int stop = AllNNames.size() - 1, i = 0; i <= stop; i++)
  {
    ActiveCircuit[1]->Ic.Insert( i, 0, CZero );
    ActiveCircuit[1]->V_0.Insert( i, 0, CZero );
  }
}

/********************************************************************************
*              Inverts ZCC to obtain its admittance equivalent Y4              *
*                      This is the heart of A-Diakoptics                       *
********************************************************************************/


void Calc_Y4( )
{
  complex Value;
  int NumRows = 0, NumCols = 0, col = 0, idx = 0;
  TcMatrix TempMat;
// 4 Debugging
//  myFile    : TextFile;
//  Text      : String;
  /*# with ActiveCircuit[1], ActiveCircuit[1]->Solution do */
  {
    auto with0 = ActiveCircuit[1];
    {
    //  Moves ZCC into an equivalent compatible with TcMatrix
      TempMat = TcMatrix(with0->ZCC.NRows() );
      for ( int stop = with0->ZCC.CData.size() - 1, idx = 0; idx <= stop; idx++)
      {
        TempMat.SetElement(with0->ZCC.CData[idx].Row + 1, with0->ZCC.CData[idx].col + 1, with0->ZCC.CData[idx].Value );
      }
    //  Inverts the ZCC equivalent
      TempMat.Invert();
      with0->Y4.sparse_matrix_Cmplx(with0->ZCC.NRows(), with0->ZCC.NCols());
      NumRows = with0->ZCC.NRows() - 1;
      NumCols = with0->ZCC.NCols() - 1;
    // Moves the inverse into Y4 for furhter use
      for ( int stop = NumRows, idx = 0; idx <= stop; idx++)
      {
        for ( int stop = NumCols, col = 0; col <= stop; col++)
        {
          Value = TempMat.GetElement( idx + 1, col + 1 );
          if ( ( Value.re != 0 ) && ( Value.re != 0 ) )
              with0->Y4.Insert( idx, col, Value );
        }
      }
//********************Dbug************************************
/*
    AssignFile(myFile, 'C:\Temp\Y4Mat.csv');
    ReWrite(myFile);
    IOResultToException();
    Text        :=  '';
    for idx := 0 to (length(Y4.CData)- 1) do
    Begin
        Text  :=  inttostr(Y4.CData[idx].Row) + ',' + inttostr(Y4.CData[idx].Col) +
        ',' + floattostr(Y4.CData[idx].Value.re);
        if Y4.CData[idx].Value.im < 0 then
          Text  :=  Text  + '-i' +  floattostr(-1*Y4.CData[idx].Value.im)
        else
          Text  :=  Text  + '+i' +  floattostr(Y4.CData[idx].Value.im);
        WriteLn(myFile,Text);
    End;
    CloseFile(myFile);
*/
    }
  }
}

/********************************************************************************
*              Calculates the Connections matrix ZCC in the                    *
*                      contours-contours domain                                *
********************************************************************************/


void Calc_ZCC( int Links )
{
  int Row = 0, col = 0, idx3 = 0, idx2 = 0, idx = 0;
  unsigned int NNodes = 0;
  pComplexArray CVector, ZVector;
  complex Ctemp;
// 4 Debugging
//  myFile    : TextFile;
//  Text      : String;
  /*# with ActiveCircuit[1], ActiveCircuit[1]->Solution do */
  {
    auto with0 = ActiveCircuit[1];
    {
      GetSize(with0->Solution->hY, &NNodes );
      col = NNodes;
      Links--;
      with0->ZCT.sparse_matrix_Cmplx( col, Links * 3 );
      CVector = new complex[ (col + 1) ];
      ZVector = new complex[ (col + 1) ];
      idx3 = Links * 3 - 1;
      for ( int stop = idx3, idx2 = 0; idx2 <= stop; idx2++)
      {
        for ( int stop = col, idx = 1; idx <= stop; idx++)
          CVector[idx] = CZero;  // Makes it zero
        for ( int stop = Length( &( with0->Contours.CData ) ), idx = 1; idx <= stop; idx++)
        {
          if ( with0->Contours.CData[idx - 1].col == idx2 )
          {
            Row = with0->Contours.CData[idx - 1].Row + 1;
            CVector[Row] = with0->Contours.CData[idx - 1].Value;
          }
        }
        SolveSparseSet(with0->Solution->hY,(complex*) &(ZVector[1]), (complex*) (&CVector[1]) );
        for ( int stop = col, idx = 1; idx <= stop; idx++)           // inserts result into the ZCT matrix
        {
          Ctemp = ZVector[idx];
          if ( ( Ctemp.re != 0 ) && ( Ctemp.im != 0 ) )
              with0->ZCT.Insert( ( idx - 1 ), idx2, ZVector[idx] );
        }
        idx = col;
      }
    // At this point we have calculated the right side of the equation
    // ZCC = CTZ(TT)C -> Z(TT)C
    // It is needed transpose the contours matrix and multiply it
      with0->ContoursT = with0->Contours.Transpose();
      with0->ZCC = with0->ContoursT.multiply(&with0->ZCT );   // Calculates ZCC with no Link impedances
      with0->ZCC = with0->ZCC.Add(&with0->ZLL );              // Adds the link impedance
      free( CVector );
      free( ZVector );
//********************Dbug************************************
/*
    AssignFile(myFile, 'C:\Temp\ZCCMat.csv');
    ReWrite(myFile);
    IOResultToException();
    Text        :=  '';
    for idx2 := 0 to (length(ZCC.CData)- 1) do
    Begin
        Text  :=  inttostr(ZCC.CData[idx2].Row) + ',' + inttostr(ZCC.CData[idx2].Col) +
        ',' + floattostr(ZCC.CData[idx2].Value.re);
        if ZCC.CData[idx2].Value.im < 0 then
          Text  :=  Text  + '-i' +  floattostr(-1*ZCC.CData[idx2].Value.im)
        else
          Text  :=  Text  + '+i' +  floattostr(ZCC.CData[idx2].Value.im);
        WriteLn(myFile,Text);
    End;
    CloseFile(myFile);
*/
    }
  }
}

/********************************************************************************
*                   Calculates the contours matrix based                       *
*             on the location in the graph of the link branches                *
*             if there is an error returns <> 0                                *
********************************************************************************/


int Calc_C_Matrix( PString PLinks, int NLinks )
{
  int result = 0;
  int LIdx = 0, k = 0, l = 0, j = 0, CDirection = 0, NumPhases = 0, i = 0;
  std::vector < std::string> Elem_Buses, Node_Names;
  String temp;
  bool Go_Flag = false;
  TTextRec myFile;         // For debugging
  ActiveActor = 1;
  /*# with ActiveCircuit[ActiveActor] do */
  {
    auto with0 = ActiveCircuit[ActiveActor];
    {
      result = 0;
      Elem_Buses.resize( 2 );
      Node_Names.clear();
      for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
      {
        /*# with MapNodeToBus^[i] do */
        {
          auto with1 = with0->MapNodeToBus[i - 1];
          Node_Names.push_back( Format( "%s.%-d",  LowerCase( with0->BusList.Get( with1.BusRef ) ).c_str(), with1.NodeNum));
        }
      }
      with0->Contours.sparse_matrix_Cmplx( Node_Names.size(), ( NLinks - 1 ) * 3 );
      for ( int stop = ( NLinks - 1 ), LIdx = 1; LIdx <= stop; LIdx++)
      {
        PLinks++;                  // Pointing to the Next link branch (starting in 1)
        temp = ( *PLinks );
        j = ansipos( ".", temp );
        temp = LowerCase( temp.substr( 0, ( j - 1 ) ) );
        if ( temp == "line" )
        {
          i = with0->SetElementActive( (*PLinks ) );
        // Gest the names of the buses fot this PDElement
        // If it is something different from a Transformer reports an error
        // Since a link branch cannot be a transformer
          for ( int stop = with0->get_FActiveCktElement()->Get_NTerms(), i = 1; i <= stop; i++)
          {
            Elem_Buses[i - 1] = with0->get_FActiveCktElement()->GetBus( i );
            j = ansipos( ".", Elem_Buses[i - 1] );
            if ( j != 0 )
              Elem_Buses[i - 1] = Elem_Buses[i - 1].substr( 0, j - 1 );
            else
              Elem_Buses[i - 1] = Elem_Buses[i - 1] + ".";
          }
        //  Marks the connection point in the contours matrix
          NumPhases = with0->get_FActiveCktElement()->Get_NPhases();
          for ( int stop = NumPhases, l = 1; l <= stop; l++)
          {
            for ( int stop = 1, i = 0; i <= stop; i++)
            {
              temp = Elem_Buses[i] + IntToStr( l );
              Go_Flag = true;
              j = 0;
              while ( Go_Flag && ( j <= Node_Names.size() - 1 ) )
              {
                k = ansipos( temp, Node_Names[j] );
                if ( k != 0 )
                {
                  if ( i == 0 )
                    CDirection = 1;
                  else
                    CDirection = - 1;
                  with0->Contours.Insert( j, ( ( l - 1 ) + ( LIdx - 1 ) * 3 ), cmplx( CDirection, 0 ) );
                  Go_Flag = false;
                }
                j++;
              }
            }
          }
        }
        else
        {
          result = - 1; // There was an error when selecting the link branches (MeTIS)
          break;  // Abort
        }
      }
    // More error checking
      if ( result == 0 )
      {
        if ( with0->Contours.NZero() != 0)
          result = 0;
        else
          result = 1;
      }
    }
  }
  return result;
}

/********************************************************************************
*            Calculates the Link branches matrix for further use                *
*                if there is an error returns <> 0                             *
********************************************************************************/


int Calc_ZLL( PString PLinks, int NLinks )
{
  int result = 0;
  int NValues = 0, idx = 0, k = 0, j = 0, Row = 0, col = 0, count = 0, i = 0;
  pComplexArray cValues;
  bool ErrorFlag = false;
  TSparse_Complex localMat;
  TcMatrix LinkPrim;
  NLinks--;
  ErrorFlag = false;
  LinkPrim = TcMatrix( 3 );
  ActiveActor = 1;
  /*# with ActiveCircuit[ActiveActor] do */
  {
    auto with0 = ActiveCircuit[ActiveActor];
    {
      with0->ZLL.sparse_matrix_Cmplx( NLinks * 3, NLinks * 3 );
      for ( int stop = NLinks, i = 1; i <= stop; i++)
      {
        PLinks++;
        idx = with0->SetElementActive( (*PLinks ) );
        if (with0->get_FActiveCktElement() != NULL )
          /*# with ActiveCktElement do */
          {
            auto with1 = with0->get_FActiveCktElement();
            NValues = sqr( with1->Yorder );
            cValues = with1->GetYPrimValues( ALL_YPRIM );  // Get pointer to complex array of values
            if ( cValues != NULL )
            {
              k = 1;
              idx = ( i - 1 ) * 3;
              Row = 1;
              col = 1;
              count = 0;
            // Extracts the YPrim of the Link branch
              for ( int stop = ( NValues / 4 ), j = 1; j <= stop; j++)
              {
                LinkPrim.SetElement( Row, col, cValues[k] );
                count++;
                if ( count > 2 )
                {
                  Row++;
                  col = 1;
                  count = 0;
                  k = k + 4;
                }
                else
                {
                  col++;
                  k++;
                }
              }
            // Inverts the Y primitive
              LinkPrim.Invert();
            // Inserts the Z primitive values into ZLL
              Row = 0;
              col = 0;
              count = 0;
/**/
              for ( int stop = ( NValues / 4 ), j = 1; j <= stop; j++)
              {
                with0->ZLL.Insert( ( Row + idx ), ( col + idx ), LinkPrim.GetElement( Row + 1, col + 1 ) );
                count++;
                if ( count > 2 )
                {
                  Row++;
                  col = 0;
                  count = 0;
                }
                else
                  col++;
              }
/* */
            }
            else
              ErrorFlag = true;
          }
      }
      if ( ErrorFlag )
        result = 1;
      else
        result = 0;
    }
  }
  return result;
}

/********************************************************************************
*           Tears the system using considering the number of                   *
*           circuits specified by the user                                     *
*           The flag AddISrc indicates if its necessary to create              *
*           Isources at the edges of the link branches, the ISource            *
*           magnitude is equal to 0.000001, angle 0 (for A-Diakoptics)         *
********************************************************************************/


int ADiakoptics_Tearing( bool AddISrc )
{
  int result = 0;

                              // Stores the previous solution mode

  int Prev_Mode = 0                  // Stores the number of Sub-Circuits created
  , Num_Ckts = 0;
  /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor]->Solution do */
  {
    auto with0 = ActiveCircuit[ActiveActor];
    auto with1 = ActiveCircuit[ActiveActor]->Solution;
    {
      ActiveActor = 1;
      Num_Ckts = with0->Tear_Circuit( );
      Prev_Mode = with1->DynaVars.SolutionMode;
      with1->DynaVars.SolutionMode = 0;          // Shapshot mode
      DSSExecutive[ActiveActor]->Set_Command("set controlmode=off");
      BuildYMatrix( WHOLEMATRIX, false, ActiveActor );
//    DoSolveCmd();
      if ( ! SolutionAbort )
      {
        with0->Save_SubCircuits( AddISrc );
        with1->DynaVars.SolutionMode = Prev_Mode;  // Goes back to the previous solution mode
        ActiveCircuit[1]->Num_SubCkts = Num_Ckts;
        GlobalResult = "Sub-Circuits Created: " + IntToStr( Num_Ckts );
        result = 0;
      }
      else
      {
        GlobalResult = "There was an error when tearing the circuit ";
        result = 1;
      }
    }
  }
  return result;
}

/********************************************************************************
*            Generates the subsystems, actors and memory space                 *
*                     For using the A-Diakoptics parallelism                   *
********************************************************************************/


void ADiakopticsInit( )
{
  TEnergyMeterObj* EMeter;
  int j = 0, Local_State = 0, Num_States = 0, ErrorCode = 0, DIdx = 0, Diak_Actors = 0;
  String Dir, Proj_Dir, prog_Str, ErrorStr, Fileroot;
  std::vector <String> Links;                        // List of the Link Branches

  bool MQuit = false;                                // To quit the State Machine

// The program is built as a state machine to facilitate the error detection
// and quitting the routines after an error is detected wihtout killing the prog
  MQuit = false;
  Num_States = 9;                          // Number of states of the machine
  Local_State = 0;                          // Current state
  prog_Str = "A-Diakoptics initialization summary:" + CRLF + CRLF;
  ActiveActor = 1;
  // Checks if the number of actors is within a reasonable limit
  if ( ActiveCircuit[1]->Num_SubCkts > ( CPU_Cores - 2 ) )
    ActiveCircuit[1]->Num_SubCkts = CPU_Cores - 2;
  while ( ! MQuit )
  {
    switch ( Local_State )
    {
      case 0:
      {                       // Create subcircuits
        prog_Str = prog_Str + "- Creating Sub-Circuits..." + CRLF;
        ErrorCode = ADiakoptics_Tearing( false );
        if ( ErrorCode != 0 )
          ErrorStr = "Error" + CRLF + "The circuit cannot be decomposed" + CRLF;
        else
          ErrorStr = "  " + IntToStr( ActiveCircuit[1]->Num_SubCkts ) + " Sub-Circuits Created" + CRLF;
        prog_Str = prog_Str + ErrorStr;
      }
      break;
      case 1:
      {                      // Saves the Link Branch list locally
        Diak_Actors = ActiveCircuit[1]->Num_SubCkts + 1;
        prog_Str = prog_Str + "- Indexing link branches...";
        Links.resize( ActiveCircuit[1]->Link_Branches.size() );
        for ( int stop = Links.size() - 1, DIdx = 0; DIdx <= stop; DIdx++)
          Links[DIdx] = ActiveCircuit[1]->Link_Branches[DIdx];
        prog_Str = prog_Str + "Done";
        ErrorCode = 0;          // No error handling here
      }
      break;
      case 2:
      {                      // Compile subsystems
        ErrorCode = 0;
        prog_Str = prog_Str + CRLF + "- Setting up the Actors...";
        // Clears everything to create the actors and compile the subsystems
        Parallel_enabled = false;
        DSSExecutive[ActiveActor]->ClearAll();
        Fileroot = GetCurrentDir();    //  Gets the current directory
        SolutionAbort = false;

        // Compiles the interconnected Circuit for further calculations on actor 1
        ActiveActor = 1;
        Proj_Dir = String( "compile \"" ) + Fileroot + DIRSEP_STR "Torn_Circuit" DIRSEP_STR "master_interconnected.dss\"";
        DSSExecutive[ActiveActor]->Set_Command(Proj_Dir);
        DSSExecutive[ActiveActor]->Set_Command("set controlmode=Off");
        // Disables the Energymeters for the zones
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor]->Solution do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            EMeter = (TEnergyMeterObj*) with0->EnergyMeters.Get_First();
            while ( EMeter != NULL )
            {
              j = ansipos( "zone_", ( (TDSSObject*) EMeter )->get_Name() );
              if ( j != 0 )
                ( (TDSSCktElement*) EMeter )->Set_Enabled(false);
              EMeter = (TEnergyMeterObj*) with0->EnergyMeters.Get_Next();
            }
          }
        }
        BuildYMatrix( WHOLEMATRIX, false, ActiveActor );
        DoSolveCmd();
        ActiveActor = 1;
        // Creates the other actors
        for ( int stop = Diak_Actors, DIdx = 2; DIdx <= stop; DIdx++)
        {
          New_Actor_Slot( );
          if ( DIdx == 2 )
            Dir = "";
          else
            Dir = "zone_" + IntToStr( DIdx - 1 ) + DIRSEP_STR;
          Proj_Dir = String( "compile \"" ) + Fileroot + DIRSEP_STR "Torn_Circuit" DIRSEP_STR + Dir + "master.dss\"";
          DSSExecutive[ActiveActor]->Set_Command(Proj_Dir);
          if ( DIdx > 2 )
            DSSExecutive[ActiveActor]->Set_Command(Links[DIdx - 2] + ".enabled=False");
          DSSExecutive[ActiveActor]->Set_Command("set controlmode=Off");
          DoSolveCmd();
          if ( SolutionAbort )
          {
            ErrorCode = 1;
            break;
          }
        }
        if ( ErrorCode != 0 )
          ErrorStr = "Error" + CRLF + "One or sub-systems cannot be compiled" + CRLF;
        else
        {
          ErrorStr = "Done";
        }
        prog_Str = prog_Str + ErrorStr;
      }
      break;
      case 3:
      {
        // Opens the link branches in the interconnected Circuit and recalculates the YBus
        // The opening happens by replacing the line with a very high series impedance
        ActiveActor = 1;
        prog_Str = prog_Str + CRLF + "- Opening link branches...";
        for ( int stop = Links.size() - 1, DIdx = 1; DIdx <= stop; DIdx++)
        {
          ActiveCircuit[ActiveActor]->SetElementActive( ((String) Links[DIdx] ) );
          ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_Enabled(false);
        }
        ActiveCircuit[ActiveActor]->Set_BusNameRedefined(false);
        BuildYMatrix( WHOLEMATRIX, false, ActiveActor );
        prog_Str = prog_Str + "Done";
        ErrorCode = 0;          // No error handling here
      }
      break;
      case 4:
      {                      // Creates the contours matrix
        ActiveActor = 1;
        prog_Str = prog_Str + CRLF + "- Building Contours...";
        // Builds the contour matrix
        ErrorCode = Calc_C_Matrix( &Links[0], Links.size() );
        if ( ErrorCode != 0 )
          ErrorStr = "Error" + CRLF + "One or more link branches are not lines" + CRLF;
        else
          ErrorStr = "Done";
        prog_Str = prog_Str + ErrorStr;
      }
      break;
      case 5:
      {                       // Builds the ZLL matrix
        ActiveActor = 1;
        prog_Str = prog_Str + CRLF + "- Building ZLL...";
        ErrorCode = Calc_ZLL( &Links[0], Links.size() );
        if ( ErrorCode != 0 )
          ErrorStr = "Error";
        else
          ErrorStr = "Done";
        prog_Str = prog_Str + ErrorStr;
      }
      break;
      case 6:
      {                      // Builds the ZCC matrix
        prog_Str = prog_Str + CRLF + "- Building ZCC...";
        Calc_ZCC( Links.size() );
        prog_Str = prog_Str + "Done";
      }
      break;
      case 7:
      {                      // Inverts ZCC to get Y4
        prog_Str = prog_Str + CRLF + "- Building Y4 ...";
        Calc_Y4( );
        prog_Str = prog_Str + "Done";
        // Moves back the link branches list into actor 1 for further use
        ActiveCircuit[1]->Link_Branches.resize( Links.size() );
        for ( int stop = Links.size() - 1, DIdx = 0; DIdx <= stop; DIdx++)
          ActiveCircuit[1]->Link_Branches[DIdx] = Links[DIdx];
      }
      break;
      case 8:
      {                      // Sends the index to the actors for uploading info
        prog_Str = prog_Str + CRLF + "- Assigning indexes to actors ...";
        SendIdx2Actors( );
        prog_Str = prog_Str + "Done";
      }
      break;
      case 9:
      {                      // Prints the statistics of the partitioning
        prog_Str = prog_Str + CRLF + CRLF + "Partitioning statistics";
        prog_Str = prog_Str + get_Statistics( );
        // Assigns the processor per actor
        for ( int stop = NumOfActors, DIdx = 1; DIdx <= stop; DIdx++)
        {
          ActorCPU[DIdx] = DIdx;
          if ( ActorHandle[DIdx] != NULL )
          {
            //ActorHandle[DIdx]->CPU = ActorCPU[DIdx];
            //ActorHandle[DIdx]->Priority = 6;
          }
        }
        // Compiles the interconnected Circuit for further calculations on actor 1
        ActiveActor = 1;
        prog_Str = prog_Str + CRLF + "- Closing link branches...";
        for ( int stop = Links.size() - 1, DIdx = 1; DIdx <= stop; DIdx++)
        {
          ActiveCircuit[ActiveActor]->SetElementActive( ((String) Links[DIdx] ) );
          ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Set_Enabled(true);
        }
        ActiveCircuit[ActiveActor]->Set_BusNameRedefined(false);
        BuildYMatrix( WHOLEMATRIX, false, ActiveActor );
        ActiveCircuit[ActiveActor]->Solution->SendCmd2Actors( INIT_ADIAKOPTICS );
        ADiak_Init = true;
      }
      break;
    default:
    {
    }
    }
    Local_State++;
    MQuit = ( Local_State > Num_States ) || ( ErrorCode != 0 );
  }
  ActiveActor = 1;
  if ( ErrorCode != 0 )
  {
    ErrorStr = "One or more errors found";
    ADiakoptics = false;
  }
  else
  {
    ErrorStr = "A-Diakoptics initialized";
    Parallel_enabled = true;
    ADiakoptics = true;
    ADiak_Init = false;    // Needed to force the subzones to remove VSource.Source
  }
  ProgressCmd = true;
  prog_Str = CRLF + prog_Str + CRLF + ErrorStr + CRLF;
  GlobalResult = ErrorStr;
  GlobalResult = prog_Str;
  // TEMc: TODO: should we report something here under FPC?
  // Davis: Done: This will add the needed report
  SolutionAbort = false;
}









