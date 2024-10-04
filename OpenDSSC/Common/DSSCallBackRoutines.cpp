
#pragma hdrstop

#include "DSSCallBackRoutines.h"



#include "ParserDel.h"
#include "DSSGlobals.h"
#include "Executive.h"
#include "Sysutils.h"
#include "CktElement.h"
#include <math.h>
#include "PDElement.h"

#include "System.h"


typedef unsigned int unsignedint;




TDSSCallBacks CallBackRoutines;



TParser CallBackParser;
String CB_ParamName, CB_Param;

/*====================================================================================================================*/


void __stdcall DoSimpleMsgCallback( char* S, unsignedint maxlen ) // Call back for user-written models

{
  DoSimpleMsg( ((String) S ), 9000 );
}

   /*These routines should work well with Fortran as well as C and VB*/
void __stdcall DoSimpleMsgCallback() // Call back for user-written models
{

}

/*====================================================================================================================*/


void __stdcall ParserLoad( char* S, unsignedint maxlen )
{
  CallBackParser.SetCmdString((String) S );
}

/*====================================================================================================================*/


void __stdcall ParserIntValue( int& i )
{
  /*# with CallBackParser do */
  {
    i = CallBackParser.MakeInteger_();
  }
}


/*====================================================================================================================*/


void __stdcall ParserDblValue( double& x )
{
  /*# with CallBackParser do */
  {
    x = CallBackParser.MakeDouble_();
  }
}

/*====================================================================================================================*/


void __stdcall ParserStrValue( char* S, unsignedint maxlen )

/*Copies null-terminated string into location pointed to by S up to the max chars specified*/
{
  /*# with CallBackParser do */
  {
	S = new char[maxlen + 1];
    StrLCopy( S, ((AnsiString) CB_Param ).c_str(), maxlen );
	S[maxlen] = 0;
  }
}

/*====================================================================================================================*/


int __stdcall ParserNextParam( char* ParamName, unsignedint maxlen )
{
  int result = 0;
  /*# with CallBackParser do */
  {
    CB_ParamName = CallBackParser.GetNextParam();
    CB_Param = CallBackParser.MakeString_();
  }
  StrLCopy( ParamName, ((AnsiString) CB_ParamName ).c_str(), maxlen ); // Copies up to Maxlen
  result = CB_Param.size( );
  return result;
}

/*====================================================================================================================*/


void __stdcall DoDSSCommandCallBack( char* S, unsignedint maxlen )
{
  SolutionAbort = false;
  DSSExecutive[ActiveActor]->Set_Command( (String) S );
}

/*====================================================================================================================*/


void __stdcall GetActiveElementBusNamesCallBack( char* Name1, unsignedint Len1, char* Name2, unsignedint Len2 )
  /*Get first two bus names of active Circuit Element for labeling graphs, etc.*/
  /*Coordinate must be defined else returns null string*/
{
  int BusIdx = 0;
  StrLCopy( Name1, ( PAnsiChar ) ( "" ), Len1 );  // Initialize to null
  StrLCopy( Name2, ( PAnsiChar ) ( "" ), Len2 );
  if ( ActiveCircuit[ActiveActor] != NULL )
  {
    if ( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL )
    {
      TDSSCktElement &CktElement = *(ActiveCircuit[ActiveActor]->get_FActiveCktElement());
     /*First bus*/
      BusIdx = CktElement.Terminals[1 - 1].BusRef;
      if ( BusIdx > 0 )
        /*# with ActiveCircuit[ActiveActor]->Buses^[BusIdx] do */
        
        if (ActiveCircuit[ActiveActor]->Buses[BusIdx - 1]->CoordDefined )
          StrLCopy( Name1, ((AnsiString) ActiveCircuit[ActiveActor]->BusList.Get( BusIdx ) ).c_str(), Len1 );
      /*Second bus*/
      BusIdx = CktElement.Terminals[2 - 1].BusRef;
      if ( BusIdx > 0 )
        /*# with ActiveCircuit[ActiveActor]->Buses^[BusIdx] do */
        if (ActiveCircuit[ActiveActor]->Buses[BusIdx - 1]->CoordDefined )
          StrLCopy( Name2, ((AnsiString) ActiveCircuit[ActiveActor]->BusList.Get( BusIdx ) ).c_str(), Len2 );
    }
  }  /*If ActiveCircuit[ActiveActor]*/
}

/*====================================================================================================================*/


void __stdcall GetActiveElementVoltagesCallBack( int& NumVoltages, pComplexArray V )
/*NumVoltages is size of the V buffer*/
{
  int i = 0, j = 0;
  if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActiveActor] do */
    {
      auto with0 = ActiveCircuit[ActiveActor];
      {
        /*# with ActiveCktElement do */
          auto with1 = with0->get_FActiveCktElement();
        {
          NumVoltages = min<long>( with1->Yorder, NumVoltages );  // reset buffer size
          for ( int stop = NumVoltages, i = 1; i <= stop; i++)
          {
            if ( (!ADiakoptics) || (ActiveActor == 1) )
              (V)[i - 1] = with0->Solution->NodeV[with1->NodeRef[i - 1]];
            else
              (V)[i - 1] = with0->Solution->VoltInActor1( with1->NodeRef[i - 1] );
          }
        }
      }
    }
}

/*====================================================================================================================*/


void __stdcall GetActiveElementCurrentsCallBack( int& NumCurrents, pComplexArray Curr, int ActorID )
{
  int i = 0;
  if (( ActiveCircuit[ActorID]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActorID] do */
    {
      auto with0 = ActiveCircuit[ActorID];
      /*# with ActiveCktElement do */
      {
        auto with1 = with0->get_FActiveCktElement();
        with1->ComputeIterminal( ActorID );
        NumCurrents = min<long>( with1->Yorder, NumCurrents ); // Reset to actual number of elements returned
        for ( int stop = NumCurrents, i = 1; i <= stop; i++)
          Curr[i - 1] = with1->Iterminal[i - 1];
      }
    }
}

/*====================================================================================================================*/


void __stdcall GetActiveElementLossesCallBack( complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID )
{
  TotalLosses = CZero;
  LoadLosses = CZero;
  NoLoadLosses = CZero;
  if (( ActiveCircuit[ActorID]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActorID] do */
    {
      auto with0 = ActiveCircuit[ActorID];
      /*# with ActiveCktElement do */
      {
        with0->get_FActiveCktElement()->GetLosses( TotalLosses, LoadLosses, NoLoadLosses, ActorID );
      }
    }
}

/*====================================================================================================================*/


void __stdcall GetActiveElementPowerCallBack( int Terminal, complex& TotalPower )
{
  TotalPower = CZero;
  if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActiveActor] do */
    {
      auto with0 = ActiveCircuit[ActiveActor];
      /*# with ActiveCktElement do */
      {
             //----ActiveTerminalIdx := Terminal;
        TotalPower = with0->get_FActiveCktElement()->Get_Power(Terminal, ActiveActor);
      }
    }
}

/*====================================================================================================================*/


void __stdcall GetActiveElementNumCustCallBack( int& Numcust, int& TotalCust )
{
  TPDElement PDElem;
  Numcust = 0;
  TotalCust = 0;
  if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
    if ( dynamic_cast< TPDElement* >( ActiveCircuit[ActiveActor]->get_FActiveCktElement() ) )
    {
      PDElem = *( ( TPDElement* ) ActiveCircuit[ActiveActor]->get_FActiveCktElement() );
      Numcust = PDElem.BranchNumCustomers;
      TotalCust = PDElem.BranchTotalCustomers;
    }
}

/*====================================================================================================================*/


void __stdcall GetActiveElementNodeRefCallBack( int Maxsize, pIntegerArray NodeReferenceArray )// calling program must allocate

{
  int i = 0;
  if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActiveActor] do */
    {
      auto with0 = ActiveCircuit[ActiveActor];
      /*# with ActiveCktElement do */
      {
          auto with1 = with0->get_FActiveCktElement();
        for ( int stop = min<long>(  with1->Yorder,  Maxsize ), i = 1; i <= stop; i++)
          NodeReferenceArray[i - 1] = with1->NodeRef[i - 1];
      }
    }
}

/*====================================================================================================================*/


int __stdcall GetActiveElementBusRefCallBack( int Terminal )
{
  int result = 0;
  result = 0;
  if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActiveActor] do */
    {
      auto with0 = ActiveCircuit[ActiveActor];
      /*# with ActiveCktElement do */
      {
        result = with0->get_FActiveCktElement()->Terminals[Terminal - 1].BusRef;
      }
    }
  return result;
}

/*====================================================================================================================*/


void __stdcall GetActiveElementTerminalInfoCallBack( int& NumTerminals, int& NumConds, int& NumPhases )
{
  if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActiveActor] do */
    {
      auto with0 = ActiveCircuit[ActiveActor];
      /*# with ActiveCktElement do */
      {
        NumTerminals = with0->get_FActiveCktElement()->Get_NTerms();
        NumConds = with0->get_FActiveCktElement()->Get_NConds();
        NumPhases = with0->get_FActiveCktElement()->Get_NPhases();
      }
    }
}

/*====================================================================================================================*/


void __stdcall GetPtrToSystemVarrayCallBack( void*& V, int& iNumNodes ) // Returns pointer to Solution.V and size

{
  if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActiveActor] do */
    {
      auto with0 = ActiveCircuit[ActiveActor];
      /*# with ActiveCktElement do */
      {
        V = &(with0->Solution->NodeV[0]);  // Return Pointer to Node Voltage array
        iNumNodes = with0->NumNodes;
      }
    }
}

/*====================================================================================================================*/


int __stdcall GetActiveElementIndexCallBack( )
    /*Usually just checking to see if this result >0*/
{
  int result = 0;
  result = 0;
  if (( ActiveCircuit[ActiveActor] != NULL ) )
    if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
      result = ( (TDSSObject*) ActiveCircuit[ActiveActor]->get_FActiveCktElement() )->ClassIndex;
  return result;
}

/*====================================================================================================================*/


bool __stdcall IsActiveElementEnabledCallBack( )
{
  bool result = false;
  result = false;
  if (( ActiveCircuit[ActiveActor] != NULL ) )
    if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
      result = ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Get_Enabled();
  return result;
}

/*====================================================================================================================*/


bool __stdcall IsBusCoordinateDefinedCallback( int BusRef, int ActorID )
{
  bool result = false;
  result = false;
  if (( ActiveCircuit[ActorID] != NULL ) && ( BusRef > 0 ) )
    result = ActiveCircuit[ActorID]->Buses[BusRef - 1]->CoordDefined;
  return result;
}

/*====================================================================================================================*/


void __stdcall GetBusCoordinateCallback( int BusRef, double& x, double& y, int ActorID )
{
  x = 0.0;
  y = 0.0;
  if (( ActiveCircuit[ActorID] != NULL ) && ( BusRef > 0 ) )
  {
    x = ActiveCircuit[ActorID]->Buses[BusRef - 1]->x;
    y = ActiveCircuit[ActorID]->Buses[BusRef - 1]->y;
  }
}

/*====================================================================================================================*/


double __stdcall GetBuskVBaseCallback( int BusRef, int ActorID )
{
  double result = 0.0;
  result = 0.0;
  if (( ActiveCircuit[ActorID] != NULL ) && ( BusRef > 0 ) )
  {
    result = ActiveCircuit[ActorID]->Buses[BusRef - 1]->kVBase;
  }
  return result;
}

/*====================================================================================================================*/


double __stdcall GetBusDistFromMeterCallback( int BusRef, int ActorID )
{
  double result = 0.0;
  result = 0.0;
  if (( ActiveCircuit[ActorID] != NULL ) && ( BusRef > 0 ) )
  {
    result = ActiveCircuit[ActorID]->Buses[BusRef - 1]->DistFromMeter;
  }
  return result;
}

/*====================================================================================================================*/


void __stdcall GetDynamicsStructCallBack( void*& DynamicsStruct, int ActorID )
{
  if (( ActiveCircuit[ActorID] != NULL ) )
  {
    DynamicsStruct = &ActiveCircuit[ActorID]->Solution->DynaVars;
  }
}

/*====================================================================================================================*/


double __stdcall GetStepSizeCallBack( int ActorID )
{
  double result = 0.0;
  result = 0.0;
  if (( ActiveCircuit[ActorID] != NULL ) )
  {
    result = ActiveCircuit[ActorID]->Solution->DynaVars.h;
  }
  return result;
}

/*====================================================================================================================*/


double __stdcall GetTimeSecCallBack( int ActorID )
{
  double result = 0.0;
  result = 0.0;
  if (( ActiveCircuit[ActorID] != NULL ) )
  {
    result = ActiveCircuit[ActorID]->Solution->DynaVars.T;
  }
  return result;
}

/*====================================================================================================================*/


double __stdcall GetTimeHrCallBack( int ActorID )
{
  double result = 0.0;
  result = 0.0;
  if (( ActiveCircuit[ActorID] != NULL ) )
  {
    result = ActiveCircuit[ActorID]->Solution->DynaVars.dblHour;
  }
  return result;
}

/*====================================================================================================================*/


void __stdcall GetPublicDataPtrCallBack( void*& pPublicData, int& PublicDataBytes, int ActorID )
{
  if (( ActiveCircuit[ActorID]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActorID] do */
    {
      auto with0 = ActiveCircuit[ActorID];
      /*# with ActiveCktElement do */
      {
        pPublicData = with0->get_FActiveCktElement()->PublicDataStruct;
        PublicDataBytes = with0->get_FActiveCktElement()->PublicDataSize;
      }
    }
}


int __stdcall GetActiveElementNameCallBack( char* FullName, unsignedint maxlen, int ActorID )
/*Maxlen is num of chars the calling program allocates for the string*/
{
  int result = 0;
  String S;
  result = 0;
  if (( ActiveCircuit[ActorID]->get_FActiveCktElement() != NULL ) )
    /*# with ActiveCircuit[ActorID] do */
    {
      auto with0 = ActiveCircuit[ActorID];
      /*# with ActiveCktElement do */
      {
        S = ( (TDSSObject*) with0->get_FActiveCktElement() )->ParentClass->get_myClass_name() + "." + ((TDSSObject*)with0->get_FActiveCktElement())->get_Name();
        StrLCopy( FullName, ((AnsiString) S ).c_str(), maxlen );
        result = maxlen;
      }
    }
  return result;
}


void* __stdcall GetActiveElementPtrCallBack( int ActorID )  // Returns pointer to active circuit element

{
  void* result = NULL;
  result = ((void*) ActiveCircuit[ActorID]->get_FActiveCktElement() );
  return result;
}


int __stdcall ControlQueuePushCallBack( const int Hour, const double Sec, const int Code, const int ProxyHdl, void* Owner, int ActorID )
{
  int result = 0;
  result = ActiveCircuit[ActorID]->ControlQueue.Push( Hour, Sec, (EControlAction) Code, ProxyHdl, (TControlElem*) Owner, ActorID );
  return result;
}


void __stdcall GetResultStrCallBack( char* S, unsignedint maxlen )
{
  StrLCopy( S, ((AnsiString) GlobalResult ).c_str(), maxlen );
}

/*====================================================================================================================*/

/*Initialize Function Interface variables for user-Written Callbacks*/

void DSSCallBackRoutines_initialization()
{
  /*# with CallBackRoutines do */
  {
    auto &with0 = CallBackRoutines;
    //with0.MsgCallBack = (void*);// &((int)DoSimpleMsgCallback()); // for user-written callbacks
    with0.GetIntValue = ParserIntValue;
    with0.GetDblValue = ParserDblValue;
    with0.GetStrValue = ParserStrValue;
    with0.LoadParser = ParserLoad;
    with0.NextParam = ParserNextParam;
    with0.DoDSSCommand = DoDSSCommandCallBack;
    with0.GetActiveElementBusNames = GetActiveElementBusNamesCallBack;
    with0.GetActiveElementVoltages = GetActiveElementVoltagesCallBack;
    with0.GetActiveElementCurrents = GetActiveElementCurrentsCallBack;
    with0.GetActiveElementLosses = GetActiveElementLossesCallBack;
    with0.GetActiveElementPower = GetActiveElementPowerCallBack;
    with0.GetActiveElementNumCust = GetActiveElementNumCustCallBack;
    with0.GetActiveElementNodeRef = GetActiveElementNodeRefCallBack;
    with0.GetActiveElementBusRef = GetActiveElementBusRefCallBack;
    with0.GetActiveElementTerminalInfo = GetActiveElementTerminalInfoCallBack;
    with0.GetPtrToSystemVarray = GetPtrToSystemVarrayCallBack;
    with0.GetActiveElementIndex = GetActiveElementIndexCallBack;
    with0.IsActiveElementEnabled = IsActiveElementEnabledCallBack;
    with0.IsBusCoordinateDefined = IsBusCoordinateDefinedCallback;
    with0.GetBusCoordinate = GetBusCoordinateCallback;
    with0.GetBuskVBase = GetBuskVBaseCallback;
    with0.GetBusDistFromMeter = GetBusDistFromMeterCallback;

         // Added 4-9-2012
    with0.GetDynamicsStruct = GetDynamicsStructCallBack;
    with0.GetStepSize = GetStepSizeCallBack;
    with0.GetTimeSec = GetTimeSecCallBack;
    with0.GetTimeHr = GetTimeHrCallBack;
    with0.GetPublicDataPtr = GetPublicDataPtrCallBack;
    with0.GetActiveElementName = GetActiveElementNameCallBack;
    with0.GetActiveElementPtr = GetActiveElementPtrCallBack;
    with0.ControlQueuePush = ControlQueuePushCallBack;
    with0.GetResultStr = GetResultStrCallBack;
  }
/*====================================================================================================================*/
}

void DSSCallBackRoutines_finalization()
{
}

class DSSCallBackRoutines_unit
{
public:
DSSCallBackRoutines_unit()
{
  DSSCallBackRoutines_initialization();
}
~DSSCallBackRoutines_unit(){ DSSCallBackRoutines_finalization(); }
};
DSSCallBackRoutines_unit _DSSCallBackRoutines_unit;








