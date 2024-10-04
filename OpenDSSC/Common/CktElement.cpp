

#pragma hdrstop


#include "CktElement.h"

#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "d2c_structures.h"
#include <math.h>


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

namespace CktElement
{
    TDSSCktElement::TDSSCktElement(String ClassName) : inherited(ClassName) {}
    TDSSCktElement::TDSSCktElement() {}

    int TDSSCktElement::InitializeStates(int ActorID)
    {
        //  Should not get to this point, or error ocurred!
        return 0;
    }

    TDSSCktElement::TDSSCktElement(TDSSClass* ParClass )
     : inherited(ParClass),
       FEnabled(false),
       FEnabledProperty(0),
       FActiveTerminal(0),
       FYPrimInvalid(false),
       FHandle(0),
       Fnterms(0),
       Fnconds(0),
       Fnphases(0),
       BusIndex(0),
       FYprimFreq(0.0),
       NodeRef(0),
       Yorder(0),
       LastTerminalChecked(0),
       Checked(false),
       HasEnergyMeter(false),
       HasSensorObj(false),
       IsIsolated(false),
       HasControl(false),
       IsMonitored(false),
       IsPartofFeeder(false),
       Drawn(false),
       HasOCPDevice(false),
       HasAutoOCPDevice(false),
       HasSwtControl(false),
       BaseFrequency(0.0),
       PublicDataSize(0),
       PublicDataStruct(NULL)
    {
      int i = 0;
      //
      YPrim_Series = NULL;
      YPrim_Shunt = NULL;
      YPrim = NULL;
      Terminals.clear();
      FBusNames.clear();
      Vterminal.clear();
      Iterminal.clear();  // present value of terminal current
      ComplexBuffer.clear();
      PublicDataStruct = NULL;   // pointer to fixed struct of data to be shared
      PublicDataSize = 0;
      FHandle = - 1;
      BusIndex = 0;
      Fnterms = 0;
      Fnconds = 0;
      Fnphases = 0;
      DSSObjType = 0;
      Yorder = 0;
      Set_YprimInvalid(ActiveActor,true);
      FEnabled = true;
      HasEnergyMeter = false;
      HasSensorObj = false;
      HasOCPDevice = false;
      HasAutoOCPDevice = false;
      HasSwtControl = false;
      HasControl = false;
      IsMonitored = false;
      IsPartofFeeder = false;
      IsIsolated = false;
      Drawn = false;
      GFM_Mode = false;
         // Make list for a small number of controls with an increment of 1
      ControlElementList = TPointerList( 1 );
      FActiveTerminal = 1;
      LastTerminalChecked = 0;

    /*    Indicates which solution Itemp is computed for    */
      IterminalSolutionCount = new int[CPU_Cores + 1];
      for ( int stop = CPU_Cores, i = 0; i <= stop; i++)
        IterminalSolutionCount[i] = - 1;
      BaseFrequency = ActiveCircuit[ActiveActor]->Fundamental;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    TDSSCktElement::~TDSSCktElement( )
    {
      int i = 0;
      Terminals.clear();
      FBusNames.clear();
      Iterminal.clear();
      Vterminal.clear();
      NodeRef.clear();
      ComplexBuffer.clear();
      //if (( ControlElementList != NULL ) )


        /*Dispose YPrims*/
      //if ( YPrim_Series != NULL )
       // delete YPrim_Series; //YPrim_Series->~TcMatrix();
      //if ( YPrim_Shunt != NULL )
       // delete YPrim_Shunt; //YPrim_Shunt->~TcMatrix();
      //if ( YPrim != NULL )
       // delete YPrim; // YPrim->~TcMatrix();
      // todo check:  inherited::Destroy;

      delete[] IterminalSolutionCount;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    int TDSSCktElement::get_FActiveTerminal()
    {
        return FActiveTerminal;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


    bool TDSSCktElement::Get_YprimInvalid( int ActorID, const bool Value)
    {
      bool result = false;
      result = FYPrimInvalid;
      return result;
    }


    void TDSSCktElement::Set_YprimInvalid( int ActorID, const bool Value )
    {
      FYPrimInvalid = Value;
      if ( Value )
      {

            // If this device is in the circuit, then we have to rebuild Y on a change in Yprim
        if ( FEnabled )
          ActiveCircuit[ActorID]->Solution->SystemYChanged = true;
      }
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::Set_ActiveTerminal( int Value )
    {
      if ( ( Value > 0 ) && ( Value <= Fnterms ) )
      {
        FActiveTerminal = Value;
        ActiveTerminal = &(Terminals[Value - 1]);
      }
    }

    int TDSSCktElement::Get_ActiveTerminal()
    {
        return FActiveTerminal;
    }


    void TDSSCktElement::Set_Handle( int Value )
    {
      FHandle = Value;
    }


    bool TDSSCktElement::Get_ConductorClosed( int Index, int ActorID, bool Value)

    // return state of selected conductor
    // if index=0 return true if all phases closed, else false

    {
      bool result = false;
      int i = 0;
      if ( Index == 0 )
      {
        result = true;
        for ( int stop = Fnphases, i = 1; i <= stop; i++)
        {
          if ( ! (Terminals[(FActiveTerminal) - 1].Conductors[i - 1].Closed) )
          {
            result = false;
            break;
          }
        }
      }
      else
        if ( ( Index > 0 ) && ( Index <= Fnconds ) )
          result = Terminals[(FActiveTerminal) - 1].Conductors[(Index) - 1].Closed;
        else
          result = false;
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    int TDSSCktElement::Get_Handle()
    {
        return FHandle;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    bool TDSSCktElement::Get_Enabled()
    {   
        return FEnabled;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    double TDSSCktElement::Get_YPrimFreq()
    {
        return FYprimFreq;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    int TDSSCktElement::Get_NTerms()
    {
        return Fnterms;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    int TDSSCktElement::Get_NConds()
    {
        return Fnconds;

    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    int TDSSCktElement::Get_NPhases()
    {
        return Fnphases;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    void TDSSCktElement::Set_ConductorClosed( int Index, int ActorID, bool Value )
    {
      int i = 0;
      if ( Index == 0 )
      {  // Do all conductors
        for ( int stop = Fnphases, i = 1; i <= stop; i++)
          Terminals[(FActiveTerminal) - 1].Conductors[i- 1].Closed = Value;
        ActiveCircuit[ActorID]->Solution->SystemYChanged = true;  // force Y matrix rebuild
        Set_YprimInvalid(ActorID,true);
      }
      else
      {
        if ( ( Index > 0 ) && ( Index <= Fnconds ) )
        {
          Terminals[(FActiveTerminal) - 1].Conductors[(Index) - 1].Closed = Value;
          ActiveCircuit[ActorID]->Solution->SystemYChanged = true;
          Set_YprimInvalid(ActorID,true);
        }
      }
    }



    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::Set_Nconds( int Value )
    {
    // Check for an almost certain programming error
      if ( Value <= 0 )
      {
        DoSimpleMsg( "Invalid number of terminals (" + to_string(Value) + ") for " + ParentClass->get_myClass_name() + "." + get_Name() , 749 );
        return;
      }
      if ( Value != Fnconds )
        ActiveCircuit[ActiveActor]->Set_BusNameRedefined(true);
      Fnconds = Value;
      Set_NTerms( Fnterms );  // ReallocTerminals    NEED MORE EFFICIENT WAY TO DO THIS
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::Set_NPhases( int Value )
    {
      if ( Value > 0 )
        Fnphases = Value;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::Set_NTerms( int Value )
    {
      int i = 0;
      pStringArray NewBusNames;

    // Check for an almost certain programming error
      if ( Value <= 0 )
      {
        DoSimpleMsg( ("Invalid number of terminals (" + to_string(Value) + ") for " + ParentClass->get_myClass_name() + "." + get_Name()), 749 );
        return;
      }

    // If value is same as present value, no reallocation necessary;
    // If either Nterms or Nconds has changed then reallocate
      if ( ( Value != Fnterms ) || ( Value * Fnconds != Yorder ) )
      {

            /*Sanity Check*/
        if ( Fnconds > 101 )
        {
          DoSimpleMsg( "Warning: Number of conductors is very large (" + to_string(Fnconds) +
              ") for Circuit Element: " + ParentClass->get_myClass_name() + "." + get_Name() + "." +
              "Possible error in specifying the Number of Phases for element.", 750 );
        }


             /*ReAllocate BusNames    */
             // because they are Strings, we have to do it differently
        if ( Value < Fnterms )
          FBusNames.resize(Value);  // Keeps old values; truncates storage
        else
        {
          if ( FBusNames.empty() )
          {
                    // First allocation
                      /*  Always allocate  arrays of strings with AllocMem so that the pointers are all nil
                         else Delphi thinks non-zero values are pointing to an existing string.*/
            FBusNames.resize( Value ); //    fill with zeros or strings will crash
            for ( int stop = Value, i = 1; i <= stop; i++)
              FBusNames[i- 1] = get_Name() + "_" + IntToStr( i );  // Make up a bus name to stick in.
                         // This is so devices like transformers which may be defined on multiple commands
                         // will have something in the BusNames array.
          }
          else
          {

            NewBusNames = new string[ Value ];  // make some new space
            for ( int stop = Fnterms, i = 1; i <= stop; i++)
              NewBusNames[i - 1] = FBusNames[i- 1];   // copy old into new
//            for ( int stop = Fnterms, i = 1; i <= stop; i++)     
//              FBusNames[i - 1] = "";   // decrement usage counts by setting to nil string
            for ( int stop = Value, i = Fnterms + 1; i <= stop; i++)
              NewBusNames[i - 1] = get_Name() + "_" + IntToStr( i );  // Make up a bus name to stick in.

            FBusNames.resize(Value);
            if (Value >= (Fnterms + 1))
            {
                for (int stop = Value, i = Fnterms + 1; i <= stop; i++)
                    FBusNames[i- 1] = NewBusNames[i - 1];
            }
            delete[] NewBusNames;
          }
        }

             /*Reallocate Terminals if Nconds or NTerms changed*/
        if (Terminals.size() != 0)
            Terminals.clear();
        Terminals.resize(Value + 1);
        Fnterms = Value;    // Set new number of terminals
        Yorder = Fnterms * Fnconds;
        Vterminal.resize((Yorder));
        Iterminal.resize((Yorder));
        ComplexBuffer.resize((Yorder) + 1 );    // used by both PD and PC elements
        for ( int stop = Value, i = 1; i <= stop; i++)
          Terminals[i- 1] = TPowerTerminal( Fnconds );
      }
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::Set_Enabled( bool Value )
    //  If disabled, but defined, just have to processBusDefs.  Adding a bus OK
    // If being removed from circuit, could remove a node or bus so have to rebuild
    //VAR
    //  NumNodesSaved:Integer;

    {
      /*# with ActiveCircuit[ActiveActor] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        if ( Value != FEnabled )
        {  // don't change unless this represents a change

           // This code was too cute and prevented rebuilding of meter zones
           // Removed 7/24/01
           /*IF Value THEN Begin

             NumNodesSaved := NumNodes;
             ProcessBusDefs;     // If we create new nodes, force rebuild of bus lists
             If NumNodes>NumNodesSaved Then BusNameRedefined := True
             ELSE Solution->SystemYChanged:= True; //  just rebuild of yPrim
           End
           ELSE   BusNameRedefined := True;  // Force Rebuilding of BusLists anyway
           */
          FEnabled = Value;
          with0->Set_BusNameRedefined(true);  // forces rebuilding of Y matrix and bus lists
        }
      }
    }



    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    int TDSSCktElement::GetYPrim( TcMatrix& Ymatrix, int Opt )
    //returns pointer to actual YPrim

    {
      int result = 0;
      switch ( Opt )
      {
        case ALL_YPRIM:
          Ymatrix = *YPrim;
        break;
        case SERIES:
          Ymatrix = *YPrim_Series;
        break;
        case SHUNT:
          Ymatrix = *YPrim_Shunt;
        break;
      }
      result = 0;
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    pComplexArray TDSSCktElement::GetYPrimValues( int Opt )
    // Return a pointer to the Beginning the storage arrays for fast access

    {
      pComplexArray result;
      int Norder = 0;
      result = NULL;
      switch ( Opt )
      {
        case ALL_YPRIM:
          if ( YPrim != NULL )
            result = YPrim->GetValuesArrayPtr( Norder );
        break;
        case SERIES:
          if ( YPrim_Series != NULL )
            result = YPrim_Series->GetValuesArrayPtr( Norder );
        break;
        case SHUNT:
          if ( YPrim_Shunt != NULL )
            result = YPrim_Shunt->GetValuesArrayPtr( Norder );
        break;
      }
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::GetCurrents( pComplexArray Curr, int ActorID )  //Get present value of terminal Curr for reports

    {
      DoErrorMsg( "Something is Wrong.  Got to base CktElement GetCurrents for Object:" + CRLF + Get_myPName() + "." + get_Name(), "N/A", "Should not be able to get here. Probable Programming Error.", 751 );
    }


    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::GetInjCurrents( pComplexArray Curr, int ActorID )
    {
      DoErrorMsg( "Something is Wrong.  Got to base CktElement GetInjCurrents for Object:" + CRLF + Get_myPName() + "." + get_Name(), "****", "Should not be able to get here. Probable Programming Error.", 752 );
    }


    void TDSSCktElement::GetLosses( complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID )
    {
      /*For no override, Default behavior is:
        Just return total losses and set LoadLosses=total losses and noload losses =0*/
      TotalLosses = Get_Losses(ActorID);  // Watts, vars
      LoadLosses = TotalLosses;
      NoLoadLosses = CZero;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    int TDSSCktElement::InjCurrents( int ActorID )  // Applies to PC Elements

    {
      int result = 0;
      result = 0;
      DoErrorMsg( ( "Improper call to InjCurrents for Element: " + get_Name() + "." ), "****", "Called CktElement class base function instead of actual.", 753 );
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::SetNodeRef( int iTerm, pIntegerArray NodeRefArray )

    // Also allocates VTemp  & Itemp

    {
      int Size = 0, Size2 = 0;
    // Allocate NodeRef and move new values into it.
      Size = Yorder * sizeof( longInt );
      Size2 = sizeof( longInt ) * Fnconds;  // Size for one terminal
      NodeRef.resize(Yorder);  // doesn't do anything if already properly allocated

      Move( NodeRefArray, &(NodeRef[((iTerm - 1) * Fnconds)]), Size2);  // Zap
      Move( NodeRefArray, &(Terminals[iTerm - 1].TermNodeRef[0]), Size2);  // Copy in Terminal as well

    // Allocate temp array used to hold voltages and currents for calcs
      Vterminal.resize(Yorder);
      Iterminal.resize(Yorder);
      ComplexBuffer.resize((Yorder) + 1 );
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    String TDSSCktElement::Get_FirstBus( )
    {
      String result;
      if ( Fnterms > 0 )
      {
        BusIndex = 1;
        result = FBusNames[BusIndex - 1];
      }
      else
        result = "";
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    String TDSSCktElement::Get_NextBus( )
    {
      String result;
      result = "";
      if ( Fnterms > 0 )
      {
        BusIndex++;
        if ( BusIndex <= Fnterms )
          result = FBusNames[(BusIndex) - 1];
        else
          BusIndex = Fnterms;
      }
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    String TDSSCktElement::GetBus( int i )  // Get bus name by index

    {
      String result;
      if ( i <= Fnterms )
        result = FBusNames[i - 1];
      else
        result = "";
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::SetBus( int i, const String S ) // Set bus name by index

    {
      if ( i <= Fnterms )
      {
          if (FBusNames.size() < Fnterms)
              FBusNames.resize(Fnterms);
        FBusNames[i - 1] = LowerCase( S );
        ActiveCircuit[ActiveActor]->Set_BusNameRedefined(true);  // Set Global Flag to signal circuit to rebuild busdefs
      }
      else
        DoSimpleMsg( Format( "Attempt to set bus name for non-existent circuit element terminal(%d): \"%s\"",  i, S.c_str()), 7541 );
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::Set_Freq( double Value )
    {
      if ( Value > 0.0 )
        FYprimFreq = Value;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::RecalcElementData( int ActorID )
    {
      DoSimpleMsg( "Virtual proc RecalcElementData in Base CktElement Class Called for Device = \"" + get_Name() + "\"", 754 );
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::CalcYPrim( int ActorID )
    {
      if ( YPrim_Series != NULL )
        DoYprimCalcs( YPrim_Series );
      if ( YPrim_Shunt != NULL )
        DoYprimCalcs( YPrim_Shunt );
      if ( YPrim != NULL )
        DoYprimCalcs( YPrim );
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::ComputeIterminal( int ActorID )
    {

    // to save time, only recompute if a different solution than last time it was computed.
      if ( IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount && Yorder > 0)
      {
        GetCurrents( &(Iterminal[0]), ActorID);
        IterminalSolutionCount[ActorID] = ActiveCircuit[ActorID]->Solution->SolutionCount;
      }
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    double TDSSCktElement::MaxTerminalOneIMag( int ActorID )

    /* Get max of phase currents on the first terminal; Requires computing Iterminal
    */
    {
      double result = 0.0;
      int i = 0;
      result = 0.0;
      if ( FEnabled )
        for ( int stop = Fnphases, i = 1; i <= stop; i++)
          result = max( result, cabs( Iterminal[i - 1] ) );
    //    Result := Sqrt(MaxI);  // just do the sqrt once and save a little time
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::Get_Current_Mags( pDoubleArray cMBuffer, int ActorID )
    {
      int i = 0;
      for ( int stop = Fnphases, i = 1; i <= stop; i++)
        cMBuffer[i - 1] = cabs( Iterminal[i - 1] );
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    complex TDSSCktElement::Get_Power( int idxTerm, int ActorID )    // Get total complex power in active terminal

    {
      complex result = cmplx(0,0);
      complex cPower = cmplx(0,0);
      int i = 0, k = 0, n = 0;
      cPower = CZero;
      Set_ActiveTerminal(idxTerm);
      if ( FEnabled )
      {
        ComputeIterminal( ActorID );

        // Method: Sum complex power going into phase conductors of active terminal
        /*# with ActiveCircuit[ActorID].Solution do */
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          k = ( idxTerm - 1 ) * Fnconds;
          for ( int stop = Fnconds, i = 1; i <= stop; i++)     // 11-7-08 Changed from Fnphases - was not accounting for all conductors
          {
            n = ActiveTerminal->TermNodeRef[i - 1]; // don't bother for grounded node
            double re = with0->Solution->NodeV[n].re;
            double im = with0->Solution->NodeV[n].im;
            if ( (!ADiakoptics) || (ActorID == 1))
            {
              if ( n > 0 )
                  caccum(cPower, cmul(with0->Solution->NodeV[n], conjg(Iterminal[(k) + i - 1])));
              else
                if ( n > 0 )
                  caccum( cPower, cmul(with0->Solution->VoltInActor1( n ), conjg( Iterminal[(k) + i - 1] ) ) );
            }
          }
        }

           /*If this is a positive sequence circuit, then we need to multiply by 3 to get the 3-phase power*/
        if ( with0->PositiveSequence )
          cPower = cmulreal( cPower, 3.0 );
      }
      result = cPower;
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::LossCalc( int& ActorID, complex& cLoss, int& k, int& n )  // of k-th conductor

    {
      /*# with ActiveCircuit[ActorID].Solution do */
      TDSSCircuit* with0 = ActiveCircuit[ActorID];
      {
        n = (NodeRef)[k - 1];
        if ( n > 0 )
        {
          if ( (! ADiakoptics) || ( ActorID == 1 ) )
          {
            if (with0->PositiveSequence )
              caccum( cLoss, cmulreal( cmul( with0->Solution->NodeV[n], conjg( (Iterminal)[(k) - 1] ) ), 3.0 ) );
            else
              caccum( cLoss, cmul( with0->Solution->NodeV[n], conjg( (Iterminal)[(k) - 1] ) ) );
          }
          else
          {
                // In the context of actor 1 voltages
            if ( with0->PositiveSequence )
              caccum( cLoss, cmulreal( cmul( with0->Solution->VoltInActor1( n ), conjg( (Iterminal)[(k) - 1] ) ), 3.0 ) );
            else
              caccum( cLoss, cmul(with0->Solution->VoltInActor1( n ), conjg( (Iterminal)[(k) - 1] ) ) );
          }
        }
      }
    }


    complex TDSSCktElement::Get_Losses( int ActorID )
    // get total losses in circuit element, all phases, all terminals.
    // Returns complex losses (watts, vars)

    {
      complex result;
      complex cLoss;
      int k = 0

       /*Local nested Procedure*/, i = 0, j = 0, n = 0;
      cLoss = CZero;
      if ( FEnabled && Yorder > 0)
      {
        ComputeIterminal( ActorID );

        // Method: Sum complex power going into all conductors of all terminals
             /*Special for AutoTransformer - sum based on NPhases rather then Yorder*/
        if ( ( ( CLASSMASK & this->DSSObjType ) ) == AUTOTRANS_ELEMENT )
        {
          k = 0;
          for ( int stop = Get_NTerms(), j = 1; j <= stop; j++)
          {
            for ( int stop = Get_NPhases(), i = 1; i <= stop; i++)
            {
              k++;
              LossCalc(ActorID, cLoss, k, n);
            }
            k += Get_NPhases();
          }
        }
        else  // for all other elements
        {
          for ( int stop = Yorder, k = 1; k <= stop; k++)
          {
            LossCalc(ActorID, cLoss, k, n);
          }
        }
      }
      result = cLoss;
      return result;
    }


    double TDSSCktElement::Get_MaxVoltage( int idxTerm, int ActorID )
    /*Get Voltage at the specified terminal 09/17/2019*/
    {
      double result = 0.0;
      complex Volts, VN{}, cPower{};
      int ClassIdx = 0, i = 0, k = 0, l = 0, m = 0, nrefN = 0, Nref = 0;
      double MaxCurr = 0.0, CurrMag = 0.0;
      int MaxPhase = 0;
      Set_ActiveTerminal(idxTerm);   // set active Terminal
      cPower = CZero;
      if ( FEnabled )
      {
        ComputeIterminal( ActorID );

        // Method: Checks what's the phase with maximum current
        // retunrs the voltage for that phase
        MaxCurr = 0.0;
        MaxPhase = 1;  // Init this so it has a non zero value
        k = ( idxTerm - 1 ) * Fnconds; // starting index of terminal
        for ( int stop = Fnphases, i = 1; i <= stop; i++)
        {
          CurrMag = cabs( Iterminal[((k) + i) - 1] );
          if ( CurrMag > MaxCurr )
          {
            MaxCurr = CurrMag;
            MaxPhase = i;
          }
        }
        ClassIdx = DSSObjType & CLASSMASK;              // gets the parent class descriptor (int)
        Nref = (ActiveTerminal->TermNodeRef)[MaxPhase - 1]; // reference to the phase voltage with the max current
        nrefN = (ActiveTerminal->TermNodeRef)[Fnconds - 1];  // reference to the ground terminal (GND or other phase)
        /*# with ActiveCircuit[ActorID].Solution do */     // Get power into max phase of active terminal
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          if ( (!ADiakoptics ) || (ActorID == 1))
          {
            if ( ! ( ClassIdx == XFMR_ELEMENT ) )  // Only for transformers
              Volts = with0->Solution->NodeV[Nref];
            else
              Volts = csub(with0->Solution->NodeV[Nref], with0->Solution->NodeV[nrefN] );
          }
          else
          {
            if ( ! ( ClassIdx == XFMR_ELEMENT ) )  // Only for transformers
              Volts = with0->Solution->VoltInActor1( Nref );
            else
              Volts = csub(with0->Solution->VoltInActor1( Nref ), with0->Solution->VoltInActor1( nrefN ) );
          }
        }
      }
      result = cabs( Volts );
      return result;
    }
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    double TDSSCktElement::Get_MaxCurrentAng(int idxTerm, int ActorID)
    {
        int i, k, nref{}, MaxPhase = 0;
        double CurrAng, MaxCurr, CurrMag, Result = 0.0;

        Set_ActiveTerminal(idxTerm);
        MaxCurr             = 0.0;
        CurrAng             = 0.0;
        if (FEnabled)
        {
            ComputeIterminal(ActorID);
            MaxPhase    = 1;
            k           = (idxTerm - 1) * Fnconds;
            for (i = 1; i <= Fnphases; i++)
            {
                CurrMag = cabs(Iterminal[(k) + i - 1]);
                if (CurrMag > MaxCurr)
                {
                    MaxCurr     = CurrMag;
                    CurrMag     = cang(Iterminal[(k) + i - 1]);
                    MaxPhase    = i;
                }
            }
        }
        return Result;
    }
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    double TDSSCktElement::Get_MaxVoltageAng(int idxTerm, int ActorID)
    {
        double result = 0.0;
        complex Volts, VN{}, cPower;
        int ClassIdx = 0, i = 0, k = 0, l = 0, m = 0, nrefN = 0, Nref = 0;
        double MaxCurr = 0.0, CurrMag = 0.0;
        int MaxPhase = 0;
        Set_ActiveTerminal(idxTerm);   // set active Terminal
        cPower = CZero;
        if (FEnabled)
        {
            ComputeIterminal(ActorID);

            // Method: Checks what's the phase with maximum current
            // retunrs the voltage for that phase
            MaxCurr = 0.0;
            MaxPhase = 1;  // Init this so it has a non zero value
            k = (idxTerm - 1) * Fnconds; // starting index of terminal
            for (int stop = Fnphases, i = 1; i <= stop; i++)
            {
                CurrMag = cabs(Iterminal[((k) + i) - 1]);
                if (CurrMag > MaxCurr)
                {
                    MaxCurr = CurrMag;
                    MaxPhase = i;
                }
            }
            ClassIdx = DSSObjType & CLASSMASK;              // gets the parent class descriptor (int)
            Nref = (ActiveTerminal->TermNodeRef)[MaxPhase - 1]; // reference to the phase voltage with the max current
            nrefN = (ActiveTerminal->TermNodeRef)[Fnconds - 1];  // reference to the ground terminal (GND or other phase)
            /*# with ActiveCircuit[ActorID].Solution do */     // Get power into max phase of active terminal
            TDSSCircuit* with0 = ActiveCircuit[ActorID];
            {
                if ((!ADiakoptics) || (ActorID == 1))
                {
                    if (!(ClassIdx == XFMR_ELEMENT))  // Only for transformers
                        Volts = with0->Solution->NodeV[Nref];
                    else
                        Volts = csub(with0->Solution->NodeV[Nref], with0->Solution->NodeV[nrefN]);
                }
                else
                {
                    if (!(ClassIdx == XFMR_ELEMENT))  // Only for transformers
                        Volts = with0->Solution->VoltInActor1(Nref);
                    else
                        Volts = csub(with0->Solution->VoltInActor1(Nref), with0->Solution->VoltInActor1(nrefN));
                }
            }
        }
        result = cang(Volts);
        return result;
    }
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    complex TDSSCktElement::Get_MaxPower( int idxTerm, int ActorID )
    /*Get power in the phase with the max current and return equivalent power as if it were balanced in all phases
     2/12/2019*/
    {
      complex result;
      complex Volts, VN{}, cPower;
      int ClassIdx = 0, i = 0, k = 0, l = 0, m = 0, nrefN = 0, Nref = 0;
      double MaxCurr = 0.0, CurrMag = 0.0;
      int MaxPhase = 0;
      Set_ActiveTerminal(idxTerm);   // set active Terminal
      cPower = CZero;
      if ( FEnabled )
      {
        ComputeIterminal( ActorID );

        // Method: Get power in the phase with max current of active terminal
        // Multiply by Nphases and return
        MaxCurr = 0.0;
        MaxPhase = 1;  // Init this so it has a non zero value
        k = ( idxTerm - 1 ) * Fnconds; // starting index of terminal
        for ( int stop = Fnphases, i = 1; i <= stop; i++)
        {
          CurrMag = cabs( (Iterminal)[(k) + i - 1] );
          if ( CurrMag > MaxCurr )
          {
            MaxCurr = CurrMag;
            MaxPhase = i;
          }
        }
        ClassIdx = DSSObjType & CLASSMASK;              // gets the parent class descriptor (int)
        Nref = (ActiveTerminal->TermNodeRef)[MaxPhase - 1]; // reference to the phase voltage with the max current
        nrefN = (ActiveTerminal->TermNodeRef)[Fnconds - 1];  // reference to the ground terminal (GND or other phase)
        /*# with ActiveCircuit[ActorID].Solution do */     // Get power into max phase of active terminal
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          if ( (! ADiakoptics) || ( ActorID == 1 ) )
          {
            if ( ! ( ClassIdx == XFMR_ELEMENT ) )  // Only for transformers
              Volts = with0->Solution->NodeV[Nref];
            else
              Volts = csub(with0->Solution->NodeV[Nref], with0->Solution->NodeV[nrefN] );
          }
          else
          {
            if ( ! ( ClassIdx == XFMR_ELEMENT ) )  // Only for transformers
              Volts = with0->Solution->VoltInActor1( Nref );
            else
              Volts = csub(with0->Solution->VoltInActor1( Nref ), with0->Solution->VoltInActor1( nrefN ) );
          }
          cPower = cmul( Volts, conjg( (Iterminal)[(k) + MaxPhase - 1] ) );
        }

           // Compute equivalent total power of all phases assuming equal to max power in all phases
        /*# with cPower do */
        {
          cPower.re = cPower.re * Fnphases;  // let compiler handle type coercion
          cPower.im = cPower.im * Fnphases;
        }

           /*If this is a positive sequence circuit (Fnphases=1),
            then we need to multiply by 3 to get the 3-phase power*/
        if (with0->PositiveSequence )
          cPower = cmulreal( cPower, 3.0 );
      }
      result = cPower;
      return result;
    }


    double TDSSCktElement::Get_MaxCurrent( int idxTerm, int ActorID )
    {
      double result = 0.0;
      int i = 0, k = 0, Nref = 0;
      double MaxCurr = 0.0, CurrMag = 0.0;
      int MaxPhase = 0;
      Set_ActiveTerminal(idxTerm);   // set active Terminal
      MaxCurr = 0.0;
      if ( FEnabled )
      {
        ComputeIterminal( ActorID );
        // Method: Get max current at terminal (magnitude)
        MaxCurr     = 0.0;
        MaxPhase    = 1;  // Init this so it has a non zero value
        k           = ( idxTerm - 1 ) * Fnconds; // starting index of terminal
        for ( int stop = Fnphases, i = 0; i < stop; i++)
        {
          CurrMag = cabs( (Iterminal)[(k) + i] );
          if ( CurrMag > MaxCurr )
          {
            MaxCurr = CurrMag;
            MaxPhase = i + 1;
          }
        }
      }
      result = MaxCurr;
      return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::GetPhasePower( pComplexArray PowerBuffer, int ActorID )
    // Get the power in each phase (complex losses) of active terminal
    // neutral conductors are ignored by this routine

    {
      int i = 0, n = 0;
      if ( FEnabled )
      {
        ComputeIterminal( ActorID );
        /*# with ActiveCircuit[ActorID].Solution do */
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        for ( int stop = Yorder, i = 1; i <= stop; i++)
        {
          n = (NodeRef)[i - 1]; // increment through terminals
          if ( n > 0 )
          {
            if ( (!ADiakoptics) || (ActorID == 1))
            {
              if (with0->PositiveSequence )
                (PowerBuffer)[i - 1] = cmulreal( cmul(with0->Solution->NodeV[n], conjg( (Iterminal)[i - 1] ) ), 3.0 );
              else
                (PowerBuffer)[i - 1] = cmul(with0->Solution->NodeV[n], conjg( (Iterminal)[i - 1] ) );
            }
            else
            {
                    // In the context of actor 1 votlages
              if (with0->PositiveSequence )
                (PowerBuffer)[i - 1] = cmulreal( cmul(with0->Solution->VoltInActor1( n ), conjg( (Iterminal)[i - 1] ) ), 3.0 );
              else
                (PowerBuffer)[i - 1] = cmul(with0->Solution->VoltInActor1( n ), conjg( (Iterminal)[i - 1] ) );
            }
          }
        }
      }
      else
        for ( int stop = Yorder, i = 1; i <= stop; i++)
          (PowerBuffer)[i - 1] = CZero;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::GetPhaseLosses( int& Num_Phases, pComplexArray LossBuffer, int ActorID )
    // Get the losses in each phase (complex losses);  Power difference coming out
    // each phase. Note: This can be misleading if the nodev voltage is greatly unbalanced.
    // neutral conductors are ignored by this routine

    {
      int i = 0, j = 0, k = 0, n = 0;
      complex cLoss;
      Num_Phases = Fnphases;
      if ( FEnabled )
      {
        ComputeIterminal( ActorID );
        /*# with ActiveCircuit[ActorID].Solution do */
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        for ( int stop = Num_Phases, i = 1; i <= stop; i++)
        {
          cLoss = cmplx( 0.0, 0.0 );
          for ( int stop = Fnterms, j = 1; j <= stop; j++)
          {
            k = ( j - 1 ) * Fnconds + i - 1;
            n = (NodeRef)[k]; // increment through terminals
            if ( n > 0 )
            {
              if ( (! ADiakoptics) || ( ActorID == 1 ) )
              {
                if (with0->PositiveSequence )
                  caccum( cLoss, cmulreal( cmul(with0->Solution->NodeV[n], conjg( (Iterminal)[k] ) ), 3.0 ) );
                else
                  caccum( cLoss, cmul(with0->Solution->NodeV[n], conjg( (Iterminal)[k] ) ) );
              }
              else
              {
                        // In the context of actor 1 voltage
                if (with0->PositiveSequence )
                  caccum( cLoss, cmulreal( cmul(with0->Solution->VoltInActor1( n ), conjg( (Iterminal)[k] ) ), 3.0 ) );
                else
                  caccum( cLoss, cmul(with0->Solution->VoltInActor1( n ), conjg( (Iterminal)[k] ) ) );
              }
            }
          }
          (LossBuffer)[i - 1] = cLoss;
        }
      }
      else
        for ( int stop = Num_Phases, i = 1; i <= stop; i++)
          (LossBuffer)[i - 1] = CZero;
    }
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::DumpProperties( Textfile& F, bool Complete )
    {
      int i = 0, j = 0;
      inherited::DumpProperties( F, Complete );
      if ( FEnabled )
      {
        WriteLn( F , "! ENABLED" );
      }
      else
      {
        WriteLn( F , "! DISABLED" );
      }
      if ( Complete )
      {
        Write( F, "! NPhases = " ); WriteLn( F, Fnphases, 0 );
        Write( F, "! Nconds = " ); WriteLn( F, Fnconds, 0 );
        Write( F, "! Nterms = " ); WriteLn( F, Fnterms, 0 );
        Write( F, "! Yorder = " ); WriteLn( F, to_string(Yorder), 0 );
        Write( F, "! NodeRef = \"" );
        if ( NodeRef.empty())
        {
          Write( F, "nil" );
        }
        else
          for ( int stop = Yorder, i = 1; i <= stop; i++)
          {
            Write( F, NodeRef[i - 1], 0 ); Write( F, ' ' );
          }
        WriteLn( F, '\"' );
        Write( F, "! Terminal Status: [" );
        for ( int stop = Fnterms, i = 1; i <= stop; i++)
          for ( int stop = Fnconds, j = 1; j <= stop; j++)
          {
            if ( Terminals[i - 1].Conductors[j - 1].Closed )
            {
              Write( F, "C " );
            }
            else
            {
              Write( F, "O " );
            }
          }
        WriteLn( F, ']' );
        Write( F, "! Terminal Bus Ref: [" );
        for ( int stop = Fnterms, i = 1; i <= stop; i++)
          for ( int stop = Fnconds, j = 1; j <= stop; j++)
          {
            Write( F, Terminals[i - 1].BusRef, 0 ); Write( F, ' ' );
          }
        WriteLn( F, ']' );
        WriteLn( F );
        if ( YPrim != NULL )
        {
          WriteLn( F, "! YPrim (G matrix)" );
          for ( int stop = Yorder, i = 1; i <= stop; i++)
          {
            Write( F, "! " );
            for ( int stop = Yorder, j = 1; j <= stop; j++)
            {
              Write( F, Format( " %13.10g |",  YPrim->GetElement( i, j ).re) );
            }
            WriteLn( F );
          }
          WriteLn( F, "! YPrim (B Matrix) = " );
          for ( int stop = Yorder, i = 1; i <= stop; i++)
          {
            Write( F, "! " );
            for ( int stop = Yorder, j = 1; j <= stop; j++)
            {
              Write( F, Format( " %13.10g |",  YPrim->GetElement( i, j ).im ) );
            }
            WriteLn( F );
          }
        }
      }  /*If complete*/
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCktElement::DoYprimCalcs( TcMatrix* Ymatrix )
    {
      int i = 0, j = 0, k = 0, ii = 0, jj = 0, ElimRow = 0;
      complex Ynn, Yij, Yin, Ynj;
      std::vector <longInt> RowEliminated;
      bool ElementOpen = false;
      complex cEpsilon;
         /*Now Account for Open Conductors
          Perform a Kron Reduction on rows where I is forced to zero.
          Then for any conductor that is open, zero out row and column.
          */
      /*# with Ymatrix do */
      {
        ElementOpen = false;
        k = 0;
        for ( int stop = Fnterms, i = 1; i <= stop; i++)
        {
          for ( int stop = Fnconds, j = 1; j <= stop; j++)
          {
            if ( ! Terminals[i - 1].Conductors[(j) - 1].Closed )
            {
              if ( ! ElementOpen )
              {
                RowEliminated.resize( Yorder );
                ElementOpen = true;
                cEpsilon = cmplx( EPSILON, 0.0 );
              }
                    // First do Kron Reduction
              ElimRow = j + k;
              Ynn = Ymatrix->GetElement( ElimRow, ElimRow );
              if ( cabs( Ynn ) == 0.0 )
                Ynn.re = EPSILON;
              RowEliminated[(ElimRow) - 1] = 1;
              for ( int stop = Yorder, ii = 1; ii <= stop; ii++)
              {
                if ( RowEliminated[(ii) - 1] == 0 )
                {
                  Yin = Ymatrix->GetElement( ii, ElimRow );
                  for ( int stop = Yorder, jj = ii; jj <= stop; jj++)
                    if ( RowEliminated[(jj) - 1] == 0 )
                    {
                      Yij = Ymatrix->GetElement( ii, jj );
                      Ynj = Ymatrix->GetElement( ElimRow, jj );
                      Ymatrix->SetElemsym( ii, jj, csub( Yij, cdiv( cmul( Yin, Ynj ), Ynn ) ) );
                    }
                }
              }
                    // Now zero out row and column
              Ymatrix->ZeroRow( ElimRow );
              Ymatrix->ZeroCol( ElimRow );
                    // put a small amount on the diagonal in case node gets isolated
              Ymatrix->SetElement( ElimRow, ElimRow, cEpsilon );
            }
          }
          k = k + Fnconds;
        }
           /* Clean up at end of loop.
             Add in cEpsilon to diagonal elements of remaining rows to avoid leaving a bus hanging.
             This happens on low-impedance simple from-to elements when one terminal opened.
           */
        if ( ElementOpen )
        {
          for ( int stop = Yorder, ii = 1; ii <= stop; ii++)
            if ( RowEliminated[(ii) - 1] == 0 )
              Ymatrix->AddElement( ii, ii, cEpsilon );
          RowEliminated.clear();
        }
      }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void TDSSCktElement::SumCurrents( int ActorID )

    // sum Terminal Currents into System  Currents Array
    // Primarily for Newton Iteration

    {
      int i = 0;
      if ( FEnabled )
      {
        ComputeIterminal( ActorID );
        /*# with ActiveCircuit[ActorID].Solution do */
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        for ( int stop = Yorder, i = 1; i <= stop; i++)
          caccum(with0->Solution->Currents[NodeRef[i - 1]], Iterminal[i - 1] );  // Noderef=0 is OK
      }
    }


    void TDSSCktElement::GetTermVoltages( int iTerm, pComplexArray VBuffer, int ActorID )

    // Bus Voltages at indicated terminal
    // Fill Vbuffer array which must be adequately allocated by calling routine

    {
      int Ncond = 0, i = 0, j = 0;
      try
      {
        Ncond = Get_NConds();

         /*return Zero if terminal number improperly specified*/
        if ( ( iTerm < 1 ) || ( iTerm > Fnterms ) )
        {
          for ( int stop = Ncond, i = 1; i <= stop; i++)
            (VBuffer)[i - 1] = CZero;
          return;
        }
        /*# with ActiveCircuit[ActorID].Solution do */
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        for ( int stop = Ncond, i = 1; i <= stop; i++)
        {
          if ( (! ADiakoptics) || ( ActorID == 1 ) )
            VBuffer[i - 1] = with0->Solution->NodeV[ Terminals[(iTerm) - 1].TermNodeRef[i - 1]];
          else
            VBuffer[i - 1] = with0->Solution->VoltInActor1( Terminals[(iTerm) - 1].TermNodeRef[i - 1] );
        }
      }
      catch( exception & E )
      {
        DoSimpleMsg( "Error filling voltage buffer in GetTermVoltages for Circuit Element:" + Get_myPName() + "." + get_Name() + CRLF + "Probable Cause: Invalid definition of element." + CRLF + "System Error Message: " + (string) E.what(), 755 );
      }
    }


    void TDSSCktElement::InitPropertyValues( int ArrayOffset )
    {
      Set_PropertyValue(ArrayOffset + 1, to_string( BaseFrequency ));  // Base freq
      Set_PropertyValue(ArrayOffset + 2, "true");  // Enabled
      FEnabledProperty = ArrayOffset + 2;     // keep track of this
      inherited::InitPropertyValues( ArrayOffset + 2 );
    }


    String TDSSCktElement::GetPropertyValue( int Index )
    {
      String result = "";
      if ( Index == FEnabledProperty )
      {
        if ( Get_Enabled() )
          result = "true";
        else
          result = "false";
               // *** RCD 6-18-03 commented out PropertyValue[FEnabledProperty] := Result; // Keep this in synch
      }
      else
        result = inherited::GetPropertyValue( Index );
      return result;
    }


    void TDSSCktElement::GetSeqLosses( complex& PosSeqLosses, complex& NegSeqLosses, complex& ZeroModeLosses, int ActorID )
    {

    /* For the base class, just return CZERO*/

    /*Derived classes have to supply appropriate function*/
      PosSeqLosses = CZero;
      NegSeqLosses = CZero;
      ZeroModeLosses = CZero;
    }


    bool IsGroundBus( const String S )
    {
      bool result = false;
      size_t i = 0;
      result = true;
      i = S.find( ".1" );
      if ( i != String::npos )
        result = false;
      i = S.find( ".2" );
      if ( i != String::npos )
        result = false;
      i = S.find( ".3" );
      if ( i != String::npos )
        result = false;
      i = S.find( "." );
      if ( i == String::npos )
        result = false;
      return result;
    }

    double TDSSCktElement::Get_PCEValue(int idxTerm, int ValType, int ActorID)
    {
        double Result = 0.0;
        switch (ValType)
        {
        case    0:
        case    7:  Result = -1 * Get_Power(1,ActorID).re;         // P, P0
            break;
        case    1:
        case    8:  Result = -1 * Get_Power(1, ActorID).im;         // Q, Q0
            break;
        case    2:  Result = Get_MaxVoltage(1,ActorID);            // VMag
            break;
        case    3:  Result = Get_MaxVoltageAng(1,ActorID);         // VAng
            break;
        case    4:  Result = Get_MaxCurrent(1,ActorID);            // IMag
            break;
        case    5:  Result = Get_MaxCurrentAng(1,ActorID);         // IAng
            break;
        case    6:  Result = cabs(Get_Power(1,ActorID));           // S
            break;
        default:    Result = 0.0;
            break;
        }
        return Result;
    }


    void TDSSCktElement::MakePosSequence( int ActorID )
    {
      int i = 0;
      bool grnd = false;
      for ( int stop = Fnterms, i = 1; i <= stop; i++)
      {
        grnd = IsGroundBus( FBusNames[i - 1] );
        FBusNames[i - 1] = StripExtension( FBusNames[i - 1] );
        if ( grnd )
          FBusNames[i - 1] = FBusNames[i - 1] + ".0";
      }
    }


    void TDSSCktElement::ComputeVterminal( int ActorID )

    /*Put terminal voltages in an array*/
    {
      int i = 0, j = 0;
      /*# with ActiveCircuit[ActorID].Solution do */
      TDSSCircuit* with0 = ActiveCircuit[ActorID];
      for ( int stop = Yorder, i = 1; i <= stop; i++)
        if ( (! ADiakoptics) || ( ActorID == 1 ) )
          Vterminal[i - 1] = with0->Solution->NodeV[ NodeRef[i - 1] ];
        else
          Vterminal[i - 1] = with0->Solution->VoltInActor1( NodeRef[i - 1] );
    }


    void TDSSCktElement::ZeroITerminal( )
    {
      int i = 0;
      for ( int stop = Yorder, i = 1; i <= stop; i++)
        Iterminal[i - 1] = CZero;
    }

}






