

#pragma hdrstop

#include "ExportResults.h"



#include "Ucomplex.h"
#include "Arraydef.h"
#include "Sysutils.h"
#include "Circuit.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Ucmatrix.h"
#include "Solution.h"
#include "CktElement.h"
#include "Utilities.h"
#include "Bus.h"
#include "mathutil.h"
#include "DSSClass.h"
#include "PDElement.h"
#include "PCElement.h"
#include "generator.h"
#include "Sensor.h"
#include "Load.h"
#include "RegControl.h"
#include "Transformer.h"
#include "ParserDel.h"
#include <math.h>
#include "YMatrix.h"
#include "LineGeometry.h"
#include "WireData.h"
#include "LineCode.h"
#include "XfmrCode.h"
#include "NamedObject.h"
#include "GICTransformer.h"
#include "PVsystem.h"
#include "Storage.h"
#include "klusolve.h"
#include "ExportCIMXML.h"
#include "LineSpacing.h"
#include "CNData.h"
#include "TSData.h"

#include "System.h"


namespace ExportResults
{
    std::vector<double> Registers(67,0);


    void WriteElementVoltagesExportFile( Textfile& F, TDSSCktElement* pElem, int MaxNumNodes )
    {
      int Ncond = 0, Nterm = 0, i = 0, j = 0, k = 0, m = 0, Nref = 0, bref = 0;
      String BusName;
      complex Volts;
      double Vpu = 0.0, Vmag = 0.0;
      
      Ncond = pElem->Get_NConds();
      Nterm = pElem->Get_NTerms();
      k = 0;
      BusName = ( StripExtension( pElem->Get_FirstBus() ) );
       Write( F, ( (TNamedObject*) pElem )->Get_myPName() + "."  + ((TDSSObject*)pElem)->get_Name() );
       Write( F, Format( ",%d",  Nterm ));
      for ( int stop = Nterm, j = 1; j <= stop; j++)
      {
         Write( F, Format( ",%d,",  j ));
         Write( F, Format( "%d,%d,",  Ncond, pElem->Get_NPhases() ));
         Write( F, UpperCase( BusName ) );
        for ( int stop = Ncond, i = 1; i <= stop; i++)
        {
          k++;
          Nref = (pElem->NodeRef)[k - 1];
          Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
          Vmag = cabs( Volts ) * 0.001;
          if ( Nref == 0 )
            Vpu = 0.0;
          else
            /*# with ActiveCircuit[ActiveActor] do */
            {
              auto with0 = ActiveCircuit[ActiveActor];
              {
                bref = with0->MapNodeToBus[Nref - 1].BusRef;
                if ( with0->Buses[bref - 1]->kVBase != 0.0 )
                  Vpu = Vmag / with0->Buses[bref - 1]->kVBase;
                else
                  Vpu = 0.0;
                if ( i == 1 )
                {
                    Write( F, Format("%6.3f", with0->Buses[bref - 1]->kVBase * sqrt(3)));
                }
              }
            }
          /*# with ActiveCircuit[ActiveActor] do */
          {
            auto with1 = ActiveCircuit[ActiveActor];
            {
                Write( F, Format(", %d, %10.6g, %6.3f, %9.5g", k, Vmag, cdang(Volts), Vpu));
            }
          } //end with ActiveCircuit
        } //end numconductors

       /*Zero Fill row*/
        for ( int stop = ( MaxNumNodes ), m = ( Ncond + 1 ); m <= stop; m++)
        {
           Write( F, ", 0, 0, 0, 0" );
        }
        BusName = StripExtension( pElem->Get_NextBus() );
      } // end for numterminals
    } //end procedure

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportSeqVoltages( String Filenm )

    // Export Symmetrical Component bus voltages

    {
      TTextRec F = {};
      int       i           = 0, 
                j           = 0,
                Nref        = 0;
      complex   Vph[4]      = {cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0)}, 
                VphLL[4]    = { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) }, 
                V012[4]     = { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) };
      double    V0          = 0.0, 
                v1          = 0.0, 
                V2          = 0.0, 
                Vpu         = 0.0, 
                V2V1        = 0.0, 
                V0V1        = 0.0,
                V_NEMA      = 0.0;
      complex Vresidual     = cmplx(0,0);
      
      
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Bus,  V1,  p.u.,Base kV, V2, %V2/V1, V0, %V0/V1, Vresidual, %NEMA" );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            {
              if ( with0->Buses[i - 1]->get_FNumNodesThisBus() < 3 )
              {
                V0 = 0.0;
                V2 = 0.0;
                V_NEMA = 0.0;
                if ( ( with0->Buses[i - 1]->get_FNumNodesThisBus() == 1 ) && with0->PositiveSequence )
                { // first node
                  Nref = with0->Buses[i - 1]->GetRef( 1 );
                  Vph[1 - 1] = with0->Solution->NodeV[Nref];
                  v1 = cabs( Vph[1 - 1] );
                }
                else
                  v1 = 0.0;
              }
              else
              {
                /*# with ActiveCircuit[ActiveActor]->Solution, Buses^[i] do */
                {
                  auto with1 = with0->Buses[i - 1];
                  auto with2 = with0->Solution;
                  for ( int stop = 3, j = 1; j <= stop; j++)
                  {      // first nodes named  1, 2, 3
                    Vph[j - 1] = with2->NodeV[with1->GetRef( with1->FindIdx( j ) )];
                  }
                }

                 /*Compute LL voltages for Nema unbalance calc*/
                VphLL[1 - 1] = csub( Vph[1 - 1], Vph[2 - 1] );
                VphLL[2 - 1] = csub( Vph[2 - 1], Vph[3 - 1] );
                VphLL[3 - 1] = csub( Vph[3 - 1], Vph[1 - 1] );
                Phase2SymComp( &Vph[0], &V012[0]);
                V0 = cabs( V012[1 - 1] );
                v1 = cabs( V012[2 - 1] );
                V2 = cabs( V012[3 - 1] );
                V_NEMA = PctNemaUnbalance( &VphLL[0]);
              }
              if ( with0->Buses[i-1]->kVBase != 0.0 )
                Vpu = 0.001 * v1 / with0->Buses[i-1]->kVBase;
              else
                Vpu = 0.0;
              if ( v1 > 0.0 )
              {
                V2V1 = 100.0 * V2 / v1;
                V0V1 = 100.0 * V0 / v1;
              }
              else
              {
                V2V1 = 0.0;
                V0V1 = 0.0;
              }
              Vresidual = CZero;
              /*# with ActiveCircuit[ActiveActor]->Solution do */
              for ( int stop = with0->Buses[i - 1]->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                caccum( Vresidual, with0->Solution->NodeV[with0->Buses[i - 1]->GetRef( j )] );
              
              WriteLn( F, Format("\"%s\", %10.6g, %9.5g, %8.2f, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g",
                  UpperCase(with0->BusList.Get(i)).c_str(), v1, Vpu,
                  (with0->Buses[i - 1]->kVBase * SQRT3), V2, V2V1, V0, V0V1, cabs(Vresidual), V_NEMA));
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    //-------------------------------------------------------------------



    void ExportVoltages( String Filenm )

    // Export Symmetrical Component bus voltages

    {
      int MaxNumNodes = 0;
      TTextRec F;
      int i = 0, j = 0, jj = 0;
      String BusName;
      complex Volts;
      int Nref = 0;
      int NodeIdx = 0;
      double Vmag = 0.0, Vpu = 0.0;
      

      /*Find max nodes at a bus*/
      MaxNumNodes = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
          MaxNumNodes = max( MaxNumNodes, with0->Buses[i - 1]->get_FNumNodesThisBus() );
      }
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        Write( F, "Bus, BasekV" );
        for (int stop = MaxNumNodes, i = 1; i <= stop; i++)
        {
            Write(F, Format(", Node%d, Magnitude%d, Angle%d, pu%d", i, i, i, i));
        }
        WriteLn( F );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with1 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with1->NumBuses, i = 1; i <= stop; i++)
            {
              BusName = with1->BusList.Get( i );
              
              Write( F, Format("\"%s\", %.5g", UpperCase(BusName).c_str(), with1->Buses[i - 1]->kVBase * SQRT3));
              jj = 1;
              /*# with Buses^[i] do */
              {
                auto with2 = with1->Buses[i - 1];
                for ( int stop = with2->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                {
                  do
                  {
                    NodeIdx = with2->FindIdx( jj );     // Try to find nodes in order
                    jj++;
                  }
                  while ( ! ( NodeIdx > 0 ) );
                  Nref = with2->GetRef( NodeIdx );
                  Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                  Vmag = cabs( Volts );
                  if ( with2->kVBase != 0.0 )
                    Vpu = 0.001 * Vmag / with2->kVBase;
                  else
                    Vpu = 0.0;
                  
                  Write( F, Format(", %d, %10.6g, %6.1f, %9.5g", with2->GetNum(NodeIdx), Vmag, cdang(Volts), Vpu));
                }
              }
               /*Zero Fill row*/
              for ( int stop = MaxNumNodes, j = with1->Buses[i - 1]->get_FNumNodesThisBus() + 1; j <= stop; j++)
                Write( F, ", 0, 0, 0, 0" );
              WriteLn( F );
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void CalcAndWriteSeqCurrents( Textfile& F, int j, TDSSCktElement* pElem, pComplexArray cBuffer, bool DoRatings )
    {
      double    I0      = 0.0, 
                I1      = 0.0, 
                I2      = 0.0, 
                I2I1    = 0.0, 
                I0I1    = 0.0, 
                iNormal = 0.0, 
                iEmerg  = 0.0,
                I_NEMA  = 0.0;
      int       i       = 0, 
                k       = 0, 
                Ncond   = 0;
      complex   Iph[4]  = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) }, 
                I012[4] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };
      complex   Iresidual = cmplx(0,0);

      
      Ncond = pElem->Get_NConds();

      if ( pElem->Get_NPhases() >= 3 )
      {
        for ( int stop = 3, i = 1; i <= stop; i++)
        {
          k = ( j - 1 ) * Ncond + i;
          Iph[i - 1] = cBuffer[k - 1];
        }
        Phase2SymComp( &Iph[0], &I012[0]);
        I0 = cabs( I012[1 - 1] );
        I1 = cabs( I012[2 - 1] );
        I2 = cabs( I012[3 - 1] );
        I_NEMA = PctNemaUnbalance(&Iph[0]);
      }
      else
      {
        I0 = 0.0;
        I1 = 0.0;
        I2 = 0.0;
        I_NEMA = 0.0;
        if    // Use phase 1 only
        ( ActiveCircuit[ActiveActor]->PositiveSequence )
          I1 = cabs( Iph[1 - 1] );
      }
      if ( I1 > 0.0 )
      {
        I2I1 = 100.0 * I2 / I1;
        I0I1 = 100.0 * I0 / I1;
      }
      else
      {
        I2I1 = 0.0;
        I0I1 = 0.0;
      }
      if  // Only for 1st Terminal
      ( DoRatings && ( j == 1 ) )
      {
        iNormal = ( (TPDElement*) pElem )->NormAmps;
        if ( iNormal > 0.0 )
          iNormal = I1 / iNormal * 100.0;
        iEmerg = ( (TPDElement*) pElem )->EmergAmps;
        if ( iEmerg > 0.0 )
          iEmerg = I1 / iEmerg * 100.0;
      }
      else
      {
        iNormal = 0.0;
        iEmerg = 0.0;
      }
      Iresidual = CZero;
      for ( int stop = Ncond, i = 1; i <= stop; i++)
        caccum( Iresidual, cBuffer[i - 1] );
      
       WriteLn( F, Format("\"%s\", %3d, %10.6g, %8.4g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g",
           (pElem->Get_myPName() + "." + UpperCase(pElem->get_Name())).c_str(),
           j, I1, iNormal, iEmerg, I2, I2I1, I0, I0I1, cabs(Iresidual), I_NEMA));
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportSeqCurrents( String Filenm )
    {
      TTextRec F;
      int j = 0;
      TDSSCktElement* pElem;
      TPDElement* PDElem;
      TPCElement* PCelem;
      pComplexArray cBuffer;  // Allocate to max total conductors

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();


         /*Sequence Currents*/
        WriteLn( F, "Element, Terminal,  I1, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1, Iresidual, %NEMA" );

         /*Allocate cBuffer big enough for largest circuit element*/
        cBuffer = new complex[ GetMaxCktElementSize() ];


         //Sources Get_First()
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            pElem->GetCurrents( cBuffer, ActiveActor );
            for ( int stop = pElem->Get_NTerms(), j = 1; j <= stop; j++)
              CalcAndWriteSeqCurrents( F, j, pElem, cBuffer, false );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }


         // PDELEMENTS Next
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if ( PDElem->Get_Enabled() )
          {
            PDElem->GetCurrents( cBuffer, ActiveActor );
            for ( int stop = PDElem->Get_NTerms(), j = 1; j <= stop; j++)
              CalcAndWriteSeqCurrents( F, j, PDElem, cBuffer, true );
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

        // PCelemENTS next
        PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( PCelem != NULL )
        {
          if ( ( (TDSSCktElement*) PCelem )->Get_Enabled() )
          {
            PCelem->GetCurrents( cBuffer, ActiveActor );
            for ( int stop = ((TDSSCktElement*)PCelem)->Get_NTerms(), j = 1; j <= stop; j++)
              CalcAndWriteSeqCurrents( F, j, PCelem, cBuffer, false );
          }
          PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }


         //Faults Get_Next()
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            pElem->GetCurrents( cBuffer, ActiveActor );
            for ( int stop = pElem->Get_NTerms(), j = 1; j <= stop; j++)
              CalcAndWriteSeqCurrents( F, j, pElem, cBuffer, false );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
        }
        GlobalResult = Filenm;
      /* }
      __finally
      {*/
        if (( cBuffer != NULL ) )
          free( cBuffer );
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void CalcAndWriteCurrents( Textfile& F, TDSSCktElement* pElem, pComplexArray cBuffer, int CondWidth, int TermWidth )
    {
      int i = 0, j = 0, k = 0;
      complex Iresid;


      k = 0;
       Write( F, pElem->Get_myPName() + "." + UpperCase(pElem->get_Name() ));
      for ( int stop = pElem->Get_NTerms(), j = 1; j <= stop; j++)
      {
        Iresid = CZero;
        for ( int stop = pElem->Get_NConds(), i = 1; i <= stop; i++)
        {
            k++;
            
            Write( F, Format(", %10.6g, %8.2f", cabs(cBuffer[k - 1]), cdang(cBuffer[k - 1])));
            caccum( Iresid, cBuffer[k - 1] );
        }
        for ( int stop = CondWidth, i = pElem->Get_NConds() + 1; i <= stop; i++)
        {
            Write( F, Format(", %10.6g, %8.2f", 0.0, 0.0));
        }
        
        Write( F, Format(", %10.6g, %8.2f", cabs(Iresid), cdang(Iresid)));
      }

        /*Filler if no. terms less than termwidth*/
      for ( int stop = TermWidth, j = pElem->Get_NTerms() + 1; j <= stop; j++)
        for ( int stop = CondWidth + 1, i = 1; i <= stop; i++)
        {
           Write( F, Format(", %10.6g, %8.2f", 0.0, 0.0));
        }
      WriteLn( F );
    }


    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void CalcAndWriteMaxCurrents( Textfile& F, TPDElement* pElem, pComplexArray cBuffer )
    {
      int RatingIdx = 0, i = 0;
      double EmergAmps = 0.0, NormAmps = 0.0, CurrMag = 0.0, MaxCurrent = 0.0;
      complex LocalPower;
      TXYcurveObj* RSignal;
      
        // Initializes NomrAmps and EmergAmps with the default values for the PDElement
      NormAmps = pElem->NormAmps;
      EmergAmps = pElem->EmergAmps;
      if ( SeasonalRating )
      {
        if ( SeasonSignal != "" )
        {
          RSignal = (TXYcurveObj*) XYCurveClass[ActiveActor]->Find( SeasonSignal );
          if ( RSignal != NULL )
          {
            RatingIdx = trunc( RSignal->GetYValue_( ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour ) );
              // Brings the seasonal ratings for the PDElement
            if ( ( RatingIdx <= pElem->NumAmpRatings ) && ( pElem->NumAmpRatings > 1 ) )
            {
              NormAmps = pElem->AmpRatings[RatingIdx];
              EmergAmps = pElem->AmpRatings[RatingIdx];
            }
          }
          else
            SeasonalRating = false;   // The XYCurve defined doesn't exist
        }
        else
          SeasonalRating = false;    // The user didn't define the seasonal signal
      }
       Write( F, pElem->Get_myPName() + "."  + UpperCase(pElem->get_Name()));
      MaxCurrent = 0.0;
      for ( int stop = pElem->Get_NPhases(), i = 1; i <= stop; i++)
      {
        CurrMag = cabs( cBuffer[i - 1] );
        if ( CurrMag > MaxCurrent )
          MaxCurrent = CurrMag;
      }
        //----pElem.ActiveTerminalIdx := 1;
      LocalPower = cmulreal( pElem->Get_Power(1, ActiveActor), 0.001 );
      if ( ( pElem->NormAmps == 0.0 ) || ( pElem->EmergAmps == 0.0 ) )
      {
          Write( F, Format(", %10.6g, %8.2f, %8.2f", MaxCurrent, 0.0, 0.0));
      }
      else
      {
         Write( F, Format(", %10.6g, %8.2f, %8.2f", MaxCurrent, MaxCurrent / NormAmps * 100.0, MaxCurrent / EmergAmps * 100.0));
      }
      
      Write( F, Format(", %10.6g, %10.6g, %d, %d, %d", LocalPower.re, LocalPower.im, pElem->BranchNumCustomers, pElem->BranchTotalCustomers, pElem->Get_NPhases()));
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {
           Write( F, Format(", %-.3g ", (with0->Buses[with0->MapNodeToBus[pElem->NodeRef[0] - 1].BusRef - 1]->kVBase)));
        }
      }
      WriteLn( F );
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportCurrents( String Filenm )
    {
      TTextRec F;
      pComplexArray cBuffer;
      TDSSCktElement* pElem;
      int MaxCond = 0, MaxTerm = 0;
      int i = 0, j = 0;
      

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        cBuffer = new complex[ GetMaxCktElementSize() ];

         /*Calculate the width of the file*/
        MaxCond = 1;
        MaxTerm = 2;
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_NTerms() > MaxTerm )
            MaxTerm = pElem->Get_NTerms();
          if ( pElem->Get_NConds() > MaxCond )
            MaxCond = pElem->Get_NConds();
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_Next();
        }


         /*Branch Currents*/
        Write( F, "Element" );
        for ( int stop = MaxTerm, i = 1; i <= stop; i++)
        {
            for (int stop = MaxCond, j = 1; j <= stop; j++)
            {
                Write(F, Format(", I%d_%d, Ang%d_%d", i, j, i, j));
            }
            
            Write( F, Format(", Iresid%d, AngResid%d", i, i));
        }
        WriteLn( F );


         // Sources first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            pElem->GetCurrents( cBuffer, ActiveActor );
            CalcAndWriteCurrents( F, pElem, cBuffer, MaxCond, MaxTerm );
          }
          pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }


         // PDELEMENTS first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            pElem->GetCurrents( cBuffer, ActiveActor );
            CalcAndWriteCurrents( F, pElem, cBuffer, MaxCond, MaxTerm );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // Faults
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            pElem->GetCurrents( cBuffer, ActiveActor );
            CalcAndWriteCurrents( F, pElem, cBuffer, MaxCond, MaxTerm );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
        }

         // PCELEMENTS next
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            pElem->GetCurrents( cBuffer, ActiveActor );
            CalcAndWriteCurrents( F, pElem, cBuffer, MaxCond, MaxTerm );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        if (( cBuffer != NULL ) )
          free( cBuffer );
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteNodeList( Textfile& F, const String CktElementName )
    {
      int NValues = 0, i = 0;
      
      if ( ActiveCircuit[ActiveActor] != NULL )
        if ( ! ActiveCircuit[ActiveActor]->Issolved )
        {
          DoSimpleMsg( "Circuit must be solved for this command to execute properly.", 222001 );
          return;
        }
      if ( CktElementName.size( ) > 0 )
      {
        SetObject( CktElementName );
        if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
          /*# with ActiveCircuit[ActiveActor]->ActiveCktElement do */
          {
            auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
            
             Write( F, Format("\"%s\", %d, %d", CktElementName.c_str(), with0->Get_NTerms(), with0->Get_NConds()));
            NValues = with0->Get_NConds() * with0->Get_NTerms();
            for ( int stop = NValues, i = 1; i <= stop; i++)
            {
                Write( F, Format(", %d", GetNodeNum(with0->NodeRef[i - 1])));
            }
            WriteLn( F );
          }
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportNodeOrder( String Filenm )

    /* Writes NodeLists in same order as Export Currents function
    */
    {
      TTextRec F;
      TDSSCktElement* pElem;
      String strName;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

         /*Header Record*/
        Write( F, "Element, Nterminals, Nconductors, Node-1, Node-2, Node-3, ..." );
        WriteLn( F );


         // Sources first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*) pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*) pElem)->get_Name();
            WriteNodeList( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }


         // PDELEMENTS first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteNodeList( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // Faults
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteNodeList( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
        }

         // PCELEMENTS next
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteNodeList( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteElemCurrents( Textfile& F, const String CktElementName )
    {
      int NValues = 0, i = 0;
      
      if ( ActiveCircuit[ActiveActor] != NULL )
        if ( ! ActiveCircuit[ActiveActor]->Issolved )
        {
          DoSimpleMsg( "Circuit must be solved for this command to execute properly.", 222001 );
          return;
        }
      if ( CktElementName.size( ) > 0 )
      {
        SetObject( CktElementName );
        if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
          /*# with ActiveCircuit[ActiveActor]->ActiveCktElement do */
          {
            auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();

            with0->ComputeIterminal( ActiveActor );
            
             Write( F, Format("\"%s\", %d, %d", CktElementName.c_str(), with0->Get_NTerms(), with0->Get_NConds()));
            NValues = with0->Get_NConds() * with0->Get_NTerms();
            for ( int stop = NValues, i = 1; i <= stop; i++)
            {
                Write( F, Format(", %10.6g, %8.2f", cabs(with0->Iterminal[i - 1]), cdang(with0->Iterminal[i - 1])));
            }
            WriteLn( F );
          }
      }
    }


    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportElemCurrents( String Filenm )

    /* Export currents in same order as NodeOrder export
    */
    {
      TTextRec F;
      TDSSCktElement* pElem;
      String strName;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

         /*Header Record*/
        Write( F, "Element, Nterminals, Nconductors, I_1, Ang_1, ..." );
        WriteLn( F );


         // Sources first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*) pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemCurrents( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }


         // PDELEMENTS first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemCurrents( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // Faults
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemCurrents( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
        }

         // PCELEMENTS next
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemCurrents( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteElemVoltages( Textfile& F, const String CktElementName )
    {
      int NValues = 0, i = 0;
      
      if ( ActiveCircuit[ActiveActor] != NULL )
        if ( ! ActiveCircuit[ActiveActor]->Issolved )
        {
          DoSimpleMsg( "Circuit must be solved for this command to execute properly.", 222001 );
          return;
        }
      if ( CktElementName.size( ) > 0 )
      {
        SetObject( CktElementName );
        if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
          /*# with ActiveCircuit[ActiveActor]->ActiveCktElement do */
          {
            auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();

            with0->ComputeVterminal( ActiveActor );
            
            Write( F, Format("\"%s\", %d, %d", CktElementName.c_str(), with0->Get_NTerms(), with0->Get_NConds()));
            NValues = with0->Get_NConds() * with0->Get_NTerms();
            for ( int stop = NValues, i = 1; i <= stop; i++)
            {
                Write( F, Format(", %10.6g, %8.2f", cabs(with0->Vterminal[i - 1]), cdang(with0->Vterminal[i - 1])));
            }
            WriteLn( F );
          }
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportElemVoltages( String Filenm )
    /* Export conductor voltages in same order as NodeOrder export
    */
    {
      TTextRec F;
      TDSSCktElement* pElem;
      String strName;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

         /*Header Record*/
        Write( F, "Element, Nterminals, Nconductors, V_1, Ang_1, ..." );
        WriteLn( F );


         // Sources first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemVoltages( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }


         // PDELEMENTS first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemVoltages( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // Faults
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemVoltages( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
        }

         // PCELEMENTS next
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemVoltages( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }


    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteElemPowers( Textfile& F, const String CktElementName )
    {
      int NValues = 0, i = 0;
      
      complex S;
      if ( ActiveCircuit[ActiveActor] != NULL )
        if ( ! ActiveCircuit[ActiveActor]->Issolved )
        {
          DoSimpleMsg( "Circuit must be solved for this command to execute properly.", 222001 );
          return;
        }
      if ( CktElementName.size( ) > 0 )
      {
        SetObject( CktElementName );
        if (( ActiveCircuit[ActiveActor]->get_FActiveCktElement() != NULL ) )
          /*# with ActiveCircuit[ActiveActor]->ActiveCktElement do */
          {
            auto with0 = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
            with0->ComputeVterminal( ActiveActor );
            with0->ComputeIterminal( ActiveActor );
            
             Write( F, Format("\"%s\", %d, %d", CktElementName.c_str(), with0->Get_NTerms(), with0->Get_NConds()));
            NValues = with0->Get_NConds() * with0->Get_NTerms();
            for ( int stop = NValues, i = 1; i <= stop; i++)
            {
                S = cmul( with0->Vterminal[i - 1], conjg( with0->Iterminal[i - 1] ) );
                
                Write( F, Format(", %10.6g, %10.6g", S.re * 0.001, S.im * 0.001));
            }
            WriteLn( F );
          }
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportElemPowers( String Filenm )

    /* Export conductor powers in same order as NodeOrder export
    */
    {
      TTextRec F;
      TDSSCktElement* pElem;
      String strName;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

         /*Header Record*/
        Write( F, "Element, Nterminals, Nconductors, P_1, Q_1, ..." );
        WriteLn( F );


         // Sources first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemPowers( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }


         // PDELEMENTS first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemPowers( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // Faults
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemPowers( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
        }

         // PCELEMENTS next
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            strName = ((TDSSObject*)pElem)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)pElem)->get_Name();
            WriteElemPowers( F, strName );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportPowers( String Filenm, int Opt )

    /*Opt = 0: kVA
     opt = 1: MVA
     */
    {
      TTextRec F;
      int Nterm = 0, j = 0;
      TPDElement* PDElem;
      TPCElement* PCelem;
      complex S;
      String Separator;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        Separator = ", ";
        switch ( Opt )
        {
          case 1:
            WriteLn( F, "Element, Terminal, P(MW), Q(Mvar), P_Normal, Q_Normal, P_Emergency, Q_Emergency" );
          break;
        default:
          WriteLn( F, "Element, Terminal, P(kW), Q(kvar),  P_Normal, Q_Normal, P_Emergency, Q_Emergency" );
        }

         // PDELEMENTS first
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
            if (UpperCase(PDElem->get_Name()) == "TPX2224490448C0")
                int a = 10;
          if ( PDElem->Get_Enabled() )
          {
            Nterm = PDElem->Get_NTerms();
            for ( int stop = Nterm, j = 1; j <= stop; j++)
            {
              Write( F, Pad( "\"" + PDElem->Get_myPName() + "." + UpperCase(PDElem->get_Name() ) + "\"", 24 ) ); Write( F, Separator ); Write( F, j, 3 );
               //----PDElem.ActiveTerminalIdx := j;
              S = PDElem->Get_Power(j, ActiveActor);
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F,Format( "%8.1f",S.re * 0.001));
              Write( F, Separator ); Write( F, Format("%8.1f", S.im * 0.001));
              if ( j == 1 )
              {
                 //----PDelem.ActiveTerminalIdx := 1;
                S = PDElem->Get_ExcessKVANorm(1, ActiveActor);
                if ( Opt == 1 )
                  S = cmulreal( S, 0.001 );
                Write(F, Separator); Write(F, Format("%8.1f", Abs(S.re)));
                Write(F, Separator); Write(F, Format("%8.1f", Abs(S.im)));
                S = PDElem->Get_ExcessKVAEmerg(1, ActiveActor);
                if ( Opt == 1 )
                  S = cmulreal( S, 0.001 );
                Write( F, Separator ); Write( F, Format("%8.1f", Abs( S.re )));
                Write( F, Separator ); Write( F, Format("%8.1f", Abs( S.im )));
              }
              WriteLn( F );
            }
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // PCELEMENTS Get_Next()
        PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( PCelem != NULL )
        {
          if ( ( (TDSSCktElement*) PCelem)->Get_Enabled() )
          {
            Nterm = ((TDSSCktElement*)PCelem)->Get_NTerms();
            for ( int stop = Nterm, j = 1; j <= stop; j++)
            {
              Write( F, Pad( "\"" + PCelem->Get_myPName() + "." + UpperCase(PCelem->get_Name()) + "\"", 24 ) ); Write( F, Separator ); Write( F, j, 3 );
               //----pcElem.ActiveTerminalIdx := j;
              S = ((TDSSCktElement*)PCelem)->Get_Power(j, ActiveActor);
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F, Format("%8.1f", S.re * 0.001));
              Write( F, Separator ); Write( F, Format("%8.1f", S.im * 0.001));
              WriteLn( F );
            }
          }
          PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportLosses( String Filenm )

    /*Opt = 0: kVA
     opt = 1: MVA
     */
    {
      TTextRec F;
      TPDElement* PDElem;
      
      complex S_total, S_Load, S_NoLoad;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Element,  Total(W), Total(var),  I2R(W), I2X(var), No-load(W), No-load(var)" );
         // PDELEMENTS first
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if ( PDElem->Get_Enabled() )
          {
            PDElem->GetLosses( S_total, S_Load, S_NoLoad, ActiveActor );
            
            WriteLn( F, Format("%s.%s, %.7g, %.7g, %.7g, %.7g, %.7g, %.7g",
                PDElem->ParentClass->get_myClass_name().c_str(),
                UpperCase(PDElem->get_Name()).c_str(),
                S_total.re, S_total.im, S_Load.re, S_Load.im, S_NoLoad.re, S_NoLoad.im));
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // ===============================================================================



    void ExportPbyphase( String Filenm, int Opt )

    /* Export Powers by phase */

    /*Opt = 0: kVA
     opt = 1: MVA
     */
    {
      TTextRec F;
      int i = 0;
      
      TPDElement* PDElem;
      TPCElement* PCelem;
      complex S;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        switch ( Opt )
        {
          case 1:
            WriteLn( F, "Element, NumTerminals, NumConductors, NumPhases, MW1, Mvar1, MW2, Mvar2, MW3, Mvar3, ... " );
          break;
        default:
          WriteLn( F, "Element, NumTerminals, NumConductors, NumPhases, kW1, kvar1, kW2, kvar2, kW3, kvar3, ... " );
        }

         // PDELEMENTS first
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if ( PDElem->Get_Enabled() )
          {
            /*# with PDElem do */
            {
              auto with0 = PDElem;
              with0->ComputeIterminal( ActiveActor );
              with0->ComputeVterminal( ActiveActor );
              
              Write( F, Format("\"%s.%s\", %d, %d, %d",
                  with0->Get_myPName().c_str(),
                  UpperCase(((TDSSObject*)with0)->get_Name()).c_str(),
                  with0->Get_NTerms(), with0->Get_NConds(), with0->Get_NPhases()));
              for ( int stop = with0->Yorder, i = 1; i <= stop; i++)
              {
                S = cmulreal( cmul( with0->Vterminal[i - 1], conjg( with0->Iterminal[i - 1] ) ), 0.001 );
                if ( Opt == 1 )
                  S = cmulreal( S, 0.001 );   // convert to MVA
                
                Write( F, Format(", %10.3f, %10.3f", S.re, S.im));
              }
            }
            WriteLn( F );
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // PCELEMENTS Get_Next()
        PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( PCelem != NULL )
        {
          if ( ( (TDSSCktElement*) PCelem )->Get_Enabled() )
          {
            /*# with PCelem do */
            {
              auto with0 = PCelem;
              with0->ComputeIterminal( ActiveActor );
              with0->ComputeVterminal( ActiveActor );
              
              Write( F, Format("\"%s.%s\", %d, %d, %d",
                  with0->Get_myPName().c_str(),
                  UpperCase(with0->get_Name()).c_str(),
                  with0->Get_NTerms(), with0->Get_NConds(), with0->Get_NPhases()));

              for ( int stop = with0->Yorder, i = 1; i <= stop; i++)
              {
                S = cmulreal( cmul((with0->Vterminal)[i - 1], conjg((with0->Iterminal)[i - 1] ) ), 0.001 );
                if ( Opt == 1 )
                  S = cmulreal( S, 0.001 );   // convert to MVA
                
                Write( F, Format(", %10.3f, %10.3f", S.re, S.im));
              }
            }
            WriteLn( F );
          }
          PCelem = (TPCElement*)ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportSeqPowers( String Filenm, int Opt )

    /*Opt = 0: kVA
     opt = 1: MVA
     */
    {
      TTextRec      F;
      vector<complex> cBuffer;
      int           Ncond   = 0, 
                    Nterm   = 0, 
                    i       = 0, 
                    j       = 0, 
                    k       = 0,
                    Nref    = 0;
      TPDElement*   PDElem  = nullptr;
      TPCElement*   PCelem  = nullptr;
      complex       Volts   = cmplx(0,0),
                    S       = cmplx(0, 0);
      complex       Vph[4]  = { cmplx(0,0),cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) }, 
                    V012[4] = { cmplx(0,0),cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) };
      complex       Iph[4]  = { cmplx(0,0),cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) }, 
                    I012[4] = { cmplx(0,0),cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) };
      String Separator;

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        Separator = ", ";
        cBuffer.resize(GetMaxCktElementSize() + 1);
        switch ( Opt )
        {
          case 1:
            WriteLn( F, "Element, Terminal, P1(MW), Q1(Mvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency" );
          break;
        default:
          WriteLn( F, "Element, Terminal, P1(kW), Q1(kvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency" );
        }

         // PDELEMENTS first
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if ( PDElem->Get_Enabled() )
          {
            Ncond = PDElem->Get_NConds();
            Nterm = PDElem->Get_NTerms();
            PDElem->GetCurrents( &cBuffer[0], ActiveActor);
            for ( int stop = Nterm, j = 1; j <= stop; j++)
            {
              Write( F, Pad( "\"" + ( (TDSSObject*) PDElem )->Get_myPName() + "." + UpperCase( ( (TDSSObject*) PDElem )->get_Name() ) + "\"", 24 ) ); Write( F, Separator ); Write( F, j, 3 );
              for ( int stop = PDElem->Get_NPhases(), i = 1; i <= stop; i++)
              {
                k = ( j - 1 ) * Ncond + i;
                Nref = (PDElem->NodeRef)[k - 1];
                Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                if (cBuffer[k].re < -1e+10 && cBuffer[k].im < -1e+10)
                {
                    cBuffer[k].re = 0;
                    cBuffer[k].im = 0;
                }
                Iph[i - 1] = (cBuffer)[k-1];
                Vph[i - 1] = Volts;
              }
              if ( PDElem->Get_NPhases() >= 3 )
              {
                Phase2SymComp( &Iph[0], &I012[0]);
                Phase2SymComp( &Vph[0], &V012[0]);
              }
              else
              {
                V012[1 - 1] = CZero;
                I012[1 - 1] = CZero;
                V012[3 - 1] = CZero;
                I012[3 - 1] = CZero;
                if ( ActiveCircuit[ActiveActor]->PositiveSequence )
                {
                  V012[2 - 1] = Vph[1 - 1];
                  I012[2 - 1] = Iph[1 - 1];
                }
                else
                {
                  V012[2 - 1] = CZero;
                  I012[2 - 1] = CZero;
                }
              }
              S = cmul( V012[2 - 1], conjg( I012[2 - 1] ) );
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F, Format("%10.1f",S.re * 0.003));
              Write( F, Separator ); Write( F, Format("%10.1f", S.im * 0.003));
              S = cmul( V012[3 - 1], conjg( I012[3 - 1] ) );
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F, Format("%10.1f", S.re * 0.003));
              Write( F, Separator ); Write( F, Format("%10.1f", S.im * 0.003));
              S = cmul( V012[1 - 1], conjg( I012[1 - 1] ) );
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F, Format("%8.1f", S.re * 0.003));
              Write( F, Separator ); Write( F, Format("%8.1f", S.im * 0.003));
              if ( j == 1 )
              {
                     //----PDelem.ActiveTerminalIdx := 1;
                S = PDElem->Get_ExcessKVANorm(1, ActiveActor);
                if ( Opt == 1 )
                  S = cmulreal( S, 0.001 );
                Write( F, Separator ); Write( F, Format("%10.1f", Abs( S.re )));
                Write( F, Separator ); Write( F, Format("%10.1f", Abs( S.im )));
                S = PDElem->Get_ExcessKVAEmerg(1, ActiveActor);
                if ( Opt == 1 )
                  S = cmulreal( S, 0.001 );
                Write( F, Separator ); Write( F, Format("%10.1f", Abs( S.re )));
                Write( F, Separator ); Write( F, Format("%10.1f", Abs( S.im )));
              }
              WriteLn( F );
            }
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }

         // PCELEMENTS Get_Next()
        PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( PCelem != NULL )
        {
          if ( ( (TDSSCktElement*) PCelem )->Get_Enabled() )
          {
            Ncond = ((TDSSCktElement*)PCelem)->Get_NConds();
            Nterm = ((TDSSCktElement*)PCelem)->Get_NTerms();
            PCelem->GetCurrents( &cBuffer[0], ActiveActor);
            for ( int stop = Nterm, j = 1; j <= stop; j++)
            {
              Write( F, Pad( "\"" + ((TDSSObject*)PCelem)->Get_myPName() + "." + UpperCase(((TDSSObject*)PCelem)->get_Name() ) + "\"", 24 ) ); Write( F, Separator ); Write( F, j, 3 );
              for ( int stop = ((TDSSCktElement*)PCelem)->Get_NPhases(), i = 1; i <= stop; i++)
              {
                k = ( j - 1 ) * Ncond + i;
                Nref = (((TDSSCktElement*)PCelem)->NodeRef)[k - 1];
                Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                Iph[i - 1] = (cBuffer)[k-1];
                Vph[i - 1] = Volts;
              }
              if (((TDSSCktElement*)PCelem)->Get_NPhases() >= 3 )
              {
                Phase2SymComp( &Iph[0], &I012[0]);
                Phase2SymComp( &Vph[0], &V012[0]);
              }
              else
              {
                V012[1 - 1] = CZero;
                I012[1 - 1] = CZero;
                V012[3 - 1] = CZero;
                I012[3 - 1] = CZero;
                if ( ActiveCircuit[ActiveActor]->PositiveSequence )
                {
                  V012[2 - 1] = Vph[1 - 1];
                  I012[2 - 1] = Iph[1 - 1];
                }
                else
                {
                  V012[2 - 1] = CZero;
                  I012[2 - 1] = CZero;
                }
              }
              S = cmul( V012[2 - 1], conjg( I012[2 - 1] ) );
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F, Format("%10.1f", S.re * 0.003));
              Write( F, Separator ); Write( F, Format("%10.1f", S.im * 0.003));
              S = cmul( V012[3 - 1], conjg( I012[3 - 1] ) );
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F, Format("%10.1f", S.re * 0.003));
              Write( F, Separator ); Write( F, Format("%10.1f", S.im * 0.003));
              S = cmul( V012[1 - 1], conjg( I012[1 - 1] ) );
              if ( Opt == 1 )
                S = cmulreal( S, 0.001 );
              Write( F, Separator ); Write( F, Format("%10.1f", S.re * 0.003));
              Write( F, Separator ); Write( F, Format("%10.1f", S.im * 0.003));
              WriteLn( F );
            }
          }
          PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
        GlobalResult = Filenm;
      /*}
      __finally
      {*/
        cBuffer.resize(0);
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportFaultStudy( String Filenm )
    {
        int     i = 0,
                iBus = 0,
                iphs = 0,
                iphs2 = 0;
      TcMatrix  YFault = {};
      vector <complex> Vfault;  /*Big temp array*/
      TTextRec  F = {};
      complex   GFault = CZero;
      String    Separator = "";
      double    MaxCurr = 0.0, 
                CurrMag = 0.0;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        Separator = ", ";

       /* Set source voltage injection currents */
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            /*# with Solution do */
            {
              auto with1 = ActiveCircuit[ActiveActor]->Solution;
         /*All Phase Faults*/
              WriteLn( F, "Bus,  3-Phase,  1-Phase,  L-L" );
              for ( int stop = with0->NumBuses, iBus = 1; iBus <= stop; iBus++)
               /*Bus Norton Equivalent Current, Isc has been previously computed*/
                /*# with Buses^[iBus] do */
                {
                  auto& with2 = with0->Buses[iBus - 1];
                  {
                    Write( F, Pad( UpperCase(with0->BusList.Get( iBus ) ), 12 ) );
                    MaxCurr = 0.0;
                    for ( int stop = with2->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
                    {
                      if ( MaxCurr < cabs(with2->BusCurrent[i - 1] ) )
                        MaxCurr = cabs( with2->BusCurrent[i - 1] );
                    }
                    Write( F, Separator ); Write( F, Format("%10.0f", MaxCurr));

               /*One Phase Faults*/

       /* Solve for Fault Injection Currents*/
                    YFault = TcMatrix( with2->get_FNumNodesThisBus() );
                    Vfault.resize(with2->get_FNumNodesThisBus() + 1);

                 /*Build YscTemp*/
                    GFault = cmplx( 10000.0, 0.0 );
                    MaxCurr = 0.0;
                    for ( int stop = with2->get_FNumNodesThisBus(), iphs = 1; iphs <= stop; iphs++)
                    {
                      YFault.CopyFrom( &(with2->Ysc) );
                      YFault.AddElement( iphs, iphs, GFault );

                       /* Solve for Injection Currents*/
                      YFault.Invert();
                      YFault.MVmult( &Vfault[0], &(with2->BusCurrent[0]));  /*Gets voltage appearing at fault*/
                      CurrMag = cabs( cmul( Vfault[iphs - 1], GFault ) );
                      if ( CurrMag > MaxCurr )
                        MaxCurr = CurrMag;
                    } /*For iphase*/
                 /*Now, Stuff it in the Css Array where it belongs*/
                    Write( F, Separator ); Write( F, Format("%10.0f",MaxCurr));
                    Vfault.clear();

               /*Node-Node Faults*/

               /*Bus Norton Equivalent Current, Isc has been previously computed*/
                    YFault = TcMatrix( with2->get_FNumNodesThisBus() );
                    Vfault.resize(with2->get_FNumNodesThisBus() + 1);
                    GFault = cmplx( 10000.0, 0.0 );
                    MaxCurr = 0.0;
                    for ( int stop = with2->get_FNumNodesThisBus(), iphs = 1; iphs <= stop; iphs++)
                    {
                      YFault.CopyFrom( &(with2->Ysc) );

                      if (iphs == with2->get_FNumNodesThisBus())
                          iphs2 = 1;
                      else
                          iphs2 = iphs + 1;

                      YFault.AddElement( iphs, iphs, GFault );
                      YFault.AddElement( iphs2, iphs2, GFault );
                      YFault.AddElemsym( iphs, iphs2, cnegate( GFault ) );

                       /* Solve for Injection Currents*/
                      YFault.Invert();
                      YFault.MVmult(&(Vfault[0]), &(with2->BusCurrent[0]));  /*Gets voltage appearing at fault*/
                      CurrMag = cabs( cmul( csub( Vfault[iphs - 1], Vfault[iphs2 - 1] ), GFault ) );
                      if ( CurrMag > MaxCurr )
                        MaxCurr = CurrMag;
                    } /*For iphase*/
                 /*Now, Stuff it in the Css Array where it belongs*/
                    Write( F, Separator ); Write( F, Format("%10.0f", MaxCurr));
                    Vfault.clear();
                    WriteLn( F );
                  }
                }  /*With bus*/
            } /*With Solution*/
          }
        } /*With ActiveCircuit*/
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ZeroTempXArray( double* TempX )
    {
      int ii = 0;
      for ( int stop = 3, ii = 1; ii <= stop; ii++)
        TempX[ii - 1] = 0.0;
    }


    void ExportEstimation( String Filenm )
    {
      TTextRec F;
      int i = 0;
      TEnergyMeterObj* pEnergyMeterObj;
      TSensorObj* pSensorObj;
      
      double TempX[ 4/*# range 1..3*/ ]; // temp number buffer
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );   // clears file
        IOResultToException();

              /*Do the EnergyMeters first*/
        WriteLn( F, "\"Energy Meters\" " );
        WriteLn( F, "\"energyMeter\", \"I1 Target\", \"I2 Target\", \"I3 Target\", \"I1 Calc\", \"I2 Calc\", \"I3 Calc\", \"I1 %Err\", \"I2 %Err\", \"I3 %Err\""/*, "I1 Factor", "I2 Factor", "I3 Factor"'*/ );
        pEnergyMeterObj = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
        pEnergyMeterObj->TakeSample(ActiveActor);
        while ( pEnergyMeterObj != NULL )
        {
          if ( ( (TDSSCktElement*) pEnergyMeterObj)->Get_Enabled() )
          {
            Write( F, "\"Energymeter." + ( (TDSSObject*) pEnergyMeterObj)->get_Name() + "\"");
                      /*Sensor currents (Target)*/
            ZeroTempXArray( TempX );
            for ( i =0; i < pEnergyMeterObj->Get_NPhases(); i++)
              TempX[i] = (pEnergyMeterObj->SensorCurrent)[i];
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*Calculated Currents*/
            ZeroTempXArray(TempX);
            for ( i = 0; i < pEnergyMeterObj->Get_NPhases(); i++)
              TempX[i] = cabs( (pEnergyMeterObj->CalculatedCurrent)[i] );
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*Percent Error*/
            for ( i = 0; i < pEnergyMeterObj->Get_NPhases(); i++)
              TempX[i] = ( 1.0 - TempX[i] / max( 0.001, (pEnergyMeterObj->SensorCurrent)[i] ) ) * 100.0;
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }

                      /****  Not all that useful
                      {Allocation Factors}
                      ZeroTempXArray;
                      For i := 1 to pEnergyMeterObj.Nphases do TempX[i] := pEnergyMeterObj.PhsAllocationFactor^[i - 1];
                      For i := 1 to 3 do Write(F, Format(' %.6g,',[TempX[i]]));
                      *****/
            WriteLn( F );
          }
          pEnergyMeterObj = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_Next();
        }

              /*Do the Sensors Get_Next()*/
        WriteLn( F );
        WriteLn( F, "\"Sensors\" " );
        Write( F, "\"Sensor\", \"I1 Target\", \"I2 Target\", \"I3 Target\", \"I1 Calc\", \"I2 Calc\", \"I3 Calc\", \"I1 %Err\", \"I2 %Err\", \"I3 %Err\"," );
        WriteLn( F, " \"V1 Target\", \"V2 Target\", \"V3 Target\", \"V1 Calc\", \"V2 Calc\", \"V3 Calc\", \"V1 %Err\", \"V2 %Err\", \"V3 %Err\", \"WLS Voltage Err\", \"WLS Current Err\"" );
        pSensorObj = (TSensorObj*) ActiveCircuit[ActiveActor]->Sensors.Get_First();
        while ( pSensorObj != NULL )
        {
          if ( ( (TDSSCktElement*) pSensorObj )->Get_Enabled() )
          {
            Write( F, "Sensor." + UpperCase( ( (TDSSObject*) pSensorObj )->get_Name() ) );
                      /*Sensor currents (Target)*/
            ZeroTempXArray(TempX);
            for ( i = 0; i < pSensorObj->Get_NPhases(); i++)
              TempX[i] = (pSensorObj->SensorCurrent)[i];
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*Calculated Currents*/
            ZeroTempXArray(TempX);
            for ( i = 0; i < pSensorObj->Get_NPhases(); i++)
              TempX[i] = cabs( pSensorObj->CalculatedCurrent[i] );
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*Percent Error*/
            for ( i = 0; i < pSensorObj->Get_NPhases(); i++)
              TempX[i] = ( 1.0 - TempX[i] / max( 0.001, pSensorObj->SensorCurrent[i] ) ) * 100.0;
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*Sensor Voltage (Target)*/
            ZeroTempXArray(TempX);
            for ( i = 0; i < pSensorObj->Get_NPhases(); i++)
              TempX[i] = pSensorObj->SensorVoltage[i];
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*Calculated Voltage*/
            ZeroTempXArray(TempX);
            for (i = 0; i < pSensorObj->Get_NPhases(); i++)
              TempX[i] = cabs( pSensorObj->CalculatedVoltage[i] );
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*Percent Error*/
            for (i = 0; i < pSensorObj->Get_NPhases(); i++)
              TempX[i] = ( 1.0 - TempX[i] / max( 0.001, (pSensorObj->SensorVoltage)[i] ) ) * 100.0;
            for (i = 0; i < 3; i++)
            {
                Write(F, Format(", %.6g", TempX[i]));
            }
                      /*WLS Errors*/
            ZeroTempXArray(TempX);
            Write( F, Format(", %.6g, %.6g", pSensorObj->Get_WLSVoltageError(), pSensorObj->Get_WLSCurrentError()));
            WriteLn( F );
          }
          pSensorObj = (TSensorObj*) ActiveCircuit[ActiveActor]->Sensors.Get_Next();
        }
    /* }
      __finally
      {*/
        AppendGlobalResult( Filenm );
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }



    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteMultipleMeterFiles( )
    {
      TTextRec F;
      int               i = 0, 
                        j = 0;
      TEnergyMeterObj*  pElem       = nullptr;
      TEnergyMeter*     MeterClass  = nullptr;
      String            Filenm      = "", 
                        Separator   = "";
      
      MeterClass = (TEnergyMeter*) GetDSSClassPtr( "Energymeter" );
      if ( MeterClass == NULL )
        return;  // oops somewhere!!
      Separator = ", ";
      pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
      while ( pElem != NULL )
      {
        if ( ( (TDSSCktElement*) pElem )->Get_Enabled() )
        {
          try
          {
            Filenm = GetOutputDirectory() + "EXP_MTR_" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + RepTermination;
            if ( ! FileExists( Filenm ) )
            {
              AssignFile( F, Filenm );
              Rewrite( F );
              IOResultToException();
                    /*Write New Header*/
              Write( F, "Year, LDCurve, Hour, Meter" );
              for ( int stop = NumEMRegisters, i = 1; i <= stop; i++)
              {
                Write( F, Separator ); Write( F, "\"" + pElem->RegisterNames[i - 1] + "\"" );
              }
              WriteLn( F );
              CloseFile( F );
            }
            AssignFile( F, Filenm );
            Append( F );
            IOResultToException();
            Write( F, ActiveCircuit[ActiveActor]->Solution->get_Fyear(), 0 ); Write( F, Separator );
            Write( F, ActiveCircuit[ActiveActor]->LoadDurCurve ); Write( F, Separator );
            Write( F, ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
            Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
            for ( int stop = NumEMRegisters, j = 1; j <= stop; j++)
            {
              Write( F, Separator ); Write( F, pElem->Registers[j - 1], 10, 0 );
            }
            WriteLn( F );
            AppendGlobalResult( Filenm );
    /* }
          __finally
          {*/
            CloseFile( F );
          }
          catch (...)
          {
              //
          }
        }
        pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_Next();
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteSingleMeterFile( const String Filenm )
    {
      TTextRec F;
      int               i = 0, 
                        j = 0;
      TEnergyMeterObj*  pElem = nullptr;
      String            TestStr = "", 
                        Separator = ", ";
      bool              RewriteFile = false;
  
      try
      {
        if ( FileExists( Filenm ) )
        {  // See if it has already been written on
          AssignFile( F, Filenm );
          Reset( F );
          IOResultToException();
          if ( ! Eof( F ) )
          {
            Read( F, TestStr );
                 /*See if it likely that the file is OK*/
            if ( CompareText( TestStr.substr( 0, 4 ), "Year" ) == 0 )
              RewriteFile = false;       // Assume the file is OK
            else
              RewriteFile = true;
          }
          else
            RewriteFile = true;
          CloseFile( F );
        }
        else
        {
          RewriteFile = true;
          AssignFile( F, Filenm );
        }

       /*Either open or append the file*/
        if ( RewriteFile )
        {
          Rewrite( F );
          IOResultToException();
            /*Write New Header*/
          pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
          Write( F, "Year, LDCurve, Hour, Meter" );
          for ( int stop = NumEMRegisters, i = 1; i <= stop; i++)
          {
            Write( F, Separator ); Write( F, "\"" + pElem->RegisterNames[i - 1] + "\"" );
          }
          WriteLn( F );
        }
        else
        {
          Append( F );
          IOResultToException();
        }
        pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
        while ( pElem != NULL )
        {
          if ( ( (TDSSCktElement*) pElem )->Get_Enabled() )
          {
            Write( F, ActiveCircuit[ActiveActor]->Solution->get_Fyear(), 0 ); Write( F, Separator );
            Write( F, ActiveCircuit[ActiveActor]->LoadDurCurve ); Write( F, Separator );
            Write( F, ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
            Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
            //pElem->ResetRegisters();
            //pElem->TakeSample(ActiveActor);
/*
==============This code was added by another group and can be discarded, the original DSS doesn't do this.=================

             auto with1 = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_First();
             auto with2 = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_First();
             auto with3 = (TLineObj*)ActiveCircuit[ActiveActor]->Lines.Get_First();
             
             with1->Get_ExceedsNormal(ActiveActor);
             with1->Get_Unserved(ActiveActor);
             with3->Get_Losses(ActiveActor);

             if (with2->HasEnergyMeter)
             {
                 pElem->Registers[5] = with1->kWBase + with2->kWBase;
                 pElem->Registers[6] = with1->kvarBase + with2->kvarBase;
                 pElem->Registers[7] = with1->kWBase + with2->kWBase;
                 pElem->Registers[8] = with1->kVABase;
                 pElem->Registers[11] = with1->EEN_Factor;
                 pElem->Registers[12] = with1->UE_Factor;
                 pElem->Registers[62] = with1->kWBase + with2->kWBase;
             }
             else
             {
                 pElem->Registers[5] = with1->kWBase;
                 pElem->Registers[6] = with1->kvarBase;
                 pElem->Registers[7] = with1->kWBase;
                 pElem->Registers[8] = with1->kVABase;
                 pElem->Registers[11] = with1->EEN_Factor;
                 pElem->Registers[12] = with1->UE_Factor;
                 pElem->Registers[62] = with1->kWBase;
                 
             }                           
             */
            for ( int stop = NumEMRegisters, j = 1; j <= stop; j++)
            {
              Write( F, Separator ); 
              Write( F, Format("%10.0f", pElem->Registers[j - 1]));
            }
            WriteLn( F );
          }
          pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportMeters( String Filenm )

    // Export Values of  Meter Elements

    // These records are appended to an existing file so a running account is kept for some kinds of simulations

    // If switch /m is specified, a separate file is created for each meter using the meter's name

    {
      if ( LowerCase( Filenm.substr( 0, 2 ) ) == "/m" )
        WriteMultipleMeterFiles();
      else
        WriteSingleMeterFile( Filenm );
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteMultipleGenMeterFiles( )
    {
      TTextRec F;
      int i = 0, j = 0;
      TGeneratorObj* pElem;
    //****   GeneratorClass:TGenerator;

      String Filenm, Separator;

     //*****     GeneratorClass := TGenerator(GetDSSClassPtr('generator'));
      if ( GeneratorClass == NULL )
        return;  // oops somewhere!!
      Separator = ", ";
      pElem = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_First();
      while ( pElem != NULL )
      {
        if ( pElem->Get_Enabled() )
        {
          try
          {
            Filenm = GetOutputDirectory() + "EXP_GEN_" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + RepTermination;
            if ( ! FileExists( Filenm ) )
            {
              AssignFile( F, Filenm );
              Rewrite( F );
              IOResultToException();
                    /*Write New Header*/
              Write( F, "Year, LDCurve, Hour, Generator" );
              for ( int stop = Generator::NumGenRegisters, i = 1; i <= stop; i++)
              {
                Write( F, Separator ); Write( F, "\"" + GeneratorClass->RegisterNames[i - 1] + "\"" );
              }
              WriteLn( F );
              CloseFile( F );
            }
            AssignFile( F, Filenm );
            Append( F );
            IOResultToException();
            /*# with ActiveCircuit[ActiveActor] do */
            {
              auto with0 = ActiveCircuit[ActiveActor];
              {
                Write( F, with0->Solution->get_Fyear(), 0 ); Write( F, Separator );
                Write( F, with0->LoadDurCurve ); Write( F, Separator );
                Write( F, with0->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
                Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
                for ( int stop = Generator::NumGenRegisters, j = 1; j <= stop; j++)
                {
                  Write( F, Separator ); Write( F, Format("%8.0f",pElem->Registers[j - 1]));
                }
                WriteLn( F );
              }
            }
            AppendGlobalResult( Filenm );
    /* }
          __finally
          {*/
            CloseFile( F );
          }
          catch (...)
          {
              //
          }
        }
        pElem = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_Next();
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteSingleGenMeterFile( String Filenm )
    {
      TTextRec F;
      int i = 0, j = 0;
      TGeneratorObj* pElem;
    //****   GeneratorClass:TGenerator;

      String Separator, TestStr;
      bool RewriteFile = false;


     //**** GeneratorClass := TGenerator(GetDSSClassPtr('generator'));
      if ( GeneratorClass == NULL )
        return;  // oops somewhere!!
      Separator = ", ";
      try
      {
        if ( FileExists( Filenm ) )
        {  // See if it has already been written on
          AssignFile( F, Filenm );
          Reset( F );
          IOResultToException();
          if ( ! Eof( F ) )
          {
            Read( F, TestStr );
                 /*See if it likely that the file is OK*/
            if ( CompareText( TestStr.substr( 0, 4 ), "Year" ) == 0 )
              RewriteFile = false;       // Assume the file is OK
            else
              RewriteFile = true;
          }
          else
            RewriteFile = true;
          CloseFile( F );
        }
        else
        {
          RewriteFile = true;
          AssignFile( F, Filenm );
        }

       /*Either open or append the file*/
        if ( RewriteFile )
        {
          Rewrite( F );
          IOResultToException();
            /*Write New Header*/
          Write( F, "Year, LDCurve, Hour, Generator" );
          for ( int stop = Generator::NumGenRegisters, i = 1; i <= stop; i++)
          {
            Write( F, Separator ); Write( F, "\"" + GeneratorClass->RegisterNames[i-1] + "\"" );
          }
          WriteLn( F );
        }
        else
        {
          Append( F );
          IOResultToException();
        }
        pElem = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_First();
        //(TGeneratorObj*)ActiveGeneratorObj->TakeSample(ActiveActor);
        while ( pElem != NULL )
        {
          if ( ( (TDSSCktElement*) pElem )->Get_Enabled() )
            /*# with ActiveCircuit[ActiveActor] do */
            {
              auto with0 = ActiveCircuit[ActiveActor];
              {
                Write( F, with0->Solution->get_Fyear(), 0 ); Write( F, Separator );
                Write( F, with0->LoadDurCurve ); Write( F, Separator );
                Write( F, with0->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
                Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
                pElem->ResetRegisters();
                pElem->TakeSample(ActiveActor);
                for ( int stop = Generator::NumGenRegisters, j = 1; j <= stop; j++)
                {
                  Write( F, Separator ); Write( F, Format("%8.0f", pElem->Registers[j - 1]));
                }
                WriteLn( F );
              }
            }
          pElem = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteMultiplePVSystemMeterFiles( )
    {
      TTextRec F;
      int i = 0, j = 0;
      TPVsystemObj* pElem;
      String Filenm, Separator;
      if ( PVSystemClass[ActiveActor] == NULL )
        return;  // oops somewhere!!
      Separator = ", ";
      pElem = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_First();
      while ( pElem != NULL )
      {
        if ( ( (TDSSCktElement*) pElem )->Get_Enabled() )
        {
          try
          {
            Filenm = GetOutputDirectory() + "EXP_PV_" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + RepTermination;
            if ( ! FileExists( Filenm ) )
            {
              AssignFile( F, Filenm );
              Rewrite( F );
              IOResultToException();
                    /*Write New Header*/
              Write( F, "Year, LDCurve, Hour, PVSystem" );
              for ( int stop = NumPVSystemRegisters, i = 1; i <= stop; i++)
              {
                Write( F, Separator ); Write( F, "\"" + PVSystemClass[ActiveActor]->RegisterNames[i - 1] + "\"" );
              }
              WriteLn( F );
              CloseFile( F );
            }
            AssignFile( F, Filenm );
            Append( F );
            IOResultToException();
            /*# with ActiveCircuit[ActiveActor] do */
            {
              auto with0 = ActiveCircuit[ActiveActor];
              {
                Write( F, with0->Solution->get_Fyear(), 0 ); Write( F, Separator );
                Write( F, with0->LoadDurCurve ); Write( F, Separator );
                Write( F, with0->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
                Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
                for ( int stop = NumPVSystemRegisters, j = 1; j <= stop; j++)
                {
                  Write( F, Separator ); Write( F, pElem->Registers[j - 1], 10, 0 );
                }
                WriteLn( F );
              }
            }
            AppendGlobalResult( Filenm );
    /* }
          __finally
          {*/
            CloseFile( F );
          }
          catch (...)
          {
              //
          }
        }
        pElem = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteSinglePVSystemMeterFile( String Filenm )
    {
      TTextRec F;
      int i = 0, j = 0;
      TPVsystemObj* pElem;
      String Separator, TestStr;
      bool RewriteFile = false;
      if ( PVSystemClass[ActiveActor] == NULL ) return;  // oops somewhere!!
      Separator = ", ";
      try
      {
        if ( FileExists( Filenm ) )
        {  // See if it has already been written on
          AssignFile( F, Filenm );
          Reset( F );
          IOResultToException();
          if ( ! Eof( F ) )
          {
            Read( F, TestStr );
                 /*See if it likely that the file is OK*/
            if ( CompareText( TestStr.substr( 0, 4 ), "Year" ) == 0 )
              RewriteFile = false;       // Assume the file is OK
            else
              RewriteFile = true;
          }
          else
            RewriteFile = true;
          CloseFile( F );
        }
        else
        {
          RewriteFile = true;
          AssignFile( F, Filenm );
        }

       /*Either open or append the file*/
        if ( RewriteFile )
        {
          Rewrite( F );
          IOResultToException();
            /*Write New Header*/
          Write( F, "Year, LDCurve, Hour, PVSystem" );
          for ( int stop = Generator::NumGenRegisters, i = 1; i <= stop; i++)
          {
            Write( F, Separator ); Write( F, "\"" + PVSystemClass[ActiveActor]->RegisterNames[i - 1] + "\"" );
          }
          WriteLn( F );
        }
        else
        {
          Append( F );
          IOResultToException();
        }
        pElem = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_First();
        while ( pElem != NULL )
        {
          if ( ( (TDSSCktElement*) pElem )->Get_Enabled() )
            /*# with ActiveCircuit[ActiveActor] do */
            {
              auto with0 = ActiveCircuit[ActiveActor];
              {
                Write( F, with0->Solution->get_Fyear(), 0 ); Write( F, Separator );
                Write( F, with0->LoadDurCurve ); Write( F, Separator );
                Write( F, with0->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
                Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
                for ( int stop = NumPVSystemRegisters, j = 1; j <= stop; j++)
                {
                  Write( F, Separator ); Write( F, pElem->Registers[j - 1], 10, 0 );
                }
                WriteLn( F );
              }
            }
          pElem = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteMultipleStorageMeterFiles( )
    {
      TTextRec F;
      int i = 0, j = 0;
      TStorageObj* pElem;
      String Filenm, Separator;
      if ( StorageClass[ActiveActor] == NULL ) return;  // oops somewhere!!
      Separator = ", ";
      pElem = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_First();
      while ( pElem != NULL )
      {
        if ( ( (TDSSCktElement*) pElem )->Get_Enabled() )
        {
          try
          {
            Filenm = GetOutputDirectory() + "EXP_PV_" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + RepTermination;
            if ( ! FileExists( Filenm ) )
            {
              AssignFile( F, Filenm );
              Rewrite( F );
              IOResultToException();
                    /*Write New Header*/
              Write( F, "Year, LDCurve, Hour, Storage" );
              for ( int stop = NumStorageRegisters, i = 1; i <= stop; i++)
              {
                Write( F, Separator ); Write( F, "\"" + StorageClass[ActiveActor]->RegisterNames[i - 1] + "\"" );
              }
              WriteLn( F );
              CloseFile( F );
            }
            AssignFile( F, Filenm );
            Append( F );
            IOResultToException();
            /*# with ActiveCircuit[ActiveActor] do */
            {
              auto with0 = ActiveCircuit[ActiveActor];
              {
                Write( F, with0->Solution->get_Fyear(), 0 ); Write( F, Separator );
                Write( F, with0->LoadDurCurve ); Write( F, Separator );
                Write( F, with0->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
                Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
                for ( int stop = NumStorageRegisters, j = 1; j <= stop; j++)
                {
                  Write( F, Separator ); Write( F, pElem->Registers[j - 1], 10, 0 );
                }
                WriteLn( F );
              }
            }
            AppendGlobalResult( Filenm );
    /* }
          __finally
          {*/
            CloseFile( F );
          }
          catch (...)
          {
              //
          }
        }
        pElem = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteSingleStorageMeterFile( String Filenm )
    {
      TTextRec F;
      int i = 0, j = 0;
      TStorageObj* pElem;
      String Separator, TestStr;
      bool RewriteFile = false;
      if ( StorageClass[ActiveActor] == NULL ) return;  // oops somewhere!!
      Separator = ", ";
      try
      {
        if ( FileExists( Filenm ) )
        {  // See if it has already been written on
          AssignFile( F, Filenm );
          Reset( F );
          IOResultToException();
          if ( ! Eof( F ) )
          {
            Read( F, TestStr );
                 /*See if it likely that the file is OK*/
            if ( CompareText( TestStr.substr( 0, 4 ), "Year" ) == 0 )
              RewriteFile = false;       // Assume the file is OK
            else
              RewriteFile = true;
          }
          else
            RewriteFile = true;
          CloseFile( F );
        }
        else
        {
          RewriteFile = true;
          AssignFile( F, Filenm );
        }

       /*Either open or append the file*/
        if ( RewriteFile )
        {
          Rewrite( F );
          IOResultToException();
            /*Write New Header*/
          Write( F, "Year, LDCurve, Hour, Storage" );
          for ( int stop = NumStorageRegisters, i = 1; i <= stop; i++)
          {
            Write( F, Separator ); Write( F, "\"" + StorageClass[ActiveActor]->RegisterNames[i - 1] + "\"" );
          }
          WriteLn( F );
        }
        else
        {
          Append( F );
          IOResultToException();
        }
        pElem = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_First();
        while ( pElem != NULL )
        {
          if ( ( (TDSSCktElement*) pElem )->Get_Enabled() )
            /*# with ActiveCircuit[ActiveActor] do */
            {
              auto with0 = ActiveCircuit[ActiveActor];
              {
                Write( F, with0->Solution->get_Fyear(), 0 ); Write( F, Separator );
                Write( F, with0->LoadDurCurve ); Write( F, Separator );
                Write( F, with0->Solution->DynaVars.intHour, 0 ); Write( F, Separator );
                Write( F, Pad( "\"" + UpperCase( ( (TDSSObject*) pElem )->get_Name() ) + "\"", 14 ) );
                for ( int stop = NumStorageRegisters, j = 1; j <= stop; j++)
                {
                  Write( F, Separator ); Write( F, pElem->Registers[j - 1], 10, 0 );
                }
                WriteLn( F );
              }
            }
          pElem = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportGenMeters( String Filenm )

    // Export Values of Generator Meter Elements
    // If switch /m is specified, a separate file is created for each generator using the generator's name

    {
      if ( LowerCase( Filenm.substr( 0, 2 ) ) == "/m" )
        WriteMultipleGenMeterFiles();
      else
        WriteSingleGenMeterFile( Filenm );
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportPVSystemMeters( String Filenm )

    // Export Values of Generator Meter Elements
    // If switch /m is specified, a separate file is created for each generator using the generator's name

    {
      if ( LowerCase( Filenm.substr( 0, 2 ) ) == "/m" )
        WriteMultiplePVSystemMeterFiles();
      else
        WriteSinglePVSystemMeterFile( Filenm );
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportStorageMeters( String Filenm )

    // Export Values of Generator Meter Elements
    // If switch /m is specified, a separate file is created for each generator using the generator's name

    {
      if ( LowerCase( Filenm.substr( 0, 2 ) ) == "/m" )
        WriteMultipleStorageMeterFiles();
      else
        WriteSingleStorageMeterFile( Filenm );
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportLoads( String Filenm )

    // Export Loads to view present allocation

    {
      TTextRec F;
      TLoadObj* pElem;
      String Separator;
      Separator = ", ";
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
         /*Write  Header*/
        WriteLn( F, "Load, Connected KVA, Allocation Factor, Phases, kW, kvar, PF, Model" );
        pElem = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
        while ( pElem != NULL )
        {
            if (((TDSSCktElement*)pElem)->Get_Enabled())
                /*# with pElem do */
            {
              auto with1 = pElem;
              Write( F, UpperCase( ( (TDSSObject*) with1 )->get_Name() ) );
              Write( F, Separator ); Write( F, Format("%8.1f", with1->get_FConnectedkVA()));
              Write( F, Separator ); Write( F, Format("%8.1f", with1->get_FkVAAllocationFactor()));
              Write( F, Separator ); Write( F, ( (TDSSCktElement*) with1 )->Get_NPhases(), 0 );
              Write( F, Separator ); Write( F, Format("%8.1f", with1->kWBase));
              Write( F, Separator ); Write( F, Format("%8.1f", with1->kvarBase));
              Write( F, Separator ); Write( F, Format("%8.1f", with1->PFNominal) );
              Write( F, Separator ); Write(F, with1->FLoadModel);
            }
          WriteLn( F );
          pElem = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportCapacity( String Filenm )

    /*
     Similar to export currents except does only max of the phases and compares that
     to the Normamps and Emergamps rating
    */
    {
      TTextRec F;
      pComplexArray cBuffer;
      TPDElement*   pElem = nullptr;
      TPCElement* pLoads = nullptr;
      int* TotalCust = 0; // Pointer to count the number of times the bus is. 

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        cBuffer = new complex[ GetMaxCktElementSize() ];
        WriteLn( F, "Name, Imax, %normal, %emergency, kW, kvar, NumCustomers, TotalCustomers, NumPhases, kVBase" );

        
        

         // PDELEMENTS ONLY
        pElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();  

        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() )
          {
            pElem->GetCurrents( cBuffer, ActiveActor );  
            CalcAndWriteMaxCurrents( F, pElem, cBuffer );
          }
          pElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        if (( cBuffer != NULL ) )
          free( cBuffer );
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportOverloads( String Filenm )
    {
      TTextRec F;
      vector<complex> cBuffer ;  // Allocate to max total conductors
      
      int           Ncond   = 0, 
                    i       = 0, 
                    j       = 0;
      TPDElement*   PDElem  = nullptr;
      complex       Iph[4]  = { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0)},
                    I012[4] = { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0)};
      double        I0 = 0.0, 
                    I1 = 0.0, 
                    I2 = 0.0, 
                    iNormal = 0.0, 
                    iEmerg = 0.0, 
                    Cmax = 0.0,
                    Spower = 0.0;
      String        Separator = "";

      
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

        /*Allocate cBuffer big enough for largest circuit element*/
        cBuffer.resize(GetMaxCktElementSize() + 1);

         /*Sequence Currents*/
        WriteLn( F, "Element, Terminal,  I1, AmpsOver, kVAOver, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1" );
        Separator = ", ";

         // PDELEMENTS Only
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if ( PDElem->Get_Enabled() )
            if    // ignore caps
            ( ( ( CLASSMASK & ( (TDSSObject*) PDElem )->DSSObjType ) ) != CAP_ELEMENT )
            {
              Ncond = PDElem->Get_NConds();
              PDElem->GetCurrents( &cBuffer[0], ActiveActor);
              for ( int stop = 1, j = 1; j <= stop; j++)       // only for terminal 1
              {
                Cmax = 0.0;
                int stop1 = 3;
                if (PDElem->Get_NPhases() <= 3)
                    stop1 = PDElem->Get_NPhases();
                for ( i = 1; i <= stop1; i++)
                {   // Check only first 3 phases
                  Iph[i - 1] = cBuffer[(( j - 1 ) * Ncond + (i - 1))];
                  Cmax = max( Cmax, cabs( Iph[i - 1] ) );
                }
                if ( PDElem->Get_NPhases() >= 3 )
                {   // Report Symmetrical Component Currents for
                  Phase2SymComp( &Iph[0], &I012[0]);
                  I0 = cabs( I012[0] );   // Get abs values to report
                  I1 = cabs( I012[1] );
                  I2 = cabs( I012[2] );
                }
                else
                {   // Other than 3-phase
                  I0 = 0.0;
                  I1 = cabs( Iph[0] );    // Ambiguous: Report only first phase
                  I2 = 0.0;
                  Cmax = I1;
                }
                if ( ( PDElem->NormAmps > 0.0 ) || ( PDElem->EmergAmps > 0.0 ) )
                  if ( ( Cmax > PDElem->NormAmps ) || ( Cmax > PDElem->EmergAmps ) )
                  {
                   // Get terminal 1 power
                    Spower = cabs( PDElem->Get_Power(1, ActiveActor) ) * 0.001;   // kW
                    Write( F, Format("%s, %d, ", Pad(("\"" + PDElem->Get_myPName() + "." + UpperCase(PDElem->get_Name()) + "\""), 22).c_str(), j));
                    
                    Write( F, Format("%8.2f, ", I1));
                    if ( j == 1 )
                    { // Only for 1st Terminal
                      iNormal = PDElem->NormAmps;
                      if ( iNormal > 0.0 )
                      {
                        Write( F, Format("%8.2f, %10.2f", (Cmax - iNormal), (Spower * (Cmax - iNormal) / iNormal)));
                        Write( F, Separator ); Write( F,  Format ( "%8.1f", Cmax / iNormal * 100.0));
                      }
                      else
                      {
                        Write( F, Separator ); Write( F, "     0.0" );
                      }
                      iEmerg = PDElem->EmergAmps;
                      if ( iEmerg > 0.0 )
                      {
                        Write( F, Separator ); Write( F, Format("%8.1f",Cmax / iEmerg * 100.0));
                      }
                      else
                      {
                        Write( F, Separator ); Write( F, "     0.0" );
                      }
                    }
                    else
                    {
                      Write( F, Separator ); Write( F, "       0" ); Write( F, Separator ); Write( F, "       0" );
                    }
                    Write( F, Separator ); Write( F, Format("%8.1f", I2));
                    if ( I1 > 0.0 )
                    {
                      Write( F, Separator ); Write( F, Format("%8.1f", 100.0 * I2 / I1));
                    }
                    else
                    {
                      Write( F, Separator ); Write( F, "0.0" );
                    }
                    Write( F, Separator ); Write( F, Format("%8.1f", I0));
                    if ( I1 > 0.0 )
                    {
                      Write( F, Separator ); Write( F, Format("%8.1f", 100.0 * I0 / I1));
                    }
                    else
                    {
                      Write( F, Separator ); Write( F, "0.0" );
                    }
                    WriteLn( F );
                  }
              }
            }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
        DSSGlobals::GlobalResult = Filenm;
    /*  }
      __finally
      {*/
        cBuffer.resize(0);
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    void ExportUnserved( String Filenm, bool UE_Only )
    {
      TTextRec F;
      TLoadObj* pLoad;
      bool DoIt = false;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Load, Bus, kW, EEN_Factor,  UE_Factor" );

         // Load
        pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
        while ( pLoad != NULL )
        {
          if ( ( (TDSSCktElement*) pLoad )->Get_Enabled() )
          {
            DoIt = false;
            if ( UE_Only )
            {
              if ( pLoad->Get_Unserved(ActiveActor) )
                DoIt = true;
            }
            else
              if ( pLoad->Get_ExceedsNormal( ActiveActor ) )
                DoIt = true;
            if ( DoIt )
            {
              Write( F, UpperCase( ( (TDSSObject*) pLoad )->get_Name() ) ); Write( F, ", " );
              Write( F, ( (TDSSCktElement*) pLoad )->GetBus( 1 ) ); Write( F, ", " );
              Write( F, Format("%8.3f", pLoad->kWBase)); Write(F, ", ");
              Write( F, Format("%8.3f", pLoad->EEN_Factor)); Write( F, ", " );
              Write( F, Format("%8.3f", pLoad->UE_Factor));
              WriteLn( F );
            }
          }
          pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }


    void ExportYprim( String Filenm )

    /*Exports  YPrim matrices for all  Circuit Elements*/
    {
      TTextRec F;
      int i = 0, j = 0, k = 0;
      pComplexArray cValues;
      if ( ActiveCircuit[ActiveActor] == NULL )
        return;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with0->NumDevices, k = 1; k <= stop; k++)
            {
              with0->Set_ActiveCktElement((TDSSCktElement*) with0->CktElements.Get( k ));
              if (with0->get_FActiveCktElement()->Get_Enabled() )
              {
                if ( ( dynamic_cast< TPDElement* >(with0->get_FActiveCktElement() ) ) || ( dynamic_cast< TPCElement* >(with0->get_FActiveCktElement() ) ) )
                  /*# with ActiveCktElement do */
                  {
                    auto with1 = with0->get_FActiveCktElement();
                    Write( F, with1->ParentClass->get_myClass_name() ); Write( F, '.' ); WriteLn( F, UpperCase( with1->get_Name() ) );
                    cValues = with1->GetYPrimValues( ALL_YPRIM );
                    for ( int stop = with1->Yorder, i = 1; i <= stop; i++)
                    {
                        for (int stop = with1->Yorder, j = 1; j <= stop; j++)
                        {
                            Write(F, Format(" % -13.10g, % -13.10g, ", cValues[(i + (j - 1) * with1->Yorder) - 1].re, cValues[(i + (j - 1) * with1->Yorder) - 1].im));
                        }
                      WriteLn( F );
                    }
                  }
              }
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // Exports the Jacobian matrix calculated when using NCIM solution algorithm
    void ExportJacobian(String Filenm)
    {
        TTextRec F;
        unsigned int i = 0, j = 0, p = 0;
        klusparseset_t hY;
        unsigned int NBus = 0, nNZ = 0;
        std::vector <unsigned int> ColPtr;
        std::vector <unsigned int> RowIdx;
        std::vector <complex> cVals;

        if (ActiveCircuit[ActiveActor] == NULL)
            return;
        hY = ActiveCircuit[ActiveActor]->Solution->Jacobian;
        if (!hY)
        {
            DoSimpleMsg("Jacobian Matrix not Built.", 222);
            return;
        }
        // this compresses the entries if necessary - no extra work if already solved
        FactorSparseMatrix(hY);
        GetNNZ(hY, &nNZ);
        GetSize(hY, &NBus); // we should already know this

        try
        {
            AssignFile(F, Filenm);
            Rewrite(F);
            IOResultToException();
            ColPtr.resize(nNZ);
            RowIdx.resize(nNZ);
            cVals.resize(nNZ);
            GetTripletMatrix(hY, nNZ, &(RowIdx[0]), &(ColPtr[0]), &(cVals[0]));
            WriteLn(F, "Row,Col,Value");
            for (i = 0; i < nNZ; i++)
            {
                WriteLn(F, Format("%d,%d,%.10g", RowIdx[i], ColPtr[i], cVals[i].re));
            }
            DSSGlobals::GlobalResult = Filenm;
            CloseFile(F);
        }
        catch (...)
        {
            //
        }
    }

    // Exports the deltaF vector obtained in the last iteration of the NCIM solution algorithm (if used)
    void ExportdeltaF(String Filenm)
    {
        TTextRec F;
        double  re          = 0.0;
        auto&   mydeltaF    = ActiveCircuit[ActiveActor]->Solution->deltaF;
        if (ActiveCircuit[ActiveActor] == NULL)
            return;
        if (mydeltaF.size() == 0)
            return;
        try
        {
            AssignFile(F, Filenm);
            Rewrite(F);
            IOResultToException();
            int stop        = mydeltaF.size();
            for (int i = 0; i < stop; i++)
            {
                    re = mydeltaF[i].re;
                    WriteLn(F, Format("%.10g", re));
            }
            DSSGlobals::GlobalResult = Filenm;
            CloseFile(F);
        }
        catch (...)
        {
            //
        }
    }
    
    // Exports the deltaZ vector obtained in the last iteration of the NCIM solution algorithm (if used)
    void ExportdeltaZ(String Filenm)
    {
        TTextRec F;
        double  re = 0.0;
        auto& mydeltaZ = ActiveCircuit[ActiveActor]->Solution->deltaZ;
        if (ActiveCircuit[ActiveActor] == NULL)
            return;
        if (mydeltaZ.size() == 0)
            return;
        try
        {
            AssignFile(F, Filenm);
            Rewrite(F);
            IOResultToException();
            int stop = mydeltaZ.size();
            for (int i = 0; i < stop; i++)
            {
                re = mydeltaZ[i].re;
                WriteLn(F, Format("%.10g", re));
            }
            DSSGlobals::GlobalResult = Filenm;
            CloseFile(F);
        }
        catch (...)
        {
            //
        }
    }

    // illustrate retrieval of System Y using compressed column format
    void ExportY( String Filenm, bool TripletOpt )

    /*Exports System Y Matrix in Node Order*/
    {
      TTextRec F;
      unsigned int i = 0, j = 0, p = 0;
      unsigned int col = 0, Row = 0;
      klusparseset_t hY;
      unsigned int NBus = 0, nNZ = 0;
      unsigned int* ColPtr = nullptr;
      unsigned int* RowIdx = nullptr;
      complex* cVals = nullptr;
      double re = 0.0, im = 0.0;
      if ( ActiveCircuit[ActiveActor] == NULL )
        return;
      hY = ActiveCircuit[ActiveActor]->Solution->hY;
      if ( ! hY )
      {
        DoSimpleMsg( "Y Matrix not Built.", 222 );
        return;
      }
      // this compresses the entries if necessary - no extra work if already solved
      FactorSparseMatrix(hY);
      GetNNZ(hY, &nNZ );
      GetSize(hY, &NBus ); // we should already know this

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        if ( TripletOpt )
        {
          ColPtr = new unsigned[nNZ];
          RowIdx = new unsigned[nNZ];
          cVals = new complex[nNZ];
          GetTripletMatrix( hY, nNZ, RowIdx, ColPtr, &(cVals[0]));
          WriteLn( F, "Row,Col,G,B" );
          for ( int stop = nNZ - 1, i = 0; i <= stop; i++)
          {
            col = ColPtr[i] + 1;
            Row = RowIdx[i] + 1;
            if ( Row >= col )
            {
              re = cVals[i].re;
              im = cVals[i].im;
              WriteLn( F, Format("%d,%d,%.10g,%.10g", Row, col, re, im));
            }
          }
        }
        else
        {
          ColPtr = new unsigned[NBus + 1];
          RowIdx = new unsigned[nNZ];
          cVals  = new complex[nNZ];

          GetCompressedMatrix( hY, NBus + 1, nNZ, ColPtr, RowIdx, cVals);
           /*Write out fully qualified Bus Names*/
          /*# with ActiveCircuit[ActiveActor] do */
          {
            auto with0 = ActiveCircuit[ActiveActor];
            {
              WriteLn( F, Format("% d, ", with0->NumNodes ) );
      /*        For i := 1 to NumNodes DO BEGIN
                 j :=  MapNodeToBus^[i].BusRef;
                 Write(F, Format('%s.%-d, +j,',[BusList.Get(j), MapNodeToBus^[i].NodeNum]));
              END;
              Writeln(F);
      */
              for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
              {
                j = with0->MapNodeToBus[i - 1].BusRef;
                Write( F, Format("\"% s.%-d\", ", UpperCase(with0->BusList.Get(j)).c_str(), with0->MapNodeToBus[i - 1].NodeNum) );
                for ( int stop = with0->NumNodes, j = 1; j <= stop; j++)
                {
                  re = 0.0;
                  im = 0.0;
                    // search for a non-zero element [i,j]
                    //  DSS indices are 1-based, KLU indices are 0-based
                  for ( int stop = ColPtr[j] - 1, p = ColPtr[j - 1]; p <= stop; p++)
                  {
                    if ( RowIdx[p] + 1 == i )
                    {
                      re = cVals[p].re;
                      im = cVals[p].im;
                    }
                  }
                  Write( F, Format("%-13.10g, +j %-13.10g,", re, im));
                }
                WriteLn( F );
              }
            }
          }
        }
        DSSGlobals::GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    void ExportSeqZ( String Filenm )

    // Export Symmetrical Component Impedances at each bus

    {
      TTextRec F;
      int i = 0;
      complex Z1, Z0;
      double X1R1 = 0.0, X0R0 = 0.0;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Bus,  NumNodes, R1, X1, R0, X0, Z1, Z0, \"X1/R1\", \"X0/R0\"" );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            {
              Z1 = with0->Buses[i - 1]->Get_Zsc1();
              Z0 = with0->Buses[i - 1]->Get_Zsc0();
              if ( Z1.re != 0.0 )
                X1R1 = double( Z1.im ) / Z1.re;
              else
                X1R1 = 1000.0;
              if ( Z0.re != 0.0 )
                X0R0 = double( Z0.im ) / Z0.re;
              else
                X0R0 = 1000.0;
              WriteLn( F, Format("\"%s\", %d, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %8.4g, %8.4g",
                  UpperCase(with0->BusList.Get(i)).c_str(),
                  with0->Buses[i - 1]->get_FNumNodesThisBus(),
                  Z1.re, Z1.im, Z0.re, Z0.im,
                  cabs(Z1), cabs(Z0), X1R1, X0R0));
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }


    void ExportUuids( String Filenm )
    {
      TTextRec F;
      TLineCode* clsLnCd;
      TLineGeometry* clsGeom;
      TWireData* clsWire;
      TXfmrCode* clsXfCd;
      TLineSpacing* clsSpac;
      TTSData* clsTape;
      TCNData* clsConc;
      TNamedObject* pName;
      int i = 0;
      try
      {
        clsLnCd = (TLineCode*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "linecode" ) );
        clsWire = (TWireData*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "wiredata" ) );
        clsGeom = (TLineGeometry*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "linegeometry" ) );
        clsXfCd = (TXfmrCode*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "xfmrcode" ) );
        clsSpac = (TLineSpacing*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "linespacing" ) );
        clsTape = (TTSData*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "TSData" ) );
        clsConc = (TCNData*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "CNData" ) );
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        pName = (TNamedObject*) ActiveCircuit[ActiveActor];
        WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " "  + pName->Get_ID() );
        for ( int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
        {
          pName = ActiveCircuit[ActiveActor]->Buses[0];
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
        }
        pName = (TNamedObject*) ActiveCircuit[ActiveActor]->CktElements.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ActiveCircuit[ActiveActor]->CktElements.Get_Next();
        }
        pName = (TNamedObject*) ( (TDSSClass*) clsLnCd )->ElementList.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ( (TDSSClass*) clsLnCd )->ElementList.Get_Next();
        }
        pName = (TNamedObject*) ( (TDSSClass*) clsWire )->ElementList.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ( (TDSSClass*) clsWire )->ElementList.Get_Next();
        }
        pName = (TNamedObject*) ( (TDSSClass*) clsGeom )->ElementList.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ( (TDSSClass*) clsGeom )->ElementList.Get_Next();
        }
        pName = (TNamedObject*) ( (TDSSClass*) clsXfCd )->ElementList.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ( (TDSSClass*) clsXfCd)->ElementList.Get_Next();
        }
        pName = (TNamedObject*) ( (TDSSClass*) clsSpac )->ElementList.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ( (TDSSClass*) clsSpac )->ElementList.Get_Next();
        }
        pName = (TNamedObject*) ( (TDSSClass*) clsTape )->ElementList.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ( (TDSSClass*)  clsTape )->ElementList.Get_Next();
        }
        pName = (TNamedObject*) ( (TDSSClass*) clsConc )->ElementList.Get_First();
        while ( pName != NULL )
        {
          WriteLn( F, pName->Get_myPName() + "." + pName->Get_myLName() + " " + pName->Get_ID());
          pName = (TNamedObject*) ( (TDSSClass*) clsConc )->ElementList.Get_Next();
        }
        WriteHashedUUIDs( F );
    /* }
      __finally
      {*/
        CloseFile( F );
        FreeUuidList();
      }
      catch (...)
      {
          //
      }
    }

    void ExportCounts( String Filenm )
    {
      TTextRec F;
      TDSSClass* cls;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Format: DSS Class Name = Instance Count" );
        WriteLn( F );
        cls = (TDSSClass*) DSSClassList[ActiveActor].Get_First();
        while ( cls != NULL )
        {
            WriteLn( F, Format("%s = %d", cls->get_myClass_name().c_str(), cls->Get_ElementCount()));
            cls = (TDSSClass*) DSSClassList[ActiveActor].Get_Next();
        }
      /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }


    void ExportSummary( String Filenm )
    {
      TTextRec F;
      complex cPower, cLosses;
      try
      {
        AssignFile( F, Filenm );
        if ( FileExists( Filenm ) )
        {
          Append( F );
          IOResultToException();
        }
        else
        {    // Create and write the header
          Rewrite( F );
          IOResultToException();
          Write( F, "DateTime, CaseName, " );
          Write( F, "Status, Mode, Number, LoadMult, NumDevices, NumBuses, NumNodes" );
          Write( F, ", Iterations, ControlMode, ControlIterations" );
          Write( F, ", MostIterationsDone" );
          if ( ActiveCircuit[ActiveActor] != NULL )
            if ( ActiveCircuit[ActiveActor]->Issolved && !ActiveCircuit[ActiveActor]->get_FBusNameRedefined() )
            {
              Write( F, ", Year, Hour, MaxPuVoltage, MinPuVoltage, TotalMW, TotalMvar" );
              Write( F, ", MWLosses, pctLosses, MvarLosses, Frequency" );
            }
          WriteLn( F );
        }
        std::time_t t = std::time(nullptr);
        std::tm tm = *std::localtime(&t);

        // Format hour
        char buffer[20];
        std::strftime(buffer, sizeof(buffer), "%m/%d/%Y %I:%M:%S", &tm);

        // remove trailing zeros to the left (matchign delphi)
        char buffer2[20];
        int idx2 = 0;
        bool writeD = false;
        for (int idx = 0; idx < 20; idx++)
        {
            writeD = false;
            if (buffer[idx] == '0')
            {
                if (idx != 0)
                    writeD = (buffer[idx - 1] != ' ');
            }
            else
                writeD = true;
            if (writeD)
            {
                buffer2[idx2] = buffer[idx];
                idx2++;
            }
        }

        // Create string object from buffer
	      std::string am_pm = (tm.tm_hour < 12 ) ? "AM" : "PM";		
        std::string formattedTime(buffer2);
	      formattedTime += " " + am_pm;
        //Write( F, "\"" + DateTimeToStr(Now())  + "\"" + ", " );
        Write(F, "\"" + formattedTime + "\"" + ", ");

        if ( ActiveCircuit[ActiveActor] != NULL )
          Write( F, ActiveCircuit[ActiveActor]->get_FCaseName() + ", ");
        else
          Write( F, "NONE, " );
        if ( ActiveCircuit[ActiveActor]->Issolved )
          Write( F, "SOLVED" );
        else
          Write( F, "UnSolved" );
        Write( F, ", " + GetSolutionModeID() );
        Write(F, Format(", %d", ActiveCircuit[ActiveActor]->Solution->NumberOfTimes));
        Write(F, Format(", %8.3f", ActiveCircuit[ActiveActor]->get_FLoadMultiplier()));
        Write(F, Format(", %d", ActiveCircuit[ActiveActor]->NumDevices));
        Write(F, Format(", %d", ActiveCircuit[ActiveActor]->NumBuses));
        Write(F, Format(", %d", ActiveCircuit[ActiveActor]->NumNodes));
        Write(F, Format(", %d", ActiveCircuit[ActiveActor]->Solution->Iteration));
        Write(F, ", " + GetControlModeID() );
        Write(F, Format(", %d", ActiveCircuit[ActiveActor]->Solution->ControlIteration));
        Write(F, Format(", %d", ActiveCircuit[ActiveActor]->Solution->MostIterationsDone));

        if ( ActiveCircuit[ActiveActor] != NULL )
          if ( ActiveCircuit[ActiveActor]->Issolved && !ActiveCircuit[ActiveActor]->get_FBusNameRedefined() )
          {
              Write(F, Format(", %d", ActiveCircuit[ActiveActor]->Solution->get_Fyear()));
              Write(F, Format(", %d", ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour));
              Write(F, Format(", %-.5g", GetMaxPUVoltage()));
              Write(F, Format(", %-.5g", GetMinPUVoltage(true)));

              cPower = cmulreal( GetTotalPowerFromSources( ActiveActor ), 0.000001 );  // MVA
              Write(F, Format(", %-.6g", cPower.re));
              Write(F, Format(", %-.6g", cPower.im));

                cLosses = cmulreal( ActiveCircuit[ActiveActor]->Get_Losses(ActiveActor), 0.000001 );
                if (cPower.re != 0.0)
                {
                    Write(F, Format(", %-.6g, %-.4g", cLosses.re, (double(cLosses.re) / cPower.re * 100.0)));
                }
                else
                    Write(F, "Total Active Losses:   ****** MW, (*** %%)");

              Write(F, Format(", %-.6g", cLosses.im));
              Write(F, Format(", %-g", ActiveCircuit[ActiveActor]->Solution->get_FFrequency()));
          }
        WriteLn( F );
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    void ExportBusCoords( String Filenm )
    // Export bus x, y coordinates

    {
      TTextRec F;
      int i = 0;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
          {
              if (with0->Buses[i - 1]->CoordDefined)
              {
                  WriteLn(F, Format("%s, %-13.11g, %-13.11g", CheckForBlanks(UpperCase(with0->BusList.Get(i))).c_str(), with0->Buses[i - 1]->x, with0->Buses[i - 1]->y));
              }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }


    void WriteNewLine( Textfile& F, const String CktElementName, double DistFromMeter1, double puV1, double DistFromMeter2, double puV2, int ColorCode, int Thickness, int LineType, int MarkCenter, int CenterMarkerCode, int NodeMarkerCode, int NodeMarkerWidth )
    {
       Write( F, Format( "%s, %.6g, %.6g, %.6g, %.6g,",  UpperCase( CktElementName ).c_str(), DistFromMeter1, puV1, DistFromMeter2, puV2 ));
       Write( F, Format( "%d, %d, %d, ",  ColorCode, Thickness, LineType ));
       Write( F, Format( "%d, ",  MarkCenter ));
       Write( F, Format( "%d, %d, %d",  CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth ));
      WriteLn( F );
    }

    void ExportProfile( String Filenm, int PhasesToPlot )
    {
      int iEnergyMeter = 0;
      TEnergyMeterObj* ActiveEnergyMeter;
      TDSSCktElement* PresentCktElement;
      TDSSBus* Bus1;
      TDSSBus* Bus2;
      double puV1 = 0.0, puV2 = 0.0;
      int iphs = 0;
      int iphs2 = 0;
      String S;
      TTextRec F;
      int LineType = 0;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        Write( F, "Name, Distance1, puV1, Distance2, puV2, Color, Thickness, Linetype, Markcenter, Centercode, NodeCode, NodeWidth," );

        /*New graph created before this routine is entered*/
        switch ( PhasesToPlot )
        {
          case PROFILELL: case PROFILELLALL: case PROFILELLPRI:
            S = "L-L Voltage Profile";
          break;
        default:
          S = "L-N Voltage Profile";
        }
        Write( F, "Title=" ); Write( F, S ); WriteLn( F, ", Distance in km" );
        iEnergyMeter = EnergyMeterClass[ActiveActor]->Get_First();
        while ( iEnergyMeter > 0 )
        {
          ActiveEnergyMeter = (TEnergyMeterObj*) EnergyMeterClass[ActiveActor]->GetActiveObj();
              /*Go down each branch list and draw a line*/
          PresentCktElement = (TDSSCktElement*) ActiveEnergyMeter->BranchList->Get_First();
          while ( PresentCktElement != NULL )
          {
            if ( IslineElement( PresentCktElement ) )
              /*# with ActiveCircuit[ActiveActor] do */
              {
                auto with0 = ActiveCircuit[ActiveActor];
                {
                  Bus1 = with0->Buses[PresentCktElement->Terminals[1 - 1].BusRef - 1];
                  Bus2 = with0->Buses[PresentCktElement->Terminals[2 - 1].BusRef - 1];
                /*Now determin which phase to plot*/
                  if ( ( Bus1->kVBase > 0.0 ) && ( Bus2->kVBase > 0.0 ) )
                    switch ( PhasesToPlot )
                    {
                      case
                      /*3ph only*/ PROFILE3PH:
                        if ( ( PresentCktElement->Get_NPhases() >= 3 ) && ( Bus1->kVBase > 1.0 ) )
                          for ( int stop = 3, iphs = 1; iphs <= stop; iphs++)
                          {
                            puV1 = double( double( cabs(with0->Solution->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs ) )] ) ) ) / Bus1->kVBase / 1000.0;
                            puV2 = double( double( cabs(with0->Solution->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs ) )] ) ) ) / Bus2->kVBase / 1000.0;
                            WriteNewLine( F, ( (TDSSObject*) PresentCktElement )->get_Name(), Bus1->DistFromMeter, puV1, Bus2->DistFromMeter, puV2, iphs, 2, 0, 0, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth );
                          }
                      break;
                      /*Plot all phases present (between 1 and 3)*/
                      case PROFILEALL:
                      {
                        for ( int stop = 3, iphs = 1; iphs <= stop; iphs++)
                          if ( ( Bus1->FindIdx( iphs ) > 0 ) && ( Bus2->FindIdx( iphs ) > 0 ) )
                          {
                            if ( Bus1->kVBase < 1.0 )
                              LineType = 2;
                            else
                              LineType = 0;
                            puV1 = double( double( cabs(with0->Solution->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs ) )] ) ) ) / Bus1->kVBase / 1000.0;
                            puV2 = double( double( cabs(with0->Solution->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs ) )] ) ) ) / Bus2->kVBase / 1000.0;
                            WriteNewLine( F, ( (TDSSObject*) PresentCktElement )->get_Name(), Bus1->DistFromMeter, puV1, Bus2->DistFromMeter, puV2, iphs, 2, LineType, 0, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth );
                          }
                      }
                      break;
                      /*Plot all phases present (between 1 and 3) for Primary only*/
                      case PROFILEALLPRI:
                      {
                        if ( Bus1->kVBase > 1.0 )
                          for ( int stop = 3, iphs = 1; iphs <= stop; iphs++)
                            if ( ( Bus1->FindIdx( iphs ) > 0 ) && ( Bus2->FindIdx( iphs ) > 0 ) )
                            {
                              if ( Bus1->kVBase < 1.0 )
                                LineType = 2;
                              else
                                LineType = 0;
                              puV1 = double( double( cabs(with0->Solution->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs ) )] ) ) ) / Bus1->kVBase / 1000.0;
                              puV2 = double( double( cabs(with0->Solution->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs ) )] ) ) ) / Bus2->kVBase / 1000.0;
                              WriteNewLine( F, ( (TDSSObject*) PresentCktElement )->get_Name(), Bus1->DistFromMeter, puV1, Bus2->DistFromMeter, puV2, iphs, 2, LineType, 0, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth );
                            }
                      }
                      break;					
                      case PROFILELL:
                      {
                        if ( ( (TDSSCktElement*) PresentCktElement )->Get_NPhases() >= 3 )
                          for ( int stop = 3, iphs = 1; iphs <= stop; iphs++)
                          {
                            iphs2 = iphs + 1;
                            if ( iphs2 > 3 )
                              iphs2 = 1;
                            if ( ( Bus1->FindIdx( iphs ) > 0 ) && ( Bus2->FindIdx( iphs ) > 0 ) && ( Bus1->FindIdx( iphs2 ) > 0 ) && ( Bus2->FindIdx( iphs2 ) > 0 ) )
                            {
                              if ( Bus1->kVBase < 1.0 )
                                LineType = 2;
                              else
                                LineType = 0;
                              /*# with Solution do */
                              {
                                auto with1 = with0->Solution;
                                puV1 = double( double( cabs( csub( with1->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs ) )], with1->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs2 ) )] ) ) ) ) / Bus1->kVBase / 1732.0;
                                puV2 = double( double( cabs( csub( with1->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs ) )], with1->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs2 ) )] ) ) ) ) / Bus2->kVBase / 1732.0;
                              }
                              WriteNewLine( F, ( (TDSSObject*) PresentCktElement )->get_Name(), Bus1->DistFromMeter, puV1, Bus2->DistFromMeter, puV2, iphs, 2, LineType, 0, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth );
                            }
                          }
                      }
                      break;
                      case PROFILELLALL:
                      {
                        for ( int stop = 3, iphs = 1; iphs <= stop; iphs++)
                        {
                          iphs2 = iphs + 1;
                          if ( iphs2 > 3 )
                            iphs2 = 1;
                          if ( ( Bus1->FindIdx( iphs ) > 0 ) && ( Bus2->FindIdx( iphs ) > 0 ) && ( Bus1->FindIdx( iphs2 ) > 0 ) && ( Bus2->FindIdx( iphs2 ) > 0 ) )
                          {
                            if ( Bus1->kVBase < 1.0 )
                              LineType = 2;
                            else
                              LineType = 0;
                            /*# with Solution do */
                            {
                              auto with1 = with0->Solution;
                              puV1 = double( double( cabs( csub( with1->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs ) )], with1->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs2 ) )] ) ) ) ) / Bus1->kVBase / 1732.0;
                              puV2 = double( double( cabs( csub(with1->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs ) )], with1->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs2 ) )] ) ) ) ) / Bus2->kVBase / 1732.0;
                            }
                            WriteNewLine( F, ( (TDSSObject*) PresentCktElement )->get_Name(), Bus1->DistFromMeter, puV1, Bus2->DistFromMeter, puV2, iphs, 2, LineType, 0, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth );
                          }
                        }
                      }
                      break;
                      case PROFILELLPRI:
                      {
                        if ( Bus1->kVBase > 1.0 )
                          for ( int stop = 3, iphs = 1; iphs <= stop; iphs++)
                          {
                            iphs2 = iphs + 1;
                            if ( iphs2 > 3 )
                              iphs2 = 1;
                            if ( ( Bus1->FindIdx( iphs ) > 0 ) && ( Bus2->FindIdx( iphs ) > 0 ) && ( Bus1->FindIdx( iphs2 ) > 0 ) && ( Bus2->FindIdx( iphs2 ) > 0 ) )
                            {
                              if ( Bus1->kVBase < 1.0 )
                                LineType = 2;
                              else
                                LineType = 0;
                              /*# with Solution do */
                              { 
                                auto with1 = with0->Solution;
                                puV1 = double( double( cabs( csub( with1->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs ) )], with1->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs2 ) )] ) ) ) ) / Bus1->kVBase / 1732.0;
                                puV2 = double( double( cabs( csub(with1->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs ) )], with1->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs2 ) )] ) ) ) ) / Bus2->kVBase / 1732.0;
                              }
                              WriteNewLine( F, ( (TDSSObject*) PresentCktElement )->get_Name(), Bus1->DistFromMeter, puV1, Bus2->DistFromMeter, puV2, iphs, 2, LineType, 0, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth );
                            }
                          }
                      }
                      break;
                    default:     // plot just the selected phase
                      iphs = PhasesToPlot;
                      if ( ( Bus1->FindIdx( iphs ) > 0 ) && ( Bus2->FindIdx( iphs ) > 0 ) )
                      {
                        if ( Bus1->kVBase < 1.0 )
                          LineType = 2;
                        else
                          LineType = 0;
                        puV1 = double( double( cabs( with0->Solution->NodeV[Bus1->GetRef( Bus1->FindIdx( iphs ) )] ) ) ) / Bus1->kVBase / 1000.0;
                        puV2 = double( double( cabs( with0->Solution->NodeV[Bus2->GetRef( Bus2->FindIdx( iphs ) )] ) ) ) / Bus2->kVBase / 1000.0;
                        WriteNewLine( F, ( (TDSSObject*) PresentCktElement )->get_Name(), Bus1->DistFromMeter, puV1, Bus2->DistFromMeter, puV2, iphs, 2, LineType, 0, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth );
                      }
                    }
                }
              }
            PresentCktElement = (TDSSCktElement*) ActiveEnergyMeter->BranchList->Get_Forward();
          }
          iEnergyMeter = EnergyMeterClass[ActiveActor]->Get_Next();
        }
        GlobalResult = Filenm;
    /*  }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    void ExportEventLog( String Filenm )
    // Export the present set of EventStrings

    {
      //EventStrings[ActiveActor].SaveToFile( Filenm );
      SaveStrArr2File(&(EventStrings[ActiveActor]), Filenm);
      GlobalResult = Filenm;
    }


    void ExportErrorLog( String Filenm )
    // Export the present set of ErrorStrings

    {
      //ErrorStrings[ActiveActor].SaveToFile( Filenm );
      SaveStrArr2File(&(EventStrings[ActiveActor]), Filenm);
      GlobalResult = Filenm;
    }


    void ExportIncMatrix( String Filenm )
    {
      TTextRec F;
      int i = 0;
      /*# with ActiveCircuit[ActiveActor]->Solution do */
      {
        auto with0 = ActiveCircuit[ActiveActor]->Solution;
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Row,Col,Value" );
        for ( int stop = ( with0->IncMat.NZero() - 1 ), i = 0; i <= stop; i++)
        {
          WriteLn( F, IntToStr( with0->IncMat.data[i][0] ) + "," + IntToStr( with0->IncMat.data[i][1] ) + "," + IntToStr( with0->IncMat.data[i][2] ) );
        }
        GlobalResult = Filenm;
        CloseFile( F );
      }
    }


    void ExportIncMatrixRows( String Filenm )
    {
      TTextRec F;
      int i = 0;
      /*# with ActiveCircuit[ActiveActor]->Solution do */
      {
        auto with0 = ActiveCircuit[ActiveActor]->Solution;
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "B2N Incidence Matrix Row Names (PDElements)" );
        for ( int stop = ( with0->Inc_Mat_Rows.size() - 1 ), i = 0; i <= stop; i++)
        {
          WriteLn( F, with0->Inc_Mat_Rows[i] );
        }
        GlobalResult = Filenm;
        CloseFile( F );
      }
    }
    //-------------------------------------------------------------------



    void ExportIncMatrixCols( String Filenm )
    {
      TTextRec F;
      int i = 0;
      /*# with ActiveCircuit[ActiveActor]->Solution do */
      {
        auto with0 = ActiveCircuit[ActiveActor]->Solution;
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "B2N Incidence Matrix Column Names (Buses)" );
        for ( int stop = ( with0->Inc_Mat_Cols.size() - 1 ), i = 0; i <= stop; i++)
        {
          WriteLn( F, with0->Inc_Mat_Cols[i] );
        }
        GlobalResult = Filenm;
        CloseFile( F );
      }
    }
    //-------------------------------------------------------------------



    void ExportBusLevels( String Filenm )
    {
      TTextRec F;
      int i = 0;
      /*# with ActiveCircuit[ActiveActor]->Solution do */
      {
        auto with0 = ActiveCircuit[ActiveActor]->Solution;
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "B2N Incidence Matrix Column Names (Buses) and their level within the matrix" );
        WriteLn( F, "Bus Name,Bus Level" );
        for ( int stop = ( with0->Inc_Mat_Cols.size() - 1 ), i = 0; i <= stop; i++)
        {
          WriteLn( F, with0->Inc_Mat_Cols[i] + "," + IntToStr( with0->Inc_Mat_levels[i] ) );
        }
        GlobalResult = Filenm;
        CloseFile( F );
      }
    }

    //-------------------------------------------------------------------



    void ExportLaplacian( String Filenm )
    {
      TTextRec F;
      int i = 0;
      /*# with ActiveCircuit[ActiveActor]->Solution do */
      {
        auto with0 = ActiveCircuit[ActiveActor]->Solution;
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Row,Col,Value" );
        for ( int stop = ( with0->Laplacian.NZero() - 1 ), i = 0; i <= stop; i++)
        {
          WriteLn( F, IntToStr( with0->Laplacian.data[i][0] ) + "," + IntToStr(with0->Laplacian.data[i][1] ) + "," + IntToStr(with0->Laplacian.data[i][2] ) );
        }
        GlobalResult = Filenm;
        CloseFile( F );
      }
    }
    //-------------------------------------------------------------------



    void ExportZLL( String Filenm )
    {
      TTextRec F;
      int i = 0;
      if ( ADiakoptics )
      {
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor]->Solution do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          auto with1 = ActiveCircuit[ActiveActor]->Solution;
          {
            AssignFile( F, Filenm );
            Rewrite( F );
            IOResultToException();
            WriteLn( F, "Row,Col,Value(Real), Value(Imag)" );
            for ( int stop = ( with0->ZLL.NZero() - 1 ), i = 0; i <= stop; i++)
            {
              WriteLn( F, IntToStr(with0->ZLL.CData[i].Row ) + "," + IntToStr(with0->ZLL.CData[i].col ) + "," + to_string( with0->ZLL.CData[i].Value.re ) + "," + to_string( with0->ZLL.CData[i].Value.im ) );
            }
            GlobalResult = Filenm;
            CloseFile( F );
          }
        }
      }
    }
    //-------------------------------------------------------------------



    void ExportZCC( String Filenm )
    {
      TTextRec F;
      int i = 0;
      if ( ADiakoptics )
      {
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor]->Solution do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            AssignFile( F, Filenm );
            Rewrite( F );
            IOResultToException();
            WriteLn( F, "Row,Col,Value(Real), Value(Imag)" );
            for ( int stop = ( with0->ZCC.NZero() - 1 ), i = 0; i <= stop; i++)
            {
              WriteLn( F, IntToStr( with0->ZCC.CData[i].Row ) + "," + IntToStr( with0->ZCC.CData[i].col ) + "," + to_string( with0->ZCC.CData[i].Value.re ) + "," + to_string( with0->ZCC.CData[i].Value.im ) );
            }
            GlobalResult = Filenm;
            CloseFile( F );
          }
        }
      }
    }
    //-------------------------------------------------------------------



    void ExportY4( String Filenm )
    {
      TTextRec F;
      int i = 0;
      if ( ADiakoptics )
      {
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor]->Solution do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            AssignFile( F, Filenm );
            Rewrite( F );
            IOResultToException();
            WriteLn( F, "Row,Col,Value(Real), Value(Imag)" );
            for ( int stop = (with0->Y4.NZero() - 1 ), i = 0; i <= stop; i++)
            {
              WriteLn( F, IntToStr( with0->Y4.CData[i].Row ) + "," + IntToStr( with0->Y4.CData[i].col ) +
                  "," + to_string( with0->Y4.CData[i].Value.re ) + "," + to_string( with0->Y4.CData[i].Value.im ) );
            }
            GlobalResult = Filenm;
            CloseFile( F );
          }
        }
      }
    }
    //-------------------------------------------------------------------



    void ExportC( String Filenm )
    {
      TTextRec F;
      int i = 0;
      if ( ADiakoptics )
      {
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor]->Solution do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            AssignFile( F, Filenm );
            Rewrite( F );
            IOResultToException();
            WriteLn( F, "Row,Col,Value" );
            for ( int stop = ( with0->Contours.NZero() - 1 ), i = 0; i <= stop; i++)
            {
              WriteLn( F, IntToStr( with0->Contours.CData[i].Row ) + "," + 
                  IntToStr( with0->Contours.CData[i].col ) +
                  "," + to_string( with0->Contours.CData[i].Value.re ) );
            }
            GlobalResult = Filenm;
            CloseFile( F );
          }
        }
      }
    }

    //-------------------------------------------------------------------



    void ExportVoltagesElements( String Filenm )

    // Export element voltages, by terminal and node/bus

    {
      int MaxNumNodes = 0;
      int MaxNumTerminals = 0;
      TTextRec F;
      int i = 0, j = 0;
      TDSSCktElement* pElem;
      MaxNumTerminals = 2;
      MaxNumNodes = 0;
      pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_First();
      while ( pElem != NULL )
      {
        MaxNumTerminals = max( MaxNumTerminals, pElem->Get_NTerms() );
        MaxNumNodes = max( MaxNumNodes, pElem->Get_NConds() );
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get_Next();
      }
    /*
        MaxNumNodes := 0;
        With ActiveCircuit Do
        For j := 1 to NumBuses Do
           MaxNumNodes := max(MaxNumNodes, Buses^[j].NumNodesThisBus);
    */
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        Write( F, "Element,NumTerminals" );

           //Write out the header
        for ( int stop = MaxNumTerminals, i = 1; i <= stop; i++)
        {
          Write( F, Format(", Terminal%d", i));
          Write( F, ",NumConductors,NPhases," );
          Write( F, "Bus, BasekV" );
          for (int stop = MaxNumNodes, j = 1; j <= stop; j++)
          {
              Write(F, Format(", Node%d_%d, Magnitude%d_%d, Angle%d_%d, pu%d_%d", i, j, i, j, i, j, i, j));
          }
        }
        WriteLn( F );

           //Go through all the sources
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            pElem = (TDSSCktElement*) with0->Sources.Get_First();
            while ( pElem != NULL )
            {
              if ( pElem->Get_Enabled() )
              {
                WriteElementVoltagesExportFile( F, pElem, MaxNumNodes );
                WriteLn( F );
              }
              pElem = (TDSSCktElement*) with0->Sources.Get_Next();
            }


           //Go through all the PDElements
            pElem = (TDSSCktElement*) with0->PDElements.Get_First();
            while ( pElem != NULL )
            {
              if ( pElem->Get_Enabled() )
              {
                WriteElementVoltagesExportFile( F, pElem, MaxNumNodes );
                WriteLn( F );
              }
              pElem = (TDSSCktElement*) with0->PDElements.Get_Next();
            }



         //Go through all the PCElements
            pElem = (TDSSCktElement*) with0->PCElements.Get_First();
            while ( pElem != NULL )
            {
              if ( pElem->Get_Enabled() )
              {
                WriteElementVoltagesExportFile( F, pElem, MaxNumNodes );
                WriteLn( F );
              }
              pElem = (TDSSCktElement*) with0->PCElements.Get_Next();
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportGICMvar( String Filenm )
    {
      TTextRec F;
      TGICTransformerObj* pElem;
      TGICTransformer* GICClass;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        GICClass =  (TGICTransformer*) GetDSSClassPtr( "GICTransformer" );
        WriteLn( F, "Bus, Mvar, GIC Amps per phase" );
        pElem = (TGICTransformerObj*) ( (TDSSClass*) GICClass )->ElementList.Get_First() ;
        while ( pElem != NULL )
        {
          pElem->WriteVarOutputRecord( F, ActiveActor );
          pElem = (TGICTransformerObj*) ((TDSSClass*)GICClass)->ElementList.Get_Next() ;
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportBusReliability( String Filenm )
    {
        TTextRec F = {};
      int i = 0;

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Bus, Lambda, Num-Interruptions, Num-Customers, Cust-Interruptions, Duration, Total-Miles" );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            /*# with Buses^[i] do */
            {
              auto with1 = with0->Buses[i - 1];
              {
                WriteLn( F, Format("%s, %-.11g, %-.11g, %d, %-.11g, %-.11g, %-.11g",
                    CheckForBlanks(UpperCase(with0->BusList.Get(i))).c_str(),
                    with1->BusFltRate,
                    with1->Bus_Num_Interrupt,
                    with1->BusTotalNumCustomers,
                    with1->BusCustInterrupts,
                    with1->Bus_Int_Duration,
                    with1->BusTotalMiles));
              }
            }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportBranchReliability( String Filenm )
    {
      TTextRec F;
      TPDElement* pElem;
      double SAIFI = 0.0;
      int MaxCustomers = 0;

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Element, Lambda, \"Accumulated-Lambda\", Num-Customers, Total-Customers, Num-Interrupts, Cust-Interruptions, Cust-Durations, Total-Miles, Cust-Miles, SAIFI" );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {

         // Find Maxcustomers of any PDElement for Duke Recloser siting algorithm
            MaxCustomers = 0;
            pElem = (TPDElement*) with0->PDElements.Get_First();
            while ( pElem != NULL )
            {
              if ( pElem->Get_Enabled() )
                /*# with pElem do */
                {
                  TDSSBus& pBus (*( with0->Buses[pElem->Terminals[pElem->FromTerminal - 1].BusRef - 1] ));
                  /*# with pBus do */
                  if ( pBus.BusTotalNumCustomers > MaxCustomers )
                    MaxCustomers = pBus.BusTotalNumCustomers;
                }
              pElem = (TPDElement*) with0->PDElements.Get_Next();
            }


         // write report for PDELEMENTS only
            pElem = (TPDElement*) with0->PDElements.Get_First();
            while ( pElem != NULL )
            {
              if ( pElem->Get_Enabled() )
                /*# with pElem do */
                {
                  TDSSBus& pBus(*( with0->Buses[pElem->Terminals[pElem->FromTerminal - 1].BusRef - 1] ));
                  /*# with pBus do */
                  if ( pBus.BusTotalNumCustomers > 0 )
                    SAIFI = double( pBus.BusCustInterrupts ) / pBus.BusTotalNumCustomers;
                  else
                    SAIFI = 0.0;
                  WriteLn( F, Format("%s.%s, %-.11g, %-.11g, %d, %d, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g",
                      pElem->ParentClass->get_myClass_name().c_str(), pElem->get_Name().c_str(), pElem->BranchFltRate, pElem->AccumulatedBrFltRate,
                      pElem->BranchNumCustomers, pElem->BranchTotalCustomers, pBus.Bus_Num_Interrupt,
                      pElem->BranchTotalCustomers * pBus.Bus_Num_Interrupt, pBus.BusCustDurations,
                      pElem->AccumulatedMilesDownStream, (MaxCustomers - pElem->BranchTotalCustomers) * pElem->AccumulatedMilesDownStream, SAIFI));
                }
              pElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportNodeNames( String Filenm )
    {
      TTextRec F;
      int i = 0;
      int j = 0;

      String BusName;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Node_Name" );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            {
              BusName = with0->BusList.Get( i );
              /*# with Buses^[i] do */
              {
                auto with1 = with0->Buses[i - 1];
                for ( int stop = with1->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                {
                    WriteLn( F, Format("%s.%d ", BusName.c_str(), with1->GetNum(j)));
                }
              }
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int TapPosition( const TTransfObj* Transformer, int iWind )

    /*Assumes 0  is 1.0 per unit tap*/
    {
      int result = 0;
      /*# with Transformer do */
      auto with0 = Transformer;
      result = Round( double( (with0->Get_PresentTap(iWind,ActiveActor) - double( (with0->Get_MaxTap(iWind) + with0->Get_MinTap(iWind) ) ) / 2.0 ) ) / with0->Get_TapIncrement(iWind) );
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ExportTaps( String Filenm )
    {
      TTextRec F;
      int iWind = 0;
      TRegControlObj* pReg;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        WriteLn( F, "Name, Tap, Min, Max, Step, Position" );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            pReg = (TRegControlObj*) with0->RegControls.Get_First();
            while ( pReg != NULL )
            {
              /*# with pReg.Transformer do */
              {
                auto with1 = pReg->Get_Transformer();
                iWind = pReg->Get_Winding();
                Write( F, with1->get_Name() );
                WriteLn( F, Format(", %8.5f, %8.5f, %8.5f, %8.5f, %d", with1->Get_PresentTap(iWind,ActiveActor), with1->Get_MinTap(iWind),
                    with1->Get_MaxTap(iWind), with1->Get_TapIncrement(iWind), TapPosition((pReg->Get_Transformer()), iWind)));
              }
              pReg = (TRegControlObj*) with0->RegControls.Get_Next();
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

    void ExportResult( String Filenm )
    {
      TTextRec F;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        ParserVars->Lookup( "@result" );
        WriteLn( F, ParserVars->Get_Value() );
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }


    void ExportYNodeList( String Filenm )
    {
      int i = 0;
      
      TTextRec F;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        if ( ActiveCircuit[ActiveActor] != NULL )
          /*# with ActiveCircuit[ActiveActor] do */
          {
            auto with0 = ActiveCircuit[ActiveActor];
            {
              for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
              {
                /*# with MapNodeToBus^[i] do */
                {
                  auto with1 = with0->MapNodeToBus[i - 1];
                  WriteLn( F, Format("\"%s.%-d\"", UpperCase(with0->BusList.Get(with1.BusRef)).c_str(), with1.NodeNum));
                }
              }
            }
          }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }


    void ExportYVoltages( String Filenm )
    {
      int i = 0;
      
      TTextRec F;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        if ( ActiveCircuit[ActiveActor] != NULL )
          /*# with ActiveCircuit[ActiveActor] do */
          {
            auto with0 = ActiveCircuit[ActiveActor];
            {
              for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
              {
                /*# with Solution->NodeV^[i] do */
                auto with1 = with0->Solution->NodeV[i];
                WriteLn( F, Format(" % 10.6g, % 10.6g", with1.re, with1.im));
              }
            }
          }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
        //
      }
    }

    void ExportYCurrents( String Filenm )
    {
      int i = 0;
      
      TTextRec F;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        if ( ActiveCircuit[ActiveActor] != NULL )
          /*# with ActiveCircuit[ActiveActor] do */
          {
            auto with0 = ActiveCircuit[ActiveActor];
            {
              complex with1 = cmplx(0, 0);
              for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
              {
                with1 = with0->Solution->Currents[i];
                WriteLn( F, Format(" % 10.6g, % 10.6g", with1.re, with1.im));
              }
            }
          }
        GlobalResult = Filenm;
    /* }
        __finally
        {*/
        CloseFile(F);
      }
      catch (...)
      {
          //
      }
    }


    void ExportSections( String Filenm, TEnergyMeterObj pMeter )
    {
      TEnergyMeterObj* MyMeterPtr;
      int iMeter = 0, i = 0;
      TTextRec F;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

         // Write Header
        WriteLn( F, "Meter, SectionID, SeqIndex, DeviceType, NumCustomers, NumBranches, AvgRepairHrs, TotalDownlineCust, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates, HeadBranch " );
//        if (( &pMeter != NULL ) ) // always evaluates to true
        if ( true )
         // If a meter is specified, export that meter only
          /*# with pMeter do */
          {
            auto with0 = pMeter;
            for ( int stop = with0.SectionCount, i = 1; i <= stop; i++)
              /*# with FeederSections^[i] do */
              {
                auto& with1 = with0.FeederSections[i];
                {
                  ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*) (with0.SequenceList->Get(with1.SeqIndex ) ));
                  WriteLn( F, with0.get_Name() +
                      Format( ", %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s",
                       i, 
                          with1.SeqIndex, 
                          GetOCPDeviceTypeString(with1.OCPDeviceType ).c_str(),
                          with1.NCustomers, 
                          with1.NBranches, 
                          with1.AverageRepairTime, 
                          with1.TotalCustomers, 
                          with1.SectFaultRate,
                          with1.SumFltRatesXRepairHrs, 
                          with1.SumBranchFltRates ) + 
                      FullName( ActiveCircuit[ActiveActor]->get_FActiveCktElement()).c_str() );
                }
              }
          }
        else    // export sections for all meters
        {
          iMeter = EnergyMeterClass[ActiveActor]->Get_First();
          while ( iMeter > 0 )
          {
            MyMeterPtr = (TEnergyMeterObj*) EnergyMeterClass[ActiveActor]->GetActiveObj();
            /*# with MyMeterPtr do */
            {
              for ( int stop = MyMeterPtr->SectionCount, i = 1; i <= stop; i++)
                /*# with FeederSections^[i] do */
                {
                  auto& with1 = MyMeterPtr->FeederSections[i];
                  {
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)( MyMeterPtr->SequenceList->Get( with1.SeqIndex ) ));
                    WriteLn( F, MyMeterPtr->get_Name() +
                        Format( ", %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s",
                         i, with1.SeqIndex, 
                            GetOCPDeviceTypeString(with1.OCPDeviceType ).c_str(),
                            with1.NCustomers, 
                            with1.NBranches, 
                            with1.AverageRepairTime, 
                            with1.TotalCustomers, 
                            with1.SectFaultRate, 
                            with1.SumFltRatesXRepairHrs, 
                            with1.SumBranchFltRates) +
                            FullName( ActiveCircuit[ActiveActor]->get_FActiveCktElement() ).c_str() );
                  }
                }
              iMeter = EnergyMeterClass[ActiveActor]->Get_Next();
            }
          }
        }
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
      }
      catch (...)
      {
          //
      }
    }

}// Namespace ExportResults






