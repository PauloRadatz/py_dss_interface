
#pragma hdrstop

#include "ShowResults.h"
#include <iostream>
#include <string>
#include <algorithm>



#include "Ucomplex.h"
#include "Arraydef.h"
#include "Sysutils.h"
#include "Circuit.h"
#include "DSSClass.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Ucmatrix.h"
#include "Solution.h"
#include "CktElement.h"
#include "Utilities.h"
#include "Bus.h"
#include "mathutil.h"
#include "PDElement.h"
#include "PCElement.h"
#include "generator.h"
#include "Transformer.h"
#include "EnergyMeter.h"
#include "Load.h"
#include "RegControl.h"
#include "ParserDel.h"
#include "CktTree.h"
#include "CmdForms.h"
#include <math.h>
#include "Line.h"
#include "LineUnits.h"
#include "LineGeometry.h"
#include "YMatrix.h"
#include "SwtControl.h"
#include "klusolve.h"
#include "System.h"


namespace ShowResults
{


    int MaxBusNameLength = 0;
    int MaxDeviceNameLength = 0;


    Char TABCHAR = Char( 9 );


    void SetMaxBusNameLength( )
    {
      int i = 0;
      MaxBusNameLength = 4;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
          MaxBusNameLength = max<int64_t>( MaxBusNameLength, with0->BusList.Get( i ).size() );
      }
    }


    void SetMaxDeviceNameLength( )
    {
      int i = 0;
      String DevName, DevClassName;
      MaxDeviceNameLength = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        for ( int stop = with0->NumDevices, i = 1; i <= stop; i++)
        {
          DevName = with0->DeviceList.Get( i );
          DevClassName = ((TDSSClass*) DSSClassList[ActiveActor].Get( with0->DeviceRef[i - 1].CktElementClass))->get_myClass_name();
          MaxDeviceNameLength = max<int64_t>( MaxDeviceNameLength, ( DevName.size( ) + DevClassName.size( ) + 1 ) );
        }
      }
    }

    void WriteSeqVoltages( Textfile& F, int i, bool LL )
    {
      int j = 0, k = 0;
      std::vector <complex> Vph;
      std::vector <complex> VLL;
      std::vector <complex> V012;


      double V0 = 0.0, v1 = 0.0, V2 = 0.0, Vpu = 0.0, V2V1 = 0.0, V0V1 = 0.0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
          Vph.resize(3 + 1);
          VLL.resize(3 + 1);
          V012.resize(3 + 1);

        auto with0 = ActiveCircuit[ActiveActor];
        {
          if ( with0->Buses[i - 1]->get_FNumNodesThisBus() >= 3 )
          {

         // compute sequence voltages for Nodes 1, 2, and 3 only
            /*# with Buses^[i] do */
            {
              auto with1 = with0->Buses[i - 1];
              for ( int stop = 3, j = 1; j <= stop; j++)
                Vph[j] = with0->Solution->NodeV[with1->GetRef( with1->FindIdx( j ) )];
            }
            if ( LL )
            {
              for ( int stop = 3, j = 1; j <= stop; j++)
              {
                k = j + 1;
                if ( k > 3 )
                  k = 1;
                VLL[j] = csub( Vph[j], Vph[k] );
              }
              Phase2SymComp( &VLL[1], &V012[1]);
            }
            else
            {
              Phase2SymComp( &Vph[1], &V012[1] );
            }
            V0 = cabs( V012[1] );
            v1 = cabs( V012[2] );
            V2 = cabs( V012[3] );
          }
          else
          {
            Vph[1] = with0->Solution->NodeV[with0->Buses[i - 1]->GetRef( 1 )];
            V0 = 0.0;
            v1 = cabs( Vph[1] );     // Use first phase value for non-three phase buses
            V2 = 0.0;
          }
          v1 = v1 / 1000.0;    /*Convert to kV*/
          V2 = V2 / 1000.0;
          V0 = V0 / 1000.0;

         // Calc per unit value
          if ( with0->Buses[i - 1]->kVBase != 0.0 )
            Vpu = v1 / with0->Buses[i - 1]->kVBase;
          else
            Vpu = 0.0;
          if ( LL )
            Vpu = Vpu / SQRT3;
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
          System::WriteLn(F, Format("%s %9.4g  %9.4g  %9.4g  %9.4g %9.4g %9.4g",
              Pad(with0->BusList.Get(i), MaxBusNameLength).c_str(), v1, Vpu, V2, V2V1, V0, V0V1));
        }
      } /*With*/
      Vph.clear();
      VLL.clear();
      V012.clear();
    }

    string trim(const std::string& str) {
        size_t first = str.find_first_not_of(' ');
        if (std::string::npos == first) {
            return str;
        }
        size_t last = str.find_last_not_of(' ');
        return str.substr(first, (last - first + 1));
    }


    void WriteBusVoltages( Textfile& F, int i, bool LL )

    // 6/11/14 Modified to write both LL and LN voltages out for LN case

    {
      int nref1 = 0, nref2 = 0, j = 0, k = 0;
      complex Volts, VoltsLL;
      double Vmag = 0.0, VmagLL = 0.0, Vpu = 0.0, VpuLL = 0.0;
      String Bname;
      
      int NodeIdx = 0;
      int jj = 0, kk = 0;
      char NodeName[1000];
      char NodeNameLL[1000];

      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {
          jj = 1;
          /*# with Buses^[i] do */
          {
            auto with1 = with0->Buses[i - 1];
            for ( int stop = with1->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
            {
             // Get the index of the next Node in numerical sequence
              do
              {
                NodeIdx = with1->FindIdx( jj );  // Get the index of the Node that matches jj
                jj++;
              }
              while ( ! ( NodeIdx > 0 ) );
              nref1 = with1->GetRef( NodeIdx );   // Get the onverall node reference number
              Volts = with0->Solution->NodeV[nref1];
              kk = 1; // keep compiler from complaining
              if /*LL and*/ ( jj <= 4 )
             // Line-to-line voltages
              {         // Convert to Line-Line assuming no more than 3 phases
                  // k is 1, 2, or 3
                k = jj;
                if ( k > 3 )
                  k = 1;
                kk = with1->FindIdx( k );
                if ( kk <= with1->get_FNumNodesThisBus() )
                {
                  nref2 = with0->Buses[i - 1]->GetRef( kk ); // reference for next phase in sequence
                  VoltsLL = csub( Volts, with0->Solution->NodeV[nref2] );
                }
              }
              Vmag = cabs( Volts ) * 0.001;
              VmagLL = cabs( VoltsLL ) * 0.001;
              if ( with1->kVBase != 0.0 )
              {
                Vpu = Vmag / with1->kVBase;
                VpuLL = double( VmagLL ) / with1->kVBase / SQRT3;
              }
              else
              {
                Vpu = 0.0;
                VpuLL = 0.0;
              }
              if /*LL and*/ ( jj <= 4 )
              {
                // Vpu := Vpu/SQRT3;
                sprintf(NodeNameLL,"%d-%d", with1->GetNum( NodeIdx ), with1->GetNum( kk ) );
              }
              sprintf(NodeName,"%d  ",with1->GetNum( NodeIdx ) );

              if ( j == 1 )
                Bname = Paddots( with0->BusList.Get( i ), MaxBusNameLength );
              if ( LL )
              {
                if ( kk > 0 )
                {
                  System::WriteLn( F, Format("%s %s %10.5g /_ %6.1f %9.5g %9.3f", UpperCase(Bname).c_str(), NodeNameLL, VmagLL, cdang(VoltsLL), VpuLL, with1->kVBase * SQRT3));
                  Bname = Pad( "   -", MaxBusNameLength );
                }
              }
              else
              {
                System::Write(F, Format("%s %s %10.5g /_ %6.1f %9.5g %9.3f", UpperCase(Bname).c_str(), NodeName, Vmag, cdang(Volts), Vpu, with1->kVBase * SQRT3));

                if ( ( with1->get_FNumNodesThisBus() > 1 ) && ( kk > 0 ) && ( jj <= 4 ) )
                {
                    System::Write( F, Format("        %s %10.5g /_ %6.1f %9.5g", NodeNameLL, VmagLL, cdang(VoltsLL), VpuLL));
                }
                System::WriteLn( F );
                Bname = Pad( "   -", MaxBusNameLength );
              }
            }
          }
        }
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteElementVoltages( Textfile& F, TDSSCktElement* pElem, bool LL )
    {
      int Ncond = 0, Nterm = 0, i = 0, j = 0, k = 0, Nref = 0, bref = 0;
      String BusName;
      complex Volts;
      double Vpu = 0.0, Vmag = 0.0;
      Ncond = pElem->Get_NConds();
      Nterm = pElem->Get_NTerms();


      k = 0;
      BusName = Pad( StripExtension( pElem->Get_FirstBus() ), MaxBusNameLength );
      System::WriteLn(F, "ELEMENT = \"" + pElem->Get_myPName() + "." + UpperCase( pElem->get_Name() ) + "\"" );
      for ( int stop = Nterm, j = 1; j <= stop; j++)
      {
        for ( int stop = Ncond, i = 1; i <= stop; i++)
        {
          k++;
          Nref = pElem->NodeRef[k - 1];
          Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
          Vmag = cabs( Volts ) * 0.001;
          /*# with ActiveCircuit[ActiveActor] do */
          {
            int myNodeNum = 0;
            auto with0 = ActiveCircuit[ActiveActor];
            {
              if ( Nref == 0 )
                Vpu = 0.0;
              else
              {
                bref = with0->MapNodeToBus[Nref - 1].BusRef;
                if ( with0->Buses[bref - 1]->kVBase != 0.0 )
                  Vpu = Vmag / with0->Buses[bref - 1]->kVBase;
                else
                  Vpu = 0.0;
              }
              if ( LL )
                Vpu = Vpu / SQRT3;
              if (Nref != 0) myNodeNum = with0->MapNodeToBus[Nref - 1].NodeNum;
              else           myNodeNum = 0;

              System::WriteLn(F, Format("%s  (%3d) %4d    %13.5g (%8.4g) /_ %6.1f", UpperCase(BusName).c_str(), Nref, myNodeNum, Vmag, Vpu, cdang(Volts)));
            }
          }
        }
        if ( j < Nterm )
        {
          System::WriteLn(F, "------------" );
        }
        BusName = Pad( StripExtension( pElem->Get_NextBus() ), MaxBusNameLength );
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteElementDeltaVoltages( Textfile& F, TDSSCktElement* pElem )
    {
      int Ncond = 0, Node1 = 0, Node2 = 0, Bus1 = 0, Bus2 = 0, i = 0;
      double Vmag = 0.0;
      complex Volts1, Volts2;
      String ElemName;


      Ncond = pElem->Get_NConds();
      ElemName = Pad( pElem->Get_myPName() + "." + UpperCase( pElem->get_Name() ), MaxDeviceNameLength );
      for ( int stop = Ncond, i = 1; i <= stop; i++)
      {
        Node1 = (pElem->NodeRef)[i - 1];
        Node2 = (pElem->NodeRef)[i + Ncond - 1];
        auto with0 = ActiveCircuit[ActiveActor];
        if ( Node1 > 0 )
          Bus1 = ActiveCircuit[ActiveActor]->MapNodeToBus[Node1-1].BusRef;
        else
          Bus1 = 0;
        if ( Node2 > 0 )
          Bus2 = ActiveCircuit[ActiveActor]->MapNodeToBus[Node2-1].BusRef;
        else
          Bus2 = 0;
        if ( ( Bus1 > 0 ) && ( Bus2 > 0 ) )
        {
          Volts1 = ActiveCircuit[ActiveActor]->Solution->NodeV[Node1];   // OK if Node1 or Node2 = 0
          Volts2 = ActiveCircuit[ActiveActor]->Solution->NodeV[Node2];
          Volts1 = csub( Volts1, Volts2 );   // diff voltage
          /*# with ActiveCircuit[ActiveActor] do */
          {
            auto with0 = ActiveCircuit[ActiveActor];
            {
              if ( with0->Buses[Bus1 - 1]->kVBase != with0->Buses[Bus2 - 1]->kVBase )
                Vmag = 0.0;
              else
              {
                if ( with0->Buses[Bus1 - 1]->kVBase > 0.0 )
                  Vmag = double( cabs( Volts1 ) ) / ( 1000.0 * with0->Buses[Bus1 - 1]->kVBase ) * 100.0;
                else
                  Vmag = 0.0;
              }
              System::WriteLn( F, Format("%s,  %4d,    %12.5g, %12.5g, %12.5g, %6.1f", ElemName.c_str(), i, cabs(Volts1), Vmag, with0->Buses[Bus1 - 1]->kVBase, cdang(Volts1)));
            }
          }
        }
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowVoltages(String Filenm, bool LL, int ShowOptionCode)

    // Show bus voltages by circuit element terminal

    {
        TTextRec        F = {};
        int             i = 0;
        TDSSCktElement* pElem = nullptr;
        try
        {
            SetMaxBusNameLength();
            AssignFile(F, Filenm);
            Rewrite(F);
            IOResultToException();
            switch (ShowOptionCode)
            {
            case 0:
            {
                System::WriteLn(F);
                if (LL)
                    System::WriteLn(F, "SYMMETRICAL COMPONENT PHASE-PHASE VOLTAGES BY BUS (for 3-phase buses)");
                else
                    System::WriteLn(F, "SYMMETRICAL COMPONENT VOLTAGES BY BUS (for 3-phase buses)");
                System::WriteLn(F);
                System::Write(F, Pad("Bus", MaxBusNameLength)); System::WriteLn(F, "  Mag:   V1 (kV)    p.u.     V2 (kV)   %V2/V1    V0 (kV)    %V0/V1");
                System::WriteLn(F);
                for (int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
                    WriteSeqVoltages(F, i, LL);
            }
            break; /*ShowOptionCode Case 0*/
            case 1:
            {
                System::WriteLn(F);
                if (LL)
                    System::WriteLn(F, "LINE-LINE VOLTAGES BY BUS & NODE");
                else
                    System::WriteLn(F, "LINE-GROUND and LINE-LINE VOLTAGES BY BUS & NODE");
                System::WriteLn(F);
                if (LL)
                {
                    System::Write(F, Pad("Bus", MaxBusNameLength)); System::WriteLn(F, " Node    VLN (kV)   Angle      pu     Base kV ");
                }
                else
                {
                    System::Write(F, Pad("Bus", MaxBusNameLength)); System::WriteLn(F, " Node    VLN (kV)   Angle      pu     Base kV    Node-Node   VLL (kV)  Angle      pu");
                }
                System::WriteLn(F);
                for (int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
                    WriteBusVoltages(F, i, LL);
            }
            break; /*ShowOptionCode Case 1*/
            case 2:
            {
                System::WriteLn(F);
                System::WriteLn(F, "NODE-GROUND VOLTAGES BY CIRCUIT ELEMENT");
                System::WriteLn(F);
                System::WriteLn(F, "Power Delivery Elements");
                System::WriteLn(F);
                System::Write(F, Pad("Bus", MaxBusNameLength)); System::WriteLn(F, " (node ref)  Phase    Magnitude, kV (pu)    Angle");
                System::WriteLn(F);


                // SOURCES first
                pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Sources.Get_First();
                while (pElem != NULL)
                {
                    if (pElem->Get_Enabled())
                        WriteElementVoltages(F, pElem, LL);
                    System::WriteLn(F);
                    pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Sources.Get_Next();
                }

                // PDELEMENTS first
                pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_First();
                while (pElem != NULL)
                {
                    if (pElem->Get_Enabled())
                        WriteElementVoltages(F, pElem, LL);
                    System::WriteLn(F);
                    pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_Next();
                }
                System::WriteLn(F, "= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =");
                System::WriteLn(F);
                System::WriteLn(F, "Power Conversion Elements");
                System::WriteLn(F);
                System::Write(F, Pad("Bus", MaxBusNameLength)); System::WriteLn(F, " (node ref)  Phase    Magnitude, kV (pu)    Angle");
                System::WriteLn(F);

                // PCELEMENTS next
                pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PCElements.Get_First();
                while (pElem != NULL)
                {
                    if (pElem->Get_Enabled())
                        WriteElementVoltages(F, pElem, LL);
                    pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PCElements.Get_Next();
                    System::WriteLn(F);
                }
            }
            break; /*ShowOptionCode Case 2*/
            default: {}
                /*nada*/
            }
            //  }
            //  __finally
            //  {
            CloseFile(F);
            if (AutoDisplayShowReport)
                FireOffEditor(Filenm);
            ParserVars->Add("@lastshowfile", Filenm);
        }
        catch (...)
        {
            //
        }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void GetI0I1I2( double& I0, double& I1, double& I2, double& Cmax, int NPhases, int koffset, pComplexArray cBuffer )
    {
      double    cmag    = 0.0;
      int       i       = 0;
      complex   Iph[3]  = { cmplx(0,0), cmplx(0,0) ,cmplx(0,0)},
                I012[3] = { cmplx(0,0), cmplx(0,0) ,cmplx(0,0) };
      if ( NPhases >= 3 )
      {
        Cmax = 0.0;
        for ( int stop = 3, i = 0; i < stop; i++)
        {
          Iph[i] = (cBuffer)[koffset + i];
          cmag = cabs( Iph[i] );
          if ( cmag > Cmax )
            Cmax = cmag;
        }
        Phase2SymComp( & Iph[0], &I012[0]);
        I0 = cabs( I012[0] );
        I1 = cabs( I012[1] );
        I2 = cabs( I012[2] );
      }
      else
      {
        I0 = 0.0;
        I1 = cabs( (cBuffer)[1 + koffset - 1] );
        I2 = 0.0;
        Cmax = I1;
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteSeqCurrents( Textfile& F, const String PaddedBrName, double I0, double I1, double I2, double Cmax, double NormAmps, double EmergAmps, int j, int DSSObjType )
    {
      double iNormal = 0.0, iEmerg = 0.0, I2I1 = 0.0, I0I1 = 0.0;
      String Name;
      iNormal = 0.0;
      iEmerg = 0.0;
      if ( j == 1 )
        Name = PaddedBrName;
      else
        Name = Pad( "   -", PaddedBrName.size( ) );
      if ( I1 > 0.0 )
        I2I1 = 100.0 * I2 / I1;
      else
        I2I1 = 0.0;
      if ( I1 > 0.0 )
        I0I1 = 100.0 * I0 / I1;
      else
        I0I1 = 0.0;
      if ( ( ( ( CLASSMASK & DSSObjType ) ) != CAP_ELEMENT ) && ( j == 1 ) )
      {    // only write overloads for non-capacitors and terminal 1
        if ( NormAmps > 0.0 )
          iNormal = Cmax / NormAmps * 100.0;
        if ( EmergAmps > 0.0 )
          iEmerg = Cmax / EmergAmps * 100.0;
      }

      System::WriteLn( F, Format( "%s %3d  %10.5g   %10.5g %8.2f  %10.5g %8.2f  %8.2f %8.2f", UpperCase(Name).c_str(), j, I1, I2, I2I1, I0, I0I1, iNormal, iEmerg));
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteTerminalCurrents( Textfile& F, TDSSCktElement* pElem, bool ShowResidual )
    {
      int j = 0, i = 0, k = 0, Ncond = 0, Nterm = 0;
      pComplexArray cBuffer;
      String FromBus;
      complex Ctotal;
      polar ResidPolar;
      int Ntimes = 0;
      //cBuffer = NULL;
      Ncond = pElem->Get_NConds();
      Nterm = pElem->Get_NTerms();
      try
      {
        cBuffer = (pComplexArray) malloc( sizeof(complex) * Ncond * Nterm );
        pElem->GetCurrents( cBuffer, ActiveActor );
        k = 0;
        FromBus = Pad( StripExtension( pElem->Get_FirstBus() ), MaxBusNameLength );
        System::Write( F, "ELEMENT = " ); System::WriteLn( F, FullName( pElem ) );
        for ( int stop = Nterm, j = 1; j <= stop; j++)
        {
          Ctotal = CZero;
          if ( ( ( CLASSMASK & pElem->DSSObjType ) ) == AUTOTRANS_ELEMENT )
            Ntimes = pElem->Get_NPhases();    // Special case for AutoTrans
          else
            Ntimes = Ncond;
          for ( int stop = Ntimes, i = 1; i <= stop; i++)
          {
            k++;
            if ( ShowResidual )
              caccum( Ctotal, (cBuffer)[k-1] );
            System::WriteLn( F, Format( "%s  %4d    %13.5g /_ %6.1f =  %9.5g +j %9.5g",  UpperCase(FromBus).c_str(), GetNodeNum((pElem->NodeRef)[k - 1]), cabs((cBuffer)[k - 1]), cdang((cBuffer)[k - 1]), (cBuffer)[k - 1].re, (cBuffer)[k - 1].im));
          }
          if ( ShowResidual && ( pElem->Get_NPhases() > 1 ) )
          {
            ResidPolar = ctopolardeg( cnegate( Ctotal ) );
            System::WriteLn( F, Format( "%s Resid    %13.5g /_ %6.1f =   %9.5g +j %9.5g",  UpperCase( FromBus).c_str(), ResidPolar.mag, ResidPolar.ang, -Ctotal.re, -Ctotal.im));
          }
          if ( j < Nterm )
          {
              System::WriteLn( F, "------------" );
          }
          FromBus = Pad( StripExtension( pElem->Get_NextBus() ), MaxBusNameLength );
          if ( ( ( CLASSMASK & pElem->DSSObjType ) ) == AUTOTRANS_ELEMENT )
            k += Ntimes;  // Special case for AutoTrans
        }
        System::WriteLn( F );
    //  }
    //  __finally
    //  {
        if (( cBuffer != NULL ) )
          free( cBuffer );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowCurrents( String Filenm, bool ShowResidual, int ShowOptionCode )
    {
      TTextRec F;
      vector<complex>   cBuffer;
      int               Ncond = 0, 
                        Nterm = 0, 
                        j = 0;
      TDSSCktElement*   pElem = nullptr;
      TPDElement*       PDElem = nullptr;
      TPCElement*       PCelem = nullptr;
      double            I0 = 0.0, 
                        I1 = 0.0, 
                        I2 = 0.0, 
                        Cmax = 0.0;
      SetMaxDeviceNameLength();
      SetMaxBusNameLength();
      try
      {
        try
        {
          AssignFile( F, Filenm );
          Rewrite( F );
          IOResultToException();
          switch ( ShowOptionCode )
          {
            case 0:
            {  /*Sequence Currents*/
              System::WriteLn( F );
              System::WriteLn( F, "SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)" );
              System::WriteLn( F );
              System::Write( F, Pad( "Element", MaxDeviceNameLength + 2 ) ); System::WriteLn( F, " Term      I1         I2         %I2/I1    I0         %I0/I1   %Normal %Emergency" );
              System::WriteLn( F );


    //Sources Get_First()
              pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
              while ( pElem != NULL )
              {
                if ( pElem->Get_Enabled() )
                {
                  Ncond = pElem->Get_NConds();
                  Nterm = pElem->Get_NTerms();
                  cBuffer.resize( Ncond * Nterm + 1);
                  pElem->GetCurrents( &cBuffer[0], ActiveActor);
                  for ( int stop = Nterm, j = 1; j <= stop; j++)
                  {
                    GetI0I1I2( I0, I1, I2, Cmax, pElem->Get_NPhases(), ( j - 1 ) * Ncond, &cBuffer[0]);
                    /*# with pElem do */
                    WriteSeqCurrents( F, Paddots( FullName( pElem ), MaxDeviceNameLength + 2 ), I0, I1, I2, Cmax, 0.0, 0.0, j, pElem->DSSObjType );
                  }
                  cBuffer.resize(0);
                }
                pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
              }


         // PDELEMENTS Get_Next()
              PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
              while ( PDElem != NULL )
              {
                if (((TDSSCktElement*)PDElem)->Get_Enabled() )
                {
                  Ncond = ((TDSSCktElement*)PDElem)->Get_NConds();
                  Nterm = ((TDSSCktElement*)PDElem)->Get_NTerms();
                  cBuffer.resize(Ncond * Nterm + 1);
                  PDElem->GetCurrents( &cBuffer[0], ActiveActor);
                  for ( int stop = Nterm, j = 1; j <= stop; j++)
                  {
                    GetI0I1I2( I0, I1, I2, Cmax, ((TDSSCktElement*)PDElem)->Get_NPhases(), ( j - 1 ) * Ncond, &cBuffer[0]);
                    /*# with PDElem do */
                    WriteSeqCurrents( F, Paddots( FullName( PDElem ), MaxDeviceNameLength + 2 ), I0, I1, I2, Cmax, PDElem->NormAmps, PDElem->EmergAmps, j, ((TDSSCktElement*)PDElem)->DSSObjType );
                  } /*For*/
                  cBuffer.resize(0);
                }
                PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
              }

        // PCelemENTS next
              PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
              while ( PCelem != NULL )
              {
                if (((TDSSCktElement*)PCelem)->Get_Enabled() )
                {
                  Ncond = ((TDSSCktElement*)PCelem)->Get_NConds();
                  Nterm = ((TDSSCktElement*)PCelem)->Get_NTerms();
                  cBuffer.resize(Ncond * Nterm + 1);
                  PCelem->GetCurrents( &cBuffer[0], ActiveActor);
                  for ( int stop = Nterm, j = 1; j <= stop; j++)
                  {
                    GetI0I1I2( I0, I1, I2, Cmax, ((TDSSCktElement*)PCelem)->Get_NPhases(), ( j - 1 ) * Ncond, &cBuffer[0]);
                    /*# with PCelem do */
                    WriteSeqCurrents( F, Paddots( FullName( PCelem ), MaxDeviceNameLength + 2 ), I0, I1, I2, Cmax, 0.0, 0.0, j, ((TDSSCktElement*)PCelem)->DSSObjType );
                  }
                  cBuffer.resize(0);
                }
                PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
              }


         //Faults next
              pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
              while ( pElem != NULL )
              {
                if ( pElem->Get_Enabled() )
                {
                  Ncond = pElem->Get_NConds();
                  Nterm = pElem->Get_NTerms();
                  cBuffer.resize(Ncond * Nterm + 1);
                  pElem->GetCurrents( &cBuffer[0], ActiveActor);
                  for ( int stop = Nterm, j = 1; j <= stop; j++)
                  {
                    GetI0I1I2( I0, I1, I2, Cmax, pElem->Get_NPhases(), ( j - 1 ) * Ncond, &cBuffer[0]);
                    /*# with pElem do */
                    WriteSeqCurrents( F, Paddots( FullName( pElem ), MaxDeviceNameLength + 2 ), I0, I1, I2, Cmax, 0.0, 0.0, j, pElem->DSSObjType );
                  }
                  cBuffer.resize(0);
                }
                pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
              }
            }
            break; /*Code 0:*/
            case 1:
            {  /*Element branch Currents*/
              System::WriteLn( F );
              System::WriteLn( F, "CIRCUIT ELEMENT CURRENTS" );
              System::WriteLn( F );
              System::WriteLn( F, "(Currents into element from indicated bus)" );
              System::WriteLn( F );
              System::WriteLn( F, "Power Delivery Elements" );
              System::WriteLn( F );
              System::Write( F, Pad( "  Bus", MaxBusNameLength ) ); System::WriteLn( F, " Phase    Magnitude, A     Angle      (Real)   +j  (Imag)" );
              System::WriteLn( F );



         // Sources first
              pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
              while ( pElem != NULL )
              {
                if ( pElem->Get_Enabled() )
                  WriteTerminalCurrents( F, pElem, false );
                pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
              }

         // PDELEMENTS first
              pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
              while ( pElem != NULL )
              {
                if ( pElem->Get_Enabled() )
                  WriteTerminalCurrents( F, pElem, ShowResidual );
                pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
              }

         // Faults
              pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
              while ( pElem != NULL )
              {
                if ( pElem->Get_Enabled() )
                  WriteTerminalCurrents( F, pElem, false );
                pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
              }
              System::WriteLn( F, "= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =" );
              System::WriteLn( F );
              System::WriteLn( F, "Power Conversion Elements" );
              System::WriteLn( F );
              System::Write( F, Pad( "  Bus", MaxBusNameLength ) ); System::WriteLn( F, " Phase    Magnitude, A     Angle      (Real)   +j  (Imag)" );
              System::WriteLn( F );

         // PCELEMENTS next
              pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
              while ( pElem != NULL )
              {
                if ( pElem->Get_Enabled() )
                  WriteTerminalCurrents( F, pElem, false );
                pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
              }
            }
            break;  /*code:1*/
            default: {}
          } /*CASE*/
    //    }
    //    __finally
    //    {
          CloseFile( F );
          if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
          ParserVars->Add( "@lastshowfile", Filenm );
        }
        catch (...)
        {
            //
        }
      }
      catch( std::exception & E )
      {
        DoSimpleMsg( "Exception raised in ShowCurrents: " + (std::string) E.what(), 2190 );
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowPowers( String Filenm, int Opt, int ShowOptionCode )

    /*Opt = 0: kVA
     opt = 1: MVA
     */
    {
      String                FromBus = "";
      TTextRec              F = {};
      std::vector <complex> c_Buffer;
      int                   Ncond   = 0, 
                            Nterm   = 0, 
                            i       = 0, 
                            j       = 0, 
                            k       = 0,
                            Nref    = 0, 
                            Ntimes  = 0;
      TDSSCktElement*       p_Elem  = nullptr;
      TPDElement*           PDElem  = nullptr;
      TPCElement*           PCelem  = nullptr;
      complex               Volts   = cmplx(0,0),
                            S       = cmplx(0,0), 
                            Saccum  = cmplx(0,0);
      complex               Vph[4]  = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) }, 
                            V012[4] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) },
                            Iph[4]  = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) }, 
                            I012[4] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };

      c_Buffer.resize(0);
      SetMaxDeviceNameLength();
      SetMaxBusNameLength();
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

          /*Allocate c_Buffer big enough for largest circuit element*/
        c_Buffer.resize(GetMaxCktElementSize() + 1);
        switch ( ShowOptionCode )
        {
          case 0:
          {
         /*Sequence Currents*/
            System::WriteLn( F );
            System::WriteLn( F, "SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)                                     Excess Power" );
            System::WriteLn( F );
            switch ( Opt )
            {
              case 1:
              {
                System::Write( F, Pad( "Element", MaxDeviceNameLength + 2 ) ); System::WriteLn( F, " Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg" );
              }
              break;
            default:
            {
              System::Write( F, Pad( "Element", MaxDeviceNameLength + 2 ) ); System::WriteLn( F, " Term    P1(kW)   Q1(kvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg" );
            }
            }
            System::WriteLn( F );

         // Sources first
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
              {
                Ncond = p_Elem->Get_NConds();
                Nterm = p_Elem->Get_NTerms();
                p_Elem->GetCurrents( &( c_Buffer[0] ), ActiveActor);
                for ( int stop = Nterm, j = 1; j <= stop; j++)
                {
                  System::Write( F, Pad( FullName( p_Elem ), MaxDeviceNameLength + 2 ) ); System::Write( F, j, 3 );
                  int stop1 = 3;
                  if (p_Elem->Get_NPhases() <= 3)
                      stop1 = p_Elem->Get_NPhases();
                  for ( i = 1; i <= stop1; i++)
                  {
                    k = ( j - 1 ) * Ncond + i;
                    Nref = (p_Elem->NodeRef)[k - 1];
                    Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                    Iph[i] = (c_Buffer)[k - 1];
                    Vph[i] = Volts;
                  }
                  if ( p_Elem->Get_NPhases() >= 3 )
                  {
                    Phase2SymComp( &(Iph[1]), &(I012[1]));
                    Phase2SymComp( &(Vph[1]), &(V012[1]) );
                  }
                  else
                  {      // Handle single phase and pos seq models
                    V012[1] = CZero;
                    I012[1] = CZero;
                    V012[3] = CZero;
                    I012[3] = CZero;
                    if ( ActiveCircuit[ActiveActor]->PositiveSequence )
                    {
                      V012[2] = Vph[1];
                      I012[2] = Iph[1];
                    }
                    else
                    {
                      V012[2] = CZero;
                      I012[2] = CZero;
                    }
                  }
                  S = cmul( V012[2], conjg( I012[2] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%11.1f", S.re * 0.003));
                  System::Write( F, Format("%11.1f", S.im * 0.003));
                  S = cmul( V012[3], conjg( I012[3] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%11.1f", S.re * 0.003));
                  System::Write( F, Format("%11.1f", S.im * 0.003));
                  S = cmul( V012[1], conjg( I012[1] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%8.1f", S.re * 0.003));
                  System::Write( F, Format("%8.1f", S.im * 0.003));
                  System::WriteLn( F );
                }
              }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
            }


         // PDELEMENTS next
            PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
            while ( PDElem != NULL )
            {
              if (PDElem->Get_Enabled() )
              {
                Ncond = PDElem->Get_NConds();
                Nterm = PDElem->Get_NTerms();
                PDElem->GetCurrents(&(c_Buffer[0]), ActiveActor );
                for ( int stop = Nterm, j = 1; j <= stop; j++)
                {
                  System::Write( F, Pad( FullName( PDElem ), MaxDeviceNameLength + 2 ) ); System::Write( F, j, 3 );
                  for ( int stop = min<int64_t>( 3, PDElem->Get_NPhases() ), i = 1; i <= stop; i++)
                  {
                    k = ( j - 1 ) * Ncond + i;
                    Nref = PDElem->NodeRef[k - 1];
                    Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                    Iph[i] = c_Buffer[k - 1];
                    Vph[i] = Volts;
                  }
                  if (PDElem->Get_NPhases() >= 3 )
                  {
                    Phase2SymComp( &(Iph[1]), &(I012[1]) );
                    Phase2SymComp( &(Vph[1]), &(V012[1]) );
                  }
                  else
                  {      // Handle single phase and pos seq models
                    V012[1] = CZero;
                    I012[1] = CZero;
                    V012[3] = CZero;
                    I012[3] = CZero;
                    if ( ActiveCircuit[ActiveActor]->PositiveSequence )
                    {
                      V012[2] = Vph[1];
                      I012[2] = Iph[1];
                    }
                    else
                    {
                      V012[2] = CZero;
                      I012[2] = CZero;
                    }
                  }
                  S = cmul( V012[2], conjg( I012[2] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%11.1f", S.re * 0.003));
                  System::Write( F, Format("%11.1f", S.im * 0.003));
                  S = cmul( V012[3], conjg( I012[3] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%11.1f", S.re * 0.003));
                  System::Write( F, Format("%11.1f", S.im * 0.003));
                  S = cmul( V012[1], conjg( I012[1] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%8.1f", S.re * 0.003));
                  System::Write( F, Format("%8.1f", S.im * 0.003));
                  if ( j == 1 )
                  {
                   //----PDelem.ActiveTerminalIdx := 1;
                    S = PDElem->Get_ExcessKVANorm(1, ActiveActor);
                    if ( Opt == 1 )
                      S = cmulreal( S, 0.001 );
                    System::Write( F, Format("%11.1f", S.re));
                    System::Write( F, Format("%11.1f", S.im));
                    S = PDElem->Get_ExcessKVAEmerg(1, ActiveActor);
                    if ( Opt == 1 )
                      S = cmulreal( S, 0.001 );
                    System::Write( F, Format("%11.1f", S.re));
                    System::Write( F, Format("%11.1f", S.im));
                  }
                  System::WriteLn( F );
                }
              }
              PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
            }

         // PCELEMENTS Get_Next()
            PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
            while ( PCelem != NULL )
            {
              if (PCelem->Get_Enabled() )
              {
                Ncond = PCelem->Get_NConds();
                Nterm = PCelem->Get_NTerms();
                PCelem->GetCurrents(&(c_Buffer[0]), ActiveActor );
                for ( int stop = Nterm, j = 1; j <= stop; j++)
                {
                  System::Write( F, Pad( FullName( PCelem ), MaxDeviceNameLength + 2 ) ); System::Write( F, j, 3 );
                  for ( int stop = min<int64_t>( 3, (PCelem)->Get_NPhases() ), i = 1; i <= stop; i++)
                  {
                    k = ( j - 1 ) * Ncond + i;
                    Nref = PCelem->NodeRef[k - 1];
                    Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                    Iph[i] = c_Buffer[k - 1];
                    Vph[i] = Volts;
                  }
                  if (((TDSSCktElement*)PCelem)->Get_NPhases() >= 3 )
                  {
                    Phase2SymComp(&(Iph[1]), &(I012[1]) );
                    Phase2SymComp(&(Vph[1]), &(V012[1]) );
                  }
                  else
                  {   // Handle single phase and pos seq models
                    V012[1] = CZero;
                    I012[1] = CZero;
                    V012[3] = CZero;
                    I012[3] = CZero;
                    if ( ActiveCircuit[ActiveActor]->PositiveSequence )
                    {
                      V012[2] = Vph[1];
                      I012[2] = Iph[1];
                    }
                    else
                    {
                      V012[2] = CZero;
                      I012[2] = CZero;
                    }
                  }
                  S = cmul( V012[2], conjg( I012[2] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%11.1f", S.re * 0.003));
                  System::Write( F, Format("%11.1f", S.im * 0.003));
                  S = cmul( V012[3], conjg( I012[3] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%11.1f", S.re * 0.003));
                  System::Write( F, Format("%11.1f", S.im * 0.003));
                  S = cmul( V012[1], conjg( I012[1] ) );
                  if ( Opt == 1 )
                    S = cmulreal( S, 0.001 );
                  System::Write( F, Format("%8.1f", S.re * 0.003));
                  System::Write( F, Format("%8.1f", S.im * 0.003));
                  System::WriteLn( F );
                }
              }
              PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
            }
          }
          break; /*ShowOptionCode=0*/
          case 1:
          {

         /*Branch Powers*/
            System::WriteLn( F );
            System::WriteLn( F, "CIRCUIT ELEMENT POWER FLOW" );
            System::WriteLn( F );
            System::WriteLn( F, "(Power Flow into element from indicated Bus)" );
            System::WriteLn( F );
            System::WriteLn( F, "Power Delivery Elements" );
            System::WriteLn( F );
            switch ( Opt )
            {
              case 1:
              {
                System::Write( F, Pad( "  Bus", MaxBusNameLength ) ); System::WriteLn( F, " Phase     MW     +j   Mvar         MVA         PF" );
              }
              break;
            default:
            {
              System::Write( F, Pad( "  Bus", MaxBusNameLength ) ); System::WriteLn( F, " Phase     kW     +j   kvar         kVA         PF" );
            }
            }
            System::WriteLn( F );

            // Sources first
            p_Elem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Sources.Get_First();
            while (p_Elem != NULL)
            {
                if (p_Elem->Get_Enabled())
                {
                    Ncond = p_Elem->Get_NConds();
                    Nterm = p_Elem->Get_NTerms();
                    p_Elem->GetCurrents(&(c_Buffer[0]), ActiveActor);
                    k = 0;
                    FromBus = Pad(StripExtension(p_Elem->Get_FirstBus()), MaxBusNameLength);
                    System::Write(F, "ELEMENT = "); System::WriteLn(F, FullName(p_Elem));

                    for (int stop = Nterm, j = 1; j <= stop; j++)
                    {
                        Saccum = CZero;
                        for (int stop = Ncond, i = 1; i <= stop; i++)
                        {
                            ++k;
                            Nref = p_Elem->NodeRef[k - 1];
                            Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                            S = cmul(Volts, conjg(c_Buffer[k - 1]));
                            if /* (p_Elem.nphases=1) and */ (ActiveCircuit[ActiveActor]->PositiveSequence)
                                S = cmulreal(S, 3.0);
                            if (Opt == 1)
                                S = cmulreal(S, 0.001);
                            caccum(Saccum, S);
                            System::Write(F, UpperCase(FromBus)); 
                            System::Write(F, "  "); 
                            System::Write(F, GetNodeNum(p_Elem->NodeRef[k - 1]), 4); 
                            System::Write(F, "    "); 
                            System::Write(F, Format("%8.1f", S.re / 1000.0));
                            System::Write(F, " +j "); 
                            System::Write(F, Format("%8.1f", S.im / 1000.0));
                            System::Write(F, "   "); 
                            System::Write(F, Format("%8.1f", cabs(S) / 1000.0));
                            System::Write(F, "     "); 
                            System::WriteLn(F, Format("%8.4f", PowerFactor(S)));
                        }
                        System::Write(F, Paddots("   TERMINAL TOTAL", MaxBusNameLength + 10)); 
                        System::Write(F, Format("%8.1f", Saccum.re / 1000.0));
                        System::Write(F, " +j "); 
                        System::Write(F, Format("%8.1f", Saccum.im / 1000.0));
                        System::Write(F, "   "); 
                        System::Write(F, Format("%8.1f", cabs(Saccum) / 1000.0));
                        System::Write(F, "     "); 
                        System::WriteLn(F, Format("%8.4f", PowerFactor(Saccum)));
                        FromBus = Pad(StripExtension(p_Elem->Get_NextBus()), MaxBusNameLength);
                    }
                    System::WriteLn(F);
                }
                p_Elem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Sources.Get_Next();
            }

            // PDELEMENTS first
            p_Elem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_First();
            int Ntimes = 0;
            while (p_Elem != NULL)
            {
                if (p_Elem->Get_Enabled())
                {
                    Ncond = p_Elem->Get_NConds();
                    Nterm = p_Elem->Get_NTerms();
                    p_Elem->GetCurrents(&(c_Buffer[0]), ActiveActor);
                    k = 0;
                    FromBus = Pad(StripExtension(p_Elem->Get_FirstBus()), MaxBusNameLength);
                    System::Write(F, "ELEMENT = "); System::WriteLn(F, FullName(p_Elem));

                    if ((CLASSMASK & p_Elem->DSSObjType) == AUTOTRANS_ELEMENT)           Ntimes = p_Elem->Get_NPhases();
                    else                                                                 Ntimes = Ncond;

                    for (int stop = Nterm, j = 1; j <= stop; j++)
                    {
                        Saccum = CZero;
                        for (int stop = Ntimes, i = 1; i <= stop; i++)
                        {
                            ++k;
                            Nref = p_Elem->NodeRef[k - 1];
                            Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                            S = cmul(Volts, conjg(c_Buffer[k - 1]));
                            if /* (p_Elem.nphases=1) and */ (ActiveCircuit[ActiveActor]->PositiveSequence)
                                S = cmulreal(S, 3.0);
                            if (Opt == 1)
                                S = cmulreal(S, 0.001);
                            caccum(Saccum, S);
                            System::Write(F, UpperCase(FromBus));
                            System::Write(F, "  ");
                            System::Write(F, GetNodeNum(p_Elem->NodeRef[k - 1]), 4);
                            System::Write(F, "    ");
                            System::Write(F, Format("%8.1f", S.re / 1000.0));
                            System::Write(F, " +j ");
                            System::Write(F, Format("%8.1f", S.im / 1000.0));
                            System::Write(F, "   ");
                            System::Write(F, Format("%8.1f", cabs(S) / 1000.0));
                            System::Write(F, "     ");
                            System::WriteLn(F, Format("%8.4f", PowerFactor(S)));
                        }
                        System::Write(F, Paddots("   TERMINAL TOTAL", MaxBusNameLength + 10));
                        System::Write(F, Format("%8.1f", Saccum.re / 1000.0));
                        System::Write(F, " +j ");
                        System::Write(F, Format("%8.1f", Saccum.im / 1000.0));
                        System::Write(F, "   ");
                        System::Write(F, Format("%8.1f", cabs(Saccum) / 1000.0));
                        System::Write(F, "     ");
                        System::WriteLn(F, Format("%8.4f", PowerFactor(Saccum)));
                        FromBus = Pad(StripExtension(p_Elem->Get_NextBus()), MaxBusNameLength);

                        if ((CLASSMASK & p_Elem->DSSObjType) == AUTOTRANS_ELEMENT)           k += Ntimes;
                    }
                    System::WriteLn(F);
                }
                p_Elem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_Next();
            }

            System::WriteLn(F, "= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =");
            System::WriteLn(F);
            System::WriteLn(F, "Power Conversion Elements");
            System::WriteLn(F);
            switch (Opt)
            {
                case 1:  
                    System::WriteLn(F, Pad("  Bus", MaxBusNameLength) + " Phase     MW   +j  Mvar         MVA         PF");
                    break;
                default:
                    System::WriteLn(F, Pad("  Bus", MaxBusNameLength) + " Phase     kW   +j  kvar         kVA         PF");
            }
            System::WriteLn(F);

         // PCELEMENTS next
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
              {
                Ncond = p_Elem->Get_NConds();
                Nterm = p_Elem->Get_NTerms();
                p_Elem->GetCurrents(&(c_Buffer[0]), ActiveActor );
                k = 0;
                FromBus = Pad( StripExtension( p_Elem->Get_FirstBus() ), MaxBusNameLength );
                System::Write( F, "ELEMENT = " ); System::WriteLn( F, FullName( p_Elem ) );
                for ( int stop = Nterm, j = 1; j <= stop; j++)
                {
                  Saccum = CZero;
                  for ( int stop = Ncond, i = 1; i <= stop; i++)
                  {
                    k++;
                    Nref = p_Elem->NodeRef[k - 1];
                    Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
                    S = cmul( Volts, conjg( c_Buffer[k - 1] ) );
                    if /* (p_Elem.nphases=1) and */ ( ActiveCircuit[ActiveActor]->PositiveSequence )
                      S = cmulreal( S, 3.0 );
                    if ( Opt == 1 )
                      S = cmulreal( S, 0.001 );
                    caccum( Saccum, S );
                    System::Write( F, UpperCase( FromBus ) ); 
                    System::Write( F, "  " ); 
                    System::Write( F, GetNodeNum( p_Elem->NodeRef[k - 1] ), 4 );
                    System::Write( F, "    " ); 
                    System::Write( F, Format("%6.1f", S.re / 1000.0));
                    System::Write( F, " +j " ); 
                    System::Write( F, Format("%6.1f", S.im / 1000.0));
                    System::Write( F, "   " ); 
                    System::Write( F, Format("%8.1f", cabs(S) / 1000.0));
                    System::Write( F, "     " ); 
                    System::WriteLn( F, Format("%8.4f", PowerFactor(S)) );
                  }
                  System::Write( F, Paddots( "  TERMINAL TOTAL ", MaxBusNameLength + 10 ) ); 
                  System::Write( F, Format("%8.1f", Saccum.re / 1000.0 ) );
                  System::Write( F, " +j " ); 
                  System::Write( F, Format("%8.1f", Saccum.im / 1000.0) );
                  System::Write( F, "   " ); 
                  System::Write( F, Format("%8.1f", cabs(Saccum) / 1000.0) );
                  System::Write( F, "     " ); 
                  System::WriteLn( F, Format("%8.4f", PowerFactor( Saccum )));
                  FromBus = Pad( StripExtension( p_Elem->Get_NextBus() ), MaxBusNameLength );
                }
                System::WriteLn( F );
              }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
            }
          }
          break; /*ShowOptionCode=1*/
          default: {}
        } /*CASE*/
        System::WriteLn( F );
        S = cmulreal( ActiveCircuit[ActiveActor]->Get_Losses(ActiveActor), 0.001 );
        if ( Opt == 1 )
          S = cmulreal( S, 0.001 );
        System::Write( F, "Total Circuit Losses = " ); System::Write( F, Format("%6.1f",S.re)); System::Write(F, " +j "); System::WriteLn(F, Format("%6.1f", S.im));
    //  }
    //  __finally
    //  {
        if (!c_Buffer.empty())
          c_Buffer.clear();
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    bool CheckBusReference( TDSSCktElement* cktElem, int BusReference, int& TerminalIndex )

    /*Check all terminals of cktelement to see if bus connected to busreference*/
    {
      bool result = false;
      int i = 0;
      result = false;
      /*# with cktElem do */
      auto with0 = cktElem;
      for ( int stop = with0->Get_NTerms(), i = 1; i <= stop; i++)
      {
        if ( with0->Terminals[i-1].BusRef == BusReference )
        {
          TerminalIndex = i;
          result = true;
          break;
        }
      }
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void WriteTerminalPowerSeq( Textfile& F, TDSSCktElement* cktElem, int j, int Opt )
    {
      int       i       = 0, 
                k       = 0, 
                Ncond   = 0, 
                Nref    = 0;
      complex   Volts   = cmplx(0,0), 
                S       = cmplx(0,0);
      complex   Vph[4]  = { cmplx(0,0), cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) }, 
                V012[4] = { cmplx(0,0), cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) };
      complex   Iph[ 4] = { cmplx(0,0), cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) },
                I012[4] = { cmplx(0,0), cmplx(0,0) ,cmplx(0,0) ,cmplx(0,0) };
      vector<complex> c_Buffer;  // Allocate to max total conductors

      try
      {
     /*Allocate c_Buffer big enough for this circuit element*/
        c_Buffer.resize(cktElem->Yorder + 1);
        Ncond = cktElem->Get_NConds();
        cktElem->GetCurrents( &c_Buffer[0], ActiveActor);
        System::Write( F, Pad( FullName( cktElem ), MaxDeviceNameLength + 2 ) ); System::Write(F, j, 3 );
        for ( int stop = min<int>( cktElem->Get_NPhases(), 3 ), i = 1; i <= stop; i++)
        {
          k = (j - 1) * Ncond + i;
          Nref = cktElem->NodeRef[k - 1];
          Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
          Iph[i-1] = c_Buffer[k-1];
          Vph[i-1] = Volts;
        }
        if ( cktElem->Get_NPhases() >= 3 )
        {
          Phase2SymComp( &(Iph[0]), &(I012[0]) );
          Phase2SymComp( &(Vph[0]), &(V012[0]) );        }
        else
        {      // Handle single phase and pos seq models
          V012[0] = CZero;
          I012[0] = CZero;
          V012[2] = CZero;
          I012[2] = CZero;
          if ( ActiveCircuit[ActiveActor]->PositiveSequence )
          {
            V012[1] = Vph[0];
            I012[1] = Iph[0];
          }
          else
          {
            V012[1] = CZero;
            I012[1] = CZero;
          }
        }


      // Pos Seq or Single Phase
        switch ( cktElem->Get_NPhases() )
        {
          case 1:
            S = cmul( Vph[1], conjg( Iph[1] ) );
          break;
          case 2:
            S = cadd( cmul( Vph[1], conjg( Iph[1] ) ), cmul( Vph[2], conjg( Iph[3] ) ) );
          break;
        default:
          S = cmul( V012[1], conjg( I012[1] ) );
        }
        if ( Opt == 1 )
          S = cmulreal( S, 0.001 );
        System::Write(F, Format("%11.1f",S.re * 0.003));
        System::Write(F, Format("%11.1f", S.im * 0.003));
        S = cmul( V012[2], conjg( I012[2] ) );
        if ( Opt == 1 )
          S = cmulreal( S, 0.001 );
        System::Write(F, Format("%11.1f", S.re * 0.003));
        System::Write(F, Format("%11.1f", S.im * 0.003));
        S = cmul( V012[0], conjg( I012[0] ) );
        if ( Opt == 1 )
          S = cmulreal( S, 0.001 );
        System::Write(F, Format("%11.1f", S.re * 0.003));
        System::Write(F, Format("%11.1f", S.im * 0.003));
        System::WriteLn( F );
    //  }
    //  __finally
    //  {
      c_Buffer.resize(0);
      }
      catch (...)
      {
          //
      }
    }

    void WriteTerminalPower( Textfile& F, TDSSCktElement* cktElem, int jTerm, int Opt )
    {
      int i = 0, k = 0, Ncond = 0, Nref = 0;
      complex Volts, S;
      complex Saccum;
      pComplexArray c_Buffer;  // Allocate to max total conductors

      String FromBus;
      c_Buffer = NULL;
      try
      {
        c_Buffer = (pComplexArray) malloc(sizeof(complex) * cktElem->Yorder);
        Ncond = cktElem->Get_NConds();
        cktElem->GetCurrents( c_Buffer, ActiveActor );
        FromBus = Pad( StripExtension( cktElem->GetBus( jTerm ) ), 12 );
        System::Write(F, "ELEMENT = " ); System::WriteLn(F, Pad( FullName( cktElem ), MaxDeviceNameLength + 2 ) );
        Saccum = CZero;
        for ( int stop = Ncond, i = 1; i <= stop; i++)
        {
          k = ( jTerm - 1 ) * Ncond + i;
          Nref = (cktElem->NodeRef)[k - 1];
          Volts = ActiveCircuit[ActiveActor]->Solution->NodeV[Nref];
          S = cmul( Volts, conjg( (c_Buffer)[k - 1] ) );
          if /* (CktElem.nphases=1) and */ ( ActiveCircuit[ActiveActor]->PositiveSequence )
            S = cmulreal( S, 3.0 );
          if ( Opt == 1 )
            S = cmulreal( S, 0.001 );
          caccum( Saccum, S );
          System::WriteLn(F, Format( "%s %4d %10.5g +j %10.5g    %10.5g    %8.4f",  UpperCase( FromBus ).c_str(), GetNodeNum((cktElem->NodeRef)[k - 1]), double(S.re) / 1000.0, double(S.im) / 1000.0, double(cabs(S)) / 1000.0, PowerFactor(S)));
        }
        System::WriteLn(F, Format( "    TERMINAL TOTAL  %10.5g +j %10.5g    %10.5g    %8.4f",  double( Saccum.re ) / 1000.0, double( Saccum.im ) / 1000.0, double( cabs( Saccum ) ) / 1000.0, PowerFactor( Saccum ) ));
    //  }
    //  __finally
    //  {
        if (( c_Buffer != NULL ) )
          free( c_Buffer );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowBusPowers( String Filenm, String BusName, int Opt, int ShowOptionCode )

    /*Report power flow around a specified Bus*/

    /*Opt = 0: kVA
     opt = 1: MVA
     */
    {
      TTextRec F;
      int               j = 0, 
                        Ncond = 0, 
                        Nterm = 0,
                        BusReference = 0,
                        jTerm = 0;
      TDSSCktElement*   p_Elem = nullptr;
      TPDElement*       PDElem = nullptr;
      TPCElement*       PCelem = nullptr;
      double            I0 = 0.0, 
                        I1 = 0.0, 
                        I2 = 0.0, 
                        Cmax = 0.0;
      vector <complex>  c_Buffer;  // Allocate to max total conductors


      SetMaxDeviceNameLength();


      /*Get Bus Reference*/
      BusReference = ActiveCircuit[ActiveActor]->BusList.Find( BusName );
      if ( BusReference == 0 )
      {
        DoSimpleMsg( "Bus \"" + UpperCase( BusName ) + "\" not found.", 219 );
        return;
      }
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

          /*Allocate c_Buffer big enough for largest circuit element*/
        c_Buffer.resize(GetMaxCktElementSize());
        switch ( ShowOptionCode )
        {
          case 0:
          {

         /*System::Write Bus Voltage*/
            System::WriteLn( F );
            System::WriteLn( F, "Bus      V1 (kV)    p.u.    V2 (kV)      %V2/V1    V0 (kV)  %V0/V1" );
            System::WriteLn( F );
            WriteSeqVoltages( F, BusReference, false );
     
         /*Sequence Currents*/
            System::WriteLn( F );
            System::WriteLn( F, "SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)" );
            System::WriteLn( F );
            System::WriteLn( F, "Element                Term      I1         I2       %I2/I1       I0      %I0/I1   %Normal %Emergency" );
            System::WriteLn( F );

         // Sources first 
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, j ) )
                {

            /*Use j set by CheckBusReference*/
                  Ncond = p_Elem->Get_NConds();
                  Nterm = p_Elem->Get_NTerms();
                  p_Elem->GetCurrents( &c_Buffer[0], ActiveActor);
                  for ( int stop = Nterm, j = 1; j <= stop; j++)
                  {
                    GetI0I1I2( I0, I1, I2, Cmax, p_Elem->Get_NPhases(), ( j - 1 ) * Ncond, &c_Buffer[0]);
                    /*# with p_Elem do */
                    WriteSeqCurrents( F, Paddots( FullName( p_Elem ), MaxDeviceNameLength + 2 ), I0, I1, I2, Cmax, 0.0, 0.0, j, p_Elem->DSSObjType );
                  }
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
            }


         // PDELEMENTS next   
            PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
            while ( PDElem != NULL )
            {
              if ( PDElem->Get_Enabled() )
                if ( CheckBusReference( PDElem, BusReference, j ) )
                {  // Is this connected to the bus
            /*Use j set by CheckBusReference*/
                  Ncond = PDElem->Get_NConds();
                  Nterm = PDElem->Get_NTerms();
                  PDElem->GetCurrents(&c_Buffer[0], ActiveActor );
                  for ( int stop = Nterm, j = 1; j <= stop; j++)
                  {
                    GetI0I1I2( I0, I1, I2, Cmax, PDElem->Get_NPhases(), ( j - 1 ) * Ncond, &c_Buffer[0]);
                    /*# with PDElem do */
                    WriteSeqCurrents( F, Paddots( FullName( PDElem ), MaxDeviceNameLength + 2 ), I0, I1, I2, Cmax, 0.0, 0.0, j, PDElem->DSSObjType );
                  }
                }
              PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
            }

         // PCELEMENTS Get_Next()
            PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
            while ( PCelem != NULL )
            {
              if (PCelem->Get_Enabled() )
                if ( CheckBusReference(PCelem, BusReference, j ) )
                {
                  Ncond = PCelem->Get_NConds();
                  Nterm = PCelem->Get_NTerms();
                  PCelem->GetCurrents(&c_Buffer[0], ActiveActor );
                  for ( int stop = Nterm, j = 1; j <= stop; j++)
                  {
                    GetI0I1I2( I0, I1, I2, Cmax, PCelem->Get_NPhases(), ( j - 1 ) * Ncond, &c_Buffer[0]);
                    /*# with PCelem do */
                    WriteSeqCurrents( F, Paddots( FullName( PCelem ), MaxDeviceNameLength + 2 ), I0, I1, I2, Cmax, 0.0, 0.0, j, PCelem->DSSObjType );
                  }
                }
              PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
            }

         /*Sequence Powers */
            System::WriteLn( F );
            System::WriteLn( F, "SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)" );
            System::WriteLn( F );
            switch ( Opt )
            {
              case 1:
                System::WriteLn( F, "Element                      Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0   " );
              break;
            default:
              System::WriteLn( F, "Element                      Term    P1(kW)   Q1(kvar)         P2         Q2      P0      Q0  " );
            }
            System::WriteLn( F );



         // Sources first ((TDSSCktElement*)PCelem)->
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, j ) )
                {
            /*Use j set by CheckBusReference*/
                  WriteTerminalPowerSeq( F, p_Elem, j, Opt );
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
            }


         // PDELEMENTS next   
            PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
            while ( PDElem != NULL )
            {
              if ( (PDElem)->Get_Enabled() )
                if ( CheckBusReference(PDElem, BusReference, j ) )
                {  // Is this connected to the bus
                  WriteTerminalPowerSeq( F, PDElem, j, Opt );
                }
              PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
            }

         // PCELEMENTS Get_Next()
            PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
            while ( PCelem != NULL )
            {
              if ( ((TDSSCktElement*)PCelem)->Get_Enabled() )
                if ( CheckBusReference(PCelem, BusReference, j ) )
                {
                  WriteTerminalPowerSeq( F, PCelem, j, Opt );
                }
              PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
            }
          }
          break; /*ShowOptionCode=0 */
          case 1:
          {

         /*System::Write Bus Voltage */
            System::WriteLn( F );
            System::WriteLn( F, "  Bus   (node ref)  Node       V (kV)    Angle    p.u.   Base kV" );
            System::WriteLn( F );
            WriteBusVoltages( F, BusReference, false );

         /*Element Currents  --10*/
            System::WriteLn( F );
            System::WriteLn( F, "CIRCUIT ELEMENT CURRENTS" );
            System::WriteLn( F );
            System::WriteLn( F, "(Currents into element from indicated bus)" );
            System::WriteLn( F );
            System::WriteLn( F, "Power Delivery Elements" );
            System::WriteLn( F );
            System::WriteLn( F, "  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)" );
            System::WriteLn( F );


              // Sources first   
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, j ) )
                {
                  WriteTerminalCurrents( F, p_Elem, false );
                  System::WriteLn( F );
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
            }


         // PDELEMENTS first   
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, j ) )
                {
                  WriteTerminalCurrents( F, p_Elem, true );
                  System::WriteLn( F );
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
            }
            System::WriteLn( F, "= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =" );
            System::WriteLn( F );
            System::WriteLn( F, "Power Conversion Elements" );
            System::WriteLn( F );
            System::WriteLn( F, "  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)" );
            System::WriteLn( F );

         // PCELEMENTS next  
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, j ) )
                {
                  WriteTerminalCurrents( F, p_Elem, false );
                  System::WriteLn( F );
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
            }

          // FAULTs next   
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, j ) )
                {
                  WriteTerminalCurrents( F, p_Elem, false );
                  System::WriteLn( F );
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Faults.Get_Next();
            }

         /*Branch Powers */
            System::WriteLn( F );
            System::WriteLn( F, "CIRCUIT ELEMENT POWER FLOW" );
            System::WriteLn( F );
            System::WriteLn( F, "(Power Flow into element from indicated Bus)" );
            System::WriteLn( F );
            switch ( Opt )
            {
              case 1:
                System::WriteLn( F, "  Bus       Phase     MW     +j   Mvar           MVA           PF" );
              break;
            default:
              System::WriteLn( F, "  Bus       Phase     kW     +j   kvar           kVA           PF" );
            }
            System::WriteLn( F );
         // Sources first   
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, j ) )
                {
                  WriteTerminalPower( F, p_Elem, j, Opt );
                  System::WriteLn( F );
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
            }


         // PDELEMENTS first 
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, jTerm ) )
                {
                  WriteTerminalPower( F, p_Elem, jTerm, Opt );

              /*Get the other buses for the report --18*/
                  for ( int stop = p_Elem->Get_NTerms(), j = 1; j <= stop; j++)
                    if ( j != jTerm )
                    {
                      System::WriteLn( F, "------------" );
                      WriteTerminalPower( F, p_Elem, j, Opt );
                    }
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
            }
            System::WriteLn( F, "= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =" );
            System::WriteLn( F );
            System::WriteLn( F, "Power Conversion Elements" );
            System::WriteLn( F );
            switch ( Opt )
            {
              case 1:
                System::WriteLn( F, "  Bus         Phase     MW   +j  Mvar         MVA         PF" );
              break;
            default:
              System::WriteLn( F, "  Bus         Phase     kW   +j  kvar         kVA         PF" );
            }
            System::WriteLn( F );

         // PCELEMENTS next   
            p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
            while ( p_Elem != NULL )
            {
              if ( p_Elem->Get_Enabled() )
                if ( CheckBusReference( p_Elem, BusReference, jTerm ) )
                {
                  WriteTerminalPower( F, p_Elem, jTerm, Opt );
                  System::WriteLn( F );
                }
              p_Elem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
            }
          }
          break; /*ShowOptionCode=1 */
          default: {}
        } /*CASE*/
    //  }
    //  __finally
    //  {
        c_Buffer.clear();
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowFaultStudy( String Filenm )
    {
        int         i = 0,
                    iBus = 0,
                    iphs = 0,
                    iphs2 = 0;
      TcMatrix      YFault, 
                    ZFault;
      vector <complex> Vfault;  /*Big temp array*/
      TTextRec      F = {};
      complex       GFault = CZero, 
                    IFault = CZero;
      double        Vphs = 0.0,
                    CurrMag = 0.0;
      String        S = "";
      SetMaxBusNameLength();
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

       /* Set source voltage injection currents */
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          auto with1 = ActiveCircuit[ActiveActor]->Solution;
          {
            /*# with Solution do */
            {

         /*All Phase Faults*/
              System::WriteLn( F, "FAULT STUDY REPORT" );
              System::WriteLn( F );
              System::WriteLn( F, "ALL-Node Fault Currents" );
              System::WriteLn( F );
              string Fheader = Pad("Bus", MaxBusNameLength) + "Node" + Pad(" ", 12) + "Amps" + Pad(" ", 3) + "X/R " + 
                  "Node" + Pad(" ", 12) + "Amps" + Pad(" ", 3) + "X/R " + "Node" + Pad(" ", 12) + "Amps" + Pad(" ", 3) + "X/R ...";
              System::WriteLn( F, Fheader);
              System::WriteLn( F );
              for ( int stop = with0->NumBuses, iBus = 1; iBus <= stop; iBus++)
               /*Bus Norton Equivalent Current, Isc has been previously computed*/
                /*# with Buses^[iBus] do */
                {
                  auto& with2 = with0->Buses[iBus - 1];
                  {
                    System::Write( F, Pad( EncloseQuotes( UpperCase(with0->BusList.Get( iBus ) ) ) + " ", MaxBusNameLength + 2));
                    for ( int stop = with2->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
                    {
                      CurrMag = cabs( with2->BusCurrent[i - 1] );
                      if ( i > 1 )
                        System::Write( F, "    " );
                      System::Write(F, with2->GetNum(i));
                      System::Write(F, " ");
                      System::Write( F, Format("%15.0f", CurrMag));
                      if ( CurrMag > 0.0 )
                      {
                        System::Write( F, " " ); System::Write( F, Format("%5.1f", GetXR(cdiv(with2->VBus[i - 1], with2->BusCurrent[i - 1]))));
                      }
                      else
                        System::Write( F, "   N/A" );
                    }
                    System::WriteLn( F );
                  }
                }
              System::WriteLn( F );

              // One Phase Faults
              System::WriteLn( F );
              System::WriteLn( F, "ONE-Node to ground Faults" );
              System::WriteLn( F );
              System::WriteLn( F, "                                      pu Node Voltages (L-N Volts if no base)" );
              System::Write( F, Pad( "Bus", MaxBusNameLength ) ); System::WriteLn( F, "   Node  Amps         Node 1     Node 2     Node 3    ..." );
              System::WriteLn( F );

            // Solve for Fault Injection Currents
                for ( int stop = with0->NumBuses, iBus = 1; iBus <= stop; iBus++)
            //Bus Norton Equivalent Current, Isc has been previously computed
            // # with Buses^[iBus] do
                {
                  auto& with2 = with0->Buses[iBus - 1];
                  {
                    ZFault = TcMatrix(with2->get_FNumNodesThisBus() );
                    ZFault.CopyFrom(&with2->Zsc );
                    for ( int stop = with2->get_FNumNodesThisBus(), iphs = 1; iphs <= stop; iphs++)
                    {
                      IFault = cdiv( (with2->VBus)[iphs - 1], with2->Zsc.GetElement( iphs, iphs ) );
                      S = Format( "%s %4u %12.0f ",  Pad( EncloseQuotes( UpperCase(with0->BusList.Get( iBus ) ) ).c_str(), MaxBusNameLength + 2).c_str(), with2->GetNum(iphs), cabs(IFault));
                      System::Write( F, S ); System::Write( F, "   " );
                      for ( int stop = with2->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
                      {
                        Vphs = cabs( csub( (with2->VBus)[i - 1], cmul(with2->Zsc.GetElement( i, iphs ), IFault ) ) );
                        if (with2->kVBase > 0.0 )
                        {
                          Vphs = 0.001 * Vphs / with2->kVBase;
                          System::Write( F, ' ' ); System::Write( F, Format("%10.3f", Vphs));
                        }
                        else
                        {
                          System::Write( F, ' ' ); System::Write( F, Format("%10.1f",Vphs));
                        }
                      }
                      System::WriteLn( F );
                    } // For iphase
                    // Now, Stuff it in the Css Array where it belongs
                  }
                }  // With bus

               // Node-Node Faults 
              System::WriteLn( F );
              System::WriteLn( F, "Adjacent Node-Node Faults" );
              System::WriteLn( F );
              System::WriteLn( F, "                                        pu Node Voltages (L-N Volts if no base)" );
              System::WriteLn( F, "Bus          Node-Node      Amps        Node 1     Node 2     Node 3    ..." );
              System::WriteLn( F );

       // Solve for Fault Injection Currents
              for ( int stop = with0->NumBuses, iBus = 1; iBus <= stop; iBus++)
               // Bus Norton Equivalent Current, Isc has been previously computed
               // # with Buses^[iBus] do 
                {
                  auto& with3 = with0->Buses[iBus - 1];
                  {
                    YFault = TcMatrix(with3->get_FNumNodesThisBus() );
                    Vfault.resize(with3->get_FNumNodesThisBus() + 1);
                    GFault = cmplx( 10000.0, 0.0 );

                    for ( int stop = with3->get_FNumNodesThisBus(), iphs = 1; iphs <= stop; iphs++)
                    {
                        for (iphs2 = 1; iphs2 <= with3->get_FNumNodesThisBus(); iphs2++)
                        {
                            if (iphs < iphs2)
                            {
                                YFault.CopyFrom(&(with3->Ysc));
                                YFault.AddElement(iphs, iphs, GFault);
                                YFault.AddElement(iphs2, iphs2, GFault);
                                YFault.AddElemsym(iphs, iphs2, cnegate(GFault));

                                //  Solve for Injection Currents
                                YFault.Invert();
                                YFault.MVmult(&Vfault[0], &(with3->BusCurrent[0]));   // Gets voltage appearing at fault
                                System::Write(F, Pad(EncloseQuotes(UpperCase(with0->BusList.Get(iBus))), MaxBusNameLength + 2));
                                System::Write(F, with3->GetNum(iphs), 4);
                                System::Write(F, with3->GetNum(iphs2), 4);
                                System::Write(F, Format("%12.0f", cabs(cmul(csub(Vfault[iphs - 1], Vfault[iphs2 - 1]), GFault))));
                                System::Write(F, "   ");
                                for (int stop = with3->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
                                {
                                    Vphs = cabs(Vfault[i - 1]);
                                    if (with3->kVBase > 0.0)
                                    {
                                        Vphs = 0.001 * Vphs / with3->kVBase;
                                        System::Write(F, ' '); System::Write(F, Format("%10.3f", Vphs));
                                    }
                                    else
                                    {
                                        System::Write(F, ' '); System::Write(F, Format("%10.1f", Vphs));
                                    }
                                }
                                System::WriteLn(F);
                            }
                        }
                    } //For iphase
                  // Now, Stuff it in the Css Array where it belongs
                    Vfault.clear();
                  }
                }  // With bus 
            } // With Solution
          }
        } // With ActiveCircuit[ActiveActor]
    //  }
    //  __finally
    //  {
        
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void WriteElementRecord( Textfile& F, TDSSCktElement* pElem )
    {
      int       Nterm = 0, 
                j = 0;
      String    BusName = "";
      String    EName = "";

      Nterm     = pElem->Get_NTerms();
      BusName   = Pad( StripExtension( pElem->Get_FirstBus() ), MaxBusNameLength );
      System::Write(F, Pad( FullName( pElem ), MaxDeviceNameLength + 2 ) ); 
      System::Write(F," ");
      for ( int stop = Nterm, j = 1; j <= stop; j++)
      {
        System::Write(F, UpperCase(BusName));
        System::Write(F," ");
        BusName = Pad( StripExtension( pElem->Get_NextBus() ), MaxBusNameLength );
      }
      System::WriteLn( F );
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowElements( String Filenm, String ClassName )

    // Show Elements and bus connections

    {
      TTextRec          F = {}, 
                        FDisabled = {};
      int               i = 0;
      String            DisabledFileNm = "";
      TDSSCktElement*   pElem = nullptr;

      SetMaxBusNameLength();
      SetMaxDeviceNameLength();
      try
      {
        try
        {
          AssignFile( F, Filenm );
          Rewrite( F );
          IOResultToException();
        }
        catch( std::exception & E )
        {
          DoSimpleMsg( String( "Error Trying to open element file \"" ) + Filenm + "\" file:" + (std::string) E.what(), 219000 );
        }
        try
        {
          DisabledFileNm = StripExtension( Filenm ) + "_Disabled.txt";
          AssignFile( FDisabled, DisabledFileNm );
          Rewrite( FDisabled );
          IOResultToException();
        }
        catch (std::exception &E)
        {
          DoSimpleMsg( String( "Error Trying to open disabled element file \"" ) + DisabledFileNm + "\" file:" + (std::string) E.what(), 219000 );
        }
        if ( ClassName.size( ) > 0 )
        {  // Just give a list of Active elements of a particular Class
          if ( SetObjectClass( ClassName ) )
          {
            System::Write( F, "All Elements in Class \"" ); System::Write( F, ClassName ); System::WriteLn( F, '\"' );
            System::WriteLn( F );
            System::Write( FDisabled, "All DISABLED Elements in Class \"" ); System::Write( FDisabled, ClassName ); System::WriteLn( FDisabled, '\"' );
            System::WriteLn( FDisabled );
            ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get( LastClassReferenced[ActiveActor] );
            for ( int stop = ActiveDSSClass[ActiveActor]->Get_ElementCount(), i = 1; i <= stop; i++)
            {
              ActiveDSSClass[ActiveActor]->Set_Active(i);
              if ( ( ( ActiveDSSClass[ActiveActor]->DSSClassType & BaseClassMask ) ) > 0 )
              {
                if (((TDSSCktElement*)ActiveDSSObject[ActiveActor])->Get_Enabled())                    
                  System::WriteLn( F, UpperCase( (( TDSSCktElement* ) ActiveDSSObject[ActiveActor] )->get_Name() ) );
                else
                  System::WriteLn( FDisabled, UpperCase( ( ( TDSSCktElement* ) ActiveDSSObject[ActiveActor] )->get_Name() ) );
              }
              else
                System::WriteLn( F, UpperCase(((TDSSCktElement*)ActiveDSSObject[ActiveActor])->get_Name()) );   // non cktelements
            }
          }
        }
        else
        {  // Default - Just do PD and PC Element in active circuit
          System::WriteLn( F );
          System::WriteLn( F, "Elements in Active Circuit: " + ActiveCircuit[ActiveActor]->Get_Name() );
          System::WriteLn( F );
          System::WriteLn( F, "Power Delivery Elements" );
          System::WriteLn( F );
          System::Write( F, Pad( "Element", MaxDeviceNameLength + 2 ) ); System::Write( F, Pad( " Bus1", MaxBusNameLength ) ); System::Write( F, Pad( " Bus2", MaxBusNameLength ) ); System::Write( F, Pad( " Bus3", MaxBusNameLength ) ); System::WriteLn( F, " ..." );
          System::WriteLn( F );
          System::WriteLn( FDisabled );
          System::WriteLn( FDisabled, "DISABLED Elements in Active Circuit: " + ActiveCircuit[ActiveActor]->Get_Name() );
          System::WriteLn( FDisabled );
          System::WriteLn( FDisabled, "DISABLED Power Delivery Elements" );
          System::WriteLn( FDisabled );
          System::Write( FDisabled, Pad( "DISABLED Element", MaxDeviceNameLength + 2 ) ); System::Write( FDisabled, Pad( " Bus1", MaxBusNameLength ) ); System::Write( FDisabled, Pad( " Bus2", MaxBusNameLength ) ); System::Write( FDisabled, Pad( " Bus3", MaxBusNameLength ) ); System::WriteLn( FDisabled, " ..." );
          System::WriteLn( FDisabled );

         // PDELEMENTS first
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
          while ( ASSIGNED(pElem) )
          {
            if ( pElem->Get_Enabled() )
            {
              WriteElementRecord( F, pElem );
            }
            else
            {
              WriteElementRecord(FDisabled, pElem);
            }
            pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
          }
          
          System::WriteLn( F );
          System::WriteLn( F, "Power Conversion Elements" );
          System::WriteLn( F );
          System::Write( F, Pad( "Element", MaxDeviceNameLength + 2 ) ); System::Write( F, Pad( " Bus1", MaxBusNameLength ) ); System::Write( F, Pad( " Bus2", MaxBusNameLength ) ); System::Write( F, Pad( " Bus3", MaxBusNameLength ) ); System::WriteLn( F, " ..." );
          System::WriteLn( F );


          System::WriteLn( FDisabled );
          System::WriteLn( FDisabled, "DISABLED Power Conversion Elements" );
          System::WriteLn( FDisabled );
          System::Write( FDisabled, Pad( "DISABLED Element", MaxDeviceNameLength + 2 ) ); System::Write( FDisabled, Pad( " Bus1", MaxBusNameLength ) ); System::Write( FDisabled, Pad( " Bus2", MaxBusNameLength ) ); System::Write( FDisabled, Pad( " Bus3", MaxBusNameLength ) ); 
          System::WriteLn(FDisabled, " ..." );
          System::WriteLn( FDisabled );
         
         // PCELEMENTS next
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
          while ( pElem != NULL )
          {
            if ( pElem->Get_Enabled() )
            {
             WriteElementRecord( F, pElem );
            }
            else
            {
              WriteElementRecord( FDisabled, pElem );
            }
            pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
          }
          
        }
    //  }
    //  __finally
    //  {
        CloseFile(FDisabled);
        if (AutoDisplayShowReport)
            FireOffEditor( DisabledFileNm );
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowBuses( String Filenm )

    // Show bus names and nodes in uses

    {
      TTextRec F;
      int i = 0, j = 0;
      try
      {
        SetMaxBusNameLength();
        MaxBusNameLength += 2;
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F );
        System::WriteLn( F, "BUSES AND NODES IN ACTIVE CIRCUIT: " + ActiveCircuit[ActiveActor]->Get_Name() );
        System::WriteLn( F );
        System::Write( F, Pad( "     ", MaxBusNameLength ) ); System::WriteLn( F, "                         Coord                                 Number of     Nodes" );
        System::Write( F, Pad( "  Bus", MaxBusNameLength ) ); System::WriteLn( F, "    Base kV             (x, y)                      Keep?       Nodes        connected ..." );
        System::WriteLn( F );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            {
              System::Write( F, Pad( EncloseQuotes( with0->BusList.Get( i ) ), MaxBusNameLength ) ); System::Write( F, ' ' );
              TDSSBus& pBus (*( with0->Buses[i - 1] ));
              if ( pBus.kVBase > 0.0 )
                System::Write( F, Format("%8.3f",(pBus.kVBase * SQRT3)));
              else
                System::Write( F, "   NA " );
              System::Write( F, "          (" );
              if ( pBus.CoordDefined )
                System::Write( F, Format( " %-13.11g, %-13.11g)",  pBus.x, pBus.y ));
              else
                System::Write( F, "           NA,            NA )" );
              if ( pBus.Keep )
                System::Write( F, "     Yes  " );
              else
                System::Write( F, "     No  " );
              System::Write( F, "     " );
              System::Write( F, pBus.get_FNumNodesThisBus(), 5 );
              System::Write( F, "       " );
              for ( int stop = pBus.get_FNumNodesThisBus(), j = 1; j <= stop; j++)
              {
                System::Write( F, pBus.GetNum( j ), 4 ); System::Write( F, ' ' );
              }
              System::WriteLn( F );
            }
          }
        }
    //  }
    //  __finally
    //  {
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }


    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowMeters( String Filenm )

    // Show Values of  Meter Elements

    {
      TTextRec F;
      int i = 0, j = 0;
      TEnergyMeterObj* pElem;
      TEnergyMeter* MeterClass;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F );
        System::WriteLn( F, "ENERGY METER VALUES" );
        System::WriteLn( F );
        System::WriteLn( F, "Registers:" );
        MeterClass = (TEnergyMeter*) GetDSSClassPtr( "Energymeter" );
        if ( MeterClass == NULL )
          return;  // oops somewhere!!
        if ( ( (TDSSClass*) MeterClass )->Get_ElementCount() == 0 )
        {
          System::WriteLn( F, "No Energymeter Elements Defined." );
        }
        else
        { ;
          pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();   // write registernames for first meter only
          for ( int stop = NumEMRegisters, i = 1; i <= stop; i++)
          {
            System::Write( F, "Reg " + IntToStr( i ) + " = " ); System::WriteLn( F, pElem->RegisterNames[i - 1] );
          }
          System::WriteLn( F );
          pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_First();
          if ( pElem != NULL )
          {
            System::Write( F, "Meter        " );
            for ( int stop = NumEMRegisters, i = 1; i <= stop; i++)
              System::Write( F, Pad( "   Reg " + IntToStr( i ), 11 ) );
            System::WriteLn( F );
            System::WriteLn( F );
            while ( pElem != NULL )
            {
              if ( ( (TMeterElement*) pElem )->Get_Enabled() )
              {
                System::Write( F, Pad( ( ( TMeterElement* )pElem )->get_Name(), 12 ) );                             
                
                
                for ( int stop = NumEMRegisters, j = 1; j <= stop; j++)
                {
                  System::Write( F, Format("%8.0f", pElem->Registers[j - 1])); System::Write(F, ' ');
                }
              }
              pElem = (TEnergyMeterObj*) ActiveCircuit[ActiveActor]->EnergyMeters.Get_Next();
              System::WriteLn( F );
            }
          }
        }
    //  }
    //  __finally
    //  {
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          ///
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowGenMeters( String Filenm )

    // Show Values of Generator Meter Elements

    {
        TTextRec F;
        int i = 0, j = 0;
        TGeneratorObj* pElem;
        TGenerator* GeneratorClass;
        try
        {
            AssignFile(F, Filenm);
            Rewrite(F);
            IOResultToException();
            System::WriteLn(F);
            System::WriteLn(F, "GENERATOR ENERGY METER VALUES");
            System::WriteLn(F);
            pElem = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_First();
            if (pElem != NULL)
            {
                GeneratorClass = (TGenerator*)pElem->ParentClass;
                System::Write(F, "Generator          ");
                for (int stop = Generator::NumGenRegisters, i = 1; i <= stop; i++)
                    System::Write(F, Pad(GeneratorClass->RegisterNames[i - 1], 11));
                System::WriteLn(F);
                System::WriteLn(F);
                while (pElem != NULL)
                {
                    if (((TDSSCktElement*)pElem)->Get_Enabled())
                    {
                        System::Write(F, Pad(((TDSSCktElement*)pElem)->get_Name(), 12));
                        auto with0 = ActiveCircuit[ActiveActor]->Solution;
                        pElem->Registers[0] = pElem->kWBase;
                        pElem->Registers[1] = pElem->kvarBase;
                        pElem->Registers[4] = with0->IntervalHrs;
                        pElem->Registers[5] = ActiveCircuit[ActiveActor]->PriceSignal;

                        for (int stop = Generator::NumGenRegisters-1, j = 0; j <= stop; j++)
                        {
                            if (pElem->Registers[j] < -6.2e+10)
                            {
                                System::Write(F, 0); System::Write(F, ' ');
                            }
                            else
                            {
                                System::Write(F, Format("%8.0f", pElem->Registers[j])); System::Write(F, ' ');
                            }
                        }
                    }
                    pElem = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_Next();
                    System::WriteLn(F);
                }
            }
            //  }
            //  __finally
            //  {
            CloseFile(F);
            if (AutoDisplayShowReport)
                FireOffEditor(Filenm);
            ParserVars->Add("@lastshowfile", Filenm);
        }
        catch (...)
        {
            //
        }
    }


    int TapPosition( TTransfObj* Transformer, int iWind )

    /*Assumes 0  is 1.0 per unit tap*/
    {
      int result = 0;
      /*# with Transformer do */
      auto with0 = Transformer;
      result = Round( double( ( with0->Get_PresentTap(iWind,ActiveActor) - double( ( with0->Get_MaxTap(iWind) + with0->Get_MinTap(iWind) ) ) / 2.0 ) ) / with0->Get_TapIncrement(iWind) );
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowRegulatorTaps( String Filenm )
    {
      TTextRec F;
      TRegControlObj* pReg;
      int iWind = 0;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F );
        System::WriteLn( F, "CONTROLLED TRANSFORMER TAP SETTINGS" );
        System::WriteLn( F );
        System::WriteLn( F, "Name    RegControl        Tap      Min       Max     Step      Position      Winding      Direction       CogenMode" );
        System::WriteLn( F );
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
                //System::Write( F, Pad( ( (TDSSObject*) with1 )->get_Name(), 12 ) ); System::Write( F, ' ' );
                System::Write( F, Pad( ( with1 )->get_Name(), 12 ) ); System::Write( F, ' ' );
                System::Write(F, pReg->LName); System::Write(F, "          ");

                string direction = "";
                if (with1->DeltaDirection == 1)  direction = "Forward";
                else  direction = "Backwards";

                System::Write(F, Format("%8.5f %8.5f %8.5f %8.5f     %d      %d", with1->Get_PresentTap(iWind, ActiveActor), with1->Get_MinTap(iWind), with1->Get_MaxTap(iWind), with1->Get_TapIncrement(iWind), TapPosition(pReg->Get_Transformer(), iWind), iWind));
                string CGEn = "      ";
                System::Write(F, CGEn);
                System::Write(F, direction);
                if (pReg->CogenEnabled) CGEn += "True"; else CGEn += "False";
                System::Write(F, CGEn);
                System::Write(F, "\n");
                }
              pReg = (TRegControlObj*) with0->RegControls.Get_Next();
            }
          }
        }
    //  }
    //  __finally
    //  {
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ShowMeterZone( String Filenm )
    {
      TTextRec F;
      int               i = 0;
      TEnergyMeterObj*  pMtr = nullptr;
      TEnergyMeterObj*  pMeter = nullptr;
      TEnergyMeter*     pMtrClass = nullptr;
      TPDElement*       PDElem = nullptr;
      TLoadObj*         LoadElem = nullptr;
      String            ParamName = "",
                        Param = "";
      try
      {
        Filenm = StripExtension( Filenm );
        ParamName = Parser[ActiveActor]->GetNextParam();
        Param = Parser[ActiveActor]->MakeString_();
        Filenm = Filenm + "_" + Param + ".txt";
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        GlobalResult = Filenm;
        pMtrClass = (TEnergyMeter*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "energymeter" ) );
        if ( Param.size( ) > 0 )
        {
          pMtr = (TEnergyMeterObj*) pMtrClass->Find( Param );
          if ( pMtr == NULL )
            DoSimpleMsg( String( "EnergyMeter \"" ) + Param + "\" not found.", 220 );
          else
            if ( pMtr->BranchList != NULL )
            {
              System::Write( F, "Branches and Load in Zone for EnergyMeter " ); System::WriteLn( F, Param );
              System::WriteLn( F );
              PDElem = (TPDElement* ) pMtr->BranchList->Get_First();
              while ( PDElem != NULL )
              {
                for ( int stop = pMtr->BranchList->Get_Level(), i = 1; i <= stop; i++)
                    System::Write( F, TABCHAR );
                    //System::Write(F, pMtr.BranchList.Level:0,' ');
                System::Write( F,PDElem->ParentClass->get_myClass_name() ); 
                System::Write( F, '.' ); 
                System::Write( F, PDElem->LName);             
                /*# with pMtr.BranchList.PresentBranch do */
                {
                auto with0 = pMtr->BranchList->PresentBranch;
                    if ( with0->IsParallel )
                        System::Write( F, "(PARALLEL:" + ( (TDSSCktElement*) with0->LoopLineObj )->get_Name() + ")" );
                    if (with0->IsLoopedHere )
                        System::Write( F, "(LOOP:" + ((TDSSCktElement*)with0->LoopLineObj)->ParentClass->get_myClass_name() + "." + ( (TDSSCktElement*) with0->LoopLineObj )->get_Name() + ")" );
                }
                if (( PDElem->SensorObj != NULL ) )
                    System::Write( F, " (Sensor: " + PDElem->SensorObj->ParentClass->get_myClass_name() +  "." + PDElem->SensorObj->get_Name() + ") " );
                else
                    System::Write( F, " (Sensor: NIL)" );

                System::WriteLn( F );
                LoadElem = (TLoadObj*) pMtr->BranchList->Get_FirstObject();
                while (LoadElem != NULL)
                {
                    for (int stop = pMtr->BranchList->Get_Level() + 1, i = 1; i <= stop; i++)
                        System::Write(F, TABCHAR);
                    System::Write(F, LoadElem->ParentClass->get_myClass_name()); System::Write(F, '.'); System::Write(F, LoadElem->get_Name());
                    if (ASSIGNED(LoadElem->SensorObj))
                        System::Write(F, " (Sensor: " + LoadElem->SensorObj->ParentClass->get_myClass_name() + "." + LoadElem->SensorObj->get_Name() + ") ");
                    else
                        System::Write(F, " (Sensor: NIL)");
                    System::WriteLn(F);
                    LoadElem = (TLoadObj*)pMtr->BranchList->Get_NextObject();
                 }
                PDElem = (TPDElement*)pMtr->BranchList->Get_Forward();
              }
            }
        }
        else
          DoSimpleMsg( "Meter Name Not Specified." + CRLF + Parser[ActiveActor]->get_CmdBuffer(), 221 );
    //  }
    //  __finally
    //  {
        CloseFile( F );
        ParamName = Parser[ActiveActor]->GetNextParam();
        Param = Parser[ActiveActor]->MakeString_();
        switch ( Param.size( ) )
        {
            case 0:
            {
                if (AutoDisplayShowReport)
                    FireOffEditor(Filenm);
            }
          break;
        default:
            ShowTreeView( Filenm );
        }
        ParserVars->Add( "@lastshowfile", Filenm );
         //
      }
      catch (...)
      {
          //
      }
    }

    void ShowOverloads( String Filenm )
    {
      TTextRec F;
      vector<complex>   c_Buffer;  // Allocate to max total conductors

      int               Ncond = 0, 
                        i = 0, 
                        j = 0, 
                        k = 0;
      TPDElement*       PDElem = nullptr;
      complex           Iph[3]  = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) }, 
                        I012[3] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
      double            I0 = 0.0, 
                        I1 = 0.0, 
                        I2 = 0.0, 
                        cmag = 0.0, 
                        Cmax = 0.0;

      SetMaxDeviceNameLength();
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

          /*Allocate c_Buffer big enough for largest circuit element*/
        c_Buffer.resize(GetMaxCktElementSize() + 1);

         /*Sequence Currents*/
        System::WriteLn( F );
        System::WriteLn( F, "Power Delivery Element Overload Report" );
        System::WriteLn( F );
        System::WriteLn( F, "SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT " );
        System::WriteLn( F );
        System::WriteLn( F, "Element                             Term    I1    IOver %Normal  %Emerg     I2    %I2/I1    I0    %I0/I1" );
        System::WriteLn( F );



         // PDELEMENTS
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if ( ( (TDSSCktElement*) PDElem )->Get_Enabled() )
            if     // Ignore capacitors
            ( ( ( CLASSMASK & ((TDSSCktElement*)PDElem)->DSSObjType ) ) != CAP_ELEMENT )
            {
              Ncond = ((TDSSCktElement*)PDElem)->Get_NConds();
              PDElem->GetCurrents( &c_Buffer[0], ActiveActor);
              for ( int stop = 1, j = 1; j <= stop; j++)     // Check only terminal 1 for overloads
              {
                if (((TDSSCktElement*)PDElem)->Get_NPhases() >= 3 )
                {
                  Cmax = 0.0;
                  for ( int stop = 3, i = 0; i < stop; i++)
                  {
                    k = ( j - 1 ) * Ncond + i + 1;
                    Iph[i] = (c_Buffer)[k-1];
                    cmag = cabs( Iph[i] );
                    if ( cmag > Cmax )
                      Cmax = cmag;
                  }
                  Phase2SymComp( & Iph[0], &I012[0]);
                  I0 = cabs( I012[0] );
                  I1 = cabs( I012[1] );
                  I2 = cabs( I012[2] );
                }
                else
                {
                  I0 = 0.0;
                  I1 = cabs( c_Buffer[1 + ( j - 1 ) * Ncond] );
                  I2 = 0.0;
                  Cmax = I1;
                }
                if ( ( PDElem->NormAmps > 0.0 ) || ( PDElem->EmergAmps > 0.0 ) )
                  if ( ( Cmax > PDElem->NormAmps ) || ( Cmax > PDElem->EmergAmps ) )
                  {
                    System::Write( F, Pad( FullName( PDElem ), MaxDeviceNameLength + 2 ) ); System::Write( F, j, 3 );
                    System::Write( F, Format("%8.1f", I1));
                    if ( PDElem->NormAmps > 0.0 )
                      System::Write( F, Format("%8.2f", Cmax - PDElem->NormAmps ));
                    else
                      System::Write( F, "     0.0" );
                    if ( PDElem->NormAmps > 0.0 )
                      System::Write( F, Format("%8.1f", Cmax / PDElem->NormAmps * 100.0));
                    else
                      System::Write( F, "     0.0" );
                    if ( PDElem->EmergAmps > 0.0 )
                      System::Write( F, Format("%8.1f", Cmax / PDElem->EmergAmps * 100.0));
                    else
                      System::Write( F, "     0.0" );
                    System::Write( F, Format("%8.1f", I2));
                    if ( I1 > 0.0 )
                      System::Write( F, Format("%8.1f", 100.0 * I2 / I1));
                    else
                      System::Write( F, "     0.0" );
                    System::Write( F, Format("%8.1f", I0));
                    if ( I1 > 0.0 )
                      System::Write( F, Format("%8.1f", 100.0 * I0 / I1));
                    else
                      System::Write( F, "     0.0" );
                    System::WriteLn( F );
                  }
              } /*For*/
            }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
    //  }
    //  __finally
    //  {
        c_Buffer.resize(0);
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    /* - -- - - - - ------------------------------*/


    void ShowUnserved( String Filenm, bool UE_Only )
    {
      TTextRec F;
      TLoadObj* pLoad;
      bool DoIt = false;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F );
        System::WriteLn( F, "UNSERVED  LOAD  REPORT" );
        System::WriteLn( F );
        System::WriteLn( F, "Load Element        Bus        Load kW  EEN Factor  UE Factor" );
        System::WriteLn( F );

         // Load
        pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
        while ( pLoad != NULL )
        {
          if ( ( (TDSSCktElement*) pLoad)->Get_Enabled() )
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
              //pLoad->Get_Unserved(ActiveActor);
              //pLoad->Get_ExceedsNormal(ActiveActor);
              System::Write( F, Pad( pLoad->get_Name(), 20));
              System::Write( F, Pad( ( (TDSSCktElement*) pLoad )->GetBus( 1 ), 10 ) );
              System::Write( F, Format("%8.0f", pLoad->kWBase));
              System::Write( F, Format("%8.3f", pLoad->EEN_Factor));
              System::Write( F, Format("%8.3f", pLoad->UE_Factor));
              System::WriteLn( F );
            }
          }
          pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
        }
    //  }
    //  __finally
    //  {
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowLosses( String Filenm )
    {
      TTextRec F;
      TPDElement*   PDElem      = nullptr;
      TPCElement*   PCelem      = nullptr;
      complex       kLosses     = cmplx(0, 0), 
                    TotalLosses = cmplx(0, 0), 
                    LineLosses  = cmplx(0, 0), 
                    TransLosses = cmplx(0, 0), 
                    TermPower   = cmplx(0, 0), 
                    LoadPower   = cmplx(0, 0);
      SetMaxDeviceNameLength();
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

         /*Sequence Currents*/
        System::WriteLn( F );
        System::WriteLn( F, "LOSSES REPORT" );
        System::WriteLn( F );
        System::WriteLn( F, "Power Delivery Element Loss Report" );
        System::WriteLn( F );
        System::WriteLn( F, "Element                  kW Losses    % of Power   kvar Losses" );
        System::WriteLn( F );
        TotalLosses = CZero;
        LineLosses = CZero;
        TransLosses = CZero;

         // PDELEMENTS
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if
           /*THEN IF (CLASSMASK AND PDElem.DSSObjType) <>  CAP_ELEMENT */    // Ignore capacitors
          ( ( PDElem )->Get_Enabled() )
          {
            //----PDelem.ActiveTerminalIdx := 1;  // activate 1st terminal for Power call
            kLosses = cmulreal( ( PDElem )->Get_Losses(ActiveActor), 0.001 );   // kW Losses in element
            caccum( TotalLosses, kLosses );
            TermPower = cmulreal( ( PDElem )->Get_Power(1, ActiveActor), 0.001 );     // Terminal 1 power
            if ( ( ( CLASSMASK & ( PDElem )->DSSObjType ) ) == XFMR_ELEMENT )    caccum( TransLosses, kLosses );
            if ( ( ( CLASSMASK & (PDElem)->DSSObjType ) ) == AUTOTRANS_ELEMENT ) caccum( TransLosses, kLosses );
            if ( ( ( CLASSMASK & (PDElem)->DSSObjType ) ) == LINE_ELEMENT )      caccum( LineLosses, kLosses );

            System::Write( F, Pad( FullName( PDElem ), MaxDeviceNameLength + 2 ) );
            System::Write( F, Format( "%10.5f, ",  kLosses.re ));
            
            if ( ( TermPower.re != 0.0 ) && ( kLosses.re > 0.0009 ) )
                System::Write(F, Format("%8.2f", (kLosses.re / Abs(TermPower.re)) * 100.0));
            else
              System::Write( F, Format("%8.1f", CZero.re));

            System::Write( F, Format( "     %.6g",  kLosses.im ));
            System::WriteLn( F );
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }      /*While*/
        System::WriteLn( F );
        System::Write( F, Pad( "LINE LOSSES=", 30 ) ); System::Write( F, Format("%10.1f", LineLosses.re)); System::WriteLn(F, " kW");
        System::Write( F, Pad( "TRANSFORMER LOSSES=", 30 ) ); System::Write( F, Format("%10.1f", TransLosses.re)); System::WriteLn( F, " kW" );
        System::WriteLn( F );
        System::Write( F, Pad( "TOTAL LOSSES=", 30 ) ); System::Write( F, Format("%10.1f", TotalLosses.re)); System::WriteLn( F, " kW" );
        LoadPower = CZero;
         // Sum the total load kW being served in the Ckt Model
        PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->Loads.Get_First();
        while ( PCelem != NULL )
        {
          if ( ( PCelem )->Get_Enabled() )
          {
            caccum( LoadPower, ( PCelem )->Get_Power(1, ActiveActor) );
          }
          PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
        }
        LoadPower = cmulreal( LoadPower, 0.001 );
        System::WriteLn( F );
        System::Write( F, Pad( "TOTAL LOAD POWER = ", 30 ) ); System::Write( F, Format("%10.1f", Abs( LoadPower.re ))); System::WriteLn( F, " kW" );
        System::Write( F, Pad( "Percent Losses for Circuit = ", 30 ) );
        if ( LoadPower.re != 0.0 )
        {
          System::Write( F, Format("%8.2f", Abs(TotalLosses.re / LoadPower.re ) * 100.0)); System::WriteLn( F, " %" );
        }
    //  }
    //  __finally
    //  {
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }


    void ShowVariables( String Filenm )
    {
      TTextRec F;
      TPCElement* PCelem;
      int i = 0;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

         /*Sequence Currents*/
        System::WriteLn( F );
        System::WriteLn( F, "VARIABLES REPORT" );
        System::WriteLn( F );
        System::WriteLn( F, "Present values of all variables in PC Elements in the circuit." );
        System::WriteLn( F );
        PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( PCelem != NULL )
        {
          if ( PCelem->Get_Enabled() && ( PCelem->NumVariables() > 0 ) )
          {
            System::Write( F, "ELEMENT: " ); System::Write( F, PCelem->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::WriteLn( F, PCelem->get_Name() );
            System::Write( F, "No. of variables: " ); System::WriteLn( F, PCelem->NumVariables(), 0 );
            for ( int stop = PCelem->NumVariables(), i = 1; i <= stop; i++)
            {
                string varname = PCelem->VariableName(i);
                if (PCelem->Get_Variable(i) < 1.2e-10 || PCelem->Get_Variable(i) > 1.2e+10)
                {
                    //PCelem->Set_Variable(i,0);
                    System::Write(F, "  "); System::Write(F, PCelem->VariableName(i)); System::Write(F, " = "); System::WriteLn(F, 0);
                }
                else
                {
                    System::Write(F, "  "); System::Write(F, PCelem->VariableName(i)); System::Write(F, " = "); System::WriteLn(F, Format("%1.6g", PCelem->Get_Variable(i)));
                }
            }
            System::WriteLn( F );
          }
          PCelem = (TPCElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowIsolated( String Filenm )

    /*Show isolated buses/branches in present circuit*/
    {
        TCktTree*   Branch_List = nullptr;
        TCktTree*   SubArea = nullptr;      // Pointers to all circuit elements

        TTextRec    F = {};
        TDSSCktElement* TestElement = nullptr;
        TDSSCktElement* TestBranch  = nullptr;
        TDSSCktElement* pElem       = nullptr;
        int         i = 0, 
                    j = 0;

         // Make sure bus list is built
      if ( ActiveCircuit[ActiveActor]->get_FBusNameRedefined() )
        ActiveCircuit[ActiveActor]->ReProcessBusDefs( ActiveActor );
      ActiveCircuit[ActiveActor]->ReProcessBusDefs( ActiveActor );
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {

             /*Initialize all Circuit Elements to not checked*/
          TestElement = (TDSSCktElement*) with0->CktElements.Get_First();
          while ( TestElement != NULL )
          {
            /*# with TestElement do */
            auto with1 = TestElement;
            {
              with1->Checked = false;
              for ( int stop = with1->Get_NTerms(), i = 1; i <= stop; i++)
                with1->Terminals[i - 1].Checked = false;
            }
            TestElement = (TDSSCktElement*) with0->CktElements.Get_Next();
          }

             // initialize the Checked Flag for all Buses
          for ( int stop = with0->NumBuses, j = 1; j <= stop; j++)
            with0->Buses[j - 1]->BusChecked = false;
        }
      }

        // Get Started at main voltage source
      TestElement = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
      Branch_List = (TCktTree*)( GetIsolatedSubArea( TestElement ) );

        /*Show Report of Elements connected and not connected*/
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F );
        System::WriteLn( F, "ISOLATED CIRCUIT ELEMENT REPORT" );
        System::WriteLn( F );
        System::WriteLn( F );
        System::WriteLn( F, "***  THE FOLLOWING BUSES HAVE NO CONNECTION TO THE SOURCE ***" );
        System::WriteLn( F );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with1 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with1->NumBuses, j = 1; j <= stop; j++)
              if ( ! with1->Buses[j - 1]->BusChecked )
                System::WriteLn( F, EncloseQuotes( with1->BusList.Get( j ) ) );
          }
        }
        System::WriteLn( F );
        System::WriteLn( F, "***********  THE FOLLOWING SUB NETWORKS ARE ISOLATED ************" );
        System::WriteLn( F );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with2 = ActiveCircuit[ActiveActor];
          {
            TestElement = (TDSSCktElement*) with2->CktElements.Get_First();
            while ( ASSIGNED(TestElement) )
            {
              if ( TestElement->Get_Enabled() )
                if ( ! TestElement->Checked )
                  if ( ( ( TestElement->DSSObjType & BaseClassMask ) ) == PD_ELEMENT )
                  {
                    SubArea = (TCktTree*)( GetIsolatedSubArea( TestElement ) );
                    System::WriteLn( F, "*** START SUBAREA ***" );
                    TestBranch = (TDSSCktElement*) SubArea->Get_First();
                    while ( TestBranch != NULL )
                    {
                      System::Write( F, '(' ); System::Write( F, SubArea->Get_Level(), 0 ); System::Write( F, ") " ); System::Write( F, TestBranch->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::WriteLn( F, TestBranch->get_Name() );
                      pElem = (TDSSCktElement*) SubArea->Get_FirstObject();
                      while ( pElem != NULL )
                      {
                        System::Write( F, "[SHUNT], " ); System::Write( F, pElem->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::WriteLn( F, pElem->get_Name() );
                        pElem = (TDSSCktElement*) SubArea->Get_NextObject();
                      }
                      TestBranch = (TDSSCktElement*) SubArea->Get_Forward();
                    }
                    delete SubArea; // SubArea->~TCktTree();
                    System::WriteLn( F );
                  }
              TestElement = (TDSSCktElement*) with2->CktElements.Get_Next();
            }
          }
        }
        System::WriteLn( F );
        System::WriteLn( F, "***********  THE FOLLOWING ENABLED ELEMENTS ARE ISOLATED ************" );
        System::WriteLn( F );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with3 = ActiveCircuit[ActiveActor];
          {

           /*Mark all controls, energy meters and monitors as checked so they don't show up*/
            for ( int stop = with3->DSSControls.get_myNumList(), i = 1; i <= stop; i++)
              ((TDSSCktElement*) with3->DSSControls.Get( i ) )->Checked = true;

            for ( int stop = with3->MeterElements.get_myNumList(), i = 1; i <= stop; i++)
              ( (TDSSCktElement*) with3->MeterElements.Get( i ) )->Checked = true;

            TestElement = (TDSSCktElement*) with3->CktElements.Get_First();
            while ( TestElement != NULL )
            {
              if ( TestElement->Get_Enabled() )
                if ( ! TestElement->Checked )
                {
                  System::Write( F, '\"' ); System::Write( F, TestElement->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::Write( F, TestElement->get_Name() ); System::Write( F, '\"' );
                  System::Write( F, "  Buses:" );
                  for ( int stop = TestElement->Get_NTerms(), j = 1; j <= stop; j++)
                  {
                    System::Write( F, "  \"" ); System::Write( F, TestElement->GetBus( j ) ); System::Write( F, '\"' );
                  }
                  System::WriteLn( F );
                }
              TestElement = (TDSSCktElement*) with3->CktElements.Get_Next();
            }
          }
        }
        System::WriteLn( F );
        System::WriteLn( F, "***  THE FOLLOWING BUSES ARE NOT CONNECTED TO ANY POWER DELIVERY ELEMENT ***" );
        System::WriteLn( F );
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with4 = ActiveCircuit[ActiveActor];
          {
            for ( int stop = with4->NumBuses, j = 1; j <= stop; j++)
              if ( ! with4->Buses[j - 1]->BusChecked )
                System::WriteLn( F, EncloseQuotes( with4->BusList.Get( j ) ) );
          }
        }
        System::WriteLn( F );
        System::WriteLn( F, "***********  CONNECTED CIRCUIT ELEMENT TREE ************" );
        System::WriteLn( F );
        System::WriteLn( F, "(Lexical Level) Element name" );
        System::WriteLn( F );
        TestBranch = (TDSSCktElement*) Branch_List->Get_First();
        while ( TestBranch != NULL )
        {
          System::Write( F, '(' ); System::Write( F, Branch_List->Get_Level(), 0 ); System::Write( F, ") " ); System::Write( F, TestBranch->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::WriteLn( F, TestBranch->get_Name() );
          TestElement = (TDSSCktElement*) Branch_List->Get_FirstObject();
          while ( TestElement != NULL )
          {
            System::Write( F, "[SHUNT], " ); System::Write( F, TestElement->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::WriteLn( F, TestElement->get_Name() );
            TestElement = (TDSSCktElement*) Branch_List->Get_NextObject();
          }
          TestBranch = (TDSSCktElement*) Branch_List->Get_Forward();
        }
    /*  }
      __finally
      {*/
        CloseFile( F );
        free(Branch_List);
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowRatings( String Filenm )
    {
      TTextRec F;
      TPDElement* PDElem;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F, "Power Delivery Elements Normal and Emergency (max) Ratings" );
        System::WriteLn( F );
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          System::Write( F, '\"' ); System::Write( F, (PDElem )->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::Write( F, PDElem->get_Name() ); System::Write( F, "\", normamps=" );
          System::Write( F, Format( "%-.4g,  %-.4g  !Amps",  PDElem->NormAmps, PDElem->EmergAmps ));
          System::WriteLn( F );
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }


    void ShowLoops( String Filenm )
    /*Show loops and paralleled branches in Meter zones*/
    {
      TTextRec F;
      TPDElement* PDElem;
      int hMeter = 0;
      TEnergyMeterObj* pMtr;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F, "Loops and Paralleled Lines in all EnergyMeter Zones" );
        System::WriteLn( F );
        hMeter = EnergyMeterClass[ActiveActor]->Get_First();
        while ( hMeter > 0 )
        {
            pMtr = (TEnergyMeterObj*)ActiveDSSObject[ActiveActor];
          if ( pMtr->BranchList!= NULL )
          {             
              PDElem = (TPDElement*)pMtr->BranchList->Get_First();
             
            while ( PDElem != NULL )
            {  
                auto with0 = pMtr->BranchList->PresentBranch;              
                
                if (with0 != NULL)
                {
                    if (with0->IsParallel)
                    {
                        System::Write(F, '('); 
                        System::Write(F, pMtr->get_Name()); 
                        System::Write(F, ") "); 
                        System::Write(F, PDElem->ParentClass->get_myClass_name()); 
                        System::Write(F, '.'); 
                        System::Write(F, UpperCase(PDElem->get_Name())); 
                        System::Write(F, ": PARALLEL WITH "); 
                        System::Write(F, ((TDSSCktElement*)with0->LoopLineObj)->ParentClass->get_myClass_name()); 
                        System::Write(F, '.'); System::WriteLn(F, ((TDSSCktElement*)with0->LoopLineObj)->get_Name());
                    }
                    if (with0->IsLoopedHere)
                    {
                        System::Write(F, '('); 
                        System::Write(F, pMtr->get_Name()); 
                        System::Write(F, ") "); 
                        System::Write(F, PDElem->ParentClass->get_myClass_name()); 
                        System::Write(F, '.'); 
                        System::Write(F, UpperCase(PDElem->get_Name())); System::Write(F, ": LOOPED TO     "); 
                        System::Write(F, ((TDSSCktElement*)with0->LoopLineObj)->ParentClass->get_myClass_name()); 
                        System::Write(F, '.'); System::WriteLn(F, ((TDSSCktElement*)with0->LoopLineObj)->get_Name());
                    }
                }
                PDElem = (TPDElement*)pMtr->BranchList->Get_Forward();
            }
          }
          hMeter = ( (TDSSClass*) EnergyMeterClass[ActiveActor] )->Get_Next();
        }
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void TopoLevelTabs( Textfile& F, int nLevel )
    {
      int nTabs = 0, i = 0;
      nTabs = 30;
      if ( nLevel < nTabs )
        nTabs = nLevel;
      for ( int stop = nTabs, i = 1; i <= stop; i++)
      {
        System::Write(F,TABCHAR );
      }
      if ( nLevel > nTabs )
      {
        System::Write(F, Format( "(* %d *)",  nLevel ));
      }
    }


    void ShowTopology( String Fileroot )
    {
      TTextRec F, Ftree;
      String Filenm, TreeNm;
      TPDElement* PDElem;
      TDSSCktElement* pControlElem;
      TLoadObj* LoadElem;
      TCktTree* Topo;
      int nLoops = 0, nParallel = 0, nLevels = 0, nIsolated = 0, nSwitches = 0;
      try
      {
        Filenm = Fileroot + "TopoSumm.Txt";
        TreeNm = Fileroot + "TopoTree.Txt";
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F, "Topology analysis for switch control algorithms" );
        System::WriteLn( F );
        AssignFile( Ftree, TreeNm );
        Rewrite( Ftree );
        IOResultToException();
        System::WriteLn( Ftree, "Branches and Loads in Circuit " + ActiveCircuit[ActiveActor]->Get_Name() );
        System::WriteLn( Ftree );
        Topo = &(ActiveCircuit[ActiveActor]->GetTopology());
        nLoops = 0;
        nParallel = 0;
        nLevels = 0;
        nIsolated = 0;
        nSwitches = 0;
        if (( Topo != NULL ) )
        {
          PDElem = (TPDElement*) Topo->Get_First();
          while (( PDElem != NULL ) )
          {
            if ( Topo->Get_Level() > nLevels )
              nLevels = Topo->Get_Level();
            TopoLevelTabs( Ftree, Topo->Get_Level() );
            System::Write( Ftree, ( (TDSSCktElement*) PDElem )->ParentClass->get_myClass_name() ); System::Write( Ftree, '.' ); System::Write( Ftree, ( (TDSSCktElement*) PDElem )->get_Name() );
            /*# with Topo.PresentBranch do */
            {
              auto with0 = Topo->PresentBranch;
              if ( with0->IsParallel )
              {
                nParallel++;
                System::Write( Ftree, "(PARALLEL:" + ( (TDSSCktElement*) with0->LoopLineObj )->get_Name() + ")" );
              }
              if ( with0->IsLoopedHere )
              {
                nLoops++;
                System::Write( Ftree, "(LOOP:" + ( (TDSSCktElement*) with0->LoopLineObj )->ParentClass->get_myClass_name() + "." + ( (TDSSCktElement*) with0->LoopLineObj )->get_Name() + ")" );
              }
              if ( ( (TDSSCktElement*) PDElem )->HasSensorObj )
                System::Write( Ftree, "Sensor: " + PDElem->SensorObj->ParentClass->get_myClass_name() + PDElem->SensorObj->get_Name() + "." + ") ");
              if ( ( (TDSSCktElement*) PDElem )->HasControl )
              {
                pControlElem = (TDSSCktElement*) ( (TDSSCktElement*) PDElem )->ControlElementList.Get_First();
                while ( pControlElem != NULL )
                {                                // accommodate multiple controls on same branch
                  System::Write( Ftree," (Control: " + pControlElem->ParentClass->get_myClass_name() + "." + pControlElem->get_Name() + ") ");
                  if ( ( ( pControlElem->DSSObjType & CLASSMASK ) ) == SWT_CONTROL )
                    nSwitches++;
                  pControlElem = (TDSSCktElement*) ( (TDSSCktElement*) PDElem )->ControlElementList.Get_Next();
                }
              }
              if ( ( (TDSSCktElement*) PDElem )->HasEnergyMeter )
                System::Write( Ftree, " (Meter:" + PDElem->MeterObj->get_Name() + ") ");
            }
            System::WriteLn( Ftree );
            LoadElem = (TLoadObj*) Topo->Get_FirstObject();
            while (( LoadElem != NULL ) )
            {
              TopoLevelTabs( Ftree, Topo->Get_Level() + 1 );
              System::Write( Ftree, ( (TDSSCktElement*) LoadElem )->ParentClass->get_myClass_name() ); System::Write( Ftree, '.' ); System::Write( Ftree, ( (TDSSCktElement*) LoadElem )->get_Name() );
              if ( ( (TDSSCktElement*) LoadElem )->HasSensorObj )
                System::Write( Ftree, " (Sensor: " + LoadElem->SensorObj->ParentClass->get_myClass_name() + "." + LoadElem->SensorObj->get_Name() + ") ");
              if ( ( (TDSSCktElement*) LoadElem )->HasControl )
              {
                pControlElem = (TDSSCktElement*) ( (TDSSCktElement*) LoadElem )->ControlElementList.Get_First();
                while ( pControlElem != NULL )
                {                                // accommodate multiple controls on same branch
                  System::Write( Ftree, " (Control: " + pControlElem->ParentClass->get_myClass_name() + "."  + pControlElem->get_Name() + ") ");
                  if ( ( ( pControlElem->DSSObjType & CLASSMASK ) ) == SWT_CONTROL )
                    nSwitches++;
                  pControlElem = (TDSSCktElement*) ( (TDSSCktElement*) LoadElem )->ControlElementList.Get_Next();
                }
              }
              if ( ( (TDSSCktElement*) LoadElem )->HasEnergyMeter )
                System::Write( Ftree, " (Meter: " + LoadElem->MeterObj->get_Name() + ") ");
              System::WriteLn( Ftree );
              LoadElem = (TLoadObj*) Topo->Get_NextObject();
            }
            PDElem = (TPDElement*) Topo->Get_Forward();
          }
        }
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while (( PDElem != NULL ) )
        {
          if ( ( PDElem )->IsIsolated )
          {
            System::Write( Ftree, "Isolated: " + (  PDElem )->ParentClass->get_myClass_name() + ( PDElem )->get_Name() );
            if ((PDElem)->HasSensorObj )
              System::Write( Ftree, " (Sensor: " + PDElem->SensorObj->ParentClass->get_myClass_name() + PDElem->SensorObj->get_Name() + "." + ") " );
            if ((PDElem)->HasControl )
            {
              pControlElem = (TDSSCktElement*) (PDElem)->ControlElementList.Get_First();
              while ( pControlElem != NULL )
              {                                // accommodate multiple controls on same branch
                System::Write( Ftree, " (Control: " + pControlElem->ParentClass->get_myClass_name() + "." + pControlElem->get_Name() + ") ");
                if ( ( ( pControlElem->DSSObjType & CLASSMASK ) ) == SWT_CONTROL )
                  nSwitches++;
                pControlElem = (TDSSCktElement*)PDElem->ControlElementList.Get_Next();
              }
            }
            if ((PDElem)->HasEnergyMeter )
              System::Write( Ftree, " (Meter: " + PDElem->MeterObj->get_Name() + ") " );
            System::WriteLn( Ftree );
            nIsolated++;
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
        nLoops = nLoops / 2;  // TODO, see if parallel lines also counted twice
        System::WriteLn( F, Format( "%d Levels Deep",  nLevels ));
        System::WriteLn( F, Format( "%d Loops",  nLoops ));
        System::WriteLn( F, Format( "%d Parallel PD elements",  nParallel ));
        System::WriteLn( F, Format( "%d Isolated PD components",  nIsolated ));
        System::WriteLn( F, Format( "%d Controlled Switches",  nSwitches ));
    /*  }
      __finally
      {*/
        CloseFile( F );
        CloseFile( Ftree );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
        ShowTreeView( TreeNm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowLineConstants( String Filenm, double Freq, int Units, double Rho )
    {
      TTextRec  F, F2;
      int       p = 0,
                i = 0, 
                j = 0;
      TLineGeometryObj* pElem = nullptr;
      TcMatrix  Z, YC;
      complex   ZS, 
                ZM, 
                Z1, 
                Z0;
      complex   YCM;
      double    w = 0.0,
                CS = 0.0, 
                CM = 0.0,
                C1 = 0.0, 
                C0 = 0.0,
                XCM = 0.0,
                CCM = 0.0;  // Common mode capacitance

      String LineCodesFileNm;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F, "LINE CONSTANTS" );
        System::WriteLn( F, Format( "Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m",  Freq, Rho ));
        System::Write( F, "Earth Model = " ); System::WriteLn( F, GetEarthModel( DefaultEarthModel ) );
        System::WriteLn( F );
        LineCodesFileNm = "LineConstantsCode.DSS";
        AssignFile( F2, LineCodesFileNm );
        Rewrite( F2 );
        IOResultToException();
        System::WriteLn( F2, "!--- OpenDSS Linecodes file generated from Show LINECONSTANTS command" );
        System::WriteLn( F2, Format( "!--- Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m",  Freq, Rho ));
        System::Write( F2, "!--- Earth Model = " ); System::WriteLn( F2, GetEarthModel( DefaultEarthModel ) );
        LineGeometryClass = (TLineGeometry*) DSSClassList[ActiveActor].Get( ClassNames[ActiveActor].Find( "LineGeometry" ) );
        ActiveEarthModel[ActiveActor] = DefaultEarthModel;
        p = ( (TDSSClass*) LineGeometryClass )->Get_First();
        while ( p > 0 )
        {
          pElem = (TLineGeometryObj*) ((TDSSClass*)LineGeometryClass)->GetActiveObj();
          try
          {
                    // Get impedances per unit length
            pElem->Set_RhoEarth(Rho);
            Z = *pElem->Get_Zmatrix(Freq,1.0, Units);
            YC = *pElem->Get_YCmatrix(Freq,1.0, Units);
          }
          catch( std::exception & E )
          {
            DoSimpleMsg( "Error computing line constants for LineGeometry." + ( (TDSSObject*) pElem )->get_Name() + "; Error message: " + (std::string) E.what(), 9934 );
          }
          System::WriteLn( F );
          System::WriteLn( F, "--------------------------------------------------" );
          System::Write( F, "Geometry Code = " ); System::WriteLn( F, ( (TDSSObject*) pElem )->get_Name() );
          System::WriteLn( F );
          System::Write( F, "R MATRIX, ohms per " ); System::WriteLn( F, LineUnitsStr( Units ) );
          for ( int stop = Z.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
            {
              System::Write( F, Format( "%.6g, ",  Z.GetElement( i, j ).re ));
            }
            System::WriteLn( F );
          }
          System::WriteLn( F );
          System::Write( F, "jX MATRIX, ohms per " ); System::WriteLn( F, LineUnitsStr( Units ) );
          for ( int stop = Z.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
            {
              System::Write( F, Format( "%.6g, ",  Z.GetElement( i, j ).im ));
            }
            System::WriteLn( F );
          }
          System::WriteLn( F );
          System::Write( F, "Susceptance (jB) MATRIX, S per " ); System::WriteLn( F, LineUnitsStr( Units ) );
          for ( int stop = YC.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
            {
              System::Write( F, Format( "%.6g, ",  YC.GetElement( i, j ).im ));
            }
            System::WriteLn( F );
          }
          w = double( Freq ) * TwoPi / 1.0E3;
          System::WriteLn( F );
          System::Write( F, "L MATRIX, mH per " ); System::WriteLn( F, LineUnitsStr( Units ) );
          for ( int stop = Z.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
            {
              System::Write( F, Format( "%.6g, ",  double( Z.GetElement( i, j ).im ) / w ));
            }
            System::WriteLn( F );
          }
          w = double( Freq ) * TwoPi / 1.0E9;
          System::WriteLn( F );
          System::Write( F, "C MATRIX, nF per " ); System::WriteLn( F, LineUnitsStr( Units ) );
          for ( int stop = YC.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
            {
              System::Write( F, Format( "%.6g, ",  double( YC.GetElement( i, j ).im ) / w ));
            }
            System::WriteLn( F );
          }

                /*System::Write DSS LineCode record*/
                //System::Writeln(F);
                //System::Writeln(F,'-------------------------------------------------------------------');
                //System::Writeln(F,'-------------------DSS Linecode Definition-------------------------');
                //System::Writeln(F,'-------------------------------------------------------------------');
          System::WriteLn( F2 );
          System::WriteLn( F2, Format("New Linecode.%s nphases=%d  Units=%s", pElem->get_Name().c_str(), Z.get_Norder(), LineUnitsStr(Units).c_str()));
          System::Write( F2, "~ Rmatrix=[" );
          for ( int stop = Z.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
              System::Write( F2, Format( "%.6g  ",  Z.GetElement( i, j ).re ));
            if ( i < Z.get_Norder() )
              System::Write( F2, '|' );
          }
          System::WriteLn( F2, ']' );
          System::Write( F2, "~ Xmatrix=[" );
          for ( int stop = Z.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
              System::Write( F2, Format( "%.6g  ",  Z.GetElement( i, j ).im ));
            if ( i < Z.get_Norder() )
              System::Write( F2, '|' );
          }
          System::WriteLn( F2, ']' );
          w = double( Freq ) * TwoPi / 1.0E9;
          System::Write( F2, "~ Cmatrix=[" );
          for ( int stop = YC.get_Norder(), i = 1; i <= stop; i++)
          {
            for ( int stop = i, j = 1; j <= stop; j++)
              System::Write( F2, Format( "%.6g  ",  double( YC.GetElement( i, j ).im ) / w ));
            if ( i < YC.get_Norder() )
              System::Write( F2, '|' );
          }
          System::WriteLn( F2, ']' );

                /*Add pos- and zero-sequence approximation here*/
                /*Kron reduce to 3 phases first*/
                /*Average diagonals and off-diagonals*/
          ZS = CZero;
          ZM = CZero;
          CS = 0.0;
          CM = 0.0;
          if ( Z.get_Norder() == 3 )
          {
            System::WriteLn( F );
            System::WriteLn( F, "-------------------------------------------------------------------" );
            System::WriteLn( F, "-------------------Equiv Symmetrical Component --------------------" );
            System::WriteLn( F, "-------------------------------------------------------------------" );
            System::WriteLn( F );
            for ( int stop = 3, i = 1; i <= stop; i++)
              caccum( ZS, Z.GetElement( i, i ) );
            for ( int stop = 3, i = 1; i <= stop; i++)
              for ( int stop = i - 1, j = 1; j <= stop; j++)
                caccum( ZM, Z.GetElement( i, j ) );
            Z1 = cdivreal( csub( ZS, ZM ), 3.0 );
            Z0 = cdivreal( cadd( cmulreal( ZM, 2.0 ), ZS ), 3.0 );
            w = double( Freq ) * TwoPi / 1000.0;
            System::WriteLn( F );
            System::Write( F, "Z1, ohms per " ); System::Write( F, LineUnitsStr( Units ) ); System::WriteLn( F, Format( " = %.6g + j %.6g (L1 = %.6g mH) ",  Z1.re, Z1.im, double( Z1.im ) / w ));
            System::Write( F, "Z0, ohms per " ); System::Write( F, LineUnitsStr( Units ) ); System::WriteLn( F, Format( " = %.6g + j %.6g (L0 = %.6g mH) ",  Z0.re, Z0.im, double( Z0.im ) / w ));
            System::WriteLn( F );

                   /*Compute Common Mode Series Impedance*/
            Z.Invert();
            YCM = CZero;
            for ( int stop = 3, i = 1; i <= stop; i++)    // Add up all elements of Z inverse
              for ( int stop = 3, j = 1; j <= stop; j++)
                caccum( YCM, Z.GetElement( i, j ) );
            XCM = cinv( YCM ).im;
            w = double( Freq ) * TwoPi / 1.0E9;
                   /*Capacitance*/
            for ( int stop = 3, i = 1; i <= stop; i++)
              CS = CS + YC.GetElement( i, i ).im;
            for ( int stop = 3, i = 1; i <= stop; i++)
              for ( int stop = i - 1, j = 1; j <= stop; j++)
                CM = CM + YC.GetElement( i, j ).im;
            C1 = ( CS - CM ) / 3.0 / w;   // nF
            C0 = ( CS + 2.0 * CM ) / 3.0 / w;

                   /*Compute Common Mode Shunt Capacitance*/
            YCM = CZero;
            for ( int stop = 3, i = 1; i <= stop; i++)    // Add up all elements of Z inverse
              for ( int stop = 3, j = 1; j <= stop; j++)
                caccum( YCM, YC.GetElement( i, j ) );
            CCM = double( YCM.im ) / w;
            System::Write( F, "C1, nF per " ); System::Write( F, LineUnitsStr( Units ) ); System::WriteLn( F, Format( " = %.6g",  C1 ));
            System::Write( F, "C0, nF per " ); System::Write( F, LineUnitsStr( Units ) ); System::WriteLn( F, Format( " = %.6g",  C0 ));
            System::WriteLn( F );
            w = Freq * TwoPi;
            System::WriteLn( F, "Surge Impedance:" );
            System::WriteLn( F, Format( "  Positive sequence = %.6g ohms",  sqrt( double( Z1.im ) / w / ( C1 * 1.0e-9) ) ));
            System::WriteLn( F, Format( "  Zero sequence     = %.6g ohms",  sqrt( double( Z0.im ) / w / ( C0 * 1.0e-9) ) ));
            System::WriteLn( F, Format( "  Common Mode       = %.6g ohms",  sqrt( XCM / w / ( CCM * 1.0e-9) ) ));
            System::WriteLn( F );
            System::WriteLn( F, "Propagation Velocity (Percent of speed of light):" );
            System::WriteLn( F, Format( "  Positive sequence = %.6g ",  double( 1.0 ) / ( sqrt( double( Z1.im ) / w * ( C1 * 1.0e-9) ) ) / 299792458.0 / To_per_Meter( Units ) * 100.0 ));
            System::WriteLn( F, Format( "  Zero sequence     = %.6g ",  double( 1.0 ) / ( sqrt( double( Z0.im ) / w * ( C0 * 1.0e-9) ) ) / 299792458.0 / To_per_Meter( Units ) * 100.0 ));
            System::WriteLn( F );
          }
          p = ((TDSSClass*)LineGeometryClass)->Get_Next();
        }
    /*  }
      __finally
      {*/
        CloseFile( F );
        CloseFile( F2 );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        if (AutoDisplayShowReport)
            FireOffEditor( LineCodesFileNm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowYPrim( String Filenm )
    {
      TTextRec F;
      pComplexArray cValues;
      int i = 0, j = 0;
      if ( ActiveCircuit[ActiveActor] != NULL )
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            if ( with0->get_FActiveCktElement() != NULL )
            {
              try
              {
                AssignFile( F, Filenm );
                Rewrite( F );
                IOResultToException();
                /*# with ActiveCktElement do */
                {
                  auto with1 = with0->get_FActiveCktElement();
                  System::Write( F, "Yprim of active circuit element: " ); System::Write( F, with1->ParentClass->get_myClass_name() ); System::Write( F, '.' ); System::WriteLn( F, with1->get_Name() );
                  System::WriteLn( F );
                  cValues = with1->GetYPrimValues( ALL_YPRIM );
                  if ( cValues != NULL )
                  {
                    System::WriteLn( F );
                    System::WriteLn( F, "G matrix (conductance), S" );
                    System::WriteLn( F );
                    for ( int stop = with1->Yorder, i = 1; i <= stop; i++)
                    {
                      for ( int stop = i, j = 1; j <= stop; j++)
                        System::Write( F, Format( "%13.10g ",  (cValues-1)[i + ( j - 1 ) * with1->Yorder].re ));
                      System::WriteLn( F );
                    }
                    System::WriteLn( F );
                    System::WriteLn( F, "jB matrix (Susceptance), S" );
                    System::WriteLn( F );
                    for ( int stop = with1->Yorder, i = 1; i <= stop; i++)
                    {
                      for ( int stop = i, j = 1; j <= stop; j++)
                        System::Write( F, Format( "%13.10g ", (cValues-1)[i + ( j - 1 ) * with1->Yorder].im ));
                      System::WriteLn( F );
                    }
                  }
                  else
                    System::WriteLn( F, "Yprim matrix is Nil" );
                }
    /* }
              __finally
              {*/
                CloseFile( F );
                if (AutoDisplayShowReport)
                    FireOffEditor( Filenm );
                ParserVars->Add( "@lastshowfile", Filenm );
              }
              catch (...)
              {
                  //
              }
            }
          }
        }
    }

    // shows how to retrieve the System Y in Triplet form



    void ShowY( String Filenm )
    {
      TTextRec F;
      klusparseset_t hY;
      unsigned int nNZ = 0, NBus = 0;
      unsigned int i = 0, Row = 0, col = 0;
      double re = 0.0, im = 0.0;
      unsigned int* ColIdx;
      unsigned int* RowIdx;
      complex* cVals;
      if ( ActiveCircuit[ActiveActor] == NULL )
        return;
      hY = ActiveCircuit[ActiveActor]->Solution->hY;
      if ( ! hY )
      {
        DoSimpleMsg( "Y Matrix not Built.", 222 );
        return;
      }
      // print lower triangle of G and B using new functions
      // this compresses the entries if necessary - no extra work if already solved
      FactorSparseMatrix( hY );
      GetNNZ( hY, &nNZ );
      GetSize( hY, &NBus ); // we should already know this
      try
      {
        ColIdx = new unsigned[nNZ];
        RowIdx = new unsigned[nNZ];
        cVals = new complex[nNZ];
        GetTripletMatrix( hY, nNZ, RowIdx, ColIdx, cVals );
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        System::WriteLn( F, "System Y Matrix (Lower Triangle by Columns)" );
        System::WriteLn( F );
        System::WriteLn( F, "  Row  Col               G               B" );
        System::WriteLn( F );

        // shows how to easily traverse the triplet format
        for ( int stop = nNZ - 1, i = 0; i <= stop; i++)
        {
          col = ColIdx[i] + 1;
          Row = RowIdx[i] + 1;
          if ( Row >= col )
          {
            re = cVals[i].re;
            im = cVals[i].im;
            System::WriteLn( F, Format( "[%4d,%4d] = %13.10g + j%13.10g",  Row, col, re, im ));
          }
        }
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowNodeCurrentSum( String Filenm )
    {


      TTextRec F;
      int       i = 0, 
                j = 0, 
                k = 0,
                Nref = 0;
      String    Bname = "",
                pctError = "";
      TDSSCktElement* pCktElement = nullptr;
      vector <double> MaxNodeCurrent;
      complex   Ctemp = CZero;

      double dTemp = 0.0;

      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor]->Solution do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          auto with2 = ActiveCircuit[ActiveActor]->Solution;
          {
            // Zero out the nodal current array
            for (i = 0; i <= with0->NumNodes; i++)
              with2->Currents[i] = CZero;
            // Make temp storage for max current at node
            MaxNodeCurrent.resize(with0->NumNodes + 1);
            for (i = 0; i <= with0->NumNodes; i++)
              MaxNodeCurrent[i] = 0.0;
            // Now Sum in each device current, keep track of the largest current at a node.
            pCktElement = (TDSSCktElement*) with0->CktElements.Get_First();
            while ( pCktElement != NULL )
            {
              if ( pCktElement->Get_Enabled() )
                /*# with pCktElement do */
                {
                  auto with1 = pCktElement;
                  with1->ComputeIterminal( ActiveActor );
                  if ( ( CLASSMASK & with1->DSSObjType ) == AUTOTRANS_ELEMENT )
                  {
                    k = 0;              /*Special for Autotransformer*/
                    for ( int stop = with1->Get_NTerms(), i = 0; i <= stop; i++)
                    {
                      for ( int stop = with1->Get_NPhases(), j = 0; j <= stop; j++)
                      {
                       
                        Ctemp = (with1->Iterminal)[k];
                        Nref = (with1->NodeRef)[k];
                        caccum(with2->Currents[Nref], Ctemp );  // Noderef=0 is OK
                        if ( cabs( Ctemp ) > MaxNodeCurrent[Nref] )
                          MaxNodeCurrent[Nref] = cabs( Ctemp );
                        k++;
                      }
                      k += with1->Get_NPhases();
                    }
                  }
                  else
                    for (i = 1; i <= with1->Yorder; i++)
                    {
                      Ctemp = with1->Iterminal[i - 1];
                      Nref = with1->NodeRef[i - 1];
                      caccum( with2->Currents[Nref], Ctemp );  // Noderef=0 is OK
                      if ( cabs( Ctemp ) > MaxNodeCurrent[Nref] )
                        MaxNodeCurrent[Nref] = cabs( Ctemp );
                    }
                }
              pCktElement = (TDSSCktElement*)with0->CktElements.Get_Next();
            }

            // Now write report
            SetMaxBusNameLength();
            MaxBusNameLength = MaxBusNameLength + 2;
            System::WriteLn( F );
            System::WriteLn( F, "Node Current Mismatch Report" );
            System::WriteLn( F );
            System::WriteLn( F );
            System::Write( F, Pad( "Bus,", MaxBusNameLength ) ); System::WriteLn( F, " Node, \"Current Sum (A)\", \"%error\", \"Max Current (A)\"" );

              // Ground Bus
            Nref = 0;
            dTemp = cabs( with2->Currents[Nref] );
            if ( ( MaxNodeCurrent[Nref] == 0.0 ) || ( MaxNodeCurrent[Nref] == dTemp ) )
              pctError = Format( "%10.1f",  0.0 );
            else
              pctError = Format( "%10.6f",  dTemp / MaxNodeCurrent[Nref] * 100.0 );
            Bname = Pad( "\"System Ground\"", MaxBusNameLength );
            System::WriteLn( F, Format( "%s, %2d, %10.5f,       %s, %10.5f",  Bname.c_str(), Nref, dTemp, pctError.c_str(), MaxNodeCurrent[Nref] ));
            for ( int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
            {
              for ( int stop = with0->Buses[i - 1]->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
              {
                Nref = with0->Buses[i - 1]->GetRef( j );
                dTemp = cabs( with2->Currents[Nref] );
                if ( ( MaxNodeCurrent[Nref] == 0.0 ) || ( MaxNodeCurrent[Nref] == dTemp ) )
                  pctError = Format( "%10.1f",  0.0 );
                else
                  pctError = Format( "%10.6f",  dTemp / MaxNodeCurrent[Nref] * 100.0 );
                if ( j == 1 )
                  Bname = Paddots( EncloseQuotes(with0->BusList.Get( i ) ), MaxBusNameLength );
                else
                  Bname = Pad( "\"   -\"", MaxBusNameLength );
                System::WriteLn( F, Format( "%s, %2d, %10.5f,       %s, %10.5f",  Bname.c_str(), with0->Buses[i - 1]->GetNum( j ), dTemp, pctError.c_str(), MaxNodeCurrent[Nref] ));
              }
            }
          }
        }
      /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
        MaxNodeCurrent.clear(); // Dispose of temp memory
      }
      catch (...)
      {
          //
      }
    }

    void ShowkVBaseMismatch( String Filenm )
    {
      TTextRec F;
      TLoadObj* pLoad;
      TGeneratorObj* pGen;
      TDSSBus* pBus;
      double BuskV = 0.0;
      String BusName;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();

            /*Check Loads*/
        if ( ActiveCircuit[ActiveActor]->Loads.get_myNumList() > 0 )
        {
          System::WriteLn( F );
          System::WriteLn( F, "!!!  LOAD VOLTAGE BASE MISMATCHES" );
          System::WriteLn( F );
        }
        pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
        while ( pLoad != NULL )
        {
               /*Find Bus To Which Load Connected*/
          //pBus = ActiveCircuit[ActiveActor]->Buses[( pLoad->BusIndex )->Terminals[1 - 1].BusRef - 1];
            pBus = ActiveCircuit[ActiveActor]->Buses[pLoad->BusIndex - 1];
          BusName = ActiveCircuit[ActiveActor]->BusList.Get( ( pLoad )->Terminals[1 - 1].BusRef );
          if ( pBus->kVBase != 0.0 )
          {
            if ( (((TDSSCktElement*)pLoad)->Get_NPhases() == 1 ) && ( pLoad->Connection == 0 ) )
            {
              if ( Abs( pLoad->kVLoadBase - pBus->kVBase ) > 0.10 * pBus->kVBase )
              {
                System::WriteLn( F, Format( "!!!!! Voltage Base Mismatch, Load.%s.kV=%.6g, Bus %s LN kvBase = %.3g", (pLoad)->get_Name().c_str(), pLoad->kVLoadBase, (pLoad)->GetBus(1).c_str(), pBus->kVBase));
                System::WriteLn( F, Format( "!setkvbase %s kVLN=%.3g",  BusName.c_str(), pLoad->kVLoadBase ));
                System::WriteLn( F, Format( "!Load.%s.kV=%.3g", (pLoad)->get_Name().c_str(), pBus->kVBase ));
              }
            }
            else
            {
              BuskV = pBus->kVBase / SQRT3;              
              if ( Abs( pLoad->kVLoadBase - BuskV ) > 0.10 * BuskV )
              {
                System::WriteLn( F, Format( "!!!!! Voltage Base Mismatch, Load.%s.kV=%.6g, Bus %s kvBase = %.3g", (pLoad)->get_Name().c_str(), pLoad->kVLoadBase, pLoad->GetBus(1).c_str(), BuskV));
                System::WriteLn( F, Format( "!setkvbase %s kVLL=%.3g",  BusName.c_str(), pLoad->kVLoadBase ));
                System::WriteLn( F, Format( "!Load.%s.kV=%.3g", pLoad->get_Name().c_str(), BuskV));
              }
            }
          }
          pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
        }


            /*Check Generators*/
        if ( ActiveCircuit[ActiveActor]->Generators.get_myNumList() > 0 )
        {
          System::WriteLn( F );
          System::WriteLn( F, "!!!  GENERATOR VOLTAGE BASE MISMATCHES" );
          System::WriteLn( F );
        }
        pGen = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_First();
        while ( pGen != NULL )
        {
               /*Find Bus To Which Generator Connected*/
          pBus = ActiveCircuit[ActiveActor]->Buses[( pGen )->Terminals[1 - 1].BusRef - 1];
          BusName = ActiveCircuit[ActiveActor]->BusList.Get((pGen)->Terminals[1 - 1].BusRef );
          if ( pBus->kVBase != 0.0 )
          {
            if ( (((TDSSCktElement*)pGen)->Get_NPhases() == 1 ) && ( pGen->Connection == 0 ) )
            {
              if ( Abs( pGen->GenVars.kVGeneratorBase - pBus->kVBase ) > 0.10 * pBus->kVBase )
              {
                System::WriteLn( F, Format( "!!! Voltage Base Mismatch, Generator.%s.kV=%.6g, Bus %s LN kvBase = %.3g", pGen->get_Name().c_str(), pGen->GenVars.kVGeneratorBase, pGen->GetBus(1).c_str(), pBus->kVBase));
                System::WriteLn( F, Format( "!setkvbase %s kVLN=%.3g",  BusName.c_str(), pGen->GenVars.kVGeneratorBase ));
                System::WriteLn( F, Format( "!Generator.%s.kV=%.3g", pGen->get_Name().c_str(), pBus->kVBase));
              }
            }
            else
            {
              BuskV = pBus->kVBase * SQRT3;
              if ( Abs( pGen->GenVars.kVGeneratorBase - BuskV ) > 0.10 * BuskV )
              {
                System::WriteLn( F, Format( "!!! Voltage Base Mismatch, Generator.%s.kV=%.6g, Bus %s kvBase = %.2g", pGen->get_Name().c_str(), pGen->GenVars.kVGeneratorBase, pGen->GetBus(1).c_str(), BuskV));
                System::WriteLn( F, Format( "!setkvbase %s kVLL=%.2g",  BusName.c_str(), pGen->GenVars.kVGeneratorBase ));
                System::WriteLn( F, Format( "!Generator.%s.kV=%.2g", pGen->get_Name().c_str(), BuskV));
              }
            }
          }
          pGen = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_Next();
        }
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowDeltaV( String Filenm )
    {
      TTextRec F;
      TDSSCktElement* pElem;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        SetMaxDeviceNameLength();
        System::WriteLn( F );
        System::WriteLn( F, "VOLTAGES ACROSS CIRCUIT ELEMENTS WITH 2 TERMINALS" );
        System::WriteLn( F );
        System::WriteLn( F, "Source Elements" );
        System::WriteLn( F );
        System::Write( F, Pad( "Element,", MaxDeviceNameLength ) ); System::WriteLn( F, " Conductor,     Volts,   Percent,           kVBase,  Angle" );
        System::WriteLn( F );


             // SOURCES first
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() && ( pElem->Get_NTerms() == 2 ) )
          {
            WriteElementDeltaVoltages( F, pElem );
            System::WriteLn( F );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }
        System::WriteLn( F );
        System::WriteLn( F, "Power Delivery Elements" );
        System::WriteLn( F );
        System::Write( F, Pad( "Element,", MaxDeviceNameLength ) ); System::WriteLn( F, " Conductor,     Volts,   Percent,           kVBase,  Angle" );
        System::WriteLn( F );


             // PDELEMENTS next
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() && ( pElem->Get_NTerms() == 2 ) )
          {
            WriteElementDeltaVoltages( F, pElem );
            System::WriteLn( F );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
        System::WriteLn( F, "= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =" );
        System::WriteLn( F );
        System::WriteLn( F, "Power Conversion Elements" );
        System::WriteLn( F );
        System::Write( F, Pad( "Element,", MaxDeviceNameLength ) ); System::WriteLn( F, " Conductor,     Volts,   Percent,           kVBase,  Angle" );
        System::WriteLn( F );

             // PCELEMENTS next
        pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First();
        while ( pElem != NULL )
        {
          if ( pElem->Get_Enabled() && ( pElem->Get_NTerms() == 2 ) )
          {
            WriteElementDeltaVoltages( F, pElem );
            System::WriteLn( F );
          }
          pElem = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
        }
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }


    void ShowControlledElements( String Filenm )
    {
      TTextRec F;
      TPDElement* PDElem;
      TDSSCktElement* pctrlelem;
      int i = 0;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First();
        while ( PDElem != NULL )
        {
          if ( ( (TDSSCktElement*) PDElem )->HasControl )
          {
            /*# with PDElem do */
            System::Write( F, ( (TDSSCktElement*) PDElem )->ParentClass->get_myClass_name() + "." + ( (TDSSCktElement*) PDElem )->get_Name() );
            for ( int stop = ((TDSSCktElement*)PDElem)->ControlElementList.get_myNumList(), i = 1; i <= stop; i++)
            {
              pctrlelem = (TDSSCktElement*) ((TDSSCktElement*)PDElem)->ControlElementList.Get( i );
              /*# with pctrlelem do */
              System::Write( F, ", " + pctrlelem->ParentClass->get_myClass_name() + "." + pctrlelem->get_Name() + " ");
            }
            System::WriteLn( F );
          }
          PDElem = (TPDElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next();
        }
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    void ShowResult( String Filenm )
    {
      TTextRec F;
      try
      {
        AssignFile( F, Filenm );
        Rewrite( F );
        IOResultToException();
        ParserVars->Lookup( "@result" );
        System::WriteLn( F, ParserVars->Get_Value() );
        GlobalResult = Filenm;
    /* }
      __finally
      {*/
        CloseFile( F );
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }


    void ShowEventLog( String Filenm )
    {
      try
      {
    //    EventStrings[ActiveActor].SaveToFile( Filenm );
          // replacing with an equivalent in C++
          TTextRec F;
          int i;
          AssignFile(F, Filenm);
          Rewrite(F);
          IOResultToException();

          for (i = 0; i < EventStrings[ActiveActor].size(); i++)
          {
              System::WriteLn(F, EventStrings[ActiveActor][i]);
          }
          CloseFile(F);

          GlobalResult = Filenm;
    /* }
      __finally
      {*/
        if (AutoDisplayShowReport)
            FireOffEditor( Filenm );
        ParserVars->Add( "@lastshowfile", Filenm );
      }
      catch (...)
      {
          //
      }
    }

    //-----Shows the list of generators converted from PV to PQ bus during an NCIM solution step --------------
    void ShowPV2PQGen(String Filenm)
    {
        auto            with0 = ActiveCircuit[ActiveActor];
        auto            with1 = with0->Solution;
        TGeneratorObj* pGen = nullptr;
        int             NumGens = with0->Generators.get_myNumList(),
            BIdx = 0;

        TTextRec F = {};
        try
        {
            AssignFile(F, Filenm);
            Rewrite(F);
            IOResultToException();
            Write(F, "------------------------------------------------------------------------------");
            Write(F, CRLF);
            Write(F, "LIST OF GENERATORS CONVERTED FROM PV TO PQ BUS DURING THE LAST SOLUTION (NCIM)");
            Write(F, CRLF);
            Write(F, "------------------------------------------------------------------------------");
            Write(F, CRLF);
            Write(F, CRLF);
            if (NumGens > 0)
            {
                pGen = (TGeneratorObj*)with0->Generators.Get_First();
                for (int i = 0; i < NumGens; i++)
                {
                    if (pGen->Get_Enabled())
                    {
                        BIdx = Find(&with1->PV2PQList, i);
                        if (BIdx >= 0)
                        {
                            Write(F, "Generator." + pGen->get_Name());
                            Write(F, CRLF);
                        }
                    }
                    pGen = (TGeneratorObj*)with0->Generators.Get_Next();
                }
            }
            CloseFile(F);
            GlobalResult = Filenm;
            if (AutoDisplayShowReport)
                FireOffEditor(Filenm);
            ParserVars->Add("@lastshowfile", Filenm);
        }
        catch (std::exception& E)
        {
            DoSimpleMsg(String("Error writing file ") + Filenm + CRLF + (std::string)E.what(), 438);
        }
    }

    void ShowResults_initialization()
    {
      MaxDeviceNameLength = 30;
      MaxBusNameLength = 12;
    }

    class ShowResults_unit
    {
    public:
    ShowResults_unit()
    {
      ShowResults_initialization();
    }
    };
    ShowResults_unit _ShowResults_unit;

}// namespace ShowResults