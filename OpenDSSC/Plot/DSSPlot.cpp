

#pragma hdrstop

#include "DSSPlot.h"



#include "DSSGraph.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "generator.h"
#include "EnergyMeter.h"
#include "GICLine.h"
#include "Utilities.h"
#include "LoadShape.h"
#include "TempShape.h"
#include "PriceShape.h"
#include "Sysutils.h"
#include "math.h"
//#include "DlgPlotOptions.h"
#include "Bus.h"
#include "Monitor.h"
#include "Capacitor.h"
#include "PVsystem.h"
#include "Storage.h"
#include "RegControl.h"
#include "fuse.h"
#include "Recloser.h"
#include "Relay.h"



#include "System.h"

namespace DSSPlot
{

    TDSSPlot* DSSPlotObj = NULL;
    int SinglePhLineStyle = 0;
    int ThreePhLineStyle = 0;



    const double Eps = 0.002;


    const int DSSG_LINECLASS = 1;
    const int DSSG_CIRCLECLASS = 2;
    const int DSSG_TEXTCLASS = 3;
    const int DSSG_MARKERCLASS = 4;
    const int DSSG_CURVECLASS = 5;
    const int DSSG_SUBSTCLASS = 6;


    vector<string> BusLabels;

    /*
      TDSSPlot
    */


    void AllocateBusLabels()
    {
        int i = 0;
        BusLabels.resize(ActiveCircuit[ActiveActor]->NumBuses);
        for (int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
            BusLabels[i - 1] = "";
    }


    void FreeBusLabels()
    {
        int i = 0;
        /* Get rid of string pointed to by each element */
        for (int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
            BusLabels[i - 1] = "";
       BusLabels.resize(0);
    }


    int TDSSPlot::GetAutoColor(double Scale)

        /* One way to get a gradient */
    {
        int result;
        if (Scale < TriColorMid)
            result = Color1;
        else
            if (Scale < TriColorMax)
                result = Color2;
            else
                result = Color3;
        return result;
    }


    int TDSSPlot::GetColor()
    {
        int result = 0;
        TDSSBus* pBus;
        double Factor = 0.0;
        int i = 0, j = 0;
        switch (PlotType)
        {
        case ptCircuitplot: case ptdaisyplot: case ptGeneralCircuitPlot:
            switch (Quantity)
            {
            case pqVoltage:
            {
                pBus = ActiveCircuit[ActiveActor]->Buses[Bus2Idx];
                if (ActiveCircuit[ActiveActor]->Issolved && (pBus->kVBase > 0.0))
                {
                    /* Find min phase voltage at bus - check nodes 1..3 */
                    Factor = ActiveCircuit[ActiveActor]->NormalMaxVolts;
                    for (int stop = pBus->FNumNodesThisBus, i = 1; i <= stop; i++)
                    {
                        j = pBus->GetNum(i);
                        if ((j > 0) && (j <= 3))
                            Factor = min<double>(Factor, double(0.001) * cabs(ActiveCircuit[ActiveActor]->Solution->NodeV[pBus->GetRef(i)]) / pBus->kVBase);
                    }
                    if (Factor > ActiveCircuit[ActiveActor]->NormalMinVolts)
                        result = Color1;
                    else
                        if (Factor > ActiveCircuit[ActiveActor]->EmergMinVolts)
                            result = Color2;
                        else
                            result = Color3;
                }
                else
                    result = Color1;
            }
            break;
            case pqCurrent:
            {
                result = Color1;
                if (pLine->NormAmps > 0.0)
                    if (MaxCurrent() > pLine->NormAmps)
                        result = Color3;
            }
            break;
            case pqPower:
                result = Color1;
                break;
            case pqLosses:
                result = Color1;
                break;
            case pqCapacity:
                result = Color1;
                break;
            default: /* Case Quantity */
                result = Color1; // Default to black
                if ((double(abs(pLine->GeneralPlotQuantity)) / MaxScale) > 0.99)
                    result = Color2;
            }
            break;
        default: /* Case Plottype */
            result = Color1; // Default to black
        }
        return result;
    }


    void TDSSPlot::AddBusMarkers()
    {
        TBusMarker* BusMarker   = nullptr;
        TDSSBus*    Bus         = nullptr;
        int i = 0;

        for (int stop = ActiveCircuit[ActiveActor]->BusMarkerList.size() - 1, i = 0; i <= stop; i++)
        {
            BusMarker = (TBusMarker*) ActiveCircuit[ActiveActor]->BusMarkerList[i];
            Bus1Idx = ActiveCircuit[ActiveActor]->BusList.Find(BusMarker->BusName);
            if (Bus1Idx > 0)
                /*# with BusMarker do */
            {
                auto with0 = BusMarker;
                Bus = ActiveCircuit[ActiveActor]->Buses[Bus1Idx];
                if (Bus->CoordDefined)
                    AddNewMarker(Bus->x, Bus->y, with0->AddMarkerColor, with0->AddMarkerCode, with0->AddMarkerSize);
                else
                    DoSimpleMsg("Bus Coordinates not defined for bus " + with0->BusName, 28709);
            }
        }
    }


    bool TDSSPlot::CoordinateSame(int i1, int i2)
    {
        bool result = false;
        result = false;
        if ((i1 == 0) || (i2 == 0))
            return result;
        try
        { /* Trap Divide by zero error */
          /*# with ActiveCircuit[ActiveActor] do */
            {
                auto with0 = ActiveCircuit[ActiveActor];
                if ((abs(1.0 - abs(with0->Buses[i1 - 1]->x / with0->Buses[i2 - 1]->x)) < Eps) && (abs(1.0 - abs(with0->Buses[i1 - 1]->y / with0->Buses[i2 - 1]->y)) < Eps))
                    result = true;
                else
                    result = false;
            }
        }
        catch (...)
        {
            result = false; /* Likely a divide by zero error, ignore */
        }
        return result;
    }

    TDSSPlot::TDSSPlot()
        : ActiveColorIdx(0),
        Bus1Idx(0),
        Bus2Idx(0),
        FMaxLineThickness(0),
        PlotType(ptAutoAddLogPlot),
        MatrixType(pIncMatrix),
        MaxScale(0.0),
        MinScale(0.0),
        Dots(false),
        Labels(false),
        ShowLoops(false),
        ShowSubs(false),
        Quantity(pqVoltage),
        ValueIndex(0),
        MarkerIdx(0),
        PhasesToPlot(0),
        ProfileScale(0),
        TriColorMax(0.0),
        TriColorMid(0.0),
        MaxScaleIsSpecified(false),
        MinScaleIsSpecified(false)
    {
        SetDefaults();
        DaisyBusList.resize(0);
        /* Initialize Plotting DLL */
        // --deprecated --- DSSGraphInit(@CallBackRoutines);  // send a pointer to the DSS Callback routines struct
        PhasesToPlot = PROFILE3PH;
        ProfileScale = PROFILEPUKM;
    }

    TDSSPlot::~TDSSPlot()
    {
        // todo check:  inherited::Destroy();
    }


    void TDSSPlot::DoAutoAddPlot()
    {
        int Color1Save = Color1;
        Dots = false;
        Quantity = pqNone;
        Color1 = 0x00FF0000;
        DoCircuitPlot();
        Color1 = Color1Save;
        DoGeneralPlot();
    }


    void TDSSPlot::DoBusLabel(const int Idx, const String BusLabel)
    {
        /* Only label a bus once */
        if (Idx > 0)
            if (Length(BusLabels[Idx - 1]) == 0)
                switch (PlotType)
                {
                case ptMeterZones:
                    BusLabels[Idx - 1] = BusLabel + "(" + FeederName + ")";
                    break;
                default:
                    BusLabels[Idx - 1] = BusLabel;
                }
    }


    void TDSSPlot::DoBusLabels(const int Idx1, const int Idx2)
    {
        if (CoordinateSame(Idx1, Idx2))
        {
            /* Special label for overlapping labels */
            BusLabels[Idx1 - 1] = ""; // Force label to change
            BusLabels[Idx2 - 1] = "";
            DoBusLabel(Idx1, ActiveCircuit[ActiveActor]->BusList.Get(Idx1) + "/" + ActiveCircuit[ActiveActor]->BusList.Get(Idx2));
        }
        else
        {
            DoBusLabel(Idx1, ActiveCircuit[ActiveActor]->BusList.Get(Idx1));
            DoBusLabel(Idx2, ActiveCircuit[ActiveActor]->BusList.Get(Idx2));
        }
    }


    double MaxGICCurrent(TGICLineObj* pGICLine)
    {
        double result = 0.0;
        int iGIC = 0;
        pGICLine->ComputeIterminal(ActiveActor); // load element Iterminal buffer

        /* ******************  Code for GICLines ************************** */

           /* Draw the lines */
        result = 0.0;
        for (int stop = pGICLine->Fnphases, iGIC = 1; iGIC <= stop; iGIC++)
            if (cabs(pGICLine->Iterminal[iGIC - 1]) > result)
                result = cabs(pGICLine->Iterminal[iGIC - 1]);
        return result;
    }


    void TDSSPlot::DoCircuitPlot()
    {
        int             LineStyleType = 0;
        int             GICThickness = 0;
        TGICLineObj*    pGICLine = new TGICLineObj();
        TGICLine*       pGICLineClass = new TGICLine();


        /* ******************  Code for GICLines ************************** */
        pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
        while (pLine != NULL)
            /*# with ActiveCircuit[ActiveActor] do */
        {
            auto with0 = ActiveCircuit[ActiveActor];
            {
                if (pLine->FEnabled)
                {
                    pLine->Drawn = true;
                    // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
                    // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
                    with0->Set_ActiveCktElement(pLine);
                    Bus1Idx = pLine->Terminals[1 - 1].BusRef - 1;
                    Bus2Idx = pLine->Terminals[2 - 1].BusRef - 1;
                    if (with0->Buses[Bus1Idx]->CoordDefined && with0->Buses[Bus2Idx]->CoordDefined)
                    {
                        if (pLine->IsSwitch)
                            AddNewLine(with0->Buses[Bus1Idx]->x, with0->Buses[Bus1Idx]->y, with0->Buses[Bus2Idx]->x, with0->Buses[Bus2Idx]->y, 0x000000, 1, Style(1), Dots, "Line." + pLine->LName, with0->MarkSwitches, with0->SwitchMarkerCode, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                        else
                            if (pLine->IsIsolated)
                                AddNewLine(with0->Buses[Bus1Idx]->x, with0->Buses[Bus1Idx]->y, with0->Buses[Bus2Idx]->x, with0->Buses[Bus2Idx]->y, 0x00FF00FF, 3, Style(1), Dots, "Line." + pLine->LName, with0->MarkSwitches, with0->SwitchMarkerCode, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                            else
                            {
                                if (pLine->Fnphases == 1)
                                    LineStyleType = Style(SinglePhLineStyle);
                                else
                                    LineStyleType = Style(ThreePhLineStyle);
                                AddNewLine(with0->Buses[Bus1Idx]->x, with0->Buses[Bus1Idx]->y, with0->Buses[Bus2Idx]->x, with0->Buses[Bus2Idx]->y, GetColor(), Thickness(), LineStyleType, Dots, "Line." + pLine->LName, false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                            }
                        if (Labels)
                            DoBusLabels(Bus1Idx + 1, Bus2Idx + 1);
                    }
                }
                pLine = (TLineObj*) with0->Lines.Get_Next();
            }
        }

        /* ******************  Code for GICLines ************************** */
        pGICLineClass   = (TGICLine*) GetDSSClassPtr("GICLine");
        pGICLine        = (TGICLineObj*) pGICLineClass->ElementList.Get_First();

        while (pGICLine != nullptr)
            /*# with ActiveCircuit[ActiveActor] do */
        {
            auto with1 = ActiveCircuit[ActiveActor];
            {
                if (pGICLine->FEnabled)
                {
                    // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
                    // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
                    with1->Set_ActiveCktElement(pGICLine);
                    Bus1Idx = pGICLine->Terminals[1 - 1].BusRef - 1;
                    Bus2Idx = pGICLine->Terminals[2 - 1].BusRef - 1;
                    if (with1->Buses[Bus1Idx]->CoordDefined && with1->Buses[Bus2Idx]->CoordDefined)
                    {
                        if (pGICLine->Fnphases == 1)
                            LineStyleType = Style(SinglePhLineStyle);
                        else
                            LineStyleType = Style(ThreePhLineStyle);
                        GICThickness = min<int>(7, Round(5.0 * (MaxGICCurrent(pGICLine) / MaxScale)));
                        AddNewLine(with1->Buses[Bus1Idx]->x, with1->Buses[Bus1Idx]->y, with1->Buses[Bus2Idx]->x, with1->Buses[Bus2Idx]->y, Color1, GICThickness, LineStyleType, Dots, "GICLine." + pGICLine->LName, false, 0, with1->NodeMarkerCode, with1->NodeMarkerWidth);
                        if (Labels)
                            DoBusLabels(Bus1Idx + 1, Bus2Idx + 1);
                    }
                }
                pGICLine = (TGICLineObj*) pGICLineClass->ElementList.Get_Next();
            }
        }

        /* ******************  Code for Transformers ************************** */
        pTransf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_First();

        while (pTransf != nullptr)
            /*# with ActiveCircuit[ActiveActor] do */
        {
            auto with2 = ActiveCircuit[ActiveActor];
            {
                if (pTransf->FEnabled)
                {
                    with2->Set_ActiveCktElement(pTransf);
                    Bus1Idx = pTransf->Terminals[1 - 1].BusRef - 1;
                    Bus2Idx = pTransf->Terminals[2 - 1].BusRef - 1;
                    if (with2->Buses[Bus1Idx]->CoordDefined && with2->Buses[Bus2Idx]->CoordDefined)
                        AddNewLine(with2->Buses[Bus1Idx]->x, with2->Buses[Bus1Idx]->y, with2->Buses[Bus2Idx]->x, with2->Buses[Bus2Idx]->y, 0x00808080, 3, Style(1), Dots, "transformer." + pTransf->LName, false, 0, with2->NodeMarkerCode, with2->NodeMarkerWidth);
                }
                pTransf = (TTransfObj*) with2->Transformers.Get_Next();
            }
        }

        /* ******************  Code for special Bus Markers ************************** */

     //   AddBusMarkers;
    }


    int GenPlotItemCompare(void* Item1, void* Item2)
    {
        int result = 0;
        double Test = 0.0;
        Test = ((TGenPlotItem*)Item1)->Value - ((TGenPlotItem*)Item2)->Value;
        if (Test < 0.0)
            result = -1;
        else
            if (Test > 0.0)
                result = 1;
            else
                result = 0;
        return result;
    }

    void TDSSPlot::DoGeneralPlot()
    {
        double      MaxValue = 0.0, 
                    MinValue = 0.0, 
                    Value = 0.0, 
                    Diff = 0.0;
        TTextRec    F = {};
        String      Line = "", 
                    FieldName = "";
        int         Idx = 0, 
                    i = 0;
        TGenPlotItemList   GenPlotItems;
        TGenPlotItem*       GenPlotItem = nullptr;

        try
        {
            try
            {
                AssignFile(F, ObjectName);
                Reset(F);
                IOResultToException();
                ReadLn(F, Line); // Get FieldName
                /*# with AuxParser[ActiveActor] do */
                {
                    auto with0 = AuxParser[ActiveActor];
                    {
                        with0->set_FAutoIncrement(false);
                        with0->set_delimchars(",=\x09"); /* Redefine delimiters */
                        with0->SetCmdString(Line);
                        with0->GetNextParam(); /* Bus Name */
                        for (int stop = ValueIndex, i = 1; i <= stop; i++)
                            with0->GetNextParam(); /* Skip to parameter wanted */
                        FieldName = with0->MakeString_(); /* Get field name */
                    }
                }

                /* Find min and max */
                MaxValue = -1.0E50;
                MinValue = 1.0E50;
                GenPlotItems = TGenPlotItemList();
                while (!Eof(F))
                {
                    ReadLn(F, Line);
                    if (Line.size() > 0)
                        /*# with AuxParser[ActiveActor] do */
                    {
                        auto with1 = AuxParser[ActiveActor];
                        {
                            with1->SetCmdString(Line); // Load up AuxParser
                            with1->GetNextParam(); /* Bus Name */
                            GenPlotItem = new TGenPlotItem;
                            GenPlotItem->Name = with1->MakeString_(); // Bus Name
                            for (int stop = ValueIndex, i = 1; i <= stop; i++)
                                with1->GetNextParam(); // Skip to desired field
                            if (with1->MakeString_().size() > 0)
                            { /* Ignore empty fields */
                                Value = with1->MakeDouble_();
                                MaxValue = max<double>(Value, MaxValue);
                                MinValue = min<double>(Value, MinValue);
                                GenPlotItem->Value = Value;
                            }
                            GenPlotItems.push_back(GenPlotItem);
                        }
                    }
                } /* WHILE */

                   /* Do some sanity checking on the numbers.  Don't want to include negative numbers in autoadd plot */
                if (PlotType == ptAutoAddLogPlot)
                {
                    if (MinValue < 0.0)
                        MinValue = 0.0;
                    if (MaxValue < 0.0)
                        MaxValue = 0.0;
                }
                if (MaxScaleIsSpecified)
                    MaxValue = MaxScale; // Override with user specified value
                if (MinScaleIsSpecified)
                    MinValue = MinScale; // Override with user specified value
                Diff = MaxValue - MinValue;
                if (Diff == 0.0)
                    Diff = MaxValue;
                if (Diff == 0.0)
                    Diff = 1.0; // Everything is zero

                // Sort min to max and plot
                sort(GenPlotItems.begin(), GenPlotItems.end());
                //GenPlotItems-> (GenPlotItemCompare);
                // sorts using user-written routine
                Set_ChartCaption(Format("%s, Max=%-.3g ", FieldName.c_str(), MaxValue));
                for (int stop = GenPlotItems.size() - 1, i = 0; i <= stop; i++)
                {
                    GenPlotItem = (TGenPlotItem*) GenPlotItems[i];
                    Idx = ActiveCircuit[ActiveActor]->BusList.Find(GenPlotItem->Name);
                    if (Idx > 0)
                        /*# with ActiveCircuit[ActiveActor].Buses^[Idx] do */
                    {
                        auto with3 = ActiveCircuit[ActiveActor]->Buses[Idx];
                        if (with3->CoordDefined)
                        {
                            switch (PlotType)
                            {
                            case ptGeneralDataPlot:
                                AddNewMarker(with3->x, with3->y, InterpolateGradientColor(Color1, Color2, IntPower((GenPlotItem->Value - MinValue) / Diff, 1)), MarkerIdx, ActiveCircuit[ActiveActor]->NodeMarkerWidth);
                                break;
                            case ptAutoAddLogPlot:
                                AddNewMarker(with3->x, with3->y, GetAutoColor((GenPlotItem->Value - MinValue) / Diff), MarkerIdx, ActiveCircuit[ActiveActor]->NodeMarkerWidth);
                                break;
                            default:
                                break;
                            }
                            if (Labels)
                                DoBusLabel(Idx, ActiveCircuit[ActiveActor]->BusList.Get(Idx));
                        }
                    }
                } /* WHILE */
            }
            catch (exception& E)
            {
                DoSimpleMsg(String("Error opening \"") + ObjectName + "\": " + (std::string)E.what(), 190);
            }
        }
        catch (...)
        {
        }

        CloseFile(F);
        GenPlotItems.resize(0);
    }


    void TDSSPlot::DoTheDaisies()
    {
        TGeneratorObj*  pGen = nullptr;
        vector<int>     BusCount;
        int             i = 0, 
                        j = 0, 
                        Idx = 0;
        double          Xc = 0.0, 
                        Yc = 0.0, 
                        Radius = 0.0, 
                        Angle = 0.0, 
                        StartAngle = 0.0;

        TDSSGraphProperties* ActiveGraphProps = new TDSSGraphProperties;

        BusCount.resize(ActiveCircuit[ActiveActor]->NumBuses);
        if (DaisyBusList.empty())
        {
            /* If Daisy Bus List not filled, then fill it with Generator Buses by default */
            pGen = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_First();
            while (pGen != NULL)
            {
                if (pGen->FEnabled)
                {
                    DaisyBusList.push_back(StripExtension(pGen->GetBus(1)));
                }
                pGen = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_Next();
            }
        }

        /* Count the number of Objects at each bus */
        for (int stop = DaisyBusList.size(), i = 0; i < stop; i++)
        {
            Idx = ActiveCircuit[ActiveActor]->BusList.Find(DaisyBusList[i]);
            if (Idx > 0)
                BusCount[Idx - 1]++;
        }
        //Randomize;

        /* Draw the generators in */
        Get_Properties(ActiveGraphProps); // Get active graph properties
        Radius = 0.005 * DaisySize * (ActiveGraphProps->Xmax - ActiveGraphProps->Xmin);
        for (int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
        {
            if ((BusCount[i - 1] > 0) && ActiveCircuit[ActiveActor]->Buses[i - 1]->CoordDefined)
            {
                StartAngle = TwoPi; /* * Random */
                Angle = (double(TwoPi) / BusCount[i - 1]); // Radians
                for (int stop = BusCount[i - 1], j = 1; j <= stop; j++)
                {
                    Xc = ActiveCircuit[ActiveActor]->Buses[i - 1]->x + 2.0 * Radius * cos(Angle * (j - 1) + StartAngle);
                    Yc = ActiveCircuit[ActiveActor]->Buses[i - 1]->y + 2.0 * Radius * sin(Angle * (j - 1) + StartAngle);
                    AddNewLine(ActiveCircuit[ActiveActor]->Buses[i - 1]->x, ActiveCircuit[ActiveActor]->Buses[i - 1]->y, Xc, Yc, clRed, 1, 0, false, "Gen", false, 0, 0, 0);
                    AddNewCircle(Xc, Yc, Radius, clRed, clYellow);
                }
            }
        }

        /* Put Labels on */
        if (Labels)
            for (int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
                if ((BusCount[i - 1] > 0) && ActiveCircuit[ActiveActor]->Buses[i - 1]->CoordDefined)
                    DoBusLabel(i, ActiveCircuit[ActiveActor]->BusList.Get(i));
        BusCount.resize(0); /* Clean up allocated memory */
    }

    void DrawMeterZoneLine(TColor clr, const String Nam, int Idx1, int Idx2, TPenStyle LineStyleType)
    { /* Local proc */
        if (ActiveCircuit[ActiveActor]->Buses[Idx1]->CoordDefined && ActiveCircuit[ActiveActor]->Buses[Idx2]->CoordDefined)
        {
            AddNewLine(ActiveCircuit[ActiveActor]->Buses[Idx1]->x, ActiveCircuit[ActiveActor]->Buses[Idx1]->y, ActiveCircuit[ActiveActor]->Buses[Idx2]->x, ActiveCircuit[ActiveActor]->Buses[Idx2]->y, clr, 1, LineStyleType, 0, String("Line.") + Nam, false, 0, 0, 0);
 /*           if (Labels)
                DoBusLabels(Idx1, Idx2);*/
        }
    }


    void TDSSPlot::DoMeterZonePlot()

        /* Draws feeder lines using the meter zones only
          Each feeder is drawn in a different color
          */
    {
        TEnergyMeterObj*    pMeter = nullptr;
        int                 hMeter = 0, 
                            Idx1 = 0, 
                            Idx2 = 0,
                            FdrColor = 0;
        TPenStyle           LineStyleType = 0;
        TLineObj*           LoopLine = nullptr;
        String              S = "";


        /* --------------------------------------------------------------------------------- */
        hMeter = EnergyMeterClass[ActiveActor]->Get_First();
        ActiveColorIdx = 0; /* Nextcolor does an Inc() */
        while (hMeter > 0)
        {
            if (Length(ObjectName) > 0) // look for a specific object {Else Draw Them All}
                if (CompareText(ObjectName, ((TDSSCktElement*) ActiveDSSObject[ActiveActor])->LName) != 0)
                {
                    hMeter = EnergyMeterClass[ActiveActor]->Get_Next();
                    continue;
                }
            pMeter = (TEnergyMeterObj*)ActiveDSSObject[ActiveActor];
            if (!pMeter->FEnabled) // skip disabled energymeters
            {
                hMeter = EnergyMeterClass[ActiveActor]->Get_Next();
                continue;
            }
            FeederName = pMeter->LName;
            pLine = (TLineObj*) pMeter->BranchList->Get_First();

            /* Mark Meter Location */
            Idx1 = pLine->Terminals[pMeter->MeteredTerminal - 1].BusRef - 1;
            Set_LineWidth(4);
            if (ActiveCircuit[ActiveActor]->Buses[Idx1]->CoordDefined)
                AddNewMarker(ActiveCircuit[ActiveActor]->Buses[Idx1]->x, ActiveCircuit[ActiveActor]->Buses[Idx1]->y, 0x000000FF, 24, 3);
            if (ShowLoops)
                FdrColor = Color1;
            else
                FdrColor = NextColor();
            while (pLine != NULL)
            {
                if (pLine->FEnabled)
                {
                    pLine->Drawn = true;
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLine);
                    Idx1 = pLine->Terminals[1 - 1].BusRef - 1;
                    Idx2 = pLine->Terminals[2 - 1].BusRef - 1;
                    if (pLine->Fnphases == 1)
                        LineStyleType = Style(SinglePhLineStyle);
                    else
                        LineStyleType = Style(ThreePhLineStyle);
                    if (ShowLoops && pMeter->BranchList->PresentBranch->IsLoopedHere)
                    {
                        DrawMeterZoneLine(Color3, pLine->LName, Idx1, Idx2, LineStyleType);

                        /* Draw 2nd Line in loop in alternate color, also, if coordinates defined */
                        LoopLine = (TLineObj*)pMeter->BranchList->PresentBranch->LoopLineObj;
                        Idx1 = LoopLine->Terminals[1 - 1].BusRef - 1;
                        Idx2 = LoopLine->Terminals[2 - 1].BusRef - 1;
                        DrawMeterZoneLine(Color3, LoopLine->LName, Idx1, Idx2, LineStyleType);
                    }
                    else
                        DrawMeterZoneLine(FdrColor, pLine->LName, Idx1, Idx2, LineStyleType); // normal show zone
                }
                pLine = (TLineObj*) pMeter->BranchList->Get_Forward();
            }
            hMeter = EnergyMeterClass[ActiveActor]->Get_Next();
        }
        if (Length(ObjectName) > 0)
            S = "Meter Zone: " + ObjectName;
        else
            S = "All Meter Zones";
        Set_ChartCaption(S);
    }


    void TDSSPlot::Execute()
    {
        double              Aspect = 0.0, 
                            XRange = 0.0,
                            RangeLoX = 0.0, 
                            RangeHiX = 0.0, 
                            RangeLoY = 0.0, 
                            RangeHiY = 0.0;
        String              S = "",
                            Fname = "";
        TDSSGraphProperties* DSSGraphProps = new TDSSGraphProperties();
        //Width, LRim, RRim, Height, Trim, Brim: Integer;
        int                 i = 0;

        /*Init line.Drawn variable to Not Drawn*/
        pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
        while ((pLine != nullptr))
        {
            pLine->Drawn = false;
            pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_Next();
        }
        /*# with DSSPlotObj do */
        if ((PlotType == ptCircuitplot) && (Quantity == pqNone) && (FileExists(ObjectName)))
            PlotType = ptGeneralCircuitPlot;

        /* *** Make a New DSSGraph Plot *** */
       // If MakeNewGraph(DSSDataDirectory + CircuitName_ + 'Plot.DSV') = 0 Then
      //  Begin
      //     DoSimpleMsg('Make New Plot failed in DSSPlot Execute.', 8734);
      //     Exit;
      //  End;
        try
        {
            switch (PlotType)
            {
               case ptmonitorplot:
                {
                    Fname = GetOutputDirectory() + CircuitName_[ActiveActor] + "MONITOR-" + UpperCase(ObjectName);
                    for (int stop = Channels.size(), i = 0; i < stop; i++)
                        Fname = Fname + Format("-ch%d", Channels[i]);
                    if (MakeNewGraph(Fname + ".DSV") > 0)
                    {
                        DoMonitorPlot();
                    }
                    else
                    {
                        DoSimpleMsg("Make New Plot failed for Monitor Plot.", 87341);
                    }
                }
                break;   /*Monitor Plot*/
                case ptLoadShape:
                {
                    if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + Format("Loadshape_%s.DSV", ObjectName.c_str())) > 0)
                    {
                        DoLoadShapePlot(ObjectName);
                    }
                    else
                    {
                        DoSimpleMsg("Make New Plot failed for Loadshape Plot.", 87342);
                    }
                }
                break;
                case ptTShape:
                {
                    if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + Format("TempShape_%s.DSV", ObjectName.c_str())) > 0)
                    {
                        DoTempShapePlot(ObjectName);
                    }
                    else
                    {
                        DoSimpleMsg("Make New Plot failed for TempShape Plot.", 87343);
                    }
                }
                break;
                case ptPriceShape:
                {
                    if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + Format("Priceshape_%s.DSV", ObjectName.c_str())) > 0)
                    {
                        DoPriceShapePlot(ObjectName);
                    }
                    else
                    {
                        DoSimpleMsg("Make New Plot failed for PriceShape Plot.", 87344);
                    }
                }
                break;
                case ptProfile:
                {
                    if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + Format("Profile%d.DSV", PhasesToPlot)) > 0)
                    {
                        DoProfilePlot();
                     }
                    else
                    {
                        DoSimpleMsg("Make New Plot failed for Profile Plot.", 87345);
                    }
                }
                break;
                default: /* All other plots */
                {
                    switch (PlotType)
                    {
                        case ptAutoAddLogPlot:
                        {
                            if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "AutoADD.DSV") == 0)
                            {
                                DoSimpleMsg("Make New Plot failed for AutoADD Plot.", 8734);
                                return;
                            }
                        }
                        break;
                        case ptCircuitplot:
                        {
                            Fname = GetOutputDirectory() + CircuitName_[ActiveActor];
                            switch (Quantity)
                            {
                            case pqVoltage:
                                Fname = Fname + "Voltage.DSV";
                                break;
                            case pqCurrent:
                                Fname = Fname + "Current.DSV";
                                break;
                            case pqPower:
                                Fname = Fname + "Power.DSV";
                                break;
                            case pqLosses:
                                Fname = Fname + "Losses.DSV";
                                break;
                            case pqCapacity:
                                Fname = Fname + "Capacity.DSV";
                                break;
                            case pqNone:
                                Fname = Fname + "Circuit.DSV";
                                break;
                            }
                            if (MakeNewGraph(Fname) == 0)
                            {
                                DoSimpleMsg("Make New Plot failed for Circuit Plot.", 87346);
                                return;
                            }
                        }
                        break;
                        case ptGeneralDataPlot:
                        {
                            if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "General.DSV") == 0)
                            {
                                DoSimpleMsg("Make New Plot failed for General Data Plot.", 87347);
                                return;
                            }
                        }
                        break;
                        case ptGeneralCircuitPlot:
                        {
                            if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "GeneralCircuit.DSV") == 0)
                            {
                                DoSimpleMsg("Make New Plot failed for GeneralCircuit Plot.", 87348);
                                return;
                            }
                        }
                        break;
                        case ptMeterZones:
                        {
                            if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "MeterZone.DSV") == 0)
                            {
                                DoSimpleMsg("Make New Plot failed for MeterZone Plot.", 87349);
                                return;
                            }
                        }
                        break;
                        case ptdaisyplot:
                        {
                            if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "Daisy.DSV") == 0)
                            {
                                DoSimpleMsg("Make New Plot failed for Daisy Plot.", 87340);
                                return;
                            }
                        }
                        break;
                    }
                    AllocateBusLabels();
                    Get_Properties(DSSGraphProps);
                    /*# with DSSGraphProps do */
                    {
                        DSSGraphProps->GridStyle = gsNone;
                        DSSGraphProps->ChartColor = 0x00FFFFFF;
                        DSSGraphProps->WindColor = 0x00FFFFFF;
                        DSSGraphProps->Isometric = true;
                        EnableClickonDiagram();
                    }
                    Set_Properties(DSSGraphProps);
                    S = "X";
                    Set_XaxisLabel(S);
                    S = "Y";
                    Set_YaxisLabel(S);
                    Set_TextAlignment(1); /* Left Justify; 2 = center; 3=right */
                    Set_KeyClass(DSSG_LINECLASS); /* Line for searches */


                    switch (PlotType)
                    {
                        case ptAutoAddLogPlot:
                        {
                            MarkerIdx = 26;
                            Set_KeyClass(DSSG_MARKERCLASS); /* Marker */
                            DoAutoAddPlot();
                            MarkSpecialClasses();
                        }
                        break;
                        case ptCircuitplot:
                        {
                            SetMaxScale();
                            S = String(ActiveCircuit[ActiveActor]->FCaseName) + ":" + QuantityString() + " " + Format("max=%.3g", MaxScale);
                            Set_ChartCaption(S);
                            DoCircuitPlot();
                            MarkSpecialClasses();
                        }
                        break;
                        case ptGeneralDataPlot:
                        {
                            Dots = false;
                            DoCircuitPlot();
                            Set_KeyClass(DSSG_MARKERCLASS); /* Marker */
                            MarkerIdx = ActiveCircuit[ActiveActor]->NodeMarkerCode; // 24;
                            DoGeneralPlot();
                            MarkSpecialClasses();
                        }
                        break;
                        case ptGeneralCircuitPlot:
                        {
                            LoadGeneralLineData();
                            SetMaxScale();
                            S = String(ActiveCircuit[ActiveActor]->FCaseName) + ":" + QuantityString() + " " + Format("max=%.3g", MaxScale);
                            Set_ChartCaption(S);
                            DoGeneralCircuitPlot();
                            MarkSpecialClasses();
                        }
                        break;
                        case ptMeterZones:
                        {
                            DoMeterZonePlot();
                            MarkSpecialClasses();
                        }
                        break;
                        case ptdaisyplot:
                        {
                            S = "Device Locations / " + QuantityString();
                            Set_ChartCaption(S);
                            if (Labels)
                            {
                                Labels = false; /* Temporarily turn off */
                                DoCircuitPlot();
                                Labels = true; /* Turn back on to label generators */
                            }
                            else
                                DoCircuitPlot();
                            MarkSpecialClasses();
                            DoTheDaisies();
                        }
                        break;
                        default: /* Case PlotType */
                        {
                            /* Nada */
                        }
                        break;
                    }
                    LabelBuses(); /* Add labels on top of lines */
                    FreeBusLabels();

                    /* Make sure both X and Y have the same scale */
                    // --deprecated--   Get_PlotWindowParms(Width, LRim, RRim, Height, Trim, Brim);
                    // --deprecated--   Aspect :=  (Width - LRim - RRim)/(Height - Trim - Brim);

                    Aspect = 1.5; // Default aspect ratio
                    Get_Properties(DSSGraphProps);
                    /*# with DSSGraphProps do */

                    XRange = max<double>((DSSGraphProps->Xmax - DSSGraphProps->Xmin), (DSSGraphProps->YMax - DSSGraphProps->Ymin) * Aspect);
                    /* Add 2%Margin */
                    XRange = 1.02 * XRange;
                    // --deprecated--        Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
                    RangeLoX = (DSSGraphProps->Xmin + DSSGraphProps->Xmax - XRange) / 2.0; // Xmin - Mar;    {Isometric=true forces Y to have same range as X}
                    RangeHiX = (DSSGraphProps->Xmin + DSSGraphProps->Xmax + XRange) / 2.0; // Xmin + HiX + Mar;
                    RangeLoY = DSSGraphProps->Ymin - 0.02 * XRange / Aspect;
                    RangeHiY = RangeLoY + (XRange / Aspect);
                    Set_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

                    /* Keep this range for quick resetting */
                    DSSGraphProps->Xmin = RangeLoX;
                    DSSGraphProps->Xmax = RangeHiX;
                    DSSGraphProps->Ymin = RangeLoY;
                    DSSGraphProps->YMax = RangeHiY;
                    Set_Properties(DSSGraphProps);

                    Set_KeepAspectRatio(true);
                }
            } /* CASE */
        }
        catch (...)
        {
        }
        ShowGraph();
    }

    void TDSSPlot::DSSVizPlot()
    {
        /* old Delphi way - to be replaced
        if (!(DSSConnectObj != NULL)) // First connection
        {
            DSSConnectObj = TDSSConnect.Create;  // Creates the connection
        }
        DSSConnectObj.Connect;  // Connects to the server
        switch (PlotType)
        {
            case  // Classifies the plot message
            ptmonitorplot:
                DSSConnectObj.MonitorPlotMsg(ObjectName);
                break;
            case ptLoadShape:
                DSSConnectObj.LoadshapePlotMsg(ObjectName);
                break;
            case ptProfile:
            {
                DSSConnectObj.ProfilePlotMsg(ObjectName, PlotID);
                PlotID = "";
            }
            break;
            case ptScatterPlot:
            {
                DSSConnectObj.ScatterPlotMsg(PlotID);
                PlotID = "";
            }
            break;
            case ptEvolutionPlot:
                DSSConnectObj.EvolutionPlotMsg;
                break;
            case ptEnergyPlot:
                DSSConnectObj.EnergyMeterPlotMsg(ObjectName);
                break;
            case ptMatrixplot:
            {
                if (MatrixType == pLaplacian)
                    DSSConnectObj.MatrixPlotMsg(1);
                else
                    DSSConnectObj.MatrixPlotMsg(0);
            }
            break;
            case ptPhaseVoltage:
                DSSConnectObj.PhaseVoltageMsg(ObjectName);
                break;
        }
        */
    }


    int InterpByte(int B1, int B2, double& RatioToUse)
    {
        int result = 0;
        result = Round(B1 + RatioToUse * (B2 - B1));
        return result;
    }


    TColor TDSSPlot::InterpolateGradientColor(TColor Color1, TColor Color2, double Ratio)
    {
        TColor      result = 0;
        const int   Redmask = 0x000000FF;
        const int   GreenMask = 0x0000FF00;
        const int   BlueMask = 0x00FF0000;
        int         R1 = 0, 
                    G1 = 0, 
                    B1 = 0, 
                    R2 = 0, 
                    G2 = 0, 
                    B2 = 0;
        double      RatioToUse = 0.0;

        RatioToUse = max<double>(0.0, min<double>(1.0, Ratio)); // Limit to 0.0 .. 1.0
        R1 = Color1 & Redmask;
        G1 = (Color1 & GreenMask) >> 8;
        B1 = (Color1 & BlueMask) >> 16;
        R2 = Color2 & Redmask;
        G2 = (Color2 & GreenMask) >> 8;
        B2 = (Color2 & BlueMask) >> 16;
        result = InterpByte(R1, R2, RatioToUse) + (InterpByte(G1, G2, RatioToUse) << 8) + (InterpByte(B1, B2, RatioToUse) << 16);
        if (result <= 0)
            result = Color1;
        return result;
    }


    double TDSSPlot::MaxCurrent()
    {
        double result = 0.0;
        int i = 0;
        pLine->ComputeIterminal(ActiveActor); // load element Iterminal buffer
        result = 0.0;
        for (int stop = pLine->Fnphases, i = 1; i <= stop; i++)
            if (cabs(pLine->Iterminal[i - 1]) > result)
                result = cabs(pLine->Iterminal[i - 1]);
        return result;
    }


    TColor TDSSPlot::NextColor()
    {
        TColor result;
        ActiveColorIdx++;
        if (ActiveColorIdx > 17)
            ActiveColorIdx = 1;
        result = ColorArray[ActiveColorIdx - 1];
        return result;
    }


    void TDSSPlot::SetColorArray()
    {
        ColorArray[1 - 1] = TColor(0x000000);
        ColorArray[2 - 1] = TColor(0x0000FF);
        ColorArray[3 - 1] = TColor(0xFF0000);
        ColorArray[4 - 1] = TColor(0xFF00FF);
        ColorArray[5 - 1] = TColor(0x008000);
        ColorArray[6 - 1] = TColor(0x00FF80);
        ColorArray[7 - 1] = TColor(0x4080FF);
        ColorArray[8 - 1] = TColor(0x21DEDA);
        ColorArray[9 - 1] = TColor(0xFF6AB5);
        ColorArray[10 - 1] = TColor(0x004080);
        ColorArray[11 - 1] = TColor(0x008080);
        ColorArray[12 - 1] = TColor(0xA00000);
        ColorArray[13 - 1] = TColor(0x8080FF);
        ColorArray[14 - 1] = TColor(0x800000);
        ColorArray[15 - 1] = TColor(0x7F7F7F);
        ColorArray[16 - 1] = TColor(0x7B0F8E);
        ColorArray[17 - 1] = TColor(0x8E9607);
    }

    void TDSSPlot::SetDefaults()
    {
        MaxScale            = 0.0; // Find MaxScale
        MaxScaleIsSpecified = false; // indicates take the default
        MinScale            = 0.0; // Find MinScale
        MinScaleIsSpecified = false; // indicates take the default
        Dots                = false;
        Labels              = false;
        ShowLoops           = false;
        ShowSubs            = false;
        Quantity            = pqPower;
        PlotType            = ptCircuitplot;
        MarkerIdx           = 24;
        ObjectName          = "";
        FMaxLineThickness   = 10;
        Channels.resize(3);
        Channels[0]         = 1;
        Channels[1]         = 3;
        Channels[2]         = 5;
        Bases.resize(3);
        Bases[0]            = 1.0;
        Bases[1]            = 1.0;
        Bases[2]            = 1.0;
        Color1              = 0x00FF0000;
        Color2              = 0x00008000;
        Color3              = 0x000000FF;
        TriColorMax         = 0.85;
        TriColorMid         = 0.50;
        ActiveColorIdx      = 0;
        SetColorArray();
        ThreePhLineStyle    = 1;
        SinglePhLineStyle   = 1;
    }

    TPenStyle TDSSPlot::Style(int Code)
    {
        TPenStyle result;
        switch (Code)
        {
        case 1:
            result = psSolid;
            break;
        case 2:
            result = psDash;
            break;
        case 3:
            result = psDot;
            break;
        case 4:
            result = psDashDot;
            break;
        case 5:
            result = psDashDotDot;
            break;
        case 6:
            result = psClear;
            break;
        case 7:
            result = psInsideFrame;
            break;
        default:
            result = psSolid;
        }
        return result;
    }


    int TDSSPlot::Thickness()
    {
        int result = 0;
        switch (PlotType)
        {
        case ptmonitorplot:
            result = 1;
            break;
        default:
        {
            pLine->Set_ActiveTerminal(1); // just for good measure
            switch (Quantity)
            {
            case pqNone:
            {
                if (PlotType == ptGeneralCircuitPlot)
                    result = Round(8.0 * (double(abs(pLine->GeneralPlotQuantity)) / MaxScale));
                else
                    result = 1;
            }
            break;
            case pqVoltage:
                result = 1;
                break;
            case pqCurrent:
            {
                if (pLine->NormAmps > 0.0)
                    result = Round(double(5.0) * MaxCurrent() / pLine->NormAmps);
                else
                    result = 1;
            }
            break;
            case pqPower:
            {
                result = Round(5.0 * (abs(pLine->Get_Power(1, ActiveActor).re) * 0.001 / MaxScale)); // kW
            }
            break;
            case pqLosses:
            { // Losses per unit length
                result = Round(5.0 * (abs(pLine->Get_Losses(ActiveActor).re / pLine->Len) * 0.001 / MaxScale));
            }
            break;
            case pqCapacity:
            {
                if (pLine->NormAmps > 0.0)
                    result = Round(5.0 * (1.0 - MaxCurrent() / pLine->NormAmps));
                else
                    result = FMaxLineThickness;
            }
            break;
            default:
                result = 1;
            }
        }
        }
        if (result <= 0)
            result = 1;
        if (result > FMaxLineThickness)
            result = FMaxLineThickness;
        return result;
    }

    void TDSSPlot::DoTempShapePlot(const String TempShapeName)
    {
        TTShapeObj* Temp_Shape = nullptr;
        vector <double> Xarray;
        double      X = 0.0, 
                    Xinc = 0.0;
        int         i = 0,
                    Xsize = 0;
        String      XLabel = "",
                    S = "";
        bool        UseXarray = false;

        Temp_Shape = (TTShapeObj*)TShapeClass[ActiveActor]->Find(TempShapeName);
        if (Temp_Shape == NULL)
        {
            DoSimpleMsg(String("Tshape object not found: \"") + TempShapeName + "\"", 87341);
            return;
        }
        UseXarray = false;
        Xarray.clear();
        Xsize = 0; // Init
        if (Temp_Shape->Interval != 0.0)
            /*# with Temp_Shape do */
        { // have to gen up Xarray
            auto with0 = Temp_Shape;
            UseXarray = true;
            Xsize = sizeof(Xarray[1]) * with0->FNumPoints;
            Xarray.resize(Xsize); // SetLength(Xarray, Numpoints);
            X = 0.0;
            if (with0->Interval * with0->FNumPoints < 1.0)
            {
                Xinc = with0->Interval * 3600.0; // Plot secs
                XLabel = "Seconds";
            }
            else
            {
                Xinc = with0->Interval;
                XLabel = "Hours";
            }
            for (int stop = with0->FNumPoints, i = 1; i <= stop; i++)
            {
                Xarray[i] = X;
                X = X + Xinc;
            }
        }

        // ** already exists MakeNewGraph;
        S = "TShape." + TempShapeName;
        Set_Caption(S);
        S = "TShape = " + TempShapeName;
        Set_ChartCaption(S);
        Set_XaxisLabel(XLabel);
        Set_YaxisLabel("Temperature");
        if (UseXarray)
            AddNewCurve(&(Xarray[0]), Temp_Shape->TValues, Temp_Shape->FNumPoints, Color1, 1, 0, false, 1, TempShapeName);
        else
            AddNewCurve(Temp_Shape->Hours, Temp_Shape->TValues, Temp_Shape->FNumPoints, Color1, 1, 0, false, 1, TempShapeName);
        Set_KeepAspectRatio(false);
        if (UseXarray)
        Xarray.clear();
        Set_AutoRange(2.0); // 2% rim
        //***   ShowGraph; { Form Freed on close }
    }


    void TDSSPlot::DoLoadShapePlot(const String LoadShapeName)
    {
        TLoadShapeObj*  Load_Shape = nullptr;
        vector<double>  Xarray;
        double          X = 0.0, 
                        Xinc = 0.0;
        int             i = 0, 
                        Xsize = 0;
        String          S = "", 
                        XLabel = "";
        bool            UseXarray = false;

        Load_Shape = (TLoadShapeObj*) LoadShapeClass[ActiveActor]->Find(LoadShapeName);
        if (Load_Shape == NULL)
        {
            DoSimpleMsg(String("Loadshape object not found: \"") + LoadShapeName + "\"", 87341);
            return;
        }
        UseXarray = false;
        Xarray.clear();
        Xsize = 0; // Init
        if (Load_Shape->Interval != 0.0)
            /*# with Load_Shape do */
        { // have to gen up Xarray
            auto with0 = Load_Shape;
            UseXarray = true;
            Xsize = with0->FNumPoints;
            Xarray.resize(Xsize); // SetLength(Xarray, Numpoints);
            X = 0.0;
            if (with0->Interval * with0->FNumPoints < 1.0)
            {
                Xinc = with0->Interval * 3600.0; // Plot secs
                XLabel = "Seconds";
            }
            else
            {
                Xinc = with0->Interval;
                XLabel = "Hours";
            }
            for (int stop = with0->FNumPoints, i = 0; i < stop; i++)
            {
                Xarray[i] = X;
                X = X + Xinc;
            }
        }

        // ** already exists MakeNewGraph;
        S = String("Loadshape.") + LoadShapeName;
        Set_Caption(S);
        S = String("Loadshape = ") + LoadShapeName;
        Set_ChartCaption(S);
        Set_XaxisLabel(XLabel);
        if (Load_Shape->UseActual)
            Set_YaxisLabel("kW, kvar");
        else
            Set_YaxisLabel("p.u.");
        if (Load_Shape->UseMMF)
        {
            Load_Shape->PMultipliers.resize(Load_Shape->FNumPoints);
            for (int stop = Load_Shape->FNumPoints, i = 1; i <= stop; i++)
                Load_Shape->PMultipliers[i - 1] = InterpretDblArrayMMF(Load_Shape->myView, Load_Shape->myFileType, Load_Shape->myColumn, i, Load_Shape->myLineLen);
        }
        if (UseXarray)
            AddNewCurve(&(Xarray[0]), &(Load_Shape->PMultipliers[0]), Load_Shape->FNumPoints, Color1, 1, 0, false, 1, LoadShapeName);
        else
            AddNewCurve(&(Load_Shape->Hours[0]), &(Load_Shape->PMultipliers[0]), Load_Shape->FNumPoints, Color1, 1, 0, false, 1, LoadShapeName);
        if (!(Load_Shape->QMultipliers.empty()))
        {
            if (Load_Shape->UseMMF)
            {
                Load_Shape->QMultipliers.resize(Load_Shape->FNumPoints);
                for (int stop = Load_Shape->FNumPoints, i = 1; i <= stop; i++)
                    Load_Shape->QMultipliers[i - 1] = InterpretDblArrayMMF(Load_Shape->myViewQ, Load_Shape->myFileTypeQ, Load_Shape->myColumnQ, i, Load_Shape->myLineLenQ);
            }
            if (UseXarray)
                AddNewCurve(&(Xarray[0]), &(Load_Shape->QMultipliers[0]), Load_Shape->FNumPoints, Color2, 1, 0, false, 1, LoadShapeName);
            else
                AddNewCurve(&(Load_Shape->Hours[0]), &(Load_Shape->QMultipliers[0]), Load_Shape->FNumPoints, Color2, 1, 0, false, 1, LoadShapeName);
        }
        Set_KeepAspectRatio(false);
        if (UseXarray)
            Xarray.clear();
        Set_AutoRange(2.0); // 2% rim
        //***   ShowGraph; { Form Freed on close }
    }

    /* --------------------------------------------------------- */


    void LoadRegisters(pDoubleArray RegisterArray)
    {
        int i = 0;
        AuxParser[ActiveActor]->ParseAsVector(NumEMRegisters + 1, RegisterArray);
        for (int stop = NumEMRegisters, i = 1; i <= stop; i++)
            RegisterArray[i] = RegisterArray[i] * 0.001;
    }

    /* --------------------------------------------------------- */


    void PeakDayLoadRegisters(TTextRec& F, pDoubleArray RegisterArray)
    {
        int iday = 0, i = 0;
        double TempRegisters[NumEMRegisters];
        String S;
        for (int stop = NumEMRegisters, i = 0; i < stop; i++)
            RegisterArray[i] = 0.0;
        for (int stop = 24, iday = 1; iday <= stop; iday++)
            if (!Eof(F))
            {
                ReadLn(F, S);
                AuxParser[ActiveActor]->SetCmdString("\"" + S + "\"");
                AuxParser[ActiveActor]->GetNextParam();
                LoadRegisters(&(TempRegisters[0]));
                for (int stop = NumEMRegisters, i = 0; i < stop; i++)
                    RegisterArray[i] = max<double>(RegisterArray[i], TempRegisters[i]);
            }
    }

    /* --------------------------------------------------------- */


    void TDSSPlot::DoDI_Plot(const String CaseName, int CaseYear, const int* iRegisters, int iRegisters_maxidx, bool PeakDay, const String MeterName)
    {
        TTextRec    F;
        TStringList Names;
        String      S = "",
                    FileName = "",
                    Param = "";
        double      Registers1[NumEMRegisters - 0 + 1], 
                    Registers2[NumEMRegisters - 0 + 1];
        int         i = 0;
        TDSSGraphProperties* ActiveGraphProps = new TDSSGraphProperties;
        /* Plot Demand interval data from saved results DI_Totals.CSV */
        /* If PeakDay=True then we only plot the peak of a 24-hr day */
        Names.clear();
        /* Open File */
        FileName = CaseName + "\\di_yr_" + Trim(IntToStr(CaseYear)) + "\\" + MeterName + ".CSV";
        if (!FileExists(FileName))
        {
            DoSimpleMsg("File \"" + FileName + "\" does not exist.", 191);
            return;
        }
        else
        {
            try
            {
                AssignFile(F, FileName);
                Reset(F);
                IOResultToException();
                ReadLn(F, S); // Read input line
                /*# with AuxParser[ActiveActor] do */
                {
                    auto with0 = AuxParser[ActiveActor];
                    {
                        with0->SetCmdString(S);
                        with0->GetNextParam();
                        Param = with0->MakeString_();
                        while (Param.size() > 0)
                        {
                            Names.push_back(Param);
                            with0->GetNextParam();
                            Param = AuxParser[ActiveActor]->MakeString_();
                        }
                    }
                } /* With */
            }
            catch (exception& E)
            {
                DoSimpleMsg(String("Error Reading File \"") + FileName + "\". " + (string) E.what(), 192);
            }
        }
        if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "DIPlot.DSV") == 0)
        {
            DoSimpleMsg("Make New Plot failed in DSSPlot - DI plot.", 8734);
            return;
        }

        /* POssibly change some properties of the graph */
        Get_Properties(ActiveGraphProps);
        /*# with ActiveGraphProps do */
        {
            auto with1 = ActiveGraphProps;
            with1->ChartColor    = 0x00FFFFFF;
            with1->WindColor     = 0x00FFFFFF;
        }
        Set_Properties(ActiveGraphProps);
        S = CaseName + Format(", Yr=%d, ", CaseYear);
        Set_Caption(S);
        Set_XaxisLabel("Hour");
        S = "MW, MWh or MVA";
        Set_YaxisLabel(S);
        S = "Registers: ";
        for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
            S = S + Names[iRegisters[i]] + ", ";
        Set_ChartCaption(S);

        /* Get started - initializer Registers 1 */
        try
        {
            try
            {
                if (!Eof(F))
                {
                    if (PeakDay)
                    {
                        PeakDayLoadRegisters(F, &Registers1[0]);
                    }
                    else
                    {
                        ReadLn(F, S);
                        /*# with AuxParser[ActiveActor] do */
                        {
                            auto with1 = AuxParser[ActiveActor];
                            {
                                with1->SetCmdString("\"" + S + "\"");
                                with1->GetNextParam();
                                LoadRegisters(&Registers1[0]);
                            }
                        }
                    }
                }
                while (!Eof(F))
                {
                    if (PeakDay)
                        PeakDayLoadRegisters(F, &Registers2[0]);
                    else
                    {
                        ReadLn(F, S);
                        /*# with AuxParser[ActiveActor] do */
                        {
                            auto with2 = AuxParser[ActiveActor];
                            {
                                with2->SetCmdString("\"" + S + "\"");
                                with2->MakeString_();
                                LoadRegisters(&Registers2[0]);
                            }
                        }
                    }
                    ActiveColorIdx = 0;
                    for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                    {
                        AddNewLine(Registers1[0], Registers1[iRegisters[i]], Registers2[0], Registers2[iRegisters[i]], NextColor(), 1, 0, false, " ", false, 0, 0, 0);
                    }
                    for (int stop = NumEMRegisters, i = 0; i <= stop; i++)
                        Registers1[i] = Registers2[i];
                }
            }
            catch (exception& E)
            {
                DoSimpleMsg(String("Error Reading File \"") + FileName + "\". " + (string) E.what(), 193);
            }
            Set_KeepAspectRatio(false);
            Set_AutoRange(2.0); // 2% rim
            //****      ShowGraph; { Form Freed on close }
        }
        catch (...)
        {
        }
        CloseFile(F);
        Names.clear();
    }


    double GetDiff(double Yvalue, double XValue, int MaxYear, double** X, double** Y)
        /* Interpolation routine */
    {
        double result = 0.0;
        int k = 0, lastk = 0;
        lastk = 0;
        for (int stop = MaxYear, k = 0; k <= stop; k++)
        {
            if (X[2][k - 1] > 0.0)
            {
                lastk = k;
                if (Yvalue == 0.0)
                {
                    result = 0.0;
                    return result;
                }
                else
                    if (Y[2][k - 1] == Yvalue)
                    {
                        result = X[2][k - 1] - XValue;
                        return result;
                    }
                    else
                        if (Y[2][k - 1] > Yvalue)
                        {
                            if (k == 0)
                                result = X[2][k - 1] - XValue;
                            else
                                if ((Y[2][k - 1] - Y[2][k - 1 - 1]) == 0.0)
                                    result = X[2][k - 1 - 1] - XValue;
                                else
                                    result = X[2][k - 1 - 1] + (Yvalue - Y[2][k - 1 - 1]) / (Y[2][k - 1] - Y[2][k - 1 - 1]) * (X[2][k - 1] - X[2][k - 1 - 1]) - XValue;
                            return result;
                        }
            }
        }
        /* If we get here, didn't find anything.  Extrapolate last two points */

  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
     /* Plot Demand interval data from saved results DI_Totals.CSV */
        if (lastk == 0)
            result = 0.0;
        else
            result = X[2][lastk - 1 - 1] + (Yvalue - Y[2][lastk - 1 - 1]) / (Y[2][lastk - 1] - Y[2][lastk - 1 - 1]) * (X[2][lastk - 1] - X[2][lastk - 1 - 1]) - XValue;
        return result;
    }


    void MakeDiffCurve(double* HorizDiff, int MaxYear, double** X, double** Y)
    {
        int j = 0;
        for (int stop = MaxYear, j = 0; j <= stop; j++)
        {
            if (X[1][j - 1] > 0.0)
                HorizDiff[j] = GetDiff(Y[1][j - 1], X[1][j - 1], MaxYear, X, Y);
        }
    }


    bool ReadS(TColor CaseYear, Textfile& F, String& S, bool SearchForMeterName, String WhichFile)
    {
        bool result = false;
        result = true;
        if (SearchForMeterName)
        {
            do
            {
                ReadLn(F, S);
                AuxParser[ActiveActor]->SetCmdString(S);
                AuxParser[ActiveActor]->GetNextParam();
            } while (!((CompareText(WhichFile, AuxParser[ActiveActor]->MakeString_()) == 0) || Eof(F)));
            if (CompareText(WhichFile, AuxParser[ActiveActor]->MakeString_()) == 0)
            {
                S = IntToStr(CaseYear) + S.substr(S.find(","), 9999);
            }
            else
            {
                result = false;
            }
        }
        else
            ReadLn(F, S);
        return result;
    }


    void TDSSPlot::DoCompareCases(String CaseName1, String CaseName2, String WhichFile, int Reg)

        /* Compare a register from to cases in the Totals.CSV file, compute horiz distance,
          plot vs 1st register of totals.csv file */
    {
        TTextRec    F;
        String      S = "", 
                    FileName = "",
                    Param = "", 
                    CaseName = "";
        TStringList Names;
        double      Registers1[NumEMRegisters - 0 + 1] = {},
                    Registers2[NumEMRegisters - 0 + 1] = {},
                    X[3/*# range 1..2*/][21/*# range 0..20*/] = {},
                    Y[3/*# range 1..2*/][21/*# range 0..20*/] = {};
        double      HorizDiff[21/*# range 0..20*/] = {};
        double      X1 = 0.0, 
                    Y1 = 0.0,
                    Xinc = 0.0, 
                    Yinc = 0.0, 
                    LegendX = 0.0, 
                    LegendY = 0.0;
        TColor      i = 0, 
                    iPass = 0, 
                    iCase = 0, 
                    CaseYear = 0, 
                    PrevCaseYear = 0, 
                    ActiveColorStartThisCase = 0, 
                    DiffColor = 0;

        /* Arrays to hold the two curves for diff curve */

        int         MinYear = 0, 
                    MaxYear = 0,
                    LabelIdx = 0;
        bool        SearchForMeterName = false;
        bool        FirstYear = false;
        TDSSGraphProperties* ActiveGraphProps = new TDSSGraphProperties;
        TColor      DatColor = 0;

        /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

        /* Internal Procs */
        Names.clear();

        /* Init holding array */
        for (int stop = 20, i = 0; i <= stop; i++)
        {
            X[1][i - 1] = -1.0; // signify no value at this point
            X[2][i - 1] = -1.0;
            Y[1][i - 1] = 0.0;
            Y[2][i - 1] = 0.0;
        }
        MinYear = 20;
        MaxYear = 0;
        if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "CompPlot.DSV") == 0)
        {
            DoSimpleMsg("Make New Plot failed in DSSPlot - comparison Plot.", 8734);
            return;
        }
        Get_Properties(ActiveGraphProps);
        /*# with ActiveGraphProps do */
        {
            ActiveGraphProps->ChartColor = 0x00FFFFFF;
            ActiveGraphProps->WindColor  = 0x00FFFFFF;
        }
        Set_Properties(ActiveGraphProps);
        S = String("Comparision of Yearly Curves for case(s):") + CaseName1 + ", " + CaseName2;
        Set_Caption(S);
        S = "Total Area MW";
        Set_XaxisLabel(S);
        S = "MW, MWh or MVA";
        Set_YaxisLabel(S);

        /* Loop Through Cases */
        ActiveColorStartThisCase = 0;
        CaseName = CaseName1;
        for (int stop = 2, iCase = 1; iCase <= stop; iCase++)
        {

            // Get X values from Totals.CSV on first pass
            for (int stop = 2, iPass = 1; iPass <= stop; iPass++)
            {
                /* Loop through possible caseyears (0..20) */
                FirstYear = true;
                for (int stop = 20, CaseYear = 0; CaseYear <= stop; CaseYear++)
                {
                    /* Open File */
                    SearchForMeterName = false;
                    switch (iPass)
                    {
                    case 1:
                        FileName = CaseName + "\\di_yr_" + Trim(IntToStr(CaseYear)) + "\\Totals.CSV";
                        break;
                    case 2:
                        if ((CompareText(WhichFile, "Totals") == 0) || (CompareText(WhichFile, "Systemmeter") == 0))
                        {
                            FileName = CaseName + "\\di_yr_" + Trim(IntToStr(CaseYear)) + "\\" + WhichFile + ".CSV";
                        }
                        else
                        {
                            FileName = CaseName + "\\di_yr_" + Trim(IntToStr(CaseYear)) + "\\" + "EnergyMeterTotals.CSV";
                            SearchForMeterName = true;
                        }
                        break;
                    }
                    if (!FileExists(FileName))
                    {
                        continue; // Skip if it doesnt exist
                    }
                    else
                    {
                        try
                        {
                            MaxYear = max<int>(MaxYear, CaseYear);
                            MinYear = min<int>(MinYear, CaseYear);
                            AssignFile(F, FileName);
                            Reset(F);
                            IOResultToException();
                            ReadLn(F, S); // Read header line
                            if ((iCase == 1) && FirstYear)
                            {
                                AuxParser[ActiveActor]->SetCmdString(S);
                                AuxParser[ActiveActor]->GetNextParam();
                                Param = AuxParser[ActiveActor]->MakeString_();
                                while (Param.size() > 0)
                                {
                                    Names.push_back(Param);
                                    AuxParser[ActiveActor]->GetNextParam();
                                    Param = AuxParser[ActiveActor]->MakeString_();
                                }
                                S = String("Meter: ") + WhichFile + " Register: " + Names[Reg];
                                Set_ChartCaption(S);
                            }
                        }
                        catch (exception& E)
                        {
                            DoSimpleMsg(String("Error Reading File \"") + FileName + "\". " + (string)E.what(), 194);
                        }
                    }
                    /* Get started - initialize Registers 1 */
                    PrevCaseYear = CaseYear;
                    try
                    {
                        try
                        {
                            if (FirstYear)
                            {
                                if (!Eof(F))
                                {
                                    if (!ReadS(CaseYear, F, S, SearchForMeterName,WhichFile))
                                    {
                                        DoSimpleMsg(String("Meter Not Found: \"") + WhichFile + "\"", 1941);
                                        return; // Abort
                                    }
                                    AuxParser[ActiveActor]->SetCmdString("\"" + S + "\"");
                                    AuxParser[ActiveActor]->GetNextParam();
                                    LoadRegisters(&Registers1[0]);
                                    switch (iPass)
                                    {
                                    case 1:
                                        X[iCase][CaseYear - 1] = Registers1[7];
                                        break;
                                    case 2:
                                        Y[iCase][CaseYear - 1] = Registers1[Reg];
                                        break;
                                    }
                                    FirstYear = false;
                                }
                            }
                            else
                                if (!Eof(F))
                                { // Gotta have at least 2 years to make a plot
                                    ReadS(CaseYear, F, S, SearchForMeterName, WhichFile);
                                    AuxParser[ActiveActor]->SetCmdString("\"" + S + "\"");
                                    AuxParser[ActiveActor]->GetNextParam();
                                    LoadRegisters(&Registers2[0]);
                                    switch (iPass)
                                    {
                                    case 1:
                                        X[iCase][CaseYear - 1] = Registers2[7];
                                        break;
                                    case 2:
                                        Y[iCase][CaseYear - 1] = Registers2[Reg];
                                        break;
                                    }
                                    switch (iPass)
                                    {
                                    case 2:
                                    {
                                        ActiveColorIdx = ActiveColorStartThisCase;
                                        AddNewLine(X[iCase][PrevCaseYear - 1], Registers1[Reg], X[iCase][CaseYear - 1], Registers2[Reg], NextColor(), 2, 0, false, " ", false, 0, 0, 0);
                                        MarkAt(X[iCase][CaseYear - 1], Registers2[Reg], GetMarker(ActiveColorIdx), 1);
                                        for (int stop = NumEMRegisters, i = 0; i <= stop; i++)
                                            Registers1[i] = Registers2[i];
                                    }
                                    break;
                                    default:
                                        break;
                                    }
                                }
                        }
                        catch (exception& E)
                        {
                            DoSimpleMsg(String("Error Reading File \"") + FileName + "\". " + (string) E.what(), 195);
                        }
                    }
                    catch (...)
                    {
                    }
                    CloseFile(F);
                } /* For CaseYear */
            } /* For iPass */
            ActiveColorStartThisCase = ActiveColorIdx;
            // Start next case where this one left off
            CaseName = CaseName2;
        } /* For CaseNames */

         /* Make Diff Plot and Write output file */
        MakeDiffCurve(HorizDiff,MaxYear,(double**)& X[0][0], (double**)&Y[0][0]);
        DiffColor = NextColor();
        FirstYear = true;
        X1 = 0.0;
        Y1 = 0.0;
        for (int stop = 20, CaseYear = 0; CaseYear <= stop; CaseYear++)
        {
            if (X[1][CaseYear - 1] >= 0.0)
            {
                if (FirstYear)
                {
                    X1 = X[1][CaseYear - 1];
                    Y1 = HorizDiff[CaseYear];
                    FirstYear = false;
                }
                else
                {
                    AddNewLine(X1, Y1, X[1][CaseYear - 1], HorizDiff[CaseYear], DiffColor, 1, 0, false, " ", false, 0, 0, 0);
                    MarkAt(X[1][CaseYear - 1], HorizDiff[CaseYear], GetMarker(ActiveColorIdx), 1);
                    X1 = X[1][CaseYear - 1];
                    Y1 = HorizDiff[CaseYear];
                }
            }
        }
        Set_AutoRange(2.0); // 2% rim
        /* Put on legend in upper left hand corner */
        Get_Properties(ActiveGraphProps);
        Xinc = 0.05 * (ActiveGraphProps->Xmax - ActiveGraphProps->Xmin);
        Yinc = 0.05 * (ActiveGraphProps->YMax - ActiveGraphProps->Ymin);
        LegendX = ActiveGraphProps->Xmin + Xinc;
        LegendY = ActiveGraphProps->YMax - Yinc;
        ActiveColorIdx = 0;
        DatColor = NextColor(); // Next color automatically increments
        Set_DataColor(DatColor);
        LabelIdx = AddTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc, DatColor, CaseName1, 0);
        LockInTextLabel(LabelIdx);
        LegendY = LegendY - Yinc;
        DatColor = NextColor(); // Next color automatically increments
        Set_DataColor(DatColor);
        LabelIdx = AddTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc, DatColor, CaseName2, 0);
        LockInTextLabel(LabelIdx);
        LegendY = LegendY - Yinc;
        DatColor = NextColor(); // Next color automatically increments
        Set_DataColor(DatColor);
        LabelIdx = AddTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc, DatColor, "Difference", 0);
        LockInTextLabel(LabelIdx);

        /* Write Output File */
        try
        {
            FileName = CaseName2 + "-" + CaseName1 + "_Reg" + Trim(IntToStr(Reg)) + ".CSV";
            AssignFile(F, FileName);
            Rewrite(F);
            IOResultToException();
            WriteLn(F, String("\"MW Load\", \"") + CaseName1 + "\", \"MW Load\", \"" + CaseName2 + "\", \"Incr. Cap.\"");
            for (int stop = 20, CaseYear = 0; CaseYear <= stop; CaseYear++)
            {
                if (X[1][CaseYear - 1] >= 0.0)
                    WriteLn(F, Format("%-g, %-g, %-g, %-g, %-g", X[1][CaseYear - 1], Y[1][CaseYear - 1], X[2][CaseYear - 1], Y[2][CaseYear - 1], HorizDiff[CaseYear]));
            }
            CloseFile(F);
            GlobalResult = FileName;
        }
        catch (exception& E)
        {
            DoSimpleMsg(String("Error writing file: \"") + FileName + "\". " + (string) E.what(), 196);
        }
        Set_KeepAspectRatio(false);

        //****   ShowGraph; { Form Freed on close }
        Names.clear();
    }


    void WriteFoutRecord(int opt, String CaseName, int CaseYear, Textfile& Fout, double* Registers1, double* Registers2, double* XValue, const int* iRegisters, int iRegisters_maxidx, int iX)
    {
        int i = 0;
        Write(Fout, Format("%s, %d, %.7g", CaseName.c_str(), CaseYear, XValue[iX]));
        switch (opt)
        {
        case 1:
            for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                Write(Fout, Format(", %.7g  ", Registers1[iRegisters[i]]));
            break;
        case 2:
            for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                Write(Fout, Format(", %.7g  ", Registers2[iRegisters[i]]));
            break;
        }
        WriteLn(Fout);
    }


    void TDSSPlot::DoYearlyCurvePlot(TStringList CaseNames, String WhichFile, const int* iRegisters, int iRegisters_maxidx)

        /* Plot yearly results from specified cases and registers in Totals.CSV files
          Vs Register 1 */
    {
        TTextRec    F, Fout;
        String      S = "", 
                    FileName = "", 
                    Param = "", 
                    CaseName = "";
        TStringList Names;
        double      Registers1[NumEMRegisters + 1] = {}, 
                    Registers2[NumEMRegisters + 1] = {},      
                    XValue[21/*# range 0..20*/] = {};
        int         i = 0, 
                    iPass = 0, 
                    iX = 0, 
                    iCase = 0, 
                    CaseYear = 0, 
                    ActiveColorStartThisCase = 0;
        bool        FirstYear = false;
        double      LegendX = 0.0, 
                    LegendY = 0.0, 
                    Xinc = 0.0, 
                    Yinc = 0.0;
        int         LabelIdx = 0;
        bool        SearchForMeterName = false;
        TDSSGraphProperties* ActiveGraphProps = new TDSSGraphProperties;
        TColor      DatColor = 0;

        /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * * * * */
        /* Internal Procs */
     /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
        /* Plot Demand interval data from saved results DI_Totals.CSV */
        Names.clear();
        if (MakeNewGraph(GetOutputDirectory() + CircuitName_[ActiveActor] + "YearlyPlot.DSV") == 0)
        {
            DoSimpleMsg("Make New Plot failed in DSSPlot -- yearly plot.", 8734);
            return;
        }
        S = "Yearly Curves for case(s)";
        for (int stop = CaseNames.size() - 1, i = 0; i <= stop; i++)
            S = S + ", " + CaseNames[i];
        Set_Caption(S);
        Get_Properties(ActiveGraphProps);
        /*# with ActiveGraphProps do */
        {
            ActiveGraphProps->ChartColor = 0x00FFFFFF;
            ActiveGraphProps->WindColor  = 0x00FFFFFF;
        }
        Set_Properties(ActiveGraphProps);
        S = "Total Area MW";
        Set_XaxisLabel(S);
        S = "MW, MWh or MVA";
        Set_YaxisLabel(S);
        try
        { /* ... Finally */
            AssignFile(Fout, "LastYearlyCurvePlot.CSV");
            Rewrite(Fout);
            IOResultToException();
            Write(Fout, "Case, Year, TotalMW");
            if ((ActiveEnergyMeterObj != NULL))
                for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                    Write(Fout, Format(", \"%s\"", ActiveEnergyMeterObj->RegisterNames[iRegisters[i] - 1].c_str()));
            else
                for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                    Write(Fout, Format(", \"Reg %d\"", iRegisters[i]));
            WriteLn(Fout);

            /* Loop Through Cases */
            FirstYear = true;
            ActiveColorStartThisCase = 0;
            for (int stop = CaseNames.size(), iCase = 0; iCase < stop; iCase++)
            {
                CaseName = CaseNames[iCase];
                if (DirectoryExists(CaseName))
                    // Do This in Two Passes to set the X Values at Register 7 of Totals.CSV
                for (int stop = 2, iPass = 1; iPass <= stop; iPass++)
                {
                    /* Loop through possible caseyears (0..20) */
                    FirstYear = true;
                    for (int stop = 20, CaseYear = 0; CaseYear <= stop; CaseYear++)
                    {
                        /* Open File */
                        SearchForMeterName = false;
                        switch (iPass)
                        {
                        case 1:
                            FileName = CaseName + "\\di_yr_" + Trim(IntToStr(CaseYear)) + "\\Totals.CSV";
                            break;
                        default:
                        {
                            if ((CompareText(WhichFile, "Totals") == 0) || (CompareText(WhichFile, "Systemmeter") == 0))
                            {
                                FileName = CaseName + "\\di_yr_" + Trim(IntToStr(CaseYear)) + "\\" + WhichFile + ".CSV";
                            }
                            else
                            {
                                FileName = CaseName + "\\di_yr_" + Trim(IntToStr(CaseYear)) + "\\" + "EnergyMeterTotals.CSV";
                                SearchForMeterName = true;
                            }
                        }
                        }
                        if (!FileExists(FileName))
                        {
                            continue; // Skip if it doesnt exist
                        }
                        else
                        {
                            try
                            {
                                AssignFile(F, FileName);
                                Reset(F);
                                IOResultToException();
                                ReadLn(F, S); // Read header line
                                switch (iPass)
                                {
                                case 2:
                                    if ((iCase == 0) && FirstYear)
                                    {
                                        AuxParser[ActiveActor]->SetCmdString(S);
                                        AuxParser[ActiveActor]->GetNextParam();
                                        Param = AuxParser[ActiveActor]->MakeString_();
                                        while (Param.size() > 0)
                                        {
                                            Names.push_back(Param);
                                            AuxParser[ActiveActor]->GetNextParam();
                                            Param = AuxParser[ActiveActor]->MakeString_();
                                        }
                                        S = String("Meter: ") + WhichFile + ", Registers: ";
                                        for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                                            S = S + Names[iRegisters[i]] + ", ";
                                        Set_ChartCaption(S);
                                    }
                                    break;
                                default:
                                    /* Nada */
                                    break;
                                }
                            }
                            catch (exception& E)
                            {
                                DoSimpleMsg(String("Error Reading File \"") + FileName + "\". " + (string) E.what(), 197);
                            }
                        }

                        /* Get started - initialize Registers 1 */
                        try
                        {
                            try
                            {
                                if (FirstYear)
                                {
                                    if (!Eof(F))
                                    {
                                        if (!ReadS(CaseYear, F, S, SearchForMeterName, WhichFile))
                                        { // Reads S
                                            DoSimpleMsg(String("Meter not found: \"") + WhichFile + "\"", 1971);
                                            return;
                                        }
                                        AuxParser[ActiveActor]->SetCmdString("\"" + S + "\"");
                                        AuxParser[ActiveActor]->GetNextParam();
                                        LoadRegisters(&Registers1[0]); // from auxparser
                                        iX = 0;
                                        switch (iPass)
                                        {
                                        case 1:
                                            XValue[iX] = Registers1[7];
                                            break;
                                        default:
                                            WriteFoutRecord(1, CaseName, CaseYear, Fout, Registers1, Registers2, XValue, iRegisters, iRegisters_maxidx, iX);
                                        }
                                        FirstYear = false;
                                    }
                                }
                                else
                                    if (!Eof(F))
                                    { // Gotta have at least 2 years to make a plot
                                        ReadS(CaseYear, F, S, SearchForMeterName, WhichFile); // Reads S  -- any errors will be caught on first pass
                                        AuxParser[ActiveActor]->SetCmdString("\"" + S + "\"");
                                        // enclose in quotes to parse as array
                                        AuxParser[ActiveActor]->GetNextParam();
                                        LoadRegisters(&Registers2[0]); // from auxparser
                                        iX++;
                                        switch (iPass)
                                        {
                                        case 1:
                                            XValue[iX] = Registers2[7];
                                            break;
                                        default:
                                            ActiveColorIdx = ActiveColorStartThisCase;
                                            for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                                            {
                                                AddNewLine(XValue[iX - 1], Registers1[iRegisters[i]], XValue[iX], Registers2[iRegisters[i]], NextColor(), 2, 0, false, " ", false, 0, 0, 0);
                                                MarkAt(XValue[iX], Registers2[iRegisters[i]], GetMarker(ActiveColorIdx), 1);
                                            }
                                            WriteFoutRecord(2, CaseName, CaseYear, Fout, Registers1, Registers2, XValue, iRegisters, iRegisters_maxidx, iX);
                                            for (int stop = NumEMRegisters, i = 0; i <= stop; i++)
                                                Registers1[i] = Registers2[i];
                                        }
                                    }
                            }
                            catch (exception& E)
                            {
                                DoSimpleMsg(String("Error Reading File \"") + FileName + "\". " + (string) E.what(), 198);
                            }
                        }
                        catch (...)
                        {
                        }
                        CloseFile(F);
                    } /* For CaseYear */
                } /* For iPass */
                ActiveColorStartThisCase = ActiveColorIdx;
                // Start next case where this one left off
            } /* For CaseNames */
            if (FirstYear)
            {
                DoSimpleMsg("No Files Found", 199);
            }
            else
            {
                /* Put on legend in upper left hand corner */
                Get_Properties(ActiveGraphProps);
                Xinc = 0.05 * (ActiveGraphProps->Xmax - ActiveGraphProps->Xmin);
                Yinc = 0.05 * (ActiveGraphProps->YMax - ActiveGraphProps->Ymin);
                LegendX = ActiveGraphProps->Xmin + Xinc;
                LegendY = ActiveGraphProps->YMax - Yinc;
                ActiveColorIdx = 0;
                for (int stop = CaseNames.size(), iCase = 0; iCase < stop; iCase++)
                {
                    CaseName = CaseNames[iCase];
                    if (DirectoryExists(CaseName))
                        for (int stop = iRegisters_maxidx /*# High(iRegisters) */, i = 0; i <= stop; i++)
                        {
                            S = CaseNames[iCase] + ", " + Names[iRegisters[i]];
                            DatColor = NextColor();
                            Set_DataColor(DatColor);
                            MarkAt(LegendX, LegendY, GetMarker(ActiveColorIdx), 1);
                            LabelIdx = AddTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc, DatColor, S, 0);
                            Set_LeftJustifyTransparent(LabelIdx);
                        }
                    LegendY = LegendY - Yinc;
                }
            }
            Set_KeepAspectRatio(false);
            Set_AutoRange(2.0); // 2% rim
            //****      ShowGraph; { Form Freed on close }
            Names.clear();
        }
        catch (...)
        {
        }
        CloseFile(Fout);
        GlobalResult = "LastYearlyCurvePlot.CSV";
    }


    unsignedchar TDSSPlot::GetMarker(int Idx)
    {
        unsignedchar result = 0;
        do
        {
            if (Idx > 9)
                Idx = Idx - 9;
        } while (!(Idx < 10));
        switch (Idx)
        {
        case 1:
            result = 5;
            break;
        case 2:
            result = 15;
            break;
        case 3:
            result = 2;
            break;
        case 4:
            result = 8;
            break;
        case 5:
            result = 26;
            break;
        case 6:
            result = 36;
            break;
        case 7:
            result = 39;
            break;
        case 8:
            result = 19;
            break;
        case 9:
            result = 18;
            break;
        default:
            result = 5;
        }
        return result;
    }


    void TDSSPlot::LabelBuses()
        /* Adds text to plot labeling buses */
    {
        int i = 0;
        for (int stop = ActiveCircuit[ActiveActor]->NumBuses, i = 1; i <= stop; i++)
        {
            if (Length(BusLabels[i-1]) > 0)
                if (ActiveCircuit[ActiveActor]->Buses[i - 1]->CoordDefined)
                    AddNewText(ActiveCircuit[ActiveActor]->Buses[i - 1]->x, ActiveCircuit[ActiveActor]->Buses[i - 1]->y, 0x00000000, 8, BusLabels[i - 1]);
        }
    }


    void TDSSPlot::DoMonitorPlot()
    {
        int             Fversion = 0, 
                        FSignature = 0, 
                        iMode = 0;
        float           hr = 0.0, 
                        S = 0.0;
        unsigned int    i = 0, 
                        Nread = 0, 
                        RecordSize = 0, 
                        RecordBytes = 0;
        float           sngBuffer[100/*# range 1..100*/] = {};      // a big buffer

        TMonitorStrBuffer dummyRec;
        char*           pStrBuffer = nullptr;
        double          time = 0.0;
        bool            FirstRecord = false, 
                        Hours = false;
        vector< String > ChannelNames;
        String          Str = "";
        bool            ItsAFreqScan = false;
        unsigned int    NumberofRecords = 0;
        vector<double>  Xarray;
        vector<vector<double>>  Yarray;
        int             iCount = 0;
        unsigned int    iChannel = 0;


        Yarray.resize(100);
        /* Plot designated channels in monitor designated by ObjectName */
        if (MonitorClass[ActiveActor]->SetActive(ObjectName))
        {
            /*# with TMonitorObj(MonitorClass[ActiveActor].GetActiveObj) do */
            {
                auto with0 = (TMonitorObj*)MonitorClass[ActiveActor]->GetActiveObj();
                {
                    with0->Save(); // Save present buffer
                    with0->CloseMonitorStream(ActiveActor);
                    FirstRecord = true;
                    Hours = true;
                    pStrBuffer = &(with0->StrBuffer[0]);
                    /*# with MonitorStream do */
                    {            // Was throwing E2251 (ambiguous olverloaded...) in Linux
                        auto& with1 = with0->MonitorStream;
                        with1.begin(); // Start at the beginning of the Stream
                        with1.Read(&FSignature, sizeof(FSignature));
                        with1.Read(&Fversion, sizeof(Fversion));
                        with1.Read(&RecordSize);
                        with1.Read(&iMode, sizeof(iMode));
                        with1.Read(dummyRec);
                    }
                    AuxParser[ActiveActor]->set_WhiteSpaceChars("");
                    AuxParser[ActiveActor]->SetCmdString((String)pStrBuffer);
                    ChannelNames.resize(RecordSize + 3);
                    for (int stop = RecordSize + 1, i = 0; i <= stop; i++)
                    {
                        AuxParser[ActiveActor]->GetNextParam();
                        ChannelNames[i] = AuxParser[ActiveActor]->MakeString_();
                    }
                    AuxParser[ActiveActor]->ResetDelims();   // restore original delimiters
                    if (CompareText(ChannelNames[0], "Freq") == 0)
                        ItsAFreqScan = true;
                    else
                        ItsAFreqScan = false;

                    // pStr := @StrBuffer;
                    RecordBytes = sizeof(sngBuffer[1 - 1]) * RecordSize;
                    NumberofRecords = (with0->MonitorStream.Size() - with0->MonitorStream.Position()) / RecordBytes;

                    // Allocate arrays for plotting
                    Xarray.resize(NumberofRecords);
                    for (int stop = Channels.size(), i = 0; i < stop; i++)
                        Yarray[i].resize(NumberofRecords);
                    iCount = -1;  // Loop count
                    while (!(with0->MonitorStream.Position() >= with0->MonitorStream.Size()))
                    {
                        /*# with MonitorStream do */
                        {
                            auto& with2 = with0->MonitorStream;
                            with2.Read(&hr, sizeof(hr));
                            with2.Read(&S, sizeof(S));
                            Nread = with2.Read(sngBuffer, RecordBytes);
                        }
                        if (Nread < RecordBytes)
                            break;
                        iCount++;
                        if (FirstRecord)
                        {
                            if ((S > 0.0) && (S < 100.0))
                                Hours = false;
                        }
                        if (ItsAFreqScan)
                            time = hr; // frequency value
                        else
                            if (Hours)
                                time = hr + S / 3600.0; // in hrs
                            else
                                time = hr * 3600.0 + S; // in sec
                        Xarray[iCount] = time;
                        for (int stop = Channels.size(), i = 0; i < stop; i++)
                        {
                            iChannel = Channels[i];
                            if (iChannel > 0 && iChannel <= RecordSize)  // check for legal channel number
                            {
                                Yarray[i][iCount] = sngBuffer[iChannel - 1] / Bases[i];
                            }
                        }
                        FirstRecord = false;
                    }
                    with0->CloseMonitorStream(ActiveActor);

                    // Add the curves to the plot
                    ActiveColorIdx = 0;
                    for (int stop = Channels.size(), i = 0; i < stop; i++)
                    {
                        AddNewCurve(&(Xarray[0]), &(Yarray[i][0]), iCount + 1, NextColor(), 2, 0, false, 1, ChannelNames[Channels[i]]);
                    }
                    if (ItsAFreqScan)
                        Str = "Frequency, Hz";
                    else
                        if (Hours)
                            Str = "Time, H";
                        else
                            Str = "Time, s";
                    Set_XaxisLabel(Str);
                    Str = "Mag";
                    Set_YaxisLabel(Str);
                    if (Channels[0] <= RecordSize)
                        Str = ObjectName + ": " + ChannelNames[Channels[0] + 1];
                    for (int stop = Channels.size(), i = 1; i < stop; i++)
                        if (Channels[i] <= RecordSize)
                            Str = Str + Format(", %s", ChannelNames[Channels[i] + 1].c_str());
                    Set_ChartCaption(Str);
                }
            } /* With */
            Set_AutoRange(2.0); // 2% rim
            //***      ShowGraph;

              // de-Allocate arrays used for plotting
            Xarray.clear();
            for (int stop = Channels.size(), i = 0; i < stop; i++)
                Yarray[i].clear();
            Yarray.clear();
        }
        else
            DoSimpleMsg(String("Monitor \"") + ObjectName + "\" not found.", 200);
    }


    void TDSSPlot::DoPriceShapePlot(const String PriceShapeName)
    {
        TPriceShapeObj* Price_Shape = nullptr;
        vector <double> Xarray;
        double          X = 0.0, 
                        Xinc = 0.0;
        int             i = 0,
                        Xsize = 0;
        String          XLabel = "",
                        S = "";
        bool            UseXarray = false;

        Price_Shape = (TPriceShapeObj*) PriceShapeClass[ActiveActor]->Find(PriceShapeName);
        if (Price_Shape == nullptr)
        {
            DoSimpleMsg(String("PriceShape object not found: \"") + PriceShapeName + "\"", 87341);
            return;
        }
        UseXarray = false;
        Xarray.clear();
        Xsize = 0; // Init
        if (Price_Shape->Interval != 0.0)
            /*# with Price_Shape do */
        { // have to gen up Xarray
            auto with0  = Price_Shape;
            UseXarray   = true;
            Xsize       = sizeof(Xarray[1]) * with0->get_FNumPoints();
            Xarray.resize(Xsize + 1); // SetLength(Xarray, Numpoints);
            X = 0.0;
            if (with0->Interval * with0->get_FNumPoints() < 1.0)
            {
                Xinc = with0->Interval * 3600.0; // Plot secs
                XLabel = "Seconds";
            }
            else
            {
                Xinc = with0->Interval;
                XLabel = "Hours";
            }
            for (int stop = with0->get_FNumPoints(), i = 1; i <= stop; i++)
            {
                Xarray[i] = X;
                X = X + Xinc;
            }
        }

        // ** already exists MakeNewGraph;
        S = String("PriceShape.") + PriceShapeName;
        Set_Caption(S);
        S = String("PriceShape = ") + PriceShapeName;
        Set_ChartCaption(S);
        Set_XaxisLabel(XLabel);
        Set_YaxisLabel("Price");
        if (UseXarray)
            AddNewCurve(&(Xarray[0]), Price_Shape->PriceValues, Price_Shape->get_FNumPoints(), Color1, 1, 0, false, 1, PriceShapeName);
        else
            AddNewCurve(&(Price_Shape->Hours[0]), Price_Shape->PriceValues, Price_Shape->get_FNumPoints(), Color1, 1, 0, false, 1, PriceShapeName);
        Set_KeepAspectRatio(false);
        if (UseXarray)
            Xarray.clear();
        Set_AutoRange(2.0); // 2% rim
        //***   ShowGraph; { Form Freed on close }
    }

    void TDSSPlot::DoProfilePlot()

        /* Voltage profile plot. Tom Short Plot with lines */
    {
        int                 iEnergyMeter = 0;
        int                 iphs = 0;
        int                 iphs2 = 0;
        TEnergyMeterObj*    ActiveEnergyMeter = nullptr;
        TDSSCktElement*     PresentCktElement = nullptr;
        TDSSBus*            Bus1 = nullptr; 
        TDSSBus*            Bus2 = nullptr;
        double              puV1 = 0.0, 
                            puV2 = 0.0;
        double              RangeLoY = 0.0, 
                            RangeHiY = 0.0;
        double              DenomLL = 0.0, 
                            DenomLN = 0.0, 
                            LenScale = 0.0, 
                            RangeScale = 0.0;
        String              S = "";
        TColor              MyColor = 0;
        TPenStyle           LineType = 0;
        TDSSGraphProperties* DSSGraphProps = new TDSSGraphProperties();


        /* New graph created before this routine is entered */
        switch (PhasesToPlot)
        {
        case PROFILELL: case PROFILELLALL: case PROFILELLPRI:
            S = "L-L Voltage Profile";
            break;
        default:
            S = "L-N Voltage Profile";
        }
        Set_Caption(S);
        Set_ChartCaption(S);
        if (ProfileScale == PROFILE120KFT)
        {
            Set_XaxisLabel("Distance (kft)");
            Set_YaxisLabel("120 Base Voltage");
            DenomLN = 1000.0 / 120.0;
            DenomLL = 1732.0 / 120.0;
            LenScale = 3.2809;
            RangeScale = 120.0;
        }
        else
        {
            Set_XaxisLabel("Distance (km)");
            Set_YaxisLabel("p.u. Voltage");
            DenomLN = 1000.0;
            DenomLL = 1732.0;
            LenScale = 1.0;
            RangeScale = 1.0;
        }
        Get_Properties(DSSGraphProps);
        /*# with DSSGraphProps do */
        {
            DSSGraphProps->GridStyle = gsDotLines;
            DSSGraphProps->ChartColor = 0x00FFFFFF;
            DSSGraphProps->WindColor = 0x00FFFFFF;
            DSSGraphProps->Isometric = false;
            EnableClickonDiagram();
        }
        Set_Properties(DSSGraphProps);
        Set_TextAlignment(1);
        Set_KeyClass(DSSG_LINECLASS); /* Line for searches */
        iEnergyMeter = EnergyMeterClass[ActiveActor]->Get_First();
        while (iEnergyMeter > 0)
        {
            ActiveEnergyMeter = (TEnergyMeterObj*) EnergyMeterClass[ActiveActor]->GetActiveObj();
            /* Go down each branch list and draw a line */
            PresentCktElement = (TDSSCktElement*) ActiveEnergyMeter->BranchList->Get_First();
            while (PresentCktElement != nullptr)
            {
                if (IslineElement(PresentCktElement))
                    /*# with ActiveCircuit[ActiveActor] do */
                {
                    auto with0 = ActiveCircuit[ActiveActor];
                    {
                        with0->Set_ActiveCktElement(PresentCktElement);
                        Bus1 = with0->Buses[PresentCktElement->Terminals[1 - 1].BusRef - 1];
                        Bus2 = with0->Buses[PresentCktElement->Terminals[2 - 1].BusRef - 1];
                        /* Now determin which phase to plot */
                        if ((Bus1->kVBase > 0.0) && (Bus2->kVBase > 0.0))
                            switch (PhasesToPlot)
                            {
                                case
                                /* 3ph only */ PROFILE3PH:
                                    if ((PresentCktElement->Fnphases >= 3) && (Bus1->kVBase > 1.0))
                                        for (int stop = 3, iphs = 1; iphs <= stop; iphs++)
                                        {
                                            puV1 = cabs(with0->Solution->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs))]) / Bus1->kVBase / DenomLN;
                                            puV2 = cabs(with0->Solution->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs))]) / Bus2->kVBase / DenomLN;
                                            AddNewLine(Bus1->DistFromMeter * LenScale, puV1, Bus2->DistFromMeter * LenScale, puV2, ColorArray[iphs - 1], 2, 0, 
                                                Dots, Format("%s.%s",PresentCktElement->ParentClass->Class_Name.c_str(), PresentCktElement->get_Name().c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                                        }
                                    break;
                                    /* Plot all phases present (between 1 and 3) */
                                case PROFILEALL:
                                {
                                    for (int stop = 3, iphs = 1; iphs <= stop; iphs++)
                                        if ((Bus1->FindIdx(iphs) > 0) && (Bus2->FindIdx(iphs) > 0))
                                        {
                                            if (Bus1->kVBase < 1.0)
                                                LineType = psDot;
                                            else
                                                LineType = psSolid;
                                            MyColor = ColorArray[iphs - 1];
                                            puV1 = cabs(with0->Solution->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs))]) / Bus1->kVBase / DenomLN;
                                            puV2 = cabs(with0->Solution->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs))]) / Bus2->kVBase / DenomLN;
                                            AddNewLine(Bus1->DistFromMeter * LenScale, puV1, Bus2->DistFromMeter * LenScale, puV2, MyColor, 2, LineType, 
                                                Dots, Format("%s.%s", PresentCktElement->ParentClass->Class_Name.c_str(), PresentCktElement->get_Name().c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                                        }
                                }
                                break;
                                /* Plot all phases present (between 1 and 3) for Primary only */
                                case PROFILEALLPRI:
                                {
                                    if (Bus1->kVBase > 1.0)
                                        for (int stop = 3, iphs = 1; iphs <= stop; iphs++)
                                            if ((Bus1->FindIdx(iphs) > 0) && (Bus2->FindIdx(iphs) > 0))
                                            {
                                                if (Bus1->kVBase < 1.0)
                                                    LineType = psDot;
                                                else
                                                    LineType = psSolid;
                                                MyColor = ColorArray[iphs - 1];
                                                puV1 = cabs(with0->Solution->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs))]) / Bus1->kVBase / DenomLN;
                                                puV2 = cabs(with0->Solution->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs))]) / Bus2->kVBase / DenomLN;
                                                AddNewLine(Bus1->DistFromMeter * LenScale, puV1, Bus2->DistFromMeter * LenScale, puV2, MyColor, 2, LineType, 
                                                    Dots, Format("%s.%s", PresentCktElement->ParentClass->Class_Name.c_str(), PresentCktElement->get_Name().c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                                            }
                                }
                                break;
                                case PROFILELL:
                                {
                                    if (PresentCktElement->Fnphases >= 3)
                                        for (int stop = 3, iphs = 1; iphs <= stop; iphs++)
                                        {
                                            iphs2 = iphs + 1;
                                            if (iphs2 > 3)
                                                iphs2 = 1;
                                            if ((Bus1->FindIdx(iphs) > 0) && (Bus2->FindIdx(iphs) > 0) && (Bus1->FindIdx(iphs2) > 0) && (Bus2->FindIdx(iphs2) > 0))
                                            {
                                                if (Bus1->kVBase < 1.0)
                                                    LineType = psDot;
                                                else
                                                    LineType = psSolid;
                                                MyColor = ColorArray[iphs - 1];
                                                /*# with Solution do */
                                                {
                                                    auto with1 = with0->Solution;
                                                    puV1 = cabs(csub(with1->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs))], with1->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs2))])) / Bus1->kVBase / DenomLL;
                                                    puV2 = cabs(csub(with1->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs))], with1->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs2))])) / Bus2->kVBase / DenomLL;
                                                }
                                                AddNewLine(Bus1->DistFromMeter * LenScale, puV1, Bus2->DistFromMeter * LenScale, puV2, MyColor, 2, LineType, 
                                                    Dots, Format("%s.%s",PresentCktElement->ParentClass->Class_Name.c_str(), PresentCktElement->get_Name().c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                                            }
                                        }
                                }
                                break;
                                case PROFILELLALL:
                                {
                                    for (int stop = 3, iphs = 1; iphs <= stop; iphs++)
                                    {
                                        iphs2 = iphs + 1;
                                        if (iphs2 > 3)
                                            iphs2 = 1;
                                        if ((Bus1->FindIdx(iphs) > 0) && (Bus2->FindIdx(iphs) > 0) && (Bus1->FindIdx(iphs2) > 0) && (Bus2->FindIdx(iphs2) > 0))
                                        {
                                            if (Bus1->kVBase < 1.0)
                                                LineType = psDot;
                                            else
                                                LineType = psSolid;
                                            MyColor = ColorArray[iphs - 1];
                                            /*# with Solution do */
                                            {
                                                auto with3 = with0->Solution;
                                                puV1 = cabs(csub(with3->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs))], with3->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs2))])) / Bus1->kVBase / DenomLL;
                                                puV2 = cabs(csub(with3->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs))], with3->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs2))])) / Bus2->kVBase / DenomLL;
                                            }
                                            AddNewLine(Bus1->DistFromMeter * LenScale, puV1, Bus2->DistFromMeter * LenScale, puV2, MyColor, 2, LineType, 
                                                Dots, Format("%s.%s", PresentCktElement->ParentClass->Class_Name.c_str(), PresentCktElement->get_Name().c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                                        }
                                    }
                                }
                                break;
                            PROFILELLPRI:
                                {
                                    if (Bus1->kVBase > 1.0)
                                        for (int stop = 3, iphs = 1; iphs <= stop; iphs++)
                                        {
                                            iphs2 = iphs + 1;
                                            if (iphs2 > 3)
                                                iphs2 = 1;
                                            if ((Bus1->FindIdx(iphs) > 0) && (Bus2->FindIdx(iphs) > 0) && (Bus1->FindIdx(iphs2) > 0) && (Bus2->FindIdx(iphs2) > 0))
                                            {
                                                if (Bus1->kVBase < 1.0)
                                                    LineType = psDot;
                                                else
                                                    LineType = psSolid;
                                                MyColor = ColorArray[iphs - 1];
                                                /*# with Solution do */
                                                {
                                                    auto with4 = with0->Solution;
                                                    puV1 = cabs(csub(with4->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs))], with4->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs2))])) / Bus1->kVBase / DenomLL;
                                                    puV2 = cabs(csub(with4->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs))], with4->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs2))])) / Bus2->kVBase / DenomLL;
                                                }
                                                AddNewLine(Bus1->DistFromMeter * LenScale, puV1, Bus2->DistFromMeter * LenScale, puV2, MyColor, 2, LineType, 
                                                    Dots, Format("%s.%s", PresentCktElement->ParentClass->Class_Name.c_str(), PresentCktElement->get_Name().c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                                            }
                                        }
                                }
                                break;
                                default: // plot just the selected phase
                                    iphs = PhasesToPlot;
                                    if ((Bus1->FindIdx(iphs) > 0) && (Bus2->FindIdx(iphs) > 0))
                                    {
                                        if (Bus1->kVBase < 1.0)
                                            LineType = psDot;
                                        else
                                            LineType = psSolid;
                                        MyColor = ColorArray[iphs - 1];
                                        puV1 = cabs(with0->Solution->NodeV[Bus1->GetRef(Bus1->FindIdx(iphs))]) / Bus1->kVBase / DenomLN;
                                        puV2 = cabs(with0->Solution->NodeV[Bus2->GetRef(Bus2->FindIdx(iphs))]) / Bus2->kVBase / DenomLN;
                                        AddNewLine(Bus1->DistFromMeter * LenScale, puV1, Bus2->DistFromMeter * LenScale, puV2, MyColor, 2, LineType, 
                                            Dots, Format("%s.%s", PresentCktElement->ParentClass->Class_Name.c_str(), PresentCktElement->get_Name().c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                                    }
                            }
                    }
                }
                PresentCktElement = (TDSSCktElement*) ActiveEnergyMeter->BranchList->Get_Forward();
            }
            iEnergyMeter = EnergyMeterClass[ActiveActor]->Get_Next();
        }
        Set_KeepAspectRatio(false);
        Set_AutoRange(2.0); // 2% rim
        Get_Properties(DSSGraphProps);

      /*# with DSSGraphProps, ActiveCircuit[ActiveActor] do */
        {
            auto with5 = ActiveCircuit[ActiveActor];
            auto with6 = DSSGraphProps;
            {
                // AddNewLine(0.0, NormalMaxVolts, Xmax, NormalMaxVolts, ColorArray[1], 1, psDash, FALSE, 'Upper Limit', False, 0,0,0);
                // AddNewLine(0.0, NormalMinvolts, Xmax, NormalMinvolts, ColorArray[1], 1, psDash, FALSE, 'Lower Limit', False, 0,0,0);

                // --deprecated-- Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
                RangeLoY = 0.90 * RangeScale;
                RangeHiY = 1.10 * RangeScale;
                with6->Xmin = 0.0;
                Set_Range(with6->Xmin, with6->Xmax, RangeLoY, RangeHiY);

                /* Keep this range for quick resetting */
                // --deprecated-- Xmin := RangeLoX;
                // --deprecated-- Xmax := RangeHiX;
                with6->Ymin = RangeLoY;
                with6->YMax = RangeHiY;
                Set_LineWidth(3);
                Set_DataColor(0x000000FF);
                MoveTo(0.0, with5->NormalMaxVolts * RangeScale);
                DrawTo(with6->Xmax, with5->NormalMaxVolts * RangeScale);
                MoveTo(0.0, with5->NormalMinVolts * RangeScale);
                DrawTo(with6->Xmax, with5->NormalMinVolts * RangeScale);
            }
        }

        Set_Properties(DSSGraphProps);
        Set_AutoRange(2.0); // 2% rim
        //****   ShowGraph;
    }


    void TDSSPlot::MarkSpecialClasses()
        /*
           Place markers  at certain locations for special types of devices
        */
    {
        if (ActiveCircuit[ActiveActor]->MarkTransformers)
            MarkTheTransformers();
        if (ActiveCircuit[ActiveActor]->MarkCapacitors)
            MarkTheCapacitors();
        if (ActiveCircuit[ActiveActor]->MarkRegulators)
            MarkTheRegulators();
        if (ActiveCircuit[ActiveActor]->MarkPVSystems)
            MarkThePVSystems();
        // If ActiveCircuit[ActiveActor].MarkPVSystems2   Then MarkThePVSystems2;
        if (ActiveCircuit[ActiveActor]->MarkStorage)
            MarkTheStorage();
        // If ActiveCircuit[ActiveActor].MarkStorage2     Then MarkTheStorage2;
        if (ActiveCircuit[ActiveActor]->MarkFuses)
            MarkTheFuses();
        if (ActiveCircuit[ActiveActor]->MarkReclosers)
            MarkTheReclosers();
        if (ActiveCircuit[ActiveActor]->MarkRelays)
            MarkTheRelays();
        if (ShowSubs)
            MarkSubTransformers();
        AddBusMarkers();
    }


    void TDSSPlot::MarkSubTransformers()
    {
        /* Mark Locations of Substation Transformers */
        pTransf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_First();
        Set_LineWidth(4);
        while (pTransf != nullptr)
        {
            if (pTransf->FEnabled)
                if (pTransf->IsSubstation)
                {
                    Bus2Idx = pTransf->Terminals[2 - 1].BusRef - 1;
                    if (Bus2Idx > 0)
                        if (ActiveCircuit[ActiveActor]->Buses[Bus2Idx]->CoordDefined)
                        {
                            AddNewMarker(ActiveCircuit[ActiveActor]->Buses[Bus2Idx]->x, ActiveCircuit[ActiveActor]->Buses[Bus2Idx]->y, 0x000000FF, 36, 4);
                            if (Length(pTransf->SubstationName) > 0)
                                AddNewText(ActiveCircuit[ActiveActor]->Buses[Bus2Idx]->x, ActiveCircuit[ActiveActor]->Buses[Bus2Idx]->y, 0x00000000, 10, ("  " + pTransf->SubstationName));
                        }
                }
            pTransf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_Next();
        }
    }


    void TDSSPlot::MarkTheCapacitors()
    {
        TCapacitorObj*  pCapacitor = nullptr;
        int             BusIdx = 0;
        TDSSBus*        MyBus = nullptr;

        pCapacitor = (TCapacitorObj*) ActiveCircuit[ActiveActor]->ShuntCapacitors.Get_First();
        while (pCapacitor != nullptr)
        {
            if (pCapacitor->FEnabled)
            {
                BusIdx = pCapacitor->Terminals[1 - 1].BusRef - 1;
                if (BusIdx > 0)
                    /*# with ActiveCircuit[ActiveActor] do */
                {
                    auto with0 = ActiveCircuit[ActiveActor];
                    {
                        MyBus = with0->Buses[BusIdx];
                        if (MyBus->CoordDefined)
                        {
                            AddNewMarker(MyBus->x, MyBus->y, 0x000000FF, with0->CapMarkerCode, with0->CapMarkerSize);
                        }
                    }
                }
            }
            pCapacitor = (TCapacitorObj*) ActiveCircuit[ActiveActor]->ShuntCapacitors.Get_Next();
        }
    }


    void TDSSPlot::MarkTheFuses()
    {
        TFuseObj*   pFuse = nullptr;
        int         BusIdx = 0;
        TDSSBus*    MyBus = nullptr;
        TFuse*      FuseClass = nullptr;

        FuseClass = (TFuse*) GetDSSClassPtr("fuse");

        pFuse = (TFuseObj*) FuseClass->ElementList.Get_First();
        while (pFuse != nullptr)
        {
            if (pFuse->FEnabled)
                if (pFuse->FControlledElement->Drawn)
                {
                    BusIdx = pFuse->FControlledElement->Terminals[pFuse->ElementTerminal - 1].BusRef - 1;
                    if (BusIdx > 0)
                        /*# with ActiveCircuit[ActiveActor] do */
                    {
                        auto with0 = ActiveCircuit[ActiveActor];
                        {
                            MyBus = with0->Buses[BusIdx];
                            if (MyBus->CoordDefined)
                            {
                                AddNewMarker(MyBus->x, MyBus->y, 0x000000FF, with0->FuseMarkerCode, with0->FuseMarkerSize);
                            }
                        }
                    }
                }
            pFuse = (TFuseObj*) FuseClass->ElementList.Get_Next();
        }
    }


    void TDSSPlot::MarkTheReclosers()
    {
        TRecloserObj*   pRecloser = nullptr;
        int             BusIdx = 0;
        TDSSBus*        MyBus = nullptr;
        /* Mark only reclosers on Lines that are in the circuit*/
        pRecloser = (TRecloserObj*) RecloserClass->ElementList.Get_First();
        while (pRecloser != nullptr)
        {
            if (pRecloser->FEnabled)
            {
                if (pRecloser->FControlledElement->Drawn)
                {
                    BusIdx = pRecloser->FControlledElement->Terminals[pRecloser->ElementTerminal - 1].BusRef - 1;
                    if (BusIdx > 0)
                        /*# with ActiveCircuit[ActiveActor] do */
                    {
                        auto with0 = ActiveCircuit[ActiveActor];
                        {
                            MyBus = with0->Buses[BusIdx];
                            if (MyBus->CoordDefined)
                            {
                                AddNewMarker(MyBus->x, MyBus->y, 0x0000FF00, with0->RecloserMarkerCode, with0->RecloserMarkerSize);
                            }
                        }
                    }
                }
            }
            pRecloser = (TRecloserObj*) RecloserClass->ElementList.Get_Next();
        }
    }


    void TDSSPlot::MarkTheRelays()
    {
        TRelayObj*      pRelay = nullptr;
        int             BusIdx = 0;
        TDSSBus*        MyBus = nullptr;
        TRelay*         RelayClass = nullptr;

        RelayClass = (TRelay*) GetDSSClassPtr("Relay");

        pRelay = (TRelayObj*) RelayClass->ElementList.Get_First();
        while (pRelay != nullptr)
        {
            if (pRelay->FEnabled)
            {
                if (pRelay->FControlledElement->Drawn)
                {
                    BusIdx = pRelay->FControlledElement->Terminals[pRelay->ElementTerminal - 1].BusRef - 1;
                    if (BusIdx > 0)
                        /*# with ActiveCircuit[ActiveActor] do */
                    {
                        auto with0 = ActiveCircuit[ActiveActor];
                        {
                            MyBus = with0->Buses[BusIdx];
                            if (MyBus->CoordDefined)
                            {
                                AddNewMarker(MyBus->x, MyBus->y, 0x00000080, with0->RelayMarkerCode, with0->RelayMarkerSize);
                            }
                        }
                    }
                }
            }
            pRelay = (TRelayObj*) RelayClass->ElementList.Get_Next();
        }
    }


    void TDSSPlot::MarkThePVSystems()
    {
        TPVsystemObj*   pPVSystem = nullptr;
        int             BusIdx = 0;
        TDSSBus*        MyBus = nullptr;

        pPVSystem = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_First();
        while (pPVSystem != nullptr)
        {
            if (pPVSystem->FEnabled)
            {
                BusIdx = pPVSystem->Terminals[1 - 1].BusRef - 1;
                if (BusIdx > 0)
                    /*# with ActiveCircuit[ActiveActor] do */
                {
                    auto with0 = ActiveCircuit[ActiveActor];
                    {
                        MyBus = with0->Buses[BusIdx];
                        if (MyBus->CoordDefined)
                        {
                            AddNewMarker(MyBus->x, MyBus->y, 0x000000FF, with0->PVMarkerCode, with0->PVMarkerSize);
                        }
                    }
                }
            }
            pPVSystem = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
        }
    }


    void TDSSPlot::MarkTheStorage()
    {
        TStorageObj*    pStorage = nullptr;
        int             BusIdx = 0;
        TDSSBus*        MyBus = nullptr;

        pStorage = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_First();
        while (pStorage != nullptr)
        {
            if (pStorage->FEnabled)
            {
                BusIdx = pStorage->Terminals[1 - 1].BusRef - 1;
                if (BusIdx > 0)
                    /*# with ActiveCircuit[ActiveActor] do */
                {
                    auto with0 = ActiveCircuit[ActiveActor];
                    {
                        MyBus = with0->Buses[BusIdx];
                        if (MyBus->CoordDefined)
                        {
                            AddNewMarker(MyBus->x, MyBus->y, 0x000000FF, with0->StoreMarkerCode, with0->StoreMarkerSize);
                        }
                    }
                }
            }
            pStorage = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
        }
    }


    void TDSSPlot::MarkTheRegulators()
    {
        TRegControlObj* pRegControl = nullptr;
        TTransfObj*     pXfmr = nullptr;
        int             BusIdx = 0;
        TDSSBus*        MyBus = nullptr;

        pRegControl = (TRegControlObj*) ActiveCircuit[ActiveActor]->RegControls.Get_First();
        while (pRegControl != nullptr)
        {
            if (pRegControl->FEnabled)
            {
                pXfmr = pRegControl->Get_Transformer();
                BusIdx = pXfmr->Terminals[pRegControl->Get_Winding() - 1].BusRef - 1;
                if (BusIdx > 0)
                    /*# with ActiveCircuit[ActiveActor] do */
                {
                    auto with0 = ActiveCircuit[ActiveActor];
                    {
                        MyBus = with0->Buses[BusIdx];
                        if (MyBus->CoordDefined)
                        {
                            AddNewMarker(MyBus->x, MyBus->y, 0x000000FF, with0->RegMarkerCode, with0->RegMarkerSize);
                        }
                    }
                }
            }
            pRegControl = (TRegControlObj*) ActiveCircuit[ActiveActor]->RegControls.Get_Next();
        }
    }


    void TDSSPlot::MarkTheTransformers()
    {
        int     Bus1Idx = 0;
        int     Bus2Idx = 0;
        double  Xtr = 0.0, 
                Ytr = 0.0;
        /* Mark Locations of  Transformers */
        pTransf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_First();
        Set_LineWidth(1);
        while (pTransf != nullptr)
        {
            if (pTransf->FEnabled)
                if (!pTransf->IsSubstation)
                {
                    Bus1Idx = pTransf->Terminals[1 - 1].BusRef - 1;
                    Bus2Idx = pTransf->Terminals[2 - 1].BusRef - 1;
                    if ((Bus1Idx > 0) && (Bus2Idx > 0))
                        /*# with ActiveCircuit[ActiveActor] do */
                    {
                        auto with0 = ActiveCircuit[ActiveActor];
                        if (with0->Buses[Bus1Idx]->CoordDefined || with0->Buses[Bus2Idx]->CoordDefined)
                        {
                            if (with0->Buses[Bus1Idx]->CoordDefined && with0->Buses[Bus2Idx]->CoordDefined)
                            {
                                Xtr = (with0->Buses[Bus1Idx]->x + with0->Buses[Bus2Idx]->x) / 2.0;
                                Ytr = (with0->Buses[Bus1Idx]->y + with0->Buses[Bus2Idx]->y) / 2.0;
                            }
                            else
                                if (with0->Buses[Bus1Idx]->CoordDefined)
                                {
                                    Xtr = with0->Buses[Bus1Idx]->x;
                                    Ytr = with0->Buses[Bus1Idx]->y;
                                }
                                else
                                {
                                    Xtr = with0->Buses[Bus2Idx]->x;
                                    Ytr = with0->Buses[Bus2Idx]->y;
                                }
                            AddNewMarker(Xtr, Ytr, 0x000000FF, with0->TransMarkerCode, with0->TransMarkerSize);
                        }
                    }
                }
            pTransf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_Next();
        }
    }

    /* TGenPlotItem */

    /*
    TGenPlotItem::TGenPlotItem()
    {
        Value = 0;
        Name.clear();
    }


     TGenPlotItem::~TGenPlotItem()
    {
        Name = "";
        // todo check:  inherited::Destroy();
    }

    /* TGenPlotItemList */

    /*
    TGenPlotItemList::~TGenPlotItemList()
    {
                int i = 0;
        for (int stop = size() - 1, i = 0; i <= stop; i++)
            delete ((TGenPlotItem*)items[i]);
        
        // todo check:  inherited::Destroy();
    }
    */

    String TDSSPlot::QuantityString()
    {
        String result;
        switch (Quantity)
        {
        case pqVoltage:
            result = "Voltage";
            break;
        case pqPower:
            result = "Power";
            break;
        case pqCurrent:
            result = "Current";
            break;
        case pqLosses:
            result = "Loss Density";
            break;
        case pqCapacity:
            result = "Capacity";
            break;
        case pqNone:
        {
            if (PlotType == ptGeneralCircuitPlot)
                result = FGeneralCircuitPlotQuantity;
            else
                result = "";
        }
        break;
        default:
            result = "";
        }
        return result;
    }


    void TDSSPlot::SetMaxScale()
    {
        if (!MaxScaleIsSpecified)
            switch (Quantity)
            {
            case pqVoltage:
            {
            }
            break;
            case pqLosses:
            {
                MaxScale = 0.0;
                pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
                while (pLine != nullptr)
                {
                    if (pLine->FEnabled)
                        /*# with pLine do */
                    {
                        // ----ActiveTerminalIdx := 1;
                        MaxScale = max<double>(MaxScale, abs(double(pLine->Get_Losses(ActiveActor).re) / pLine->Len));
                    }
                    pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_Next();
                }
                MaxScale = MaxScale * 0.001;
            }
            break;
            case pqPower:
            {
                MaxScale = 0.0;
                pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
                while (pLine != nullptr)
                {
                    if (pLine->FEnabled)
                        /*# with pLine do */
                    {
                        // ----ActiveTerminalIdx := 1;
                        MaxScale = max<double>(MaxScale, abs(pLine->Get_Power(1, ActiveActor).re));
                    }
                    pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_Next();
                }
                MaxScale = MaxScale * 0.001;
            }
            break;
            case pqCurrent:
            {
            }
            break;
            case pqCapacity:
            {
            }
            break;
            case pqNone:
            {
                if (PlotType == ptGeneralCircuitPlot)
                {
                    pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
                    while (pLine != nullptr)
                    {
                        if (pLine->FEnabled)
                            /*# with pLine do */
                        {
                            // ----ActiveTerminalIdx := 1;
                            MaxScale = max<double>(MaxScale, abs(pLine->GeneralPlotQuantity));
                        }
                        pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_Next();
                    }
                }
            }
            break;
            default:
                break;
            }
    }


    void TDSSPlot::DoGeneralCircuitPlot()
    {
        TPenStyle LineStyleType;

        /* Draw the lines With the thickness proportional to the data loaded in the general line data file */
        pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
        /*# with ActiveCircuit[ActiveActor] do */
        {
            auto with0 = ActiveCircuit[ActiveActor];
            while (pLine != nullptr)
            {
                if (pLine->FEnabled)
                {
                    pLine->Drawn = true;
                    with0->Set_ActiveCktElement(pLine);
                    Bus1Idx = pLine->Terminals[1 - 1].BusRef - 1;
                    Bus2Idx = pLine->Terminals[2 - 1].BusRef - 1;
                    if (with0->Buses[Bus1Idx]->CoordDefined && with0->Buses[Bus2Idx]->CoordDefined)
                    {
                        if (pLine->IsSwitch)
                            AddNewLine(with0->Buses[Bus1Idx]->x, with0->Buses[Bus1Idx]->y, with0->Buses[Bus2Idx]->x, with0->Buses[Bus2Idx]->y, TColor(0x0080FF), 1, Style(1), Dots, Format("Line.%s", pLine->LName.c_str()), with0->MarkSwitches, with0->SwitchMarkerCode, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                        else
                            if (pLine->IsIsolated)
                                AddNewLine(with0->Buses[Bus1Idx]->x, with0->Buses[Bus1Idx]->y, with0->Buses[Bus2Idx]->x, with0->Buses[Bus2Idx]->y, 0x00FF00FF, 3, Style(1), Dots, Format("Line.%s", pLine->LName.c_str()), with0->MarkSwitches, with0->SwitchMarkerCode, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                            else
                            {
                                if (pLine->Fnphases == 1)
                                    LineStyleType = Style(SinglePhLineStyle);
                                else
                                    LineStyleType = Style(ThreePhLineStyle);
                                AddNewLine(with0->Buses[Bus1Idx]->x, with0->Buses[Bus1Idx]->y, with0->Buses[Bus2Idx]->x, with0->Buses[Bus2Idx]->y, GetColor(), Thickness(), LineStyleType, Dots, Format("Line.%s", pLine->LName.c_str()), false, 0, with0->NodeMarkerCode, with0->NodeMarkerWidth);
                            }
                        if (Labels)
                            DoBusLabels(Bus1Idx + 1, Bus2Idx + 1);
                    }
                }
                pLine = (TLineObj*) with0->Lines.Get_Next();
            }
        }
        // AddBusMarkers; // Add default bus markers to line plot
    }


    void TDSSPlot::LoadGeneralLineData()
    {
        TTextRec    F;
        String      Line    = "";
        String      Param   = "";
        String      LineNm  = "";
        int         i = 0;
        TLine*      LineClass = nullptr;
        double      MaxValue = 0.0;
        double      MinValue = 0.0;
        bool        IsLine = false;


        LineClass = (TLine*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("Line"));

        /* Initialize General Line Quantity */
        pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
        while (pLine != NULL)
        {
            if (pLine->FEnabled)
                pLine->GeneralPlotQuantity = 0.0;
            pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_Next();
        }
        try
        {
            AssignFile(F, ObjectName);
            Reset(F);
            IOResultToException();
            ReadLn(F, Line); // Get FieldName
            AuxParser[ActiveActor]->SetCmdString(Line);
            AuxParser[ActiveActor]->GetNextParam(); /* Bus Name */
            for (int stop = ValueIndex, i = 1; i <= stop; i++)
                AuxParser[ActiveActor]->GetNextParam();
            FGeneralCircuitPlotQuantity = AuxParser[ActiveActor]->MakeString_();

            /* Find min and max */
            MaxValue = -1.0E50;
            MinValue = 1.0E50;
            while (!Eof(F))
            {
                ReadLn(F, Line);
                if (Line.size() > 0)
                {
                    AuxParser[ActiveActor]->SetCmdString(Line);
                    AuxParser[ActiveActor]->GetNextParam(); /* Branch Name */
                    Param = AuxParser[ActiveActor]->MakeString_();

                    /* Look for a line with this name */
                    IsLine = true;
                    if (Param.find(".") != String::npos)
                    {
                        if (CompareTextShortest(Param, "line") == 0)
                            LineNm = Param.substr(Param.find(".") + 1, Param.size());
                        else
                            IsLine = false;
                    }
                    else
                        LineNm = Param;
                    if (IsLine)
                        if (LineClass->SetActive(LineNm))
                        {
                            for (int stop = ValueIndex, i = 1; i <= stop; i++)
                                AuxParser[ActiveActor]->GetNextParam();
                            if (Length(AuxParser[ActiveActor]->MakeString_()) > 0)
                            { /* Ignore empty fields */
                              /*# with TLineObj(LineClass.GetActiveObj) do */
                                {
                                    auto with0 = (TLineObj*) LineClass->GetActiveObj();
                                    {
                                        with0->GeneralPlotQuantity = AuxParser[ActiveActor]->MakeDouble_();
                                        MaxValue = max<double>(with0->GeneralPlotQuantity, MaxValue);
                                        MinValue = min<double>(with0->GeneralPlotQuantity, MinValue);
                                    }
                                }
                            }
                        }
                }
            } /* WHILE */
        }
        catch (...)
        {
        }
        CloseFile(F);
    }


    void GetS(int Ncond, int Quantity, String S1, String S2, pComplexArray cBuffer, int k, double* kVBase1)
    {
        switch (Quantity)
        {
        case vizPOWER:
        {
            S1 = Format("%-.6g + j", cBuffer[k].re);
            S2 = Format("%-.6g kVA", cBuffer[k].im);
        }
        break;
        case vizVOLTAGE:
        {
            if (k <= Ncond)
                S1 = Format("%-.6g", cabs(cBuffer[k]) / kVBase1[1 - 1]);
            else
                S1 = Format("%-.6g", cabs(cBuffer[k]) / kVBase1[2 - 1]);
            S2 = Format(" /_ %8.2f", cdang(cBuffer[k]));
        }
        break;
        default:
            S1 = Format("%-.6g", cabs(cBuffer[k]));
            S2 = Format(" /_ %8.2f", cdang(cBuffer[k]));
        }
    }


    void DrawArrow(double Y, const String Txt1, const String Txt2, int iopt, double Xmx, String arrowLeft, String arrowright)
    {
        Set_FontStyle(2);
        if (iopt == 1)
        {
            MoveTo(0.0, Y);
            DrawTo(100.0, Y);
            CenteredText15(15.0, (Y + 2.0), 10, Txt1);
            CenteredText15(60.0, (Y + 2.0), 10, Txt2);
            CenteredText15(90.0, (Y + 2.0), 10, arrowright);

            // idx := AddTextLabel(50.0, (Y+1.0), clBlack, , 0);
        }
        else
        {
            MoveTo(Xmx, Y);
            DrawTo(Xmx - 100.0, Y);
            CenteredText15(Xmx - 90.0, (Y + 2.0), 10, arrowLeft);
            CenteredText15(Xmx - 60.0, (Y + 2.0), 10, Txt1);
            CenteredText15(Xmx - 20.0, (Y + 2.0), 10, Txt2);
            // idx := AddTextLabel(Xmx-50, (Y+1.0), clBlack, Arrowleft+Txt, 0);
        }

        // TextLabels[idx].Font.Style := [fsBold];


        /* ------------------------------------------------------------------- */
        /* Plot Lines representing the phases and ground */
    }


    void TDSSPlot::DoVisualizationPlot(TDSSCktElement* Element, int Quantity)
    {
        vector <complex>cBuffer;
        int             Nterm = 0, Ncond = 0;
        double          kVBase1[3/*# range 1..2*/] = {};
        double          TopY = 0.0, 
                        Xmx = 0.0;
        double          xx = 0.0;
        int             i = 0, 
                        j = 0, 
                        k = 0;
        int             Idx = 0;
        String          S1 = "", 
                        S2 = "", 
                        S = "", 
                        arrowLeft = "", 
                        arrowright = "";
        String          Fname = "";
        bool            CBufferAllocated = false;
        complex         cResidual = CZero;


        TDSSGraphProperties* ActiveGraphProps = new TDSSGraphProperties;

        // RangeLoX, RangeHiX, RangeLoY, RangeHiY: Double;

        /* ----------------------INTERNAL FUNCTIONS--------------------------- */
        Ncond = Element->Fnconds;
        Nterm = Element->Fnterms;
        CBufferAllocated = false;
        Element->ComputeIterminal(ActiveActor);
        Element->ComputeVterminal(ActiveActor);
        Xmx = 300.0; // don't use Xmax -- already used
        for (int stop = 2, i = 1; i <= stop; i++)
            kVBase1[i - 1] = 1.0;
        switch (Quantity)
        {
        case vizVOLTAGE:
        {
            arrowLeft = "^ ";
            arrowright = " ^";
        }
        break;
        default:
            arrowLeft = "<- ";
            arrowright = " ->";
        }
        Fname = GetOutputDirectory() + CircuitName_[ActiveActor];
        switch (Quantity)
        {
        case vizVOLTAGE:
        {
            Fname = Fname + Format("%s_%s_V.DSV", Element->ParentClass->Class_Name.c_str(), Element->LName.c_str());
            cBuffer = Element->Vterminal;
            for (int stop = min<int>(2, Nterm), i = 1; i <= stop; i++)
                kVBase1[i - 1] = max<double>(1.0, 1000.0 * ActiveCircuit[ActiveActor]->Buses[Element->Terminals[i - 1].BusRef - 1]->kVBase);
        }
        break;
        case vizCURRENT:
        {
            Fname = Fname + Format("%s_%s_I.DSV", Element->ParentClass->Class_Name.c_str(), Element->LName.c_str());
            cBuffer = Element->Iterminal;
        }
        break;
        case vizPOWER:
        {
            Fname = Fname + Format("%s_%s_PQ.DSV", Element->ParentClass->Class_Name.c_str(), Element->LName.c_str());
            cBuffer.resize(Element->Yorder + 1);
            CBufferAllocated = true;
            /*# with Element do */
            {
                for (int stop = Element->Yorder, i = 1; i <= stop; i++)
                    cBuffer[i] = cmulreal(cmul(Element->Vterminal[i - 1], conjg(Element->Iterminal[i - 1])), 0.001);
            }
        }
        break;
        }
        if (MakeNewGraph(Fname) == 0)
        {
            DoSimpleMsg("Make New Plot failed in DSSPlot - visualization plot.", 8734);
            return;
        }
        Get_Properties(ActiveGraphProps);
        xx = 0.0;
        /*# with ActiveGraphProps do */
        {
            auto with3 = ActiveGraphProps;
            with3->ChartColor = 0x00FFFFFF;
            with3->WindColor = 0x00FFFFFF;
            with3->GridStyle = gsNone;
            Set_NoScales(); // Set for no scales on X or Y
            S1 = Element->ParentClass->Class_Name + "." + UpperCase(Element->LName);
            switch (Quantity)
            {
            case vizVOLTAGE:
                S = S1 + " Voltages";
                break;
            case vizCURRENT:
                S = S1 + " Currents";
                break;
            case vizPOWER:
                S = S1 + " Powers";
                break;
            }
            Set_Caption(S);
            Set_ChartCaption(S);

            /* Draw a box */
            TopY = 10.0 + (Ncond + 1) * 10.0;
            Rectangle(100.0, 10.0, Xmx - 100.0, TopY);
            Idx = AddTextLabel(Xmx / 2.0, 15.0, 0x00000000, S1, 0);
            BoldTextLabel(Idx);

            /* Draw the Ground Plane */
            Set_LineWidth(7);
            Set_DataColor(0x00808080);
            MoveTo(0.0, 0.0);
            DrawTo(Xmx, 0.0);
            Set_DataColor(0x00000000);

            /* Put the Quantities on The Box */
            k = 0;
            for (int stop = min<int>(2, Nterm), i = 1; i <= stop; i++)
            {
                Set_LineWidth(3);
                for (int stop = Element->Fnphases, j = 1; j <= stop; j++)
                {
                    k++;
                    GetS(Ncond, Quantity, S1, S2, &(cBuffer[0]), k, kVBase1);
                    DrawArrow(TopY - j * 10.0, S1, S2, i, Xmx, arrowLeft, arrowright);
                }
                Set_LineWidth(1);
                for (int stop = Ncond, j = Element->Fnphases + 1; j <= stop; j++)
                {
                    k++;
                    GetS(Ncond, Quantity, S1, S2, &(cBuffer[0]), k, kVBase1);
                    DrawArrow(TopY - j * 10.0, S1, S2, i, Xmx, arrowLeft, arrowright);
                }

                /* Add Residual Current */
                if (Quantity == vizCURRENT)
                {
                    cResidual = CZero;
                    for (int stop = Ncond, j = 1; j <= stop; j++)
                        caccum(cResidual, cnegate(cBuffer[j + (i - 1) * Ncond]));
                    S1 = Format("%-.6g", cabs(cResidual));
                    S2 = Format(" /_ %8.2f", cdang(cResidual));
                    DrawArrow(-10.0, S1, S2, i, Xmx, arrowLeft, arrowright);
                }

                /* Draw Bus and Label */
                Set_LineWidth(7);
                switch (i)
                {
                case 1:
                    xx = -5.0;
                    break;
                case 2:
                    xx = Xmx + 5.0;
                    break;
                }
                MoveTo(xx, 5.0);
                DrawTo(xx, TopY - 5.0);
                switch (i)
                {
                case 1:
                    xx = 25;
                    break;
                case 2:
                    xx = Xmx - 25.0;
                    break;
                }
                CenteredText15(xx, TopY, 10, UpperCase(Element->GetBus(i)));
            }
            switch (Quantity)
            {
            case vizVOLTAGE:
                S = " Voltages";
                break;
            case vizCURRENT:
                S = " Currents";
                break;
            case vizPOWER:
                S = " Powers";
                break;
            }
            Set_Caption(S);
            Set_AutoRange(5.0); // 5% rim

            /* OLD WAY
              With  ActiveGraphProps Do Begin

              Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

              {Keep this range for quick resetting}
              Xmin := RangeLoX;
              Xmax := RangeHiX;
              Ymin := RangeLoY;
              Ymax := RangeHiY;
              End;
              Set_Properties(ActiveGraphProps);
              */
            ShowGraph();
        }
        if (CBufferAllocated)
            cBuffer.clear();
    }

    void TDSSPlot::Set_MaxLineThickness(const int Value)
    {
        if (Value > 0)
            FMaxLineThickness = Value;
    }

    void DSSPlot_initialization()
    {
        DSSPlotObj = nullptr; // Instantiate only if Plot command issued
        SinglePhLineStyle = 1;
        ThreePhLineStyle = 1;
    }



    void DSSPlot_finalization()
    {
        if ((DSSPlotObj != nullptr))
            delete DSSPlotObj;
    }

    class DSSPlot_unit
    {
    public:
        DSSPlot_unit()
        {
            DSSPlot_initialization();
        }
        ~DSSPlot_unit() { DSSPlot_finalization(); }
    };
    DSSPlot_unit _DSSPlot_unit;

} // namespace DSSPlot