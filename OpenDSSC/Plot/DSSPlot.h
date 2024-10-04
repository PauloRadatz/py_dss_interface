#ifndef DSSPlotH
#define DSSPlotH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
  Unit for interfacing to Plotting form
  3/29/03
  8/13/2008
*/
/*$M+*/
/*$WARN UNIT_PLATFORM OFF*/


#include "System.h"

#include "Line.h"
#include "Transformer.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "CktElement.h"
#include "DSSCallBackRoutines.h"
#include "d2c_structures.h"

namespace DSSPlot
{

    class TDSSPlot;
    class TGenPlotItem;
    class TGenPlotItemList;
    typedef unsigned char unsignedchar;


    const int vizCURRENT = 1;
    const int vizVOLTAGE = 2;
    const int vizPOWER = 3;


    enum TPlotType {
        ptAutoAddLogPlot,
        ptCircuitplot,
        ptGeneralDataPlot,
        ptGeneralCircuitPlot,
        ptmonitorplot,
        ptdaisyplot,
        ptMeterZones,
        ptLoadShape,
        ptTShape,
        ptPriceShape,
        ptProfile,
        ptScatterPlot,
        ptEvolutionPlot,
        ptEnergyPlot,
        ptMatrixplot,
        ptPhaseVoltage
    };
    enum TPlotQuantity {
        pqVoltage,
        pqCurrent,
        pqPower,
        pqLosses,
        pqCapacity,
        pqNone
    };
    enum TMatrixType {
        pIncMatrix,
        pLaplacian
    }; // The types of matrices that can be plotted


    class TDSSPlot : public TObject {
        typedef TObject inherited;
        friend class TGenPlotItem;
    public:
        int ActiveColorIdx;
        int ColorArray[17/*# range 1..17*/];
        TLineObj* pLine;
        TTransfObj* pTransf;
        int Bus1Idx, Bus2Idx;
        String FGeneralCircuitPlotQuantity;
        int FMaxLineThickness;

        /* Main procedures for the various types of plots ... called from execute */
        void DoGeneralPlot();
        void DoAutoAddPlot();
        void DoTheDaisies();
        void DoCircuitPlot();
        void DoGeneralCircuitPlot();
        void DoMeterZonePlot();
        void DoMonitorPlot();
        void DoProfilePlot();

        /* Misc support procedures */
        void MarkSubTransformers();
        void MarkTheTransformers();
        void MarkTheCapacitors();
        void MarkTheRegulators();
        void MarkThePVSystems();
        // procedure MarkThePVSystems2;
        void MarkTheStorage();
        // procedure MarkTheStorage2;
        void MarkTheFuses();
        void MarkTheReclosers();
        void MarkTheRelays();
        void MarkSpecialClasses();
        void DoBusLabels(const int Idx1, const int Idx2);
        void DoBusLabel(const int Idx, const String BusLabel);
        void LabelBuses();
        void LoadGeneralLineData();
        void SetColorArray();
        void SetMaxScale();
        void AddBusMarkers();
        int GetColor();
        int Thickness();
        double MaxCurrent();
        int NextColor();
        String QuantityString();
        int Style(int Code);
        int GetAutoColor(double Scale);
        unsignedchar GetMarker(int Idx);
        bool CoordinateSame(int i1, int i2);
        int InterpolateGradientColor(int Color1, int Color2, double Ratio);

        /* Property support */
        void Set_MaxLineThickness(const int Value);
        //protected:
    public:
        TPlotType PlotType;
        TMatrixType MatrixType;
        double MaxScale, MinScale; /* applies to Meterzone plots only */
        bool Dots, Labels, ShowLoops, ShowSubs;
        TPlotQuantity Quantity;
        String ObjectName, FeederName;
        String PlotID;
        int ValueIndex, MarkerIdx; /* For General & AutoAdd */
        int PhasesToPlot; // Profile Plot
        int ProfileScale; // CYMDIST or pu/km scaling
        vector< unsigned int > Channels; // for Monitor Plot
        vector< double > Bases; // for Monitor Plot
        int Color1, Color2, Color3;

        /* Tri-color plots */
        double TriColorMax, TriColorMid;
        bool MaxScaleIsSpecified;
        bool MinScaleIsSpecified;
        TStringList DaisyBusList;
        TDSSPlot();
        virtual ~TDSSPlot();
        void Execute();
        void SetDefaults();
        void DSSVizPlot();
        void DoLoadShapePlot(const String LoadShapeName);
        void DoTempShapePlot(const String TempShapeName);
        void DoPriceShapePlot(const String PriceShapeName);
        void DoDI_Plot(const String CaseName, int CaseYear, const int* iRegisters, int iRegisters_maxidx, bool PeakDay, const String MeterName);
        void DoCompareCases(String CaseName1, String CaseName2, String WhichFile, int Reg);
        void DoYearlyCurvePlot(TStringList CaseNames, String WhichFile, const int* iRegisters, int iRegisters_maxidx);
        void DoVisualizationPlot(TDSSCktElement* Element, int Quantity);
    };


    class TGenPlotItem : public TObject {
        typedef TObject inherited;
    public:
        String Name;
        double Value;

    };

    /* List of General Plot Items */


    class TGenPlotItemList : public TList {
        typedef TList inherited;
        friend class TDSSPlot;
        friend class TGenPlotItem;

    };


    extern TDSSPlot* DSSPlotObj;
    extern int SinglePhLineStyle;
    extern int ThreePhLineStyle;

} // namespace DSSplot

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace DSSPlot;
#endif

#endif //  DSSPlotH