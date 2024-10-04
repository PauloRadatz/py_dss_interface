#pragma hdrstop

#include "DSSGraph.h"



#include "DSSGlobals.h"
#include "Utilities.h"
#include "math.h"
#include "PDElement.h"
#include "ParserDel.h"



namespace DSSGraph
{

    TTextRec    ActiveDSSGraphFile,
                ActiveSolutionFileHdl;
    int         TextLabelCount          = 0;
    String      ActiveFileName          = "", 
                ActiveSolutionFileName  = "";
    TDSSGraphProperties* ActiveGraphProps = new TDSSGraphProperties;

    EDSSGraphProblem::EDSSGraphProblem(const String &Msg) : inherited(Msg) {}


    void Checkminmax(const double X, const double Y)
    {
        /*# with ActiveGraphProps do */
        {
            ActiveGraphProps->Xmin = min<double>(X, ActiveGraphProps->Xmin);
            ActiveGraphProps->Xmax = max<double>(X, ActiveGraphProps->Xmax);
            ActiveGraphProps->Ymin = min<double>(Y, ActiveGraphProps->Ymin);
            ActiveGraphProps->YMax = max<double>(Y, ActiveGraphProps->YMax);
        }
    }


    int MakeNewGraph(const String Filename)      // Call this to make a new graph file

    {
        int result = 0;
        result = 0;
        ActiveFileName = "";
        try
        {
            AssignFile(ActiveDSSGraphFile, Filename);
            Rewrite(ActiveDSSGraphFile);
            IOResultToException();
        }
        catch (exception& E)
        {
            {
                DoSimpleMsg(String("Error opening DSSView file: ") + Filename + ", " + (string)E.what(), 45001);
                return result;
            }
        }
        ActiveFileName = Filename;
        ActiveSolutionFileName = ChangeFileExt(ActiveFileName, ".dbl");
        try
        {
            AssignFile(ActiveSolutionFileHdl, ActiveSolutionFileName);
            Rewrite(ActiveSolutionFileHdl);
            IOResultToException();
        }
        catch (exception& E)
        {
            {
                DoSimpleMsg(String("Error opening DSSView Solution file: ") + Filename + ", " + (string)E.what(), 45001);
                return result;
            }
        }
        /*# with ActiveGraphProps do */
        {
            ActiveGraphProps->Xmin = 1.0e50;
            ActiveGraphProps->Xmax = -1.0e50;
            ActiveGraphProps->Ymin = 1.0e50;
            ActiveGraphProps->YMax = -1.0e50;
            ActiveGraphProps->ChartColor = 0x00FFFFFF;
            ActiveGraphProps->WindColor = 0x00FFFFFF;
            ActiveGraphProps->Isometric = false;
            ActiveGraphProps->GridStyle = gsLines;
        }
        TextLabelCount = 0;
        Set_Properties(ActiveGraphProps);
        result = 1;
        return result;
    }


    int64_t WriteActiveCktElementVIToFile() 
    {
        int64_t result = 0;
        unsigned int Count = 0;
        unsigned int CountWritten = 0;
        // get present file position
        /*  Old delphi way
        result = SysUtils.FileSeek(ActiveSolutionFileHdl, ((__int64)0), 1);
        */
        /*# with ActiveCircuit[ActiveActor] do */
        {
            auto with0 = ActiveCircuit[ActiveActor];
            {
                with0->FActiveCktElement->ComputeVterminal(ActiveActor);
                with0->FActiveCktElement->ComputeIterminal(ActiveActor);
                Count = with0->FActiveCktElement->Yorder * 2 * sizeof(double);
                /* Old delphi way
                CountWritten = SysUtils.FileWrite(ActiveSolutionFileHdl, with0->FActiveCktElement->Vterminal, Count);
                if (CountWritten == Count)
                    CountWritten = SysUtils.FileWrite(ActiveSolutionFileHdl, with0->FActiveCktElement->Iterminal, Count);
                */
                //Write(ActiveSolutionFileHdl, with0->FActiveCktElement->Vterminal);
            }
        }
        if (CountWritten != Count)
        {
            /* old delphi way
            SysUtils.FileClose(ActiveSolutionFileHdl);
            */
            //CloseFile(ActiveDSSGraphFile);
            //throw @EDSSGraphProblem ::(String("Aborting. Problem writing solution file: ") + ActiveSolutionFileName);
        }
        return result;
    }


    void AddNewLine(double X1, double Y1, double X2, double Y2, int Color, unsignedchar Thickness, TPenStyle Style, bool Dots, const String LineName, bool MarkCenter, int CenterMarkerCode, int NodeMarkerCode, int NodeMarkerWidth)
    {
        int64_t     Offset = 0;
        String      Bus1 = "", 
                    Bus2 = "";
        int         Bus1Idx = 0,
                    DataCount = 0,
                    NumCust = 0, 
                    TotalCust = 0;
        double      kV_Base = 0.0,
                    Dist = 0.0;
        TPDElement* pDElem = nullptr;

        Offset = WriteActiveCktElementVIToFile();

        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].ActiveCktElement do */
        {
            auto with0 = ActiveCircuit[ActiveActor];
            auto with1 = with0->FActiveCktElement;
            {
                Bus1Idx = with1->Terminals[0].BusRef - 1;
                kV_Base = with0->Buses[Bus1Idx]->kVBase;
                Dist    = with0->Buses[Bus1Idx]->DistFromMeter;
                Bus1    = with1->GetBus(1);
                Bus2    = with1->GetBus(2);
                DataCount = with1->Yorder;
            }
        }
        if (dynamic_cast<TPDElement*>(ActiveCircuit[ActiveActor]->FActiveCktElement))
        {
            pDElem      = (TPDElement*)ActiveCircuit[ActiveActor]->FActiveCktElement;
            NumCust     = pDElem->BranchNumCustomers;
            TotalCust   = pDElem->BranchTotalCustomers;
        }
        else
        {
            NumCust     = 0;
            TotalCust   = 0;
        }      

        System::WriteLn(ActiveDSSGraphFile, Format("Line, \"%s\", \"%s\", \"%s\", %d, %d,  %d, %d, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %d, %d, %d, %d, %d, %d, %d, %d", 
            LineName.c_str(), Bus1.c_str(), Bus2.c_str(), Offset, DataCount, NumCust, TotalCust, kV_Base, Dist, X1, Y1, X2, Y2, Color, Thickness, Style, Dots, MarkCenter, CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth));
        Checkminmax(X1, Y1);
        Checkminmax(X2, Y2);
    }


    void AddNewCurve(pDoubleArray Xarray, pDoubleArray Yarray, int NumPoints, TColor Color, unsignedchar Thickness, TPenStyle Style, bool Curvemarkers, int CurveMarker, const String CurveName)
    {
        int i = 0;
        Write(ActiveDSSGraphFile, Format("Curve, %d, %d, %d, %d, %d, %d, \"%s\"", NumPoints, Color, Thickness, Style, Curvemarkers, CurveMarker, CurveName.c_str()));
        for (int stop = NumPoints, i = 0; i < stop; i++)
            Write(ActiveDSSGraphFile, Format(", %.8g", Xarray[i]));
        for (int stop = NumPoints, i = 0; i < stop; i++)
            Write(ActiveDSSGraphFile, Format(", %.8g", Yarray[i]));
        WriteLn(ActiveDSSGraphFile);

        /*???? Check min and max of curve*/
    }


    void AddNewText(double X1, double Y1, int Color, int Size, String S)
    {
        WriteLn(ActiveDSSGraphFile, Format("Text, %.8g, %.8g, %d, %d, \"%s\"", X1, Y1, Color, Size, S.c_str()));
        Checkminmax(X1, Y1);
    }


    void AddNewCircle(double Xc, double Yc, double Radius, int LineColor, int FColor)
    {
        WriteLn(ActiveDSSGraphFile, Format("Circle, %.8g, %.8g, %.8g, %d, %d", Xc, Yc, Radius, LineColor, FColor));
        Checkminmax(Xc, Yc);
    }


    void AddNewMarker(double X, double Y, int Color, unsignedchar Symbol, unsignedchar Size)
    {
        WriteLn(ActiveDSSGraphFile, Format("Marker, %.8g, %.8g,  %d, %d, %d", X, Y, Color, Symbol, Size));
        Checkminmax(X, Y);
    }

    /*Routines to manage DSSGraph Properties. */
    /*Invoke a Get_Properties to populate the Props struct first then change values*/


    void Set_Properties(TDSSGraphProperties* Props)
    {
        ActiveGraphProps = Props;
         WriteLn(ActiveDSSGraphFile, Format("SetProp, %.8g, %8g, %.8g, %.8g, %d, %d, %d, %d", ActiveGraphProps->Xmin, ActiveGraphProps->Xmax, ActiveGraphProps->Ymin, ActiveGraphProps->YMax, ActiveGraphProps->ChartColor, ActiveGraphProps->WindColor, int(ActiveGraphProps->Isometric), int(ActiveGraphProps->GridStyle)));
    }


    void Get_Properties(TDSSGraphProperties* &Props)
    {
        Props = ActiveGraphProps;
    }

    //\"%s\"" Not needed
    void Set_XaxisLabel(String S)
    {
        WriteLn(ActiveDSSGraphFile, "Xlabel, \"" + S + "\"");
    }


    void Set_YaxisLabel(String S)
    {
        WriteLn(ActiveDSSGraphFile, "Ylabel, \"" + S + "\"");
    }


    void Set_Caption(String S)
    {
        WriteLn(ActiveDSSGraphFile, "Caption, \"" + S + "\"");
    }


    void Set_ChartCaption(String S)
    {
        WriteLn(ActiveDSSGraphFile, "ChartCaption, \"" + S + "\"");
    }


    void Set_LineWidth(int Width)
    {
        WriteLn(ActiveDSSGraphFile, Format("Width, %d", Width));
    }


    void Set_AutoRange(double PctRim)
    {
        WriteLn(ActiveDSSGraphFile, Format("PctRim, %.8g", PctRim));
    }


    void Set_KeepAspectRatio(bool Value)
    {
        WriteLn(ActiveDSSGraphFile, Format("KeepAspect, %d", int(Value)));
    }


    void Set_DataColor(TColor clr)
    {
        WriteLn(ActiveDSSGraphFile, Format("DataColor, %d", clr));
    }


    void Set_TextAlignment(int Option)
    {
        WriteLn(ActiveDSSGraphFile, Format("TxtAlign, %d", Option));
    }


    void Set_KeyClass(int Value)
    {
        WriteLn(ActiveDSSGraphFile, Format("KeyClass, %d", Value));
    }


    void Set_Range(double LoX, double HiX, double LoY, double HiY)
    {
        WriteLn(ActiveDSSGraphFile, Format("Range, %.8g, %.8g, %.8g, %.8g", LoX, HiX, LoY, HiY));
    }


    void Set_FontStyle(TFontStyle Fs)
    {
        WriteLn(ActiveDSSGraphFile, Format("FStyle, %d", Fs));
    }


    void Set_NoScales()
    {
        WriteLn(ActiveDSSGraphFile, "NoScales,");
    }


    void Set_LeftJustifyTransparent(int LabelIdx)
    {
        WriteLn(ActiveDSSGraphFile, Format("LJust, %d", LabelIdx));
    }


    void MoveTo(double X, double Y)
    {
        WriteLn(ActiveDSSGraphFile, Format("Move, %.8g, %.8g", X, Y));
    }


    void DrawTo(double X, double Y)
    {
        WriteLn(ActiveDSSGraphFile, Format("Draw, %.8g, %.8g", X, Y));
    }


    void Rectangle(double X1, double Y1, double X2, double Y2)
    {
        WriteLn(ActiveDSSGraphFile, Format("Rect, %.8g, %.8g, %.8g, %.8g", X1, Y1, X2, Y2));
    }


    void EnableClickonDiagram()
    {
        WriteLn(ActiveDSSGraphFile, "ClickOn");
    }


    bool IsOpen(const TTextRec& Txt)
    {
        bool result = false;
        const int fmTextOpenRead = 55217;
        const int fmTextOpenWrite = 55218;
        result = (((TTextRec*)&Txt)->Mode == fmTextOpenRead) || (((TTextRec*)&Txt)->Mode == fmTextOpenWrite);
        return result;
    }


    void ShowGraph()
    {
        int         retval = 0;
        String      DSSViewFile = "";
        if (IsOpen(ActiveDSSGraphFile))
        {
            CloseFile(ActiveDSSGraphFile);
            CloseFile(ActiveSolutionFileHdl);
        }
            try
            {
                if (FileExists(ActiveFileName))
                {
                    DSSViewFile = DSSDirectory + "OpenDSSView\\DSSCView.exe";

                    if (FileExists(DSSViewFile)) // Only if the file exists
                    {
#ifdef _WIN32
                        ShellExecute(0, "open", EncloseQuotes(DSSViewFile).c_str(), EncloseQuotes(ActiveFileName).c_str(), NULL, SW_SHOWDEFAULT);
#else
#pragma message("Needs ShellExecute equivalent in DSSGraph.cpp")
#endif
                    }

                    ParserVars->Add("@LastPlotFile", ActiveFileName);
                    /*switch (retval)
                    {
                    case 0:
                        DoSimpleMsg("System out of memory. ", 45700);
                        break;
                    case ERROR_BAD_FORMAT:
                        DoSimpleMsg(String("Graphics output file \"") + ActiveFileName + "\" is Invalid.", 45701);
                        break;
                    case ERROR_FILE_NOT_FOUND:
                        DoSimpleMsg(DSSViewFile + " File  Not Found." + CRLF + "It should be in the same directory as the OpenDSS program", 45702);
                        break;
                    case ERROR_PATH_NOT_FOUND:
                        DoSimpleMsg(String("Path for DSSView program \"") + DSSViewFile + "\" Not Found.", 45703);
                        break;
                    }
                    */
                }
            }
            catch (exception& E)
            {
                DoErrorMsg("ShowGraph.", (string)E.what(), "Is DSSView.EXE correctly installed???", 45704);
            }
            GlobalResult = ActiveFileName;
        
    }


    int AddTextLabel(double X, double Y, TColor TextColor, String Txt, int Template)
    {
        int result = 0;
        WriteLn(ActiveDSSGraphFile, Format("Label, %.8g, %.8g, %d, \"%s\", %d", X, Y, TextColor, Txt.c_str(), Template));
        TextLabelCount++;
        result = TextLabelCount;
        Checkminmax(X, Y);
        return result;
    }


    void LockInTextLabel(int Idx)
    {
        WriteLn(ActiveDSSGraphFile, Format("LockInLabel, %d", Idx));
    }


    void BoldTextLabel(int Idx)
    {
        WriteLn(ActiveDSSGraphFile, Format("BoldLabel, %d", Idx));
    }


    void MarkAt(double X, double Y, unsignedchar Marker, unsignedchar Size)
    {
        WriteLn(ActiveDSSGraphFile, Format("MarkAt,  %.8g, %.8g, %d, %d", X, Y, Marker, Size));
        Checkminmax(X, Y);
    }


    void CenteredText15(double X, double Y, int Size, String Txt)
    {
        WriteLn(ActiveDSSGraphFile, Format("Center,  %.8g, %.8g, %d, \"%s\"", X, Y, Size, Txt.c_str()));
        Checkminmax(X, Y);
    }

} // namespace DSSGraph
