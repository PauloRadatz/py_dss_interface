#ifndef DSSGraphH
#define DSSGraphH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
   Interface to DSSView Program

   Writes some files with the appropriate info and then invokes DSSView.exe

   Adapted from interface to old DSSGraph.DLL
*/


#include "System.h"

#include "Arraydef.h"

namespace DSSGraph
{

    class       EDSSGraphProblem;
    struct      TDSSGraphProperties;
    typedef unsigned char unsignedchar;
    typedef int TColor;
    typedef int TPenStyle;
    typedef int TFontStyle;




    enum GridStyleType {
        gsNone,
        gsPoints,
        gsVertLines,
        gsHorizLines,
        gsLines,
        gsHorizDotLines,
        gsVertDotLines,
        gsDotLines
    };

    class EDSSGraphProblem : public std::runtime_error {
	    typedef std::runtime_error inherited;	
	    EDSSGraphProblem(const String &Msg);
    };


#pragma pack(push, 1)
    struct TDSSGraphProperties {
        double  Xmin = 0,
                Xmax = 0,
                Ymin = 0,
                YMax = 0;
        int     ChartColor = 0x000000,
                WindColor = 0xFFFFFF;
        bool    Isometric = false;
        GridStyleType GridStyle;
    };
#pragma pack(pop)


    //    Procedure DSSGraphInit(ptrCallBackStruct:pDSSCallBacks); StdCall;  external 'DSSGraph.dll' name 'Init';  // Call this once



    int MakeNewGraph(const String Filename);      // Call this to make a new graph

    void AddNewLine(double X1, double Y1, double X2, double Y2, int Color, unsignedchar Thickness, TPenStyle Style, bool Dots, const String LineName, bool MarkCenter, int CenterMarkerCode, int NodeMarkerCode, int NodeMarkerWidth);
    void AddNewCurve(pDoubleArray Xarray, pDoubleArray Yarray, int NumPoints, TColor Color, unsignedchar Thickness, TPenStyle Style, bool Curvemarkers, int CurveMarker, const String CurveName);
    void AddNewText(double X1, double Y1, int Color, int Size, String S);
    void AddNewCircle(double Xc, double Yc, double Radius, int LineColor, int FColor);
    void AddNewMarker(double X, double Y, int Color, unsignedchar Symbol, unsignedchar Size);

    /*Routines to manage DSSGraph Properties. */
    /*Invoke a Get_Properties to populate the Props struct first then change values*/
    void Set_Properties(TDSSGraphProperties* Props);
    void Get_Properties(TDSSGraphProperties* &Props);
    void Set_XaxisLabel(String S);
    void Set_YaxisLabel(String S);
    void Set_Caption(String S);
    void Set_ChartCaption(String S);
    void Set_LineWidth(int Width);
    void Set_AutoRange(double PctRim);
    void Set_KeepAspectRatio(bool Value);
    void Set_DataColor(TColor clr);
    void Set_TextAlignment(int Option);
    void Set_KeyClass(int Value);
    void Set_Range(double LoX, double HiX, double LoY, double HiY);
    void Set_FontStyle(TFontStyle Fs);
    void Set_NoScales();
    void Set_LeftJustifyTransparent(int LabelIdx);
    void MoveTo(double X, double Y);
    void DrawTo(double X, double Y);
    void Rectangle(double X1, double Y1, double X2, double Y2);
    void EnableClickonDiagram();
    void ShowGraph();
    int AddTextLabel(double X, double Y, TColor TextColor, String Txt, int Template);
    void LockInTextLabel(int Idx);
    void BoldTextLabel(int Idx);
    void MarkAt(double X, double Y, unsignedchar Marker, unsignedchar Size);
    void CenteredText15(double X, double Y, int Size, String Txt);

} // namespace DSSGraph

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace DSSGraph;
#endif

#endif //  DSSGraphH