#ifndef TOPExportH
#define TOPExportH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Supports Creation of a STO file for interfacing to TOP and the
 invoking of TOP.*/


#include "System.h"

#include "Arraydef.h"



//class TOutFile32;
//struct ToutfileHdr;



namespace TOPExport
{
    //typedef int time_t;

#pragma pack(push, 1)
    struct ToutfileHdr {
        WORD Size;
        Char Signature[16/*# range 0..15*/];
        WORD VersionMajor, VersionMinor;
        double FBase, VBase;
        int tStart, tFinish;
        double StartTime, StopT, DeltaT;
        unsigned int Nsteps;
        WORD NVoltages, NCurrents, VoltNameSize, CurrNameSize;
        int IdxVoltNames, IdxCurrentNames, IdxBaseData, IdxData;
        char Title1[80/*# range 0..79*/], Title2[80/*# range 0..79*/], Title3[80/*# range 0..79*/], Title4[80/*# range 0..79*/], Title5[80/*# range 0..79*/];  // Fixed length 80-byte string  space
    };
#pragma pack(pop)


    class TOutFile32 : public TObject {
        typedef TObject inherited;
    public:
        ToutfileHdr Header;
        String Fname;  /*Default is RLCWOUT.STO'*/
        file Fout;
    private:
    public:
        /*constructor Create(Owner: TObject);*/
        void Open();
        void Close();
        void WriteHeader(const double t_start, const double t_stop, const double h, const int NV, const int NI, const int NameSize, const String Title);
        void WriteNames(TStringList& Vnames, TStringList& Cnames);
        void WriteData(const double t, const pDoubleArray V, const pDoubleArray Curr);
        void OpenR();  /*Open for Read Only*/
        void ReadHeader(); /*Opposite of WriteHeader*/
        void GetVoltage(pDoubleArray t, pDoubleArray V, int idx, int MaxPts); /*Read a single node voltage from disk*/
        void SendToTop();
        std::string get_Fname();
        void set_Fname(std::string S);

    public:
        TOutFile32();
    };


    extern TOutFile32* TOPTransferFile;
    extern Variant TOP_Object;  // For Top Automation

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace TOPExport;
#endif

#endif //  TOPExportH








