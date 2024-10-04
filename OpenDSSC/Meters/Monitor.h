#ifndef MonitorH
#define MonitorH

#include "System.h"

#include "Command.h"
#include "MeterClass.h"
#include "MeterElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "d2c_structures.h"
#include "mathutil.h"




namespace Monitor
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change Log
   12-7-99 Modified Getcurrents override
   1-22-00 Derived from MeterElement Class
   5-30-00 Added test for positive sequence ckt model
           Fixed resetting of Nphases to match metered element
   10-27-00 Changed default to magnitude and angle instead of real and imag
   12-18-01 Added Transformer Tap Monitor Code
   12-18-02 Added Monitor Stream
   2-19-08 Added SampleCount
   01-19-13 Added flicker meter mode
   08-18-15 Added Solution monitor mode
   08-10-16 Added mode 6 for storing capacitor switching
   06-04-18 Added modes 7-9
   11-29-18 Added mode 10; revised mode 8
   12-4-18  Added link to AutoTransformer
   08-21-20 Added mode 11

*/

/*
  A monitor is a circuit element that is connected to a terminal of another
  circuit element.  It records the voltages and currents at that terminal as
  a function of time and can report those values upon demand.

  A Monitor is defined by a New commands:

  New Type=Monitor Name=myname Element=elemname Terminal=[1,2,...] Buffer=clear|save

  Upon creation, the monitor buffer is established.  There is a file associated
  with the buffer.  It is named "Mon_elemnameN.mon"  where N is the terminal no.
  The file is truncated to zero at creation or buffer clearing.

  The Monitor keeps results in the in-memory buffer until it is filled.  Then it
  appends the buffer to the associated file and resets the in-memory buffer.

  For buffer=save, the present in-memory buffer is appended to the disk file so
  that it is saved for later reference.

  The Monitor is a passive device that takes a sample whenever its "TakeSample"
  method is invoked.  The SampleAll method of the Monitor ckt element class will
  force all monitors elements to take a sample.  If the present time (for the most
  recent solution is greater than the last time entered in to the monitor buffer,
  the sample is appended to the buffer.  Otherwise, it replaces the last entry.

  Monitor Files are simple binary files of Singles.  The first record
  contains the number of conductors per terminal (NCond). (always use 'round' function
  when converting this to an integer). Then subsequent records consist of time and
  voltage and current samples for each terminal (all complex doubles) in the order
  shown below:

  <NCond>
           <--- All voltages first ---------------->|<--- All currents ----->|
  <hour 1> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...
  <hour 2> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...
  <hour 3> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...

  The time values will not necessarily be in a uniform time step;  they will
  be at times samples or solutions were taken.  This could vary from several
  hours down to a few milliseconds.

  The monitor ID can be determined from the file name.  Thus, these values can
  be post-processed at any later time, provided that the monitors are not reset.

  Modes are:
   0: Standard mode - V and I,each phase, Mag and Angle and Freq // Frequency added by BLS 2/1/23
   1: Power each phase, complex (kw and kvars)
   2: Transformer Tap
   3: State Variables
   4: Flicker level and severity index by phase (no modifiers apply)
   5: Solution Variables (Iteration count, etc.)
   6: Capacitor Switching (Capacitors only)
   7: Storage Variables
   8: Transformer Winding Currents
   9: Losses (watts and vars)
  10: Transformer Winding Voltages (across winding)
  11: All terminal V and I, all conductors, mag and angle

   +16: Sequence components: V012, I012
   +32: Magnitude Only
   +64: Pos Seq only or Average of phases

*/
typedef AnsiChar TMonitorStrBuffer[256/*# range 1..256*/];

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   /*This has to be named TDSSMonitor because Delphi has a TMonitor Class and the compiler will get confused*/

class TDSSMonitor : public MeterClass::TMeterClass
{
	friend class TMonitorObj;
public:
	typedef MeterClass::TMeterClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String MonitorName);
public:
	TDSSMonitor();
	virtual ~TDSSMonitor();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	virtual void ResetAll(int ActorID);
	virtual void SampleAll(int ActorID);  // Force all monitors to take a sample
	void SampleAllMode5(int ActorID);  // Sample just Mode 5 monitors
	virtual void SaveAll(int ActorID);   // Force all monitors to save their buffers to disk
	void PostProcessAll(int ActorID);
	void TOPExport(String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TMonitorObj : public MeterElement::TMeterElement
{
	friend class TDSSMonitor;
public:
	typedef MeterElement::TMeterElement inherited;	
//private:
	int BufferSize;
	int Hour;
	double Sec;    // last time entered in the buffer
	std::vector <float> MonBuffer;
	int BufPtr;  // point to present (last) element in buffer must be incremented to add
	std::vector <complex> CurrentBuffer;
	std::vector <complex> VoltageBuffer;
	std::vector <complex> WdgCurrentsBuffer;
	std::vector <complex> WdgVoltagesBuffer;
	std::vector <complex> PhsVoltagesBuffer;
	int NumTransformerCurrents;
	int NumWindingVoltages;
	int NumStateVars;
	
	std::vector <double> StateBuffer;
	std::vector <complex> FlickerBuffer; // store phase voltages in polar form
                                       // then convert to re=flicker level, update every time step
                                       //             and im=Pst, update every 10 minutes
	std::vector <double> SolutionBuffer;
	bool IncludeResidual;
	bool VIpolar;
	bool Ppolar;
	int FileSignature;
	int FileVersion;
	double BaseFrequency;
	String BufferFile;  // Name of file for catching buffer overflow
	bool IsFileOpen;
	bool ValidMonitor;
	bool IsProcessed;
	void AddDblsToBuffer(Arraydef::pDoubleArray Dbl, int Ndoubles);
	void AddDblToBuffer(double Dbl);
	void DoFlickerCalculations(int ActorID);  // call from CloseMonitorStream
       // function  Get_FileName: String;
public:
	int Mode;

	int stepCount = 0;              // BLS - used for frequency meter modeled after PSLF FMETER
	complex Vold;                   //
	double Told;                    //
	double simdt;					//
	double U1;						//
	double U2;						//
	double U3;						//
	double Tf = 0.05;			    // Low pass filter time constant (sec). Default value is 0.05 sec
	double ds = 0;                  // Initialize low pass filter - ds is derivative state variable
	double s = 0;                   // Initialize low pass filter - s is state variable
	double Freq;					// Output of frequency calcualtion
	TPICtrl Filter_F;				// Object for PI Control to update state variable s

	TMemoryStream MonitorStream;
	int SampleCount;           // This is the number of samples taken
	int myHeaderSize;           // size of the header of this monitor
	vector<AnsiChar> StrBuffer; // Header
	TMonitorObj(DSSClass::TDSSClass* ParClass, const String MonitorName);
	virtual ~TMonitorObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model, reset nphases
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a monitor
	virtual void TakeSample(int ActorID); // Go add a sample to the buffer
	void ResetIt(int ActorID);
	void Save();     // Saves present buffer to file
	void PostProcess(int ActorID); // calculates Pst or other post-processing
	void Add2Header(AnsiString myText);
	void OpenMonitorStream();
	void ClearMonitorStream(int ActorID);
	void CloseMonitorStream(int ActorID);
	void TranslateToCSV(bool Show, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	String Get_FileName(int ActorID);
       //Property  MonitorFileName:String read BufferFile;

//       Property CSVFileName:String Read Get_FileName;
	TMonitorObj(DSSClass::TDSSClass* ParClass);
	TMonitorObj(String ClassName);
	TMonitorObj();

// Freq Calc
	double FreqCalc(int stepCount, complex* V, complex Vo, double time);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
extern TMonitorObj* ActiveMonitorObj;

/*--------------------------------------------------------------------------*/


}  // namespace Monitor

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Monitor;
#endif

#endif // MonitorH





