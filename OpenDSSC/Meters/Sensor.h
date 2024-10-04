#ifndef SensorH
#define SensorH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "MeterClass.h"
#include "MeterElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "d2c_structures.h"

namespace Sensor
{



/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change Log
   8-24-2007 Created from Monitor Object
   Sept-Oct 2008 Modified for new load allocation and state estimator algorithms
*/

/*
   Sensor compares voltages and currents. Power quantities are converted to current quantities
   based on rated kVBase, or actual voltage if voltage measurement specified.
*/

/*==============================================================================*/

class TSensor : public MeterClass::TMeterClass
{
	friend class TSensorObj;
public:
	typedef MeterClass::TMeterClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String SensorName);
public:
	TSensor();
	virtual ~TSensor();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	virtual void ResetAll(int ActorID);
	virtual void SampleAll(int ActorID);  // Force all Sensors to take a sample
	virtual void SaveAll(int ActorID);   // Force all Sensors to save their buffers to disk
	void SetHasSensorFlag();
};

/*==============================================================================*/

class TSensorObj : public MeterElement::TMeterElement
{
	friend class TSensor;
public:
	typedef MeterElement::TMeterElement inherited;	
//private:
	bool ValidSensor;
	Arraydef::pDoubleArray SensorkW;
	Arraydef::pDoubleArray Sensorkvar;
	double kVBase; // value specified
	double VBase; // in volts
	int FConn;
	bool Vspecified;
	bool Ispecified;
	bool Pspecified;
	bool Qspecified;
	bool ClearSpecified;
	int FDeltaDirection;
	void Set_Conn(int Value);
	void Set_Action(const String Value);
	void ZeroSensorArrays();
	void AllocateSensorObjArrays();
	void RecalcVbase();
	int RotatePhases(int j);
	int LimitToPlusMinusOne(int i);
	void ClearSensor();
	void UpdateCurrentVector();
	double Get_WLSCurrentError();
	double Get_WLSVoltageError();
public:
	double pctError;
	double Weight;
	TSensorObj(DSSClass::TDSSClass* ParClass, const String SensorName);
	virtual ~TSensorObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model, reset nphases
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a Sensor
	virtual void TakeSample(int ActorID); // Go add a sample to the buffer
	void ResetIt();
	void Save();  // Saves present buffer to file
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

	int get_FConn();
	int get_FDeltaDirection();
	double get_kVBase();
	pDoubleArray get_SensorkW();
	pDoubleArray get_Sensorkvar();

	TSensorObj(DSSClass::TDSSClass* ParClass);
	TSensorObj(String ClassName);
	TSensorObj();
};

/*==============================================================================*/
extern TSensorObj* ActiveSensorObj;

/*==============================================================================*/


}  // namespace Sensor

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Sensor;
#endif

#endif // SensorH





