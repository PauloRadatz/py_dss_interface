#ifndef ControlQueueH
#define ControlQueueH
/*
   ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   11-1-00 added Handle and delete function
*/

/*$M+*/


#include "System.h"

#include "Arraydef.h"
#include "ControlElem.h"
#include "ControlActionsDefs.h"

namespace ControlQueue
{


	struct TActionRecord;
	class TControlQueue;
	struct TTimeRec;




	struct TTimeRec {
		int Hour;
		double Sec;
	};


	typedef TActionRecord* pActionRecord;

	struct TActionRecord {
		TTimeRec ActionTime;
		int ActionCode;
		int ActionHandle;
		int ProxyHandle;
		TControlElem* ControlElement;
	};


	class TControlQueue : public TObject {
		typedef TObject inherited;
	public:
//	private:
		TList ActionList;
		bool DebugTrace;
		TTextRec Tracefile;
		int ctrlHandle;
		int Temp_Int[4/*# range 0..3*/]; // Temporary registers, Int Type
		double Temp_dbl[8/*# range 0..7*/];  // Temporary registers, dbl type
		TTimeRec Ltimer;
		TControlElem* Pop(const TTimeRec& ActionTime, int& Code, int& ProxyHdl, int& Hdl, int ActorID);  // Pop action from queue <= given time
		TControlElem* Pop_Time(const TTimeRec& ActionTime, int& Code, int& ProxyHdl, int& Hdl, double& ATime, bool KeepIn, int ActorID);  // Pop action from queue <= given time
		void DeleteFromQueue(int i, bool popped, int ActorID);
		double TimeRecToTime(TTimeRec Trec);
		void Set_Trace( bool Value);
		void WriteTraceRecord(const String ElementName, const int Code, double TraceParameter, const String S, int ActorID);
		int Get_QueueSize();
		void Recalc_Time_Step(int ActorID);
		void Restore_Time_Step(int ActorID);
	public:
		TControlQueue();
		virtual ~TControlQueue();
		int Push(const int Hour, const double Sec, const int Code, const int ProxyHdl, TControlElem* Owner, int ActorID) /*# overload */;
		int Push(const int Hour, const double Sec, const EControlAction Code, const int ProxyHdl, TControlElem* Owner, int ActorID) /*# overload */;
		//  int Push( const int Hour, const double Sec, const EControlAction Code, const int ProxyHdl, const TControlElem Owner, int ActorID ) /*# overload */;
		void Clear();
		void DoAllActions(int ActorID);
		bool DoNearestActions(int& Hour, double& Sec, int ActorID);  // Do only actions with lowest time
		bool DoActions(const int Hour, const double Sec, int ActorID);  // Do actions with time <= t
		bool DoMultiRate(const int Hour, const double Sec, int ActorID);  // Do actions with time <= t
		bool IsEmpty();
		void Delete(int Hdl, int ActorID);  // Delete queue item by handle
		void ShowQueue(const String Filenm);

		bool get_DebugTrace();

		String QueueItem(int Qidx);
	};


} // namespace ControlQueue

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ControlQueue;
#endif

#endif //  ControlQueueH









