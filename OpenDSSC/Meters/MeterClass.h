#ifndef MeterClassH
#define MeterClassH

#include "System.h"
#include "Sysutils.h"

#include "CktElementClass.h"

namespace MeterClass
{


	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/
	/*
	   Base for Meter classes
	*/


	/*$M+*/

	class TMeterClass : public TCktElementClass
	{
	public:
		typedef TCktElementClass inherited;	
	private:
	protected:
		int ClassEdit(const void* ActiveMeterObj, int ParamPointer);
		void ClassMakeLike(const void* OtherObj);
		void CountProperties();  // Add no. of intrinsic properties
		void DefineProperties();  // Add Properties of this class to propName
	public:
		int NumMeterClassProps;
		TMeterClass();
		virtual ~TMeterClass();
		void ResetAll(int ActorID);
		void SampleAll(int ActorID);  // Force all monitors to take a sample
		void SaveAll(int ActorID);   // Force all monitors to save their buffers to disk
	//__published:
	};


}  // namespace MeterClass

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace MeterClass;
#endif

#endif // MeterClassH





