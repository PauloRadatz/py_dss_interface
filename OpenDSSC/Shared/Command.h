#ifndef CommandH
#define CommandH

#include "System.h"
#include "HashList.h"


//class TCommandList;


namespace Command
{


	 /*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/


	/*$M+*/

	class TCommandList
	{
	public:

		THashList CommandList;
		bool AbbrevAllowed;
		int Get_NumCommands();
	public:
		TCommandList(std::string* Commands, int Commands_maxidx);
		virtual ~TCommandList();
		void AddCommand(std::string cmd);
		int Getcommand(std::string cmd);

		bool get_AbbrevAllowed();
		void set_AbbrevAllowed(bool myValue);

		std::string Get(int i);
	//__published:
	//public:
		TCommandList();
	};


}  // namespace Command

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Command;
#endif

#endif // CommandH




