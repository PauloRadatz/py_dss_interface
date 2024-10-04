//#include <vcl.h>
#pragma hdrstop

#include "Command.h"

using namespace std;
using namespace Hashlist;
using namespace System;

using namespace Command;


Command::TCommandList::TCommandList() {}



TCommandList::TCommandList(std::string* Commands, int Commands_maxidx)
:CommandList(THashList(Commands_maxidx /*# High(Commands) */ + 1)),
AbbrevAllowed(true)
{
	int i = 0;
	int stop = 0;

	for(stop = (int) (Commands_maxidx - 1) /*# High(Commands) */, i = 0; i <= stop; i++)
	{
      // Fill the HashList
		CommandList.Add(Commands[i]);
	}
}

Command::TCommandList::~TCommandList()
{
	// inherited::Destroy();
}


void Command::TCommandList::AddCommand(std::string cmd)
{
	CommandList.Add(cmd);
}

int Command::TCommandList::Getcommand(std::string cmd)
{
	int result = 0;
	result = CommandList.Find(cmd);
/*If no match found on whole command, check for abbrev*/
/*This routine will generally be faster if one enters the whole command*/
	if(result == 0)
	{
		if(AbbrevAllowed)
			result = CommandList.FindAbbrev(cmd);
	}
	return result;
}

std::string Command::TCommandList::Get(int i)
{
	string result;
	result = CommandList.Get((unsigned int) i);
	return result;
}

int Command::TCommandList::Get_NumCommands()
{
	int result = 0;
	result = (int) CommandList.Get_NumElements();
	return result;
}

bool Command::TCommandList::get_AbbrevAllowed()
{
	return AbbrevAllowed;
}

void Command::TCommandList::set_AbbrevAllowed(bool myValue)
{
	AbbrevAllowed = myValue;
}









