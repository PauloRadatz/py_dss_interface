
#pragma hdrstop

#include "RCDList.h"

using namespace std;


namespace RCDList
{



TRCDList::TRCDList()
 : PresentItem((short int) -1)
{
	;
}

TRCDList::~TRCDList()
{
	// inherited::Destroy();
}


void* TRCDList::FirstItem()
{
	void* result = nullptr;
	if(Count > 0)
	{
		result = Items[0];
		PresentItem = 0;
	}
	else
	{
		result = nullptr;
		PresentItem = (short int) -1;
	}
	return result;
}

void* TRCDList::NextItem(void* PrevItem)
{
	void* result = nullptr;
	int i = 0;
	if(PrevItem != Items[PresentItem])
        /*List has been used by someone after it was initiated.. Reset list to match PreviousItem*/
	{
		int stop = 0;
		PresentItem = (short int) (Count - 1);
		for(stop = Count - 1, i = 0; i <= stop; i++)
		{
			if(Items[i] == PrevItem)
			{
				PresentItem = (short int) i;
				break;
			}
		}
        /*If we can't make it match, PresentItem will point to last Item and
         the next operation (below) will return nil*/
	}
	++PresentItem;
	if(Count > PresentItem)
	{
		result = Items[PresentItem];
	}
	else
	{
		result = nullptr;
		PresentItem = (short int) (Count - 1);
	}
	return result;
}

int TRCDList::ItemIndex()
{
	int result = 0;
	result = PresentItem;
	return result;
}




}  // namespace RCDList




