#ifndef RCDListH
#define RCDListH

#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"


namespace RCDList
{


class TRCDList : public TList
{
public:
	typedef TList inherited;	
private:
	short int PresentItem;
public:
	TRCDList();
	virtual ~TRCDList();
	void* FirstItem();
	void* NextItem(void* PrevItem);
	int ItemIndex();
};


}  // namespace RCDList

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace RCDList;
#endif

#endif // RCDListH




