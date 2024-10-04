#ifndef d2c_systobjH
#define d2c_systobjH




namespace System {


class TObject
{

public:
  TObject() {}
//  Free();  -> use delete 
  virtual ~TObject(){}
};

 
} //namespace System


#endif
