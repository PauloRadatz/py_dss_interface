#include <vcl.h>
#pragma hdrstop

#include "ReportForm.h"

using namespace std;
using namespace d2c_system;
using namespace System;
using namespace System::Classes;


TReport::TReport(System::Classes::TComponent* AOwner) : inherited(AOwner) {}


TReport* Report = nullptr;
#pragma resource "*.DFM" 






