#ifndef GraphPropertiesH
#define GraphPropertiesH


#include <System.hpp>

#include <graphics.hpp>
#include "rchart.h"



struct TDSSGraphProperties;




//     GridStyleType = (gsNone, gsPoints, gsVertLines, gsHorizLines, gsLines, gsHorizDotLines, gsVertDotLines, gsDotLines);

#pragma pack(push, 1)
struct TDSSGraphProperties {
  double Xmin;
  double Xmax;
  double Ymin;
  double YMax;
  TColor ChartColor;
  TColor WindColor;
  bool Isometric;
  GridStyleType GridStyle;
};
#pragma pack(pop)


#endif //  GraphPropertiesH








