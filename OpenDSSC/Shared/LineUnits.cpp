
#pragma hdrstop

#include "LineUnits.h"


using namespace std;


namespace LineUnits
{



int GetUnitsCode(const String s)
{
	int result = 0;
	String Stest;
	result = 0;
	Stest = s.substr(0, 2);  // copy first 2 chars for MOST OF the test
	if(CompareText(Stest, "no") == 0)      // no units specified
		result = UNITS_NONE;
	else
	{
		if(CompareText(Stest, "mi") == 0)      // per mile
			result = UNITS_MILES;
		else
		{
			if(CompareText(Stest, "kf") == 0)  // per 1000 ft (kft)
				result = UNITS_KFT;
			else
			{
				if(CompareText(Stest, "km") == 0)  // per km
					result = UNITS_KM;
				else
				{
					if(CompareText(Stest, "m") == 0)  // per meter
						result = UNITS_M;
					else
					{
						if(CompareText(Stest, "me") == 0)  // per meter
							result = UNITS_M;
						else
						{
							if(CompareText(Stest, "ft") == 0)
								result = UNITS_FT;
							else
							{
								if(CompareText(Stest, "in") == 0)
									result = UNITS_IN;
								else
								{
									if(CompareText(Stest, "cm") == 0)
										result = UNITS_CM;
									else
									{
										if(CompareText(Stest, "mm") == 0)
											result = UNITS_MM;
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return result;
}

String LineUnitsStr(int Units)
{
	String result;
	switch(Units)
	{
		case 	0:
		result = "none";
		break;
		case 	UNITS_MILES:
		result = "mi";
		break;
		case 	UNITS_KFT:
		result = "kft";
		break;
		case 	UNITS_KM:
		result = "km";
		break;
		case 	UNITS_M:
		result = "m";
		break;
		case 	UNITS_FT:
		result = "ft";
		break;
		case 	UNITS_IN:
		result = "in";
		break;
		case 	UNITS_CM:
		result = "cm";
		break;
		case 	UNITS_MM:
		result = "mm";
		break;
		default:
		result = "none";
		break;
	}
	return result;
}

double To_Meters(int Units)
{
	double result = 0.0;
	switch(Units)
	{
		case 	UNITS_MILES:
		result = 1609.344;
		break;
		case 	UNITS_KFT:
		result = 304.8;
		break;
		case 	UNITS_KM:
		result = 1000.0;
		break;
		case 	UNITS_M:
		result = 1.0;
		break;
		case 	UNITS_FT:
		result = 0.3048;
		break;
		case 	UNITS_IN:
		result = 0.0254;
		break;
		case 	UNITS_CM:
		result = 0.01;
		break;
		case 	UNITS_MM:
		result = 0.001;
		break;
		default:
		result = 1.0;
		break;
	}
	return result;
}

double To_per_Meter(int Units)
{
	double result = 0.0;
	result = 1.0 / To_Meters(Units);
	return result;
}

double From_per_Meter(int Units)
{
	double result = 0.0;
	result = To_Meters(Units);
	return result;
}

double From_Meters(int Units)
{
	double result = 0.0;
	result = 1.0 / To_Meters(Units);
	return result;
}

double ConvertLineUnits(int FromUnits, int ToUnits)
{
	double result = 0.0;
	if((FromUnits == UNITS_NONE) || (ToUnits == UNITS_NONE)) // Don't know what to convert
		result = 1.0;
	else
		result = From_Meters(ToUnits) * To_Meters(FromUnits);
	return result;
}




}  // namespace LineUnits




