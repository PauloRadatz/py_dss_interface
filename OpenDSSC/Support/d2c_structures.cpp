
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  library tcreated to complement the missing units or function equivalent in Delphi

*/

#include <cstdint> // uint8_t
#include "d2c_structures.h"
#include "Sysutils.h"
#include <string>
#include <algorithm>
#include "CNData.h"
#include <stdio.h>
#include <stdarg.h>
#include <cstring>
#include "DSSGlobals.h"

GUID StringToGuid(const std::string& str)
{
	GUID guid;
	sscanf(str.c_str(),
		"{%8x-%4hx-%4hx-%2hhx%2hhx-%2hhx%2hhx%2hhx%2hhx%2hhx%2hhx}",
		&guid.Data1, &guid.Data2, &guid.Data3,
		&guid.Data4[0], &guid.Data4[1], &guid.Data4[2], &guid.Data4[3],
		&guid.Data4[4], &guid.Data4[5], &guid.Data4[6], &guid.Data4[7]);
	return guid;
}

std::string GuidToString(const GUID& guid)
{
	char guid_cstr[39];
	snprintf(guid_cstr, sizeof(guid_cstr),
		"{%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}",
		guid.Data1, guid.Data2, guid.Data3,
		guid.Data4[0], guid.Data4[1], guid.Data4[2], guid.Data4[3],
		guid.Data4[4], guid.Data4[5], guid.Data4[6], guid.Data4[7]);
	
	return std::string(guid_cstr);
}


std::string Format(const char* arg, ...)
{
	std::string result;
	char Buff[10000];

	va_list argptr;
	va_start(argptr, arg);
	vsnprintf(Buff, 10000, arg, argptr);
	va_end(argptr);

	result = Buff;

	return result;
}


const std::string WHITESPACE = " \n\r\t\f\v";

std::string ltrim(const std::string& s)
{
	size_t start = s.find_first_not_of(WHITESPACE);
	return (start == std::string::npos) ? "" : s.substr(start);
}

std::string rtrim(const std::string& s)
{
	size_t end = s.find_last_not_of(WHITESPACE);
	return (end == std::string::npos) ? "" : s.substr(0, end + 1);
}

std::string trim(const std::string& s) {
	return rtrim(ltrim(s));
}

void Move2(pIntegerArray Source, pIntegerArray Dest, int Count)
{
	int myCount = Count / sizeof(longInt);
	for (int idx = 0; idx < myCount; idx++)
		Dest[idx] = Source[idx];
}

void val(std::string S, double* result, int* Code)
{
	S = trim(S);
	double myNum = 0;
	bool HasLettrs = false;
	try
	{
		myNum = stod(S);
		// if everything was OK, then check again for no numeric chars
		// otherwise it will jump into the exception
		HasLettrs = (S[S.size() - 1] > 57) || (S[S.size() - 1] < 44);

		if (HasLettrs)
		{// It has a letter at the end
			*result = 0;
			*Code = 1;
		}
		else
		{
			*result = myNum;
			*Code = 0;
		}
	}
	catch (...)
	{
		*result = 0;
		*Code	= 1;
	}
}

int MaxIntValue(std::vector < int > * myarray)
{
	int i;

	// Initialize maximum element
	int max = (*myarray)[0];

	// Traverse array elements
	// from second and compare
	// every element with current max
	for (i = 0; i < ((*myarray).size() - 1); i++)
		if ((*myarray)[i] > max)
			max = (*myarray)[i];

	return max;
}

int MinIntValue(std::vector < int >* myarray)
{
	int i;

	// Initialize maximum element
	int min = (*myarray)[0];

	// Traverse array elements
	// from second and compare
	// every element with current min
	for (i = 0; i < ((*myarray).size() - 1); i++)
		if ((*myarray)[i] < min)
			min = (*myarray)[i];

	return min;
}

double MaxValue(std::vector <double>* myarray)
{
	int i;

	// Initialize maximum element
	double max = (*myarray)[0];

	// Traverse array elements
	// from second and compare
	// every element with current max
	for (i = 1; i < myarray->size(); i++)
		if ((*myarray)[i] > max)
			max = (*myarray)[i];

	return max;
}


double MinValue(std::vector <double>* myarray)
{
	int i;

	// Initialize maximum element
	double min = (*myarray)[0];

	// Traverse array elements
	// from second and compare
	// every element with current min
	for (i = 0; i < ((*myarray).size() - 1); i++)
		if ((*myarray)[i] < min)
			min = (*myarray)[i];

	return min;
}

float MaxValue(std::vector <float>* myarray)
{
	int i;

	// Initialize maximum element
	float max = (*myarray)[0];

	// Traverse array elements
	// from second and compare
	// every element with current max
	for (i = 0; i < ((*myarray).size() - 1); i++)
		if ((*myarray)[i] > max)
			max = (*myarray)[i];

	return max;
}


float MinValue(std::vector <float>* myarray)
{
	int i;

	// Initialize maximum element
	float min = (*myarray)[0];

	// Traverse array elements
	// from second and compare
	// every element with current min
	for (i = 0; i < ((*myarray).size() - 1); i++)
		if ((*myarray)[i] < min)
			min = (*myarray)[i];

	return min;
}

int FindInStrVector(std::vector<string>* myarray, std::string myValue)
{
	int idx = -1;
	for (int i = 0; i < myarray->size(); i++)
		if ((*myarray)[i] == myValue)
		{
			idx = i;
			break;
		}
	return idx;
}
int Find(std::vector<int>* myarray, int myvalue)
{
	int idx = -1;
	for (int i = 0; i < myarray->size(); i++)
		if ((*myarray)[i] == myvalue)
		{
			idx = i;
			break;
		}
	return idx;
}


std::string* Slice(std::string* myArray, int mySlice)
{
	std::string* myCopy = new std::string[mySlice];
	if (myCopy != NULL) 
	{
		for(int j = 0; j < mySlice ; j++)
			myCopy[j] = myArray[j];
	}

	return myCopy;
}

wchar_t* Str2WChar(std::string s)
{
	wchar_t* wide_string = new wchar_t[s.length() + 1];
	std::copy(s.begin(), s.end(), wide_string);
	wide_string[s.length()] = 0;

	return wide_string;
}

bool ASSIGNED(void* myPtr)
{
	bool result = ( myPtr != NULL );

	return result;
}

std::string ToUpperCaseStr(const std::string &myString)
{
	std::string myUpString = myString;
	transform(myUpString.begin(), myUpString.end(), myUpString.begin(), ::toupper);
	return myUpString;
}

std::string ToLowerCaseStr(const std::string &myString)
{
	std::string myUpString = myString;
	transform(myUpString.begin(), myUpString.end(), myUpString.begin(), ::tolower);
	return myUpString;
}

double Random()
{
	double myRandNum = ((double)rand()) / ((double)RAND_MAX);
	return myRandNum;
}

double sqr(double myNumber)
{
	return pow(myNumber, 2);
}

void SaveStrArr2File(vector <std::string>* myStrVector, std::string myPath)
{
	TTextRec F;
	int i;
	AssignFile(F, myPath);
	Rewrite(F);

	for (i = 0; i < myStrVector->size(); i++)
	{
		System::WriteLn(F, (*myStrVector)[i]);
	}
	CloseFile(F);
}

int mean(std::vector < int >* myarray)
{
	int i;
	int accumulator = 0;

	for (i = 0; i < myarray->size(); i++)
		accumulator = accumulator + (*myarray)[i];
	accumulator = accumulator / myarray->size();

	return accumulator;
}

double mean(std::vector < double > myarray)
{
	int i;
	double accumulator = 0;

	for (i = 0; i < myarray.size(); i++)
		accumulator = accumulator + (myarray)[i];
	accumulator = accumulator / myarray.size();

	return accumulator;
}

float mean(std::vector < float >* myarray)
{
	int i;
	float accumulator = 0;

	for (i = 0; i < myarray->size(); i++)
		accumulator = accumulator + (*myarray)[i];
	accumulator = accumulator / myarray->size();

	return accumulator;
}

int Length(vector < TCmplx_Data >* myPtr)
{
	return (int) myPtr->size();
}

int Length(std::string myStr)
{
	return (int) myStr.size();
}

int ansipos(std::string myChar, std::string mySrc)
{
	int myidx = mySrc.find(myChar);
	return myidx;
}

double Sqr(double myNum)
{
	double result = myNum * myNum;
	return result;
}

std::vector <double> Copy(std::vector <double> myArray)
{
	std::vector <double> result;
	result.clear();

	for (int i = 0; i < myArray.size(); i++)
		result.push_back((myArray)[i]);

	return result;
}

pDoubleArray Copy(pDoubleArray myArray)
{
	double* result;
	result = new double[sizeof(myArray)];

	for (int i = 0; i < sizeof(myArray); i++)
		result[i] = myArray[i];

	return result;
}

double Sign(double myNum)
{
	double result;
	if (myNum == 0)
		result = 0;
	else if (myNum > 0)
		result = 1;
	else
		result = -1;
	return result;
}

void Val(std::string myStr, int* myVal, int myCode)
{
	*myVal = stoi(myStr);
}

void Val(std::string myStr, double* myVal, int myCode)
{
	*myVal = stod(myStr);
}

//=========================================================================

void TMemoryStream::Write(int* value, int offset)
{
	uint8_t myBytes[4];
	int IntVar = 0;

	bool OW = true;
	if ( ( myPos + (sizeof(int) ) >= myMStrm.size() ) )
		OW = false;

	IntVar = *value;
	memcpy(&myBytes, &IntVar, sizeof(int));

	for (int i = 0; i < (sizeof(int)); i++)
	{
		if (OW)
			myMStrm[myPos] = myBytes[i];
		else
			myMStrm.push_back(myBytes[i]);
		++myPos;
	}
}

void TMemoryStream::Write(AnsiChar* value, int offset)
{
	bool OW = true;
	if ((myPos + (256) > myMStrm.size()))
		OW = false;

	for (int i = 0; i < (256); i++)
	{
		static_assert(sizeof(uint8_t) == sizeof(AnsiChar), "Wrong uint8_t size!");
		if (OW)
			myMStrm[myPos] = value[i];
		else
			myMStrm.push_back(value[i]);
		++myPos;
	}
}

void TMemoryStream::Write(std::vector <float>* value, int offset)
{
	
	uint8_t myBytes[4];
	float Fvar;

	bool OW = true;
	if ((myPos + ( sizeof( value[0] ) * offset) > myMStrm.size()))
		OW = false;

	for (int i = 0; i < offset; i++)
	{
		Fvar = (*value)[i];
		memcpy(&myBytes, &Fvar, 4);

		for (int j = 0; j < sizeof(float); j++)
		{
			if (OW)
				myMStrm[myPos] = myBytes[j];
			else
				myMStrm.push_back(myBytes[j]);
			++myPos;
		}
	}
}

void TMemoryStream::Write(float* value, int offset)
{
	uint8_t myBytes[4];

	float Fvar;

	bool OW = true;
	if ((myPos + (sizeof(value[0])  - offset) > myMStrm.size()))
		OW = false;


	Fvar = *value;
	memcpy(&myBytes, &Fvar, 4);

	for (int j = 0; j < sizeof(float); j++)
	{
		if (OW)
			myMStrm[myPos] = myBytes[j];
		else
			myMStrm.push_back(myBytes[j]);
		++myPos;
	}

}

int TMemoryStream::Read(int* value)
{
	unsigned char bytes[sizeof(int)];
	int myInit = myPos;
	int myValue;

	for (int i = 0; i < sizeof(int); i++)
	{
		bytes[i] = (unsigned char) myMStrm[myPos];
		myPos++;
	}

	std::memcpy(&myValue, bytes, sizeof(int));
	*value = myValue;

	return (myPos - myInit);
}

int TMemoryStream::Read(unsigned int* value)
{
	int myInit = myPos;
	unsigned int myValue = 0;

	unsigned char bytes[sizeof(unsigned int)];
	for (int i = 0; i < sizeof(unsigned int); i++)
	{
		bytes[i] = (unsigned char) myMStrm[myPos];
		myPos++;
	}

	std::memcpy(&myValue, bytes, sizeof(unsigned int));

	*value = myValue;

	return (myPos - myInit);
}

int TMemoryStream::Read(float* value)
{
	int myInit = myPos;

	unsigned char bytes[sizeof(float)];
	for (int i = 0; i < sizeof(float); i++)
	{
		bytes[i] = (unsigned char) myMStrm[myPos];
		myPos++;
	}

	std::memcpy(value, bytes, sizeof(unsigned int));

	return (myPos - myInit);
}

int TMemoryStream::Read(float* value, int Size)
{
	int myInit = myPos;
	unsigned char bytes[sizeof(float)];
	int NumRegs = (int)(Size / sizeof(float));

	for (int j = 0; j < NumRegs; j++)
	{
		for (int i = 0; i < sizeof(float); i++)
		{
			bytes[i] = (unsigned char)myMStrm[myPos];
			myPos++;
		}

		std::memcpy(&(value[j]), bytes, sizeof(unsigned int));
	}

	return (myPos - myInit);
}

//--------------------------------------------------------------------------------------

int IntPower(int Base, int exponent)
{
	int result = 0;
	result = (int)pow(Base, exponent);
	return result;
}

int IntPower(double Base, int exponent)
{
	int result = 0;
	result = (int)pow(Base, exponent);
	return result;
}
//--------------------------------------------------------------------------------------


int TMemoryStream::Read(double* value, int Size)
{
	int myInit = myPos;
	unsigned char bytes[sizeof(double)];
	int NumRegs = (int)(Size / sizeof(double));

	for (int j = 0; j < NumRegs; j++)
	{
		for (int i = 0; i < sizeof(double); i++)
		{
			bytes[i] = (unsigned char)myMStrm[myPos];
			myPos++;
		}

		std::memcpy(&(value[j]), bytes, sizeof(unsigned int));
	}

	return (myPos - myInit);
}

int TMemoryStream::Read(int* value, int Size)
{
	int myInit = myPos;
	unsigned char bytes[sizeof(float)];
	int NumRegs = (int)(Size / sizeof(int));

	for (int j = 0; j < NumRegs; j++)
	{
		for (int i = 0; i < sizeof(float); i++)
		{
			bytes[i] = (unsigned char)myMStrm[myPos];
			myPos++;
		}

		std::memcpy(&(value[j]), bytes, sizeof(unsigned int));
	}

	return (myPos - myInit);
}

int TMemoryStream::Read(uint8_t* value, int Size)
{
	int myInit = myPos;
	int NumRegs = Size;
	int MaxPos = myMStrm.size();

	for (int j = 0; j < NumRegs; j++)
	{
		if (myPos < MaxPos)
		{
			value[j] = myMStrm[myPos];
			myPos++;
		}
	}
	return (myPos - myInit);
}


int TMemoryStream::Read(AnsiChar* value)
{
	int myInit = myPos;

	for (int i = 0; i < 256; i++)
	{
		static_assert(sizeof(uint8_t) == sizeof(AnsiChar), "Wrong uint8_t size!");
		value[i] = AnsiChar(myMStrm[myPos]);
		myPos++;
	}

	return (myPos - myInit);
}


int TMemoryStream::begin()
{
	myPos = 0;
	return myPos;
}

int TMemoryStream::end()
{
	myPos = myMStrm.size() - 1;
	return myPos;
}

int TMemoryStream::Position()
{
	return myPos;
}

int TMemoryStream::Size()
{
	return myMStrm.size();
}

void TMemoryStream::clear()
{
	myMStrm.clear();
	myPos = 0;
}

//=========================================================================

void TBytesStream::Write(int* value, int offset)
{
	bool OW = true;
	if ((myPos + (sizeof(*value)) > myMStrm.size()))
		OW = false;

	for (int i = 0; i < (sizeof(*value)); i++)
		if (OW)
		{
			myMStrm[myPos] = (*value >> (i * 8));
			myPos++;
		}
		else
			myMStrm.push_back(*value >> (i * 8));

}

void TBytesStream::Write(AnsiChar* value, int offset)
{
	bool OW = true;
	if ((myPos + (256) > myMStrm.size()))
		OW = false;

	for (int i = 0; i < (256); i++)
		if (OW)
		{
			myMStrm[myPos] = value[i];
			myPos++;
		}
		else
			myMStrm.push_back(value[i]);
}

void TBytesStream::Write(pSingleArray value, int offset)
{
	uint32_t value2;

	bool OW = true;
	if ((myPos + (sizeof((value)[0]) * sizeof(value) - offset) > myMStrm.size()))
		OW = false;

	for (int i = offset; i < sizeof(value); i++)
	{
		value2 = (uint32_t)(value)[i];

		for (int j = 0; j < sizeof(value2); j++)
			if (OW)
			{
				myMStrm[myPos] = value2 >> (j * 8);
				myPos++;
			}
			else
				myMStrm.push_back(value2 >> (j * 8));
	}
}

int TBytesStream::Read(unsigned char* value, int count)
{
	int myInit = myPos;
	
	*value = (unsigned char)myMStrm[myPos];

	return (myPos - myInit);
}

int TBytesStream::ReadData(double* value, int count)
{
	int myInit = myPos;
	float myData = 0;

	unsigned char bytes[sizeof(float)];
	for (int i = 0; i < sizeof(float); i++)
	{
		bytes[i] = (unsigned char)myMStrm[myPos];
		myPos++;
	}

	std::memcpy(&myData, bytes, sizeof(float));
	*value = (double) myData;

	return (myPos - myInit);
}


int TBytesStream::begin()
{
	myPos = 0;
	return myPos;
}

int TBytesStream::end()
{
	myPos = myMStrm.size() - 1;
	return myPos;
}

int TBytesStream::Position()
{
	return myPos;
}

int TBytesStream::Size()
{
	return (int) myMStrm.size();
}

void TBytesStream::clear()
{
	myMStrm.clear();
	myPos = 0;
}

int TBytesStream::WriteData(int16_t Value)
{
	int myInit = myPos;

	unsigned char bytes[sizeof(int16_t)];

	memcpy(&bytes, &Value, sizeof(int16_t));

	for (int i = 0; i < sizeof(int16_t); i++)
	{
		myMStrm.push_back(bytes[i]);
		myPos++;
	}

	

	return (myPos - myInit);
}

int TBytesStream::WriteData(char Value)
{
	int myInit = myPos;

	myMStrm.push_back((uint8_t)Value);
	myPos++;




	return (myPos - myInit);
}

int TBytesStream::WriteData(float Value)
{
	int myInit = myPos;

	unsigned char bytes[sizeof(float)];

	memcpy(&bytes, &Value, sizeof(float));

	for (int i = 0; i < sizeof(float); i++)
	{
		myMStrm.push_back(bytes[i]);
		myPos++;
	}

	return (myPos - myInit);
}

//==========================================================================

TPerlRegExOptions TPerlRegEx::get_FOptions()
{
	return FOptions;
}

void TPerlRegEx::set_Options(TPerlRegExOptions value)
{
	if (FOptions != value)
	{

	}
}

std::string TPerlRegEx::get_FRegEx()
{
	return FRegEx;
}

void TPerlRegEx::set_FRegEx(std::string value)
{
	FRegEx = value;
}

std::string TPerlRegEx::get_mySubject()
{
	return mySubject;
}

void TPerlRegEx::set_mySubject(std::string value)
{
	mySubject = value;
}

bool TPerlRegEx::Match()
{
	bool result = false;

	std::string exp1 = ToLowerCaseStr(mySubject);
	std::string exp2 = ToLowerCaseStr(FRegEx);
	if (exp2 == ".*")
		result = true;
	else
		result = ( exp1 == exp2 );

	return result;
}

//===================================================================================

std::string PadRight(int myNum, std::string myChar)
{
	std::string result;
	for (int i = 0; i < myNum; i++)
		result = result + myChar;
	return result;
}

void CoutLn(std::string myStr)
{
	if (NoFormsAllowed)
		return;
	cout << myStr;
	cout << "\n";
}
