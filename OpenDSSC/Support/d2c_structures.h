#ifndef d2c_structuresH
#define d2c_structuresH


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  library tcreated to complement the missing units or function equivalent in Delphi

*/

#include <algorithm>
#include <iostream>
#include <list>
#include <string>
#include <vector>
#include <fstream>
#include <thread> 
#include <cstddef>
#include <cstdint> // uint8_t
#include <queue>
#include <cctype>
#include <locale>
#include "Sysutils.h"
#include "Arraydef.h"
#include "Sparse_Math.h"


using namespace std;

typedef std::vector <std::string> TStringList;
typedef std::vector< void* > TList;
typedef TTextRec Textfile;
typedef std::thread TThread;
typedef int TFontStyles;
#ifdef linux
typedef int32_t int32;
typedef int32_t TEvent;
typedef int8_t DynamicArrayByte;
#else
typedef __int32 int32;
typedef __int32 TEvent;
typedef __int8* DynamicArrayByte;
#endif
typedef char* pUTF8Char;
typedef wchar_t WideChar;

#ifndef _WIN32
typedef struct _GUID {
	uint32_t Data1;
	uint16_t Data2;
	uint16_t Data3;
	uint8_t Data4[8];
} GUID;
#endif

GUID StringToGuid(const std::string& str);
std::string GuidToString(const GUID& guid);
void val(std::string S, double* result, int* Code);

//-----------------------------------------------------------------------------------

int MaxIntValue(std::vector < int >* myarray);
int MinIntValue(std::vector < int >* myarray);
double MaxValue(std:: vector <double>* myarray);
double MinValue(std::vector <double>* myarray);
float MaxValue(std::vector <float>* myarray);
float MinValue(std::vector <float>* myarray);

//-----------------------------------------------------------------------------------

int mean(std::vector < int >* myarray);
double mean(std::vector < double >* myarray);
float mean(std::vector < float >* myarray);

//-----------------------------------------------------------------------------------

std::string* Slice(std::string* myArray, int mySlice);
wchar_t* Str2WChar(std::string s);
bool ASSIGNED(void* myPtr);
double sqr(double myNumber);
std::string ToUpperCaseStr(const std::string &myString);
std::string ToLowerCaseStr(const std::string &myString);
void SaveStrArr2File(vector <std::string> * myStrVector, std::string myPath);

double Random();

int FindInStrVector(std::vector<string>* myarray, std::string myValue);
int Find(std::vector<int>* myarray, int myvalue);



//--------------------------------------------------------------------------------------

int Length(std::vector < TCmplx_Data >* myPtr);
int Length(std::string myStr);

int ansipos(std::string myChar, std::string mySrc);

double Sqr(double myNum);

double Sign(double myNum);

//--------------------------------------------------------------------------------------

std::vector <double> Copy(std::vector <double> myArray);

pDoubleArray Copy(pDoubleArray myArray);

void Val(std::string myStr, int* myVal, int myCode);

void Val(std::string myStr, double* myVal, int myCode);

void Move2(pIntegerArray Source, pIntegerArray Dest, int Count);

std::string Format(const char* arg, ...); //override

//--------------------------------------------------------------------------------------

int IntPower(int Base, int exponent);

int IntPower(double Base, int exponent);

//--------------------------------------------------------------------------------------

class TMemoryStream
{
public:
	//--------------------------------------------------------------------------------------
	int myPos;
	std::vector <uint8_t> myMStrm;

	int begin();  // sets the pointer at the beginning
	int end();  // sets the pointer at the end
	int Position();  // returns the pointer's location
	void clear();
	int Size();

	void Write(int* value, int offset=0);
	void Write(AnsiChar* value, int offset = 0);
	void Write(std::vector <float>* value, int offset = 0);
	void Write(float* value, int offset = 0);

	//--------------------------------------------------------------------------------------

	int Read(int* value);
	int Read(unsigned int* value);
	int Read(float* value);
	int Read(float* value, int Size);
	int Read(double* value, int Size);
	int Read(int* value, int Size);
	int Read(uint8_t* value, int Size);
//	int Read(float value[100]);

	int Read(AnsiChar* value);

	//--------------------------------------------------------------------------------------

};

class TBytesStream
{
public:
	//--------------------------------------------------------------------------------------
	int myPos;
	std::vector <uint8_t> myMStrm;

	int begin();  // sets the pointer at the beginning
	int end();  // sets the pointer at the end
	int Position();  // returns the pointer's location
	void clear();
	int Size();

	void Write(int* value, int offset = 0);
	void Write(AnsiChar* value, int offset = 0);
	void Write(pSingleArray value, int offset = 0);
	
	int WriteData(int16_t Value);
	int WriteData(char Value);
	int WriteData(float Value);

	//--------------------------------------------------------------------------------------

	int Read(unsigned char* value, int count);

	int ReadData(double* value, int count);

	//--------------------------------------------------------------------------------------
};

//===========================================================================================

enum TPerlRegExOptions
{
	preCaseLess,       // /i -> Case insensitive
	preMultiLine,      // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string
	preSingleLine,     // /s -> Dot matches any character, including \n (newline). Otherwise, it matches anything except \n
	preExtended,       // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out
	preAnchored,       // /A -> Successful match can only occur at the start of the subject or right after the previous match
	preUnGreedy,       // Repeat operators (+, *, ?) are not greedy by default (i.e. they try to match the minimum number of characters instead of the maximum)
	preNoAutoCapture
};

enum TPerlRegExState
{
	preNotBOL,         // Not Beginning Of Line: ^ does not match at the start of Subject
	preNotEOL,         // Not End Of Line: $ does not match at the end of Subject
	preNotEmpty        // Empty matches not allowed
};

class TPerlRegEx
{
public:
	bool FCompiled, FStudied;
	TPerlRegExOptions FOptions;
	TPerlRegExState FState;
	std::string FRegEx, mySubject;
//	PCREString FReplacement;
//	PCREString FSubject;
	int FStart, FStop;


	TPerlRegExOptions get_FOptions();
	void set_Options(TPerlRegExOptions value);

	std::string get_FRegEx();
	void set_FRegEx(std::string value);

	std::string get_mySubject();
	void set_mySubject(std::string value);

	bool Match();

};

//======================================================================================

std::string PadRight(int myNum, std::string myChar);

void CoutLn(std::string myStr);


#endif //  d2c_structures
