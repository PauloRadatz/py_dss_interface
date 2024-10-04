
#pragma hdrstop

#include "MemoryMap_lib.h"

#include "DSSGlobals.h"

using namespace std;
using namespace DSSGlobals;
using namespace System;


namespace MemoryMap_lib
{


typedef vector <char> TByteArr;

WORD wordBuf = 0;
// $01A0 is Header for identifying String type data
// $02A0 is Header for identifying Double type data
//******************************************************************************
// Creates a new BytesStream for the caller
// Returns the handler to handle the new memory space
//******************************************************************************

TBytesStream* Create_Meter_Space(String Init_Str)
{
	TBytesStream* result = nullptr;
	TBytesStream* Mem_Space = nullptr;
	Mem_Space = new TBytesStream();
	Mem_Space->WriteData((int16_t) 0x01A0);   // Header for identifying String type data
	Write_String(Mem_Space, Init_Str);
	result = Mem_Space;
	return result;
}
//******************************************************************************
// Writes a string into the specified BytesStream
//******************************************************************************

void WriteintoMemStr(TBytesStream* Mem_Space, String Content)
{
	Mem_Space->WriteData((int16_t) 0x01A0);   // Header for identifying String type data
	Write_String(Mem_Space, Content);
}
//******************************************************************************
// Writes a DBL into the specified BytesStream
//******************************************************************************

void WriteintoMem(TBytesStream* Mem_Space, double Content)
{
	Mem_Space->WriteData((int16_t) 0x02A0);   // Header for identifying a double type data
	Mem_Space->WriteData((float) Content);
}
//******************************************************************************
// Saves the content of the BytesStream into the specified file path
// and destroys the ByteStream
//******************************************************************************

void CloseMHandler(TBytesStream* Mem_Space, const String Dest_Path, bool AppendFile)
{
	TTextRec f = {};
	unsigned char Buffer = 0;
	int Idx = 0;
	bool MWrite = false;
	bool FHead = false;
	int MType = 0;
	int MSize = 0;
	double TVariableDbl = 0.0;

/* Open Output file; check for errors*/
	try
	{
		AssignFile(f, Dest_Path);
		if(AppendFile)
			Append(f);
		else
			Rewrite(f);
		IOResultToException();
	}
	catch (std::exception &e)
	{
		{
			DoSimpleMsg(String("Error Attempting to open file: \"") + Dest_Path
	           + ". "
	           + (std::string) e.what(), 159000);
			CloseFile(f); System::InOutRes = 0;
			return;
		}
	}
	try
	{
		Idx = 0;
		MType = 0;  // initialize to eliminate compiler warning
		MWrite = false;
		FHead = true;
		MSize = (int) Mem_Space->Size();
		while(Idx < MSize)
		{
			Mem_Space->myPos = Idx;
			if(MWrite == false)       // Checks if we are writing
			{
				Mem_Space->Read(&Buffer, 1);
				if(Buffer == 0xA0)       // If not, checks the header of the next content
				{
					Mem_Space->myPos = Idx + 1;
					Mem_Space->Read(&Buffer, 1);
					if(Buffer < 0x03)
					{
						MWrite = true;
						MType = (int) Buffer;
						++Idx;
					}
				}
			}
			else
			{
				switch(MType)
				{
					case 	1:        // Is a string
					{
						Mem_Space->Read(&Buffer, 1);
						if(Buffer != 0xA0)
						{
							if(FHead)
								FHead = false;
							if(Buffer == 10)
							{
								WriteLn(f);
								FHead = true;
								//++Idx;
							}
							else
							{
								if(Buffer > 0)
									Write(f, ((Char&) Buffer));
							}
						}
						else
						{
							Idx = Idx - 1;
							MWrite = false;
						}
					}
					break;        // Is a Double
					case 	2:
					{
						Mem_Space->ReadData(&TVariableDbl, 4);
						Idx = Idx + 3;
						if(FHead)
							FHead = false;
						else
							Write(f, ", ");
						Write(f, Format("%.11f", TVariableDbl)); // Changed from "%-g", since %g can discard all decimal places
						MWrite = false;
					}
					break;             // Not recognized
					default:
					Idx = Idx;
					break;
				}
			}
			++Idx;
		}    // make sure we close the file
	/* }
	__finally
	{*/
		CloseFile(f);
	}
	catch (...)
	{
		//
	}
}
//******************************************************************************
// Returns the content of the BytesStream to be plotted with the OpenDSS Viewer
//******************************************************************************

void ReadMHandler(TBytesStream* Mem_Space, pDoubleArray2d X_axis, pStringArray1d Ylabels, pDoubleArray2d Y_axis)
{
	unsigned char Buffer = 0;
	int Idx = 0;
	bool MWrite = false;
	bool FHead = false;
	int MType = 0;
	int MSize = 0;
	double TVariableDbl = 0.0;
	int strCounter = 0;
	int colYCounter = 0;
	int rowYCounter = 0;
	int dblXCounter = 0;
	X_axis->resize(1);
	Y_axis->resize(1);
	Ylabels->resize(1);
	try
	{
		Idx = 0;
		MType = 0;  // initialize to eliminate compiler warning
		strCounter = -1;
		colYCounter = -1;
		rowYCounter = 0;
		dblXCounter = 0;
		MWrite = false;
		FHead = true;
		MSize = (int) Mem_Space->Size();
		while(Idx < MSize)
		{
			Mem_Space->myPos = Idx;
			if(MWrite == false)       // Checks if we are writing
			{
				Mem_Space->Read(&Buffer, 1);
				if(Buffer == 0xA0)       // If not, checks the header of the next content
				{
					Mem_Space->myPos = Idx + 1;
					Mem_Space->Read(&Buffer, 1);
					if(Buffer < 0x03)
					{
						MWrite = true;
						MType = (int) Buffer;
						++Idx;
					}
				}
			}
			else
			{
				switch(MType)
				{
					case 	1:  // Is a string
					{
						Mem_Space->Read(&Buffer, 1);
						if(Buffer != 0xA0)
						{
							if(FHead)
								FHead = false;
							if(Buffer == 10)
							{
								++colYCounter;
								rowYCounter = 0;
								FHead = true;
								++Idx;
							}
							else
							{
								if(Buffer > 0)
								{
									if((Buffer == 44) && (colYCounter < 0))  // If comma in header
									{
										++strCounter;
										Ylabels->resize( strCounter + 1 );
									}
									else
									{
										if((strCounter >= 0) && (Buffer != 34) && (colYCounter < 0))   // If char diff than " in header (second and beyond)
											(*Ylabels)[strCounter] = (*Ylabels)[strCounter] + ( (Char) Buffer );
										else
										{
											if((Buffer == 44) && (colYCounter >= 0))  // If comma in content (not header)

                        // This section removes str from the data content. It stores 0.0 instead of the str.
											{
												++rowYCounter;
												if(colYCounter > 0)
												{
													Y_axis->resize(Y_axis->size(), vector <double> (colYCounter + 1));
												}
												else
												Y_axis->resize( rowYCounter, vector <double>(colYCounter + 1) );
												(*Y_axis)[rowYCounter - 1][colYCounter] = 0.0;
											}
										}
									}
								}
							}
						}
						else
						{
							Idx = Idx - 1;
							MWrite = false;
						}
					}
					break;  // Is a Double
					case 	2:
					{
						Mem_Space->ReadData(&TVariableDbl, 8);
						Idx = Idx + 7;
						if(FHead)
							FHead = false;
						else
						{
							++rowYCounter;
						}
						if(colYCounter > 0)
						{
							Y_axis->resize( Y_axis->size(), vector <double>( colYCounter + 1 ));
						}
						else
						Y_axis->resize( rowYCounter, vector <double>(colYCounter + 1));
						if(rowYCounter == 0)
						{
							X_axis->resize( 1, vector <double>(dblXCounter + 1));
							(*X_axis)[0][dblXCounter] = TVariableDbl * 3600;
							++dblXCounter;
						}
						else
						(*Y_axis)[rowYCounter - 1][colYCounter] = TVariableDbl;
						MWrite = false;
					}
					break;  // Not recognized
					default:
					Idx = Idx;
					break;
				}
			}
			++Idx;
		}
/*	}
	__finally
	{*/
		;
	}
	catch (...)
	{
		//
	}
}
//******************************************************************************
// Writes the incomming String into the specified BytesStream
//******************************************************************************

void Write_String(TBytesStream* Mem_Space, const String Content)
{

  // Str_Sz  : Integer;
	int Idx = 0;

/*  Str_Sz  :=  length(Content)-1;
  For idx := 0 to Str_Sz do Mem_Space.WriteData(Content[idx+1]);*/
	int stop = 0;
	for(stop = (int) Content.size(), Idx = 1; Idx <= stop; Idx++)
	{
		Mem_Space->WriteData(Content[Idx - 1]);
	}
}




}  // namespace MemoryMap_lib





