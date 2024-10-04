#ifndef GISCommandsH
#define GISCommandsH

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/


#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSGlobals.h"
#include "djson.h"
#include "Line.h"
#include "Utilities.h"
#include "Arraydef.h"
#include "CmdForms.h"
  // TCP Indy libraries
#include "ExecHelper.h"
//#include <idbasecomponent.hpp>
//#include <idcomponent.hpp>
//#include <idtcpconnection.hpp>
//#include <idtcpclient.hpp>
//#include "IdThreadComponent.h"
//#include "TCP_IP.h"

namespace GISCommands
{



	const int NumGISOptions = 37;
	String DoGISCmd();
	bool start_openDSSGIS();
	String show_busGIS(String BusName);
	String Get_routeGIS();
	String Get_edgesGIS();
	String Get_distanceGIS();
	String Show_routeGIS();
	String Get_JSONrouteGIS();
	String WindowLR();
	String WindowRL();
	String ReSizeWindow();
	String GISDrawCircuit();
	String show_lineGIS(String LineName);
	String export_mapGIS();
	String GetRouteSegDistances();
	void get_line_Coords(String LineName);
	String set_map_View(String myView);
	String clear_map();
	String Draw_line_GIS();
	String Zoom_area_GIS();
	String GISPlotfile(String myPath);
	String show_LatLong();
	String GISPlotPoints(String myPath);
	String GISPlotPoint(const String myShape);
	String GISLoadBus(const String myBus);
	String GISShowBuffer();
	String GISFormat(const String FormatFrom, const String FormatTo, const String Coords);
	String GISBatchFormat(const String FormatFrom, const String FormatTo, const String myPath);
	String GISClose();
	String Get_distance();
	String GISStartSelect();
	String GISStopSelect();
	String GISGetSelect();
	String GISStartDrawLine();
	String GISStopDrawLine();
	String GISGetPolyline();
	String GISGetAddress();
	String GISText(String myText);
	String GISTextFromFile(String myPath);
	//extern TIdTCPClient GISTCPClient; // ... TIdThreadComponent

	//extern TIdThreadComponent GISThreadComponent;
	extern vector< double > myCoords;
	extern String* GISOption;
	extern String* GISHelp;
	extern Command::TCommandList* GISCommandList;

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace GISCommands;
#endif

#endif //  GISCommandsH

