

#pragma hdrstop

#include "GISCommands.h"
#include "dirsep.h"


namespace GISCommands
{

/*
TIdTCPClient GISTCPClient;
TIdThreadComponent GISThreadComponent;
*/
    std::vector< double > myCoords;
    TCommandList* GISCommandList;
    String* GISOption;
    String* GISHelp;


    void DefineOptions( )
    {

      GISOption = new String[NumGISOptions];
      GISHelp   = new String[NumGISOptions];

      GISOption[1 - 1] = "Start";
      GISOption[2 - 1] = "ShowBus";
      GISOption[3 - 1] = "FindRoute";
      GISOption[4 - 1] = "GetRoute";
      GISOption[5 - 1] = "RouteDistance";
      GISOption[6 - 1] = "ShowRoute";
      GISOption[7 - 1] = "JSONRoute";
      GISOption[8 - 1] = "WindowDistribLR";
      GISOption[9 - 1] = "WindowDistribRL";
      GISOption[10 - 1] = "WindowSize";
      GISOption[11 - 1] = "PlotCircuit";
      GISOption[12 - 1] = "showLine";
      GISOption[13 - 1] = "ExportMap";
      GISOption[14 - 1] = "RouteSegDistances";
      GISOption[15 - 1] = "MapView";
      GISOption[16 - 1] = "ClearMap";
      GISOption[17 - 1] = "DrawLine";
      GISOption[18 - 1] = "ZoomMap";
      GISOption[19 - 1] = "PlotFile";
      GISOption[20 - 1] = "GoTo";
      GISOption[21 - 1] = "PlotPoints";
      GISOption[22 - 1] = "PlotPoint";
      GISOption[23 - 1] = "LoadBus";
      GISOption[24 - 1] = "ShowCoords";
      GISOption[25 - 1] = "Format";
      GISOption[26 - 1] = "BatchFormat";
      GISOption[27 - 1] = "Close";
      GISOption[28 - 1] = "Distance";
      GISOption[29 - 1] = "Select";
      GISOption[30 - 1] = "StopSelect";
      GISOption[31 - 1] = "GetSelect";
      GISOption[32 - 1] = "DrawLines";
      GISOption[33 - 1] = "StopDraw";
      GISOption[34 - 1] = "GetPolyline";
      GISOption[35 - 1] = "GetAddress";
      GISOption[36 - 1] = "Text";
      GISOption[37 - 1] = "TextFromFile";
      GISHelp[1 - 1] = "Starts OpenDSS-GIS only if it is installed in the local machine";
      GISHelp[2 - 1] = "Shows the bus specified on the map, however, the following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[3 - 1] = String( "Finds a route between the given buses using roads and geographical information. The buses are described as an array" ) + " as follows: GISFindRoute [b1 b2 - 1], do not include phases. The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[4 - 1] = "Returns the GIS coords of the route between 2 buses step by step, however, the following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. GISFindRoute has been executed at some point before this command (at least once)" + CRLF + "4. The model needs to have the correct GISCoords file";
      GISHelp[5 - 1] = "Returns the distance (value units) of the last route calculated between 2 buses, however, the following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. GISFindRoute has been executed at some point before this command (at least once)" + CRLF + "4. The model needs to have the correct GISCoords file";
      GISHelp[6 - 1] = "Shows the last route calculated between 2 buses in OpenDSS-GIS, however, the following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. GISFindRoute has been executed at some point before this command (at least once)" + CRLF + "4. The model needs to have the correct GISCoords file";
      GISHelp[7 - 1] = "Returns the JSON script describing the last route calculated between 2 buses, however, the following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. GISFindRoute has been executed at some point before this command (at least once)" + CRLF + "4. The model needs to have the correct GISCoords file";
      GISHelp[8 - 1] = "Redistributes the windows horizontally leaving OpenDSS to the left of the screen and OpenDSS-GIS to the right, however, the following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)";
      GISHelp[9 - 1] = "Redistributes the windows horizontally leaving OpenDSS to the right of the screen and OpenDSS-GIS to the left, however, the following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)";
      GISHelp[10 - 1] = "Resizes the OpenDSS-GIS window, the coordiantes need to be given as: Left, Top, Right, Bottom. For example:" + CRLF + CRLF + "GISWindowSize 0 0 800 800" + CRLF + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF
                  + "2. OpenDSS-GIS must be initialized (use GIS Start command)";
      GISHelp[11 - 1] = "Draws the circuit on top of the map displayed in OpenDSS-GIS. The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[12 - 1] = "Shows the line specified int he argument using OpenDSS-GIS. The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[13 - 1] = "Exports the current map view into the models folder as a PNG file. The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[14 - 1] = "Returns Tree/No tree if a tree intersects with the line given in the argument. The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[15 - 1] = "Chenges the map view in OpenDSS-GIS using one of the following arguments:" + CRLF + CRLF + "- Streets" + CRLF + "- StreetsVector" + CRLF + "- StreetsNight" + CRLF + "- Satellite" + CRLF
                  + "- SatelliteLabels" + CRLF + "- SatelliteLabelsVector" + CRLF + "- DarkGrayCanvas" + CRLF + "- LightGrayCanvas" + CRLF + "- LightGrayCanvasVector" + CRLF
                  + "- Navigation" + CRLF + "- OpenStreetMap" + CRLF + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF
                  + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[16 - 1] = "Clears the Map by removing all the previous draws. The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[17 - 1] = "Draws a line at the given coordinates using the color and thickness (pix) specified." + CRLF + "The line features can be defined using GISCoords, GISColor and GISThickness from the exective options." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF
                  + "3. The model needs to have the correct GISCoords file";
      GISHelp[18 - 1] = "Zooms the map at the area specified at GISCoords." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[19 - 1] = "Plots the content of the file specified in the argument on top of the current map." + CRLF + "With this function it is expected that the content of the file describes lines, their color and thickness." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF
                  + "3. The model needs to have the correct GISCoords file";
      GISHelp[20 - 1] = String( "Shows the location  in the map at given the coordinates." ) + "The coordiantes must be defined using GISCoords." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)";
      GISHelp[21 - 1] = "Plots the content of the file specified in the argument on top of the current map." + CRLF + "This function plots the content as points, the point shape, color and size must be specified in the file." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF
                  + "3. The model needs to have the correct GISCoords file";
      GISHelp[22 - 1] = String( "plots the shape specified in the argument in the map at given the coordinates." ) + "The coordiantes must be defined using GISCoords, the size and color can be specified through the options GISColor and GISThickness." + CRLF + "The shape can be one fo the following:" + CRLF + CRLF + "  Circle" + CRLF + "  + " + CRLF + "  Diamond"
                  + CRLF + "  Square" + CRLF + "  Triangle" + CRLF + "  x" + CRLF + CRLF + "The following conditions need to be fulfilled:" + CRLF
                  + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)";
      GISHelp[23 - 1] = String( "Uploads the coordinates of the bus specified in the argument to the coordinates buffer by pushing the previous down." ) + " The coordinates buffer has 4 positions, the coordinates of the bus specified will be at the first 2 positions." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file"
                 ;
      GISHelp[24 - 1] = "Returns the content of the coordinates buffer." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[25 - 1] = String( "Formats the coordinates located at the first 2 places of the coordiantes buffer. The first argument indicates " ) + "the original format and the second argument the destination format. The format can be one of the following:" + CRLF + CRLF + "- LatLong (latitude, Longitude - WGS84))" + CRLF + "- DMS (Degrees, minutes, seconds) " + CRLF + "- UTM (Universal Transverse Mercator)" + CRLF + "- USNG"
                  + CRLF + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file"
                 ;
      GISHelp[26 - 1] = String( "Formats the coordinates within the file specified. The first argument indicates " ) + "the original format and the second argument the destination format. The third argument is the path to the source file" + " containing the coordinates, which should be organized in 2 columns comma separated. The format can be one of the following:" + CRLF + CRLF + "- LatLong (latitude, Longitude - WGS84))" + CRLF + "- DMS (Degrees, minutes, seconds) " + CRLF + "- UTM (Universal Transverse Mercator)" + CRLF
                  + "- USNG" + CRLF + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF
                  + "3. The model needs to have the correct GISCoords file";
      GISHelp[27 - 1] = "Closses all the instances of OpenDSS-GIS";
      GISHelp[28 - 1] = "Returns the distance in meters between the coordinates in the buffer." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[29 - 1] = "Commands OpenDSS-GIS to start the selection mode for allowing users to draw an area on the map." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[30 - 1] = "Stops the latest select command sent to OpenDSS-GIS. Clears the map from selections and stores the selection coords in OpenDSS-GIS." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[31 - 1] = "Requests the boundaries of the latest selection. The boundaties are returned as XMin, YMin, XMax and YMax in WGS84 coords format." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[32 - 1] = "Commands OpenDSS-GIS to start line drawing mode for allowing the user to draw a polyline over the map." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[33 - 1] = "Stops the latest line drawing mode in OpenDSS-GIS. Clears the map and stores the coordinates of the polyline drawn by the user (if any)." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[34 - 1] = "Requests the coordinates of the latest polyline drawn by the user to OpenDSS-GIS. The are returned in coordiante pairs (Longitude, latitude) in WGS84 coords format." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[35 - 1] = "Returns the address calculated at the coordinates given in GISCoords. The address is returned in a JSON string." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[36 - 1] = "Plots the text given at the argument at the coordinates given in GISCoords with the color given at GISCOlor and size given at GISThickness." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
      GISHelp[37 - 1] = "Plots the text within the file given at the path in the argument." + CRLF + "The following conditions need to be fulfilled:" + CRLF + CRLF + "1. OpenDSS-GIS must be installed" + CRLF + "2. OpenDSS-GIS must be initialized (use GIS Start command)" + CRLF + "3. The model needs to have the correct GISCoords file";
    }			  

    String DoGISCmd( )
    {
      String dummy, result;
      String FormatFrom, FormatTo, ParamName, Param;
      int ParamPointer = 0, i = 0;
 //     double DblBuffer[ 51/*# range 0..50*/ ];

      /* Get next parameter on command line */
      result = "Unknown GIS command;";
      ParamPointer = 0;
      ParamName = UpperCase( Parser[ActiveActor]->GetNextParam() );
      Param = UpperCase( Parser[ActiveActor]->MakeString_() );
      /* Interpret Parameter */
      if ( Param.size() != 0 )
      {
        ParamPointer = GISCommandList->Getcommand( Param );

        /* Check options requiring a solution and abort if no solution or circuit */
        switch ( ParamPointer )
        {
          case 1:
            if ( start_openDSSGIS( ) )
              result = "GIS Started succesfully";
            else
              result = "Error, check if OpenDSS-GIS is running and your firewall setup";
          break;
          case 2:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = show_busGIS( Parser[ActiveActor]->MakeString_() );
          }
          break;
          case 3:
            result = Get_routeGIS( );
          break;
          case 4:
            result = Get_edgesGIS( );
          break;
          case 5:
            result = Get_distanceGIS( );
          break;
          case 6:
            result = Show_routeGIS( );
          break;
          case 7:
            result = Get_JSONrouteGIS( );
          break;
          case 8:
          {
            if ( ! IsDLL )
            {
              result = WindowLR( );
    //          ControlPanel.ReSizeWindow( 0 );
            }
            else
              result = "Available only for the EXE interface";
          }
          break;
          case 9:
          {
            if ( ! IsDLL )
            {
              result = WindowRL( );
    //          ControlPanel.ReSizeWindow( 1 );
            }
            else
              result = "Available only for the EXE interface";
          }
          break;
          case 10:
          {
            result = ReSizeWindow( );
          }
          break;
          case 11:
            result = GISDrawCircuit();
          break;
            // Draws the circuit on top of the map in DSS-GIS

          case 12:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = show_lineGIS( Parser[ActiveActor]->MakeString_() );
          }
          break;
          case 13:
            result = export_mapGIS( );
          break;
            // exports the current map view into the model's folder

          case 14:
          {
            result = GetRouteSegDistances( );
              // returns the distances of all the segments of the last route estimated
          }
          break;
          case 15:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = set_map_View( LowerCase( Parser[ActiveActor]->MakeString_() ) );
          }
          break;
          case 16:
            result = clear_map( );
          break;
          case 17:
            result = Draw_line_GIS( );
          break;
          case 18:
            result = Zoom_area_GIS( );
          break;
          case 19:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISPlotfile( LowerCase( Parser[ActiveActor]->MakeString_() ) );
          }
          break;
          case 20:
          {
            result = show_LatLong( );
          }
          break;
          case 21:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISPlotPoints( LowerCase( Parser[ActiveActor]->MakeString_() ) );
          }
          break;
          case 22:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISPlotPoint( LowerCase( Parser[ActiveActor]->MakeString_() ) );
          }
          break;
          case 23:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISLoadBus( LowerCase( Parser[ActiveActor]->MakeString_() ) );
          }
          break;
          case 24:
          {
            result = GISShowBuffer( );
          }
          break;
          case 25:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            FormatFrom = LowerCase( Parser[ActiveActor]->MakeString_() );
            dummy = Parser[ActiveActor]->GetNextParam();
            FormatTo = LowerCase( Parser[ActiveActor]->MakeString_() );
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISFormat( FormatFrom, FormatTo, Parser[ActiveActor]->MakeString_() );
          }
          break;
          case 26:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            FormatFrom = LowerCase( Parser[ActiveActor]->MakeString_() );
            dummy = Parser[ActiveActor]->GetNextParam();
            FormatTo = LowerCase( Parser[ActiveActor]->MakeString_() );
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISBatchFormat( FormatFrom, FormatTo, Parser[ActiveActor]->MakeString_() );
          }
          break;
          case 27:
            result = GISClose( );
          break;
          case 28:
            result = Get_distance( );
          break;
          case 29:
            result = GISStartSelect( );
          break;
          case 30:
            result = GISStopSelect( );
          break;
          case 31:
            result = GISGetSelect( );
          break;
          case 32:
            result = GISStartDrawLine( );
          break;
          case 33:
            result = GISStopDrawLine( );
          break;
          case 34:
            result = GISGetPolyline( );
          break;
          case 35:
            result = GISGetAddress( );
          break;
          case 36:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISText( Parser[ActiveActor]->MakeString_() );
          }
          break;
          case 37:
          {
            dummy = Parser[ActiveActor]->GetNextParam();
            result = GISTextFromFile( Parser[ActiveActor]->MakeString_() );
          }
          break;
          default: {}
        }
      }
      return result;
    }

    bool start_openDSSGIS( )
    {
      bool result = false;
      String myPath, myFolder;
      result = false;
      /*
      if ( DSS_GIS_Installed )
      {
        myPath = StringReplace( DSS_GIS_path, DIRSEP_STR DIRSEP_STR, DIRSEP_CHAR, VECTOROFCONST(( rfReplaceAll, rfIgnoreCase )) );
        myPath = StringReplace( myPath, '\"', "", VECTOROFCONST(( rfReplaceAll, rfIgnoreCase )) );
        myFolder = ExtractFilePath( myPath );
        if ( ! processExists( "OpenDSSGIS.exe" ) )
        {
          // Starts OpenDSS-GIS if is not running
          ShellExecute( 0, "open", myPath.c_str(), NULL, myFolder.c_str(), SW_SHOWNORMAL );
          sleep( 5000 );
          IsGISON = false;
        }
        if ( ! IsGISON )
        {
          // ... create TIdTCPClient
          GISTCPClient = TIdTCPClient.Create( );
          // ... set properties
          GISTCPClient.Host = "localhost";
          GISTCPClient.Port = DSSGISPort;
          GISTCPClient.ReadTimeout = 1000;
          GISThreadComponent = TIdThreadComponent.Create( );
          try
          {
            GISTCPClient.Connect;
            IsGISON = true;
          }
          catch( Exception & E )
          {
          {
            IsGISON = false;
          }
          }
          result = IsGISON;
        }
        else
          result = IsGISON;
      }
      */
      return result;
    }

    /* *******************************************************************************
      *                            Closes OpenDSS-GIS                                *
      ******************************************************************************* */


    String GISClose( )
    {
      String result = "nos available";
      /*
      int myError = 0;
      myError = ShellExecute( 0, NULL, "taskkill.exe", "/IM \"OpenDSSGIS.exe\" /F", "C:\\Windows\\System32", SW_HIDE );
      if ( myError > 32 )
        result = "OpenDSS-GIS closed successfuly";
      else
        result = "An error has occurred while closing OpenDSS-GIS";
      */
      IsGISON = false;
      return result;
    }

    /* *******************************************************************************
      *                         Shows the given bus on the map                       *
      ******************************************************************************* */


    String show_busGIS( String BusName )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int i = 0;
      double lat = 0.0, long = 0.0;
      String InMsg;
      if ( IsGISON )
      {
        SetActiveBus( BusName );
        if ( ActiveCircuit[ActiveActor] != NULL )
        {
          # with ActiveCircuit[ActiveActor] do
          {
            const ActiveCircuit& with0 = ActiveCircuit[ActiveActor];
            {
              if ( ( ActiveBusIndex > 0 ) && ( ActiveBusIndex <= Numbuses ) )
                if ( Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].GISCoorddefined )
                {
                  lat = Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].lat;
                  long = Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].long;
                  InMsg = "{\"command\":\"showlocation\",\"coords\":{\"longitude\":" + floattostr( long ) + ",\"latitude\":" + floattostr( lat ) + "}}";
                  try
                  {
                    GISTCPClient.IOHandler.WriteLn( InMsg );
                    InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
                    TCPJSON = TdJSON.Parse( InMsg );
                    result = TCPJSON["showlocation"].AsString;
                  }
                  catch( Exception & E )
                  {
                  {
                    IsGISON = false;
                    result = "Error while communicating to OpenDSS-GIS";
                  }
                  }
                }
                else
                  result = "One or both of the GIS coordinates are incorrect or not defined";
            }
          }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                  Shows the given location using LatLong                      *
      ******************************************************************************* */




    String show_LatLong( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int i = 0;
      double lat = 0.0, long = 0.0;
      String InMsg;
      if ( IsGISON )
      {
        lat = GISCoords[1];
        long = GISCoords[2];
        InMsg = "{\"command\":\"showlocation\",\"coords\":{\"longitude\":" + floattostr( long ) + ",\"latitude\":" + floattostr( lat ) + "}}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["showlocation"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                 Request to calculate a route between 2 buses                 *
      ******************************************************************************* */


    String Get_routeGIS( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      String JSONCmd, InMsg, BusName;
      bool TryCom = false, error = false;
      int i = 0;
      double lat = 0.0, long = 0.0;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"route\",\"coords\":{\"long1\":" + floattostr( GISCoords[1] ) + ",\"lat1\":" + floattostr( GISCoords[2] ) + ",\"long2\":" + floattostr( GISCoords[3] ) + ",\"lat2\":" + floattostr( GISCoords[4] ) + "}}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 1000 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["route"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *  Request to coordiantes of the edges that define the last route calculated   *
      ******************************************************************************* */


    String Get_edgesGIS( )
    {
      String result;
      /*
      TdJSON Coords, TCPJSON;
      String JSONCmd, TempStr, InMsg;
      if ( IsGISON )
      {
        JSONCmd = "{\"command\":\"jsonroute\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( JSONCmd );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 2000 );
          TCPJSON = TdJSON.Parse( InMsg );
          TempStr = TCPJSON["coords"].AsString;
          result = TempStr;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                Gets the distance of the last route calculated                *
      ******************************************************************************* */


    String Get_distanceGIS( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      String JSONCmd, TempStr, InMsg;
      if ( IsGISON )
      {
        JSONCmd = "{\"command\":\"routedistance\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( JSONCmd );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 2000 );
          TCPJSON = TdJSON.Parse( InMsg );
          TempStr = TCPJSON["routedistance"].AsString;
          result = TempStr;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                 Shows on the map the last route calculated                   *
      ******************************************************************************* */


    String Show_routeGIS( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      String JSONCmd, TempStr, InMsg;
      if ( IsGISON )
      {
        JSONCmd = "{\"command\":\"showroute\"},\"color\":\"" + GISColor + "\",\"thickness\":" + GISThickness + "}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( JSONCmd );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 2000 );
          TCPJSON = TdJSON.Parse( InMsg );
          TempStr = TCPJSON["showroute"].AsString;
          result = TempStr;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *       Exports to a file the last route calculated in JSON format             *
      ******************************************************************************* */


    String Get_JSONrouteGIS( )
    {
      String result;
//      TTextRec F;
      String JSONCmd, FileName, InMsg;
      /*
      if ( IsGISON )
      {
        JSONCmd = "{\"command\":\"jsonscript\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( JSONCmd );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 20000 );
          FileName = GetOutputDirectory + CircuitName_[ActiveActor] + "JSONScript_route.txt"; // Explicitly define directory
          Assignfile( F, FileName );
          Rewrite( F );
          IOResultToException();
          Write( F, InMsg );
          CloseFile( F );
          result = FileName;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *            Distributes the windows leaving OpenDSS on the left               *
      ******************************************************************************* */


    String WindowLR( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int ScrSize = 0;
      String InMsg, TempStr, JSONCmd;
      if ( IsGISON )
      {
        JSONCmd = "{\"command\":\"resizewindow\",\"coords\":{\"left\":" + inttostr( Screen.Width / 2 ) + ",\"top\":0,\"right\":" + inttostr( Screen.Width ) + ",\"bottom\":" + inttostr( Screen.Height - 40 ) + "}}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( JSONCmd );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 2000 );
          TCPJSON = TdJSON.Parse( InMsg );
          TempStr = TCPJSON["resizewindow"].AsString;
          result = TempStr;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *            Distributes the windows leaving OpenDSS to the right              *
      ******************************************************************************* */


    String WindowRL( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int ScrSize = 0;
      String InMsg, TempStr, JSONCmd;
      if ( IsGISON )
      {
        JSONCmd = "{\"command\":\"resizewindow\",\"coords\":{\"left\":0,\"top\":0,\"right\":" + inttostr( Screen.Width / 2 ) + ",\"bottom\":" + inttostr( Screen.Height - 40 ) + "}}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( JSONCmd );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 2000 );
          TCPJSON = TdJSON.Parse( InMsg );
          TempStr = TCPJSON["resizewindow"].AsString;
          result = TempStr;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *    Resizes the OpenDSS-GIS window using the coordinates given by the user    *
      ******************************************************************************* */


    String ReSizeWindow( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int j = 0, ScrSize = 0;
      String InMsg, TempStr, JSONCmd;
      DynamicArray< String > TStrArr;
      if ( IsGISON )
      {
        TStrArr.Length = 4;
        TStrArr[0] = ",\"top\":";
        TStrArr[1] = ",\"right\":";
        TStrArr[2] = ",\"bottom\":";
        TStrArr[3] = "}}";
        JSONCmd = "{\"command\":\"resizewindow\",\"coords\":{\"left\":";
        for ( int stop = TStrArr.High, j = 0; j <= stop; j++)
        {
          Parser[ActiveActor]->GetNextParam();
          JSONCmd = JSONCmd + Parser[ActiveActor]->MakeString_() + TStrArr[j];
        }
        try
        {
          GISTCPClient.IOHandler.WriteLn( JSONCmd );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 2000 );
          TCPJSON = TdJSON.Parse( InMsg );
          TempStr = TCPJSON["resizewindow"].AsString;
          result = TempStr;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *      Generates the file required by DSS-GIS to draw the model on the map     *
      ******************************************************************************* */


    String GISDrawCircuit( )
    {
      String result = "not available";
      TLineObj LineElem;
      String TxtRow, myBus;
      int k = 0;
//      TTextRec F;
      String InMsg, TempStr, JSONCmd;
      /*
      TdJSON TCPJSON;
      bool Add2file = false;
      if ( ActiveCircuit[ActiveActor] != NULL )
      {
        if ( IsGISON )
        {
          # with ActiveCircuit[ActiveActor] do
          {
            const ActiveCircuit& with0 = ActiveCircuit[ActiveActor];
            {
              if ( Lines.get_myNumList() > 0 )
              {
                Assignfile( F, "GIS_desc.csv" );
                Rewrite( F );
                IOResultToException();
                LineElem = Lines.Get_First();
                while ( LineElem != NULL )
                {
                  TxtRow = "";
                  Add2file = true;
                  for ( int stop = 2, k = 1; k <= stop; k++)
                  {
                    myBus = StripExtension( LineElem.GetBus( k ) );
                    DSSGlobals.SetActiveBus( myBus );
                    if ( Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].GISCoorddefined )
                    {
                      TxtRow = TxtRow + floattostr( Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].long ) + "," + floattostr( Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].lat ) + ",";
                    }
                    Add2file = Add2file && ( Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].long != 0 ) && ( Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].lat != 0 );
                  }
                  if ( Add2file )
                    WriteLn( F, TxtRow );
                  LineElem = Lines.Get_Next();
                }
                CloseFile( F );
                JSONCmd = "{\"command\":\"plotcircuit\",\"path\":\"" + OutputDirectory[ActiveActor] + "GIS_desc.csv\",\"color\":\"" + GISColor + "\",\"thickness\":" + GISThickness + "}";
              // Sends the command to OpenDSS-GIS
                try
                {
                  GISTCPClient.IOHandler.WriteLn( JSONCmd );
                  InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 5000 );
                  TCPJSON = TdJSON.Parse( InMsg );
                  TempStr = TCPJSON["plotcircuit"].AsString;
                  result = TempStr;
                }
                catch( Exception & E )
                {
                {
                  IsGISON = false;
                  result = "Error while communicating to OpenDSS-GIS";
                }
                }
              }
            }
          }
        }
        else
          result = "OpenDSS-GIS is not installed or initialized";
      }*/
      return result;
    }

    /* *******************************************************************************
      *                         Shows the given line on the map                       *
      ******************************************************************************* */


    String show_lineGIS( String LineName )
    {
      String result;
    /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      TLineObj pLine;
      if ( IsGISON )
      {
        // First have to find the line
        if ( ActiveCircuit[ActiveActor] != NULL )
        {
          get_line_Coords( LineName );
          InMsg = "{\"command\":\"showline\",\"coords\":{\"long1\":" + floattostr( myCoords[0] ) + ",\"lat1\":" + floattostr( myCoords[1] ) + ",\"long2\":" + floattostr( myCoords[2] ) + ",\"lat2\":" + floattostr( myCoords[3] ) + "}}";
          try
          {
            GISTCPClient.IOHandler.WriteLn( InMsg );
            InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
            TCPJSON = TdJSON.Parse( InMsg );
            result = TCPJSON["showline"].AsString;
          }
          catch( Exception & E )
          {
          {
            IsGISON = false;
            result = "Error while communicating to OpenDSS-GIS";
          }
          }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *             Exports the current map view into the models folder              *
      ******************************************************************************* */


    String export_mapGIS( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"exportmap\", \"path\":\"" + OutputDirectory[ActiveActor] + "\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["exportmap"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *             Commands OpenDSS-GIS to return the distances for every           *
      *                     step of the last route estimated                         *
      ******************************************************************************* */


    String GetRouteSegDistances( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        // to be implemented
        if ( ActiveCircuit[ActiveActor] != NULL )
        {
          InMsg = "{\"command\":\"getdistances\"}";
          try
          {
            GISTCPClient.IOHandler.WriteLn( InMsg );
            InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
            TCPJSON = TdJSON.Parse( InMsg );
            result = TCPJSON["getdistances"].AsString;
          }
          catch( Exception & E )
          {
          {
            IsGISON = false;
            result = "Error while communicating to OpenDSS-GIS";
          }
          }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *             Commands OpenDSS-GIS to update the map view to the               *
      *                             one given by the user                            *
      ******************************************************************************* */

    String set_map_View( String myView )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = String( "{\"command\":\"mapview\",\"mymap\":\"" ) + myView + "\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["mapview"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *      Commands OpenDSS-GIS to remove all previous lines/draws from the map    *
      ******************************************************************************* */


    String clear_map( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"clearmap\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["clearmap"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                 Draws a line in the map at the given coordinates             *
      ******************************************************************************* */


    String Draw_line_GIS( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"drawline\",\"coords\":{\"long1\":" + floattostr( GISCoords[2] ) + ",\"lat1\":" + floattostr( GISCoords[1] ) + ",\"long2\":" + floattostr( GISCoords[4] ) + ",\"lat2\":" + floattostr( GISCoords[3] ) + "},\"color\":\"" + GISColor + "\",\"thickness\":"
                    + GISThickness + "}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["drawline"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *          Zooms the map at the area described by the given coordinates        *
      ******************************************************************************* */


    String Zoom_area_GIS( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"zoommap\",\"coords\":{\"long1\":" + floattostr( GISCoords[2] ) + ",\"lat1\":" + floattostr( GISCoords[1] ) + ",\"long2\":" + floattostr( GISCoords[4] ) + ",\"lat2\":" + floattostr( GISCoords[3] ) + "}}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["zoommap"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *            request the calculation of the distance between 2 points          *
      ******************************************************************************* */


    String Get_distance( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"distance\",\"coords\":{\"long1\":" + floattostr( GISCoords[1] ) + ",\"lat1\":" + floattostr( GISCoords[2] ) + ",\"long2\":" + floattostr( GISCoords[3] ) + ",\"lat2\":" + floattostr( GISCoords[4] ) + "}}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["distance"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                 Commands OpenDSS-GIS to start select mode                    *
      ******************************************************************************* */

    String GISStartSelect( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"select\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["select"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *              Commands OpenDSS-GIS to start line drawing mode                 *
      ******************************************************************************* */


    String GISStartDrawLine( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"drawlines\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["drawlines"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *            Commands OpenDSS-GIS to stop the line drawing mode                *
      ******************************************************************************* */


    String GISStopDrawLine( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"stopdrawlines\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["stopdrawlines"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                  Commands OpenDSS-GIS to stop select mode                    *
      ******************************************************************************* */


    String GISStopSelect( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"stopselect\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["stopselect"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *      gets the boundaries for the latest selection made in OpenDSS-GIS        *
      ******************************************************************************* */


    String GISGetSelect( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"getselect\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["getselect"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                     gets the address at the given coords                     *
      ******************************************************************************* */

    String GISGetAddress( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"address\",\"coords\":{\"long\":" + floattostr( GISCoords[1] ) + ",\"lat\":" + floattostr( GISCoords[2] ) + "}}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 1000 );
          result = InMsg;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *          gets the coords for the latest polyline drawn in OpenDSS-GIS        *
      ******************************************************************************* */


    String GISGetPolyline( )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"getpolyline\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 200 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["getpolyline"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *       Commands OpenDSS-GIS to draw the content of a file over the map        *
      ******************************************************************************* */


    String GISPlotfile( String myPath )
    {
      String result = "not available";
      String TxtRow, myBus;
      int k = 0;
//      TTextRec F;
      String InMsg, TempStr, JSONCmd;
      /*
      TdJSON TCPJSON;
      if ( ActiveCircuit[ActiveActor] != NULL )
      {
        if ( IsGISON )
        {
          JSONCmd = String( "{\"command\":\"plotfromfile\",\"path\":\"" ) + myPath + "\"}";
          // Sends the command to OpenDSS-GIS
          try
          {
            GISTCPClient.IOHandler.WriteLn( JSONCmd );
            InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 5000 );
            TCPJSON = TdJSON.Parse( InMsg );
            TempStr = TCPJSON["plotfromfile"].AsString;
            result = TempStr;
          }
          catch( Exception & E )
          {
          {
            IsGISON = false;
            result = "Error while communicating to OpenDSS-GIS";
          }
          }
        }
        else
          result = "OpenDSS-GIS is not installed or initialized";
      }*/
      return result;
    }

    /* *******************************************************************************
      *     Commands OpenDSS-GIS to draw the points within a file over the map       *
      ******************************************************************************* */


    String GISPlotPoints( String myPath )
    {
      String result = "not available";
      String TxtRow, myBus;
      int k = 0;
//      TTextRec F;
      String InMsg, TempStr, JSONCmd;
      /*
      TdJSON TCPJSON;
      if ( ActiveCircuit[ActiveActor] != NULL )
      {
        if ( IsGISON )
        {
          JSONCmd = String( "{\"command\":\"plotpoints\",\"path\":\"" ) + myPath + "\"}";
          // Sends the command to OpenDSS-GIS
          try
          {
            GISTCPClient.IOHandler.WriteLn( JSONCmd );
            InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 5000 );
            TCPJSON = TdJSON.Parse( InMsg );
            TempStr = TCPJSON["plotpoints"].AsString;
            result = TempStr;
          }
          catch( Exception & E )
          {
          {
            IsGISON = false;
            result = "Error while communicating to OpenDSS-GIS";
          }
          }
        }
        else
          result = "OpenDSS-GIS is not installed or initialized";
      }*/
      return result;
    }

    /* *******************************************************************************
      *         Commands OpenDSS-GIS to draw a text at specific coordinates        *
      ******************************************************************************* */


    String GISText( String myText )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int myShpCode = 0, activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = "{\"command\":\"text\",\"coords\":{\"long\":" + floattostr( GISCoords[1] ) + ",\"lat\":" + floattostr( GISCoords[2] ) + "},\"content\":\"" + myText + "\",\"color\":\"" + GISColor + "\",\"size\":" + GISThickness + "}"
                   ;
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 1000 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["text"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *                  Commands OpenDSS-GIS to draw a text from a file             *
      ******************************************************************************* */


    String GISTextFromFile( String myPath )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int myShpCode = 0, activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        InMsg = String( "{\"command\":\"textfromfile\",\"path\":\"" ) + myPath + "\"}";
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 1000 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["textfromfile"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      *         Commands OpenDSS-GIS to draw a marker at specific coordinates        *
      ******************************************************************************* */

    String GISPlotPoint( const String myShape )
    {
      String result;
      /*
      TdJSON TCPJSON;
      int myShpCode = 0, activesave = 0, i = 0;
      String InMsg;
      bool Found = false;
      TLineObj pLine;
      if ( IsGISON )
      {
        switch ( myShape[1] )
        {
          case // Parse the shape specified
          'c':
            myShpCode = 0;
          break;
          case '+':
            myShpCode = 1;
          break;
          case 'd':
            myShpCode = 2;
          break;
          case 's':
            myShpCode = 3;
          break;
          case 't':
            myShpCode = 4;
          break;
          case 'x':
            myShpCode = 5;
          break;
        }
        InMsg = "{\"command\":\"plotpoint\",\"coords\":{\"long\":" + floattostr( GISCoords[2] ) + ",\"lat\":" + floattostr( GISCoords[1] ) + "},\"color\":\"" + GISColor + "\",\"thickness\":" + GISThickness + ",\"shape\":" + inttostr( myShpCode ) + "}"
                   ;
        try
        {
          GISTCPClient.IOHandler.WriteLn( InMsg );
          InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 1000 );
          TCPJSON = TdJSON.Parse( InMsg );
          result = TCPJSON["plotpoint"].AsString;
        }
        catch( Exception & E )
        {
        {
          IsGISON = false;
          result = "Error while communicating to OpenDSS-GIS";
        }
        }
      }
      else*/
        result = "OpenDSS-GIS is not installed or initialized";
      return result;
    }

    /* *******************************************************************************
      * Commands openDSS-GIS to convert the coords given in a file into a new format *
      ******************************************************************************* */


    String GISBatchFormat( const String FormatFrom, const String FormatTo, const String myPath )
    {
      String result = "not available";
      /*
      int myEnd = 0, myStart = 0;
      TdJSON TCPJSON;
      String InMsg;
      InMsg = String( "{\"command\":\"batchformat\",\"from\":\"" ) + FormatFrom + "\",\"to\":\"" + FormatTo + "\"," + "\"path\":\"" + myPath + "\"}";
      try
      {
        GISTCPClient.IOHandler.WriteLn( InMsg );
        InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 1000 );
        myStart = ansipos( "path\":\"", InMsg ) + 6;
        myEnd = ansipos( "\"}", InMsg ) - 1;
        result = InMsg.Substring( myStart, myEnd - myStart );
      }
      catch( Exception & E )
      {
      {
        IsGISON = false;
        result = "Error while communicating to OpenDSS-GIS";
      }
      }
      */
      return result;
    }

    /* *******************************************************************************
      *      Commands openDSS-GIS to convert the coords given into a new format      *
      ******************************************************************************* */


    String GISFormat( const String FormatFrom, const String FormatTo, const String Coords )
    {
      String result= "not available";
      /*
      TdJSON TCPJSON;
      String InMsg;
      InMsg = String( "{\"command\":\"format\",\"from\":\"" ) + FormatFrom + "\",\"to\":\"" + FormatTo + "\"," + "\"coords\":\"" + Coords + "\"}";
      try
      {
        GISTCPClient.IOHandler.WriteLn( InMsg );
        InMsg = GISTCPClient.IOHandler.ReadLn( '\x0a', 1000 );
        TCPJSON = TdJSON.Parse( InMsg );
        result = TCPJSON["coords"].AsString;
      }
      catch( Exception & E )
      {
      {
        IsGISON = false;
        result = "Error while communicating to OpenDSS-GIS";
      }
      }
      */
      return result;
    }

    /* *******************************************************************************
      *          Returns a string with the content of the coordiantes buffer         *
      ******************************************************************************* */


    String GISShowBuffer( )
    {
      String result;
      int idx = 0;
      result = "";

      for ( int stop = 4, idx = 1; idx <= stop; idx++)
        result = result + std::to_string( (GISCoords)[idx] ) + ",";
      return result;
    }

    /* *******************************************************************************
      *  Loads the bus coordiantes into the first 2 places fo the coordiantes buffer *
      *  shifting it down                                                            *
      ******************************************************************************* */


    String GISLoadBus( const String myBus )
    {
      String result;
      double myLat = 0.0, myLong = 0.0;
      if ( ActiveCircuit[ActiveActor] != NULL )
        /*# with ActiveCircuit[ActiveActor] do */
        {
          auto with0 = ActiveCircuit[ActiveActor];
          {
            SetActiveBus( StripExtension( myBus ) );
            if ( ( with0->ActiveBusIndex > 0 ) && (with0->ActiveBusIndex <= with0->NumBuses ) )
            {
              if (with0->Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex - 1]->CoordDefined)
              {
                myLong = with0->Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex - 1]->longitude;
                myLat = with0->Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex - 1]->lat;
              }
              (GISCoords)[3] = (GISCoords)[1];
              (GISCoords)[4] = (GISCoords)[2];
              (GISCoords)[1] = myLat;
              (GISCoords)[2] = myLong;
              result = "done";
            }
            else
              result = "Invalid bus name";
          }
        }
      else
        result = "There is no active circuit";
      return result;
    }

    /* *******************************************************************************
      *             Loads the line Long-lat into the global array "myCoords"         *
      ******************************************************************************* */


    void get_line_Coords( String LineName )
    {
      /*
      TdJSON TCPJSON;
      int activesave = 0, i = 0;
      std::vector < String > myBuses;
      String S, InMsg;
      bool Found = false;
      TLineObj pLine;
      myCoords.size() = 4;
      myBuses.size() = 2;
      S = LineName; // Convert to Pascal String
      Found = false;
      # with ActiveCircuit[ActiveActor]->Lines do
      {
        activesave = ActiveIndex;
        pLine = Get_First();
        while ( pLine != NULL )
        {
          if ( CompareText( pLine.Name, S ) == 0 )
          {
            ActiveCircuit[ActiveActor]->ActiveCktElement = pLine;
            Found = true;
            Break;
          }
          pLine = Get_Next();
        }
      }
      // Get the names of the buses for the line
      # with ActiveCircuit[ActiveActor] do
      {
        const ActiveCircuit& with0 = ActiveCircuit[ActiveActor];
        {
          for ( int stop = 2, i = 1; i <= stop; i++)
          {
            myBuses[i] = StripExtension( pLine.GetBus( i ) );
          }

        // Get the coords of the buses
          for ( int stop = 1, i = 0; i <= stop; i++)
          {
            SetActiveBus( myBuses[i] );
            if ( ( ActiveBusIndex > 0 ) && ( ActiveBusIndex <= Numbuses ) )
            {
              if ( Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].GISCoorddefined )
              {
                myCoords[i * 2] = Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].long;
                myCoords[i * 2 + 1] = Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex].lat;
              }
            }
          }
        }
      }

      */
    }

    void DisposeStrings( )
    {
      delete[] GISOption;
      delete[] GISHelp;
    }

    void GISCommands_initialization()
    {
      DefineOptions();
      GISCommandList = new TCommandList( GISOption, NumGISOptions);
      GISCommandList->set_AbbrevAllowed(true);
    }

    void GISCommands_finalization()
    {
      DisposeStrings();
      delete GISCommandList; // GISCommandList->~TCommandList();
    }

    class GISCommands_unit
    {
    public:
    GISCommands_unit()
    {
      GISCommands_initialization();
    }
    ~GISCommands_unit(){ GISCommands_finalization(); }
    };
    GISCommands_unit _GISCommands_unit;

}   // namespace GISCommands
