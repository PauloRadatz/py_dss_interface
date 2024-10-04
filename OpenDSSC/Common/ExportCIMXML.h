#ifndef ExportCIMXMLH
#define ExportCIMXMLH  

/*
  ---------------------------------------------------------V-
  Copyright (c) 2009-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Write a CIM XML file using RDF Schema for the Common Distribution
  Power System Model, IEC 61968-13.*/


//#include <System.hpp>

#include "NamedObject.h"
  // for TUuid

void ExportCDPSM( String Filenm, String SUBSTATION, String SubGeographicRegion, String GeographicRegion, TUuid FdrUUID, TUuid SubUUID, TUuid SubGeoUUID, TUuid RgnUUID, bool Combined = true );
void StartUuidList( int Size );
void FreeUuidList( );
void WriteHashedUUIDs( Textfile& F );
void AddHashedUUID( const String& key, const String& UuidVal );
void DefaultCircuitUUIDs( TUuid& fdrID, TUuid& subID, TUuid& rgnID, TUuid& subGeoID );

#endif //  ExportCIMXMLH








