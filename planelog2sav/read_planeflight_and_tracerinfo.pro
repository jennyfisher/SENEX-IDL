; $Id: read_planeflight_and_tracerinfo.pro,v 1.1.1.1 2008/06/30 18:09:54 jpp Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        READ_PLANEFLIGHT_AND_TRACERINFO
;
; PURPOSE:
;        Reads GEOS-Chem plane flight diagnostic (ND40) data and
;        replaces the ND40 chemical tracer names (e.g. TRA_001, TRA_002,
;        ...) with the standard GEOS-Chem tracer names
; 
; CATEGORY:
;
; CALLING SEQUENCE:
;        RESULT = READ_PLANEFLIGHT_AND_TRACERINFO( PLANEFLIGHT_FILE, 
;                                                  [TRACERINFO_FILE] )
;
; INPUTS:
;        PLANEFLIGHT_FILE -> Name of the file containing data from the 
;             GEOS-CHEM
;             plane following diagnostic ND40.  If FILENAME is omitted,
;             then a dialog box will prompt the user to supply a file
;             name.
;        TRACERINFO_FILE -> Name of the file containing GEOS-Chem
;        tracerinfo for the simulation type which generated the
;        PLANEFLIGHT_FILE. If unspecified, program looks for
;        'tracerinfo.dat' in the same directory as PLANEFLIGHT_FILE
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;        RESULT -> Exactly the same as output from
;        CTM_READ_PLANEFLIGHT, except for RESULT[*].varnames, which
;        use standard GEOS-Chem tracer names (e.g. 'Ox' or 'Hg0')
;        rather than TRA_001, TRA_002
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;        PLANEINFO = READ_PLANEFLIGHT_AND_TRACERINFO('plane.log.20060309')
; 
; MODIFICATION HISTORY:
;        cdh, 27 Aug 2007: VERSION 1.00
;
;-
; Copyright (C) 2007, Christopher Holmes, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine read_planeflight_and_tracerinfo"
;-----------------------------------------------------------------------


function read_planeflight_and_tracerinfo, $
             planeflight_filename, tracerinfo_filename

   ;====================================================================
   ; Make sure that tracerinfo exists
   ;==================================================================== 

   ; If Tracerinfo file name is not given, then look in the same directory
   ; as PlaneInfo with default filename 'tracerinfo.dat'
   if not Keyword_Set( tracerinfo_filename ) then begin
 
      tracerinfo_filename = extract_path( planeflight_filename ) + $
         'tracerinfo.dat'
 
   endif
 
   ; Return with error if tracerinfo file does not exist
   if (not file_exist( tracerinfo_filename)) then begin
 
      ; Exit
      Message, 'Cannot find Tracerinfo !', /Continue
      return, 1
 
   endif

   ;====================================================================
   ; Read in PlaneInfo
   ;==================================================================== 

   ; Read plane info
   PlaneInfo = ctm_read_planeflight( planeflight_filename )
 
   ; Find out how many platforms are in this PlaneInfo structure
   nPlatforms = n_elements( PlaneInfo.platform )
   

   ;====================================================================
   ; Replace ND40 Tracer names with standard GEOS-Chem tracer names
   ; for each platform
   ;==================================================================== 

   ; Loop over each platform
   for p=0L, nPlatforms-1L do begin
 
      ; Get the positions in PlaneInfo structure which contain chemical tracers
      ind_chemtracer = where( strcmp( PlaneInfo[p].varnames, 'TRA', 3 ), $
                              count )
 
      ; Get the tracer numbers for chemical tracers
      if ( count ge 1 ) then begin
 
         ; Array to hold GEOS-Chem tracer numbers
         TracerN = intarr( count )
      
         for i=0L, count-1L do begin
 
            ; Tracer number is the digits after '_' in e.g. TRA_001
            TracerN[i] = fix( (strsplit( $
                               PlaneInfo[p].varnames[ ind_chemtracer[i] ], $
                               '_', /extract ) )[1] )
         
         endfor
 
         ; Get Tracer Info for Chemical Tracers
         ctm_tracerinfo, TracerN, TracerStruct, filename=tracerinfo_filename
 
         ; Extract short tracer names for chemical tracers
         TracerName = TracerStruct.name
 
         ; Replace PlaneInfo names with standard GEOS-Chem names
         PlaneInfo[p].varnames[ind_chemtracer] = TracerName
 
      endif
 
   endfor
 
   ;====================================================================
   ; Return result to calling program
   ;==================================================================== 

   ; Return
   return, PlaneInfo
   
end
