; $Id: planelog2flightmerge.pro,v 1.1.1.1 2008/06/30 18:09:54 jpp Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        PLANELOG2FLIGHTMERGE
;
; PURPOSE:
;        Extract data for a single platform (usually an aircraft) from
;        the GEOS-Chem ND40 diagnostic output of one or more files and
;        produce a single structure. The output structure, named 'GC',
;        is saved to a file, if given, and passed to the calling
;        procedure through a keyword, if present. The user can exclude
;        model data before a 'takeoffTime' or after a 'landingTime'
;        for the given plaform using keywords; this permits creating a
;        structure for a single flight, using an input file, or files
;        that also contain model data from other flights.    
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        PLANELOG2FLIGHTMERGE
;
; INPUTS:
;        Platform 
;        FileList 
;
; KEYWORD PARAMETERS:
;        avgMin
;        outFile 
;        flightDate
;        takeoffTime 
;        landingTime
;        GC
;        
;
; OUTPUTS:
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        cdh, 31 Aug 2007: VERSION 1.00
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
; with subject "IDL routine planelog2flightmerge"
;-----------------------------------------------------------------------


pro planelog2flightmerge, Platform, FileList, avgMin=avgMin, $
                          outFile=outFile, flightDate = flightDate, $
                          takeoffTime=takeoffTime, landingTime=landingTime, $
                          GridType=GridType, GC=GC
 
;; Convert plane.log.YYYYMMDD files from the GEOS-Chem ND40 diagnostic 
;; into IDL sav files. SAV file will contain a single structure
;; (e.g. gc_dc8) which contains all data from inFiles.
;; The structure tags will be chemical species (standard GEOS-Chem
;; names), navigational info, and met data. The data will average over 
;; a time period of avgMin, in order to average over the GEOS-Chem transport
;; timestep. 
 
   ; ======================================================
   ; Input data
   ; ======================================================
 
   If n_elements(Platform) NE 1 Then Stop, 'Must pass Platform'
 
   ; ======================================================
   ; Read in plane.log files
   ; ======================================================
 
   ; Initialize
   First = 1L
   Last  = 0L
 
   ; Loop through files
   FOR f=0L, n_elements(FileList)-1L DO BEGIN
   
       ; Check if this is the last file
       IF ( F EQ n_elements( FileList )-1L ) THEN $
         LAST = 1L
             
      ; Next file to read
      ThisFile = FileList[f]
 
      ; Status message
      print, 'Reading in ', ThisFile, $
         ' (file ', strtrim(string(f+1), 2), $
         '/', strtrim(string(n_elements(FileList)), 2), ')'
 
      CTM_Cleanup
 
      if not File_Exist( thisFile ) then $
         message, 'FILE ' + thisFile + ' DOES NOT EXIST'
 
      ; Use READ_PLANEFLIGHT_AND_TRACERINFO in order to get standard GEOS-Chem
      ; tracer names rather than default ND40 names
      PLANE = READ_PLANEFLIGHT_AND_TRACERINFO( thisfile,$
      !SENEX+'/IDL/planelog2sav/tracerinfo.dat')
 
      ;; Trim any leading or trailing blanks
      PlanePlatform = Strtrim(Plane[*].Platform, 2)
 
      ; Find which data are for desired platform
      INDEX = WHERE( PlanePlatform eq Platform,  Ct)
      
      ; ------------------------------------------------------
      ; Check if there are any data for desired platform
      ; ------------------------------------------------------
      IF (Ct LE 0) THEN BEGIN
 
         ; No data, so print message and goto next file
         print, 'NO ' + Platform + ' DATA IN THIS FILE...'
 
      ENDIF ELSE BEGIN
 
         ; Find number of variables
         NVARS           =  PLANE[INDEX].NVARS
         
         ; Find number of samples from model
         NPOINTS         =  PLANE[INDEX].NPOINTS
 
         ;; Resize arrays to the size of the data
         DATE_1          =  PLANE[INDEX].DATE     [0:NPOINTS-1  ]
         TIME_1          =  PLANE[INDEX].TIME     [0:NPOINTS-1  ]
         LAT_1           =  PLANE[INDEX].LAT      [0:NPOINTS-1  ]
         LON_1           =  PLANE[INDEX].LON      [0:NPOINTS-1  ]
         PRESS_1         =  PLANE[INDEX].PRESS    [0:NPOINTS-1  ]
         VARNAMES_1      =  PLANE[INDEX].VARNAMES [0:NVARS-1    ]
         DATA_1          =  PLANE[INDEX].DATA     [0:NPOINTS-1,*]
         DATA_1          =  DATA_1                [*, 0:NVARS-1 ]
 
         ; ------------------------------------------------------
         ; Append new data to arrays
         ; ------------------------------------------------------
         IF (FIRST) THEN BEGIN
 
             ; Discard all points before takeoff time on first day
             ; could be due to a flight that started on previous day
             if Keyword_Set( takeoffTime ) then $
               IND_thisFlight = where( TIME_1 ge takeoffTime ) $
               else $
               IND_thisFlight = indgen( n_elements(TIME_1) )
             
             ; Start Array
             DATE            =  [ DATE_1[  IND_thisFlight]   ]
             TIME            =  [ TIME_1[  IND_thisFlight]   ]
             LAT             =  [ LAT_1[   IND_thisFlight]   ]
             LON             =  [ LON_1[   IND_thisFlight]   ]
             PRESS           =  [ PRESS_1[ IND_thisFlight]   ]
             DATA            =  [ DATA_1[  IND_thisFlight,*] ]
         
         ENDIF ELSE IF (LAST) THEN BEGIN
 
             ; Discard all points after landing time on last day
             ; could be due to a flight starting later the same day
             IF Keyword_Set( landingTime ) THEN $
               IND_thisFlight = where( TIME_1 LE landingTime ) $
               ELSE $
               IND_thisFlight = indgen( n_elements( TIME_1 ) )
 
             ; Add data to array
             DATE            =  [ DATE,  DATE_1[  IND_thisFlight]   ]
             TIME            =  [ TIME,  TIME_1[  IND_thisFlight]   ]
             LAT             =  [ LAT,   LAT_1[   IND_thisFlight]   ]
             LON             =  [ LON,   LON_1[   IND_thisFlight]   ]
             PRESS           =  [ PRESS, PRESS_1[ IND_thisFlight]   ]
             DATA            =  [ DATA,  DATA_1[  IND_thisFlight,*] ]
 
         ENDIF ELSE BEGIN
 
             ; If this isn't either the first or last day/file, then 
             ; keep all data
             
             DATE            =  [DATE,      DATE_1    ]
             TIME            =  [TIME,      TIME_1    ]
             LAT             =  [LAT,       LAT_1     ]
             LON             =  [LON,       LON_1     ]
             PRESS           =  [PRESS,     PRESS_1   ]
             DATA            =  [DATA,      DATA_1    ]
         
         ENDELSE
 
         FIRST =  0L
         CTM_CLEANUP
 
      ENDELSE
   ENDFOR
 
   
   ; Check if File Searches found any data
   If (First) then begin
 
      print,  "NO DATA FOR " + Platform + " FOUND IN ANY OF GIVEN FILES..."
 
   Endif else begin
 
      ; ======================================================
      ; Misc. Data Processing
      ; ======================================================
 
      ;; Check for missing value flags
      Missing = Where(Data Eq -1000, Ct)
      If Ct Gt 0 Then Data[Missing] = !VALUES.F_NAN
 
      ;; remove pressure glitch for MILAGRO data, C130 was never above 200hPa
      if platform eq 'C130' then begin
         glitch = where ( press lt 200., ct )
         if ct gt 0. then begin
            press[glitch] = !values.f_nan
            data[glitch, *] = !values.f_nan
         endif
      endif
 
      ;; Calculate decimal day of year
      M    = Floor(Date / 100.) Mod 100
      D    = Date Mod 100
      Y    = Floor(Date / 1E4)
      H    = Floor(Time / 100.)
      Min  = Time Mod 100
      
      DOY = Get_Doy(Year=Y, Month=M, Day=D, Hour=H, Minute=Min)
 
      ; ======================================================
      ; Construct a structure for the model data
      ; ======================================================
 
      ; number of samples
      nTimes = n_elements( Time )
 
      ; Use US Standard Atmosphere to calculate altitudes
      US_alt = ussa_alt( Press )
 
      ; Set FlightDate to sample Date, unless alternate date given
      ; Allows flights during 00GMT to have 1 FlightDate rather than 2
      if Keyword_Set( FlightDate ) then $
         FD = replicate( FlightDate, nTimes ) $
         else $
         FD = Date
 
      ; Create structure with fields common to all simulations
      GC = { Date: Date,     UTC: Time,     DOY:DOY, $
             Lat:  Lat,      Lon: Lon,      Alt:US_Alt, $
             Pres: Press,    FlightDate: FD }
 
      ;-------------------------------------------------------
      ; Add Met variables to structure
      ;-------------------------------------------------------
 
      ; Add Temperature to structure, if it exists
      ind = where( Varnames_1 eq 'GMAO_TEMP', ct)
      if ( ct ) then $
         GC = StruAddVar( GC, reform( DATA[*,ind] ), 'temp' )
 
      ; Add Absolute humidity to structure, if it exists
      ind = where( Varnames_1 eq 'GMAO_ABSH', ct)
      if ( ct ) then $
         GC = StruAddVar( GC, reform( DATA[*,ind] ), 'absh' )
 
      ; Add Surface Pressure to structure, if it exists
      ind = where( Varnames_1 eq 'GMAO_PSFC', ct)
      if ( ct ) then $
         GC = StruAddVar( GC, reform( DATA[*,ind] ), 'pSurf' )
 
      ; Add U wind to structure, if it exists
      ind = where( Varnames_1 eq 'GMAO_UWND', ct)
      if ( ct ) then $
         GC = StruAddVar( GC, reform( DATA[*,ind] ), 'uwnd' )
 
      ; Add V wind to structure, if it exists
      ind = where( Varnames_1 eq 'GMAO_VWND', ct)
      if ( ct ) then $
         GC = StruAddVar( GC, reform( DATA[*,ind] ), 'vwnd' )
 
      ;-------------------------------------------------------
      ; Add other variables to structure
      ;-------------------------------------------------------
 
      ; Find indices for non-Met variables
      ind = where( Varnames_1 ne 'GMAO_TEMP' and $
                   Varnames_1 ne 'GMAO_ABSH' and $
                   Varnames_1 ne 'GMAO_PSFC' and $
                   Varnames_1 ne 'GMAO_UWND' and $
                   Varnames_1 ne 'GMAO_VWND', ct )
     
      ; Check whether any other variables
      if ( ct le 1) then begin
         
         print, 'NO NON-MET VARIABLES IN FILES...'
      
      endif else begin
 
         ; Add non-Met variables to structure
         For i=0L, ct-1L do begin
 
             ; Add next variable to structure
             GC = StruAddVar( GC, reform( DATA[*,ind[i]] ), $
                              Varnames_1[ind[i]] )
 
         Endfor
 
      Endelse
 
   Endelse
 
   ; Average along flighttrack, if averaging time is given
   if Keyword_Set( avgMin ) and Keyword_Set( GridType ) then Begin

       ; Average data onto GEOS-Chem Grid
       avg_geosgrid, GridType=GridType, TimeStep=avgMin, PlaneIn=GC, $
         Plane_geos=newGC

       ; Rename
       GC = newGC
       
   Endif
 
   ; Save GC structure if filename is given
   If Keyword_Set( outFile ) then begin
 
       ; Print message
       print, 'Saving as ' + outFile
 
       ; Save
       Save, GC, filename=outFile
 
   Endif
 
end
