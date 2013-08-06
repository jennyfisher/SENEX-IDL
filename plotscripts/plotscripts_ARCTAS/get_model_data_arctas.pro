; $Id: get_model_data_arctas.pro,v 1.8 2008/07/07 17:10:31 jaf Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        GET_MODEL_DATA_ARCTAS
;
; PURPOSE:
;        Read GEOS-Chem fields sampled along the ARCTAS aircraft track 
;        In addition to simplifying the file reading, this program
;        also defines common names for fields with awkward names
;
;        See NOTES (below) for where this function looks for the data
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        Data = GET_MODEL_DATA_ARCTAS( Field, Platform, FlightDates[,
;                   Keywords] )
;
; INPUTS:
;        Field       - Name of observation variable. e.g. 'CO', 'DOY', 'ALTP'
;        Platform    - Name of Aircraft. Current options: 'DC8', 'P3B', '*'
;        FlightDates - Date of takeoff as YYYYMMDD or '*' for all ARCTAS
;                      flights.  Also accepts wildcards e.g. '2008041*'
;
; KEYWORD PARAMETERS:
;        NoCities    - If set, the returned Data will not include observations
;                      near Fairbanks, Barrow, or Prudhoe Bay. The criteria
;                      are currently <1.5 degrees from the site and <2 km
;                      altitude.
;
;        Troposphere - If set, the data include only troposphere, defined as
;                      [O3]/[CO] < 1.25
;        AltDir      - If set, I am using output files from a non-standard
;                      run with new emissions, and the directory is changed
;                      appropriately. 
;	 TCO         - If set, I am using tagged CO output, which doesn't
;		       include O3, so O3 must be read from another run if
;		       the troposphere keyword is set.
;	 OFL         - If set, I am using offline output, which doesn't
;		       include O3 or CO, so both must be read from another 
;		       run i fthe troposphere keyword is set.
;
;
; OUTPUTS:
;        Data        - 1D array of all available data for the specified Field,
;                      Platform and FlightDates 
;
; SUBROUTINES:
;        READ_FILE_MODEL
;
; REQUIREMENTS:
;        GEOS-Chem output files (plane.log) must be procesed from ASCII to IDL
;        SAV format before using this function. 
;
; NOTES:
;        This function and its complement, GET_FIELD_DATA_ARCTAS assume
;        a particular directory structure:
;
;        !ARCTAS/
;           field_data/
;              DC8/
;                 merge_60s/        : NASA 60s merge files, converted to SAV
;                 merge_15m_2x25/   : Averages over GEOS-Chem grid and time
;              P3B/
;                 merge_60s/
;                 merge_15m_2x25/
;           gc_data/
;	       gc.v9-01-01/
;                 totHg/         : GEOS-Chem data with one file per
;                                     platform and flight date
;           or, if the AltDir keyword is set
;           gc_data/
;	       gc.v9-01-01/
;                 'name of alternate directory'
;
;        !ARCTAS is a system variable defining the user's root ARCTAS data 
;        directory  e.g. ~/data/ARCTAS 
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        cdh, 12 May 2008: VERSION 1.00
;        cdh, 12 May 2008: Added TROPOSPHERE keyword
;        jaf, 13 Aug 2008: Added AltDir keyword
;        jaf, 18 Jun 2009: Changed default directory structure
;        jaf,  7 Jul 2009: Added OFL keyword, added SOx reading
;
;-
; Copyright (C) 2008, Christopher Holmes, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine get_field_data_arctas"
;-----------------------------------------------------------------------


;--------------------------------------------------------------
; READ_FILE_MODEL is a helper function that extracts one data array
;    from one file.
;
;--------------------------------------------------------------
function read_file_model, file, field, ppt=ppt

  ;------
  ; Define common names for fields with awkward names
  ; Also set up conversion factors to convert to customary units
  ; The output units should generally be the same as the units
  ; used in the aircraft data
  ;------
  field = strlowcase( field )

  Case field of
    'co'  :    conv_factor = 1e9
    'hg0' :    conv_factor = 1e15 ; v/v -> ppqv
    'so2' :    conv_factor = 1e12 ; v/v -> pptv
    'so4' :    conv_factor = 1e12 ; mole/mole -> pptv
    'fa_so4' :    conv_factor = 1e12 ; mole/mole -> pptv
    'saga_so4' :    conv_factor = 1e12 ; mole/mole -> pptv
    'so4s':    conv_factor = 1e12 ; mole/mole -> pptv
    'dms' :    conv_factor = 1e12 ; v/v -> pptv
    'nh4' :    conv_factor = 1e12 ; mole/mole -> pptv
    'saga_nh4' :    conv_factor = 1e12 ; mole/mole -> pptv
    'nh3' :    conv_factor = 1e12 ; v/v -> pptv
    'nit' :    conv_factor = 1e12 ; mole/mole -> pptv
    'no3' :    conv_factor = 1e12 ; mole/mole -> pptv
    'sox' :    conv_factor = 1e12 ; v/v -> pptv
    'fa_sox' :    conv_factor = 1e12 ; v/v -> pptv
    'o3'  :    conv_factor = 1e9  ; v/v -> ppbv
    'oh'  :    conv_factor = 1e12 ; v/v -> pptv
    'no'  :    conv_factor = 1e12 ; pptv
    'hno3':    conv_factor = 1e12; pptv
    'ca'  :    conv_factor = 1e12; pptv 
    ; For tagged CO runs, we have several different CO types
    'cona'    :    conv_factor = 1e9
    'coeur'   :    conv_factor = 1e9
    'cosib'   :    conv_factor = 1e9
    'coas'    :    conv_factor = 1e9
    'cooth'   :    conv_factor = 1e9
    'cobna'   :    conv_factor = 1e9
    'cobeur'  :    conv_factor = 1e9
    'cobnsib' :    conv_factor = 1e9
    'cobssib' :    conv_factor = 1e9
    'cobas'   :    conv_factor = 1e9
    'coboth'  :    conv_factor = 1e9
    else:  conv_factor = 1
  endcase

 ; always forget...
 if ( field eq 'no3') then field = 'nit'

 ; to use fine_aerosol_sulfate
 if ( field eq 'fa_so4' ) then field = 'so4' 
 if ( field eq 'fa_sox' ) then field = 'sox' 

 ; to use SAGA coarse SO4 and NH4
 if ( field eq 'saga_so4' ) then field = 'so4' 
 if ( field eq 'saga_nh4' ) then field = 'nh4' 
 if ( field eq 'saga_no3' ) then field = 'nit' 

 if ( ~keyword_set(ppt) and (field eq 'so4' or field eq 'so4s' $
       or field eq 'nh4' or field eq 'nit') ) then begin
    ; first get nmol/m3 (independent of species)
    conv_factor = conv_factor * (1.29 / 28.97)
    ; next use appropriate molar mass for conversion to ug/m3
    ; use nmole/m3 instead of ug/m3
;    case field of
;         'so4'  : conv_factor = conv_factor * 96d-3
;         'so4s' : conv_factor = conv_factor * 96d-3
;         'nh4'  : conv_factor = conv_factor * 18d-3
;         'nit'  : conv_factor = conv_factor * 62d-3
;    endcase
 endif

  ; Open the Data file
  Restore, File

  ; For AOD, we need to add lots of fields together
  If StRegex( field, '^aod[0-9]*$', /Boolean ) then begin

    Print, 'Reading model AOD entries from file: '+ file +' ...'
    
    ; AOD above the aircraft altitude = Column - (col below aircraft)
    Data =  gc.aodc_sulf + gc.aodc_blkc + gc.aodc_orgc + $
            gc.aodc_sala + gc.aodc_salc - $
          ( gc.aodb_sulf + gc.aodb_blkc + gc.aodb_orgc + $
           gc.aodc_sala + gc.aodc_salc )

    ; Status is success, as long as data contains some elements
    status = n_elements( data ) gt 1

  ; For SOx, we need to add fields together
  endif else If ( field eq 'sox' ) then begin

    Print, 'Reading model SOx from file: '+ file +' ...'
    
    Data =  gc.so2 + gc.so4

    ; Status is success, as long as data contains some elements
    status = n_elements( data ) gt 1

  endif else If ( field eq 'nh4_so4_ratio' ) then begin

    Print, 'Reading model Ammonium & Sulphate from file: '+ file +' ...'
    
    Data =  gc.nh4 / gc.so4

    ; Status is success, as long as data contains some elements
    status = n_elements( data ) gt 1

  endif else If ( field eq 'dust' ) then begin

    Print, 'Reading model dust from file: '+ file +' ...'
    
    Data =  gc.dst1 + gc.dst2 + gc.dst3 + gc.dst4

    ; Status is success, as long as data contains some elements
    status = n_elements( data ) gt 1

  endif else If ( field eq 'ca' ) then begin

    Print, 'Reading model Ca from file: '+ file +' ...'

    ; sum up data, and convert to v/v using assumption of 3% by mass
    Data =  0.03 * (gc.dst1 + gc.dst2 + gc.dst3 + gc.dst4) * (29/40.) * 0.5
    ; added 0.5 to match Duncan's dust source...

    ; Status is success, as long as data contains some elements
    status = n_elements( data ) gt 1

  endif else If ( field eq 'oc' ) then begin

    Print, 'Reading model OC from file: '+ file +' ...'

    ; sum up data, and convert to ug/m3
    Data =  (gc.ocpi + gc.ocpo ) * (1e6*12) / 0.0224

    ; Status is success, as long as data contains some elements
    status = n_elements( data ) gt 1

  endif else If ( field eq 'bc' ) then begin

    Print, 'Reading model BC from file: '+ file +' ...'

    ; sum up data, and convert to ng/m3
    Data =  (gc.bcpi + gc.bcpo ) * (1e9*12) / 0.0224

    ; Status is success, as long as data contains some elements
    status = n_elements( data ) gt 1

  endif else begin

    ;----------
    ; All fields except AOD & SOx read here
    ;----------

    Print, 'Reading model '+field+' from file: '+file+'  ...'

    ; Form a string that reads the desired field 
    s = 'Data = ' + 'gc.' + field 
    status = Execute( s )  

  endelse

  ; When requested data don't exist, give the user a message and return 0
  if ( status eq 0 ) then begin
     print,'******* No model data for '+strupcase(field)
     data=0
     return,data
  endif

  ; Apply conversion factor to data
  data = data * conv_factor

  ; Special case for UTC time, since we want it in seconds after midnight
  ; just like the aircraft
  if field eq 'utc' then $
	data = ( data mod 100 ) * 60 + floor( data / 100.) * 3600

  return, Data
end

;==============================================================
;==============================================================

function get_model_data_arctas, Field_in, Platforms_in, FlightDates_in,    $
                                NoCities=NoCities, Troposphere=Troposphere,$
                                AltDir=AltDir, TCO=TCO, OFL=OFL, HG=HG,    $
                                ppt=ppt,  avgtime=avgtime, hravg=hravg

  ; Rename the internal variables to avoid changing the parameters
  ; that are passed in
  If Keyword_Set( Field_in ) then $
    Field       = Field_in
  If Keyword_Set( Platforms_in ) then $
    Platform    = Platforms_in
  If Keyword_Set( FlightDates_in ) then $
    FlightDates = FlightDates_in

  platform = strlowcase( platform )

  If n_elements(avgtime) eq 0 then avgtime='60m'
  If keyword_set(hravg) then avgtime='60m'

  ; temporary compatibility with SAGA merges
  if avgtime eq 'SAGA' then avgtime='saga'

  If Keyword_Set( AltDir ) then begin
     Model_dir = !ARCTAS+'/gc_data/gc.v9-01-01/'+AltDir+'/'
  endif else Model_dir = !ARCTAS+'/gc_data/gc.v9-01-01/totHg/'

  ; Error message informs user of correct usage
  If n_params() eq 0 then $
    message, 'usage: DATA = GET_MODEL_DATA_ARCTAS( FIELD, PLATFORM, '+$
             'FLIGHTDATES )'
 
  ; Default values for optional parameters 
  If n_params() eq 1 then $
    PLATFORM = 'DC8'
  If n_params() eq 2 then $
    FlightDates = '2008*'

  If StrMatch( Platform[0], 'DC8', /fold_case ) then begin
 
    ; Flightdates can be set as 'FAIRBANKS','CARB', or 'COLDLAKE'
    ; 20080626 is counted as both a CARB flight and a COLDLAKE
    ; flight because it left from Palmdale and arrived in Cold Lake
    If strlowcase(FlightDates_in[0]) eq 'fairbanks' then begin
       Flightdates = ['20080401','20080404','20080405','20080408',$
                      '20080409','20080412','20080416','20080417',$
                      '20080419']
    endif else if strlowcase(FlightDates_in[0]) eq 'fairbanks_ams' then begin 
       Flightdates = ['20080405','20080408','20080409','20080412',$
                      '20080416','20080417','20080419']
    endif else if strlowcase(FlightDates_in[0]) eq 'fairbanks_saga' then begin 
       Flightdates = ['20080409','20080412',$
                      '20080416','20080417','20080419']
    endif else if strlowcase(FlightDates_in[0]) eq 'carb' then begin 
       Flightdates = ['20080618','20080620','20080622','20080624',$
                      '20080626']
    endif else if strlowcase(FlightDates_in[0]) eq 'coldlake' then begin 
       Flightdates = ['20080626','20080629','20080701','20080704',$
                      '20080705','20080708','20080709','20080710',$
                      '20080713']
    endif
 
  endif else if StrMatch( Platform[0], 'P3B', /fold_case ) then begin
    If strlowcase(FlightDates_in[0]) eq 'fairbanks' then begin
       Flightdates = ['20080331','20080401','20080406','20080408',$
                      '20080409','20080413','20080415','20080419']
    endif else if strlowcase(FlightDates_in[0]) eq 'carb' then begin 
       Flightdates = ['20080622','20080624','20080626']
    endif else if strlowcase(FlightDates_in[0]) eq 'coldlake' then begin 
       Flightdates = ['20080626','20080628','20080629','20080630',$
                      '20080702','20080703','20080706','20080707',$
                      '20080709','20080712']
    endif
 
    endif

  ;--------------------------------------------------------------
  ; In order to read multiple dates or platforms
  ; we need to have equal length string arrays for PLATFORM and FLIGHTDATES.
  ; This section generates the correct length arrays if either argument
  ; is passed as a scalar (for shorthand)
  ;--------------------------------------------------------------

  ; Number of platforms (e.g. DC8, P3B) and flight dates
  ; Note that both Platforms and FlightDates can be '*'
  N_Platforms = n_elements( Platform )
  N_Dates     = n_elements( FlightDates )
  N_Entries   = 1

  ; If there are multiple flight dates, then we read the same platform
  ; for all dates
  If ( N_Platforms eq 1 ) and ( N_Dates gt 1 ) then begin

    Platform = Replicate( Platform, N_Dates )
    N_Entries = N_Dates

  endif

  ; If there are multiple platforms, then we read the same flight date
  ; for all platforms 
  If ( N_Dates eq 1 ) and ( N_Platforms gt 1 ) then begin

    FlightDates = Replicate( FlightDates, N_Platforms )
    N_Entries = N_Platforms

  endif

  ; If there are multiple platforms and multiple flight dates, then
  ; there need to be the same number of each. Error if they aren't equal
  ; length arrays
  If ( N_Platforms gt 1 ) and ( N_Dates gt 1 ) then begin
     If ( N_Dates ne N_Platforms ) then begin
        message, 'Platform and FlightDates must either be scalar strings, '+$
             'or have the same number of elements'
     endif else begin
           N_Entries = N_Dates
     endelse
  endif

  ;--------------------------------------------------------------
  ; Find the files and Read the data
  ;
  ;--------------------------------------------------------------

  Files = StrArr( N_Entries ) 

  ; Initialize, Drop this element later
  Data = [0]

  ; Neutralization
  if ( field eq 'acid') then data=get_model_acid_arctas(flightdates) $
  else begin

  ; Loop over the number of distinct Platforms or FlightDates
  ; Note: If FlightDates='*', then it's only one loop iteration here
  For i = 0, N_Entries-1L do begin

    ; The various aircraft platforms each have their own data 
    ; directory, wehich we define here 
    ; Find all of the files matching the FlightDates criteria
    If StrMatch( Platform[i], 'DC8', /fold_case ) then begin

      if avgtime eq '15m' then $
      NewFiles = $
        MFindFile(Model_dir+'*15m*dc8*'+FlightDates[i]+'*.sav') else $
      if avgtime eq '60m' then $
      NewFiles = $
        MFindFile(Model_dir+'*60m*dc8*'+FlightDates[i]+'*.sav') else $
      if avgtime eq 'saga' then $
      NewFiles = $
        MFindFile(Model_dir+'*SAGA*'+FlightDates[i]+'*.sav') else $
      return, 'No '+platform[i]+' files for averaging time '+avgtime

    endif else if StrMatch( Platform[i], 'P3B', /fold_case ) then begin

      if avgtime eq '15m' then $
      NewFiles = $
        MFindFile(Model_dir+'*15m*p3b*'+FlightDates[i]+'*.sav') else $
      if avgtime eq '60m' then $
      NewFiles = $
        MFindFile(Model_dir+'*60m*p3b*'+FlightDates[i]+'*.sav') else $
      return, 'No '+platform[i]+' files for averaging time '+avgtime
    
    endif

      ; Keep only the files that have single flights to avoid duplicate data
      ; in the files that contain multiple flights
      ;NewFiles = NewFiles[ where( StrMatch( NewFiles, '*2008????.sav' ) eq 1) ]

     ; Loop over the number of files found
      ; Read the data from each
      For j = 0L, n_elements( NewFiles )-1L do begin

        Data = [ Data, read_file_model( NewFiles[j], Field, ppt=ppt )]
	
        ; Concatenate the data if it is nonzero
        If ( n_elements( NewData ) gt 1 ) then $
          Data = [ Data, NewData ]

      endfor

  endfor

  endelse ;neutralization

  ; Return NaN if no data were found, 
  ; otherwise drop the first element which is initialized to 0
  If ( n_elements( Data ) eq 1 ) then $
    return, !Values.f_nan else $
    if ( field ne 'acid' ) then Data = Data[1:*]

  ; If keyword NoCities, then filter the data to remove observations
  ; near Fairbanks, Barrow and Prudhoe Bay (i.e. point sources)
  If Keyword_Set( NoCities ) then begin

    ; We need to get latitude, longitude and altitude
    lat = get_model_data_arctas( 'lat', Platform, Flightdates )
    lon = get_model_data_arctas( 'lon', Platform, Flightdates )
    alt = get_model_data_arctas( 'alt', Platform, Flightdates )

    ; In the following tests, we want to eliminate the grid boxes
    ; containing these cities. We are using the 2x2.5 simulation
    ; So if the latitude and longitude are within 1.5 degrees, the 
    ; observation is in the same box as the city.
    ; (At these latitudes, 1deg longitude = 20-25mi)

    ; Define the region near Fairbanks 
    ind_Fairbanks = where( abs( lat - 64.8  ) le 1.5 and $
                           abs( lon + 147.9 ) le 1.5 and $
                           alt le 2, $
                           complement=cind_Fairbanks )

    ; Define the region near Barrow
    ind_Barrow    = where( abs( lat - 76.5  ) le 1.5 and $
                           abs( lon + 156.8 ) le 1.5 and $
                           alt le 2, $
                           complement=cind_Barrow )

    ; Define the region near Prudhoe Bay 
    ind_PrudBay   = where( abs( lat - 70.3  ) le 1.5 and $
                           abs( lon + 148.4 ) le 1.5 and $
                           alt le 2, $
                           complement=cind_PrudBay )

    ; Find the intersection of all indices outside cities
    ind_noCities = cmset_op( cind_Fairbanks, 'AND', cind_Barrow  )
    ind_noCities = cmset_op( cind_PrudBay,   'AND', ind_noCities )

    ; Limit the data to points outside cities
    Data = Data[ind_noCities]
  
  endif

  ;--------------------------------------------------------------
  ; If keyword TROPOSPHERE, then exclude data where [O3]/[CO] > 1.25 
  ; 
  ;--------------------------------------------------------------
  If Keyword_Set( TROPOSPHERE ) AND ( Platform[0] eq 'dc8' ) then begin

    ; We need to read O3 and CO 
    If Keyword_Set( OFL ) or Keyword_Set( HG ) then $
    CO = get_model_data_arctas( 'CO', Platform, Flightdates, $
                                 noCities=noCities,avgtime=avgtime, $
                                 AltDir='fullchem' ) $
    else						     $
    CO = get_model_data_arctas( 'CO', Platform, Flightdates, $
                                 noCities=noCities, AltDir=AltDir, $ 
                                 avgtime=avgtime )
    If Keyword_Set( TCO ) or Keyword_Set( OFL ) or Keyword_Set( HG ) then $
    O3 = get_model_data_arctas( 'O3', Platform, Flightdates, $
                                 noCities=noCities,avgtime=avgtime,  $
                                 AltDir='fullchem' ) $
    else						     $
    O3 = get_model_data_arctas( 'O3', Platform, Flightdates, $
                                 noCities=noCities, AltDir=AltDir, $ 
                                 avgtime=avgtime )

    ind_troposphere = where( O3/CO le 1.25, count )

    If (count ge 1 ) then $
      Data = Data[ind_troposphere]

  endif

  ;--------------------------------------------------------------
  ; Return Data
  ;
  ;--------------------------------------------------------------
  return, Data
end
