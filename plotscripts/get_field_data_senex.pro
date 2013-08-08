; $Id: get_field_data_senex.pro, 2013/07/29 lei Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        GET_FIELD_DATA_SENEX
;
; PURPOSE:
;        Read observations from the SENEX aircraft 
;        In addition to simplifying the file reading, this program
;        also defines common names for fields with awkward names
;
;        See NOTES (below) for where this function looks for the data
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        Data = GET_FIELD_DATA_SENEX (Field, Platform, FlightDates[, Keywords] )
;
; INPUTS:
;        Field       - Name of observation variable. e.g. 'CO', 'DOY', 'ALTP'
;        Platform    - Name of Aircraft. Current options: 'WP3D'
;        FlightDates - Date of takeoff as YYYYMMDD or '*' for all SENEX flights.
;                      Also accepts wildcards e.g. '2008041*'
;
; KEYWORD PARAMETERS:
;        MinAvg      - If set, the returned Data are the 60s averages created
;                      by NASA. The default (MinAvg=0) is to use the observations 
;                      averaged over the GEOS-Chem grid and time resolution 
;                      (0.25x0.3125 degrees, 10 minutes)
;
;        NoCities    - NOT CURRENTLY USED. If implemented, the returned Data will
;                      not include observations near major cities. The criteria are 
;                      currently <1.5 degrees from the site and <2 km altitude.

;        Troposphere - If set, the data include only troposphere, defined as
;                      [O3]/[CO] < 1.25
;
; OUTPUTS:
;        Data        - 1D array of all available data for the specified Field,
;                      Platform and FlightDates 
;
; SUBROUTINES:
;        READ_FILE_FIELD
;
; REQUIREMENTS:
;        NASA 60s merge files must be processed into IDL SAV files before
;        using this function.
;
; NOTES:
;        This function and its complement, GET_MODEL_DATA_SENEX assume
;        a particular directory structure:
;
;        !SENEX/
;           field_data/
;              WP3D/
;                 merge_60s/               : NASA 60s merge files, converted to SAV
;                 merge_10m_0.25x0.3125/   : Averages over GEOS-Chem grid and time
;           gc_data/ :  GEOS-Chem data with one file per
;                       platform and flight date
;
;        !SENEX is a system variable defining the user's root SENEX data 
;        directory  e.g. ~/3Campiagn/SENEX
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        cdh, 12 May 2008: VERSION 1.00
;        cdh, 12 May 2008: Added TROPOSPHERE keyword
;        cdh, 13 May 2008: Now when no data are available for the requested
;                          flight dates, the program returns NaN rather than 0
;        jaf,  7 Jul 2009: Added SOx reading
;	 lei, 29 Jul 2013: Updated for SENEX
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
; READ_FILE_FIELD is a helper function that extracts one data array
;    from one file.
;
;--------------------------------------------------------------
function read_file_field_senex, file, field, platform, ppt=ppt, nss=nss
 
  ;------
  ; Define common names for fields with awkward names
  ;------
  field = strlowcase( field )
  platform = strlowcase( platform )

  ; Case for SENEX, lei
  ; Note: The AMS data are very preliminary as 
  ; they have not incorporated their calibrations.
  ; So here we only look at simpile species
  Case field of
    'alt'    : field = 'GPSAlt'
    'altp'   : field = 'PAlt'
    'lat'    : field = 'latitude'
    'lon'    : field = 'longitude'
    'press'  : field = 'PRESSURE'
    'utc'    : field = 'UTC'
    'chocho' : field = 'CHOCHO'
    'co'     : field = 'CO_ppbv'
    'hcho'   : field = 'HCHO_ppbv'
    'hno3'   : field = 'HNO3_ppbv'
    'hcooh'  : field = 'HCOOH_ppbv'
    'no'     : field = 'NO_ppbv'
    'no2'    : field = 'NO2_ppbv'
    'noy'    : field = 'NOy_ppbv'
    'o3'     : field = 'O3_ppbv'
    'so2'    : field = 'SO2_ppbv'
    'pan'    : field = 'PAN_ppbv'
    'ppn'    : field = 'PPN_ppbv'
    'ch3cn'  : field = 'acetonitrile_pptv'
    'ald2'   : field = 'acetaldehyde_pptv'
    'acet'   : field = 'acetone_pptv'
    'isop'   : field = 'isoprene_pptv'
    'mek'    : field = 'MEK_pptv'
    'so4'    : field = 'AMS_SO4'
    'no3'    : field = 'AMS_NO3'
    'nh4'    : field = 'AMS_NH4'
    'oa'     : field = 'AMS_Org'
    'cl'     : field = 'AMS_Chl'
    'bc'     : field = 'BC_MASS_90_550_NM_HDSP2'
    else:
  endcase

  ; Open the Data file
  Restore, File

  ; Special Case for reading Day of Year 
  If field eq 'doy' then begin
 
      Print, 'Reading fields for DOY from file: '+file+' ...'
 
      ; Read the integer day of year
      s = 'jday = ' + Platform + '.jday' 
      status = Execute( s )  
 
      ; Read the time UTC
      s = 'utc = ' + Platform + '.utc' 
      status = Execute( s )  

      ; Form the fractional DOY from the integer part and fractional part 
      Data = jday + utc / (24. * 3600.) 
 
  ; Special Case for altitude 
  endif else If (field eq 'GPSAlt' or field eq 'PAlt') then begin

      Print, 'Reading altitude field from file: '+file+' ...'
  
      ; Read the appropriate altitude field
      s = 'alt = ' + Platform + '.' + field
      status = Execute( s )  

      ; Convert from m to km
      Data = alt/1d3
   
  ; Special Case for SOx
  endif else If field eq 'sox' then begin
   
      Print, 'Reading fields for SOx from file: '+file+' ...'
  
      ; Read the integer day of year
      s = 'so2 = ' + Platform + '.so2_ppbv' 
      status = Execute( s )  
 
      ; Read the time UTC
      s = 'so4= ' + Platform + '.AMS_SO4' 
      status = Execute( s )  
  
  ; No sodium measurement on SENEX
  ;    if (keyword_set(nss)) then begin
  ;       s = 'sodium = ' + Platform + '.Na'
  ;       status = Execute( s )
  ;       ind = where(~finite(sodium))
  ;       if ind[0] ge 0 then so4[ind]=!Values.f_nan
  ;       ; seasalt component is 0.252*sodium
  ;       so4= so4 - (23./96.) * 0.252 * sodium
  ;    endif

      ; Sum for total SOx
      Data = so2 + so4
 
 
  ; Special Case for AMS Sulfate (units of microgram/m3) 

  endif else If field eq 'AMS_SO4' then begin
  
      Print, 'Reading fields for AMS SO4 from file: '+file+' ...'
  
      s = 'AMS_SO4 = ' + Platform + '.AMS_SO4'
  
      status = Execute( s )
  
  ;    if (keyword_set(nss)) then begin
  ;       s = 'sodium = ' + Platform + '.Na'
  ;       status = Execute( s )
  ;       ind = where(~finite(sodium))
  ;       if ind[0] ge 0 then AMS_SO4[ind]=!Values.f_nan
  ;       ; seasalt component is 0.252*sodium (MASS ratio), but Na is in pptv
  ;       AMS_SO4 = AMS_SO4 - $
  ;                  0.252 * ( ( 23.*1.29) / 28.97 ) * 1d-3 * sodium
  ;    endif
  
      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (96.*1.29) ) * 1d3 * AMS_SO4 $
      else $
      ; Convert to nmol/m3
      Data = ( 1d3 / 96. ) * AMS_SO4

  ; Special Case for AMS Ammonium (units of microgram/m3) 
  endif else If field eq 'AMS_NH4' then begin
  
      Print, 'Reading fields for AMS NH4 from file: '+file+' ...'
  
      s = 'AMS_NH4 = ' + Platform + '.AMS_NH4'
  
      status = Execute( s )
  
      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (18.*1.29) ) * 1d3 * AMS_NH4 $
      else $
      ; Convert to nmol/m3
      Data = ( 1d3 / 18. ) * AMS_NH4

  ; Special Case for AMS Nitrate (units of microgram/m3) 
  endif else If field eq 'nit' or field eq 'AMS_NO3' then begin
  
      Print, 'Reading fields for AMS NO3 from file: '+file+' ...'
  
      s = 'AMS_NO3 = ' + Platform + '.AMS_NO3'
  
      status = Execute( s )
  
      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (62.*1.29) ) * 1d3 * AMS_NO3 $
      else $
      ; Convert to nmol/m3
      Data = ( 1d3 / 62. ) * AMS_NO3

  ; Special Case for AMS Ammonium/Sulphate ratio
  endif else If field eq 'nh4_so4_ratio' then begin
  
       Print, 'Reading fields for AMS Ammonium & Sulphate from file: '+file
  
      s = 'AMS_NH4 = ' + Platform + '.AMS_NH4'
  
      status = Execute( s )
   
      s = 'AMS_SO4 = ' + Platform + '.AMS_SO4'
  
      status = Execute( s )
  
      ; Calculate molar ratio
      Data = ( 96./18. ) * ( AMS_NH4/AMS_SO4 )

  endif else If field eq 'oa' then begin
  
      Print, 'Reading OC from file: '+file
  
      s = 'oc = ' + Platform + '.AMS_Org'
  
      status = Execute( s )
  
      ; Convert from ug C/m3 to ug/m3
      Data = oc * 2.1

  endif else begin

      ;----------
      ; All fields except DOY & SOx read here
      ;----------
 
      Print, 'Reading '+field+' from file: '+file+'  ...'
 
      ; Form a string tha reads the field from the desired platform
      s = 'Data = ' + Platform + '.' + field 
      status = Execute( s )  
 
  endelse

  ; When requested data don't exist, give the user a message and return 0
  if ( status eq 0 ) then begin
     print,'******* No field data for '+strupcase(field)
     return, 0
  endif
 
  return, Data
end
 
;==============================================================
;==============================================================

function get_field_data_senex, Field_in, Platforms_in, FlightDates_in, $
                                avgtime=avgtime, NoCities=NoCities,       $
                                Troposphere=Troposphere, ppt=ppt, nss=nss,$
                                minavg=minavg
  
  ; Rename the internal variables to avoid changing the parameters
  ; that are passed in
  If Keyword_Set( Field_in ) then $
     Field       = Field_in
  If Keyword_Set( Platforms_in ) then $
     Platform    = Platforms_in
  If Keyword_Set( FlightDates_in ) then $
     FlightDates = FlightDates_in
  If n_elements( avgtime ) eq 0 then avgtime='10m'
  If Keyword_Set( minavg ) then avgtime='60s'

  platform = strlowcase(platform)

  ; Directories containing 60s merges from NASA
  ; and averages over GEOS-Chem grid and time resolution 
  If avgtime eq '60s' then mrgDir='merge_60s' $
  else mrgDir = 'merge_'+avgtime+'_0.25x0.3125' 

  ; Error message informs user of correct usage 
  If n_params() eq 0 then $
    message, 'usage: DATA = GET_FIELD_DATA_SENEX( FIELD, PLATFORM, '+$
             'FLIGHTDATES )'

  ; Default values for optional parameters 
  If n_params() eq 1 then PLATFORM = 'WP3D'
  If n_params() eq 2 then FlightDates = '*'
    
 ;--------------------------------------------------------------
  ; In order to read multiple dates or platforms
  ; we need to have equal length string arrays for PLATFORM and FLIGHTDATES.
  ; This section generates the correct length arrays if either argument
  ; is passed as a scalar (for shorthand)
  ;--------------------------------------------------------------

  ; Number of platforms (e.g. WP3D) and flight dates
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

  ; neutralization (nh4/[2*so4+no3])
  ; code this later (jaf, 8/5/13)
  ;if ( field eq 'acid' ) then data=get_field_acid_senex(flightdates) $
  ;else begin
 
  ; Loop over the number of distinct Platforms or FlightDates
  ; Note: If FlightDates='*', then it's only one loop iteration here
  For i = 0, N_Entries-1L do begin

    ; The various aircraft platforms each have their own data 
    ; directory, which we define here 
    ; Find all of the files matching the FlightDates criteria
    If StrMatch( Platform[i], 'WP3D', /fold_case ) then begin
 
      NewFiles = $
        MFindFile(!SENEX+'/field_data/WP3D/'+mrgDir+$
                  '/*'+FlightDates[i]+'*.sav')
    endif

      ; Loop over the number of files found
      ; Read the data from each
      For j = 0L, n_elements( NewFiles )-1L do begin

        NewData = read_file_field_senex( NewFiles[j], Field, Platform[i], ppt=ppt,$
		  nss=nss )

        ; Concatenate the data if it is nonzero
        If ( n_elements( NewData ) gt 1 ) then $  
          Data = [ Data, NewData ] 
 
      endfor
 
  endfor

  ;endelse ;neutralization 
  
  ; Return NaN if no data were found, 
  ; otherwise drop the first element which is initialized to 0
  If ( n_elements( Data ) eq 1 ) then $
    return, !Values.f_nan else $ 
  if ( field ne 'acid' ) then Data = Data[1:*]

  ; Comment out, lei
;  ;--------------------------------------------------------------
;  ; If keyword NoCities, then filter the data to remove observations
;  ; near cities (i.e. point sources)
;  ;--------------------------------------------------------------
;  If Keyword_Set( NoCities ) then begin
; 
;    ; We need to read latitude, longitude and altitude
;    lat = get_field_data_senex( 'lat', Platform, Flightdates, $
;                                 avgtime=avgtime, minavg=minavg )
;    lon = get_field_data_senex( 'lon', Platform, Flightdates, $
;                                 avgtime=avgtime, minavg=minavg )
;    alt = get_field_data_senex( 'alt', Platform, Flightdates, $
;                                 avgtime=avgtime, minavg=minavg )
; 
;    ; In the following searches, we locate and exclude observations
;    ; within 1.5 degrees of the source. 
;    ; (At these latitudes, 1deg longitude = 20-25mi)
; 
;    ; Define the region near Fairbanks 
;    ind_Fairbanks = where( abs( lat - 64.8         ) le 1.5 and $
;                           abs( lon - (-147.9+360) ) le 1.5 and $
;	                   alt le 2, $
;                           complement=cind_Fairbanks )
; 
;    ; Define the region near Barrow
;    ind_Barrow    = where( abs( lat - 76.5         ) le 1.5 and $
;                           abs( lon - (-156.8+360) ) le 1.5 and $
;	                   alt le 2, $
;                           complement=cind_Barrow )
; 
;    ; Define the region near Prudhoe Bay 
;    ind_PrudBay   = where( abs( lat - 70.3         ) le 1.5 and $
;                           abs( lon - (-148.4+360) ) le 1.5 and $
;	                   alt le 2, $
;                           complement=cind_PrudBay )
; 
;    ; Find the intersection of all indices outside cities
;    ind_noCities = cmset_op( cind_Fairbanks, 'AND', cind_Barrow  )
;    ind_noCities = cmset_op( cind_PrudBay,   'AND', ind_noCities )
; 
;    ; Limit the data to points outside cities
;    Data = Data[ind_noCities]
;  
;  endif
; 
  ;--------------------------------------------------------------
  ; If keyword TROPOSPHERE, then exclude data where [O3]/[CO] > 1.25 
  ; 
  ;--------------------------------------------------------------
  If Keyword_Set( TROPOSPHERE ) AND ( Platform[0] eq 'wp3d' ) then begin

    ; We need to read O3 and CO 
    CO = get_field_data_senex( 'CO', Platform, Flightdates, $
                                 avgtime=avgtime, noCities=noCities, $
                                 minavg=minavg )
    O3 = get_field_data_senex( 'O3', Platform, Flightdates, $
                                 avgtime=avgtime, noCities=noCities, $ 
                                 minavg=minavg )

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
