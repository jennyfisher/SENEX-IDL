; $Id: get_field_data_senex.pro,v 1.6 2008/07/07 17:03:04 jaf Exp $
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
;        Data = GET_FIELD_DATA_SENEX( Field, Platform, FlightDates[, Keywords] )
;
; INPUTS:
;        Field       - Name of observation variable. e.g. 'CO', 'DOY', 'ALTP'
;        Platform    - Name of Aircraft. Current options: 'DC8', 'P3B', '*'
;        FlightDates - Date of takeoff as YYYYMMDD or '*' for all ARCTAS flights.
;                      Also accepts wildcards e.g. '2008041*'
;
; KEYWORD PARAMETERS:
;        MinAvg      - If set, the returned Data are the 60s averages created
;                      by NASA. The default (MinAvg=0) is to use the observations 
;                      averaged over the GEOS-Chem grid and time resolution 
;                      (2x25 degrees, 15 minutes)
;
;        NoCities    - If set, the returned Data will not include observations
;                      near Fairbanks, Barrow, or Prudhoe Bay. The criteria are 
;                      currently <1.5 degrees from the site and <2 km altitude.
;
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
;        This function and its complement, GET_MODEL_DATA_ARCTAS assume
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
;              nrt.v8-01-1.2x25/    : GEOS-Chem data with one file per
;                                     platform and flight date
;
;        !ARCTAS is a system variable defining the user's root ARCTAS data 
;        directory  e.g. ~/data/ARCTAS 
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        cdh, 12 May 2008: VERSION 1.00
;        cdh, 12 May 2008: Added TROPOSPHERE keyword
;        cdh, 13 May 2008: Now when no data are available for the requested
;                          flight dates, the program returns NaN rather than 0
;        jaf,  7 Jul 2009: Added SOx reading
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
; READ_FILE_FIELD is a helper function that extracts one data array
;    from one file.
;
;--------------------------------------------------------------
function read_file_field_arctas, file, field, platform, ppt=ppt, nss=nss
 
  ;------
  ; Define common names for fields with awkward names
  ;------
  field = strlowcase( field )
  platform = strlowcase( platform )
 
  Case field of
    'alt': field = 'GPS_altitude'
    'lat': field = 'latitude'
    'lon': field = 'longitude'
    'co' : if platform eq 'dc8' then $
	      field = 'carbon_monoxide_mixing_ratio'
    'hg0': field = 'Hg' 
    'ch4': field = 'methane_mixing_ratio' 
    'co2': field = 'co2_mixing_ratio'
    'hcn': field = 'hcn_cit'
    'fa_so4': field = 'fine_aerosol_sulfate'
    'so4': if platform eq 'dc8' then field = 'sulphate' $
           else field = 's04_prelim'
    'nh4': if platform eq 'dc8' then field = 'ammonium' $
           else field = 'nh4__prelim'
    'no3': if platform eq 'dc8' then field = 'nitrate' $
           else field = 'no3__prelim'
    'bc' : field = 'bc_mass_1013hpa_273k'
    'isop': field = 'isoprene'
    'aod350': field = 'aerosol_optical_depth_353_5nm'
    'aod380': field = 'aerosol_optical_depth_380_0nm'
    'aod450': field = 'aerosol_optical_depth_452_6nm'
    'aod500': field = 'aerosol_optical_depth_499_4nm'
    'aod520': field = 'aerosol_optical_depth_519_4nm'
    'aod605': field = 'aerosol_optical_depth_605_8nm'
    'aod675': field = 'aerosol_optical_depth_675_1nm'
    'rea_o1d':field = 'JO3_O2_O1D'
    'rea_no2':field = 'JNO2'
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
 
  ; Special Case for SOx
  endif else If field eq 'fa_sox' then begin
 
      Print, 'Reading fields for fa SOx from file: '+file+' ...'
 
      ; Read the integer day of year
      s = 'so2 = ' + Platform + '.so2' 
      status = Execute( s )  
 
      ; Read the time UTC
      s = 'fine_aerosol_sulfate = ' + Platform + '.fine_aerosol_sulfate' 
      status = Execute( s )  

      if (keyword_set(nss)) then begin
         s = 'sodium = ' + Platform + '.Na'
         status = Execute( s )
         ind = where(~finite(sodium))
         if ind[0] ge 0 then fine_aerosol_sulfate[ind]=!Values.f_nan
         ; seasalt component is 0.252*sodium
         fine_aerosol_sulfate = fine_aerosol_sulfate - $
                    (23./96.) * 0.252 * sodium
      endif

      ; Sum for total SOx
      Data = so2 + fine_aerosol_sulfate
 
  ; Special Case for  fine_aerosol_sulfate
  endif else If field eq 'fine_aerosol_sulfate' then begin
 
      Print, 'Reading fields for fa SO4 from file: '+file+' ...'
 
      ; Read the time UTC
      s = 'fine_aerosol_sulfate = ' + Platform + '.fine_aerosol_sulfate' 
      status = Execute( s )  

      if (keyword_set(nss)) then begin
         s = 'sodium = ' + Platform + '.Na'
         status = Execute( s )
         ind = where(~finite(sodium))
         if ind[0] ge 0 then fine_aerosol_sulfate[ind]=!Values.f_nan
         ; seasalt component is 0.252*sodium in MASS units, but we have ppt
         fine_aerosol_sulfate = fine_aerosol_sulfate - $
                    (23./96.) * 0.252 * sodium
      endif

      ; Convert units from ppt to nmol/m3
      if ~( keyword_set(ppt) ) then $
      ;data = 96d-3 * ( 1.29 / 28.97 ) * fine_aerosol_sulfate $
      data = ( 1.29 / 28.97 ) * fine_aerosol_sulfate $
      else data = fine_aerosol_sulfate

  ; Special Case for coarse SAGA sulfate 
  endif else If field eq 'saga_so4' then begin
 
      Print, 'Reading fields for SO4 from file: '+file+' ...'
 
      ; Read the time UTC
      s = 'so4 = ' + Platform + '.SO4'
      status = Execute( s )  

      if (keyword_set(nss)) then begin
         s = 'sodium = ' + Platform + '.Na'
         status = Execute( s )
         ind = where(~finite(sodium))
         if ind[0] ge 0 then so4[ind]=!Values.f_nan
         ; seasalt component is 0.252*sodium
         so4 = so4 - $
                    (23./96.) * 0.252 * sodium
      endif

      ; Convert units from ppt to nmole/m3
      if ~( keyword_set(ppt) ) then $
      ;data = 96d-3 * ( 1.29 / 28.97 ) * so4 $
      data = ( 1.29 / 28.97 ) * so4 $
      else data = so4

  ; Special Case for coarse SAGA ammonium 
  endif else If field eq 'saga_nh4' then begin
 
      Print, 'Reading fields for NH4 from file: '+file+' ...'
 
      ; Read the time UTC
      s = 'nh4 = ' + Platform + '.NH4'
      status = Execute( s )  

      ; Convert units from ppt to nmole/m3
      if ~( keyword_set(ppt) ) then $
      ;data = 18d-3 * ( 1.29 / 28.97 ) * nh4 $
      data = ( 1.29 / 28.97 ) * nh4 $
      else data = nh4

  ; Special Case for coarse SAGA nitrate 
  endif else If field eq 'saga_no3' then begin
 
      Print, 'Reading fields for NO3 from file: '+file+' ...'
 
      ; Read the time UTC
      s = 'no3 = ' + Platform + '.NO3'
      status = Execute( s )  

      ; Convert units from ppt to nmole/m3
      if ~( keyword_set(ppt) ) then $
      ;data = 62d-3 * ( 1.29 / 28.97 ) * no3 $
      data = ( 1.29 / 28.97 ) * no3 $
      else data = no3

  ; Special Case for SOx
  endif else If field eq 'sox' then begin
 
      Print, 'Reading fields for SOx from file: '+file+' ...'
 
      ; Read the integer day of year
      s = 'so2 = ' + Platform + '.so2' 
      status = Execute( s )  
 
      ; Read the time UTC
      s = 'sulphate = ' + Platform + '.sulphate' 
      status = Execute( s )  

      if (keyword_set(nss)) then begin
         s = 'sodium = ' + Platform + '.Na'
         status = Execute( s )
         ind = where(~finite(sodium))
         if ind[0] ge 0 then sulphate[ind]=!Values.f_nan
         ; seasalt component is 0.252*sodium, but Na is in pptv
         sulphate = sulphate - $
                    0.252 * ( ( 23.*1.29) / 28.97 ) * 1d-3 * sodium
      endif

      ; Convert units from ug/m3 to ppt for addition to SO2 in pptv
      sulphate = ( 28.97 / (96.*1.29) ) * 1d3 * sulphate

      ; Sum for total SOx
      Data = so2 + sulphate
 
  ; Special Case for AMS Sulfate (units of microgram/m3) 
  endif else If field eq 'sulphate' then begin

      Print, 'Reading fields for AMS Sulphate from file: '+file+' ...'

      s = 'sulphate = ' + Platform + '.Sulphate'

      status = Execute( s )

      if (keyword_set(nss)) then begin
         s = 'sodium = ' + Platform + '.Na'
         status = Execute( s )
         ind = where(~finite(sodium))
         if ind[0] ge 0 then sulphate[ind]=!Values.f_nan
         ; seasalt component is 0.252*sodium (MASS ratio), but Na is in pptv
         sulphate = sulphate - $
                    0.252 * ( ( 23.*1.29) / 28.97 ) * 1d-3 * sodium
      endif

      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (96.*1.29) ) * 1d3 * sulphate $
      else $
      ; Convert to nmol/m3
      Data = ( 1d3 / 96. ) * sulphate

  ; Special Case for AMS Ammonium (units of microgram/m3) 
  endif else If field eq 'ammonium' then begin

      Print, 'Reading fields for AMS Ammonium from file: '+file+' ...'

      s = 'ammonium = ' + Platform + '.Ammonium'

      status = Execute( s )

      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (18.*1.29) ) * 1d3 * ammonium $
      else $
      ; Convert to nmol/m3
      Data = ( 1d3 / 18. ) * ammonium

  ; Special Case for AMS Nitrate (units of microgram/m3) 
  endif else If field eq 'nit' or field eq 'nitrate' then begin

      Print, 'Reading fields for AMS Nitrate from file: '+file+' ...'

      s = 'Nitrate = ' + Platform + '.Nitrate'

      status = Execute( s )

      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (62.*1.29) ) * 1d3 * Nitrate $
      else $
      ; Convert to nmol/m3
      Data = ( 1d3 / 62. ) * nitrate

  ; Special Case for AMS Ammonium/Sulphate ratio
  endif else If field eq 'nh4_so4_ratio' then begin

      Print, 'Reading fields for AMS Ammonium & Sulphate from file: '+file

      s = 'ammonium = ' + Platform + '.Ammonium'

      status = Execute( s )

      s = 'sulphate = ' + Platform + '.Sulphate'

      status = Execute( s )

      ; Calculate molar ratio
      Data = ( 96./18. ) * ( ammonium/sulphate )

  ; Special case for P3B fields
  endif else if field eq 's04_prelim' then begin

      Print, 'Reading fields for SO4 from file: '+file+' ...'
 
      ; Read the time UTC
      s = 'so4 = ' + Platform + '.S04_prelim'
      status = Execute( s )  

      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (96.*1.29) ) * 1d3 * so4 $
      else $
      ; Convert from ug/m3 to nmol/m3
      Data = ( 1d3 / 96. ) * so4 
 
  endif else if field eq 'nh4__prelim' then begin

      Print, 'Reading fields for NH4 from file: '+file+' ...'
 
      ; Read the time UTC
      s = 'nh4 = ' + Platform + '.NH4__prelim'
      status = Execute( s )  

      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (18.*1.29) ) * 1d3 * nh4 $
      else $
      ; Convert from ug/m3 to nmol/m3
      Data = (1d3 / 18.) * nh4 
 
  endif else if field eq 'no3__prelim' then begin

      Print, 'Reading fields for NO3 from file: '+file+' ...'
 
      ; Read the time UTC
      s = 'no3 = ' + Platform + '.NO3__prelim'
      status = Execute( s )  

      if (keyword_set(ppt)) then $
      ; Convert units from ug/m3 to ppt
      Data = ( 28.97 / (62.*1.29) ) * 1d3 * no3 $
      else $
      ; Convert from ug/m3 to nmol/m3
      Data = ( 1d3 / 62. ) * no3 
 
  endif else If field eq 'oc' then begin

      Print, 'Reading OC from file: '+file

      s = 'oc = ' + Platform + '.organicslt_213mz'

      status = Execute( s )

      ; Convert from ug C/m3 to ug/m3
      Data = oc / 1.8

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

function get_field_data_arctas, Field_in, Platforms_in, FlightDates_in, $
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
  If n_elements( avgtime ) eq 0 then avgtime='60m'
  If Keyword_Set( minavg ) then avgtime='60s'

  platform = strlowcase(platform)

  ; Directories containing 60s merges from NASA
  ; and averages over GEOS-Chem grid and time resolution 
  If avgtime eq '60s' then mrgDir='merge_60s' $
  else if avgtime eq 'saga' then mrgdir='merge_SAGAAERO' $
  else mrgDir = 'merge_'+avgtime+'_2x25' 

  ; Error message informs user of correct usage 
  If n_params() eq 0 then $
    message, 'usage: DATA = GET_FIELD_DATA_ARCTAS( FIELD, PLATFORM, '+$
             'FLIGHTDATES )'

  ; Default values for optional parameters 
  If n_params() eq 1 then $
    PLATFORM = 'DC8'
  If n_params() eq 2 then $
    FlightDates = '*'

    If StrMatch( Platform[0], 'DC8', /fold_case ) then begin
 
      ; Flightdates can be set as 'FAIRBANKS','CARB', or 'COLDLAKE'
      ; fairbanks_ams is to be used instead of fairbanks when using AMS
      ; observations, which had problems on the first 2 flights
      If strlowcase(FlightDates_in[0]) eq 'fairbanks' then begin
         Flightdates = ['20080401','20080404','20080405','20080408',$
                        '20080409','20080412','20080416','20080417',$
                        '20080419']
      endif else if strlowcase(Flightdates_in[0]) eq 'fairbanks_ams' then begin 
         Flightdates = ['20080405','20080408','20080409','20080412',$
			'20080416','20080417','20080419']
      endif else if strlowcase(Flightdates_in[0]) eq 'fairbanks_saga' then begin 
         Flightdates = ['20080409','20080412',$
			'20080416','20080417','20080419']
      endif else if strlowcase(Flightdates_in[0]) eq 'carb' then begin 
         Flightdates = ['20080618','20080620','20080622','20080624',$
                        '20080626']
      endif else if strlowcase(Flightdates_in[0]) eq 'coldlake' then begin 
         Flightdates = ['20080626','20080629','20080701','20080704',$
                        '20080705','20080708','20080709','20080710',$
                        '20080713']

      endif
 
    endif else if StrMatch( Platform[0], 'P3B', /fold_case ) then begin
      If strlowcase(FlightDates_in[0]) eq 'fairbanks' then begin
         Flightdates = ['20080331','20080401','20080406','20080408',$
                        '20080409','20080413','20080415','20080419' ]
      endif else if strlowcase(Flightdates_in[0]) eq 'carb' then begin 
         Flightdates = ['20080622','20080624','20080626']
      endif else if strlowcase(Flightdates_in[0]) eq 'coldlake' then begin 
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

  ; neutralization (nh4/[2*so4+no3])
  if ( field eq 'acid' ) then data=get_field_acid_arctas(flightdates) $
  else begin
 
  ; Loop over the number of distinct Platforms or FlightDates
  ; Note: If FlightDates='*', then it's only one loop iteration here
  For i = 0, N_Entries-1L do begin

    ; The various aircraft platforms each have their own data 
    ; directory, which we define here 
    ; Find all of the files matching the FlightDates criteria
    If StrMatch( Platform[i], 'DC8', /fold_case ) then begin
 
      NewFiles = $
        MFindFile(!ARCTAS+'/field_data/DC8/'+mrgDir+$
                  '/*'+FlightDates[i]+'*.sav')
 
    endif else if StrMatch( Platform[i], 'P3B', /fold_case ) then begin
 
      NewFiles = $
        MFindFile(!ARCTAS+'/field_data/P3B/'+mrgDir+$
                  '/*'+FlightDates[i]+'*.sav')
    
    endif

      ; Loop over the number of files found
      ; Read the data from each
      For j = 0L, n_elements( NewFiles )-1L do begin

        NewData = read_file_field_arctas( NewFiles[j], Field, Platform[i], ppt=ppt,$
		  nss=nss )

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

  ;--------------------------------------------------------------
  ; If keyword NoCities, then filter the data to remove observations
  ; near Fairbanks, Barrow and Prudhoe Bay (i.e. point sources)
  ;--------------------------------------------------------------
  If Keyword_Set( NoCities ) then begin
 
    ; We need to read latitude, longitude and altitude
    lat = get_field_data_arctas( 'lat', Platform, Flightdates, $
                                 avgtime=avgtime, minavg=minavg )
    lon = get_field_data_arctas( 'lon', Platform, Flightdates, $
                                 avgtime=avgtime, minavg=minavg )
    alt = get_field_data_arctas( 'alt', Platform, Flightdates, $
                                 avgtime=avgtime, minavg=minavg )
 
    ; In the following searches, we locate and exclude observations
    ; within 1.5 degrees of the source. 
    ; (At these latitudes, 1deg longitude = 20-25mi)
 
    ; Define the region near Fairbanks 
    ind_Fairbanks = where( abs( lat - 64.8         ) le 1.5 and $
                           abs( lon - (-147.9+360) ) le 1.5 and $
	                   alt le 2, $
                           complement=cind_Fairbanks )
 
    ; Define the region near Barrow
    ind_Barrow    = where( abs( lat - 76.5         ) le 1.5 and $
                           abs( lon - (-156.8+360) ) le 1.5 and $
	                   alt le 2, $
                           complement=cind_Barrow )
 
    ; Define the region near Prudhoe Bay 
    ind_PrudBay   = where( abs( lat - 70.3         ) le 1.5 and $
                           abs( lon - (-148.4+360) ) le 1.5 and $
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
    CO = get_field_data_arctas( 'CO', Platform, Flightdates, $
                                 avgtime=avgtime, noCities=noCities, $
                                 minavg=minavg )
    O3 = get_field_data_arctas( 'O3', Platform, Flightdates, $
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
