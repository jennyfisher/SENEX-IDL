; $Id: arctas_obs_mod_curtains.pro,v 1.4 2008/07/07 17:15:45 jaf Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        ARCTAS_OBS_MOD_CURTAINS
;
; PURPOSE:
;	 Make plots of the ARCTAS aircraft data along the flight
;	 track (with altitude as the y-axis), the GEOS-Chem model
;	 output along the flight track, and a "curtain" showing
;	 GEOS-Chem output at all altitudes along the latitude &
;	 longitude coordinates of the flight.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        ARCTAS_OBS_MOD_CURTAINS, Species_In, Platform, DiagN,
;	 			  Tracer[, Keywords] 
;
; INPUTS:
;	 Species_In - Name of observation variable. e.g. 'CO','ALTP'
;			Default is 'CO'.
;	 Platform   - Name of aircraft. Current options: 'DC8','P3B'
;			Default is 'DC8'.
;	 DiagN      - Name of GEOS-Chem diagnostic. e.g. 'IJ-AVG-$'	
;	 Tracer     - Number of GEOS-Chem tracer. e.g. 4 (for CO)
;
; KEYWORD PARAMETERS:
;	 Flightdates - Date of takeoff as 'YYYYMMDD'. Only one date
;		       can be used.
;			Default is '20080401'.
;	 fscale      - Scale factor for the GEOS-Chem timeseries
;		       files used to make the curtains. Most data
;		       are stored in ppbv, so use fscale to convert
;		       to different units for plotting.
;			Default is 1.
;	 MinData     - Minimum to use when plotting data.
;			Default is Min(Data)
;	 MaxData     - Maximum to use when plotting data.
;			Default is Max(Data)
;	 mMinData    - Minimum to use when plotting model output.
;			Default is MinData
;	 mMaxData    - Maximum to use when plotting model output.
;			Default is MaxData
;	 Unit        - Units of the data to be plotted.
;	 Ztop        - Uppermost altitude (in km) for the curtains.
;	 OPlot_Data  - If set, the observed data from the DC8 will
;		       be plotted on top of the curtain.
;
; OUTPUTS:
;	None
;
; SUBROUTINES:
;	None
;
; REQUIREMENTS:
;	GET_MODEL_DATA_ARCTAS
;	GET_FIELD_DATA_ARCTAS
;	SCATTERPLOT_DATACOLOR
;	TVCURTAIN
;	Several Gamap2 routines
;
; NOTES:
;	If no model diagnostics are available for the specified
;	species, the routine will stop after the observations are plotted.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        jaf, 13 May 2008: VERSION 1.00
;
;-
; Copyright (C) 2008, Jenny Fisher, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to jaf@io.as.harvard.edu
; with subject "IDL routine arctas_obs_mod_curtains"
;-----------------------------------------------------------------------

; We need Construct_Map_Labels, so compile Map_Labels
@map_labels

pro arctas_obs_mod_curtains, species_in, platform, diagn, tracer,          $
                            flightdates = flightdates, fscale   = fscale,  $
                            mindata     = mindata,     maxdata  = maxdata, $
                            mmindata    = mmindata,    mmaxdata = mmaxdata,$
                            unit        = unit,        ztop     = ztop,    $
			    oplot_data  = oplot_data,  aod      = aod,     $
                            _extra   = _extra
 
; Set default values if parameters / keywords aren't specified 
if N_Elements(species_in) eq 0 then species_in='CO'
if N_Elements(platform) eq 0 then platform = 'DC8'
if N_Elements(flightdates) eq 0 then flightdates='20080401'
if N_Elements(fscale) eq 0 then fscale = 1
if N_Elements(ztop) eq 0 then ztop = 12
if N_Elements(unit) eq 0 then unit = ''

; By default, we assume both observations and model output are
; available for the specified tracer and set Obs_Only = 0.
; If model output is not available, this will be changed later.
Obs_Only=0

; Get observational data 
species = $
  get_field_data_arctas(species_in, platform, flightdates, /minavg, $
                        _extra = _extra)
altp = get_field_data_arctas('alt', platform, flightdates, /minavg, $
                             _extra = _extra)
lat =  get_field_data_arctas('lat', platform, flightdates, /minavg, $
                             _extra = _extra)
lon =  get_field_data_arctas('lon', platform, flightdates, /minavg, $
                             _extra = _extra)
time = get_field_data_arctas('utc', platform, flightdates, /minavg, $
                             _extra = _extra)
doy =  get_field_data_arctas('doy', platform, flightdates, /minavg, $
                             _extra = _extra)

; Get model output for given species 
species_mod = get_model_data_arctas(species_in, 'DC8',flightdates, $
              _extra=_extra)
; If no model output is available, set Obs_Only keyword and skip
; irrelevant commands
if (N_Elements(species_mod) eq 1 ) then begin
   Obs_Only=1
   goto,nomodel
endif

; Get other necessary model parameters, and interpolate model output 
doy_mod = get_model_data_arctas('DOY', 'DC8', flightdates,_extra=_extra)
species_mod = interpol( species_mod, doy_mod, doy )
 
nomodel:
; nPts is the number of observations
nPts = n_elements(lat)

; Convert measured time (in seconds past midnight UTC) to hours
time  = time/3600.
 
; Several flights span two days of UTC time. If there are
; observations after midnight, convert these to hour after UTC on
; the next day.
ind2 = where(time ge 24.)
if ind2(0) gt 0 then begin
   time(ind2) = time(ind2)-24.
   ; nDay1 is the index of the last point on the first UTC day
   nDay1 = ind2(0)-1
   ; nDay2 is the index of the last point on the second UTC day
   nDay2 = ind2(n_elements(ind2)-1)
endif else nDay1 = nPts-1
 
;---------------------------------------------------------------------------
;  Plot observations along flight track
;---------------------------------------------------------------------------
 
; Constuct labels for x/y axes. p_lat and p_lon are placeholder
; lat/lon values to use for making labels.
p_lat = congrid( lat, 8, /interp, /minus_one )
p_lon = congrid( lon, 8, /interp, /minus_one )
xtickname_lat = construct_map_labels( p_lat,/nodegree,/lat, format='(F5.1)')
xtickname_lon = construct_map_labels( p_lon,/nodegree,/lon, format='(F5.1)')

; Set min and max values for data and model if none were specified.
if (n_elements(mindata ) eq 0) then mindata  = min(species)
if (n_elements(maxdata ) eq 0) then maxdata  = max(species)
if (n_elements(mmindata) eq 0) then mmindata = mindata
if (n_elements(mmaxdata) eq 0) then mmaxdata = maxdata

; Set up colortables. 33 is prefered because low values are visisble
; against the white background 
myct, 33, ncolors = 30
multipanel, /off

; Plot flight data along flight track
window, 0
scatterplot_datacolor, findgen(nPts)+1, altp, species, zmin=mindata,      $
      zmax=maxdata, /xstyle, /ystyle, ytitle = 'Pressure Altitude(km)',   $
      yrange = [0, 12], xticks = 7, xtickname = xtickname_lat,            $
      unit=unit, title = 'Observed '+strupcase(Platform)+' '+             $
      strupcase(species_in), CBposition=[0.15,0.85,.6,0.9], _extra=_extra
 
; Add longitude labels
axis, !x.window[0], /xaxis, /norm, xticks=7, xminor=1, color=1,           $
      xtickname=xtickname_lon, xcharsize=1, ycharsize=1

; If there is no model output for this species, then stop here. 
if Obs_Only then stop

; Plot model output along flight track 
window, 1
scatterplot_datacolor, findgen(nPts)+1, altp, species_mod, zmin=mmindata, $
      zmax=mmaxdata, /xstyle, /ystyle, ytitle = 'Pressure Altitude(km)',  $
      yrange = [0, 12], xticks = 7, xtickname = xtickname_lat,            $
      unit=unit, title = 'Modeled '+strupcase(Platform)+' '+              $
      strupcase(species_in), CBposition=[0.15,0.85,.6,0.9], _extra=_extra
 
; Add longitude labels
axis, !x.window[0], /xaxis, /norm, xticks=7, xminor=1, color=1,$
      xtickname=xtickname_lon, xcharsize=1, ycharsize=1
 
;---------------------------------------------------------------------------
;  Plot GEOS-Chem cross-section (curtain) along flight track
;---------------------------------------------------------------------------
; Specify filename for timeseries files
;tsfi = '/as/pub/ftp/pub/geos-chem/NRT-ARCTAS/timeseries/ts'+ $
;       flightdates+'.bpch'
tsfi = '/home/jaf/runs/run.old/run.ARCTAS.v8-02-03.2x25/for_cubison/ts'+ $
       flightdates+'.bpch'

; Get timeseries data

; Special handling for AOD
if keyword_set(AOD) then begin
   DIAGN='OD-MAP-$'
   ctm_get_data, datainfo, DiagN, filename = tsfi, tracer = 6
   ctm_get_data, datainfo1, DiagN, filename = tsfi, tracer = 9
   ctm_get_data, datainfo2, DiagN, filename = tsfi, tracer = 12
   ctm_get_data, datainfo3, DiagN, filename = tsfi, tracer = 15
   ctm_get_data, datainfo4, DiagN, filename = tsfi, tracer = 18
   ctm_get_data, datainfo5, DiagN, filename = tsfi, tracer = 4
endif else ctm_get_data, datainfo, DiagN, filename = tsfi, tracer = tracer
; nBlks is the number of blocks (nominally, one for every time)
nBlks = n_elements(datainfo)

; For ARCTAS, only the northern hemisphere is saved in the
; timeseries files, so the indexing doesn't match the default
; indexing in gamap routines. The first index in each dimension is
; saved in datainfo.first, so this is a generic way of finding
; the first index for both the longitude and latitude dimensions.
first=datainfo.first(0)
; iFirst is the first index in the longitude dimension. We subtract
; 1 so that this matches IDL indexing (starting at 0).
iFirst=first(0)-1
first=datainfo.first(1)
; jFirst is the first index in the latitude dimension.
jFirst=first(0)-1

; Extract grid information. 
ModelInfo = ctm_type( 'GEOS5', res = 2 )
GridInfo  = ctm_grid(ModelInfo)

; Find the index of the vertical gridbox nearest to the top of the
; domain, specified by ztop.
near_z = Min(abs(gridinfo.zmid-ztop),iztop)
zmid = GridInfo.zmid(0:iztop)

; Find xdim and ydim, the total size of the longitude and latitude
; dimensions, respectively
temp = *(datainfo(0).data)
temp = size(temp, /dimensions)
xdim = temp(0)
ydim = temp(1)

; Get the longitudes included in the domain
lon_mod = GridInfo.xmid(iFirst:xdim+iFirst-1)
; Convert longitudes from [-180,180] to [0, 360] for consistency
; with aircraft data
ind = where(lon_mod lt 0)
lon_mod(ind) = lon_mod(ind)+360
; Get the latitudes included in the domain
lat_mod = GridInfo.ymid(jFirst:ydim+jFirst-1)
 
; Get the UTC for each record in the timeseries files (in units of hour)
UTC = dblarr( nBlks )
for i = 0, nBlks-1 do begin
   UTC[i] = datainfo[i].tau0 - nymd2tau( long(flightdates) )
endfor
 
; Loop over the 1-min observations for the first day.
  for i = 0, nDay1 do begin
 
    ; Extract the sampled vertical profile by finding the index
    ; of the nearest model lon, lat, and time
    near_time = Min(Abs(UTC - Time[i]),j)
    near_lat  = Min(Abs(lat_mod - Lat[i]), jj)
    near_lon  = Min(Abs(lon_mod - Lon[i]), ii)
    ; Special handling for AOD
    if keyword_set(AOD) then $
       array = *( datainfo[j].data ) + *( datainfo1[j].data ) + $
               *( datainfo2[j].data ) + *( datainfo3[j].data ) + $
               *( datainfo4[j].data ) + *( datainfo5[j].data ) else $
       array = *( datainfo[j].data )
    ; Scale the profile by fscale to get the units right
    array = array * fscale
    ; Using only the data up to ztop, save the profile
    profile = reform( array(ii, jj, 0:iztop))
 
    ; Combine vertical profiles into a cross-section
    if ( i eq 0 ) then begin
      curtain = transpose( profile )
    endif else begin
      curtain = [curtain, transpose( profile ) ]
   endelse
 
  endfor  ; End of loop over # of 1-min samples for Day 1

; If the flight spans 2 UTC days, repeat the process 
if N_Elements(nDay2) ne 0 then begin
   ; Increment the flightdate by 1 in the timeseries filename
   ;tsfi = '/as/pub/ftp/pub/geos-chem/NRT-ARCTAS/timeseries/ts'+ $
   tsfi = '/home/jaf/runs/run.old/run.ARCTAS.v8-02-03.2x25/for_cubison/ts'+ $
     string(long(flightdates)+1, format = '(i8.8)')+'.bpch'
   ; Get timeseries data
   ; Special handling for AOD
   if keyword_set(AOD) then begin
      DIAGN='OD-MAP-$'
      ctm_get_data, datainfo, DiagN, filename = tsfi, tracer = 6
      ctm_get_data, datainfo1, DiagN, filename = tsfi, tracer = 9
      ctm_get_data, datainfo2, DiagN, filename = tsfi, tracer = 12
      ctm_get_data, datainfo3, DiagN, filename = tsfi, tracer = 15
      ctm_get_data, datainfo4, DiagN, filename = tsfi, tracer = 18
      ctm_get_data, datainfo5, DiagN, filename = tsfi, tracer = 4
   endif else ctm_get_data, datainfo, DiagN, filename = tsfi, tracer = tracer
   nBlks = n_elements(datainfo)
 
   ; Get UTC for each data record
   UTC = dblarr( nBlks )
   for i = 0, nBlks-1 do begin
      UTC[i] = datainfo[i].tau0 - nymd2tau( long(flightdates)+1 )
   endfor
 
   ; Loop over # of 1-min samples
   for i = nDay1+1, nDay2 do begin
 
      ; Extract sampled vertical profile
      near_time = Min(Abs(UTC - Time[i]),j)
      near_lat  = Min(Abs(lat_mod - Lat[i]), jj)
      near_lon  = Min(Abs(lon_mod - Lon[i]), ii)
       ; Special handling for AOD
       if keyword_set(AOD) then $
          array = *( datainfo[j].data ) + *( datainfo1[j].data ) + $
                  *( datainfo2[j].data ) + *( datainfo3[j].data ) + $
                  *( datainfo4[j].data ) + *( datainfo5[j].data ) else $
          array = *( datainfo[j].data )
      ; Scale data by fscale
      array = array * fscale
      ; Reform relevant part of profile
      profile = reform( array(ii, jj, 0:iztop))
 
      ; Combine vertical profiles into a cross-section 
      curtain = [curtain, transpose( profile ) ]
      
   endfor  ; End of loop over # of 1-min observations for Day 2 
   
endif
 
ytitle = 'Pressure Altitude (km)'
title = 'GEOS-CHEM '+Species_in +' ' + Unit

; Plot curtain using TVCURTAIN 
window, 2
tvcurtain, curtain, lat, lon, zmid, /ystyle, color = 1, $
           mindata=mmindata, maxdata=mmaxdata, ytitle=ytitle, title=title,$
	   /FlightTrack, FlightZ=altp, /CBar,/CBVertical,/NoAdvance
 
; If the OPlot_data keyword is set, then plot the DC8 data atop the
; curtain.
if Keyword_set(oplot_data) then $
   scatterplot_datacolor,findgen(nPts)+1,altp,species, $
   zmin=mindata,zmax=maxdata,/overplot,/nocb,_extra=_extra
 
end
