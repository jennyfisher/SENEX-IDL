; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        SENEX_OBS_MOD_PROFILES
;
; PURPOSE:
;	 Plot average vertical profiles for observations during SENEX
;	 along with GEOS-Chem averaged along SENEX flight tracks
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        SENEX_OBS_MOD_PROFILES, SPECIES, PLATFORM[, KEYWORDS]
;
; INPUTS:
;	 SPECIES     - species to plot (default 'CO')
;	 PLATFORM    - aircraft platform (default 'WP3D')
;
; KEYWORD PARAMETERS:
;        FLIGHTDATES - one or multiple flight dates (default all)
;        MINDATA     - minimum value for x-axis (called with MAXDATA)
;        MAXDATA     - maximum value for x-axis (called with MINDATA)
;        UNIT        - unit for x-axis label
;        CHOOSE_WIN  - used to plot in an IDL window besides 0
;	 LATRANGE    - specifies min,max of latitude values to include
;	 LONRANGE    - specifies min,max of lonitude values to include
;        SAVE        - set this keyword to save figure
;
; OUTPUTS:
;	 NONE
;
; SUBROUTINES:
;	 Various SENEX, GAMAP scripts
;
; REQUIREMENTS:
;	 Various SENEX, GAMAP scripts
;
; NOTES:
;
; EXAMPLE:
;        senex_obs_mod_profiles,'CO','WP3D',flightdates='2013*'
;
; MODIFICATION HISTORY:
;        jaf, 09 Aug 2013: VERSION 1.00
;
;-
; Copyright (C) 2013, Jenny Fisher, University of Wollongong
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to jennyf@uow.edu.au
; with subject "IDL routine senex_obs_mod_profiles"
;-----------------------------------------------------------------------

pro senex_obs_mod_profiles,species_in,platform,flightdates=flightdates,$
	mindata=mindata,maxdata=maxdata,unit=unit,choose_win=choose_win,$
	latrange=latrange,lonrange=lonrange,save=save,_extra=_extra
 
; Set defaults
if n_elements(species_in) eq 0 then species_in='CO'
if n_elements(platform) eq 0 then platform = 'WP3D'
if N_Elements(flightdates) eq 0 then begin
        flightdates='2013*'
        print, $
	'You didn''t specify flightdates, so all dates are being plotted!'
	flightdates=get_model_data_senex('FLIGHTDATE',platform,flightdates)
	flightdates = flightdates(uniq(flightdates))
	flightdates = string(flightdates, '(i8.8)')
endif
 
; Initialize arrays
species=[0]
altp=[0]
lat=[0]
lon=[0]
doy=[0]
species_mod=[0]
doy_mod=[0]
 
; Read data
for i = 0, n_elements(flightdates)-1 do begin
   species_tmp = get_field_data_senex(species_in,platform,flightdates[i], $
                                      _extra = _extra)
 
   ; Make sure there are observations for given species / flight
   if  finite(mean_nan(species_tmp),/nan) then begin 
	print, '********* no data for: ', flightdates[i]
	goto, nodata
   endif
 
   ; Read data, other relevant variables
   species = [species, species_tmp]
 
   altp = [altp,get_field_data_senex('altp',platform,flightdates[i], $
                                    _extra = _extra )]
   lat =  [lat,get_field_data_senex('lat',platform,flightdates[i], $
                                    _extra = _extra)]
   lon =  [lon,get_field_data_senex('lon',platform,flightdates[i], $
                                    _extra = _extra)]
   doy =  [doy,get_field_data_senex('doy','WP3D',flightdates[i], $
                                   _extra = _extra )]
 
   ; Read relevant model variables
   species_mod = [species_mod, $
	get_model_data_senex(species_in,'WP3D',flightdates[i], $
                             _extra = _extra)]
   doy_mod = [doy_mod, $
	get_model_data_senex('DOY','WP3D',flightdates[i], $
                            _extra = _extra )]
 
nodata:
endfor
 
; Remove placeholder
species  = species[1:*]
altp = altp[1:*]
lat  = lat[1:*]
lon  = lon[1:*]
doy  = doy[1:*]
species_mod = species_mod[1:*]
doy_mod = doy_mod[1:*]
 
; Interpolate model to observed space
species_mod = interpol( species_mod, doy_mod, doy )
 
; Subselect relevant region, finite data
if ( n_elements(lonrange) gt 0 and n_elements(latrange) gt 0 ) then begin
   index = where( finite(species)                                 and $
		  (lat ge min(latrange) and lat le max(latrange)) and $
		  (lon ge min(lonrange) and lon le max(lonrange))     )
endif else if ( n_elements(lonrange) gt 0 ) then begin
   index = where( finite(species)                                 and $
		  (lon ge min(lonrange) and lon le max(lonrange))     )
endif else if ( n_elements(latrange) gt 0 ) then begin
   index = where( finite(species)                                 and $
		  (lat ge min(latrange) and lat le max(latrange))     )
endif else index = where(finite(species))
 
species = species[index]
species_mod = species_mod[index]
altp = altp[index]
lat = lat[index]
lon = lon[index]
 
; Convert altitude from m to km
altp = altp/1d3
 
; From CDH:
; Add this later? (jaf, 8/8/13)
;; Read ground elevation
;restore, !HOME +'/data/smith_sandwell_topo_v8_2.sav', /verbose
;jj = value_locate( topo.lat, lat )
;ii = value_locate( topo.lon, lon )
;altp = altp - (topo.alt[ii,jj] > 0)/1000.
 
; Find the nearest integer altitude (km)
; Restrict the range 0 < alt < 12km
alt_group     = ( ( floor(altp) < 12 ) > 0 )
 
; Find the median of each species for each altitude bin
species_median   = tapply( species,  alt_group, 'median', /NaN )
altp_median  = tapply( altp, alt_group, 'median', /NaN )

; We can use the same group IDs for the model because they are
; already interpolated to the same time and location 
species_mod_median   = tapply( species_mod,  alt_group, 'median', /NaN )
 
; Also get interquartile range
species_25 = tapply( species, alt_group, 'percentiles', /Nan, value=0.25 )
species_75 = tapply( species, alt_group, 'percentiles', /Nan, value=0.75 )
species_mod_25 = tapply( species_mod, alt_group, 'percentiles', /Nan, value=0.25 )
species_mod_75 = tapply( species_mod, alt_group, 'percentiles', /Nan, value=0.75 )
 
; Set plot strings
title = 'Observed and modeled vertical profiles of '+species_in
ytitle='Altitude, km'
if n_elements(unit) eq 0 then unit = ''
xtitle=strupcase(species_in)+', '+unit

; Set up plot, full dynamic range
if Keyword_set(save) then begin
   filename=!SENEX+'/IDL/analysis/'+platform+'_'+species_in+'_profiles_full.ps'
   multipanel,rows=2,cols=1
   open_device, /ps, filename=filename, Bits=8, WinParam = [0, 300, 400], $
                /color, /portrait
   !p.font = 0
endif else if n_elements(choose_win) eq 0 then window,0 else window,choose_win

; Plot individual data points
plot, species,altp,color=1,psym=sym(1),symsize=0.2,yrange=[0,10],ystyle=9,$
	xtitle=xtitle,ytitle=ytitle,title=title,xstyle=9
oplot, species_mod, altp,color=2,psym=sym(1),symsize=0.2

; Plot IQR as error bars
for i = 0, n_elements(species_mod_25)-1 do begin
   oplot,[species_mod_25[i],species_mod_75[i]],$
          [altp_median[i]+.05,altp_median[i]+.05],color=2,linestyle=0,thick=2
endfor
for i = 0, n_elements(species_25)-1 do begin
   oplot,[species_25[i],species_75[i]],$
          [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor

; Plot median values in each altitude bin
oplot, species_median,altp_median,color=1,linestyle=0,thick=2
oplot, species_mod_median,altp_median+.05,color=2,linestyle=0,thick=2
 
; Legend
legend, lcolor=[1,2],line=[0,0],lthick=[2,2],$
	label=[strupcase(Platform)+' Obs','Model'],$
	halign=0.9, valign=0.9, charsize=1.2, /color
 
multipanel,/off
if Keyword_Set(save) then close_device
 
; Set up plot, limited range
if ( n_elements(mindata) gt 0 and n_elements(maxdata) gt 0 ) then begin
  if Keyword_set(save) then begin
     filename=!SENEX+'/IDL/analysis/'+platform+'_'+species_in+'_profiles.ps'
     multipanel,rows=2,cols=1
     open_device, /ps, filename=filename, Bits=8, WinParam = [0, 300, 400], $
                  /color, /portrait
     !p.font = 0
endif else if n_elements(choose_win) eq 0 then window,1 else window,choose_win

; Plot individual data points
plot, species,altp,color=1,psym=sym(1),symsize=0.2,yrange=[0,10],ystyle=9,$
	xrange=[mindata, maxdata], xstyle=9, xtitle=xtitle,$
	ytitle=ytitle,title=title
oplot, species_mod, altp,color=2,psym=sym(1),symsize=0.2

; Plot individual data points
for i = 0, n_elements(species_mod_25)-1 do begin
   oplot,[species_mod_25[i],species_mod_75[i]],$
          [altp_median[i]+.05,altp_median[i]+.05],color=2,linestyle=0,thick=2
endfor
for i = 0, n_elements(species_25)-1 do begin
   oplot,[species_25[i],species_75[i]],$
          [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor

; Plot median values in each altitude bin
oplot, species_median,altp_median,color=1,linestyle=0,thick=2
oplot, species_mod_median,altp_median+.05,color=2,linestyle=0,thick=2
 
; Legend
legend, lcolor=[1,2],line=[0,0],lthick=[2,2],$
	label=[strupcase(Platform)+' Obs','Model'],$
	halign=0.9, valign=0.9, charsize=1.2, /color
 
multipanel,/off
if Keyword_Set(save) then close_device
 
endif else if ( n_elements(mindata) gt 0 or n_elements(maxdata) gt 0  ) then begin
   print, ''
   print,'Specify MINDATA and MAXDATA simultaneously to plot reduced range'
   print, ''
endif
 
end
