; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        SENEX_OA_PROFILES
;
; PURPOSE:
;	 Plot average vertical profiles for observed OA during SENEX
;	 along with GEOS-Chem OA components.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        SENEX_OA_PROFILES [, KEYWORDS]
;
; INPUTS:
;	 None
;
; KEYWORD PARAMETERS:
;        FLIGHTDATES - one or multiple flight dates (default all)
;        MINDATA     - minimum value for x-axis (called with MAXDATA)
;        MAXDATA     - maximum value for x-axis (called with MINDATA)
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
;        senex_oa_profiles,flightdates='2013*'
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
; with subject "IDL routine senex_oa_profiles"
;-----------------------------------------------------------------------

pro senex_oa_profiles,flightdates=flightdates,altrange=altrange,$
	mindata=mindata,maxdata=maxdata,choose_win=choose_win,$
	latrange=latrange,lonrange=lonrange,save=save,_extra=_extra
 
; Set defaults
if N_Elements(flightdates) eq 0 then begin
        flightdates='2013*'
        print, $
	'You didn''t specify flightdates, so all dates are being plotted!'
	flightdates=get_model_data_senex('FLIGHTDATE','WP3D',flightdates)
	flightdates = flightdates(uniq(flightdates))
	flightdates = string(flightdates, '(i8.8)')
endif
if n_elements(altrange) eq 0 then altrange=[0,10]
 
; Initialize arrays
tot_oa=[0]
altp=[0]
lat=[0]
lon=[0]
doy=[0]

doy_mod=[0]
tot_oa_mod=[0]
poa_mod=[0]
soa_mod=[0]
asoa_mod=[0]
bbsoa_mod=[0]
bgsoa_mod=[0]
 
; Read data
for i = 0, n_elements(flightdates)-1 do begin
   tot_oa_tmp = get_field_data_senex('oa','WP3D',flightdates[i], $
                                      _extra = _extra)
 
   ; Make sure there are observations
   if  finite(mean_nan(tot_oa_tmp),/nan) then begin 
	print, '********* no data for: ', flightdates[i]
	goto, nodata
   endif
 
   ; Read data, other relevant variables
   tot_oa = [tot_oa, tot_oa_tmp]
 
   altp = [altp,get_field_data_senex('altp','WP3D',flightdates[i], $
                                    _extra = _extra )]
   lat =  [lat,get_field_data_senex('lat','WP3D',flightdates[i], $
                                    _extra = _extra)]
   lon =  [lon,get_field_data_senex('lon','WP3D',flightdates[i], $
                                    _extra = _extra)]
   doy =  [doy,get_field_data_senex('doy','WP3D',flightdates[i], $
                                   _extra = _extra )]
 
   ; Read relevant model variables
   tot_oa_mod = [tot_oa_mod, $
	get_model_data_senex('oa','WP3D',flightdates[i], $
                             _extra = _extra)]
   doy_mod = [doy_mod, $
	get_model_data_senex('DOY','WP3D',flightdates[i], $
                            _extra = _extra )]
   ; model OA components 
   poa_mod = [poa_mod, $
	get_model_data_senex('poa','WP3D',flightdates[i], $
                             _extra = _extra)]
   soa_mod = [soa_mod, $
	get_model_data_senex('soa','WP3D',flightdates[i], $
                             _extra = _extra)]
   asoa_mod = [asoa_mod, $
	get_model_data_senex('asoa','WP3D',flightdates[i], $
                             _extra = _extra)]
   bbsoa_mod = [bbsoa_mod, $
	get_model_data_senex('bbsoa','WP3D',flightdates[i], $
                             _extra = _extra)]
   bgsoa_mod = [bgsoa_mod, $
	get_model_data_senex('bgsoa','WP3D',flightdates[i], $
                             _extra = _extra)]
 
nodata:
endfor
 
; Remove placeholder
tot_oa  = tot_oa[1:*]
altp = altp[1:*]
lat  = lat[1:*]
lon  = lon[1:*]
doy  = doy[1:*]
tot_oa_mod = tot_oa_mod[1:*]
doy_mod = doy_mod[1:*]
poa_mod = poa_mod[1:*]
soa_mod = soa_mod[1:*]
asoa_mod  = asoa_mod[1:*]
bbsoa_mod = bbsoa_mod[1:*]
bgsoa_mod = bgsoa_mod[1:*]
 
; Interpolate model to observed space
tot_oa_mod = interpol( tot_oa_mod, doy_mod, doy )
poa_mod    = interpol( poa_mod,    doy_mod, doy )
soa_mod    = interpol( soa_mod,    doy_mod, doy )
asoa_mod   = interpol( asoa_mod,   doy_mod, doy )
bbsoa_mod  = interpol( bbsoa_mod,  doy_mod, doy )
bgsoa_mod  = interpol( bgsoa_mod,  doy_mod, doy )
 
; Subselect relevant region, finite data
if ( n_elements(lonrange) gt 0 and n_elements(latrange) gt 0 ) then begin
   index = where( finite(tot_oa)                                 and $
		  (lat ge min(latrange) and lat le max(latrange)) and $
		  (lon ge min(lonrange) and lon le max(lonrange))     )
endif else if ( n_elements(lonrange) gt 0 ) then begin
   index = where( finite(tot_oa)                                 and $
		  (lon ge min(lonrange) and lon le max(lonrange))     )
endif else if ( n_elements(latrange) gt 0 ) then begin
   index = where( finite(tot_oa)                                 and $
		  (lat ge min(latrange) and lat le max(latrange))     )
endif else index = where(finite(tot_oa))
 
tot_oa     = tot_oa[index]
tot_oa_mod = tot_oa_mod[index]
poa_mod    = poa_mod[index]
soa_mod    = soa_mod[index]
asoa_mod   = asoa_mod[index]
bbsoa_mod  = bbsoa_mod[index]
bgsoa_mod  = bgsoa_mod[index]
altp = altp[index]
lat = lat[index]
lon = lon[index]
 
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
 
; Find the median for each altitude bin
tot_oa_median   = tapply( tot_oa,  alt_group, 'median', /NaN )
altp_median  = tapply( altp, alt_group, 'median', /NaN )

; We can use the same group IDs for the model because they are
; already interpolated to the same time and location 
tot_oa_mod_median   = tapply( tot_oa_mod,  alt_group, 'median', /NaN )
poa_mod_median      = tapply( poa_mod,     alt_group, 'median', /NaN )
soa_mod_median      = tapply( soa_mod,     alt_group, 'median', /NaN )
asoa_mod_median     = tapply( asoa_mod,    alt_group, 'median', /NaN )
bbsoa_mod_median    = tapply( bbsoa_mod,   alt_group, 'median', /NaN )
bgsoa_mod_median    = tapply( bgsoa_mod,   alt_group, 'median', /NaN )
 
; Also get interquartile range
tot_oa_25 = tapply( tot_oa, alt_group, 'percentiles', /Nan, value=0.25 )
tot_oa_75 = tapply( tot_oa, alt_group, 'percentiles', /Nan, value=0.75 )
tot_oa_mod_25 = tapply( tot_oa_mod, alt_group, 'percentiles', /Nan, value=0.25 )
tot_oa_mod_75 = tapply( tot_oa_mod, alt_group, 'percentiles', /Nan, value=0.75 )
 
; Set plot strings
title = 'Observed and modeled vertical profiles of OA'
ytitle='Altitude, km'
xtitle='OA, ug/m3'

; Set up plot, full dynamic range
if Keyword_set(save) then begin
   filename=!SENEX+'/IDL/analysis/oa_profiles_full.ps'
   multipanel,rows=2,cols=1
   open_device, /ps, filename=filename, Bits=8, WinParam = [0, 300, 400], $
                /color, /portrait
   !p.font = 0
endif else if n_elements(choose_win) eq 0 then window,0 else window,choose_win

; Plot individual data points
plot, tot_oa,altp,color=1,psym=sym(1),symsize=0.2,yrange=altrange,ystyle=9,$
	xtitle=xtitle,ytitle=ytitle,title=title,xstyle=9,/nodata
;oplot, tot_oa_mod, altp,color=2,psym=sym(1),symsize=0.2

; Plot IQR as error bars
;for i = 0, n_elements(tot_oa_mod_25)-1 do begin
;   oplot,[tot_oa_mod_25[i],tot_oa_mod_75[i]],$
;          [altp_median[i]+.05,altp_median[i]+.05],color=2,linestyle=0,thick=2
;endfor
for i = 0, n_elements(tot_oa_25)-1 do begin
   oplot,[tot_oa_25[i],tot_oa_75[i]],$
          [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor

; Plot median values in each altitude bin
oplot, tot_oa_median,altp_median,color=1,linestyle=0,thick=2
oplot, tot_oa_mod_median,altp_median+.05,color=2,linestyle=0,thick=2
oplot, poa_mod_median,altp_median+.05,color=5,linestyle=0,thick=2
oplot, asoa_mod_median,altp_median+.05,color=3,linestyle=0,thick=2
oplot, bbsoa_mod_median,altp_median+.05,color=4,linestyle=0,thick=2
oplot, bgsoa_mod_median,altp_median+.05,color=6,linestyle=0,thick=2
 
; Legend
legend, lcolor=[1,2,5,3,4,6],line=intarr(6),thick=intarr(6)+2,$
	label=['Obs Total OA','Model Total OA','   POA',$
               '   An. SOA','   BB SOA','   BG SOA'],$
	halign=0.9, valign=0.9, charsize=1.2, /color
 
multipanel,/off
if Keyword_Set(save) then close_device
 
; Set up plot, limited range
if ( n_elements(mindata) gt 0 and n_elements(maxdata) gt 0 ) then begin
  if Keyword_set(save) then begin
     filename=!SENEX+'/IDL/analysis/oa_profiles.ps'
     multipanel,rows=2,cols=1
     open_device, /ps, filename=filename, Bits=8, WinParam = [0, 300, 400], $
                  /color, /portrait
     !p.font = 0
endif else if n_elements(choose_win) eq 0 then window,1 else window,choose_win

; Plot individual data points
plot, tot_oa,altp,color=1,psym=sym(1),symsize=0.2,yrange=altrange,ystyle=9,$
	xrange=[mindata, maxdata], xstyle=9, xtitle=xtitle,$
	ytitle=ytitle,title=title,/nodata
;oplot, tot_oa_mod, altp,color=2,psym=sym(1),symsize=0.2

; Plot individual data points
;for i = 0, n_elements(tot_oa_mod_25)-1 do begin
;   oplot,[tot_oa_mod_25[i],tot_oa_mod_75[i]],$
;          [altp_median[i]+.05,altp_median[i]+.05],color=2,linestyle=0,thick=2
;endfor
for i = 0, n_elements(tot_oa_25)-1 do begin
   oplot,[tot_oa_25[i],tot_oa_75[i]],$
          [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor

; Plot median values in each altitude bin
oplot, tot_oa_median,altp_median,color=1,linestyle=0,thick=2
oplot, tot_oa_mod_median,altp_median+.05,color=2,linestyle=0,thick=2
oplot, poa_mod_median,altp_median+.05,color=5,linestyle=0,thick=2
oplot, asoa_mod_median,altp_median+.05,color=3,linestyle=0,thick=2
oplot, bbsoa_mod_median,altp_median+.05,color=4,linestyle=0,thick=2
oplot, bgsoa_mod_median,altp_median+.05,color=6,linestyle=0,thick=2
 
; Legend
legend, lcolor=[1,2,5,3,4,6],line=intarr(6),thick=intarr(6)+2,$
	label=['Obs Total OA','Model Total OA','   POA',$
               '   An. SOA','   BB SOA','   BG SOA'],$
	halign=0.9, valign=0.9, charsize=1.2, /color
 
multipanel,/off
if Keyword_Set(save) then close_device
 
endif else if ( n_elements(mindata) gt 0 or n_elements(maxdata) gt 0  ) then begin
   print, ''
   print,'Specify MINDATA and MAXDATA simultaneously to plot reduced range'
   print, ''
endif
 
end
