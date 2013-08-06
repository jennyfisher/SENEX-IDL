; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        ARCTAS_3_PROFILES
;
; PURPOSE:
;        Plot observations + 2 different model simulations as median 
;	 vertical profiles
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        ARCTAS_3_PROFILES, Species_In, Platform[, Keywords]
;
; INPUTS:
;	 Species_In - species to be plotted
;	 Platform - ARCTAS platform; can be DC8 or P3B
;
; KEYWORD PARAMETERS:
;	 Flightdates - ARCTAS flight dates to plot
;	 Mindata/Maxdata - Set x-axis values
;	 Unit - Set x-axis unit label
;	 Choose_win - Use this keyword to prevent the routine from
;	              automatically plotting in windows 1 and 2
;	 Maindir - Main GEOS-Chem directory to use
;	 Altdir - Second GEOS-Chem directory to use
;	 Save - Use this keyword to save the plot to a postscript
;	 Legend - Use this keyword to plot a legend
;
; OUTPUTS:
;	 None
;
; SUBROUTINES:
;	 None
;
; REQUIREMENTS:
;	 None
;
; NOTES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        jaf, 12 Jan 2011: VERSION 1.00
;
;-
; Copyright (C) 2011, Jenny Fisher, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to jafisher@fas.harvard.edu
; or with subject "IDL routine arctas_3_profiles"
;-----------------------------------------------------------------------

pro ARCTAS_3_Profiles,Species_In,Platform,Flightdates=Flightdates,$
	Mindata=Mindata,Maxdata=Maxdata,Unit=Unit,Choose_Win=Choose_Win,$
	MainDir=MainDir, AltDir=AltDir,Save=Save,Legend=Legend,_extra=_extra
 
; Set defaults
if n_elements(species_in) eq 0 then species_in='CO'
if n_elements(platform) eq 0 then platform = 'DC8'
if N_Elements(flightdates) eq 0 then begin
        flightdates='2008*'
        print, $
	'You didn''t specify flightdates, so all dates are being plotted!'
	flightdates=get_model_data_arctas('FLIGHTDATE',platform,flightdates)
	flightdates = flightdates(uniq(flightdates))
	flightdates = string(flightdates, '(i8.8)')
endif
 
if n_elements(maindir) eq 0 then maindir='totHg'
if n_elements(altdir) eq 0 then begin
   print,'To print 2 model profiles, specify maindir and altdir'
   print,'AltDir not specified, so only 1 model profile is being plotted'
   altdir=maindir
endif
 
; Set up arrays
species=[0]
altp=[0]
lat=[0]
lon=[0]
doy=[0]
species_mod=[0]
species_mod2=[0]
doy_mod=[0]
 
; Loop over flightdates to read data, ignoring days with no data
for i = 0, n_elements(flightdates)-1 do begin
   species_tmp = get_field_data_arctas(species_in,platform,flightdates[i], $
                                      _extra = _extra)
 
   ; Print message and skip if no data for this species/date
   if  finite(mean_nan(species_tmp),/nan) then begin 
	print, '********* no data for: ', flightdates[i]
	goto, nodata
   endif
 
   species = [species, species_tmp]
 
   altp = [altp,get_field_data_arctas('altp',platform,flightdates[i], $
                                    _extra = _extra )]
   lat =  [lat,get_field_data_arctas('lat',platform,flightdates[i], $
                                    _extra = _extra)]
   lon =  [lon,get_field_data_arctas('lon',platform,flightdates[i], $
                                    _extra = _extra)]
   doy =  [doy,get_field_data_arctas('doy','dc8',flightdates[i], $
                                   _extra = _extra )]
 
   ; Read model values
   species_mod = [species_mod, $
	get_model_data_arctas(species_in,'DC8',flightdates[i], $
                             AltDir=maindir,_extra = _extra)]
   species_mod2 = [species_mod2, $
	get_model_data_arctas(species_in,'DC8',flightdates[i], $
                             AltDir=altdir,_extra = _extra)]
   doy_mod = [doy_mod, $
	get_model_data_arctas('DOY','DC8',flightdates[i], $
                            AltDir=maindir,_extra = _extra )]
 
nodata:
endfor
 
; Resize arrays
species  = species[1:*]
altp = altp[1:*]
lat  = lat[1:*]
lon  = lon[1:*]
doy  = doy[1:*]
species_mod = species_mod[1:*]
species_mod2 = species_mod2[1:*]
doy_mod = doy_mod[1:*]
 
; Interpolate model values to match observed data points
species_mod = interpol( species_mod, doy_mod, doy )
species_mod2 = interpol( species_mod2, doy_mod, doy )
 
; Only use "Arctic" data (lat > 50)
index = where( lat gt 50 and finite(species) )
species = species[index]
species_mod = species_mod[index]
species_mod2 = species_mod2[index]
altp = altp[index]
lat = lat[index]
lon = lon[index]

; From CDH:
; Read ground elevation
restore, !HOME +'/data/smith_sandwell_topo_v8_2.sav', /verbose
jj = value_locate( topo.lat, lat )
ii = value_locate( topo.lon, lon )
altp = altp - (topo.alt[ii,jj] > 0)/1000.
 
; Find the nearest integer altitude (km) for each point
; Restrict the range 0 < alt < 12km
alt_group     = ( ( floor(altp) < 12 ) > 0 )
 
; Find the median of each species for each altitude bin
species_median   = tapply( species,  alt_group, 'median', /NaN )
altp_median  = tapply( altp, alt_group, 'median', /NaN )
 
; We can use the same group IDs for the model because they are
; already interpolated to the same time and location 
species_mod_median   = tapply( species_mod,  alt_group, 'median', /NaN )
species_mod2_median   = tapply( species_mod2,  alt_group, 'median', /NaN )
 
; Also get IQR of observations for plotting
species_25 = tapply( species, alt_group, 'percentiles', /Nan, value=0.25 )
species_75 = tapply( species, alt_group, 'percentiles', /Nan, value=0.75 )
 
; Set titles
if n_elements(unit) eq 0 then unit = '' else unit=', '+unit
title = 'Observed and modeled vertical profiles of '+species_in
xtitle=strupcase(species_in)+unit
ytitle='Altitude, km'
 
; First, plot on entire range for 'big picture'
if ~keyword_set(choose_win) then window,0
 
; Individual data points
plot, species,altp,color=1,psym=sym(1),symsize=0.2,yrange=[0,10.5],ystyle=9,$
	xtitle=xtitle,ytitle=ytitle,title=title,xstyle=9
oplot, species_mod, altp,color=2,psym=sym(1),symsize=0.2
oplot, species_mod2, altp,color=3,psym=sym(1),symsize=0.2
 
; Median lines
oplot, species_median,altp_median,color=1,linestyle=0,thick=2
oplot, species_mod_median,altp_median,color=2,linestyle=0,thick=2
oplot, species_mod2_median,altp_median,color=3,linestyle=0,thick=2
 
; IQR
for i = 0, n_elements(species_25)-1 do begin
   oplot,[species_25[i],species_75[i]],$
	  [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor
 
; Plot Legend
if keyword_set(legend) then $
legend, lcolor=[1,2,3],line=[0,0,0],lthick=[2,2,2],$
	label=[strupcase(Platform)+' Obs',MainDir,AltDir],$
	halign=0.9, valign=0.9, charsize=1.2, /color
 
; Now plot on specified scale
if ( n_elements(mindata) gt 0 and n_elements(maxdata) gt 0 ) then begin
  if Keyword_set(save) then begin
     filename=!ARCTAS+'/IDL/analysis/'+platform+'_3'+$
	species_in+'_profiles_'+flightdates+'.ps'
     multipanel,rows=2,cols=1
     open_device, /ps, filename=filename, Bits=8, WinParam = [0, 300, 400], $
                  /color, /portrait
     !p.font = 0 
   endif else if ~keyword_set(choose_win) then window,1
 
; Individual points
plot, species,altp,color=1,psym=sym(1),symsize=0.2,yrange=[0,10.5],ystyle=9,$
	xrange=[mindata, maxdata], xstyle=9, xtitle=xtitle,$
	ytitle=ytitle,title=title
oplot, species_mod, altp,color=2,psym=sym(1),symsize=0.2
oplot, species_mod2, altp,color=3,psym=sym(1),symsize=0.2
 
; Median lines
oplot, species_median,altp_median,color=1,linestyle=0,thick=2
oplot, species_mod_median,altp_median,color=2,linestyle=0,thick=2
oplot, species_mod2_median,altp_median,color=3,linestyle=0,thick=2
 
; IQR
for i = 0, n_elements(species_25)-1 do begin
   oplot,[species_25[i],species_75[i]],$
	  [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor
 
if keyword_set(legend) then $
legend, lcolor=[1,2,3],line=[0,0,0],lthick=[2,2,2],$
	label=[strupcase(Platform)+' Obs',MainDir,AltDir],$
	halign=0.9, valign=0.9, charsize=1.2, /color

if Keyword_Set(save) then close_device
endif
 
end
