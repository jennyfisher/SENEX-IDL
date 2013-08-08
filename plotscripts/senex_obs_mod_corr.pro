pro senex_obs_mod_corr,species_in,platform,flightdates=flightdates,     $
	z1=z1,z2=z2,minz1=minz1,maxz1=maxz1,minz2=minz2,maxz2=maxz2,$
	mindata=mindata,maxdata=maxdata,unit=unit,_extra=_extra

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
if n_elements(z1) eq 0 then begin
	z1 = 'altp'
	print, 'You didn''t specify variable for color dimension (z1)!'
	print, '   --> Using altitude'
endif

; Check for a second color dimension
if n_elements(z2) gt 0 then plot_z2=1 else plot_z2=0

; Initialize arrays
species=[0]
z1_data=[0]
z2_data=[0]
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

   z1_data = [z1_data,get_field_data_senex(z1,platform,flightdates[i], $
                                    _extra = _extra )]
   if (keyword_set(plot_z2)) then $
   z2_data = [z2_data,get_field_data_senex(z2,platform,flightdates[i], $
                                    _extra = _extra )]
   lat =  [lat,get_field_data_senex('lat',platform,flightdates[i], $
                                    _extra = _extra)]
   lon =  [lon,get_field_data_senex('lon',platform,flightdates[i], $
                                    _extra = _extra)]
   doy =  [doy,get_field_data_senex('doy',platform,flightdates[i], $
                                   _extra = _extra )]

   ; Read relevant model variables
   species_mod = [species_mod, $
        get_model_data_senex(species_in,platform,flightdates[i], $
                             _extra = _extra)]
   doy_mod = [doy_mod, $
        get_model_data_senex('DOY',platform,flightdates[i], $
                            _extra = _extra )]

nodata:
endfor

; Remove placeholder
species  = species[1:*]
z1_data = z1_data[1:*]
lat  = lat[1:*]
lon  = lon[1:*]
doy  = doy[1:*]
species_mod = species_mod[1:*]
doy_mod = doy_mod[1:*]
if (keyword_set(plot_z2)) then z2_data=z2_data[1:*]

; Interpolate model to observed space
species_mod = interpol( species_mod, doy_mod, doy )

; Set plot strings
title = 'Correlation of Observed and modeled '+strupcase(species_in)
if n_elements(unit) eq 0 then unit = ''
xtitle='Obs '+strupcase(species_in)+', '+unit
ytitle='Model '+strupcase(species_in)+', '+unit
z1title=strupcase(z1)

; Set color ranges
if (n_elements(mindata) eq 0) then mindata=min([species,species_mod],/nan)
if (n_elements(maxdata) eq 0) then maxdata=max([species,species_mod],/nan)
if (n_elements(minz1) eq 0) then minz1=min(z1_data,/nan)
if (n_elements(maxz1) eq 0) then maxz1=max(z1_data,/nan)

; Plot correlations
myct, 33, ncolors=30

window,0

scatterplot_datacolor, species, species_mod, z1_data,   $
    /xstyle, /ystyle, xtitle=xtitle,ytitle=ytitle,      $
    xrange=[mindata,maxdata], yrange=[mindata,maxdata], $
    unit=z1title, zmin=minz1, zmax=maxz1, title=title,  $
    div=3,cbposition=[.6,.9,.8,.93]
oplot,[mindata,maxdata],[mindata,maxdata],color=1,linestyle=0

if (keyword_set(plot_z2)) then begin
  window, 1
  z2title=strupcase(z2)
  if (n_elements(minz2) eq 0) then minz2=min(z2_data,/nan)
  if (n_elements(maxz2) eq 0) then maxz2=max(z2_data,/nan)

   scatterplot_datacolor, species, species_mod, z2_data,   $
       /xstyle, /ystyle, xtitle=xtitle,ytitle=ytitle,      $
       xrange=[mindata,maxdata], yrange=[mindata,maxdata], $
       unit=z2title, zmin=minz2, zmax=maxz2, title=title,  $
       div=3,cbposition=[.6,.9,.8,.93]
   oplot,[mindata,maxdata],[mindata,maxdata],color=1,linestyle=0
endif

end
