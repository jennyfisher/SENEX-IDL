pro arctas_diffmap,species_in,platform,flightdates=flightdates,pd=pd,$
	mindata=mindata,maxdata=maxdata,unit=unit,save=save,alt=alt, $
        _extra=_extra

if n_elements(species_in) eq 0 then species_in='co'
if n_elements(platform) eq 0 then platform = 'DC8'

if N_Elements(flightdates) eq 0 then begin
        flightdates='2008*'
        print, $
	'You didn''t specify flightdates, so all dates are being plotted!'
	flightdates=get_model_data_arctas('FLIGHTDATE',platform,flightdates)
	flightdates = flightdates(uniq(flightdates))
	flightdates = string(flightdates, '(i8.8)')
endif

species=[0]
altp=[0]
lat=[0]
lon=[0]
doy=[0]
species_mod=[0]
doy_mod=[0]

for i = 0, n_elements(flightdates)-1 do begin
   species_tmp = get_field_data_arctas(species_in,platform,flightdates[i], $
                                      _extra = _extra)

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

   species_mod = [species_mod, $
	get_model_data_arctas(species_in,'DC8',flightdates[i], $
                             _extra = _extra)]
   doy_mod = [doy_mod, $
	get_model_data_arctas('DOY','DC8',flightdates[i], $
                            _extra = _extra )]

nodata:
endfor

species  = species[1:*]
altp = altp[1:*]
lat  = lat[1:*]
lon  = lon[1:*]
doy  = doy[1:*]
species_mod = species_mod[1:*]
doy_mod = doy_mod[1:*]

species_mod = interpol( species_mod, doy_mod, doy )

if n_elements(alt) gt 0 then $
   index = where( (lat gt 50) and finite(species) and $
                  (altp ge alt[0]) and (altp le alt[1]) ) $
else index = where( lat gt 50 and finite(species) )
species = species(index)
species_mod = species_mod(index)
lat = lat(index)
lon = lon(index)
if keyword_set(pd) then diff = 100*(species_mod - species)/species $
else diff = species_mod-species

title='Modeled '+Species_in+' - Observed '+Species_in+' on '+Platform+' track'
latmin = 55
if n_elements(mindata) eq 0 then mindata=-75
if n_elements(maxdata) eq 0 then maxdata=75

if keyword_set(save) then begin
	filename = !ARCTAS+'/IDL/analysis/'+$
		'obs_mod_'+species+'_diff_'+flightdates+'.ps'
	!p.font = 0
	open_device, /ps, /color, bits=8, filename=filename, $
		WinParam = [0,300,400], /portrait
endif else window, 0

; Plot the data on a polar orthographic projection of the N. America sector
arctas_map, lon, lat, diff, latmin=latmin, zmin=mindata, zmax=maxdata, $
            title=title, unit='%',/diff,_extra=_extra

if keyword_set(save) then close_device

end
