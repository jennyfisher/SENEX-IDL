pro arctas_obs_profile,species_in,platform,flightdates=flightdates,$
	mindata=mindata,maxdata=maxdata,unit=unit,choose_win=choose_win,$
	save=save,_extra=_extra

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
doy=[0]

for i = 0, n_elements(flightdates)-1 do begin
   species_tmp = get_field_data_arctas(species_in,platform,flightdates[i], $
                                      _extra = _extra)

   if  finite(mean_nan(species_tmp),/nan) then begin 
	print, '********* no data for: ', flightdates[i]
	goto, nodata
   endif

   species = [species, species_tmp]

   altp = [altp,get_field_data_arctas('alt',platform,flightdates[i], $
                                    _extra = _extra )]
   lat =  [lat,get_field_data_arctas('lat',platform,flightdates[i], $
                                    _extra = _extra)]
   doy =  [doy,get_field_data_arctas('doy','dc8',flightdates[i], $
                                   _extra = _extra )]

nodata:
endfor

species  = species[1:*]
altp = altp[1:*]
lat  = lat[1:*]
doy  = doy[1:*]

index = where( lat gt 50 )
species = species(index)
altp = altp(index)

; Find the nearest integer altitude (km)
; Restrict the range 0 < alt < 12km
alt_group     = ( ( floor(altp) < 12 ) > 0 )

; Find the median of each species for each altitude bin
species_median   = tapply( species,  alt_group, 'median', /NaN )
altp_median  = tapply( altp, alt_group, 'median', /NaN )

; Also get standard deviation of observations for plotting
i = where( finite ( species ) )
species_25 = tapply( species[i], alt_group[i], 'percentiles', /Nan, value=0.25 )
species_75 = tapply( species[i], alt_group[i], 'percentiles', /Nan, value=0.75 )

title = 'Observed vertical profile of '+species_in
xtitle=strupcase(species_in)+', '+unit
ytitle='Altitude, km'

if n_elements(unit) eq 0 then unit = ''
multipanel,/off
if ~Keyword_set(choose_win) then window,0
plot, species,altp,color=1,psym=sym(1),symsize=0.2,yrange=[0,10],ystyle=9,$
	xtitle=xtitle,ytitle=ytitle,title=title,xstyle=9
;oplot, species_median,altp_median,color=1,psym=sym(5),symsize=1.5
oplot, species_median,altp_median,color=1,linestyle=0,thick=2
; IQR
for i = 0, n_elements(species_25)-1 do begin
   oplot,[species_25[i],species_75[i]],$
          [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor

if ( n_elements(mindata) gt 0 and n_elements(maxdata) gt 0 ) then begin
  if Keyword_set(save) then begin
     filename=!ARCTAS+'/IDL/analysis/'+$
	platform+'_'+species_in+'_profiles_'+flightdates+'.ps'
     multipanel,rows=2,cols=1
     open_device, /ps, filename=filename, Bits=8, WinParam = [0, 300, 400], $
                  /color, /portrait
     !p.font = 0
   endif else if ~keyword_set(choose_win) then window,1
plot, species,altp,color=1,psym=sym(1),symsize=0.2,yrange=[0,10],ystyle=9,$
	xrange=[mindata, maxdata], xstyle=9, xtitle=xtitle,$
	ytitle=ytitle,title=title
;oplot, species_median,altp_median,color=1,psym=sym(5),symsize=1.5
oplot, species_median,altp_median,color=1,linestyle=0,thick=2
; IQR
for i = 0, n_elements(species_25)-1 do begin
   oplot,[species_25[i],species_75[i]],$
          [altp_median[i],altp_median[i]],color=1,linestyle=0,thick=2
endfor

if Keyword_Set(save) then close_device

endif

end
