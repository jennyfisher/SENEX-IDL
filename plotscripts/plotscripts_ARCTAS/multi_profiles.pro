pro multi_profiles,species_in,platform,flightdates=flightdates,$
	mindata=mindata,maxdata=maxdata,unit=unit,choose_win=choose_win,$
	dirs=dirs, lonrange=lonrange, save=save,filename=filename,_extra=_extra

if n_elements(species_in) eq 0 then species_in='co'
if n_elements(platform) eq 0 then platform = 'DC8'
if n_elements(lonrange) eq 0 then lonrange = [0, 360]

if N_Elements(flightdates) eq 0 then begin
        flightdates='2008*'
	flightdates=get_model_data_arctas('FLIGHTDATE',platform,flightdates)
	flightdates = flightdates(uniq(flightdates))
	flightdates = string(flightdates, '(i8.8)')
endif

ndirs = n_elements(dirs)
if ndirs eq 0 then begin
   print,'Must specify model directories!'
   return
endif

for d = 0, ndirs-1 do begin
 s = 'mod'+string(d,'(i1.1)')+' = [0]'
 status = execute( s )
 s = 'doy_mod'+string(d,'(i1.1)')+' = [0]'
 status = execute( s )
endfor

obs=[0]
alt=[0]
lat=[0]
lon=[0]
doy=[0]

for i = 0, n_elements(flightdates)-1 do begin
   obs_tmp = get_field_data_arctas(species_in,platform,flightdates[i], $
                                      _extra = _extra)

   if  finite(mean_nan(obs_tmp),/nan) then begin 
	print, '********* no data for: ', flightdates[i]
	goto, nodata
   endif

   obs = [obs, obs_tmp]

   alt = [alt,get_field_data_arctas('alt',platform,flightdates[i], $
                                    _extra = _extra )]
   lat =  [lat,get_field_data_arctas('lat',platform,flightdates[i], $
                                    _extra = _extra)]
   lon =  [lon,get_field_data_arctas('lon',platform,flightdates[i], $
                                    _extra = _extra)]
   doy =  [doy,get_field_data_arctas('doy',platform,flightdates[i], $
                                   _extra = _extra )]

   for d = 0, ndirs-1 do begin

       s = 'mod'+string(d,'(i1.1)')+' = [ mod'+string(d,'(i1.1)')      + $
           ', get_model_data_arctas(species_in,platform,flightdates[i]'+ $
           ',altdir=dirs[d],_extra=_extra)]'

       status = execute(s)

       doyvar='doy'
       s = 'doy_mod'+string(d,'(i1.1)')+' = [ doy_mod'+string(d,'(i1.1)') + $
           ', get_model_data_arctas(doyvar,platform,flightdates[i]'+ $
           ',altdir=dirs[d],_extra=_extra)]'

       status = execute(s)

   endfor

nodata:
endfor

obs  = obs[1:*]
alt = alt[1:*]
lat  = lat[1:*]
lon  = lon[1:*]
doy  = doy[1:*]

index = where( (lat gt 60) and finite(obs) and $
               (lon ge min(lonrange)) and (lon le max(lonrange)) )
obs = obs[index]
alt = alt[index]

; Find the nearest integer altitude (km)
; Restrict the range 0 < alt < 12km
alt_group     = floor ( floor(alt) / 2 )
;alt_group     = ( ( floor(alt) < 12 ) > 0 )

; Find the median of each species for each altitude bin
obs_median   = tapply( obs,  alt_group, 'median', /NaN )
alt_median  = tapply( alt, alt_group, 'median', /NaN )

; Also get IQR of observations for plotting
obs_25 = tapply( obs, alt_group, 'percentiles', /Nan, value=0.25 )
obs_75 = tapply( obs, alt_group, 'percentiles', /Nan, value=0.75 )

func='median'

; Same procedure for model profiles
for d = 0, ndirs-1 do begin
    s = 'mod'+string(d, '(i1.1)') + ' = mod'+string(d,'(i1.1)') + '[1:*]'
    status = execute( s )

    s = 'doy_mod'+string(d,'(i1.1)') + ' = doy_mod'+string(d,'(i1.1)') + '[1:*]'
    status = execute( s )

    s = 'mod'+string(d, '(i1.1)') + ' = interpol( mod'+string(d,'(i1.1)') + $
        ', doy_mod'+string(d,'(i1.1)')+', doy )'
    status = execute( s )

    s = 'mod'+string(d, '(i1.1)') + ' = mod'+string(d,'(i1.1)') + '[index]'
    status = execute( s )

    s = 'mod'+string(d, '(i1.1)') + '_median = tapply( mod'+string(d,'(i1.1)')+$
        ', alt_group, func, /NaN )' 
    status = execute( s )

endfor

title = 'Observed and modeled vertical profiles of '+species_in
if n_elements(unit) eq 0 then unit = ''
xtitle=strupcase(species_in)+', '+unit
ytitle='Altitude, km'

; First, plot on entire range for 'big picture'
if ~keyword_set(choose_win) then window,0

; Observations
plot, obs,alt,color=1,psym=sym(1),symsize=0.2,yrange=[0,10.5],ystyle=9,$
	xtitle=xtitle,ytitle=ytitle,title=title,xstyle=9,/nodata
oplot, obs_median,alt_median,color=1,linestyle=0,thick=2

; Model
for d = 0, ndirs-1 do begin

   s = 'oplot, mod' + string(d, '(i1.1)') + '_median, alt_median,color=' + $
       string(d+2,'(i1.1)') + ',linestyle=0,thick=2'
   status = execute( s )

endfor

; IQR
for i = 0, n_elements(obs_25)-1 do begin
   oplot,[obs_25[i],obs_75[i]],$
	  [alt_median[i],alt_median[i]],color=1,linestyle=0,thick=2
endfor

; Legend
colors=indgen(ndirs+1)+1
lines=intarr(ndirs+1)
thicks=intarr(ndirs+1)+2
labels=[strupcase(platform)+' Obs',dirs]
legend, lcolor=colors,line=lines,lthick=thicks,label=labels, $
	halign=0.9, valign=0.9, charsize=1.2, /color

; Now plot on specified scale
if ( n_elements(mindata) gt 0 and n_elements(maxdata) gt 0 ) then begin
  if Keyword_set(save) then begin
     if n_elements(filename) eq 0 then $
     filename=!ARCTAS+'/IDL/analysis/'+platform+string(ndirs+1,'(i1.1)')+$
              species_in+'_profiles_.ps' $
     else filename = !ARCTAS+'/IDL/analysis/'+filename+'.ps'
     multipanel,rows=2,cols=1
     open_device, /ps, filename=filename, Bits=8, WinParam = [0, 300, 400], $
                  /color, /portrait
     !p.font = 0 
   endif else if ~keyword_set(choose_win) then window,1

; Observations
plot, obs,alt,color=1,psym=sym(1),symsize=0.2,yrange=[0,10.5],ystyle=9,         $
      xtitle=xtitle,ytitle=ytitle,title=title,xstyle=9,xrange=[mindata,maxdata],$
      /nodata
oplot, obs_median,alt_median,color=1,linestyle=0,thick=2

; Model
for d = 0, ndirs-1 do begin

   s = 'oplot, mod' + string(d, '(i1.1)') + '_median, alt_median,color=' + $
       string(d+2,'(i1.1)') + ',linestyle=0,thick=2'
   status = execute( s )

endfor

; IQR
for i = 0, n_elements(obs_25)-1 do begin
   oplot,[obs_25[i],obs_75[i]],$
	  [alt_median[i],alt_median[i]],color=1,linestyle=0,thick=2
endfor

; Legend
colors=indgen(ndirs+1)+1
lines=intarr(ndirs+1)
thicks=intarr(ndirs+1)+2
labels=[strupcase(platform)+' Obs',dirs]
legend, lcolor=colors,line=lines,lthick=thicks,label=labels, $
	halign=0.9, valign=0.9, charsize=1.2, /color

if Keyword_Set(save) then close_device
endif
stop
end
