pro make_profiles,species,platform,flightdates=flightdates,_extra=_extra

if N_Elements(species) eq 0 then begin
        species='CO'
        print,'You didn''t specify a species, so CO is being plotted!'
endif
if N_Elements(platform) eq 0 then begin
        platform='WP3D'
        print, 'You didn''t specify a platform, so WP3D is being plotted!'
endif
if N_Elements(flightdates) eq 0 then begin
        flightdates='2013*'
        print, 'You didn''t specify flightdates, so all dates are being plotted!'
endif

species = strupcase(species)

CASE species of
    'CO' : begin
	Unit = 'ppbv'
    end
    'SO4' : begin
	Unit = 'ppt'
    end
    'SO2' : begin
	Unit = 'pptv'
    end
    'O3' : begin
	Unit = 'ppbv'
    end
    else:
ENDCASE

senex_obs_mod_profiles,species,platform,flightdates=flightdates,mindata=mindata,maxdata=maxdata,unit=unit,_extra=_extra

end
