pro make_profiles,species,platform,flightdates=flightdates,_extra=_extra

if N_Elements(species) eq 0 then begin
        species='CO'
        print,'You didn''t specify a species, so CO is being plotted!'
endif
if N_Elements(platform) eq 0 then begin
        platform='DC8'
        print, 'You didn''t specify a platform, so DC8 is being plotted!'
endif
if N_Elements(flightdates) eq 0 then begin
        flightdates='2008*'
        print, 'You didn''t specify flightdates, so all dates are being plotted!'
endif

species = strupcase(species)

CASE species of
    'CO' : begin
	Unit = 'ppbv'
    end
    'HG0' : begin
	Unit = 'ppqv'
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
    'CA' : begin
	Unit = 'pptv'
    end
    else:
ENDCASE

arctas_obs_mod_profiles,species,platform,flightdates=flightdates,mindata=mindata,maxdata=maxdata,unit=unit,_extra=_extra

end
