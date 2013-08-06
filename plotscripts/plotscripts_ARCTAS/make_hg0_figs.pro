
    HgModDir = 'totHg'

    ps_setup, /open, file='ARCTAS_Hg0.test.ps', xsize=7, ysize=8

    hg0 = get_field_data_arctas( 'hg', 'DC8', '*', /minavg )
    CO  = get_field_data_arctas( 'CO', 'DC8', '*', /minavg )
    altp = get_field_data_arctas( 'altp', 'DC8', '*', /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', '*', /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', '*', /minavg )

    ; Bin by km
    hg0_bin = tapply( Hg0, round(altp), 'median', group=alt_bin )

    multipanel, row=2, col=2

    scatterplot, hg0, altp, $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='All ARCTAS'

    scatterplot, hg0, altp, $
                 xrange=[0, 300], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='All ARCTAS'

    oplot, hg0_bin,  alt_bin, color=3, thick=3
    

    percentileplot, Hg0, altp, group=indgen(14), /vertical, $
                 xrange=[0, 200], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='All ARCTAS'

    multipanel, /off
  
    multipanel, col=1, row=1

    i = where( altp lt 4 )

    arctas_map, lon[i], lat[i], hg0[i], mindata=50, maxdata=200, $
                title='ARCTAS <4km', latmin=45, /hires, symsize=0.5, thick=1

    multipanel, /off


    ;============================================
    ; Plots for Fairbanks
    ;============================================
  
    site = 'Fairbanks'

    hg0 = get_field_data_arctas( 'hg', 'DC8', site, /minavg )
    CO  = get_field_data_arctas( 'CO', 'DC8', site, /minavg )
    altp = get_field_data_arctas( 'altp', 'DC8', site, /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', site, /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', site, /minavg )

    hg0_mod = get_model_data_arctas( 'hg0', 'DC8', site, altdir=HgModDir )
    CO_mod  = get_model_data_arctas( 'CO',  'DC8', site, altdir='fullchem' )
    alt_mod = get_model_data_arctas( 'alt', 'DC8', site, altdir=HgModDir )
    lat_mod = get_model_data_arctas( 'lat', 'DC8', site, altdir=HgModDir )
    lon_mod = get_model_data_arctas( 'lon', 'DC8', site, altdir=HgModDir )

    ; Bin by km
    hg0_bin = tapply( Hg0, round(altp), 'median', group=alt_bin )
    hg0_mod_bin = tapply( Hg0_mod, round(alt_mod), 'median', group=alt_mod_bin )
    CO_bin = tapply( CO, round(altp), 'median', group=alt_bin )
    CO_mod_bin = tapply( CO_mod, round(alt_mod), 'median', group=alt_mod_bin )

    device, ysize=8, /inches

    multipanel, row=2, col=2

    scatterplot, hg0, altp, $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    scatterplot, hg0, altp, $
                 xrange=[0, 300], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    oplot, hg0_bin,  alt_bin, color=3, thick=5
    oplot, hg0_mod_bin,  alt_mod_bin, color=2, thick=5

    legend, label=['obs','obs mean','model mean'],$
            psym=[sym(0),0,0],line=[-1,0,0],$
            lcolor=[1,3,2],/color,halign=0.9,valign=0.95,/frame

    scatterplot, CO, altp, $
                 xrange=[0, 300], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='CO, ppbv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site
    oplot, CO_bin,  alt_bin, color=3, thick=5
    oplot, CO_mod_bin,  alt_mod_bin, color=2, thick=5

    ii=where( finite(Hg0) and finite(altp) )
    percentileplot, Hg0[ii], altp[ii], group=indgen(14), /vertical, /color, $
                 xrange=[0, 200], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site
    percentileplot, hg0_mod, alt_mod, group=indgen(14), /vertical, $
                    /overplot, color=2

    multipanel, /off


    ;============================================
    ; Plots for Cold Lake
    ;============================================
  
    site = 'ColdLake'

    hg0 = get_field_data_arctas( 'hg', 'DC8', site, /minavg )
    altp = get_field_data_arctas( 'altp', 'DC8', site, /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', site, /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', site, /minavg )


    hg0_mod = get_model_data_arctas( 'hg0', 'DC8', site, altdir=HgModDir )
    alt_mod = get_model_data_arctas( 'alt', 'DC8', site, altdir=HgModDir )
    lat_mod = get_model_data_arctas( 'lat', 'DC8', site, altdir=HgModDir )
    lon_mod = get_model_data_arctas( 'lon', 'DC8', site, altdir=HgModDir )

    ; Bin by km
    hg0_bin = tapply( Hg0, round(altp), 'median', group=alt_bin )
    hg0_mod_bin = tapply( Hg0_mod, round(alt_mod), 'median', group=alt_mod_bin )

    multipanel, row=2, col=2

    scatterplot, hg0, altp, $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    scatterplot, hg0, altp, $
                 xrange=[0, 300], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    oplot, hg0_bin,  alt_bin, color=3, thick=3
    oplot, hg0_mod_bin,  alt_mod_bin, color=2, thick=5

    ii=where( finite(Hg0) and finite(altp) )
    percentileplot, Hg0[ii], altp[ii], group=indgen(14), /vertical, /color, $
                 xrange=[0, 200], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site
    percentileplot, hg0_mod, alt_mod, group=indgen(14), /vertical, $
                    /overplot, color=2

    multipanel, /off


    ;============================================
    ; Plots for CARB over land
    ;============================================
  
    site = 'CARB land'

    flightdates=['20080618', '20080620', '20080624', '20080626', '20080713']

    hg0 = get_field_data_arctas( 'hg', 'DC8', flightdates, /minavg )
    altp = get_field_data_arctas( 'altp', 'DC8', flightdates, /minavg )
    altr = get_field_data_arctas( 'radar_altitude', 'DC8', flightdates, /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', flightdates, /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', flightdates, /minavg )
    index = get_field_data_arctas( 'index', 'DC8', flightdates, /minavg )
    index = floor( index / 1e4 )

    ; Use only selected flights
    i= where( lat lt 40 and lon lt 245 )
    hg0 = hg0[i]
    altp=altp[i]
    altr=altr[i]
    lat = lat[i]
    lon = lon[i]

    hg0_mod = get_model_data_arctas( 'hg0', 'DC8', flightdates, altdir=HgModDir )
    alt_mod = get_model_data_arctas( 'alt', 'DC8', flightdates, altdir=HgModDir )
    lat_mod = get_model_data_arctas( 'lat', 'DC8', flightdates, altdir=HgModDir )
    lon_mod = get_model_data_arctas( 'lon', 'DC8', flightdates, altdir=HgModDir )

    i= where( lat_mod lt 40 and lon_mod lt -115 )
    hg0_mod = hg0_mod[i]
    alt_mod=alt_mod[i]
    lat_mod = lat_mod[i]
    lon_mod= lon_mod[i]


    ; Bin by km
    hg0_bin = tapply( Hg0, round(altp), 'mean', group=alt_bin, /nan)
    hg0_binr= tapply( Hg0, round(altr), 'mean', group=altr_bin, /nan)
    hg0_mod_bin = tapply( Hg0_mod, round(alt_mod), 'mean', group=alt_mod_bin )

    multipanel, row=2, col=2

    scatterplot, hg0, altp, $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    scatterplot, hg0, altp, $
                 xrange=[0, 300], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    oplot, hg0_bin,  alt_bin, color=3, thick=3
    oplot, hg0_binr,  altr_bin, color=4, thick=3
    oplot, hg0_mod_bin,  alt_mod_bin, color=2, thick=5

    ii=where( finite(Hg0) and finite(altp) )
    percentileplot, Hg0[ii], altp[ii], group=indgen(14), /vertical, /color, $
                 xrange=[0, 200], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site
    percentileplot, hg0_mod, alt_mod, group=indgen(14), /vertical, $
                    /overplot, color=2

    multipanel, /off

    ;============================================
    ; Plots for CARB over water
    ;============================================
  
    site = 'CARB water'

    flightdates=['20080622']

    hg0 = get_field_data_arctas( 'hg', 'DC8', flightdates, /minavg )
    altp = get_field_data_arctas( 'altp', 'DC8', flightdates, /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', flightdates, /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', flightdates, /minavg )
    index = get_field_data_arctas( 'index', 'DC8', flightdates, /minavg )
    index = floor( index / 1e4 )

    hg0_mod = get_model_data_arctas( 'hg0', 'DC8', flightdates, altdir=HgModDir )
    alt_mod = get_model_data_arctas( 'alt', 'DC8', flightdates, altdir=HgModDir )
    lat_mod = get_model_data_arctas( 'lat', 'DC8', flightdates, altdir=HgModDir )
    lon_mod = get_model_data_arctas( 'lon', 'DC8', flightdates, altdir=HgModDir )

    ; Bin by km
    hg0_bin = tapply( Hg0, round(altp), 'median', group=alt_bin )
    hg0_mod_bin = tapply( Hg0_mod, round(alt_mod), 'median', group=alt_mod_bin )

    multipanel, row=2, col=2

    scatterplot, hg0, altp, $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    scatterplot, hg0, altp, $
                 xrange=[0, 300], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site

    oplot, hg0_bin,  alt_bin, color=3, thick=3
    oplot, hg0_mod_bin,  alt_mod_bin, color=2, thick=5

    ii=where( finite(Hg0) and finite(altp) )
    percentileplot, Hg0[ii], altp[ii], group=indgen(14), /vertical, /color, $
                 xrange=[0, 200], $
                 yrange=[0, 13], /ystyle, $
                 symsize=0.2,  xtitle='Hg(0), ppqv', $
                 ytitle='Altitude, km',  $
                 title='ARCTAS '+site
    percentileplot, hg0_mod, alt_mod, group=indgen(14), /vertical, $
                    /overplot, color=2

    multipanel, /off

    ps_setup, /close

end
