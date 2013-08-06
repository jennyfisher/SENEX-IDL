; Plot correlations of observations and model
  multipanel,/off
  flightdates='20080419'
  nocities=0;1
  maxdata=250

  hg0=get_field_data_arctas('hg0','dc8',flightdates,/trop,nocities=nocities)
  co=get_field_data_arctas('co','dc8',flightdates,/trop,nocities=nocities)
  alt=get_field_data_arctas('altp','dc8',flightdates,/trop,nocities=nocities)
  doy=get_field_data_arctas('doy','dc8',flightdates,/trop,nocities=nocities)
  lat=get_field_data_arctas('lat','dc8',flightdates,/trop,nocities=nocities)

  hg0_mod  =get_model_data_arctas('hg0', 'DC8',flightdates,/trop,nocities=nocities, $
		altdir=altdir,/hg)
  doy_mod =get_model_data_arctas('DOY','DC8',flightdates,/trop,nocities=nocities, $
		altdir=altdir,/hg)

  ; Interpolate model to observation time coordinate
  hg0_mod  = interpol( hg0_mod,  doy_mod, doy )

  ind = where( lat gt 55 )
  hg0 = hg0[ind] & hg0_mod = hg0_mod[ind]
  alt = alt[ind] & co = co[ind] & lat = lat[ind]

  myct, 33, ncolors=30

  window,0

  scatterplot_datacolor, hg0_mod, hg0, alt, /xstyle,/ystyle, $
    xtitle='Model Hg0, ppt',ytitle='Obs Hg0, ppt', unit='Alt, km',$
    xrange=[0,maxdata],yrange=[0,maxdata],zmin=0,zmax=11,cbposition=[.1,.9,.5,.93]
  oplot,[0,maxdata],[0,maxdata],color=1,linestyle=0

  window, 1

  scatterplot_datacolor, hg0_mod, hg0, co, /xstyle,/ystyle, $
    xtitle='Model Hg0, ppt',ytitle='Obs Hg0, ppt', unit='CO, ppb',$
    xrange=[0,maxdata],yrange=[0,maxdata],cbposition=[.1,.9,.5,.93],$
    zmin=50,zmax=200
  oplot,[0,maxdata],[0,maxdata],color=1,linestyle=0


end
