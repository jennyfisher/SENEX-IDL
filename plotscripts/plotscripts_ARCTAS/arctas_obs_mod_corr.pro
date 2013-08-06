; Plot correlations of observations and model
  multipanel,/off
  flightdates='fairbanks'
  altdir='fc_isoII_withsnowdep'
  tco=0;1
  nocities=0;1
  maxdata=50

  co=get_field_data_arctas('co','dc8',flightdates,/trop,/nocities)
  nit=get_field_data_arctas('nit','dc8',flightdates,/trop,/nocities)
  alt=get_field_data_arctas('altp','dc8',flightdates,/trop,/nocities)
  doy=get_field_data_arctas('doy','dc8',flightdates,/trop,/nocities)
  lat=get_field_data_arctas('lat','dc8',flightdates,/trop,/nocities)

  nit_mod  =get_model_data_arctas('nit', 'DC8',flightdates,/trop,/nocities, $
		altdir=altdir,tco=tco)
  doy_mod =get_model_data_arctas('DOY','DC8',flightdates,/trop,/nocities, $
		altdir=altdir,tco=tco)

  ; Interpolate model to observation time coordinate
  nit_mod  = interpol( nit_mod,  doy_mod, doy )

  ind = where( alt le 10 and alt ge 7 )
  nit = nit[ind] & nit_mod = nit_mod[ind]
  alt = alt[ind] & co = co[ind] & lat = lat[ind]

  ind = where( lat gt 55 )
  nit = nit[ind] & nit_mod = nit_mod[ind]
  alt = alt[ind] & co = co[ind] & lat = lat[ind]


  myct, 33, ncolors=30

  window,0

  scatterplot_datacolor, nit_mod, nit, alt, /xstyle,/ystyle, $
    xtitle='Model nit, ppt',ytitle='Obs nit, ppt', unit='Alt, km',$
    xrange=[0,maxdata],yrange=[0,maxdata],zmin=0,zmax=11,cbposition=[.6,.9,.8,.93]
  oplot,[0,maxdata],[0,maxdata],color=1,linestyle=0

  window, 1

  scatterplot_datacolor, nit_mod, nit, co, /xstyle,/ystyle, $
    xtitle='Model nit, ppt',ytitle='Obs nit, ppt', unit='CO, ppb',$
    xrange=[0,maxdata],yrange=[0,maxdata],cbposition=[.6,.9,.8,.93],$
    zmin=50,zmax=250
  oplot,[0,maxdata],[0,maxdata],color=1,linestyle=0


end
