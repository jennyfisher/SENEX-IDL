pro ts_date, Species, Platform, Flightdates=Flightdates,_extra=_extra

  plot_2var_ts,Species,Platform,Flightdates, $
	title=strupcase(Platform)+' '+Flightdates, $
	thick=2, _extra=_extra

  d = get_model_data_arctas(Species, Platform,Flightdates,_extra=_extra)
  t = get_model_data_arctas('DOY',Platform,Flightdates,_extra=_extra)
  
  oplot, t, d, color=3, thick=2

  legend, lcolor=[3,4], line=[0,0], lthick=[2,2],$
	label=['Model',strupcase(Platform)],$
	halign=0.9, valign=0.9, charsize=1.2, /color

end

