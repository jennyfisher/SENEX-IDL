pro ts_date, Species, Platform, Date,_extra=_extra

  plot_2var_ts,Species,Platform,Date, title=Platform+' '+Date, thick=2,$
               _extra=_extra

  d = get_model_data_arctas(Species, Platform,Date)
  t = get_model_data_arctas('DOY',Platform,Date)
  
  oplot, t, d, color=3, thick=2

  legend, lcolor=[3,4], line=[0,0], lthick=[2,2], label=['Model',Platform],$
    halign=0.9, valign=0.9, charsize=1.2, /color

end

pro arctas_nrt_ts,Species, Platform,_extra=_extra
 
  if not Keyword_Set( Platform ) then Platform = 'DC8'

  If StrLowCase( Platform ) eq 'dc8' then begin

    window,0 
    ts_date,Species,Platform,'20080629',_extra=_extra

  endif else if StrLowCase( Platform ) eq 'p3b' then begin

    window,0 
    ts_date,Species,Platform,'20080331'

    window,1
    ts_date,Species,Platform,'20080401'

    window,2
    ts_date,Species,Platform,'20080406'

    ;window,3
    ;ts_date,Species,Platform,'20080408'

    ;window,4
    ;ts_date,Species,Platform,'20080409'

  endif


end
