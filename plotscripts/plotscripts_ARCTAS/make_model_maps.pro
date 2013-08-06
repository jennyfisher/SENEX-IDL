pro make_model_maps,  day=day,level=level,tracer=tracer, polar=polar,$
			save=save,mindata=mindata,maxdata=maxdata

;The following program reads in timesseries data and creates one of
;two plots  

;Inputs:
;level    - specify sigma level you wish to plot
;day    - specify day of april you wish to plot (april is hardwired)
; tracer - specify tracer you wish to plot (CO is default)


;Program begins here ------------------------------------------

   myct,33,ncolors=20

   ;Specify Directory
   dir = '/home/jaf/testrun/runs/run.ARCTAS.v8-01-04.2x25/bpch/tco_scaled2/'
 
   if n_elements(day) eq 0 then day = 1
   if n_elements(level) eq 0 then level = 0
   if n_elements(tracer) eq 0 then tracer = 4
 
   ;get grid information
   ModelType = ctm_type('GEOS5',  res=2)
   Grid = ctm_grid(ModelType)
   
   ;get date info
   month='04'
   i=string(day, '(i2.2)') 

   date = '2008'+ month + i

   if day le 10 then fi = dir+'tco.ctm.20080401.flambe.bpch' $
   else if day le 20 then fi = dir+'tco.ctm.20080411.flambe.bpch' $
   else fi = dir+'tco.ctm.20080421.flambe.bpch'

   taudate=8.*10000.+4*100+day
   tau0=nymd2tau(taudate,0L)

   ;read in data
   ctm_get_data,  datainfo, 'IJ-AVG-$',filename=fi,  tracer=tracer, tau0=tau0
   first=datainfo.first(0)
   iFirst=first(0)-1
   first=datainfo.first(1)
   jFirst=first(0)-1

   CO = *(datainfo.data)

   ;get only the region you want to plot
   YInd = where(grid.ymid gt 20.)-jFirst+1
   CO = CO(*, *, level)
   CO = CO(*, YInd)
      
   Title='CO at '+string(grid.zmid(level),'(f4.1)')+' km, '+date

   if n_elements(mindata) eq 0 then mindata=100
   if n_elements(maxdata) eq 0 then maxdata=250
   dcolor = (maxdata-mindata)/20.

   if keyword_set(save) then begin
	filename='model_CO_L'+string(level,'(i2.2)')+'_'+date+'.ps'
        multipanel,rows=2,cols=1
        open_device, /ps, filename=filename, Bits=8, $
                WinParam = [0, 300,400], /color, /portrait
        !p.font = 0 
   endif else window,0

   ;Plot data -see tvmap.pro for keyword definitions etc.
   if Keyword_set(Polar) then begin
      TVMap,  CO(*, *),  Grid.Xmid,  Grid.YMid(YInd+jFirst-1),      $
	 /sample, /polar,  /continents, /coasts, Divisions=6,$
         maxdata=maxdata, mindata=mindata,  /isotropic,  grid=1,    $
         Title=Title,/keepaspectratio,mparam=[90,210,0],              $
  	 cbunit='[ppbv]',/cbar, $,;c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endif else begin
      TVMap,  CO(*,*),   Grid.Xmid,  Grid.YMid(YInd+jFirst-1),      $
         /fcontour,/orthographic,/isotropic, mparam=[90,250,0],       $
         limit=[55,170,90,250,55,350,55,250], /continents,/grid,    $
	 /coasts,mindata=mindata, maxdata=maxdata, title=title,     $
  	 cbunit='[ppbv]',/cbar, c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endelse
   if keyword_set(save) then close_device

   COcol = ctm_column_du('IJ-AVG-$',filename=fi,tau0=tau0,tracer=tracer,$
                         ptau0=tau0)
   COcol = COcol(*, YInd)

   Title='CO column, '+date
   mindata=0
   maxdata=1.5
   dcolor = (maxdata-mindata)/20.

   window,2

   if Keyword_set(Polar) then begin
      TVMap, COcol(*,*)/1d18, Grid.Xmid, Grid.YMid(YInd+jFirst-1),       $
	 /polar,  /continents, /coasts, Divisions=6, /fcontour,     $
         maxdata=maxdata, mindata=mindata,  /isotropic,  /grid,          $
         Title=Title,/keepaspectratio,mparam=[90,210,0],              $
  	 cbunit='10!U18!N molec/cm!U2!N',/cbar,                      $
	 c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endif else begin
      TVMap, COcol(*,*)/1d18, Grid.Xmid, Grid.YMid(YInd+jFirst-1),   $
         /fcontour,/orthographic,/isotropic, mparam=[90,250,0],        $
         limit=[55,170,90,250,55,350,55,250], /continents,/grid,     $
	 /coasts,mindata=mindata, maxdata=maxdata, title=title,      $
  	 cbunit='10!U18!N molec/cm!U2!N',/cbar,                      $
	 c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endelse
	CLOSE,/file

end

