pro mean_model_maps,    level=level,tracer=tracer, polar=polar,$
			save=save,mindata=mindata,maxdata=maxdata,$
			scale=scale

;The following program reads in timesseries data and creates one of
;two plots  

;Inputs:
;level    - specify sigma level you wish to plot
;day    - specify day of april you wish to plot (april is hardwired)
; tracer - specify tracer you wish to plot (CO is default)


;Program begins here ------------------------------------------

   myct,33,ncolors=20

   ;Specify Directory
   dir =  '/home/jaf/testrun/runs/run.v8-01-04.2x25/bpch/tco_scaled2/'
   file = [dir+'tco.ctm.20080401.flambe.bpch',$
    	   dir+'tco.ctm.20080411.flambe.bpch',$
	   dir+'tco.ctm.20080421.flambe.bpch']
 
   if n_elements(level) eq 0 then level = 0
   if n_elements(tracer) eq 0 then tracer = 4
   scf = 1.00
   if keyword_set(scale) then begin
	if tracer eq 2 then scf = 1.04
	if tracer eq 3 then scf = 1.39
	if tracer eq 5 then scf = 1.23
	if tracer eq 10 then scf = 0.21
	if tracer eq 11 then scf = 0.32
   endif

 
   ;get grid information
   ModelType = ctm_type('GEOS5',  res=2)
   Grid = ctm_grid(ModelType)
   
   date = lindgen(30)+20080401L
   tau0=nymd2tau(date,0L)

   for i = 0, n_elements(date)-1 do begin

      if      ( date[i] le 20080410 ) then fi = file[0] $
      else if ( date[i] le 20080420 ) then fi = file[1] $
      else                                 fi = file[2]

      print,'Reading data for date: ',date(i)

      ;read in data
      ctm_get_data,  datainfo, 'IJ-AVG-$',filename=fi,  $
	tracer=tracer, tau0=tau0[i]
      first=datainfo.first(0)
      iFirst=first(0)-1
      first=datainfo.first(1)
      jFirst=first(0)-1
      if i eq 0 then YInd = where(grid.ymid gt 40.)-jFirst+1

      CO = *(datainfo.data)
      if i eq 0 then Name = datainfo.tracername

      ;get only the region you want to plot
      CO = CO(*, *, level)
      CO = CO(*, YInd)*scf

      if i eq 0 then COtot = CO else COtot = COtot + CO

      COcol = ctm_column_du('IJ-AVG-$',filename=fi,tau0=tau0[i],$
	tracer=tracer, ptau0=tau0[i])

      COcol = COcol(*, YInd)*scf

      if i eq 0 then COcoltot = COcol else COcoltot = COcoltot + COcol

   endfor

   COavg = COtot / n_elements(date)
   COcolavg = COcoltot / n_elements(date)
      
   Title='Average ARCTAS '+Name+' at '+string(grid.zmid(level),'(f4.1)')+' km'

   if n_elements(mindata) eq 0 then mindata=0
   if n_elements(maxdata) eq 0 then maxdata=100
   dcolor = (maxdata-mindata)/20.

   if keyword_set(save) then begin
	filename='../analysis/model_'+Name+'_L'+string(level,'(i2.2)')+'.ps'
        multipanel,rows=2,cols=1
        open_device, /ps, filename=filename, Bits=8, $
                WinParam = [0, 300,400], /color, /portrait
        !p.font = 0 
   endif else begin & window,0 & multipanel,/off & endelse

   ;Plot data -see tvmap.pro for keyword definitions etc.
   if Keyword_set(Polar) then begin
      TVMap,  COavg(*, *),  Grid.Xmid,  Grid.YMid(YInd+jFirst-1),      $
	 /fcontour, /polar,  /continents, /coasts, Divisions=6,$
         maxdata=maxdata, mindata=mindata,  /isotropic,  grid=1,    $
         Title=Title,/keepaspectratio,mparam=[90,250,0],              $
  	 cbunit='[ppbv]',/cbar, c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endif else begin
      TVMap,  COavg(*,*),   Grid.Xmid,  Grid.YMid(YInd+jFirst-1),      $
         /fcontour,/orthographic,/isotropic, mparam=[90,250,0],       $
         limit=[55,170,90,250,55,350,55,250], /continents,/grid,    $
	 /coasts,mindata=mindata, maxdata=maxdata, title=title,     $
  	 cbunit='[ppbv]',/cbar, c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endelse

   Title='Average ARCTAS '+Name+' column'
   mindata=0
   maxdata=1
   dcolor = (maxdata-mindata)/20.

   if ~keyword_set(save) then window,2

   if Keyword_set(Polar) then begin
      TVMap, COcolavg(*,*)/1d18, Grid.Xmid, Grid.YMid(YInd+jFirst-1),       $
	 /polar,  /continents, /coasts, Divisions=6, /fcontour,     $
         maxdata=maxdata, mindata=mindata,  /isotropic,  /grid,          $
         Title=Title,/keepaspectratio,mparam=[90,250,0],              $
  	 cbunit='10!U18!N molec/cm!U2!N',/cbar,                      $
	 c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endif else begin
      TVMap, COcolavg(*,*)/1d18, Grid.Xmid, Grid.YMid(YInd+jFirst-1),   $
         /fcontour,/orthographic,/isotropic, mparam=[90,250,0],        $
         limit=[55,170,90,250,55,350,55,250], /continents,/grid,     $
	 /coasts,mindata=mindata, maxdata=maxdata, title=title,      $
  	 cbunit='10!U18!N molec/cm!U2!N',/cbar,                      $
	 c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endelse
   if keyword_set(save) then close_device
	CLOSE,/file
stop
end
