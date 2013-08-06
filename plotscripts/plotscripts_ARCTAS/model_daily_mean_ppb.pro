pro model_daily_mean_ppb, alt0=alt0,alt1=alt1,tracer=tracer, polar=polar,$
			  day=day,save=save,mindata=mindata,maxdata=maxdata,$
			  _extra=_extra

;The following program reads in timesseries data and creates one of
;two plots  

;Inputs:
;level    - specify sigma level you wish to plot
;day    - specify day of april you wish to plot (april is hardwired)
; tracer - specify tracer you wish to plot (CO is default)


;Program begins here ------------------------------------------

 ;  myct,33,ncolors=20

   ;Specify Directory
   dir =  '/home/jaf/testrun/runs/run.ARCTAS.v8-01-04.2x25/bpch/tco_scaled2/'
   if day le 10 then file = dir+'tco.ctm.20080401.flambe.bpch' $
   else if day le 20 then file = dir+'tco.ctm.20080411.flambe.bpch' $
   else file = dir+'tco.ctm.20080421.flambe.bpch'
 
   if n_elements(tracer) eq 0 then tracer = 1
 
   ;get grid information
   ModelInfo1 = ctm_type('GEOS5_47L',  res=2)
   GridInfo1 = ctm_grid(ModelInfo1)

   tmp = min(abs(GridInfo1.zmid-alt0),ZMIN)
   tmp = min(abs(GridInfo1.zmid-alt1),ZMAX)

   ; Number of vertical levels (1 less than edges)
   IMX = GridInfo1.IMX
   JMX = GridInfo1.JMX
   LMX = ZMAX - ZMIN + 1L

   ; G0_100 is 100 / the gravity constant 
   G0_100    = 100d0 / 9.81d0

   ; Molecules air / kg air
   XNumolAir = 6.022d23 / 28.97d-3

   A_cm2 = CTM_BoxSize(GridInfo1,/GEOS,/cm2)
   A_m2  = A_cm2 / 1d4
   
   date = 20080400L + day
   tau0=nymd2tau(date,0L)

      ;read in data
      ctm_get_data,  ThisDataInfo1, 'IJ-AVG-$',filename=file,  $
	tracer=tracer, tau0=tau0
      first=ThisDataInfo1.first(0)
      iFirst=first(0)-1
      first=ThisDataInfo1.first(1)
      jFirst=first(0)-1

	YInd = where(GridInfo1.ymid gt 40.)-jFirst+1
	Name = ThisDataInfo1.tracername

      CO = *(ThisDataInfo1.data)
      CO = CO * 1d-9

      Success = CTM_Get_DataBlock( PressTemp, 'PEDGE-$',    $
                                ThisDataInfo=ThisDataInfo2, $   
                                ModelInfo=ModelInfo2,       $
                                Tracer=1,                   $
                                Tau0=Tau0,               $
                                FileName=Fi,                $
                                /Quiet,      /NoPrint )
      Press = PressTemp(*,*,0)

      ; Error check 
      if ( not Success ) then Message, 'Could not find surface pressure data!'

      InVertEdge = GridInfo1.EtaEdge[ 0:ThisDataInfo1.Dim[2] ]

      ; Define airmass array
      AirMass = DblArr( IMX, JMX, LMX )
      AirMassAll = DblArr( IMX, JMX )

      ; Loop over levels
      for L = ZMIN, ZMAX do begin
         AirMass[*,*,L-ZMIN] = Press[*,*] * A_m2[*,*] * $
		( InVertEdge[L] - InVertEdge[L+1] ) * G0_100
      endfor

      ; Convert air mass from [kg] to [molec]
      AirMass = AirMass * XNumolAir

      ; Also get overall AirMass for partial column
      AirMassAll = Press * A_m2 * G0_100 * XNuMolAir * $
		   (InVertEdge[ZMIN] - InVertEdge[ZMAX+1])
      ;====================================================================
      ; Compute number density in molec/cm2 
      ;
      ; AirMass = air mass in [molec air]
      ;
      ; C       = column in the grid box (I,J,L)
      ;         = [v/v] * [molec air] / [area of grid box in cm2]
      ;
      ; NOTES: 
      ; (1) The box heights cancel out when we do the algebra, so we 
      ;     are left w/ the above expression for Column!
      ;====================================================================

      ; Create array for column 
      C = DblArr( IMX, JMX, LMX )

      ; Compute layer column densities
      for L = ZMIN, ZMAX do begin
	C[*,*,L-ZMIN] = ( CO[*,*,L] * AirMass[*,*,L-ZMIN] ) / A_Cm2
      endfor

      ; Sum to get total column
      COcol = Total(C, 3)

      ; Convert back to weighted mean ppb value
      COppb = 1d9 * ( COcol * A_cm2 ) / AirMassAll

      ; Get the region you want to plot
      COppb = COppb(*, YInd)

   if Keyword_Set(save) then open_device,/z else window, 0

   Title=Name+', '+$
	string(alt0,'(i2.2)')+' to '+string(alt1,'(i2.2)')+' km, '+$
	string(date,'(i8.8)')
	
   if n_elements(mindata) eq 0 then mindata=0
   if n_elements(maxdata) eq 0 then maxdata=50
   dcolor = (maxdata-mindata)/20.

   if Keyword_set(Polar) then begin
      TVMap, COppb(*,*), GridInfo1.Xmid, GridInfo1.YMid(YInd+jFirst-1), $
	 /polar,  /continents, /coasts, Divisions=6, /fcontour,     $
         maxdata=maxdata, mindata=mindata,  /isotropic,  /nogylabels,$
         /keepaspectratio,mparam=[90,250,0], $
	 Title=Title,cbunit='[ppbv]',/cbar,             $
	 c_levels=indgen(20)*dcolor+mindata, $
	 /no_erase,/no_advance,$
	 _extra=_extra
      TVMap, COppb(*,*), GridInfo1.Xmid, GridInfo1.YMid(YInd+jFirst-1), $
	 /polar,  /continents, /coasts, Divisions=6, /nodata, /isotropic,$
	 /cbar,title=title,cbunit='[ppbv]',mparam=[90,250,0],$
	 mindata=mindata,maxdata=maxdata
   endif else begin
      TVMap, COppb(*,*), GridInfo1.Xmid, GridInfo1.YMid(YInd+jFirst-1), $
         /fcontour,/orthographic,/isotropic, mparam=[90,250,0],        $
         limit=[55,170,90,250,55,350,55,250], /continents,/grid,     $
	 /coasts,mindata=mindata, maxdata=maxdata, title=title,      $
  	 cbunit='[ppbv]',/cbar,                      $
	 c_levels=indgen(20)*dcolor+mindata, $
	 _extra=_extra
   endelse
   
   if keyword_set(save) then screen2gif, $
	!ARCTAS+'/IDL/analysis/gif/'+Name+'_'+string(date,'(i8.8)')+'.gif'

end
