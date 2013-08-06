pro plot_arctas2, FileName=FileName, $
                       psFileName=psFileName, $
                       PS=PS, $
                       DataRange=Range, $
                       PageTitle=PageTitle, $
                       _Extra=_Extra

   ;=======================================
   ; Setup
   ;=======================================
   
   Species = 'Hg0'
   DiagN = 'IJ-AVG-$'

   if ( not Keyword_set( FileName ) ) then $
      FileName = 'ctm.bpch'
 
   ; Plot whole globe by default
   Global = 0L
   if ( not Keyword_set( lonRange ) ) then begin
      lonRange = [-180, 180]
      Global = 1L
   endif 
   if Keyword_set( psFileName ) then $
      PS = 1L $
   else $
      PS = 0L
 
   if ( not Keyword_set( psFileName ) ) then begin
         psFileName = 'vertical.ps' 
   endif
 
   if ( not Keyword_set( Range ) ) then $
      Range = [0, 0.25]
   unit = 'pptv'

   ;=======================================
   ; Read ground elevation
   ; Convert aircraft altitude to altitude above ground
   ;=======================================

   restore, !HOME +'/data/smith_sandwell_topo_v8_2.sav', /verbose


   ;=======================================
   ; Read Observations: CARB
   ;
   ;=======================================

   ; CARB
   flightdates=['20080618', '20080620', '20080622', '20080624', '20080626', $
                '20080713']
   hg = get_field_data_arctas( 'hg', 'DC8', flightdates, /minavg )
   altgps = get_field_data_arctas( 'gps_altitude', 'DC8', flightdates, /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', flightdates, /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', flightdates, /minavg )
    ch3cn = get_field_data_arctas( 'acetonitrile_ptrms', 'DC8', flightdates, /minavg )
    O3 = get_field_data_arctas( 'O3', 'DC8', Flightdates, /minavg )

    ; Altitude above surface
    jj = value_locate( topo.lat, lat )
    ii = value_locate( topo.lon, lon )
    altg = altgps - (topo.alt[ii,jj] > 0)/1000. 

    ; CARB over water, excluding fires
    i = where(topo.alt[ii,jj] lt -1500 and  lat lt 45 and lon lt 245 and $
              ch3cn lt 0.25 )
    CARBwater = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i]}

    ; CARB over land, without fires
    i = where(topo.alt[ii,jj] gt -100 and  lat lt 45 and lon lt 245 and $
              ch3cn lt 0.25 )
    CARBland = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i] }

    ; CARB over land, with fires
    i = where(topo.alt[ii,jj] gt -100 and  lat lt 45 and lon lt 245 )
    CARBlandfires = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i] }

    
   ;=======================================
   ; Read Observations: Cold Lake
   ;
   ;=======================================

    ; Cold Lake
    site = 'ColdLake'
    hg = get_field_data_arctas( 'hg', 'DC8', site, /minavg )
    altgps = get_field_data_arctas( 'gps_altitude', 'DC8', site, /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', site, /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', site, /minavg )
    ch3cn = get_field_data_arctas( 'acetonitrile_ptrms', 'DC8', site, /minavg )
    O3 = get_field_data_arctas( 'O3', 'DC8', site, /minavg )

    ; Altitude above surface
    jj = value_locate( topo.lat, lat )
    ii = value_locate( topo.lon, lon )
    altg = altgps - (topo.alt[ii,jj] > 0)/1000. 

    ; Cold Lake with fires, exclude pyroCB
    i = where( Hg lt 300 and lat gt 45 and lat lt 70 )
    ColdLakeFires = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i] }

    ; Cold Lake without fires
    i = where(ch3cn lt 0.25 and Hg lt 300 and lat gt 45 and lat lt 70 )
    ColdLake = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i] }

    ; Cold Lake without total depletion
    i = where(ch3cn lt 0.25 and O3 lt 100 and $
              lat gt 45 and lat lt 70 )
    ColdLakeNoStrat = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i] }


   ;=======================================
   ; Read Observations: Fairbanks
   ;
   ;=======================================

    ; Fairbanks
    site = 'Fairbanks'
    hg = get_field_data_arctas( 'hg', 'DC8', site, /minavg )
    altgps = get_field_data_arctas( 'gps_altitude', 'DC8', site, /minavg )
    lat = get_field_data_arctas( 'lat', 'DC8', site, /minavg )
    lon = get_field_data_arctas( 'lon', 'DC8', site, /minavg )
    ch3cn = get_field_data_arctas( 'acetonitrile_ptrms', 'DC8', site, /minavg )
    O3 = get_field_data_arctas( 'O3', 'DC8', site, /minavg )

    ; Altitude above surface
    jj = value_locate( topo.lat, lat )
    ii = value_locate( topo.lon, lon )
    altg = altgps - (topo.alt[ii,jj] > 0)/1000. 

    ; Fairbanks with fires
    i = where( lat gt 64 )
    FairbanksFires = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i]}

    ; Fairbanks without fires
    i = where(ch3cn lt 0.25 and lat gt 64 )
    Fairbanks = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i] }

    ; Fairbanks without total depletion
    i = where(ch3cn lt 0.25 and ( Hg gt 0 or altg lt 4) and lat gt 64 )
    i = where(ch3cn lt 0.25 and O3 lt 100 and lat gt 64 )
    FairbanksNoStrat = { Hg: Hg[i],  alt: altg[i],  lat: lat[i],  lon:lon[i], O3:O3[i] }
 

    ;=======================================
    ; Read BPCH data
    ; CARB over land. This is very approximate because the 
    ; GEOS 4x5 grid boundaries do not follow California coastline
    ; We include some ocean here in the California bight
    ;=======================================

;     ctm_get_data, Hg0DataInfo, DiagN, FileName=Filename,  Tracer=1L

;      ; Get data for June
;     s = ctm_get_datablock( Hg0, DiagN, FileName=FileName, Tracer=1L, $
;                            lat=[32.7, 39.6], lon=[-121.6, -112.8], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[5].tau0 )

;     s = ctm_get_datablock( Hg2, DiagN, FileName=FileName, Tracer=2L, $
;                            lat=[32.7, 39.6], lon=[-121.6, -112.8], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[5].tau0 )

;     ; ppq
;     DC8hg0_CARBland = Hg0 * 1e3
;     DC8tgm_CARBland = (Hg0 + Hg2) * 1e3    


    ;=======================================
    ; Read BPCH data
    ; CARB over water. This is further off shore than the flights to 
    ; avoid terrestrial emissions
    ;=======================================

    ; Get data for June
;     s = ctm_get_datablock( Hg0, DiagN, FileName=FileName, Tracer=1L, $
;                            lat=[32.7, 39.6], lon=[-131.6, -128], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[5].tau0 )

;     s = ctm_get_datablock( Hg2, DiagN, FileName=FileName, Tracer=2L, $
;                            lat=[32.7, 39.6], lon=[-131.6, -128], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[5].tau0 )

;     ; ppq
;     DC8hg0_CARBwater = Hg0 * 1e3
;     DC8tgm_CARBwater = (Hg0 + Hg2) * 1e3    

    ;=======================================
    ; Read BPCH data
    ; Cold Lake in July
    ;=======================================

    ; Get data for July
;     s = ctm_get_datablock( Hg0, DiagN, FileName=FileName, Tracer=1L, $
;                            lat=[50, 70], lon=[-125, -88], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[7].tau0 )

;     s = ctm_get_datablock( Hg2, DiagN, FileName=FileName, Tracer=2L, $
;                            lat=[50, 70], lon=[-125, -88], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[7].tau0 )

;     ; ppq
;     DC8hg0_ColdLake = Hg0 * 1e3
;     DC8tgm_ColdLake = (Hg0 + Hg2) * 1e3 
    
    
    ; Hg in ppq, alt in km
    site='ColdLake'
    modelDir='totHg'
    hg0 = get_model_data_arctas( 'Hg0', 'DC8', site, altDir=modelDir )
    hg2 = get_model_data_arctas( 'Hg2', 'DC8', site, altDir=modelDir )
    alt = get_model_data_arctas( 'alt', 'DC8', site, altDir=modelDir )

    GC_ColdLake = {Hg0: hg0,  TGM: Hg0+Hg2, alt:alt}

    ;=======================================
    ; Read BPCH data
    ; Fairbanks in April
    ;=======================================

    ; Get data for April
;     s = ctm_get_datablock( Hg0, DiagN, FileName=FileName, Tracer=1L, $
;                            lat=[64, 90], lon=[-170, -60], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[3].tau0 )

;     s = ctm_get_datablock( Hg2, DiagN, FileName=FileName, Tracer=2L, $
;                            lat=[64, 90], lon=[-170, -60], average=3, $
;                            xmid=alt, tau0=Hg0DataInfo[3].tau0 )

;     ; ppq
;     DC8hg0_Fairbanks = Hg0 * 1e3
;     DC8tgm_Fairbanks = (Hg0 + Hg2) * 1e3    

    ; Hg in ppq, alt in km
    site='Fairbanks'
    hg0 = get_model_data_arctas( 'Hg0', 'DC8', site, altDir=modelDir )
    hg2 = get_model_data_arctas( 'Hg2', 'DC8', site, altDir=modelDir )
    alt = get_model_data_arctas( 'alt', 'DC8', site, altDir=modelDir )
    lat = get_model_data_arctas( 'lat', 'DC8', site, altDir=modelDir )
    lon = get_model_data_arctas( 'lon', 'DC8', site, altDir=modelDir )

    i=where(lat ge 64)

    GC_Fairbanks = {Hg0: hg0[i],  TGM: Hg0[i]+Hg2[i], alt:alt[i], $
                   lon: lon[i], lat: lat[i]}


   ;=======================================
   ; Plotting
   ;=======================================
   
   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=8, ysize=4, /landscape

   multipanel, col=4, row=1, omargin=[0.05, 0.05, 0.1, 0.1], $
               margin=0.04, pos=p

   p = getpos( 2, pos=p, margin=0 )

   bin = [-0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12.3]

   ; CARB over land
   plot_bins, CARBland.Hg,  CARBland.alt, /smooth, /color, bin=bin, $
                title='CARB land', xtitle='Hg, ppqv', $
                xrange=[50, 200], yrange=[0, 12], charsize=2, thick=3, pos=p
   plot_bins, CARBlandFires.Hg, CARBlandFires.alt, /smooth, /over, bin=bin, $
              color=5
;   oplot, DC8Hg0_CARBland, alt, color=2, thick=3
;   oplot, DC8TGM_CARBland, alt, color=3, thick=3

   multipanel, /advance, pos=p
   p = getpos( 2, pos=p, margin=0 )

   ; CARB over water
   plot_bins, CARBwater.Hg, CARBwater.alt, /smooth, /color, bin=bin, $
                title='CARB water', xtitle='Hg, ppqv', $
                xrange=[50, 200], yrange=[0, 12], charsize=2, thick=3, pos=p
;   oplot, DC8Hg0_CARBwater, alt, color=2, thick=3
;   oplot, DC8TGM_CARBwater, alt, color=3, thick=3



   multipanel, /advance, pos=p
   p = getpos( 2, pos=p, margin=0 )

   ; Cold Lake
   plot_bins, ColdLake.Hg,  ColdLake.alt, /smooth, /color, bin=bin, $
                title='Cold Lake', xtitle='Hg, ppqv', $
                xrange=[50, 200], yrange=[0, 12], charsize=2, thick=3, pos=p
   plot_bins, ColdLakeFires.Hg, ColdLakeFires.alt, /smooth, /over, bin=bin, $
              color=5
   plot_bins, ColdLakeNoStrat.Hg, ColdLakeNoStrat.alt, /smooth, /over, $
              bin=bin, color=6, /mad
   plot_bins, GC_ColdLake.Hg0, GC_ColdLake.alt, /smooth, /over, bin=bin, color=2,  thick=3, /mad
   plot_bins, GC_ColdLake.TGM, GC_ColdLake.alt, /smooth, /over, bin=bin, color=3,  thick=3, /mad


   multipanel, /advance, pos=p
   p = getpos( 2, pos=p, margin=0 )

   ; Fairbanks
   plot_bins, Fairbanks.Hg,  Fairbanks.alt, /smooth, /color, bin=bin,  $
                title='Fairbanks', xtitle='Hg, ppqv', $
                xrange=[0, 200], yrange=[0, 12], charsize=2, thick=3, pos=p
   plot_bins, FairbanksNoStrat.Hg, FairbanksNoStrat.alt, /smooth, /over, $
              color=6, bin=bin
   plot_bins, GC_Fairbanks.Hg0, GC_Fairbanks.alt, /smooth, /over, bin=bin, color=2,  thick=3, /mad
   plot_bins, GC_Fairbanks.TGM, GC_Fairbanks.alt, /smooth, /over, bin=bin, color=3,  thick=3, /mad



   multipanel, /off

   ; Make legend
   legend, label=['Obs','Obs w/fires', 'Obs no strat', 'Model Hg0', 'Model TGM'], $
           line=intarr(5), lcolor=[1, 5, 6, 2, 3], $
           halign=1.15, valign=.5, charsize=.8, /color, /frame

   If Keyword_Set( PS ) then $
      ps_setup, /close
stop
end
