; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        TRAJECT_PLOT
;
; PURPOSE:
;	 Plot forward or backward trajectories from FSU along an ARCTAS
;	 flight track.
;
; CATEGORY:
;	 None
;
; CALLING SEQUENCE:
;        TRAJECT_PLOT, Date[, Keywords]
;
; INPUTS:
;	 Date - Date of flight
;
; KEYWORD PARAMETERS:
;	 Backward - Set this keyword to plot backward trajectories
;	 Forward  - Set this keyword to plot forward trajectories
;	 LatMin   - Minimum latitude to be used in plotting
; 	 Descend  - Only plot trajectories that descend below 800 hPa
;	 Save     - Set this keyword to save the file
;
; OUTPUTS:
;	 None
;
; SUBROUTINES:
;	 External Subroutines Required:
;	 ==============================
;	 CTM_GRID	CTM_TYPE
;	 GET_DC8	TRAJ_LIMIT
;	 TVMAP		SCATTERPLOT_DATACOLOR
;
; REQUIREMENTS:
;	 Requires an input trajectory file created with
;	 READ_DC8_TRAJECT.PRO
;
; NOTES:
;	 To plot trajectories, a specific region of the flight track
;	 must be specified. This is done externally, in TRAJ_LIMIT.PRO
;	 If no limits are specified for a given flightdate, no
;	 trajectories will be plotted.
;
; EXAMPLE:
;	 TRAJ_LIMIT,20080416L,/BACK
;	
;	 Plots backward trajectories from 20080416 for the region
;	 specified in TRAJ_LIMIT.
;
; MODIFICATION HISTORY:
;        jaf, 16 Jul 2009: VERSION 1.00
;			   Copied from plot_traj_509.pro (lzh) and
;			   extensively edited.
;        jaf, 19 Jan 2010: Added DESCEND keyword to only plot trajectories
;			   that descend below 800 hPa
;
;-
; Copyright (C) 2009, Jenny Fisher, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to jaf@io.as.harvard.edu
; with subject "IDL routine traject_plot"
;-----------------------------------------------------------------------

pro traject_plot, Date, Back=Back, Frwd=Frwd, LatMin=LatMin, $
                  Descend=Descend, Save=Save
 
;================================================
; Get Defaults
;================================================
if ( N_Elements( Date ) eq 0 ) then Message, 'Must pass DATE!'
if ( Keyword_Set( Back ) and Keyword_Set( Frwd ) ) then $
        Message, 'Must choose forward OR backward trajectories'
if ( ~Keyword_Set( Back ) and ~Keyword_Set( Frwd ) ) then $
        Message, 'Must choose forward OR backward trajectories'
 
minpres = 200
maxpres = 1000
 
;================================================
; Get Grid Parameters
;================================================
if n_elements(LatMin) eq 0 then LatMin = 40
 
;model type ( GEOS-5, 2x2.5 resolution ) and corresponding grids
ModelInfo = ctm_type('GEOS5_47L', res=2)
GridInfo  = ctm_grid( ModelInfo )
 
XMid  = GridInfo.XMid         ; Array of longitude centers
YMid  = GridInfo.YMid         ; Array of latitude centers
YMid = YMid[where(YMid ge LatMin)]
 
XX  = n_elements(XMid)
YY  = n_elements(YMid)
 
;================================================
; Get DC-8 flight tracks
;================================================
DC8_file = MFindFile(!ARCTAS + $
    '/IDL/flight/DC8/*'+string(Date,'(i8.0)')+'_ra.ict')
Get_DC8, DC8_file, DC8_Long = dc8lon, DC8_Lat = dc8lat
dc8lon = dc8lon[1:*] & dc8lat = dc8lat[1:*]
dc8lon[where(dc8lon lt 0)] = dc8lon[where(dc8lon lt 0)] + 360.
close,/all
;================================================
; Get trajectories
;================================================
 
If Keyword_Set( Back ) then $
  TrajFile = !ARCTAS+'/field_data/DC8/trajectory/' + $
            'DC8-Flt-BACK_TRAJECTORY_'+String(Date,'(i8.0)')+'.sav'
 
If Keyword_Set( Frwd ) then $
  TrajFile = !ARCTAS+'/field_data/DC8/trajectory/' + $
            'DC8-Flt-FRWD_TRAJECTORY_'+String(Date,'(i8.0)')+'.sav'
 
Restore, TrajFile
 
; Set up region of interest for trajectories
Traj_Limit, Date, Lat, Lon, Pres
 
if Keyword_Set( Back ) then begin
   TrajLon = DC8_Back_traj.Lon[*,0]
   TrajLat = DC8_Back_traj.Lat[*,0]
   TrajPres = DC8_Back_traj.Pres[*,0]
endif else begin
   TrajLon = DC8_Frwd_traj.Lon[*,0]
   TrajLat = DC8_Frwd_traj.Lat[*,0]
   TrajPres = DC8_Frwd_traj.Pres[*,0]
endelse
 
Index = where(  (TrajLat  ge Lat[0])  and (TrajLat  le Lat[1]) and $
		(TrajLon  ge Lon[0])  and (TrajLon  le Lon[1]) and $
		(TrajPres ge Pres[0]) and (TrajPres le Pres[1]) ) 
 
nPts = n_elements(Index)
 
; define arrays
lon_traj  =  fltarr(nPts,250)
lat_traj  =  fltarr(nPts,250)
pres_traj =  fltarr(nPts,250)
 
for i = 0L, nPts-1L do begin
   if Keyword_Set( Back ) then begin
       tmp_lon = reform( DC8_Back_Traj.lon[ Index[i],* ] ) 
       tmp_lat = reform( DC8_Back_Traj.lat[ Index[i],* ] ) 
       tmp_pres = reform( DC8_Back_Traj.pres[ Index[i],* ] ) 
   endif else begin
       tmp_lon = reform( DC8_Frwd_Traj.lon[ Index[i],* ] )
       tmp_lat = reform( DC8_Frwd_Traj.lat[ Index[i],* ] )
       tmp_pres = reform( DC8_Frwd_Traj.pres[ Index[i],* ] )
   endelse
 
    lon_traj[i,*] = tmp_lon
    lat_traj[i,*] = tmp_lat
    pres_traj[i,*] = tmp_pres
endfor
 
nodata = where(lon_traj eq -9999.)
lon_traj[nodata]=!Values.F_NAN
lat_traj[nodata]=!Values.F_NAN
pres_traj[nodata]=!Values.F_NAN
 
;========================================
; Plot Set-Up
;========================================
SDate = String(Date,'(i8.0)')
if Keyword_Set(Back) then direction='back' else direction='frwd'
Title = 'FSU '+direction+' trajectories, '+SDate

if Keyword_Set(Save) then begin
   filename=!ARCTAS+'/IDL/analysis/'+direction+'_traj_'+SDate+'.ps'
   multipanel,rows=2,cols=1
   !p.font=0
   open_device, /ps,/color,bits=8,/portrait,filename=filename
endif else begin
   multipanel, /off & window, 0
endelse
 
;first set up blank map region
myct, /WhGrYlRd
TVMap,fltarr(XX,YY),XMid,YMid,/NoData,/isotropic,/continents,$
      /grid,/coasts,/polar,/nogylabels,mparam=[90,250,0],/noadvance, $
      Title = Title
 
; overplot the flight tracks
OPlot, dc8lon, dc8lat, color=13, thick=2
 
; plot the trajectories
myct, 33, ncolors=24
 
for i = 0L, nPts-1L do begin
 
  tmp_lon  = reform( lon_traj[ i,* ] )
  tmp_lat  = reform( lat_traj[ i,* ] )
  tmp_pres = reform( pres_traj[ i,* ] )

  if Keyword_Set(Descend) then MinP = 800 else MinP = 0

  if max(tmp_pres) ge MinP then begin
 
  ind      = where( finite(tmp_lon) and (tmp_lat ge LatMin) )
  tmp_lon  = tmp_lon[ind]
  tmp_lat  = tmp_lat[ind]
  tmp_pres = tmp_pres[ind]
 
  scatterplot_datacolor,tmp_lon,tmp_lat,tmp_pres,/overplot, /nocb, $
	zmin=minpres,zmax=maxpres,/xstyle,/ystyle,symsize=0.3

  endif
 
endfor

; Reverse the direction of the colorbar so low altitude is at the bottom
myct,33,ncolors=24,/reverse

; We also have to specify the labels, since otherwise it will print 200
; where we have really plotted 1000.
Colorbar, position=[0.80,0.20,0.82,0.80],divisions=5,/vertical,$
	  Unit='[hPa]',min=minpres,max=maxpres,color=1,charsize=1.1,$
	  Annotation=['1000','800','600','400','200']
 
; emphasize the locations
;oplot,TrajLon[Index],TrajLat[Index],color=1,psym=sym(12), symsize=0.5
 
if Keyword_Set( Save ) then close_device

End
