; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        AVG_GEOSGRID
;
; PURPOSE:
;        Average data into GEOS-Chem grid boxes and into time steps.
;        This procedure accepts either a PLANE structure or several
;        arrays and returns either another PLANE structure or several
;        other arrays. The resulting average is the same regardless of
;        method. 
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        AVG_GEOSGRID [, Keywords]
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;        GRIDTYPE - CTM type, as given by CTM_TYPE
;        TIMESTEP - Duration of averaging interval in minutes,
;                   should be the same as the GEOS-Chem dynamcal timestep
;        PLANEIN  - A planeflight structure, as given by 
;                   PLANELOG2FLIGHTMERGE, must contain tags for LAT,
;                   LON, PRES, DOY, and observations with any name
;        LAT, LON, PRS, DOY, OBS - Arrays of latitude, longitude,
;                                  pressure, day of year, and
;                                  observations. Must all have the
;                                  same size. None are needed if
;                                  using PLANEIN.
;
; KEYWORD OUTPUTS:
;        PLANE_GEOS - contains a PLANE structure averaged from
;                     PLANEIN. Used only if PLANEIN is passed.
;        LAT_GEOS, LON_GEOS, PRS_GEOS, DOY_GEOS, OBS_GEOS - Arrays of
;                     data averaged on GEOS-Chem grid
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE 1:
;        ; Create PLANE structure with data from a GEOS-Chem Plane.log file
;        PLANELOG2FLIGHTMERGE, 'DC8', 'Plane.log.20060304', GC=PLANE
;
;        ; Average all observations in PLANE structure
;        AVG_GEOSGRID, GRIDTYPE=CTM_TYPE('GEOS4',RES=2), TimeStep=15,
;        PlaneIn=Plane, Plane_GEOS=Plane_GEOS
;
; EXAMPLE 2:
;        ; Data can come from anywhere, but must be a 1D array
;        AVG_GEOSGRID, GRIDTYPE=CTM_TYPE('GEOS4',RES=2), TimeStep=15,
;        LAT=LAT, LON=LON, PRS=PRS, DOY=DOY, OBS=OBS$
;        LAT_GEOS=NEWLAT, LON_GEOS=NEWLON, PRS_GEOS=NEWPRS,
;        DOY_GEOS=NEWDOY, OBS_GEOS=NEWOBS
;
; MODIFICATION HISTORY:
;        cdh, 30 Aug 2007: VERSION 1.00 
;                          based loosely on a procedue with the same
;                          name written by rch and dbm, but this is
;                          ~500X faster. Should be compatible with
;                          old function calls.
;
;-
; Copyright (C) 2007, Christopher Holmes, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine avg_geosgrid"
;-----------------------------------------------------------------------

PRO avg_geosgrid, GridType=GridType,     $
                  PlaneIn=PlaneIn,       $
                  Plane_geos=Plane_geos, $
                  TimeStep=TimeStep, $
                  Lat=Lat,           $
                  Lon=Lon,           $
                  Prs=Prs,           $
                  DOY=DOY,           $
                  Obs=Obs,           $
                  Lat_geos=Lat_geos, $
                  Lon_geos=Lon_geos, $
                  Prs_geos=Prs_geos, $
                  DOY_geos=DOY_geos, $
                  Obs_geos=Obs_geos, $
                  ICTformat=ICTformat
 
   print, ' Average observations over GEOS grid...'
 
   ;==============================================================
   ; Check Inputs
   ;   Must either use PlaneIn structure or 
   ;   All of: Lat, Lon, Prs, DOY, Obs
   ;==============================================================
 
   ; Initialize
   OK = 0L
 
   ; Input OK if PlaneIn is used, but we need to switch to define the 
   ; variables that we will use to average
   if Keyword_Set( PlaneIn ) then begin
       OK = 1L
       If Keyword_Set( ICTformat ) then begin
           Lat = PlaneIn.Latitude
           Lon = PlaneIn.Longitude
           Prs = PlaneIn.Pressure
           DOY = PlaneIn.JDay + PlaneIn.UTC / 3600. / 24.
       endif else begin
           Lat = PlaneIn.Lat
           Lon = PlaneIn.Lon
           Prs = PlaneIn.Pres
           DOY = PlaneIn.DOY
       endelse
   endif
 
   if ( Keyword_Set( Lat ) and $
        Keyword_Set( Lon ) and $
        Keyword_Set( Prs ) and $
        Keyword_Set( DOY ) and $
        Keyword_Set( Obs ) ) then OK = 1L
   
   if (not OK) then message, 'Unacceptable inputs'
     
   ;==============================================================
   ; Set up GEOS-Chem grid, which defines areas to average
   ;==============================================================
 
   ; Get Grid info for specified grid type
   GridInfo = ctm_grid( GridType )
   nlon = GridInfo.IMX          ; Maximum I (longitude) dimension
   nlat = GridInfo.JMX          ; Maximum J (latitude) dimension)
   lonmid  = GridInfo.XMid      ; Array of longitude centers
   lonedge  = GridInfo.XEdge    ; Array of longitude edges
   latmid  = GridInfo.YMid      ; Array of latitude centers
   latedge  = GridInfo.YEdge    ; Array of latitude edges
   pmid = GridInfo.pmid         ; Array of mean pressures for sigma centers
   pedge = GridInfo.pedge       ; Array of mean pressures for sigma edges
   nlev = n_elements(pmid)      ; Number of vertical levels
 
   ;==============================================================
   ; We need to group observations into GEOS-Chem grid boxes
   ; for each TimeStep. 
   ; We could do that by looping over the lat, lon, alt, and time
   ; indices, but that is very time-consuming.
   ; Here is a faster way
   ;  1. Use INTERPOL to find the ctm index (lat,lon,lev) for each 
   ;     data point. This is also faster than using ctm_index and looping
   ;     over the whole array.
   ;  2. Calculate an integer for each interval of TimeStep minutes
   ;     Do this using rounding.
   ;  3. Create a unique integer for each time-lat-lon-lev 4-tuple
   ;     This is analogous to creating a unique date as YYYYMMDD.
   ;  4. Do averaging.
   ;==============================================================

   ; Find the index of the lat, lon, and level for each observation
   ctm_Lat_INDEX = round( interpol( indgen(nlat), latmid, lat ) )
   ctm_Lon_INDEX = round( interpol( indgen(nlon), lonmid, lon ) )
   ctm_Lev_INDEX = round( interpol( indgen(nlev), pmid,   prs ) )
   
   ; interpol extrapolates beyond the ends of the indgen array
   ; so we need to cap the INDEX
   ctm_Lat_INDEX = Long( ( ctm_Lat_INDEX > 0 ) < (nlat-1) )
   ctm_Lon_INDEX = Long( ( ctm_Lon_INDEX > 0 ) < (nlon-1) )
   ctm_Lev_INDEX = Long( ( ctm_Lev_INDEX > 0 ) < (nlev-1) )
 
   ; Now assign a unique integer to each time interval of
   ; width minutes
   ; Time_INDEX_full gives the number of TimeStep intervals that have 
   ; elapsed since the start of the year
   Time_INDEX_full = Long( Double( DOY ) * 24. * 60. / TimeStep )
 
   ; We can reduce the magnitude of Time_INDEX_full, by starting
   ; Time_INDEX at 0 for the first observation
   Time_INDEX = Time_INDEX_full - min( Time_INDEX_full )
   nTime = max( Time_INDEX )+1 
 
   ; Now construct a single number to describe the 4D grouping
   Group_INDEX = ( Time_INDEX * nlat * nlon * nlev ) + $
     ( ctm_Lat_INDEX * nlon * nlev ) + $
     ( ctm_Lon_INDEX * nlev ) + $
     ( ctm_Lev_INDEX ) 

;   ;==============================================================
;   ; Report the Lat, Lon, and Pressure as the center of a GEOS-Chem box
;   ;==============================================================
; 
;   ; Get the latitude index for the new Latitudes 
;   Lat_geos_INDEX = round( interpol( indgen(nlat), latmid, Lat_geos ) )
;   Lon_geos_INDEX = round( interpol( indgen(nlon), lonmid, Lon_geos ) )
;   Lev_geos_INDEX = round( interpol( indgen(nlev), pmid,   Prs_geos ) )
;   
;   ; interpol extrapolates beyond the ends of the indgen array
;   ; so we need to cap the INDEX
;   Lat_geos_INDEX = Long( ( Lat_geos_INDEX > 0 ) < (nlat-1) )
;   Lon_geos_INDEX = Long( ( Lon_geos_INDEX > 0 ) < (nlon-1) )
;   Lev_geos_INDEX = Long( ( Lev_geos_INDEX > 0 ) < (nlev-1) )
;
;   ; Get the lat, lon, and pressure for the center of each box
;   Lat_geos = latmid[ Lat_geos_INDEX ]
;   Lon_geos = lonmid[ Lon_geos_INDEX ]
;   Prs_geos = pmid[   Prs_geos_INDEX ]
;
;   ;==============================================================
;   ; Report the Time as the center of a TimeStep interval
;   ;   Time_INDEX_full gives the number of TimeStep intervals since YYYY0101
;   ;   Time_INDEX = 0 corresponds to the Jan 1
;   ;==============================================================
;
;   DOY_geos = ( Time_INDEX_full * TimeStep ) + ( TimeStep / 2. )


   ;==============================================================
   ; Average the observations using TAPPLY
   ;==============================================================
 
   ; Check whether we are using a plane structure
   IF Keyword_Set( PlaneIn ) THEN BEGIN
 
      ;==============================================================
      ; Average each tag over each grid box and timestep
      ;==============================================================
 
       ; Find tag names
       TagNames = tag_names( PlaneIn )
 
       ; Number of tags in the input
       nTags = n_tags( PlaneIn )
       
       ; Sorting order after averaging 
       sort_INDEX = sort( tapply( DOY, Group_INDEX, 'mean_nan', $ 
                                  /Double ) )

       ; Initialize
       FIRST = 1L
       
       ; Loop over structure tags
       FOR T=0L, nTags-1L DO BEGIN

          ; Calculate average values for this element of the structure
          Var = tapply( PlaneIn.(T), Group_INDEX, 'mean_nan', /Double )

          ; Add the variable to a new structure, preserving the same name
          IF (FIRST) THEN BEGIN
              FIRST = 0L
              Plane_geos = Create_Struct( TagNames[T], Var[sort_INDEX] )
          ENDIF ELSE BEGIN
              Plane_geos = Create_Struct( Plane_geos, TagNames[T], $
                                          Var[sort_INDEX] )
          ENDELSE

      ENDFOR
 
   ENDIF ELSE BEGIN
 
      ;==============================================================
      ; Average observations
      ;==============================================================
       
       Lat_geos = tapply( Lat, Group_INDEX, 'mean_nan', /Double )
       Lon_geos = tapply( Lon, Group_INDEX, 'mean_nan', /Double )
       Prs_geos = tapply( Prs, Group_INDEX, 'mean_nan', /Double )
       DOY_geos = tapply( DOY, Group_INDEX, 'mean_nan', /Double )
       Obs_geos = tapply( Obs, Group_INDEX, 'mean_nan', /Double )
 
   ENDELSE
 
END
