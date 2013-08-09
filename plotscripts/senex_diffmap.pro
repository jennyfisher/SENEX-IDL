; $Id: senex_diffmap.pro,v 1.7 2008/07/06 17:07:19 jaf Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        SENEX_DIFFMAP
;
; PURPOSE:
;        Plot for a given species measured on the SENEX of the difference
;        between the value observed by the aircraft and that modeled
;        by GEOS-Chem on a domain appropriate for the SENEX missions.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        SENEX_DIFFMAP,species_in,Platform[,Keywords]
;
; INPUTS:
;        species_in  - Name of species being mapped. e.g. 'CO', 'O3', 'ALTP'
;        Platform - Name of Aircraft. Current options: 'WP3D'
;
; KEYWORD PARAMETERS:
;        FlightDates - Dates (as takeoff dates) as 'YYYYMMDD'. Can also
;                      accept '*'
;        Alt         - Altitude range. Use this keyword to only plot
;                      data that falls within this range of altitudes.
;        MinData     - Minimum value to use when plotting species.
;        MaxData     - Maximum value to use when plotting species.
;        Model       - If set, modeled values will be plotted rather
;                      that observed data
;	 Save        - If set, map will be saved as a postscript rather
;		       than plotted on the screen
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        None
;
; REQUIREMENTS:
;        GET_MODEL_DATA_SENEX
;        GET_FIELD_DATA_SENEX
;        SENEX_MAP
;
; NOTES:
;    (1) This routine will not work for dates outside of the SENEX dates.
;        Modify this part of the code to use for other campaigns.
;    (2) If data is not found for the given species / platform / date
;        combination, this routine will return to the calling program.
;
; EXAMPLE:
;    SENEX_DIFFMAP,'CO','WP3D',FlightDates='*'
;
;    Plots difference in CO along the WP3D flighttrack for all flights.
;
; MODIFICATION HISTORY:
;        cdh & jaf, 14 May 2008: VERSION 1.00
;        jaf,        6 Jul 2008: Added lines to plot all Fairbanks or
;                                Cold Lake flights
;	 jaf,	    27 Aug 2008: Added SAVE keyword
;        lei,       29 Jul 2013: Updated for SENEX
;-
; Copyright (C) 2008, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine senex_diffmap"
;-----------------------------------------------------------------------

pro senex_diffmap, species_in, platform, flightdates=flightdates, pd=pd,$
alts=alts,mindata=mindata,maxdata=maxdata,save=save,$
_extra=_extra
 
; Set defaults, and alert user to choices being made.
; Default is to plot CO observations from the DC8 for all dates at ll
; pressures.
if N_Elements(species_in) eq 0 then begin
	species_in='CO'
	print,'You didn''t specify a species, so CO is being plotted!'
endif 
if N_Elements(platform) eq 0 then begin
	platform='WP3D'
	print, 'You didn''t specify a platform, so WP3D is being plotted!'
endif
if N_Elements(flightdates) eq 0 then begin
	flightdates='2013*'
	print, 'You didn''t specify flightdates, so all dates are being plotted!'
endif
;if N_Elements(press) eq 0 then begin
;	press=[0, 1100]
;	print, 'You didn''t specify pressure, so all pressures are being plotted!'
;endif
if N_Elements(alts) eq 0 then begin
	alts=[0, 12]
	print, 'You didn''t specify altitude, so all altitudes are being plotted!'
endif

species=[0]
altp=[0]
lat=[0]
lon=[0]
doy=[0]
species_mod=[0]
doy_mod=[0]

for i = 0, n_elements(flightdates)-1 do begin
   species_tmp = get_field_data_senex(species_in,platform,flightdates[i], $
                                      _extra = _extra)

   if  finite(mean_nan(species_tmp),/nan) then begin 
        print, '********* no data for: ', flightdates[i]
        goto, nodata
   endif

   species = [species, species_tmp]

   altp = [altp,get_field_data_senex('altp',platform,flightdates[i], $
                                    _extra = _extra )]
   lat =  [lat,get_field_data_senex('lat',platform,flightdates[i], $
                                    _extra = _extra)]
   lon =  [lon,get_field_data_senex('lon',platform,flightdates[i], $
                                    _extra = _extra)]
   doy =  [doy,get_field_data_senex('doy',platform,flightdates[i], $
                                   _extra = _extra )]

   species_mod = [species_mod, $
        get_model_data_senex(species_in,platform,flightdates[i], $
                             _extra = _extra)]
   doy_mod = [doy_mod, $
        get_model_data_senex('DOY',platform,flightdates[i], $
                            _extra = _extra )]

nodata:
endfor

species  = species[1:*]
altp = altp[1:*]
lat  = lat[1:*]
lon  = lon[1:*]
doy  = doy[1:*]
species_mod = species_mod[1:*]
doy_mod = doy_mod[1:*]

species_mod = interpol( species_mod, doy_mod, doy )

; Select the data that is within the correct alt range. 
index = where( altp gt min(alts) and altp lt max(alts) )
if index[0] ge 0 then begin
species = species[index]
species_mod = species_mod[index]
lat = lat[index]
lon = lon[index]
endif

if keyword_set(pd) then begin
   diff = 100*(species_mod - species)/species
   unit='%'
endif else begin
   diff = species_mod-species
   if (n_elements(unit) eq 0) then unit=''
endelse

; If MinData and/or MaxData are not specified, use the min and max
; of the actual data 
if N_elements(mindata) eq 0 then mindata=min(diff,/nan)
if N_elements(maxdata) eq 0 then maxdata=max(diff,/nan)

; Set title 
title='Modeled '+strupcase(Species_in)+' - Observed '+$
      strupcase(Species_in)+' on '+strupcase(Platform)+' track'

; Set up plot
if keyword_set(save) then begin
	save_dir=!SENEX+'/IDL/analysis/'
	filename=save_dir+$
		'obs_mod_'+platform+'_'+species_in+'_diffmap_'+flightdates+'.ps'
     	multipanel,rows=2,cols=1
     	open_device, /ps, filename=filename, Bits=8, $
		WinParam = [0, 300,400], /color, /portrait
     	!p.font = 0 
endif
 
; Plot the data on a polar orthographic projection of the N. America sector
; For SENEX, no projection is used
senex_map, lon, lat, diff, latmin=latmin, zmin=mindata, zmax=maxdata, $
            title=title, unit=unit,/diff,_extra=_extra
 
if keyword_set(save) then close_device

end
