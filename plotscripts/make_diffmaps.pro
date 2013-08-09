; $Id: make_diffmaps.pro,v 1.6 2008/07/07 17:10:42 jaf Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        MAKE_DIFFMAPS
;
; PURPOSE:
;        Wrapper routine for making maps of obs-mod difference during the
;        SENEX field campaign.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        MAKE_DIFFMAPS, Species, Platform[, Keywords]
;
; INPUTS:
;        Species  - Name of species being mapped. e.g. 'CO', 'O3', 'ALTP'
;        Platform - Name of Aircraft. Current options: 'WP3D'
;
; KEYWORD PARAMETERS:
;        FlightDates - Dates (as takeoff dates) as 'YYYYMMDD'. Can also
;                      accept '*'.
;	 PD - use percent difference
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        None
;
; REQUIREMENTS:
;        SENEX_DIFFMAP
;
; NOTES:
;
; EXAMPLE:
;        MAKE_DIFFMAPS,'CO','WP3D',Flightdates='*'
;
;        Plots difference in CO between obs & model for all flights
;
; MODIFICATION HISTORY:
;        jaf, 06 Jul 2008: VERSION 1.00
;        jaf, 08 Aug 2013: updated for SENEX
;
;-
; Copyright (C) 2008, Jenny Fisher, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to jaf@io.as.harvard.edu
; or with subject "IDL routine make_diffmaps"
;-----------------------------------------------------------------------


pro make_diffmaps,species,platform,flightdates=flightdates,pd=pd,_extra=_extra

; Set defaults, and alert user to choices being made.
; Default is to plot CO observations from the DC8 for all dates 
if N_Elements(species) eq 0 then begin
        species='CO'
        print,'You didn''t specify a species, so CO is being plotted!'
endif
if N_Elements(platform) eq 0 then begin
        platform='DC8'
        print, 'You didn''t specify a platform, so DC8 is being plotted!'
endif
if N_Elements(flightdates) eq 0 then begin
        flightdates='2013*'
        print, 'You didn''t specify flightdates, so all dates are being plotted!'
endif
 
species = strupcase(species)

; For frequently plotted species, specify the min/max and unit to be used. 
; Treat % diff differently
if (keyword_set(pd)) then begin
   mindata=-25
   maxdata=25
endif else begin
CASE species of
    'CO' : begin
	MinData = -50
	MaxData = 50 
	unit = 'ppbv'
    end
    'SO4' : begin
	MinData = -10
	MaxData = 10
	Unit = 'nmol/m3'
    end
    'SO2' : begin
	MinData = -.5
	MaxData = .5
	Unit = 'ppbv'
    end
    'ISOP' : begin
	MinData = -2d3 
	MaxData = 2d3
	Unit = 'ppt'
    end
    else:
ENDCASE
endelse

; Plot the data over the US
senex_diffmap,species,platform,flightdates=flightdates,mindata=mindata,$
                  maxdata=maxdata,unit=unit,_extra=_extra
 
end
