; $Id: make_profiles.pro,v 1.6 2008/07/07 17:10:42 jaf Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        MAKE_PROFILES
;
; PURPOSE:
;        Wrapper routine for making profiles of various species during the
;        SENEX field campaign.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        MAKE_PROFILES, Species, Platform[, Keywords]
;
; INPUTS:
;        Species  - Name of species being mapped. e.g. 'CO', 'O3', 'ALTP'
;        Platform - Name of Aircraft. Current options: 'WP3D'
;
; KEYWORD PARAMETERS:
;        FlightDates - Dates (as takeoff dates) as 'YYYYMMDD'. Can also
;                      accept '*'.
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        None
;
; REQUIREMENTS:
;        SENEX_OBS_MOD_PROFILES
;
; NOTES:
;
; EXAMPLE:
;        SENEX_OBS_MOD_PROFILES,'CO','WP3D',Flightdates='*'
;
;        Plots profiles of CO for all flights
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
;-----------------------------------------------------------------------

pro make_profiles,species,platform,flightdates=flightdates,_extra=_extra

if N_Elements(species) eq 0 then begin
        species='CO'
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

species = strupcase(species)

CASE species of
    'CO' : begin
	Unit = 'ppbv'
    end
    'SO4' : begin
	Unit = 'ppt'
    end
    'SO2' : begin
	Unit = 'ppbv'
    end
    'O3' : begin
	Unit = 'ppbv'
    end
    else:
ENDCASE

senex_obs_mod_profiles,species,platform,flightdates=flightdates,mindata=mindata,maxdata=maxdata,unit=unit,_extra=_extra

end
