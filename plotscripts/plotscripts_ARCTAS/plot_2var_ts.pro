; $Id: plot_2var_ts.pro,v 1.3 2008/07/06 20:37:18 jaf Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        PLOT_2VAR_TS
;
; PURPOSE:
;        This program plots time series of two variables against the
;        day of year. (double-y plot) This plot routine is customized
;        for ARCTAS to quickly plot model or observations along a
;        flight track. It is ideal for plotting concentrations and altitude.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        PLOT_2VAR_TS, Var1, Platform1[, FlightDates, Var2, Platform2,
;        Keywords] 
;
; INPUTS:
;        VAR1      - Name of a variable on one of the ARCTAS
;                    measurement platforms, or in the GEOS-Chem NRT.
;                    e.g. 'CO', 'AOD500', 'ALTP'
;        PLATFORM1 - Name of the Platform from which VAR1 was
;                    measured, i.e. 'DC8', 'P3B'
;  
;        FLIGHTDATES - (optional) dates for which VAR1 is
;                      desired. Default is all flights. 
;        VAR2      - (optional) second variable, plots on left
;                    axis. Default is altitude.
;        PLATFORM2 - platform for VAR2
;
; KEYWORD PARAMETERS:
;        MODEL     - Plot data from GEOS-Chem NRT
;
;        Other keywords passed to PLOT, AXIS
;
; OUTPUTS:
;
; SUBROUTINES:
;
; REQUIREMENTS:
;        GET_FIELD_DATA_ARCTAS - and associated data directories
;        GET_MODEL_DATA_ARCTAS - and associated data directories
;
; NOTES:
;
; EXAMPLE:
;        ; Simplest usage, plots CO and Altitude for all DC8 flights
;        PLOT_2VAR_TS, 'CO', 'DC8', '20080401'
;
;        ; GEOS-Chem simulation of the previous observations
;        PLOT_2VAR_TS, 'CO', 'DC8', '20080401', /Model
;
;        ; We can compare Observations and Model with altitude:
;        PLOT_2Var_TS, 'CO', 'DC8', '20080401'
;        co_mod  = GET_MODEL_DATA_ARCTAS( 'CO',  'DC8', '20080401')
;        doy_mod = GET_MODEL_DATA_ARCTAS( 'DOY', 'DC8', '20080401')
;        OPLOT, DOY_MOD, CO_MOD, COLOR=2
;
; MODIFICATION HISTORY:
;        cdh, 13 Apr 2008: VERSION 1.00
;        jaf, 06 Jul 2008: Fixed bug that occurred if called with
;                          keyword Model (model stores altitude as
;                          ALT, not ALTP
;
;-
; Copyright (C) 2008, Christopher Holmes, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine plot_2var_ts"
;-----------------------------------------------------------------------


pro plot_2var_ts, Var1, Platform1, FlightDates, $
         Var2, Platform2, Model=Model, _Extra=_Extra
 
  ; Default to use altitude as the second variable 
  If not Keyword_Set( Var2 ) and                   $
     not Keyword_set( Model ) then Var2 = 'ALTP'
  If not Keyword_Set( Var2 ) and                   $
         Keyword_set( Model ) then Var2 = 'ALT'
    
  ; Default to plot all flights
  If not Keyword_Set( FlightDates ) then FlightDates = '*'

  ; Default to use the same platform for both time series
  If not Keyword_Set( Platform2    ) then Platform2    = Platform1 
 
  ; Default to read aircraft observations
  If Keyword_Set( Model ) then GetData = 'get_model_data_arctas' $
    else GetData = 'get_field_data_arctas'
 
  ; Open the data
  Data1 = Call_Function( GetData, Var1, Platform1, FlightDates, _extra=_extra )
  Data2 = Call_Function( GetData, Var2, Platform2, FlightDates, _extra=_extra )
 
  ; Open the time coordinate
  Time1 = Call_Function( GetData, 'DOY', Platform1, FlightDates, _extra=_extra )
  Time2 = Call_Function( GetData, 'DOY', Platform2, FlightDates, _extra=_extra )
 
  ; Set up the plot without plotting any data
  plot, Time1, Data1, ystyle=4, xstyle=9, xtitle='Day of year', /noData, $
    xrange=[min(Time1, max=mx, /NaN), mx], _Extra=_Extra
 
  ; Right y-axis
  axis, /yaxis, yrange=[min(Data2, max=mx, /NaN), mx], /save, ytitle=Var2, $
    _Extra=_Extra
 
  ; Data for right y-axis
  oplot, Time2, Data2, _Extra=_Extra 
  
  ; Left y-axis
  axis, yaxis=0, yrange=[min(Data1, max=mx,/NaN), 10*floor(mx/10)+70], /save, $
    ytitle=Var1, color=4, _Extra=_Extra,/ystyle

  ; Data for left y-axis
  oplot, Time1, Data1, color=4, _Extra=_Extra
 
end
 
