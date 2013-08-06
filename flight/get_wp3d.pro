; $Id: get_wp3d.pro,v 1 2013/07/02 20:23:13 lei Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        GET_WP3D
;
; PURPOSE:
;        Reads plane flight file(s) from the WP-3D results and
;        returns the lon, lat, pressure, date, and time to the 
;        calling program as arrays.
;
;        This routine is designed to be called by MAKE_PLANEFLIGHT.
;
; CATEGORY:
;        GEOS-Chem Planeflight Diagnostic
;
; CALLING SEQUENCE:
;        GET_WP3D, FILENAMES [, Keywords ]
;
; INPUTS:
;        FILENAMES -> A scalar or vector containing the names of the
;             WP3D flight track files to read.  
;
; KEYWORD PARAMETERS:
;        WP3D_NUM   -> Use this to specify the flight number manually.
;        WP3DFLT    -> Returns an array of flight ID's  to the calling program.
;        WP3D_LONG  -> Returns an array of longitudes   to the calling program.
;        WP3D_LAT   -> Returns an array of latitudes    to the calling program.
;        WP3D_PRS   -> Returns an array of pressures    to the calling program. 
;        WP3D_MONTH -> Returns an array of months       to the calling program. 
;        WP3D_DAY   -> Returns an array of days         to the calling program. 
;        WP3D_YEAR  -> Returns an array of years        to the calling program. 
;        WP3D_HOUR  -> Returns an array of hours        to the calling program. 
;        WP3D_MIN   -> Returns an array of minutes      to the calling program. 
;        WP3D_JD    -> Returns an array of Julian Dates to the calling program. 
;        
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        ==============================
;        OPEN_FILE   STRBREAK (function)
;
; REQUIREMENTS:
;        Requires routines from GAMAP package.
;
; NOTES:
;        Arrays of structures were avoided since these seem to suck up
;        an inordinate amount of IDL memory.  We just pass the individual
;        arrays for each quantity back to the calling program.
;
; EXAMPLE:
;        GET_WP3D, 'AircraftPos_NP3_20130531_RA.ict', $
;             WP3D_FLT=WP3D_FLT,   WP3D_LONG=WP3D_LONG,   WP3D_LAT=WP3D_LAT, $  
;             WP3D_PRS=WP3D_PRS,   WP3D_MONTH=WP3D_MONTH, WP3D_DAY=WP3D_DAY, $    
;             WP3D_YEAR=WP3D_YEAR, WP3D_HOUR=DC8_HOUR,    WP3D_MIN=DC8_MIN,  $   
;             WP3D_JD=WP3D_JD    
;
;             ; Returns information contained in the WP3D data 
;             ; file "AircraftPos_NP3_20130531_RA.ict" to the calling program.
;
; MODIFICATION HISTORY:
;        lei, 02 Jul 2013: VERSION 1.00. Based on get_dc8.pro by bmy
;-----------------------------------------------------------------------


function WP3D_LineSplit, Line
   
   ;====================================================================
   ; Function WP3D_LINESPLIT splits a line by spaces or horizontal tabs.
   ;====================================================================

   ; First try to break the line by spaces ...
   Result = StrBreak( Line, ' ' )

   ; ... then if that doesn't work, by tabs
   if ( N_Elements( Result ) eq 1 ) $
      then Result = StrBreak( Line, BYTE(9B) )

   ; ... then try by comma
   ; doesn't work, need to fix later on
   ;if ( N_Elements( Result ) eq 1 ) $
   ;   then Result = StrBreak( Line, ', ' )
 
   ; Return success or stop w/ error
   if ( N_Elements( Result ) gt 0 ) then begin
      return, Result 
   endif else begin
      S = 'Could not split line by spaces, tabs or commas!'
      Message, S
   endelse
end 

;------------------------------------------------------------------------------

pro Get_WP3D, FileNames,  $
             WP3D_Flt=FIdArr,   WP3D_Long=LongArr,   WP3D_Lat=LatiArr,  $
             WP3D_Prs=PresArr,  WP3D_Month=MonthArr, WP3D_Day=DayArr,   $
             WP3D_Year=YearArr, WP3D_Hour=HourArr,   WP3D_Min=MinArr,   $
             WP3D_JD=JDarr,     WP3D_Num=FltNum

   ;====================================================================
   ; Initialization
   ;====================================================================

   ; External functions
   FORWARD_FUNCTION File_Exist, StrBreak

   ; Keywords
   if ( N_Elements( FileNames   ) eq 0 ) then Message, 'Need to pass FILENAME!'
   if ( N_Elements( FltNum      ) eq 0 ) then FltNum      = '  '  
   if ( N_Elements( HdrFile     ) eq 0 ) then HdrFile     = 'Planeflight.hdr'
   if ( N_Elements( OutFileName ) eq 0 ) then OutFileName = 'Planeflight.dat' 

   ; Initialize variables
   Time        = 0D
   Line        = ''
   Count       = 0L
 
   ; Define arrays at 1-second resolution 
   ; (use 2-bit integers where necessary)
   SEC_PER_DAY = 86400L * 2L
   FIdArr1     = StrArr( SEC_PER_DAY  )
   LongArr1    = FltArr( SEC_PER_DAY  )
   LatiArr1    = FltArr( SEC_PER_DAY  )
   PresArr1    = FltArr( SEC_PER_DAY  )
   MonthArr1   = IntArr( SEC_PER_DAY  )
   DayArr1     = IntArr( SEC_PER_DAY  )
   YearArr1    = IntArr( SEC_PER_DAY  )
   HourArr1    = IntArr( SEC_PER_DAY  )
   MinArr1     = IntArr( SEC_PER_DAY  )
   JDArr1      = DblArr( SEC_PER_DAY  )
 
   ; Define arrays at 1-minute resolution
   MIN_PER_DAY = 1440L * 2L
   FIdArr      = StrArr( MIN_PER_DAY )
   LongArr     = FltArr( MIN_PER_DAY )
   LatiArr     = FltArr( MIN_PER_DAY )
   PresArr     = FltArr( MIN_PER_DAY )
   MonthArr    = IntArr( MIN_PER_DAY )
   DayArr      = IntArr( MIN_PER_DAY )
   YearArr     = IntArr( MIN_PER_DAY )
   HourArr     = IntArr( MIN_PER_DAY )
   MinArr      = IntArr( MIN_PER_DAY )
   JDArr       = DblArr( MIN_PER_DAY )

   ;====================================================================
   ; Get information from all flighttrack files for a given day
   ;====================================================================
 
   ; Loop over number of flight files
   for F = 0L, N_Elements( FileNames )-1L do begin
 
      ; Skip files that don't exist
      if ( not File_Exist( FileNames[F] ) ) then goto, Next_File
      
      ;=================================================================
      ; Open this flight track file and skip over header
      ;=================================================================
 
      ; Open file
      Open_File, FileNames[F], Ilun_Flt, /Get_LUN
 
      ; Read first line
      ReadF, Ilun_Flt, Line

      ; Split line by spaces or tabs
      Result = WP3D_LineSplit( Line )
 
      ; Get # of lines in the header
      N_Hdr = Long( Result[0] )
 
      ; Skip header 
      for N = 0L, N_Hdr-2L do begin
 
         ; Read each lines
         ReadF, Ilun_Flt, Line

         ;------------------------
         ; Get the starting date 
         ;------------------------
         if ( N eq 5 ) then begin

            ; Split the line
            Result = WP3D_LineSplit( Line )

            ; Save into variables
            Year   = Long( Result[0] )
            Month  = Long( Result[1] )
            Day    = Long( Result[2] )

            ; Julian day at 0 UTC on this date
            JDToday = Julday( Month, Day, Year, 0, 0, 0 ) 

            ; Undefine
            Undefine, Result
         endif

         ;-------------------------------
         ; Parse line for airplane type
         ;-------------------------------
         if ( StrPos( Line, 'PLATFORM' ) ge 0 ) then begin
            if ( StrPos( Line, 'DC-8' ) ge 0 ) then Plane = 'DC8'
            if ( StrPos( Line, 'P3'   ) ge 0 ) then Plane = 'P3 ' 
            if ( StrPos( Line, 'WP-3D') ge 0 ) then Plane = 'WP3D'
	 endif
 
         ; Parse line for flight number 
         if ( StrPos( Line, 'This is Flight' ) ge 0 ) then begin 

            ; Split line
            Result = WP3D_LineSplit( Line )

            ; Strip '#' from line (if present)
            if ( StrPos( Result[3], '#' ) ge 0 ) then begin
               Result[3] = StrMid( Result[3], 1, StrLen( Result[3] )-1L )
            endif

            ; Save flight number
            FltNum = Fix( Result[3] )           
         endif

         ;--------------------------------------------------
         ; Parse line for flight number (alternate string)
         ;--------------------------------------------------
         if ( StrPos( Line, 'This is Test Flight' ) ge 0 ) then begin 

            ; Split the line
            Result = WP3D_LineSplit( Line )

            ; Strip '#' from line (if present)
            if ( StrPos( Result[4], '#' ) ge 0 ) then begin
               Result[4] = StrMid( Result[4], 1, StrLen( Result[4] )-1L )
            endif

            ; Save flight number
            FltNum = Fix( Result[4] )           
         endif

         ;-------------------------------------------------------
         ; Parse the line w/ the data field names (bmy, 3/6/06)
         ;-------------------------------------------------------
         if ( N eq N_Hdr-2L ) then begin

            ; Split the line into sepa
            Result  = WP3D_LineSplit( Line )
  
            ; Find the columns for TIME, LAT, LON, and GPS Altitude (m)
            IndTime = Where( Result eq 'AOCTimewave'    )
            IndLat  = Where( Result eq 'GpsLat'    )
            IndLon  = Where( Result eq 'GpsLon'    )
            IndAlt  = Where( Result eq 'GpsAlt' )

            ; Error check
            if ( IndTime[0] lt 0 ) then Message, 'Time not found!'
            if ( IndLat[0]  lt 0 ) then Message, 'LAT not found!'
            if ( IndLon[0]  lt 0 ) then Message, 'LON not found!'
            if ( IndAlt[0]  lt 0 ) then Message, 'GpsAlt not found!'

            ; Undefine 
            UnDefine, Result
         endif
      endfor

      ; Create Flight ID
      if ( StrLen( StrTrim( FltNum, 2 ) ) gt 0 )                $
         then FltId = Plane + String( FltNum, Format='(i2.2)' ) $
         else FltId = Plane + FltNum

      ;====================================================================
      ; Read lat, lon, and time from flight track file into arrays
      ;====================================================================
 
      ; Loop thru file
      while ( not EOF( Ilun_Flt ) ) do begin
 
         ; Read data from each line
         ReadF, Ilun_Flt, Line

         ; Split line by spaces or tabs
         Result = WP3D_LineSplit( Line )
 
         ; Seconds after midnight
         Sec = Double( Result[IndTime] )

         ; Latitude
         Lat = Float( Result[IndLat] )
      
         ; Longitude
         Lon = Float( Result[IndLon] )
 
         ; Get static pressure at the given altitude
         ; corresponding to the US Standard Atmosphere
         ; Need to convert m to km
         Prs = Float( USSA_PRESS ( Result[IndAlt] / 1000D ) ) 
 
         ; Get Julian Date at this time
         JdNew = JdToday + ( Sec / 86400D )
 
         ; Convert back into M/D/Y H:Mi:S
         CalDat, JdNew, M, D, Y, H, Mi, S
 
         ; Store quantities into arrays
         FIdArr1  [Count] = FltId
         LongArr1 [Count] = Lon
         LatiArr1 [Count] = Lat
         PresArr1 [Count] = Prs
         MonthArr1[Count] = M
         DayArr1  [Count] = D
         YearArr1 [Count] = Y
         HourArr1 [Count] = H
         MinArr1  [Count] = Mi
         JDArr1   [Count] = JdNew
         
         ; Increment Count
         Count = Count + 1L
 
      endwhile
 
      ; Close the input file
      Close,    Ilun_Flt
      Free_LUN, Ilun_Flt
 
Next_File:
   endfor
 
   ;=================================================================
   ; Resize 1-second resolution arrays
   ;=================================================================
   FIdArr1   = Temporary( FIdArr1  [0L:Count-1L] ) 
   LongArr1  = Temporary( LongArr1 [0L:Count-1L] )
   LatiArr1  = Temporary( LatiArr1 [0L:Count-1L] )
   PresArr1  = Temporary( PresArr1 [0L:Count-1L] )
   MonthArr1 = Temporary( MonthArr1[0L:Count-1L] )
   DayArr1   = Temporary( DayArr1  [0L:Count-1L] )
   YearArr1  = Temporary( YearArr1 [0L:Count-1L] )
   HourArr1  = Temporary( HourArr1 [0L:Count-1L] ) 
   MinArr1   = Temporary( MinArr1  [0L:Count-1L] )
   JDArr1    = Temporary( JDArr1   [0L:Count-1L] )

   ;=================================================================
   ; Create arrays at 1-minute resolution by averaging the 
   ; 1-second resolution data
   ;=================================================================

   ; Unique day numbers
   IndDay  = Uniq( DayArr1 )

   ; Count
   Count = 0L

   ; Loop over the number of unique days
   for D = 0L, N_Elements( IndDay )-1L do begin

      ; Get the current day
      ThisDay  = DayArr1[IndDay[D]]

      ; Loop over hours
      for H = 0L, 23L do begin

         ; Loop over minutes per hour
         for M = 0L, 59L do begin

            ; Get index of points for each minute of this hour
            ; NOTE: We need to check for missing values BEFORE 
            ; we do the 1-minute averaging (dbm, 05/16/05)
            Ind = Where( DayArr1  eq ThisDay AND    $
                         HourArr1 eq H       AND    $
			 MinArr1  eq M       AND    $
                         LongArr1 gt -999.0  AND    $
                         LatiArr1 gt -999.0  AND    $
                         PresArr1 gt -999.0, N_Pts )

            ; Cycle if not found
            if ( N_Pts eq 0 ) then goto, next
      
            ; Convert N_PTS to floating point
            Points          = Float( N_Pts )

            ; Take average of lon, lat, alt during that minute
            LongArr [Count] = Total( LongArr1[Ind] ) / Points  
            LatiArr [Count] = Total( LatiArr1[Ind] ) / Points  
            PresArr [Count] = Total( PresArr1[Ind] ) / Points  

            ; Get other quantities
            FIdArr  [Count] = FIdArr1[Ind[0]]
            MonthArr[Count] = MonthArr1[Ind[0]]
            DayArr  [Count] = DayArr1[Ind[0]]
            YearArr [Count] = YearArr1[Ind[0]]
            HourArr [Count] = H
            MinArr  [Count] = M
            JDArr   [Count] = JDArr1[Ind[0]]
  
            ; Increment count
            Count = Count + 1L
Next:
         endfor
      endfor
   endfor

   ;=================================================================
   ; Resize 1-minute resolution arrays
   ;=================================================================
   FIdArr   = Temporary( FIdArr  [0L:Count-1L] ) 
   LongArr  = Temporary( LongArr [0L:Count-1L] )
   LatiArr  = Temporary( LatiArr [0L:Count-1L] )
   PresArr  = Temporary( PresArr [0L:Count-1L] )
   MonthArr = Temporary( MonthArr[0L:Count-1L] )
   DayArr   = Temporary( DayArr  [0L:Count-1L] )
   YearArr  = Temporary( YearArr [0L:Count-1L] )
   HourArr  = Temporary( HourArr [0L:Count-1L] ) 
   MinArr   = Temporary( MinArr  [0L:Count-1L] )
   JDArr    = Temporary( JDArr   [0L:Count-1L] )

   return

end
