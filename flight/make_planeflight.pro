; $Id: make_planeflight.pro,v 1.9 2008/04/07 14:54:14 bmy Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        MAKE_PLANEFLIGHT
;
; PURPOSE:
;        Reads several NASA plane flight data files and extracts the
;        lon, lat, pressure, date, and time to a GEOS-CHEM 
;        "Planeflight.dat" style file for the ND40 diagnostic.
;
; CATEGORY:
;        GEOS-Chem Planeflight Diagnostic
;
; CALLING SEQUENCE:
;        MAKE_PLANEFLIGHT, DATE [, Keywords ]
;
; INPUTS:
;        DATE -> Date of year (in YYYYMMDD format) to process.
;
; KEYWORD PARAMETERS:
;        HDRFILE -> Name of the file which contains header information
;             (e.g. # of variables, etc) for the Planeflight.dat file.  
;             Default is "Planeflight.hdr".
;
;        OUTFILENAME -> Name of the output "Planeflight.dat"-style
;             file.  Default is "output/Planeflight.dat.YYYYMMDD". 
;
;        /UPLOAD -> If this is set, will upload the output file
;             "Planeflight.dat.YYYYMMDD" to the proper directory
;              for the NRT-ARCTAS simulation.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        ======================================
;        OPEN_FILE    STRBREAK (function)
;        GET_DC8      GET_P3B
;        GET_FCN      GET_BAE
;        GET_RHB      GET_WP3D      
;        FILE_EXIST (function)
;        DATE2YMD     ADD_DATE (function)
;
; REQUIREMENTS:
;        Requires routines from GAMAP package.
;
; NOTES:
;        None
;
; EXAMPLE:
;        MAKE_PLANEFLIGHT, 20040526, /UPLOAD
;
;             ; Creates a "Planeflight.dat" file for 2004/05/26
;             ; and uploads it to the proper directory on
;             ; the data server geos.as.harvard.edu
;
; MODIFICATION HISTORY:
;        bmy, 16 Jun 2004: VERSION 1.00
;        bmy, 09 Jul 2004: VERSION 1.01
;                          - Now checks for missing values as anything
;                            greater than -999 (not -9999)
;                          - Also try again w/ lowercase file name
;        bmy, 23 Jul 2004: VERSION 1.02
;                          - Now reads BAE plane flight
;  dbm & bmy, 16 May 2005: VERSION 1.03
;                          - Now uses final merge file names
;  dbm & bmy, 17 Oct 2005: VERSION 1.04
;                          - Now also reads files from ship cruise
;                            on MV Ron Brown (treat it like a plane
;                            skimming the surface of the ocean)
;        bmy, 13 Mar 2006: VERSION 1.05
;                          - Modified for INTEX-B file names and C130 data
;        bmy, 07 Apr 2008: VERSION 1.06
;                          - Now use DATE2YMD and ADD_DATE
;                          - Few minor updates for ARCTAS flights
;                          - Don't print out points where Pressure = 0
;	 lei, 02 Jul 2013: Add WP-3D
;                          
;-
; Copyright (C) 2004-2008,
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes.  This copyright notice must be kept with any copy of 
; this software.  If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine make_planeflight"
;-----------------------------------------------------------------------


pro Make_PlaneFlight, Date, HdrFile=HdrFile, UpLoad=UpLoad, _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================

   ; External functions
   FORWARD_FUNCTION Add_Date, File_Exist, StrBreak

   ; Keywords and Arguments
   if ( N_Elements( Date    ) eq 0 ) then Message, 'Must pass DATE!'
   if ( N_Elements( HdrFile ) eq 0 ) then HdrFile = 'Planeflight.hdr'

   ; Split today's date into Year, Month, Day
   Date2Ymd, Date, Year, Month, Day

   ; Compute yesterday's date
   Date_Yst   = Add_Date( Date, -1 )

   ; String representations of today's date in YYYYMMDD format
   YYYYMMDD   = String( Date, Format='(i8.8)' )
   YYMMDD     = StrMid( YYYYMMDD, 2, StrLen( YYYYMMDD )-1L )

   ; String representations of yesterday's date in YYYYMMDD format
   YYYYMMDD_1 = String( Date_Yst, Format='(i8.8)' )
   YYMMDD_1   = StrMid( YYYYMMDD_1, 2, StrLen( YYYYMMDD_1 )-1L )
 
   ; Initialize variables
   Line        = ''
   Is_DC8      = 0L
   Is_C130     = 0L
   Is_P3B      = 0L
   Is_FCN      = 0L
   Is_BAE      = 0L
   Is_RHB      = 0L
   Is_WP3D     = 0L
   Is_Empty    = 0L
 
   ; Define output arrays w/ one element as a placeholder
   ARRSIZE     = 1
   FIdArr      = StrArr( ARRSIZE )
   LongArr     = FltArr( ARRSIZE )
   LatiArr     = FltArr( ARRSIZE )
   PresArr     = FltArr( ARRSIZE )
   MonthArr    = IntArr( ARRSIZE )
   DayArr      = IntArr( ARRSIZE )
   YearArr     = IntArr( ARRSIZE )
   HourArr     = IntArr( ARRSIZE )
   MinArr      = IntArr( ARRSIZE )
   JDArr       = DblArr( ARRSIZE )

   ;==================================================================== 
   ; Read ship data from the MV Ronald H. Brown (RHB), if necessary
   ;==================================================================== 

   ; Meteorology files for yesterday & today
   RHB_Met =  './RHB/ShipMet_RHB_20040705_R1.ict'

   ; Position files for yesterday & today
   RHB_Pos =  './RHB/ShipPos_RHB_20040705_R1.ict'

   ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ;%%% Kludge for ARCTAS, skip the MV Ron Brown data (bmy, 4/4/08)
   ;%%%
   ;%%%; dbm: RHB data is in one long file, starting on July 5.
   ;%%%; Logical flag is TRUE if RHB data exists
   ;%%%IS_RHB  = ( YYYYMMDD ge 20040705 )
   ;%%%
   IS_RHB  = 0
   ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   ; only proceed if there is ship data
   if ( IS_RHB ) then begin

      ; Echo info
      S = 'Getting flight track info from RHB files!'
      Message, S, /Info

      ; Read data
      Get_RHB, RHB_Met,           RHB_Pos, $
               RHB_Flt=RHB_Flt,   RHB_Long=RHB_Long,   RHB_Lat=RHB_Lat,  $
               RHB_Prs=RHB_Prs,   RHB_Month=RHB_Month, RHB_Day=RHB_Day,  $
               RHB_Year=RHB_Year, RHB_Hour=RHB_Hour,   RHB_Min=RHB_Min,  $
               RHB_JD=RHB_JD

      ; Only take points w/o missing data
      RHBGood = Where( RHB_Long gt -999.0 AND $
                       RHB_Lat  gt -999.0 AND $
                       RHB_Prs  gt -999.0 )
   endif

   ;====================================================================
   ; Read data from the NASA DC8, if necessary
   ;==================================================================== 

   ; NAV files w/ DC8 data
   DC8_Pos =  [ './DC8/nav_dc8_' + YYYYMMDD_1 + '_ra.ict', $
                './DC8/nav_dc8_' + YYYYMMDD   + '_ra.ict' ]

   ; Logical flag is TRUE if DC8 data exists
   Is_DC8  = ( File_Exist( DC8_Pos[0] ) OR File_Exist( DC8_Pos[1] ) )

   ; Only proceed if DC8 data exists for this date
   if ( Is_DC8 ) then begin

      ; Echo info
      S = 'Getting flight track info from ' + DC8_Pos[0] + $
          ' and ' + DC8_Pos[1]
      Message, S, /Info

      ; Read data into arrays
      Get_DC8, DC8_Pos, $
               DC8_Flt=DC8_Flt,   DC8_Long=DC8_Long,   DC8_Lat=DC8_Lat,  $
               DC8_Prs=DC8_Prs,   DC8_Month=DC8_Month, DC8_Day=DC8_Day,  $
               DC8_Year=DC8_Year, DC8_Hour=DC8_Hour,   DC8_Min=DC8_Min,  $
               DC8_JD=DC8_JD,     _EXTRA=e

      ; Only take points w/o missing data
      DC8Good = Where( DC8_Long gt -999.0 AND $
                       DC8_Lat  gt -999.0 AND $
                       DC8_Prs  gt -999.0 )
   endif

   ;====================================================================
   ; Read data from the NCAR C130, if necessary (for INTEX-B)
   ;==================================================================== 

   ; Filename w/ C130 data
   C130_Pos =  [ './C130/nav_c130_' + YYYYMMDD_1 + '_ra.ict', $
                 './C130/nav_c130_' + YYYYMMDD   + '_ra.ict' ]

   ; Logical flag is TRUE if DC8 data exists
   Is_C130  = ( File_Exist( C130_Pos[0] ) OR File_Exist( C130_Pos[1] ) )

   ; Only proceed if DC8 data exists for this date
   if ( Is_C130 ) then begin

      ; Echo info
      S = 'Getting flight track info from ' + C130_Pos[0] + $
          ' and ' + C130_Pos[1]
      Message, S, /Info

      ; Read data into arrays
      Get_C130, C130_Pos, $
                C130_Flt=C130_Flt,     C130_Long=C130_Long,   $
                C130_Lat=C130_Lat,     C130_Prs=C130_Prs,     $
                C130_Month=C130_Month, C130_Day=C130_Day,     $
                C130_Year=C130_Year,   C130_Hour=C130_Hour,   $
                C130_Min=C130_Min,     C130_JD=C130_JD,       $
                _EXTRA=e

      ; Only take points w/o missing data
      C130Good = Where( C130_Long gt -999.0 AND $
                        C130_Lat  gt -999.0 AND $
                        C130_Prs  gt -999.0 )
   endif

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% KLUDGE FOR ARCTAS
;%%% P3B nav data is now in a single file (bmy, 4/7/08)
;   ;==================================================================== 
;   ; Read data from the NOAA P3B, if necessary
;   ;==================================================================== 
;
;   ; Meteorology files for yesterday & today
;   P3B_Met = [ './P3B/AircraftMet_NP3_' + YYYYMMDD_1 + '_R0.ict', $
;               './P3B/AircraftMet_NP3_' + YYYYMMDD   + '_R0.ict' ]
;
;   ; Position files for yesterday & today
;   P3B_Pos = [ './P3B/AircraftPos_NP3_' + YYYYMMDD_1 + '_R0.ict', $
;               './P3B/AircraftPos_NP3_' + YYYYMMDD   + '_R0.ict' ]
;
;
;   ; Logical flag is TRUE if P3B data exists
;   IS_P3B  = ( ( File_Exist( P3B_Met[0] ) AND File_Exist( P3B_Pos[0] ) ) OR $
;               ( File_Exist( P3B_Met[1] ) AND File_Exist( P3B_Pos[1] ) ) )
;
;   ; Only proceed if 
;   if ( Is_P3B ) then begin
;
;      ; Echo info
;      S = 'Getting flight track info from P3B files!'
;      Message, S, /Info
;
;      ; Read data
;      Get_P3B, P3B_Met,           P3B_Pos, $
;               P3B_Flt=P3B_Flt,   P3B_Long=P3B_Long,   P3B_Lat=P3B_Lat,  $
;               P3B_Prs=P3B_Prs,   P3B_Month=P3B_Month, P3B_Day=P3B_Day,  $
;               P3B_Year=P3B_Year, P3B_Hour=P3B_Hour,   P3B_Min=P3B_Min,  $
;               P3B_JD=P3B_JD,     _EXTRA=e
;
;      ; Only take points w/o missing data
;      P3BGood = Where( P3B_Long gt -999.0 AND $
;                       P3B_Lat  gt -999.0 AND $
;                       P3B_Prs  gt -999.0 )
;   endif
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   ;====================================================================
   ; Read data from the NASA P3B, if necessary
   ;==================================================================== 

   ; NAV files w/ P3B data
   P3B_Pos =  [ './P3B/pds_p3b_' + YYYYMMDD_1 + '_ra.ict', $
                './P3B/pds_p3b_' + YYYYMMDD   + '_ra.ict' ]

   ; Logical flag is TRUE if P3B data exists
   Is_P3B  = ( File_Exist( P3B_Pos[0] ) OR File_Exist( P3B_Pos[1] ) )

   ; Only proceed if DC8 data exists for this date
   if ( Is_P3B ) then begin

      ; Echo info
      S = 'Getting flight track info from ' + P3B_Pos[0] + $
          ' and ' + P3B_Pos[1]
      Message, S, /Info

      ; Read data into arrays
      Get_P3B, P3B_Pos, $
               P3B_Flt=P3B_Flt,   P3B_Long=P3B_Long,   P3B_Lat=P3B_Lat,  $
               P3B_Prs=P3B_Prs,   P3B_Month=P3B_Month, P3B_Day=P3B_Day,  $
               P3B_Year=P3B_Year, P3B_Hour=P3B_Hour,   P3B_Min=P3B_Min,  $
               P3B_JD=P3B_JD,     _EXTRA=e

      ; Only take points w/o missing data
      P3BGood = Where( P3B_Long gt -999.0 AND $
                       P3B_Lat  gt -999.0 AND $
                       P3B_Prs  gt -999.0 )
   endif

   ;====================================================================
   ; Read data from the European NERC FALCON, if necessary
   ;==================================================================== 

   ; Filename w/ FALCON (FCN) data -- final output files (dbm, 5/13/05)
   FCN_Pos = [ './FCN/navmet_cmet_' + YYYYMMDD_1 + '_R0.ict',  $
               './FCN/navmet_cmet_' + YYYYMMDD   + '_R0.ict' ]


   ; Logical flag is TRUE if Falcon file exists
   Is_FCN  = ( File_Exist( FCN_Pos[0] ) OR File_Exist( FCN_Pos[1] ) )

   ; Only proceed if Falcon data exists for this date
   if ( Is_FCN ) then begin

      ; Echo info
      S = 'Getting flight track info from ' + FCN_Pos[0] + $
          ' and ' + FCN_Pos[1]
      Message, S, /Info

      ; Read data
      Get_FCN, FCN_Pos, $
               FCN_Flt=FCN_Flt,   FCN_Long=FCN_Long,   FCN_Lat=FCN_Lat,  $
               FCN_Prs=FCN_Prs,   FCN_Month=FCN_Month, FCN_Day=FCN_Day,  $
               FCN_Year=FCN_Year, FCN_Hour=FCN_Hour,   FCN_Min=FCN_Min,  $
               FCN_JD=FCN_JD

      ; Only take points w/o missing data
      FCNGood = Where( FCN_Long gt -999.0 AND $
                       FCN_Lat  gt -999.0 AND $
                       FCN_Prs  gt -999.0 )
   endif

   ;====================================================================
   ; Read data from the UK BAE146, if necessary
   ;==================================================================== 
   
   ; Filenames for final merges from Mat Evans (dbm, 5/13/05)
   BAE_Pos = [ './BAE/itop-mrg-1s_faam_' + YYYYMMDD_1 + '_R4.ict',  $
               './BAE/itop-mrg-1s_faam_' + YYYYMMDD   + '_R4.ict' ]

   ; Logical flag is TRUE if DC8 data exists
   Is_BAE  = ( File_Exist( BAE_Pos[0] ) OR File_Exist( BAE_Pos[1] ) )

   ; Only proceed if DC8 data exists for this date
   if ( Is_BAE ) then begin

      ; Echo info
      S = 'Getting flight track info from ' + BAE_Pos[0] + $
          ' and' + BAE_Pos[1]
      Message, S, /Info

      ; Read data into arrays
      Get_BAE, BAE_Pos, $
               BAE_Flt=BAE_Flt,   BAE_Long=BAE_Long,   BAE_Lat=BAE_Lat,  $
               BAE_Prs=BAE_Prs,   BAE_Month=BAE_Month, BAE_Day=BAE_Day,  $
               BAE_Year=BAE_Year, BAE_Hour=BAE_Hour,   BAE_Min=BAE_Min,  $
               BAE_JD=BAE_JD

      ; Only take points w/o missing data
      BAEGood = Where( BAE_Long gt -999.0 AND $
                       BAE_Lat  gt -999.0 AND $
                       BAE_Prs  gt -999.0 )
   endif

   ;====================================================================
   ; Read data from the WP-3D, if necessary
   ; Added by Lei, 20130702
   ;==================================================================== 

   ; NAV files w/ WP3D data
   WP3D_Pos =  [ './WP3D/AircraftPos_NP3_' + YYYYMMDD_1 + '_RA.ict', $
                './WP3D/AircraftPos_NP3_' + YYYYMMDD   + '_RA.ict' ]

   ; Logical flag is TRUE if WP3D data exists
   Is_WP3D  = ( File_Exist( WP3D_Pos[0] ) OR File_Exist( WP3D_Pos[1] ) )

   ; Only proceed if WP3D data exists for this date
   if ( Is_WP3D ) then begin

      ; Echo info
      S = 'Getting flight track info from ' + WP3D_Pos[0] + $
          ' and ' + WP3D_Pos[1]
      Message, S, /Info

      ; Read data into arrays
      Get_WP3D, WP3D_Pos, $
               WP3D_Flt=WP3D_Flt,   WP3D_Long=WP3D_Long,   WP3D_Lat=WP3D_Lat,  $
               WP3D_Prs=WP3D_Prs,   WP3D_Month=WP3D_Month, WP3D_Day=WP3D_Day,  $
               WP3D_Year=WP3D_Year, WP3D_Hour=WP3D_Hour,   WP3D_Min=WP3D_Min,  $
               WP3D_JD=WP3D_JD,     _EXTRA=e

      ; Only take points w/o missing data
      WP3DGood = Where( WP3D_Long gt -999.0 AND $
                       WP3D_Lat  gt -999.0 AND $
                       WP3D_Prs  gt -999.0 )
   endif

   ;====================================================================
   ; ERROR CHECK: skip ahead if no plane points are found
   ;==================================================================== 
   if ( Is_DC8 + Is_C130 + Is_P3B + Is_FCN + IS_BAE + IS_WP3D eq 0L ) then begin
      S = 'No flight track data found for ' + YYYYMMDD + '!' 
      Message, S, /Info
      Is_Empty = 1L
      goto, Create_File
   endif

   ;====================================================================
   ; Sort all flight track data by increasing Julian Date
   ;====================================================================

   ; Echo info
   S = 'Sorting flight track points from' 
   if ( Is_DC8  ) then S = S + ' DC8 '   
   if ( Is_C130 ) then S = S + ' C130 '
   if ( Is_P3B  ) then S = S + ' P3B '
   if ( Is_FCN  ) then S = S + ' FCN '
   if ( Is_WP3D ) then S = S + ' WP3D '
   Message, S, /Info

   ; Append RHB data to arrays
   if ( Is_RHB ) then begin
      FIdArr    = [ Temporary( FIdArr   ), RHB_Flt  [RHBGood] ]
      LongArr   = [ Temporary( LongArr  ), RHB_Long [RHBGood] ]
      LatiArr   = [ Temporary( LatiArr  ), RHB_Lat  [RHBGood] ]
      PresArr   = [ Temporary( PresArr  ), RHB_Prs  [RHBGood] ]
      MonthArr  = [ Temporary( MonthArr ), RHB_Month[RHBGood] ]
      DayArr    = [ Temporary( DayArr   ), RHB_Day  [RHBGood] ]
      YearArr   = [ Temporary( YearArr  ), RHB_Year [RHBGood] ]
      HourArr   = [ Temporary( HourArr  ), RHB_Hour [RHBGood] ]
      MinArr    = [ Temporary( MinArr   ), RHB_Min  [RHBGood] ]
      JdArr     = [ Temporary( JdArr    ), RHB_JD   [RHBGood] ]
   endif

   ; Append DC8 data to arrays
   if ( Is_DC8 ) then begin
      FIdArr    = [ Temporary( FIdArr   ), DC8_Flt  [DC8Good] ]
      LongArr   = [ Temporary( LongArr  ), DC8_Long [DC8Good] ]
      LatiArr   = [ Temporary( LatiArr  ), DC8_Lat  [DC8Good] ]
      PresArr   = [ Temporary( PresArr  ), DC8_Prs  [DC8Good] ]
      MonthArr  = [ Temporary( MonthArr ), DC8_Month[DC8Good] ]
      DayArr    = [ Temporary( DayArr   ), DC8_Day  [DC8Good] ]
      YearArr   = [ Temporary( YearArr  ), DC8_Year [DC8Good] ]
      HourArr   = [ Temporary( HourArr  ), DC8_Hour [DC8Good] ]
      MinArr    = [ Temporary( MinArr   ), DC8_Min  [DC8Good] ]
      JdArr     = [ Temporary( JdArr    ), DC8_JD   [DC8Good] ]
   endif

   ; Append C130 data to arrays
   if ( Is_C130 ) then begin
      FIdArr    = [ Temporary( FIdArr   ), C130_Flt  [C130Good] ]
      LongArr   = [ Temporary( LongArr  ), C130_Long [C130Good] ]
      LatiArr   = [ Temporary( LatiArr  ), C130_Lat  [C130Good] ]
      PresArr   = [ Temporary( PresArr  ), C130_Prs  [C130Good] ]
      MonthArr  = [ Temporary( MonthArr ), C130_Month[C130Good] ]
      DayArr    = [ Temporary( DayArr   ), C130_Day  [C130Good] ]
      YearArr   = [ Temporary( YearArr  ), C130_Year [C130Good] ]
      HourArr   = [ Temporary( HourArr  ), C130_Hour [C130Good] ]
      MinArr    = [ Temporary( MinArr   ), C130_Min  [C130Good] ]
      JdArr     = [ Temporary( JdArr    ), C130_JD   [C130Good] ]
   endif

   ; Append P3B data to arrays
   if ( Is_P3B ) then begin
      FIdArr    = [ Temporary( FIdArr   ), P3B_Flt  [P3BGood] ]
      LongArr   = [ Temporary( LongArr  ), P3B_Long [P3BGood] ]
      LatiArr   = [ Temporary( LatiArr  ), P3B_Lat  [P3BGood] ]
      PresArr   = [ Temporary( PresArr  ), P3B_Prs  [P3BGood] ]
      MonthArr  = [ Temporary( MonthArr ), P3B_Month[P3BGood] ]
      DayArr    = [ Temporary( DayArr   ), P3B_Day  [P3BGood] ]
      YearArr   = [ Temporary( YearArr  ), P3B_Year [P3BGood] ]
      HourArr   = [ Temporary( HourArr  ), P3B_Hour [P3BGood] ]
      MinArr    = [ Temporary( MinArr   ), P3B_Min  [P3BGood] ]
      JdArr     = [ Temporary( JdArr    ), P3B_JD   [P3BGood] ]
   endif

   ; Append FCN data to arrays
   if ( Is_FCN ) then begin
      FIdArr    = [ Temporary( FIdArr   ), FCN_Flt  [FCNGood] ]
      LongArr   = [ Temporary( LongArr  ), FCN_Long [FCNGood] ]
      LatiArr   = [ Temporary( LatiArr  ), FCN_Lat  [FCNGood] ]
      PresArr   = [ Temporary( PresArr  ), FCN_Prs  [FCNGood] ]
      MonthArr  = [ Temporary( MonthArr ), FCN_Month[FCNGood] ]
      DayArr    = [ Temporary( DayArr   ), FCN_Day  [FCNGood] ]
      YearArr   = [ Temporary( YearArr  ), FCN_Year [FCNGood] ]
      HourArr   = [ Temporary( HourArr  ), FCN_Hour [FCNGood] ]
      MinArr    = [ Temporary( MinArr   ), FCN_Min  [FCNGood] ]
      JdArr     = [ Temporary( JdArr    ), FCN_JD   [FCNGood] ]
   endif
   
   ; Append BAE data to arrays
   if ( Is_BAE ) then begin
      FIdArr    = [ Temporary( FIdArr   ), BAE_Flt  [BAEGood] ]
      LongArr   = [ Temporary( LongArr  ), BAE_Long [BAEGood] ]
      LatiArr   = [ Temporary( LatiArr  ), BAE_Lat  [BAEGood] ]
      PresArr   = [ Temporary( PresArr  ), BAE_Prs  [BAEGood] ]
      MonthArr  = [ Temporary( MonthArr ), BAE_Month[BAEGood] ]
      DayArr    = [ Temporary( DayArr   ), BAE_Day  [BAEGood] ]
      YearArr   = [ Temporary( YearArr  ), BAE_Year [BAEGood] ]
      HourArr   = [ Temporary( HourArr  ), BAE_Hour [BAEGood] ]
      MinArr    = [ Temporary( MinArr   ), BAE_Min  [BAEGood] ]
      JdArr     = [ Temporary( JdArr    ), BAE_JD   [BAEGood] ]
   endif

   ; Append WP3D data to arrays
   if ( Is_WP3D ) then begin
      FIdArr    = [ Temporary( FIdArr   ), WP3D_Flt  [WP3DGood] ]
      LongArr   = [ Temporary( LongArr  ), WP3D_Long [WP3DGood] ]
      LatiArr   = [ Temporary( LatiArr  ), WP3D_Lat  [WP3DGood] ]
      PresArr   = [ Temporary( PresArr  ), WP3D_Prs  [WP3DGood] ]
      MonthArr  = [ Temporary( MonthArr ), WP3D_Month[WP3DGood] ]
      DayArr    = [ Temporary( DayArr   ), WP3D_Day  [WP3DGood] ]
      YearArr   = [ Temporary( YearArr  ), WP3D_Year [WP3DGood] ]
      HourArr   = [ Temporary( HourArr  ), WP3D_Hour [WP3DGood] ]
      MinArr    = [ Temporary( MinArr   ), WP3D_Min  [WP3DGood] ]
      JdArr     = [ Temporary( JdArr    ), WP3D_JD   [WP3DGood] ]
   endif

   ; Resize arrays to eliminate the first element
   ; which was just a placeholder anyway
   
   FIdArr    = Temporary( FIdArr  [1:*]  )
   LongArr   = Temporary( LongArr [1:*]  )
   LatiArr   = Temporary( LatiArr [1:*]  )
   PresArr   = Temporary( PresArr [1:*]  )
   MonthArr  = Temporary( MonthArr[1:*]  )  
   DayArr    = Temporary( DayArr  [1:*]  )  
   YearArr   = Temporary( YearArr [1:*]  )  
   HourArr   = Temporary( HourArr [1:*]  )  
   MinArr    = Temporary( MinArr  [1:*]  )  
   JdArr     = Temporary( JdArr   [1:*]  )  

   ; Number of elements left
   Count     = N_Elements( JdArr )

   ; Sort all flight points by increasing Julian Date
   IndSort   = Sort( JdArr )

   ;====================================================================
   ; Write Planeflight.dat header to output file
   ;====================================================================
Create_File:

   ; Define outfile path 
   ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ;%%% KLUDGE FOR ARCTAS -- just use YYYYMMDD for file name (bmy, 4/4/08)
   ;OutFilePath = './output/Planeflight.dat.' + YYYYMMDD + '00'
   ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   OutFilePath = './output/Planeflight.dat.' + YYYYMMDD
   
   ; Get just the filename from OUTFILEPATH
   OutFileName = Extract_FileName( OutFilePath )

   ; Echo info
   S = 'Writing flight track points for '
   if ( Is_RHB  ) then S = S + ' RHB ' 
   if ( Is_DC8  ) then S = S + ' DC8 '
   if ( Is_C130 ) then S = S + ' C130 '
   if ( Is_P3B  ) then S = S + ' P3B '
   if ( Is_FCN  ) then S = S + ' FCN '
   if ( Is_BAE  ) then S = S + ' BAE '
   if ( Is_WP3D ) then S = S + ' WP3D '
   S = S + ' to ' + OutFilePath
   Message, S, /Info

   ; Open file for output
   Open_File, OutFilePath, Ilun_OUT, /Get_LUN, /Write
 
   ; Open header file
   Open_File, HdrFile, Ilun_Hdr, /Get_LUN
 
   ; Copy lines from the header to the output file
   while ( not EOF( Ilun_HDR ) ) do begin
      ReadF,  Ilun_HDR, Line
      PrintF, Ilun_OUT, Line
   endwhile
 
   ; Close header file
   Close,    Ilun_HDR
   Free_LUN, Ilun_HDR
 
   ;====================================================================
   ; Write sorted flight track points from all planes to output file 
   ;====================================================================

   ; Only write file if planeflight points are found
   if ( not Is_Empty ) then begin

      ; Fmt str: Flt, Day, Month, Year, Hour, Min, Lat, Lon, Pressure
      Fmt = '( i5,1x,a5,x,i2.2,''-'',i2.2,''-'',i4,x,i2.2,'':'',i2.2,1x,f7.2,x,f7.2,x,f7.2 )'

      ; Counter array
      Ct = 0L

      ; Loop over all flight points
      for N = 0L, Count-1L do begin

         ; Get sorted index
         I = IndSort[N]

         ; Only print today's points
         if ( YearArr[I]  eq Year  AND $
              MonthArr[I] eq Month AND $
              DayArr[I]   eq Day ) then begin

            ; Print in sorted order
            ; Don't print out pressures that are zero! (bmy, 4/4/08)
            if ( PresArr[I] gt 0.0 ) then begin
               PrintF, Ilun_OUT, ( Ct mod 100000L ), $
                       FIdArr[I],  DayArr[I], MonthArr[I], YearArr[I], $
                       HourArr[I], MinArr[I], LatiArr[I],  LongArr[I], $
                       PresArr[I], Format=Fmt

               ; Increment count
               Ct = Ct + 1L
            endif
         endif
      endfor

   endif

   ; Write closing line to output file
   PrintF, Ilun_OUT, '99999   END 00-00-0000 00:00    0.00    0.00    0.00'
 
   ; Close input file
   Close,    Ilun_OUT
   Free_LUN, Ilun_OUT

   ;====================================================================
   ; Upload files to proper directory on geos.as.harvard.edu
   ;====================================================================
   if ( Keyword_Set( UpLoad ) ) then begin
      
      ; Directory where to upload Planeflight.dat files
      Dir = '/as2/pub/ftp/pub/geos-chem/NRT-ARCTAS/flighttrack/'

      ; Echo info
      S = 'Uploading ' + OutFileName + ' to the NRT directory'
      Message, S, /Info

      ; Upload the file
      Cmd = 'cp '+ OutFilePath + ' ' + Dir + OutFileName
      Spawn, Cmd

      ; Change permission to 664
      Cmd = 'chmod 664 ' + Dir + OutFileName  
      Spawn, Cmd

   endif
 
   ;Quit
   return
end