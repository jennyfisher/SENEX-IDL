;; Create all of the model merge files for the SENEX mission from
;; the planelog files
;; sourceDir = directory with plane.log files
;; destDir   = directory for output sav files 

PRO senex_model_merge, sourceDir, destDir, avgTime=avgTime, GridType=GridType

   ;=========================================================
   ; Setup file names and directories
   ;=========================================================

   ; Filename template for GEOS-Chem ND40 output files
   PlanelogFormat = 'plane.log.YYYYMMDD'

   ; Filename template for single-flight SAV files
   SaveOutFormat = 'mrgTIME_senex_PLATFORM_YYYYMMDD.sav'

   ; Filename output for single-platform all flights
   outFile_AllFlights  = 'mrgTIME_senex_PLATFORM.sav'

   ; Confirm using default averaging time
   if not Keyword_Set( avgTime ) then $
     avgTime = 10
   
   ; Replace TIME tokens
   outFile_AllFlights = replace_token( outFile_AllFlights,     'TIME',  $
                                       string(avgTime)+'m', delim='' )

   ;=========================================================
   ; MODIFICATIONS BELOW MAY BE UNNECESSARY
   ;==========================================================

   ;=========================================================
   ; DC8 Flight dates
   ;=========================================================

   ; Dates on which DC8 flew (or started flying if airborne at 00 GMT)
   ; Replace DD with real date in the future.
   ;DC8_flightdates = [ 201308DD, 201308DD, 201308DD, 201308DD, $
   ;		       201308DD, 20130818, 201308DD, 201308DD, $
   ;                   201308DD, 20130826, 201308DD, 201308DD, $
   ;                   201309DD, 20130905, 201309DD, 201308DD, $
   ;                   201309DD, 20130913, 201309DD, 201309DD ]

   ; Flights on which the DC8 was airborne at 00 GMT - "2day flights"
   ; Replace DD with real date in the future.
   ; Not used currently. lei, 07/22/2013
   ;DC8_twoDay      = [ 201308DD, 201308DD, 201308DD, 201308DD, $
   ;		       201308DD, 20130818, 201308DD, 201308DD, $
   ;                   201309DD, 20130913, 201309DD, 201309DD ]
                      
   ; Time of takeoff - For days on which one flight ended just after 
   ; 00 GMT and another began later in the day, this will ignore data
   ; before the takeoff time on the takeoff day. This can be 00 GMT on
   ; days with only one flight.
   ;DC8_TakeoffTime = [ 0000,     0000,     0000,     0000,      $
   ;		       0000,     0000,     0000,     0000,      $
   ;                   0000,     0000,     0000,     0000,      $
   ;                   0000,     0000,     0000,     0000,      $
   ;                   0000,     0000,     0000,     0000 ]
                       
   ; Landing time (GMT) on last day of flight, can be 2400 on days
   ; with only one flight
   ;DC8_LandingTime = [ 2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400 ]

   ;=========================================================
   ; NASA ER-2 Flight dates
   ;=========================================================

   ; Dates on which ER2 flew (or started flying if airborne at 00 GMT)
   ; Replace DD with real date in the future.
   ;ER2_flightdates = [ 201308DD, 201308DD, 201308DD, 201308DD, $
   ;		       201308DD, 20130818, 201308DD, 201308DD, $
   ;                   201308DD, 20130826, 201308DD, 201308DD, $
   ;                   201309DD, 20130905, 201309DD, 201308DD, $
   ;                   201309DD, 20130913, 201309DD, 201309DD ]

   ; Flights on which the ER2 was airborne at 00 GMT - "2day flights"
   ; Replace DD with real date in the future.
   ; Not used currently. lei, 07/22/2013
   ;ER2_twoDay      = [ 201308DD, 201308DD, 201308DD, 201308DD, $
   ;		       201308DD, 20130818, 201308DD, 201308DD, $
   ;                   201309DD, 20130913, 201309DD, 201309DD ]

   ; Time of takeoff - For days on which one flight ended just after 
   ; 00 GMT and another began later in the day, this will ignore data
   ; before the takeoff time on the takeoff day. This can be 00 GMT on
   ; days with only one flight.
   ;ER2_TakeoffTime = [ 0000,     0000,     0000,     0000,     $
   ;		       0000,     0000,     0000,     0000,     $
   ;                   0000,     0000,     0000,     0000,     $
   ;                   0000,     0000,     0000,     0000,     $
   ;                   0000,     0000,     0000,     0000 ]

   ; Landing time (GMT) on last day of flight, can be 2400 on days
   ; with only one flight
   ;ER2_LandingTime = [ 2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400,      $
   ;                   2400,     2400,     2400,     2400 ]
   
   ;=========================================================
   ; NOAA WP-3D Flight dates
   ;=========================================================

   ; Dates on which WP3D flew (or started flying if airborne at 00 GMT)
   WP3D_flightdates = [ 20130710, 20130708 ]
   ;		        20130529, 20130531, 20130603, 20130610, $
   ;                    20130611, 20130612, 20130616, 20130618, $
   ;                    20130619, 20130622, 20130623, 20130625, $
   ;                    20130626, 20130629, 20130702, 20130703, $
   ;                    20130705, 20130706, 20130708, 20130710 ]

   ; Time of takeoff - For days on which one flight ended just after 
   ; 00 GMT and another began later in the day, this will ignore data
   ; before the takeoff time on the takeoff day. This can be 00 GMT on
   ; days with only one flight.
   WP3D_TakeoffTime = [ 0000,     0000 ]
   ;		        0000,     0000,     0000,     0000,     $
   ;		        0000,     0000,     0000,     0000,     $
   ;                    0000,     0000,     0000,     0000,     $
   ;                    0000,     0000,     0000,     0000,     $
   ;                    0000,     0000,     0000,     0000 ]

   ; Landing time (GMT) on last day of flight, can be 2400 on days
   ; with only one flight
   WP3D_LandingTime = [ 2400,     2400 ]
   ;                    2400,     2400,     2400,     2400,      $
   ;                    2400,     2400,     2400,     2400,      $
   ;                    2400,     2400,     2400,     2400,      $
   ;                    2400,     2400,     2400,     2400,      $
   ;                    2400,     2400,     2400,     2400 ]


   ;=========================================================
   ; Make single-flight sav files: DC8
   ;=========================================================

   ; batch_planelog2flightmerge,'DC8', DC8_FlightDates, $
   ;  DC8_TakeoffTime, DC8_LandingTime,      $
   ;  PlanelogFormat,  SaveOutFormat,        $
   ;  sourceDir,       destDir,      avgTime,$
   ;  GridType=GridType, outFiles=DC8_savFiles

   ;=========================================================
   ; Make sav file for all DC8 flights
   ;=========================================================

   ;print, 'Reading DC8 All data...'
   
   ;merge_file_cat, DC8_savFiles, 'GC', GC

   ;outFile_AllDC8 = replace_token( outFile_AllFlights,     'PLATFORM',  $
   ;                                'dc8',                  delim='' )

   ;outFile_AllDC8 = destDir + outFile_AllDC8

   ;print, 'Saving All DC8 data as ' + outFile_AllDC8

   ; Save AllDC8 structure
   ;Save, GC, filename=outFile_AllDC8

   ; Close all DC8 files
   ;close,/all

   ;=========================================================
   ; Make single-flight sav files: ER2
   ;=========================================================

   ;batch_planelog2flightmerge, 'ER2',  ER2_FlightDates, $
   ;  ER2_TakeoffTime, ER2_LandingTime,        $
   ;  PlanelogFormat,  SaveOutFormat,          $
   ;  sourceDir,       destDir,      avgTime,  $
   ;  GridType=GridType, outFiles=ER2_savFiles

   ;=========================================================
   ; Make sav file for all ER2 flights
   ;=========================================================

   ;print, 'Reading ER2 All data...'
   
   ;merge_file_cat, ER2_savFiles, 'GC', GC

   ;outFile_AllER2 = replace_token( outFile_AllFlights,     'PLATFORM',  $
   ;                                'er2',                 delim='' )

   ;outFile_AllER2 = destDir + outFile_AllER2

   ;print, 'Saving All ER2 data as ' + outFile_AllER2

   ; Save AllER2 structure
   ;Save, GC, filename=outfile_AllER2
       
   ; Close all ER2 files
   ;close,/all
       
   ;=========================================================
   ; Make single-flight sav files: WP3D
   ;=========================================================

   batch_planelog2flightmerge, 'WP3D',  WP3D_FlightDates, $
     WP3D_TakeoffTime, WP3D_LandingTime,        $
     PlanelogFormat,  SaveOutFormat,            $
     sourceDir,       destDir,      avgTime,    $
     GridType=GridType, outFiles=WP3D_savFiles

   ;=========================================================
   ; Make sav file for all WP3D flights
   ;=========================================================

   print, 'Reading WP3D All data...'
   
   merge_file_cat, WP3D_savFiles, 'GC', GC

   outFile_AllWP3D = replace_token( outFile_AllFlights,     'PLATFORM',  $
                                   'wp3d',                 delim='' )

   outFile_AllWP3D = destDir + outFile_AllWP3D

   print, 'Saving All WP3D data as ' + outFile_AllWP3D

   ; Save AllWP3D structure
   Save, GC, filename=outfile_AllWP3D
       
   ; Close all WP3D files
   close,/all
       
end
