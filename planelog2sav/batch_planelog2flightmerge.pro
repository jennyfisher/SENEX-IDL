Pro batch_planelog2flightmerge, Platform,     FlightDates,  $
; Changed by lei, not use TwoDay keyword. 07/23/2013
;PRO batch_planelog2flightmerge, Platform,     FlightDates,  TwoDay, $
                                TakeoffTimes, LandingTimes,         $
                                FileInFormat, FileOutFormat,        $
                                sourceDir,    destDir,      avgTime,$
                                GridType=GridType, outFiles=outFiles

   ;=========================================================
   ; Make single flight sav files
   ;=========================================================
   
   outFiles=''

   For F=0L, n_elements( FlightDates )-1L do begin

       ; Name of input file or files
       inFiles = replace_token( FileInFormat, 'YYYYMMDD', $
                                string(FlightDates[F]), $
                                delim='')
       ; Not use DayTwo keyword, changed by lei, 07/23/2013
       ; Check if this is a two-day flight 
       ;If total( FlightDates[F] eq TwoDay ) then begin
           
           ; This is two-day, so add a second file
       ;    inFiles = [ inFiles, $
       ;                replace_token( FileInFormat, 'YYYYMMDD',   $
       ;                               string(FlightDates[F]+1), $
       ;                               delim=''                      ) ]
       ;Endif

       ; Name of output file
       outFile = replace_token( FileOutFormat, 'TIME', $
                                string(avgTime)+'m', delim='' )

       outFile = replace_token( outFile, 'PLATFORM', $
                                StrLowCase(Platform), delim='' )

       outFile = replace_token( outFile, 'YYYYMMDD', $
                                string(FlightDates[F]), delim='' )

        ; Add the input directory
       inFiles = sourceDir + inFiles

       ; Add the output directory
       outFile = destDir + outFile
       print, outFile
       ; Make the single-flight merge files
       planelog2flightmerge, Platform, inFiles, outFile=outFile, $
         flightDate  = FlightDates[F],       $
         takeoffTime = TakeoffTimes[F],                    $
         landingTime = LandingTimes[F], $
         GridType=GridType, avgMin=avgTime 
    
       ; Keep a list of output files
       outFiles = [ outFiles, outFile ]
       close,/all
   endfor

   ; Remove first element ''
   outFiles = outFiles[1:*]

END
