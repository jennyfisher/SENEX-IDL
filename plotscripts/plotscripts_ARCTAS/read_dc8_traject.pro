
function DC8_LineSplit, Line

;====================================================================
; Function DC8_LINESPLIT splits a line by spaces or horizontal tabs.
; (bmy, 5/16/05)
;====================================================================

   ; First try to break the line by spaces ...
   Result = StrBreak( Line, ' ' )

   ; ... then if that doesn't work, by tabs
   if ( N_Elements( Result ) eq 1 ) $
      then Result = StrBreak( Line, BYTE(9B) )

   ; ... then if that doesn't work, by commas
   if ( N_Elements( Result ) eq 1 ) $
      then Result = StrBreak( Line, ',' )

   ; Return success or stop w/ error
   if ( N_Elements( Result ) gt 0 ) then begin
      return, Result
   endif else begin
      S = 'Could not split line by spaces or tabs or commas!'
      Message, S
   endelse
end

;====================================================================
; End function DC8_LINESPLIT
;====================================================================

pro read_dc8_traject, Date, Back=Back, Frwd=Frwd

;; =============================
;; Read in datafile
;; =============================

if ( N_Elements( Date ) eq 0 ) then Message, 'Must pass DATE!'
if ( Keyword_Set( Back ) and Keyword_Set( Frwd ) ) then $
	Message, 'Must choose forward OR backward trajectories'
if ( ~Keyword_Set( Back ) and ~Keyword_Set( Frwd ) ) then $
	Message, 'Must choose forward OR backward trajectories'

IF ( Keyword_Set( Back ) ) then begin

  FileName = !ARCTAS+'/field_data/DC8/trajectory/' + $
            'DC8-FltLv-BACK_TRAJECTORY_'+String(Date,'(i8.0)')+'_R1.ict'

  OutFile = !ARCTAS+'/field_data/DC8/trajectory/' + $
            'DC8-Flt-BACK_TRAJECTORY_'+String(Date,'(i8.0)')+'.sav'

ENDIF else begin

  FileName = !ARCTAS+'/field_data/DC8/trajectory/' + $
            'DC8-FltLv-FRWD_TRAJECTORY_'+String(Date,'(i8.0)')+'_R0.ict'

  OutFile = !ARCTAS+'/field_data/DC8/trajectory/' + $
            'DC8-Flt-FRWD_TRAJECTORY_'+String(Date,'(i8.0)')+'.sav'

ENDELSE

  print, ' ### Processing ', FileName

  ;; Logical flag is TRUE if DC8 data exists
  Is_Data  = ( File_Exist( FileName) )
  if ( ~Is_Data ) then Stop,  'Data file not found!', FileName

  ;; Only proceed if data exists for this date
  If ( Is_Data ) then begin

    Open_File, FileName, Ilun_Flt, /Get_LUN

    Line = ' '

    ;; Read first line
    ReadF, Ilun_Flt, Line

    ;; Split line by spaces or tabs
    Result = DC8_LineSplit( Line )

    ;; Get # of lines in the header
    N_Hdr = Long( Result[0] )

    ;; Skip header
    for N = 0L, N_Hdr-2L do begin

      ;; Read each lines
      ReadF, Ilun_Flt, Line

      ;; Get number of variables
      if ( N eq 8 ) then begin

         ;; Split the line
         Result = DC8_LineSplit( Line )

         ;; Save into variables
         NVars  = Long( Result[0] )

         ;; Undefine
         Undefine, Result

      endif

      ;; Parse the line w/ the data field names (bmy, 3/6/06)
      if ( N eq N_Hdr-2L ) then begin

         ;; Split the line
         VarNames  = DC8_LineSplit( Line )

      endif

    endfor

     ;; Read in data
     First = 1L
     while ( not EOF( Ilun_Flt ) ) do begin
        ReadF, Ilun_Flt, Line
        junk   = StrBreak( Line, ' ' )
        junk2  = StrBreak( junk[0], '.' )

	; pflt is the flight minute and ptra is the trajectory time
        pflt = fix( junk2[0] )  &  ptra = fix( junk2[1] )

        if ( First ) then begin
          NVar = n_elements(junk)
          tmp  = [ pflt, ptra, double(junk[1:(NVar-1)]) ]
          data = [ tmp ]
        endif else begin
          tmp  = [ pflt, ptra, double(junk[1:(NVar-1)]) ]
          data = [ [data], [tmp] ]
        endelse
        First = 0L
     endwhile

     Close,    Ilun_Flt
     Free_LUN, Ilun_Flt

     ;; To save the date for each flight
     tmp_fltmin = reform( data[0, *] )
     NN  = n_elements( uniq(tmp_fltmin, sort(tmp_fltmin)) )
     print, NN
     SDate = Lonarr(NN) + Date

     Fltmin  =  indgen( NN ) + 1
     Traj =  intarr( NN, 250 )-9999
     LAT  =  fltarr( NN, 250 )-9999
     LON  =  fltarr( NN, 250 )-9999
     PRES =  fltarr( NN, 250 )-9999
     UTC  =  fltarr( NN, 250 )-9999

     for n = 0, NN-1 do begin
       index    =  where( tmp_fltmin eq n+1 )
       ntp      =  n_elements( index )
          
       Traj[n, 0:ntp-1] = data[1, index] 
       LAT [n, 0:ntp-1] = data[2, index]
       LON [n, 0:ntp-1] = data[3, index]
       PRES[n, 0:ntp-1] = data[4, index]
       UTC [n, 0:ntp-1] = data[5, index]
     endfor

  Endif

  ;; ==========================
  ;; Save data structure
  ;; ==========================

IF ( Keyword_Set( Back ) ) then begin
  DC8_Back_Traj = { Date:SDate, Fltmin:Fltmin,  Traj:Traj,  $
                 LAT:LAT, LON:LON, Pres:Pres, UTC:UTC }

  Save, DC8_Back_Traj, filename= OutFile, /COMPRESS

Endif else begin
  DC8_Frwd_Traj = { Date:SDate, Fltmin:Fltmin,  Traj:Traj,  $
                 LAT:LAT, LON:LON, Pres:Pres, UTC:UTC }

  Save, DC8_Frwd_Traj, filename= OutFile, /COMPRESS


Endelse

END
