@read_wp3d_merge

pro read_all_merges, Platform, Dir=Dir

   If n_elements(Platform) NE 1 Then $
      Stop, 'Must pass platform (''WP3D'' )'
   
   Case Platform Of
      'WP3D' : FileString = 'mrg60_wp3d*ict'
   Endcase

   ; Default to look in the current directory
   IF ( ~Keyword_Set( Dir ) ) Then Dir =  '.'

   ; Change to Directory of Interest
   CD, Dir

   ; Retrieve list of all files matching search criteria
   FileList = MFindFile(FileString)

   ;; Loop through files
   For F=0L, n_elements(FileList)-1L Do Begin

      Ctm_Cleanup
   
      ThisFile = FileList[F]
   
      print, 'Processing ', ThisFile, '...'

;--------------------------------------------------------------------
; READ_DC8_MERGE previously accepted Date input, Now takes FileName
;--------------------------------------------------------------------
;      ;; Get Date to pass to subroutine
;      Date = (StrSplit(ThisFile, '_', /EXTRACT))[2]
;      
;      Case Platform Of
;         'DC8' : read_dc8_merge, Date
;         'C130': read_c130_merge, Date
;      Endcase
;--------------------------------------------------------------------

      ;; Get FileName to pass to subroutine, without '.ict'
      FileName = (StrSplit(ThisFile, '.', /EXTRACT))[0]
      
      Case Platform Of
         'WP3D': read_wp3d_merge, FileName
      Endcase

      print, 'Done!'
   Endfor

end
