;; Concatenate each field within structures named StuctName within the files
;; specified, Return the result in outStruct

pro merge_file_cat, Files, StructName, outStruct

   StructName = StrTrim( StructName, 2 )

   FIRST = 1L

   ; Make list of files for Houston flights
   FOR F=0L, n_elements( Files )-1L DO BEGIN
print,files(f)
       ; Restore next file
       restore, Files[F]

       cmd = 'tagNames = tag_names( ' + StructName +' )'
       
       status = Execute( cmd )

       ; Number of tags
       nTags = n_elements( tagNames ) 

       ; Add next this structure to existing structure
       IF (FIRST) THEN BEGIN

           FIRST = 0L

           FOR T=0L, nTags-1L DO BEGIN

              ; Construct command to put all data into an array
               cmd = tagNames[T] + ' = '+StructName+'.'+tagNames[T]

               status = Execute( cmd )

           ENDFOR

       ENDIF ELSE BEGIN

           FOR T=0L, n_elements(tagNames)-1L DO BEGIN

               ; Construct command to concatenate arrays
               cmd = tagNames[T] + ' = [ ' + tagNames[T] + ', '+ $
                 StructName+'.'+tagNames[T] +' ]'

               ; Concatenate
               status = Execute( cmd )
 
           ENDFOR

       ENDELSE

   ENDFOR

   FIRST = 1L
   
   cmd = 'outStruct  = {'

   ; Reconstruct structure with all flight data
   FOR T=0L, nTags-1L DO BEGIN

       IF (FIRST) THEN BEGIN

           FIRST = 0L 

          cmd = cmd + ''+ tagNames[T] +':'+ tagNames[T]
       
       ENDIF ELSE BEGIN

           cmd = cmd + ',' + tagNames[T] +':'+ tagNames[T]

       ENDELSE       

 
   ENDFOR

   cmd = cmd + '}'
   status= Execute( cmd )
CLOSE,/ALL
END
