;; Average all variables along a planeflight

pro planeflight_average, Plane, Minutes

   ; Group all data by which interval of width Minutes
   ; they fall into
   group = fix( Plane.DOY * 24. * 60. / Minutes )

   ; Get Tag names
   tagNames = tag_names( Plane )

   ; Initialize
   First = 1L

   ; All Variables should have numeric types, so it's OK to average
   for t=0L, n_elements( tagNames )-1L do begin

      ; Construct command to do averaging      
      cmd = 'newVal = tapply( Plane.' + tagNames[t] + ', group, ' + $
         '"mean", /NaN, /Double )'

      ; Do averaging
      status = Execute( cmd )

      ; Convert the standard plane tags to their original type
      ; Assume the rest are floats
      case tagNames[t] of
         'DATE': newVal = long(   newVal ) 
         'UTC':  newVal = fix(    newVal ) 
         'DOY':  newVal = double( newVal )
         else:   newVal = float(  newVal )
      endcase

      ; Add new averaged variables to structure
      if First then begin
         
         cmd = 'Plane_new = {' + tagNames[t] + ': newVal }'
         status = Execute( cmd )
         First = 0L

      endif else begin

         Plane_new = struAddVar( Plane_new, newVal, tagNames[t] )

      endelse

   endfor

end
