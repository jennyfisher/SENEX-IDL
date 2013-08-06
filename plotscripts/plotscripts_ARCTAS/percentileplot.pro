pro percentileplot, X, Y, vertical=vertical, group=group, $
   overplot=overplot, _extra=_extra

   vertical = keyword_set( vertical )

   ; use oplot if overplotting, plot otherwise
   plotcmd = keyword_set( overplot ) ? 'oplot' : 'plot'

   ; If groups are not specified then divide into 10 bins
   if (not keyword_set( group ) ) then begin
   
      ; Find max and min of desired dimension
      mx = vertical ? max( Y, min=mn ) : max( X, min=mn )

      group = maken( mn, mx, 10 )

   endif
   
   ; Since group designates the midpoint, we need to identify the boundaries
   group_edge = group-ts_diff(group, 1)/2.

   ; Assign each data point to a group ID
   gID = vertical ? value_locate(group_edge, Y ) : $
                    value_locate(group_edge, X )

   ; Choose whether to calculate percentiles of X or Y
   data = vertical ? X : Y
   
   ; Median
   data_med = tapply( data, gID, 'median', _extra=_extra )

   ; Lower and upper percentiles
   data_lo = tapply( data,  gID, 'percentiles', value=0.25, $
                     _extra=_extra )
   data_hi = tapply( data,  gID, 'percentiles', value=0.75, $
                     _extra=_extra )
   

   if (vertical) then begin

      call_procedure, plotcmd,  data_med, group, _extra=_extra
      oplot, data_lo,  group, _extra=_extra
      oplot, data_hi,  group, _extra=_extra

   endif else begin
      
      call_procedure, plotcmd,  group, data_med, _extra=_extra
      oplot, group, data_lo,  _extra=_extra
      oplot, group, data_hi,  _extra=_extra

   endelse


end
