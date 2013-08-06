; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        TAPPLY
;
; PURPOSE:
;       TAPPLY reproduces the behavior of the R function tapply
;       TAPPLY applies the specified function, to all elements of ARRAY
;       which have the same GROUP value. The function returns an array with
;       as many elements as unique values within GROUP.
; 
; CATEGORY:
;
; CALLING SEQUENCE:
;        RESULT = TAPPLY( ARRAY, GROUP, FUNCTIONSTR [, KEYWORDS] ) 
;
; INPUTS:
;       ARRAY       - array of arbitrary size
;       GROUP       - array of same dimension as ARRAY while classifies
;                     elements of ARRAY
;       FUNCTIONSTR - string naming an IDL function to apply to ARRAY
;       _EXTRA      - any additional keywords are passed to the
;                     specified function
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       RESULT      - array resulting from the function application
;       GROUPVALUES - keyword array containing the unique values of GROUP, in
;                     the same order as RESULT
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;        ; CALCULATE THE AVERAGE DIURNAL CYCLE
;        ; Sinusoidal signal with 24-h period, plus noise
;        signal = cos( findgen(1024) * 2 * !pi / 24 ) + randomn( seed, 256 )
;        ; corresponding time in hours
;        time   = findgen(1024)
;
;        ; Calculate the diurnal mean
;        diurnalmean = tapply( signal, ( time mod 24 ), 'mean', groupval=hour )
;
;        ; plot the mean cycle
;        plot, hour, diurnalmean, xtitle='hour', ytitle='mean signal'
;
; MODIFICATION HISTORY:
;        cdh, 15 Apr 2011: VERSION 1.00
;
;-
; Copyright (C) 2011, Christopher Holmes, UC Irvine
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to cdholmes@post.harvard.edu
; with subject "IDL routine tapply"
;-----------------------------------------------------------------------

function tapply, array, group, functionstr, groupvalues=groupvalues, $
                 _EXTRA=_EXTRA
   
   ; Find the unique values in the classification array GROUP
   groupvalues = group[ uniq( group, sort( group ) ) ]
 
   ; Find the type of the input array
   type = size( array, /type )
 
   ; Make an array with the same type as the input array
   result = make_array( n_elements( groupvalues ),  type=type )
 
   ; Loop over the number of unique values
   for i=0, n_elements( groupvalues ) - 1L do begin
 
      ; Find which elements share the same value
      index = where( group eq groupvalues[i] )
      
      ; Apply the given function to the common elements
      result[i] = call_function( functionstr, array[index], _EXTRA=_EXTRA)
      
   endfor
   
   ; Return the result
   return, result
 
 
end