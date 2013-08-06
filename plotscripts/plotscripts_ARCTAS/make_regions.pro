pro make_regions, region, filename=filename

   if n_elements(region) eq 0 then return

   if n_elements(filename) eq 0 then begin ;& window, 0
   endif else $
   ; Set up postscript file
   open_device, /ps, bits = 8, /color, filename=filename, /portrait

   ; Get grid information
   ModelType = ctm_type('GEOS5_47L',  res=2)
   Grid = ctm_grid(ModelType)

   case region of
	'na': begin
		XInd = where(grid.xmid ge -172.5 and grid.xmid le -17.5)
		YInd = where(grid.ymid ge   24.0 and grid.ymid le  88.0)
		Title='North America'
	end
	'eur': begin
		XInd = where(grid.xmid ge  -17.5 and grid.xmid le  60.0)
		YInd = where(grid.ymid ge   33.0 and grid.ymid le  88.0)
		Title='Europe'
	end
	'sib': begin
		XInd = where(grid.xmid ge   60.0 and grid.xmid le 172.5)
		YInd = where(grid.ymid ge   50.0 and grid.ymid le  88.0)
		Title='Siberia'
	end
	'as': begin
		XInd = where(grid.xmid ge   60.0 and grid.xmid le 152.5)
		YInd = where(grid.ymid ge    0.0 and grid.ymid le  50.0)
		Title='Asia'
	end
	'nsib': begin
		XInd = where(grid.xmid ge   60.0 and grid.xmid le 172.5)
		YInd = where(grid.ymid ge   60.0 and grid.ymid le  88.0)
		Title='Boreal Siberia'
	end
	'ssib': begin
		XInd = where(grid.xmid ge   60.0 and grid.xmid le 152.5)
		YInd = where(grid.ymid ge   33.0 and grid.ymid le  60.0)
		Title='Southern Siberia'
	end
	'bas': begin
		XInd = where(grid.xmid ge   60.0 and grid.xmid le 152.5)
		YInd = where(grid.ymid ge    0.0 and grid.ymid le  33.0)
		Title='Asia'
	end
	'fires' : begin
		XInd = where(grid.xmid ge   60.0 and grid.xmid le 142.0)
		YInd = where(grid.ymid ge   40.0 and grid.ymid le  60.0)
		Title='Fires'
	end
   endcase

   R = intarr( N_Elements(Grid.XMid), N_Elements(Grid.YMid) )

   for i = 0, n_elements(XInd)-1 do begin
       for j = 0, n_elements(YInd)-1 do begin
	   R(XInd(i), YInd(j)) = 1
       endfor
   endfor


   myct,/WhBu
   TVMap, R, Grid.Xmid, Grid.YMid, $
      /sample,  /CONTINENTS, /COASTS, $
      /ISOTROPIC,  grid=1, /keepaspectratio, $
      Title=Title,maxdata=2


   ; Close postscript file
   if n_elements(filename) ne 0 then close_device

end

