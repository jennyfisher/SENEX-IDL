pro arctas_o3_map,flightdates,level=level,_extra=_extra

if n_elements(flightdates) ne 1 then begin
   print, $
   '*** Error: Must specify one and only one flightdate!'
   return
endif
if n_elements(level) eq 0 then level = 0

; Specify filename for GEOS-Chem output
file = '/as2/pub/ftp/pub/geos-chem/NRT-ARCTAS/bpch/'+ $
       'ctm.bpch.'+flightdates

; Echo info
print,'*** Reading o3 data from: ',file

; Get o3 data
ctm_get_data, datainfo, 'IJ-AVG-$', filename = file, tracer = 44
o3 = *(datainfo.data)
unit = datainfo.unit

; Get grid information to use in plotting
getmodelandgridinfo,datainfo,modelinfo,grid

; Get indices for ARCTAS plotting region
YInd = where (Grid.YMid gt 30)
XMid = Grid.XMid
YMid = Grid.YMid(YInd)
ZMid = Grid.ZMid

myct,/WhGrYlRd,no3lors=30
title='GEOS-Chem O3 on '+flightdates+ ', '+ $
      string(ZMid(level),format='(f4.1)')+ ' km'


; Make map of o3
TVMap, o3(*,YInd,level),XMid,YMid,/orthographic,           $
       /isotropic,/countries,/continents,/grid,/nogylabels,  $
       mparam=[90,250,0],limit=[45,150,90,250,45,350,45,250],$
       /cbar,CBposition=[0.2,0.1,0.8,0.13],divisions=5,      $
       title=title,unit=unit,_extra=_extra

end
