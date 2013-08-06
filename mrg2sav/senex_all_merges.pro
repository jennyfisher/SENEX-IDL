
startDir = file_expand_path( '.' )

read_all_merges, 'WP3D', Dir=!SENEX+'/field_data/WP3D/merge_60s/'
close,/all

; Now convert the 60s merges to averages over the GEOS5 grid and timetep
CD, startDir

DirIn  = !SENEX+'/field_data/WP3D/merge_60s'
DirOut = !SENEX+'/field_data/WP3D/merge_10m_0.25x0.3125'
mrgsav2geosgrid, DirIn, DirOut, $
  'WP3D',10, CTM_Type('GEOS5_47L',res=0.25)

s = "rename.pl 's/(.*)mrg60(.*)/$1mrg10m$2/' "+DirOut+"/*.sav"

close,/all

end
