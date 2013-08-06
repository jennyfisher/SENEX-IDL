pro mrgsav2geosgrid, DirIn, DirOut, Platform, AvgTime, GridType, $
                     FileInTemplate=FileInTemplate,InTime=InTime

  ; Note: AvgTime is in minutes! InTime is in seconds!
  ; Merge files are generally one minute, so if no input time is
  ; specified use 60s
  if (n_elements(InTime) eq 0) then InTime=60
  ; Convert AvgTime and InTime to strings for use in filenames
  sInTime = StrTrim(String(InTime),1)
  sAvgTime = StrTrim(String(AvgTime),1)+'m'

  ; In order to search for files matching the template, we need to
  ; replace the strings in the FileInTemplate with wildcards

  If Keyword_Set( FileInTemplate ) then begin
      FilesIn = MFindFile( DirIn+'/'+FileInTemplate )
  endif else begin
      FilesIn = MFindFile( DirIn+'/*.sav' )
  endelse

  nFiles = n_elements( FilesIn )

  For F=0, nFiles-1L do begin

    Print, '### Processing file '+FilesIn[F]+' ...'

    restore, FilesIn[F]

    s = 'avg_geosgrid, /ICTformat, GridType=GridType, '+$
      'TimeStep='+String(AvgTime)+','+$
      'PlaneIn='+Platform+', Plane_GEOS=Plane_GEOS'

    status = Execute( s )    

    s = Platform +' = Plane_GEOS'
    status = Execute( s )

    FileOut = DirOut +'/'+ Extract_Filename( FilesIn[F] )
    if ( sAvgTime ne sInTime+'m' ) then begin
       FileOut = replace_token(FileOut,sInTime,sAvgTime,delim="")
    endif else begin
       FileOut = replace_token(FileOut,sInTime+'_dc8',sAvgTime+'_dc8',delim="")
       FileOut = replace_token(FileOut,sInTime+'_er2',sAvgTime+'_er2',delim="")
       FileOut = replace_token(FileOut,sInTime+'_wp3d',sAvgTime+'_wp3d',delim="")
    endelse

    Print, '### Writing file '+FileOut+' ...'


    s = 'Save, /verbose, '+Platform+', filename= "'+FileOut+'"'
    status = Execute( s )

  endfor

end
