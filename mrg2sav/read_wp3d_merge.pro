
function WP3D_LineSplit, Line

;====================================================================
; Function WP3D_LINESPLIT splits a line by spaces or horizontal tabs.
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

pro read_wp3d_merge, FileNames

   ;; =============================
   ;; Specify a file if none is given
   ;; =============================

   if ( N_Params() EQ 0 ) Then $
      FileNames = [ '~/data/INTEXB/mrg_Hg/mrgUNHMERC_dc8_20060319_R3']

   ;; =============================
   ;; Read in datafile
   ;; =============================

   ;; for testing
   ;;FileNames = FileNames[2]

   For F=0, n_elements(FileNames)-1L Do Begin
      
      InFile  = FileNames[F] + '.ict'
      OutFile = FileNames[F] + '.sav'
      
      print, ' ### Processing ', InFile

      ;; Logical flag is TRUE if DC8 data exists
      Is_Data  = ( File_Exist( InFile) )
      if not Is_Data then Stop,  'Data file not found!'

      ;; Only proceed if data exists for this date
      If ( Is_Data ) then begin

         ; Get number of lines in file
         s=query_ascii( InFile, info )
         nlines = info.lines

         Open_File, InFile, Ilun_Flt, /Get_LUN

         Line = ' '

         ;; Read first line
         ReadF, Ilun_Flt, Line

         ;; Split line by spaces or tabs
         Result = WP3D_LineSplit( Line )

         ;; Get # of lines in the header
         N_Hdr = Long( Result[0] )

         ;; Skip header
         for N = 0L, N_Hdr-2L do begin

            ;; Read each lines
            ReadF, Ilun_Flt, Line

            ;; Get number of variables
            if ( N eq 3 ) then begin

               ;; Split the line
               Result = WP3D_LineSplit( Line )

               ;; Save into variables
               NVars  = Long( Result[0] )

               ;; Undefine
               Undefine, Result

            endif


            ;; Parse the line w/ the data field names (bmy, 3/6/06)
            if ( N eq N_Hdr-2L ) then begin

               ;; Split the line
               VarNames  = WP3D_LineSplit( Line )
	       ;  Be careful: Pressure data is GpsAlt, need to convert it to mressure!
	       IndAlt    = Where( VarNames eq 'PRESSURE' )
            endif

         endfor

         ;; Read in data
         ;; Preallocate data to avoid memory lags (cdh, 12/7/2009)
         ;Data = dblarr( nVars+1, nlines-n_hdr ); changed by lei, 07/29/2013
	 Data = dblarr( nVars, nlines-n_hdr )
	 ;print, nlines, NVars,VarNames
         i=0L
         while ( not EOF( Ilun_Flt ) ) do begin
            ReadF, Ilun_Flt, Line
            junk   = StrBreak( Line, ' ')
            data[*,i] = double( junk )
	    IF(data[IndAlt,i] ne -9999 ) THEN $
	      ;print, data[IndAlt,i]
	      data[IndAlt,i]= Float( USSA_PRESS(data[Indalt,i]/1000D ))
	      ;print, data[IndAlt,i]
            i=i+1L
         endwhile
; CDH, before 12/7/2009
;         First = 1
;         while ( not EOF( Ilun_Flt ) ) do begin
;            ReadF, Ilun_Flt, Line
;            junk   = StrBreak( Line, ' ' )
;
;            if ( First ) then data = [double( junk )] $
;            else data = [ [data], [double( junk )] ]
;            First = 0
;            if ( First ) then data = [double( junk )] $
;            else data = [ [data], [double( junk )] ]
;            First = 0
;         endwhile

         Close,    Ilun_Flt
         Free_LUN, Ilun_Flt


         ;; Put into a structure
         First_Time = 1L
         FOR V = 0L, (NVars-1) Do Begin

            Name = VarNames[V]
	    ;print, Name

            Ind = where( VarNames eq Name )
            ThisData     = [reform( data[Ind[0], *])]


            ;; ==========================
            ;; Remove illegal characters from vector name
            ;; ==========================

            If ( Name EQ 'Geometric_(Radar)_Altitude_(APN-232)' ) Then $
               Name = 'ALTR'
            If ( Name EQ 'Corrected_GPS_Altitude' Or $
                 Name EQ 'ALTITUDE_GPS' ) Then Name = 'ALTGPS'
            If ( Name EQ '1_1_1-trichloroethane_TOGA') Then $
               Name = 'trichloroethane_TOGA'
            If ( Name EQ '2-Pentanone_TOGA') Then Name = 'Penta2one_TOGA'
            If ( Name EQ '3-Pentanone_TOGA') Then Name = 'Penta3one_TOGA'
            If ( Name EQ '2-Hexanone_TOGA' ) Then Name = 'Hexa2one_TOGA'
            If ( Name EQ '3-Hexanone_TOGA' ) Then Name = 'Hexa3one_TOGA'
            If ( Name EQ '2-BuONO2_UCI'    ) Then Name = 'BuONO2_UCI'
            If ( Name EQ 'Frational_Day'        ) Then Name = 'Fractional_Day'
            If ( Name EQ 'J[O3->O2+O(1D)]'      ) Then Name = 'JO3_O2_O1D'
            If ( Name EQ 'J[NO2->NO+O(3P)]'     ) Then Name = 'JNO2'
            If ( Name EQ 'J[N2O5->NO3+NO+O(3P)]') Then Name = 'JN2O5_NO3_NO_O'
            If ( Name EQ 'J[N2O5->NO3+NO2]'     ) Then Name = 'JN2O5_NO3_NO2'
            If ( Name EQ 'J[H2O2->2OH]'         ) Then Name = 'JH2O2'
            If ( Name EQ 'J[HNO2->OH+NO]'       ) Then Name = 'JHNO2'
            If ( Name EQ 'J[HNO3->OH+NO2]'      ) Then Name = 'JHNO3'
            If ( Name EQ 'J[CH2O->H+HCO]'       ) Then Name = 'JCH2O_H_HCO'
            If ( Name EQ 'J[CH2O->H2+CO]'       ) Then Name = 'JCH2O_H2_CO'
            If ( Name EQ 'J[CH3CHO->CH3+HCO]'   ) Then Name = 'JCH3CHO_CH3_HCO'
            If ( Name EQ 'J[CH3CHO->CH4+CO]'    ) Then Name = 'JCH3CHO_CH4_CO'
            If ( Name EQ 'J[C2H5CHO->C2H5+HCO]' ) Then Name = 'JC2H5CHO'
            If ( Name EQ 'J[CHOCHO->products]'  ) Then Name = 'JCHOCHO_products'
            If ( Name EQ 'J[CHOCHO->HCO+HCO]'   ) Then Name = 'JCHOCHO_HCO_HCO'
            If ( Name EQ 'J[CHOCHO->CH2O+CO]'   ) Then Name = 'JCHOCHO_CH2O_CO'
            If ( Name EQ 'J[CHOCHO->H2+2CO]'   ) Then Name = 'JCHOCHO_H2_2CO'
            If ( Name EQ 'J[CH3COCHO->products]') Then Name = 'JCH3COCHO'
            If ( Name EQ 'J[CH3COCH3->CH3CO+CH3]') Then Name = 'JCH3COCHO_CH3CO_CH3'
            If ( Name EQ 'J[CH3COCH3]'          ) Then Name = 'JCH3COCH3'
            If ( Name EQ 'J[CH3OOH->CH3O+OH]'   ) Then Name = 'JCH3OOH'
            If ( Name EQ 'J[CH3ONO2->CH3O+NO2]' ) Then Name = 'JCH3ONO2'
            If ( Name EQ 'J[PAN+hv->products]'  ) Then Name = 'JPAN'
            If ( Name EQ 'J[PAN->CH3COO2+NO2]'  ) Then Name = 'JPAN_CH3COO2_NO2'
            If ( Name EQ 'J[CH3CH2CH2CHO->C3H7+HCO]'    ) Then $
               Name = 'JCH3CH2CH2CHO_C3H7_HCHO'
            If ( Name EQ 'J[CH3CH2CH2CHO->C2H4+CH2CHOH]') Then $
               Name = 'JCH3CH2CH2CHO_C2H4_CH2CHOH'
            If ( Name EQ 'J[CH3COCH2CH3->Products]'     ) Then $
               Name = 'JCH3COCH2CH3'
            If ( Name EQ 'J[HO2NO2->HO2+NO2]'           ) Then $
               Name = 'JHO2NO2_HO2_NO2'
            If ( Name EQ 'J[HO2NO2->OH+NO3]') Then Name = 'JHO2NO2_OH_NO3'
            If ( Name EQ 'J[CH3CH2ONO2->Products]'       ) Then $
               Name = 'JCH3CH2ONO2'
            If ( Name EQ 'J[BrONO2+hv->Br+NO3]'     ) Then Name = 'JBrONO2'
            If ( Name EQ 'J[BrONO2+hv->BrO+NO2]'    ) Then Name = 'JBrONO2_BrO'
            If ( Name EQ 'J[ClONO2+hv->Cl+NO3]'     ) Then Name = 'JClONO2'
            If ( Name EQ 'J[ClONO2+hv->ClO+NO2]'    ) Then Name = 'JClONO2_ClO'
            If ( Name EQ 'J[BrCl->Br+Cl]'       ) Then Name = 'JBrCl'
            If ( Name EQ 'J[HOBr->HO+Br]'       ) Then Name = 'JHOBr'
            If ( Name EQ 'J[BrO+hv->Br+O]'         ) Then Name = 'JBrO'
            If ( Name EQ 'J[Br2->Br+Br]'        ) Then Name = 'JBr2'
            If ( Name EQ 'J[Cl2+hv->Cl+Cl]'        ) Then Name = 'JCl2'
            If ( Name EQ 'J[Br2O->products]'    ) Then Name = 'JBr2O'
            If ( Name EQ 'Na-'                  ) Then Name = 'Na'

            If ( Name EQ 'Number_Concentration_CAS_1.0-2.0_um') Then $
               Name = 'Number_Concentration_CAS_1_2_um'
            If ( Name EQ 'Number_Concentration_CAS_2.0-5.0_um') Then $
               Name = 'Number_Concentration_CAS_2_5_um'
            If ( Name EQ 'Number_Concentration_CAS_5.0-20_um' ) Then $
               Name = 'Number_Concentration_CAS_5_20_um'
            If ( Name EQ 'Number_Concentration_CAS_20-50_um'  ) Then $
               Name = 'Number_Concentration_CAS_20_50_um'
            If ( Name EQ 'Number_Concentration_CIP_50-1550_um') Then $
               Name = 'Number_Concentration_CIP_50_1550_um'
            If ( Name EQ 'Surface_Area_Density_CAS_1.0-2.0_um') Then $
               Name = 'Surface_Area_Density_CAS_1_2_um'
            If ( Name EQ 'Surface_Area_Density_CAS_2.0-5.0_um') Then $
               Name = 'Surface_Area_Density_CAS_2_5_um'
            If ( Name EQ 'Surface_Area_Density_CAS_5.0-20_um' ) Then $
               Name = 'Surface_Area_Density_CAS_5_20_um'
            If ( Name EQ 'Surface_Area_Density_CAS_20-50_um'  ) Then $
               Name = 'Surface_Area_Density_CAS_20_50_um'
            If ( Name EQ 'Surface_Area_Density_CIP_50-1550_um') Then $
               Name = 'Surface_Area_Density_CIP_50_1550_um'
            If ( Name EQ 'Volume_Density_CAS_1.0-2.0_um'      ) Then $
               Name = 'Volume_Density_CAS_1_2_um'
            If ( Name EQ 'Volume_Density_CAS_2.0-5.0_um'      ) Then $
               Name = 'Volume_Density_CAS_2_5_um'
            If ( Name EQ 'Volume_Density_CAS_5.0-20_um'       ) Then $
               Name = 'Volume_Density_CAS_5_20_um'
            If ( Name EQ 'Volume_Density_CAS_20-50_um'        ) Then $
               Name = 'Volume_Density_CAS_20_50_um'
            If ( Name EQ 'Volume_Density_CIP_50-1550_um'      ) Then $
               Name = 'Volume_Density_CIP_50_1550_um'

            ;; CDH Added conversions for the following names 7/27/2007
            If ( Name EQ '2-BuONO2'                           ) Then $
               Name = 'BuONO2'
            If ( Name EQ '1_2-Dichloroethane'                 ) Then $
               Name = 'Dichloro12ethane'
            If ( Name EQ '3-PenONO2'                          ) Then $
               Name = 'Pen3ONO2'
            If ( Name EQ '2-PenONO2'                          ) Then $
               Name = 'Pen2ONO2'
            If ( Name EQ '3-Methyl-2-BuONO2'                  ) Then $
               Name = 'Methyl3-2-BuONO2'
            If ( Name EQ '1-Butene'                           ) Then $
               Name = 'But1ene'
            If ( Name EQ '1_3-Butadiene'                      ) Then $
               Name = 'Buta13diene'
            If ( Name EQ '2-Methylpentane'                    ) Then $
               Name = 'Methyl2pentane'
            If ( Name EQ '3-Methylpentane'                    ) Then $
               Name = 'Methyl3pentane'
            If ( Name EQ '3-Ethyltoluene'                     ) Then $
            If ( Name EQ '1_3_5-Trimethylbenzene'             ) Then $
               Name = 'Trimethyl135benzene'
            If ( Name EQ '1_2_4-Trimethylbenzene'             ) Then $
               Name = 'Trimethyl124benzene'
            ;; End CDH Changes
            
            ;; JAF Added conversions for the following names 4/10/2008
            If ( Name EQ 'Organics<213mz'                     ) Then $
	       Name = 'Organicslt_213mz'
	    If (Name EQ 'ISOPRENE&FURAN' ) THEN Name = 'Isoprene_Furan'
	    If (Name EQ 'MVK&MACR_PTRMS' ) THEN Name = 'MVK_MACR'
	    If (Name EQ '2+3-Methylpentane' ) THEN Name = 'Methylpentane'
	    If (Name EQ '2_3-Dimethylbutane' ) THEN Name = 'Dimethylbutane'
	    If (Name EQ '2-Ethyltoluene' ) THEN Name = 'Two_Ethyltoluene'
	    If (Name EQ '1_2_3-Trimethylbenzene' ) THEN Name = $
		'One_Two_Three_Trimethylbenzene'
            ;; End JAF Changes

	    ;; lei add
            ;If (Name EQ 'CO_ppbv'    ) THEN Name = 'CO'
            ;If (Name EQ 'HCHO_ppbv'  ) THEN Name = 'HCHO'
            ;If (Name EQ 'HNO3_ppbv'  ) THEN Name = 'HNO3'
            ;If (Name EQ 'HCOOH_ppbv' ) THEN Name = 'HCOOH'
            ;If (Name EQ 'HONO_ppbv'  ) THEN Name = 'HONO'
            ;If (Name EQ 'NO_ppbv'    ) THEN Name = 'NO'
            ;If (Name EQ 'NO2_ppbv'   ) THEN Name = 'NO2'
            ;If (Name EQ 'NOy_ppbv'   ) THEN Name = 'NOy'
            ;If (Name EQ 'O3_ppbv'    ) THEN Name = 'O3'
            ;If (Name EQ 'SO2_ppbv'   ) THEN Name = 'SO2'
            ;If (Name EQ 'isoprene_pptv'  ) THEN Name = 'ISOPRENE'
	    ;; end of lei changes

            Name = StrRepl(Name, '/', '_')
            Name = StrRepl(Name, '(', '_')
            Name = StrRepl(Name, ')', '_')
            Name = StrRepl(Name, '-', '_')
            Name = StrRepl(Name, '@', '_')
            Name = StrRepl(Name, '+', '_')
            Name = StrRepl(Name, '.', '_')

            print, Name

            ;; ==========================
            ;; Deal with missing values
            ;; ==========================

            ;; ------------------
            ;; Leave this out to allow folks to treat ULOD, LLOD as they wish
            ;; ------------------
            ;; Replace < LOD values with 0
         Ind  = Where( ThisData eq -888888888, Ct )
         If (Ct gt 0) then ThisData[Ind] = 0.0D0

         ;; Check for ULOD flags
         Ind  = Where( ThisData eq -777777777, Ct )
;         If (Ct gt 0) Then Stop, 'ULOD Flags Found!!'
         If (Ct gt 0) Then Begin
            ThisData[Ind] = !Values.F_NAN
            Print, 'ULOD Flags found for ', Name
         EndIf

            ;; Replace missing values with NA
         Ind  = Where( ThisData eq -9999, Ct )
         If (Ct gt 0) then ThisData[Ind] = !VALUES.F_NAN
            ;; ------------------

            IF FIRST_TIME THEN BEGIN
               wp3d = CREATE_STRUCT(NAME, ThisData)
            ENDIF ELSE BEGIN
               wp3d = CREATE_STRUCT(wp3d, Name, ThisData)
            ENDELSE

            FIRST_TIME = 0L

            ;; End loop over variables
         ENDFOR

         ;; ==========================
         ;; Save data structure
         ;; ==========================

         save, wp3d, filename= OutFile, /COMPRESS

      Endif

   Endfor

END
