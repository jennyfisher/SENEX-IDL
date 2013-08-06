;; CDH: 
;; This program came from Dylan Millet on 27 August 2007 and is based
;; on code written for ICARTT (by Qinbin Li?)
;; 

@get_doy

pro planelog2sav, PlatformNum

; ======================================================
; Input data
; ======================================================

   If n_elements(PlatformNum) NE 1 Then Stop, 'Must pass PlatformNum'

   Case PlatformNum Of
      1 :  Platform = 'P3B00'
      2 :  Platform = 'P3B10'
      3 :  Platform = 'P3B20'
      4 :  Platform = 'DC800'
      5 :  Platform = 'C130'
      6 :  Platform = 'DC8'
      7 :  Platform = 'UTP_2003'
      8 :  Platform = 'UTP_2004'
      9 :  Platform = 'TOPSE'
      10:  Platform = 'PEMTB'
      11:  Platform = 'UWB'
   Endcase

   ;Begin
;      Platform = ''
;      Print, 'Enter platform - should be one of:'
;      Print, '  P3B10     (for ITCT2K2)'
;      Print, '  P3B00     (for ICARTT)'
;      Print, '  DC800     (for INTEX-A)'
;      Print, '  C130      (for INTEX-B)'
;      Print, '  DC8B0     (for INTEX-B)'
;      Print, '  UTP_2003  (for UTOPIHAN_2003)'
;      Print, '  UTP_2004  (for UTOPIHAN_2004)'
;      Print, '  TOPSE     (for TOPSE_2000)'
;      Read, Platform, Prompt='Platform: '
;   Endif

;   Platform = Strtrim(String(Platform), 2)


   FilePath = '~/data/INTEXB/gc_data/v7-04-06.4x5.totHg/plane_out/'

   ;; Create separate datasets for UTOPIHAN 2003 (June,July) 
   ;; and 2004 (March)
   If Platform Eq 'UTP_2003' Then Begin
      
      OutName  = Platform
      Platform = 'UTP00'
      FileList = [ MFindFile ( FilePath + 'plane.log.2006060*' ), $
                   MFindFile ( FilePath + 'plane.log.2006071*' ), $
                   MFindFile ( FilePath + 'plane.log.2006072*' )  ]
      
   EndIf Else If Platform Eq 'UTP_2004' Then Begin
      
      OutName  = Platform
      Platform = 'UTP00'
      FileList = [ MFindFile ( FilePath + 'plane.log.2006030*' ), $
                   MFindFile ( FilePath + 'plane.log.2006031*' ) ]
                   
   Endif Else Begin

      OutName = Platform
      Command = 'find ' + FilePath + ' -exec grep -q ' + $
         '"' + Platform + '" ''{}'' \; -print'

      print, 'Searching for plane.log files containing ', Platform, '...'

      spawn, command, FileList

   Endelse    
   
   ;; Sort FileList
   FileList = FileList[Sort(FileList)]
   
   print, strtrim(string(n_elements(FileList)), 2), ' files found.'
      
   ;; For testing
   ;;FileList = FileList[7]
   
; ======================================================
; Read in plane.log files
; ======================================================

   First = 1L

   ;; Loop through files
   For f=0L, n_elements(FileList)-1L Do Begin

      ThisFile = FileList[f]

      print, 'Reading in ', ThisFile, $
         ' (file ', strtrim(string(f+1), 2), $
         '/', strtrim(string(n_elements(FileList)), 2), ')'

      CTM_Cleanup
      
      ; Use READ_PLANEFLIGHT_AND_TRACERINFO in order to get standard GEOS-Chem
      ; tracer names rather than default ND40 names
      ;; PLANE = CTM_READ_PLANEFLIGHT( thisfile )
      PLANE = READ_PLANEFLIGHT_AND_TRACERINFO( thisfile,'./trancerinfo.dat')

      ;; Trim any leading or trailing blanks
      PlanePlatform = Strtrim(Plane[*].Platform, 2)

      INDEX = WHERE( PlanePlatform eq Platform,  Ct)

      IF (Ct LE 0) THEN BEGIN

         print, 'NO ' + Platform + ' DATA FOR THIS DATE...'

      ENDIF ELSE BEGIN

         NVARS           =  PLANE[INDEX].NVARS
         NPOINTS         =  PLANE[INDEX].NPOINTS

         ;; Resize arrays
         DATE_1          =  PLANE[INDEX].DATE     [0:NPOINTS-1  ]
         TIME_1          =  PLANE[INDEX].TIME     [0:NPOINTS-1  ]
         LAT_1           =  PLANE[INDEX].LAT      [0:NPOINTS-1  ]
         LON_1           =  PLANE[INDEX].LON      [0:NPOINTS-1  ]
         PRESS_1         =  PLANE[INDEX].PRESS    [0:NPOINTS-1  ]
         VARNAMES_1      =  PLANE[INDEX].VARNAMES [0:NVARS-1    ]
         DATA_1          =  PLANE[INDEX].DATA     [0:NPOINTS-1,*]
         DATA_1          =  DATA_1         [*, 0:NVARS-1 ]

; ======================================================
; APPEND
; ======================================================

         IF (FIRST) THEN BEGIN
            DATE            =  [DATE_1    ]
            TIME            =  [TIME_1    ]
            LAT             =  [LAT_1     ]
            LON             =  [LON_1     ]
            PRESS           =  [PRESS_1   ]
            DATA            =  [DATA_1    ]
         ENDIF ELSE BEGIN
            DATE            =  [DATE,      DATE_1    ]
            TIME            =  [TIME,      TIME_1    ]
            LAT             =  [LAT,       LAT_1     ]
            LON             =  [LON,       LON_1     ]
            PRESS           =  [PRESS,     PRESS_1   ]
            DATA            =  [DATA,      DATA_1    ]
         ENDELSE

         FIRST =  0L
         CTM_CLEANUP

      ENDELSE
   ENDFOR

   ;; Check for missing value flags
   Missing = Where(Data Eq -1000, Ct)
   If Ct Gt 0 Then Data[Missing] = !VALUES.F_NAN

   ;; remove pressure glitch for MILAGRO data
   if platform eq 'C130' then begin
      glitch = where ( press lt 200., ct )
      if ct gt 0. then begin
         press[glitch] = !values.f_nan
         data[glitch, *] = !values.f_nan
      endif
   endif


   ;; Calculate decimal day of year
   M    = Floor(Date / 100.) Mod 100
   D    = Date Mod 100
   Y    = Floor(Date / 1E4)
   H    = Floor(Time / 100.)
   Min  = Time Mod 100
      
   JDAY = Get_Doy(Year=Y, Month=M, Day=D, Hour=H, Minute=Min)


   GC =  {   DATE       : DATE,        $
             TIME       : TIME,        $
             JDAY       : JDAY,        $
             LAT        : LAT,         $
             LON        : LON,         $
             PRESS      : PRESS,       $
             VARNAMES   : VARNAMES_1,  $
             DATA       : DATA          }

   HELP,  GC,  /STR
 
   CASE OutName OF

      'P3B20'    : OUTFILE =  'mWP3-TEXAQS_2006.sav'
      'P3B10'    : OUTFILE =  'mWP3-ITCT2K2_2002.sav'
      'P3B00'    : OUTFILE =  'mWP3-ICARTT_2004.sav'
      'DC800'    : OUTFILE =  'mDC8-INTEXA_2004.sav'
      'PEMTB'    : OUTFILE =  'mDC8-PEMTB_1999.sav'
      'C130'     : OUTFILE =  'mC130-INTEXB_2006.sav'
      'DC8'      : OUTFILE =  'mDC8-INTEXB_2006.sav'
      'UTP_2003' : OUTFILE =  'mLEAR-UTP_2003.sav'
      'UTP_2004' : OUTFILE =  'mLEAR-UTP_2004.sav'
      'TOPSE'    : OUTFILE =  'mC130-TOPSE_2000.sav'
      'UWB'      : OUTFILE =  'mUWB-INTEXB_2006.sav'
   Endcase

   Save,GC, filename= Outfile, /COMPRESS

End


