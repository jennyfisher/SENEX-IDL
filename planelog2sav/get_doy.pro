function get_doy, Year=Year, Month=Month, Day=Day, Hour=Hour, Minute=Minute

   ;; Default values
   If n_elements (Year)  Lt 1 Or $
      n_elements (Month) Lt 1 Or $
      n_elements (Day)   Lt 1 Then $
      Stop, 'Error in Get_Doy: Must pass Year, Month and Day'

   If n_elements (Hour)  Lt 1 Then $
      Hour= 0.0D0

   If n_elements (Minute)  Lt 1 Then $
      Minute= 0.0D0


   DoY = JulDay(float(Month), float(Day), float(Year)) - $
      JulDay(12., 31., Year-1.) + $
         Hour/24.0D0 + Minute/(60.0D0*24.0D0)

   Return, DoY

End
