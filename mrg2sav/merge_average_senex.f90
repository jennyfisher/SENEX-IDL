PROGRAM merge_average_senex

        ! Used to merge different PI files from senex 
        ! and then get time-averaged output
        ! Lei Zhu, 07/25/2013

        !=================================================================
        ! input : senex PI files	
        ! output: merge file and time-averaged file
        !=================================================================

        CHARACTER       control_filename*200, temp_char*200, senex_folder*200, cdate*10
        CHARACTER       input_filename*200, output_filename*200, val_name_merge*10000
        INTEGER         FileUnit, p, p1, k, line, temp_ID, Nfiles, Afiles, Nvars,Nline,Nline_avg, Ind_GpsAlt 
        INTEGER         Year, Month, Day, DOY
	REAL            average_time
	LOGICAL         FileFlag2
	LOGICAL, ALLOCATABLE      :: FileFlag(:)
        CHARACTER*50, ALLOCATABLE :: chem(:), val_name(:)
        INTEGER, ALLOCATABLE      :: headlines(:), var(:), points(:), merge_flag(:)
	REAL, ALLOCATABLE         :: wavetime(:,:), val(:,:)
        REAL, ALLOCATABLE         :: val_avg(:,:), hh(:), mm(:)
!---------------------------------------------------------------------------
! Start from here
!---------------------------------------------------------------------------

        ! Print log        
        PRINT*, ""
        PRINT*, ""
        PRINT*, "=================   MERGE SENEX DAILY DATA  ================"
        PRINT*, ""
        PRINT*, ""
!---------------------------------------------------------------------------
! Read control file to get user input

        ! Open control file
        FileUnit = 1
        control_filename = "merge_average_senex.inp"
        CALL Check_File(control_filename, FileUnit)

        ! Skip the header: lines starting with "#"
5       CONTINUE
        READ(FileUnit,*,ERR=8) temp_char
        IF(temp_char(1:3)=="#ID") THEN
          GOTO 6
        ELSE
          GOTO 5
        ENDIF
6       CONTINUE

        ! Read in keywords set by user
        PRINT*, ""
        PRINT*, " ------------   Reading from .inp file   ----------"
        READ(FileUnit,500) temp_ID, temp_char, senex_folder
        PRINT*, " Main SENEX daily data folder  : ", TRIM(senex_folder)
        READ(FileUnit,501) temp_ID, temp_char, cdate
        PRINT*, "  Process data at              : ", TRIM(cdate)
        READ(FileUnit,502) temp_ID, temp_char, Nfiles
        PRINT*, "  # of file types              : ", Nfiles
        ! Allocate data
        ALLOCATE ( chem(Nfiles) )
        ALLOCATE ( headlines(Nfiles) )
        ALLOCATE ( var(Nfiles) )
        ALLOCATE ( points(Nfiles) )
        ALLOCATE ( FileFlag(Nfiles) )
        ! Continue to read in keywords
        DO p = 1,Nfiles 
          READ(FileUnit,501) temp_ID, temp_char, chem(p)
          PRINT*, "    Type", p,"               : ", TRIM(chem(p))
        ENDDO
        READ(FileUnit,503) temp_ID, temp_char, average_time
        PRINT*, "  Average time (min)           : ", average_time
        PRINT*, " ----------------------------------------------------------"
        PRINT*, ""
        GOTO 7

500     FORMAT(I3,2x,A29,x,A100)
501     FORMAT(I3,2x,A29,x,A100)
502     FORMAT(I3,2x,A29,x,I3)
503     FORMAT(I3,2x,A29,x,F7.2)

8       CONTINUE
        PRINT*,"ERROR in reading oversampling.inp"
        STOP

7       CONTINUE
        CLOSE(FileUnit)
        
!---------------------------------------------------------------------------
! Get # of vars and lines/points

        ! Loop the files
        Nvars = 0 ! Total # of vars in all files
        Afiles = 0

        DO p = 1, Nfiles
          FileUnit =50+p
          ! First try RA
          input_filename = TRIM(senex_folder)//TRIM(cdate)//"/"//TRIM(chem(p))//"_NP3_"//TRIM(cdate)//"_RA.ict"
          INQUIRE(FILE=TRIM(input_filename),EXIST=FileFlag(p))

          IF(FileFlag(p)) THEN
            Afiles = Afiles + 1
            OPEN(FileUnit,file=input_filename)
            WRITE(*,*),'Reading in: ', TRIM(input_filename)
          ELSE
          ! If it doesn't work, then try RB
          input_filename = TRIM(senex_folder)//TRIM(cdate)//"/"//TRIM(chem(p))//"_NP3_"//TRIM(cdate)//"_RB.ict"
            INQUIRE(FILE=TRIM(input_filename),EXIST=FileFlag(p))
              IF(FileFlag(p)) THEN
                Afiles = Afiles + 1
                OPEN(FileUnit,file=input_filename)
                WRITE(*,*),'Reading in: ', TRIM(input_filename)
              ELSE    
                WRITE(*,*),'  NOT found: ', TRIM(chem(p)), " data at: ", TRIM(cdate)
              ENDIF
          ENDIF
        ENDDO

        IF(Afiles == 0) THEN
          PRINT*, "ERROR: NO file for: ", TRIM(cdate), " at all!"
          STOP
        ENDIF
        
        DO p = 1, Nfiles
          FileUnit =50+p  
          ! Get # of head lines and var in each file
          IF(FileFlag(p)) THEN
            READ(FileUnit,*) headlines(p), temp_ID
            DO line = 1, headlines(p)-2
              IF(line == 9) THEN
                READ(FileUnit,*) var(p)
               Nvars = Nvars + var(p)
              ELSE
                READ(FileUnit,*) temp_char
              ENDIF
            ENDDO

          ! Read var names, but not stored now
            READ(FileUnit,*) temp_char
            !PRINT*, TRIM(temp_char)

          ! Get # of points/lines
            points(p) = 0
11	    CONTINUE
            points(p) = points(p) + 1
            READ(FileUnit,*, END=9, ERR=10)
            GOTO 11

10	    CONTINUE
            PRINT*, "ERROR in reading: ", TRIM(input_filename), " at line: ", headlines(p)+points(p)
            STOP

9	    CONTINUE
            points(p) = points(p) -1
          ENDIF
        CLOSE(FileUnit)
        ENDDO

        ! Check files have the same # of points
        sum_temp = 0
        count_temp = 0
        DO p = 1, Nfiles
          IF(FileFlag(p)) THEN
            sum_temp = sum_temp + points(p)
            count_temp = count_temp +1
          ENDIF
        ENDDO

        DO p =1, Nfiles
          IF(FileFlag(p)) THEN
            IF(points(p)/=(sum_temp/count_temp)) THEN
              PRINT*, "ERROR: Files have different total # of points"
              STOP
            ENDIF
          ENDIF
        ENDDO

        ! Allocate data
        ALLOCATE ( wavetime(Nfiles,points(1)) )
        ALLOCATE ( val(points(1),Nvars) )
        ALLOCATE ( val_name(Nvars) )

        ! Read in values
        p1 = 0
        DO p =1, Nfiles 

          ! Get val name index
          IF(p==1) THEN
            p1 = 0
          ELSE      
            p1 = p1 + var(p-1)
          ENDIF

          IF(FileFlag(p)) THEN
            FileUnit =50+p
            ! First try RA
            input_filename = TRIM(senex_folder)//TRIM(cdate)//"/"//TRIM(chem(p))//"_NP3_"//TRIM(cdate)//"_RA.ict"
            INQUIRE(FILE=TRIM(input_filename),EXIST=FileFlag2)
            IF(FileFlag2) THEN
              OPEN(FileUnit,file=input_filename)
            ELSE
            ! If it doesn't work, then try RB
              input_filename = TRIM(senex_folder)//TRIM(cdate)//"/"//TRIM(chem(p))//"_NP3_"//TRIM(cdate)//"_RB.ict"
              INQUIRE(FILE=TRIM(input_filename),EXIST=FileFlag2)
                IF(FileFlag2) THEN
                  OPEN(FileUnit,file=input_filename)
                ELSE    
                  WRITE(*,*),'  NOT found: ', TRIM(chem(p)), " data at: ", TRIM(cdate)
                  PRINT*, "ERROR!"
                  STOP
                ENDIF
            ENDIF
          
            ! Skip the header
            DO line = 1, headlines(p)-1
              READ(FileUnit,*) temp_char
            ENDDO

            ! Read var names
            READ(FileUnit,*) temp_char, (val_name(k),k=p1+1,p1+var(p))

            ! Read val
            DO line = 1,points(1)
              READ(FileUnit,*) wavetime(p,line), (val(line, k),k=p1+1,p1+var(p))
            ENDDO

          CLOSE(FileUnit)
          ENDIF
        ENDDO

        ! Get DOY
        read( cdate(1:4), '(I4)') Year
        read( cdate(5:6), '(I2)') Month
        read( cdate(7:8), '(I2)') Day
        CALL Get_DOY(Year,MOnth,Day,DOY)        

        ! Print var names
        DO p =1, Nfiles
          IF(p==1) THEN
            p1 = 0
          ELSE
            p1 = p1 + var(p-1)
          ENDIF
          PRINT*, p, chem(p)(1:15),var(p)
          DO line = p1+1, p1+var(p)
            PRINT*, "             ", TRIM(val_name(line))
          ENDDO
        ENDDO

        ! Merge val_name
        val_name_merge = "UTC"//" JDAY "//"PRESSURE"//" "//TRIM(val_name(1))
        DO k = 2, Nvars

          IF(TRIM(val_name(k))=="GpsLat") THEN
            val_name(k)="LATITUDE"
          ENDIF

          IF(TRIM(val_name(k))=="GpsLon") THEN
            val_name(k)="LONGITUDE"
          ENDIF

          IF(TRIM(val_name(k))=="GpsAlt") THEN
            Ind_GpsAlt=k
          ENDIF

          val_name_merge = TRIM(val_name_merge)//" "//TRIM(val_name(k))
        ENDDO
        IF(LEN_TRIM(val_name_merge)==10000) THEN
          PRINT*, "Possible ERROR: Merged Val names could be larger than 10000"
          PRINT*, "Increase the length of val_name_merge"
          STOP
        ENDIF

        ! Open output file, save the orginal merge file
        FileUnit =1
        output_filename = TRIM(senex_folder)//"merge_original/merge_senex_original_wp3d_"//TRIM(cdate)//".ict"
        OPEN(FileUnit,FILE=TRIM(output_filename))

        ! Write the header
        PRINT*, "Saving original data ..."
        WRITE(FileUnit, '(I1,x,I4)') 6, 1001
        WRITE(FileUnit, '(A)') "Merged by merge_average_senex.f90, by Lei Zhu"
        WRITE(FileUnit, '(A)') "Meaning of Var name please refer to SENEX web"
        WRITE(FileUnit, '(A)') "Total number of available var: "
        WRITE(FileUnit,' (I3)') Nvars+3 !Timewave, DOY and Pressure are not counted as a var
        WRITE(FileUnit, '(A)') TRIM(val_name_merge)

        ! Check each point has the same wavetime
        DO line = 1, points(1)
          sum_temp = 0
          count_temp = 0
          DO p=1, Nfiles
          IF(FileFlag(p)) THEN
            sum_temp = sum_temp + wavetime(p,line)
            count_temp = count_temp + 1
          ENDIF
          ENDDO
        
          DO p=1, Nfiles
          IF(FileFlag(p)) THEN
            IF(wavetime(p,line)/=(sum_temp/count_temp)) THEN
              PRINT*, "ERROR: Files have different wavetime"
              PRINT*, "Stop at line: ", line
              PRINT*, "Stop at file: ", p
              STOP
            ENDIF
          ENDIF
          ENDDO
        ENDDO

        ! Write output
        DO line = 1, points(1)
          WRITE(FileUnit,'(f8.1,x,i4,x,f14.3,x,*(f14.4,x))') wavetime(1,line),DOY,val(line,Ind_GpsAlt), (val(line,k),k=1,Nvars)
        ENDDO

        CLOSE(FileUnit)

!---------------------------------------------------------------------------
! Get temporal avergae

        ! Check the temperoal resolution
        IF( points(1) /= ( wavetime(1,points(1))-wavetime(1,1)+1) ) THEN
          PRINT*, "Check temperoal resolution"
          STOP
        ENDIF

        ! Get Nline_avg
        ALLOCATE ( val_avg(1440, Nvars) )
        ALLOCATE ( merge_flag (1440) )
        ALLOCATE ( hh(points(1)) )
        ALLOCATE ( mm(points(1)) )

        ! Get hh, mm and ss for each point
        DO p =1, points(1)
          hh(p) = FLOOR( wavetime(1,p)/(3600) ) 
          mm(p) = FLOOR( (wavetime(1,p)-hh(p)*3600)/60)
        ENDDO

        ! Get average
        merge_flag = 0

        DO k = 1, Nvars
        DO p = 1, 1440
        sum_temp = 0
        count_temp = 0
        DO line = 1, points(1)
        hh_temp = FLOOR(p/60.0)
        mm_temp = p-60*hh_temp
        IF( hh(line)==hh_temp .AND. mm(line)==mm_temp .AND. val(line,k)/=-9999 ) THEN
          sum_temp = sum_temp + val(line,k)
          count_temp = count_temp + 1
        ENDIF
        ENDDO
        IF(count_temp/=0) THEN
          val_avg(p,k)= sum_temp/count_temp
          merge_flag(p) = 1
        ELSE
          val_avg(p,k)=-9999
        ENDIF
        ENDDO
        ENDDO


        ! Open output file, save the 60s merge file
        FileUnit =1
        output_filename = TRIM(senex_folder)//"merge_60s/mrg60_wp3d_"//TRIM(cdate)//".ict"
        OPEN(FileUnit,FILE=TRIM(output_filename))

        ! Write the header
        PRINT*, "Saving time averaged data ..."
        WRITE(FileUnit, '(I1,x,I4)') 6, 1001
        WRITE(FileUnit, '(A)') "Merged by merge_average_senex.f90, 60s averaged data,  by Lei Zhu"
        WRITE(FileUnit, '(A)') "Meaning of Var name please refer to SENEX web"
        WRITE(FileUnit, '(A)') "Total number of available var: "
        WRITE(FileUnit,'(I3)') Nvars+3 !Timewave, DOY and Pressure are not counted as a var
        WRITE(FileUnit, '(A)') TRIM(val_name_merge)
        
        ! Write val
        DO line = 1, 1440
          IF(merge_flag(line)==1) THEN
            WRITE(FileUnit,'(f8.1,x,i4,x,f14.4,x,*(f14.4,x))') line*60.0,DOY,val_avg(line,Ind_GpsAlt),(val_avg(line,k),k=1,Nvars)
        ENDIF
        ENDDO

        CLOSE(FileUnit)

        ! Deallocate arraies
        DEALLOCATE ( chem )
        DEALLOCATE ( headlines )
        DEALLOCATE ( var )
        DEALLOCATE ( points )
        DEALLOCATE ( wavetime )
        DEALLOCATE ( val )
        DEALLOCATE ( val_name )
        DEALLOCATE ( val_avg )
        DEALLOCATE ( hh )
        DEALLOCATE ( mm )
        DEALLOCATE ( merge_flag )
!---------------------------------------------------------------------------
END PROGRAM merge_average_senex

!---------------------------------------------------------------------------
! Check file exist or not
SUBROUTINE Check_File(filename, FileUnit)
  CHARACTER*150 filename
  INTEGER FileUnit
  LOGICAL FileFlag
  INQUIRE(FILE=TRIM(filename),EXIST=FileFlag)
  IF(FileFlag) THEN
    OPEN(FileUnit,file=filename)
    WRITE(*,*),'Reading in: ', TRIM(filename)
  ELSE
    WRITE(*,*),TRIM(filename),' NOT found'
    STOP
  ENDIF
END SUBROUTINE

SUBROUTINE Get_DOY(YEAR,MONTH,DAY,JD)
!COMPUTES THE JULIAN DATE (JD) GIVEN A GREGORIAN CALENDAR
    INTEGER YEAR,MONTH,DAY,I,J,K, JD1, JD2, JD

    I= YEAR
    J= MONTH
    K= DAY
    JD1= K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12)/12 & 
         -3*((I+4900+(J-14)/12)/100)/4

    I =YEAR
    J = 1
    K = 1
    JD2= K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12)/12 &
         -3*((I+4900+(J-14)/12)/100)/4

    JD= JD1-JD2+1
END SUBROUTINE
