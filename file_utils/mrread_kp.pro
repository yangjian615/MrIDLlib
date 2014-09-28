; docformat = 'rst'
;
; NAME:
;       VAP_Read_Events
;
;*****************************************************************************************
;   Copyright (c) 2013, University of New Hampshire                                      ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Read Kp index data downloaded from `NOAA <ftp://ftp.ngdc.noaa.gov/STP/GEOMAGNETIC_DATA/INDICES/KP_AP>`.
;
; :Categories:
;       Kp Index, Geomagnetic Activity
;
; :Params:
;       FILENAME:           in, required, type=string
;                           Name of the file to be read.
;       SDATE:              in, optional, type=string, default=''
;                           Date of the first record to be returned.
;       EDATE:              in, optional, type=string, default=''
;                           Date of the last record to be returned.
;
; :Keywords:
;       AP:                 out, optional, type=intarr
;                           Ap or Planetary Equivalent Amplitude for 3-hour intervals on
;                               each `DATE`
;       C9:                 out, optional, type=intarr
;                           a conversion of the 0-to-2.5 range of the Cp index to one
;                               digit between 0 and 9.
;       CP:                 out, optional, type=fltarr
;                           Cp or Planetary Daily Character Figure--a qualitative estimate
;                               of overall level of magnetic activity for the day determined
;                               from the sum of the eight ap amplitudes.  Cp ranges, in steps
;                               of one-tenth, from 0 (quiet) to 2.5 (highly disturbed).
;       COUNT:              out, optional, type=long
;                           Returns the number of records found. If no records exist
;                               between `SDATE` and `EDATE`, COUNT=0 and all data will
;                               be returned.
;       DATE:               out, optional, type=strarr
;                           Date on which each record was recorded 'YYMMDD'
;       FLUX_QUAL:          out, optional, type=intarr
;                           Flux Qualifier.  "0" indicates flux required no adjustment; 
;                               "1" indicates flux required adjustment for burst in progress
;                               at time of measurement; "2" indicates a flux approximated by
;                               either interpolation or extrapolation; and "3" indicates no
;                               observation.
;       KP:                 out, optional, type=8xN fltarr
;                           Kp (or planetary range) index in 3-hour increments on each `DATE`.
;       MEAN_AP:            out, optional, type=intarr
;                           Arithmetic mean of the day's eight Ap values.
;       NDAYS_ROT_CYCLE:    out, optional, type=intarr, default
;                           Number of days within the Bartels 27-day cycle.
;       RADIO_FLUX:         out, optional, type=fltarr
;                           OTTAWA 10.7-CM SOLAR RADIO FLUX ADJUSTED TO 1 AU--measured at
;                               1700 UT daily and expressed in units of 10 to the -22 Watts/
;                               meter sq/hertz.  Observations began on February 14, 1947. 
;                               From that date through December 31, 1973, the fluxes given
;                               here don't reflect the revisions Ottawa made in 1966. NOTE: 
;                               If a solar radio burst is in progress during the observation
;                               the pre-noon or afternoon value is used (as indicated by a
;                               flux qualifier value of 1 in column 71.
;       SOLAR_ROT_NUM:      out, optional, type=intarr
;                           Bartles solar rotation number --a sequence of 27-day intervals
;                               counted continuously from February 8, 1832.
;       SUN_SPOT_NUMBER:    out, optional, type=intarr
;                           International Sunspot Number.  Records contain the Zurich num-
;                               ber through December 31, 1980, and the International Brus-
;                               sels number thereafter.
;       TIME_SERIES:        in, optional, type=boolean, default=0
;                           If set, `AP`, `KP`, and `DATE` will be returned as a
;                               time-series. Insead of one column per three-hour interval,
;                               data points will be given sequentially with center-times.
;       TOTAL_KP:           out, optional, type=intarr
;                           Sum of the eight Kp indices for the day expressed to the
;                               nearest third of a unit.
;
; :Uses:
;   Uses the following external programs::
;       MrRead_Ascii.pro
;
; :Author:
;   Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/05/30  -   Written by Matthew Argall
;       2014/06/02  -   Added the TIME_SERIES keyword. - MRA
;       2014/06/07  -   Fixed the TIME_SERIES keyword. - MRA
;       2014/09/10  -   Added the COUNT keyword. - MRA
;-
pro MrRead_Kp, filename, sDate, eDate, $
COUNT=count, $
DATE=date, $
SOLAR_ROT_NUM=solar_rot_num, $
NDAYS_ROT_CYCLE=nDays_rot_cycle, $
KP=Kp, $
TOTAL_KP=Kp_sum, $
AP=Ap, $
MEAN_AP=ap_avg, $
CP=Cp, $
C9=C9, $
SUN_SPOT_NUMBER=sun_spot_number, $
TIME_SERIES=time_series, $
RADIO_FLUX=radio_flux, $
FLUX_QUAL=flux_qual
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if n_elements(lun) gt 0 then free_lun, lun
        void = cgErrorMSG()
        return
    endif
    
    ;Defaults
    if n_elements(sDate) eq 0 then sDate = ''
    if n_elements(eDate) eq 0 then eDate = ''
    time_series = keyword_set(time_series)
    
    ;Count the number of lines
    nLines = file_lines(filename)
    
    ;Allocate memory
    temp_date       = strarr(nLines)
    solar_rot_num   = intarr(nLines)
    nDays_rot_cycle = intarr(nLines)
    Kp              = fltarr(8, nLines)
    Kp_sum          = intarr(nLines)
    Ap              = intarr(8, nLines)
    Ap_avg          = intarr(nLines)
    Cp              = fltarr(nLines)
    C9              = intarr(nLines)
    sun_spot_number = intarr(nLines)
    radio_flux      = fltarr(nLines)
    flux_qual       = intarr(nLines)
    
    ;Declare types
    dat1  = ''
    dat2  = 0
    dat3  = 0
    kp10  = 0.0
    kp11  = 0.0
    kp20  = 0.0
    kp21  = 0.0
    kp30  = 0.0
    kp31  = 0.0
    kp40  = 0.0
    kp41  = 0.0
    kp50  = 0.0
    kp51  = 0.0
    kp60  = 0.0
    kp61  = 0.0
    kp70  = 0.0
    kp71  = 0.0
    kp80  = 0.0
    kp81  = 0.0
    kt1   = 0.0
    kt2   = 0.0
    dat12 = 0
    dat13 = 0
    dat14 = 0
    dat15 = 0
    dat16 = 0
    dat18 = 0
    dat19 = 0
    dat20 = 0
    dat21 = 0
    dat22 = 0.0
    dat23 = 0
    dat24 = 0
    dat25 = 0.0
    dat26 = 0

;---------------------------------------------------------------------
; Open File and Read Data ////////////////////////////////////////////
;---------------------------------------------------------------------
    openr, lun, filename, /GET_LUN
    
    ;Read each line
    for i = 0, nLines - 1 do begin
        readf, lun, FORMAT='(a6, i4, i2, 16f1.0, f2.0, f1.0, 9i3, f3.1, i1, i3, f5.1, i1)', $
                    dat1, dat2, dat3, $
                    kp10, kp11, kp20, kp21, kp30, kp31, kp40, kp41, $
                    kp50, kp51, kp60, kp61, kp70, kp71, kp80, kp81, kt1, kt2, $
                    dat13, dat14, dat15, dat16, dat17, dat18, dat19, dat20, $
                    dat21, dat22, dat23, dat24, dat25, dat26

        ;Convert from an index to a number
        ;   0 = 0
        ;   3 = 1/3
        ;   7 = 2/3
        kp10 += (kp11 eq 3) ? 1.0/3.0 : (kp11 eq 7) ? 2.0/3.0 : kp11
        kp20 += (kp21 eq 3) ? 1.0/3.0 : (kp21 eq 7) ? 2.0/3.0 : kp21
        kp30 += (kp31 eq 3) ? 1.0/3.0 : (kp31 eq 7) ? 2.0/3.0 : kp31
        kp40 += (kp41 eq 3) ? 1.0/3.0 : (kp41 eq 7) ? 2.0/3.0 : kp41
        kp50 += (kp51 eq 3) ? 1.0/3.0 : (kp51 eq 7) ? 2.0/3.0 : kp51
        kp60 += (kp61 eq 3) ? 1.0/3.0 : (kp61 eq 7) ? 2.0/3.0 : kp61
        kp70 += (kp71 eq 3) ? 1.0/3.0 : (kp71 eq 7) ? 2.0/3.0 : kp71
        kp80 += (kp81 eq 3) ? 1.0/3.0 : (kp81 eq 7) ? 2.0/3.0 : kp81


        ;Store the data
        temp_date[i]       = dat1
        solar_rot_num[i]   = dat2
        nDays_rot_cycle[i] = dat3
        Kp[*,i]            = [kp10, kp20, kp30, kp40, kp50, kp60, kp70, kp80]
        Kp_sum[i]          = dat12
        Ap[*,i]            = [dat13, dat14, dat15, dat16, dat17, dat18, dat19, dat20]
        Ap_avg[i]          = dat21
        Cp[i]              = dat22
        C9[i]              = dat23
        sun_spot_number[i] = dat24
        radio_flux[i]      = dat25
        flux_qual[i]       = dat26
    endfor
    
    ;Close the file
    free_lun, lun
    
    ;Convert date to a 4-digit year format
    MrTimeParser, temporary(temp_date), '%y%M%d', '%Y-%M-%d', date
    
;---------------------------------------------------------------------
; Time Filter ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if sDate ne '' and eDate ne '' then begin
        iKeep = where(date ge sDate and date le eDate, count)

        ;Trim the data
        if count ne 0 then begin
            date            = date[iKeep]
            solar_rot_num   = solar_rot_num[iKeep]
            nDays_rot_cycle = nDays_rot_cycle[iKeep]
            Kp              = Kp[*,iKeep]
            Kp_sum          = Kp_sum[iKeep]
            Ap              = Ap[*,iKeep]
            Ap_avg          = Ap_avg[iKeep]
            Cp              = Cp[iKeep]
            c9              = c9[iKeep]
            sun_spot_number = sun_spot_number[iKeep]
            radio_flux      = radio_flux[iKeep]
            flux_qual       = flux_qual[iKeep]
        endif
    endif
    
;---------------------------------------------------------------------
; Return Scalars? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(date) eq 1 then begin
        date            = date[0]
        solar_rot_num   = solar_rot_num[0]
        nDays_rot_cycle = nDays_rot_cycle[0]
        Kp              = Kp[0]
        Kp_sum          = Kp_sum[0]
        Ap              = Ap[0]
        Ap_avg          = Ap_avg[0]
        Cp              = Cp[0]
        c9              = c9[0]
        sun_spot_number = sun_spot_number[0]
        radio_flux      = radio_flux[0]
        flux_qual       = flux_qual[0]
    endif
    
;---------------------------------------------------------------------
; Time Series? ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if time_series then begin
        nDates = n_elements(date)
        times = strarr(8, nDates)
        
        ;Center times for each sampling period.
        times = [[date + 'T01:30:00'], $
                 [date + 'T04:30:00'], $
                 [date + 'T07:30:00'], $
                 [date + 'T10:30:00'], $
                 [date + 'T13:30:00'], $
                 [date + 'T16:30:00'], $
                 [date + 'T19:30:00'], $
                 [date + 'T22:30:00']]
        
        ;Form dates as YYYY-MM-DDThh:mm:ss
        date = reform(transpose(temporary(times)), 8*nDates, /OVERWRITE)
        
        ;Reform Kp and Ap
        Kp = reform(Kp, 8*nDates, /OVERWRITE)
        Ap = reform(Ap, 8*nDates, /OVERWRITE)
    endif

end