; docformat = 'rst'
;
; NAME:
;       CONVERT_TIME
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
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
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
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
;       An all-in-one routine for converting between the following time formats::
;           DATETIME (Year-Month-Day Hours-Minutes-Seconds)
;           HMS (Hours-Minutes-Seconds)
;           SSM (Seconds Since Midnight)
;           EPOCH (CDF Epoch values)
;           EPOCH16 (CDF Epoch16 values)
;           TT2000 (CDF TT2000 values)
;
;       You must indicate the current and the desired format when converting.
;
;       From::
;           `DATETIME`
;           `HMS`
;           `SSM`
;           `CDFEPOCH`
;           `EPOCH16`
;           `TT2000`
;
;       To::
;           `TO_DATETIME`
;           `TO_HMS`
;           `TO_SSM`,
;           `TO_EPOCH`
;           `TO_EPOCH16`
;           `TO_TT2000`
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;       
; :History:
;   Modification History::
;       Written by:  Matthew Argall 30 March 2012
;-
;-----------------------------------------------------------------------------------------
;+
;       Convert properly formatted DATE (or DATE and TIME) strings to the desired EPOCH
;       type using the CDF_PARSE functions. Optionally, DATE (and TIME) can be broken into
;       pieces to be used with the CDF_EPOCH, CDF_EPOCH, or CDF_TT2000 programs.
;
;       With this program "Properly Formatted" is now uniquely defined for all CDF types.
;       This format is 'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp' where Y=year, M=month, D=day,
;       H=hour, M=minute, S=second, m=millisecond, u=microsecond
;
;       Recall that EPOCH and EPOCH16 parsers require the format::
;
;           '25-Dec-2001 23:59:59.000.000.000.000'
;
;       So, if the input date and time are of the "Proper" format, as defined above, use
;       keywords `/DISSECT` and `/PARSE`
;
;       Year, month, day, hour, ..., can also be given, in which case the EPOCH_TIME is
;       computed from those, not from a parsed string.
;
; :Params:
;       DATE:       in, required, type=string/strarr
;                   If only one parameter is given, it represents the "properly-
;                       formatted" (as defined above). If two
;                       parameters are given, then they represent "properly-
;                       formatted" date and time strings to be joined and used with
;                       the CDF_PARSE functions. If three or more arguments are
;                       given, then it represents the numeric year to be used
;                       with CDF_EPOCH, CDF_EPOCH16, and CDF_TT2000 functions.
;       TIME:       in, optional, type=string/strarr
;                   If only two parameters were given, then it represents the
;                       "properly-formatted" time to be joined with DATE and used
;                       with the CDF_PARSE functions. If more than 2 parameters
;                       are given, it represents the numeric month to (1-12) be
;                       used with CDF_EPOCH, CDF_EPOCH16 or CDF_TT2000 functions.
;                       in the last case, if TIME=0, then DAY represents the day
;                       of the year value.
;       DAY:        in, optional, type=int/intarr
;                   The day of the month (0-31) to be converted. If TIME is 0 then
;                       this represents the year-day (0-366).
;       HOUR:       in, optional, type=int/intarr
;                   The hour (0-24) of the day to be converted.
;       MINUTE:     in, optional, type=int/intarr
;                   The minutes (0-59) to be converted.
;       SECOND:     in, optional, type=int/intarr
;                   The seconds (0-59) [or (0-60) in case of leap seconds to be
;                       converted.
;       MILLI:      in, optional, type=int/intarr
;                   The number of milliseconds (0-999) to be converted.
;       MICRO:      in, optional, type=int/intarr
;                   The number of microseconds (0-999) to be converted.
;       PICO:       in, optional, type=int/intarr
;                   The number of picoseconds (0-999) to be converted.
;       NANO:       in, optional, type=int/intarr
;                   The number of nanoseconds (0-999) to be converted.
;
; :Keywords:
;       DISSECT:        in, optional, type=Boolean, default=0
;                       Break the `DATE` (and `TIME`) into its pieces. If used with `PARSE`,
;                           these pieces will be used to create a properly-formatted
;                           date string compatible with CDF_PARSE_* routines.
;       PARSE:          in, optional, type=Boolean, default=1.
;                       indicate that the `DATE` (and `TIME`) are to be parsed into their
;                           proper EPOCH values. This is the default for <= 2
;                           parameters.
;       TO_CDFEPOCH:    in, optional, type=Boolean
;                       Convert to a CDF Epoch time. This is the default unless
;                           /TO_EPOCH16 or /TO_TT2000 are set.
;       TO_EPOCH16:     in, optional, type=Boolean, default=0.
;                       Convert to EPOCH16 instead of EPOCH
;       TO_TT2000:      in, optional, type=Boolean, default=0
;                       Convert to EPOCH_TT2000 instead of EPOCH
;
; :Returns:
;       OUT_TIME:       The EPOCH, EPOCH16, or TT2000 times of the converted DATE-TIME.
;                           Output has same number of elements as DATE and is of the
;                           type: EPOCH=dblarr, EPOCH16=dcomplex_arr, TT2000=lon64arr
;-
function convert_time_datetime, date, time, day, hour, minute, second, milli, micro, nano, pico, $
;CONVERSION KEYWORDS
TO_CDFEPOCH=to_cdfepoch, $
TO_EPOCH16=to_epoch16, $
TO_TT2000=to_tt2000, $

;OTHER KEYWORDS
DISSECT=dissect, $
PARSE=parse
    compile_opt idl2, hidden
    on_error, 2

    ;Convert to EPOCH, EPOCH, or TT2000
    out_time = datetime_to_epoch(date, time, day, hour, minute, second, milli, micro, nano, pico, $
                                       TO_EPOCH16=to_epoch16, TO_TT2000=to_tt2000, $
                                       DISSECT=dissect, PARSE=parse)
    return, out_time
end


;+
; Convert EPOCH, EPOCH16 or TT2000 to::
;   DATETIME -- The date and time of each EPOCH value [YYYYMMDD, HHMMSS.mmmuuunnnppp]
;   HMS -- The time-of day [HHMMSS.mmmuuunnnppp] on the day of each EPOCH value.
;   SSM -- Seconds Since Midnight on the day of each EPOCH value.
;
; :Returns:
;       OUT_TIME:          Depends on the keyword::
;                               `/TO_DATETIME`: A two column array. 
;                                               `DATE_TIME[0,*]` is the converted date 
;                                                                ('YYYYMMDD')
;                                               `DATE_TIME[1,*]` is the converted time
;                                                                ('HHMMSS.ddd')
;                               `/TO_HSM`: The time-of-day, EPOCH without date information.
;                                           ('HHMMSS.mmmuuunnnppp')
;                               `/TO_SSM`: Seconds Since Midnight on the day of the EPOCH value
;
; :Params:
;       EPOCH_TIME:         in, required, type=EPOCH/EPOCH16/TT2000 [array]
;                           The epoch value to be converted to seconds since midnight.
;                               Year, month, and day information is discarded.
;
; :Keywords:
;       TO_SSM:             in, optional, type=Boolean, default=0
;                           Convert `EPOCH_TIME` to seconds since midnight.
;       TO_DATETIME:        in, optional, type=Boolean, default=0
;                           Convert `EPOCH_TIME` to a date and time string ('YYYYMMDD HHMMSS.ddd)'.
;       TO_HMS:             in, optional, type=Boolean, default=0
;                           Convert `EPOCH_TIME` to a time string (HHMMSS.ddd).
;       TO_STRING:          in, optional, type=Boolean, default=0
;                           Return the output time as a string.
;       DATEDELIM:          in, optional, type=string. default=''
;                           If used with `TO_STRING`, it is the delimeter that separates the
;                               year, month and day values (e.g. YYYY-MM-DD)
;       TIMEDELIM:          in, optional, type=string. default=''
;                           If used with `TO_STRING`, it is the delimeter that separates the
;                               hour, minute, and second values (e.g. HH:MM:SS)
;-
function convert_time_epoch, epoch_time, $
;TO
TO_SSM=to_ssm, $
TO_DATETIME=to_datetime, $
TO_HMS=to_hms, $

;OTHER KEYWORDS
TO_STRING=to_string, $
DATEDELIM=datedelim, $
TIMEDELIM=timedelim, $
_EXTRA=extra
    compile_opt idl2, hidden
    on_error, 2
    
    ;Convert to SSM    
    if keyword_set(to_ssm) then begin
        out_time = epoch_to_ssm(epoch_time)
    
    ;Convert to DATETIME or HMS
    endif else if keyword_set(to_datetime) or keyword_set(to_hms) then begin
        out_time = epoch_to_datetime(epoch_time, $
                                     TO_STRING=to_string, DATEDELIM=datedelim, $
                                     TIMEDELIM=timedelim)
        
        ;pick out the HMS part if requested
        if keyword_set(to_hms) then out_time = out_time[1]
    endif
    
    return, out_time
end


;+
; Convert time from a time-of-day (HMS) value (HHMMSS.mmmuuunnnddd) to seconds since
; midnight (SSM).
;
; :Returns:
;       OUT_TIME:           out, type=dblarr/double
;                           `HMS_TIME` converted to seconds since midnight (SSM).
;
; :Params:
;       HMS_TIME;           in, required, type=string/strarr/int/intarr
;                           The time-of-day (HHMMSS.mmmuuunnnppp) to be converted to SSM
;
; :Keywords:
;       TO_SSM:             in, optional, type=Boolean, default=0
;                           Convert  to seconds since midnight.
;-
function convert_time_hms, hms_time, $
;CONVERSION KEYWORDS
TO_SSM=to_ssm
    compile_opt idl2, hidden
    on_error, 2
    
    ;Convert to SSM
    if keyword_set(to_ssm) then out_time = hms_to_ssm(hms_time)
    
    return, out_time
end


;+
; Convert EPOCH, EPOCH16 or TT2000 to::
;   DATETIME -- The date and time of each EPOCH value [YYYYMMDD, HHMMSS.mmmuuunnnppp]
;   HMS -- The time-of day [HHMMSS.mmmuuunnnppp] on the day of each EPOCH value.
;   SSM -- Seconds Since Midnight on the day of each EPOCH value.
;
; :Returns:
;       OUT_TIME:          Depends on the keyword::
;                               /TO_DATETIME: A two column array. 
;                                             `DATE_TIME[0,*]` is the converted date 
;                                                              ('YYYYMMDD')
;                                             `DATE_TIME[1,*]` is the converted time
;                                                              ('HHMMSS.ddd')
;                               /TO_HSM: The time-of-day, EPOCH without date information.
;                                           ('HHMMSS.mmmuuunnnppp')
;                               /TO_SSM: Seconds Since Midnight on the day of the EPOCH value
;                               /TO_EPOCH: An array of EPOCH values
;                               /TO_EPOCH16: An array of TO_EPOCH16 values
;                               /TO_TT2000: An array of TO_TT2000 values
;
; :Params:
;       SSM_TIME:           in, required, type=EPOCH/EPOCH16/TT2000 [array]
;                           The epoch value to be converted to seconds since midnight.
;                               Year, month, and day information is discarded.
;
; :Keywords:
;       TO_EPOCH            in, optional, type=Boolean
;                           Convert to a CDF Epoch time.
;       TO_EPOCH16:         in, optional, type=Boolean, default=0
;                           Convert to EPOCH16 instead of EPOCH
;       TO_TT2000:          in, optional, type=Boolean, default=0
;                           Convert to EPOCH_TT2000 instead of EPOCH
;       TO_HMS:             in, optional, type=Boolean, default=0
;                           Convert `SSM_TIME` to a time-of-day string (HHMMSS.ddd).
;       TO_STRING:          in, optional, type=Boolean, default=0
;                           Return the output time as a string.
;       DELIMETER:          in, optional, type=string, default=''
;                           If used with `TO_STRING`, it is the delimeter that separates the
;                               hour, minute, and second values (e.g. HH:MM:SS)
;-
function convert_time_ssm, date, ssm_time, $
;CONVERSION KEYWORDS
TO_HMS=to_hms, $
TO_EPOCH=to_epoch, $
TO_EPOCH16=to_epoch16, $
TO_TT2000=to_tt2000, $

;OTHER KEYWORDS
TO_STRING=to_string, $
DELIMETER=delimeter, $
DISSECT=dissect
    compile_opt idl2, hidden
    on_error, 2
    
    ;if only one parameter was given, then it is the SSM_TIME
    if n_elements(ssm_time) eq 0 then ssm_time = temporary(date)
    
    ;Convert to HMS
    if keyword_set(to_hms) then begin
        out_time = ssm_to_hms(ssm_time, TO_STRING=to_string, DELIMETER=delimeter)
    
    ;Convert to EPOCH
    endif else begin
        out_time = ssm_to_epoch(date, ssm_time, TO_EPOCH16=to_epoch16, TO_TT2000=to_tt2000, $
                                                DISSECT=dissect)
    endelse
    
    return, out_time
end


;+
;       An all-in-one routine for converting between the following time formats::
;           DATETIME (Year-Month-Day Hours-Minutes-Seconds)
;           HMS (Hours-Minutes-Seconds)
;           SSM (Seconds Since Midnight)
;           EPOCH (CDF Epoch values)
;           EPOCH16 (CDF Epoch16 values)
;           TT2000 (CDF TT2000 values)
;
;       You must indicate the current and the desired format when converting.
;
;       From::
;           DATETIME
;           HMS
;           SSM
;           CDFEPOCH
;           EPOCH16
;           TT2000
;
;       To::
;           TO_DATETIME
;           TO_HMS
;           TO_SSM
;           TO_EPOCH
;           TO_EPOCH16
;           TO_TT2000
;
; :Params:
;       DATE:       in, required, type=string/strarr
;                   If only one parameter is given, it represents the "properly-
;                       formatted" (as defined above). If two
;                       parameters are given, then they represent "properly-
;                       formatted" date and time strings to be joined and used with
;                       the CDF_PARSE functions. If three or more arguments are
;                       given, then it represents the numeric year to be used
;                       with CDF_EPOCH, CDF_EPOCH16, and CDF_TT2000 functions.
;       TIME:       in, optional, type=string/strarr
;                   If only two parameters were given, then it represents the
;                       "properly-formatted" time to be joined with `DATE` and used
;                       with the CDF_PARSE functions. If more than 2 parameters
;                       are given, it represents the numeric month (1-12) to be
;                       used with CDF_EPOCH, CDF_EPOCH16 or CDF_TT2000 functions.
;                       In the last case, if TIME=0, then `DAY` represents the day
;                       of the year value.
;       DAY:        in, optional, type=int/intarr
;                   The day of the month (0-31) to be converted. If TIME is 0 then
;                       this represents the year-day (0-366).
;       HOUR:       in, optional, type=int/intarr
;                   The hour (0-24) of the day to be converted.
;       MINUTE:     in, optional, type=int/intarr
;                   The minutes (0-59) to be converted.
;       SECOND:     in, optional, type=int/intarr
;                   The seconds (0-59) [or (0-60) in case of leap seconds to be
;                       converted.
;       MILLI:      in, optional, type=int/intarr
;                   The number of milliseconds (0-999) to be converted.
;       MICRO:      in, optional, type=int/intarr
;                   The number of microseconds (0-999) to be converted.
;       PICO:       in, optional, type=int/intarr
;                   The number of picoseconds (0-999) to be converted.
;       NANO:       in, optional, type=int/intarr
;                   The number of nanoseconds (0-999) to be converted.
;
; :Keywords:
;       SSM:            in, optional, type=boolean, default=0
;                       Convert from seconds since midnight.
;       HMS:            in, optional, type=boolean, default=0
;                       Convert from hour-minutes-seconds (HMS) (HHMMSS.mmmuuudddppp).
;       DATETIME:       in, optional, type=boolean, default=0
;                       Convert from year-month-date + HMS (YYYYMMDD HHMMSS.mmmuuudddppp).
;       CDFEPOCH:       in, optional, type=boolean, default=0
;                       Convert from CDF EPOCH time.
;       EPOCH16:        in, optional, type=boolean, default=0
;                       Convert from CDF EPOCH16 time.
;       TT2000:         in, optional, type=boolean, default=0
;                       Convert from CDF TT2000 time.
;       TO_DATETIME:    in, optional, type=Boolean, default=0.
;                       Convert to DATETIME format (YYYYMMDD HHMMSS.mmmuuunnnppp)
;       TO_HMS:         in, optional, type=Boolean, default=0.
;                       Convert to hour-minute_second format (HHMMSS.mmmuuunnnppp)
;       TO_SSM:         in, optional, type=Boolean, default=0.
;                       Convert to seconds since midnight format
;       TO_CDFEPOCH:    in, optional, type=Boolean, default=0.
;                       Convert to CDF EPOCH format.
;       TO_EPOCH16:     in, optional, type=Boolean, default=0.
;                       Convert to CDF EPOCH16 format.
;       TO_TT2000:      in, optional, type=Boolean, default=0
;                       Convert to CDF TT2000 format.
;       DISSECT:        in, optional, type=Boolean, default=0
;                       Break the `DATE` (and `TIME`) into its pieces. If used with `PARSE`,
;                           these pieces will be used to create a properly-formatted
;                           date string compatible with CDF_PARSE_* routines.
;       PARSE:          in, optional, type=Boolean, default=1
;                       indicate that the `DATE` (and `TIME`) are to be parsed into their
;                           proper EPOCH values. This is the default for <= 2
;                           parameters.
;       TO_STRING:      in, optional, type=Boolean, default=0
;                       Return the output time as a string.
;       DELIMETER:      in, optional, type=string, default=''
;                       If used with `TO_STRING`, it is the delimeter that separates the
;                           hour, minute, and second values (e.g. HH:MM:SS)
;       DATEDELIM:          in, optional, type=string, default=''
;                           If used with `TO_STRING`, it is the delimeter that separates the
;                               year, month and day values (e.g. YYYY-MM-DD)
;       TIMEDELIM:          in, optional, type=string, default=''
;                           If used with `TO_STRING`, it is the delimeter that separates the
;                               hour, minute, and second values (e.g. HH:MM:SS)
;
; :Returns:
;       EPOCH_TIME:     out, type=dblarr/dcomplex/lon64arr
;                       The EPOCH, EPOCH16, or TT2000 times of the converted DATE-TIME.
;                           Output has same number of elements as DATE and is of the
;                           type: EPOCH=dblarr, EPOCH16=dcomplex_arr, TT2000=lon64arr
;-
function convert_time, date, time, day, hour, minute, second, milli, micro, nano, pico, $
;FROM THESE
SSM=ssm, $
HMS=hms, $
DATETIME=datetime, $
CDFEPOCH=epoch, $
EPOCH16=epoch16, $
TT2000=tt2000, $

;TO THESE
TO_SSM=to_ssm, $
TO_HMS=to_hms, $
TO_DATETIME=to_datetime, $
TO_CDFEPOCH=to_cdfepoch, $
TO_EPOCH16=to_epoch16, $
TO_TT2000=to_tt2000, $

;OTHER KEYORDS ACCEPTED BY THE VARIOUS METHODS
DATEDELIM = datedelim, $
DELIMETER = delimeter, $
DISSECT = dissect, $
PARSE = parse, $
TIMEDELIM = timedelim, $
TO_STRING = to_string
    compile_opt idl2
    
    ;if no parameters were given, then return an instance of the class
    if n_params() eq 0 then return, 1
    
    ;otherwise call the proper conversion routine
    ;Convert from SSM
    if keyword_set(ssm) then begin
        result = convert_time_ssm(date, TO_HMS=to_hsm, TO_EPOCH=to_cdfepoch, $
                                        TO_EPOCH16=to_epoch16, TO_TT2000=to_tt2000, $
                                        TO_STRING=to_string, DELIMETER=delimeter, $
                                        DISSECT=dissect)
    
    ;Convert from HMS
    endif else if keyword_set(hms) then begin
        result = convert_time_hms(date, TO_SSM=to_ssm)
        
    ;Convert from DATETIME
    endif else if keyword_set(datetime) then begin
        result = convert_time_datetime(date, time, day, hour, minute, second, milli, micro, nano, pico, $
                                       TO_CDFEPOCH=to_cdfepoch, TO_EPOCH16=to_epoch16, TO_TT2000=to_tt2000, $
                                       DISSECT=dissect, PARSE=parse)
                                  
    ;Convert from EPOCH, EPOCH16, or TT2000
    endif else begin
        result = convert_time_epoch(date, $
                                    TO_SSM=to_ssm, TO_DATETIME=to_datetime, TO_HMS=to_hms, $
                                    DATEDELIM=datedelim, TIMEDELIM=timedelim, $
                                    TO_STRING=to_string)
    endelse
    
    return, result
end