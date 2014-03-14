; docformat = 'rst'
;
; NAME:
;       DATATIME_TO_EPOCH
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
;       keyword /DISSECT (and, optionally, /PARSE).
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
;                       Indicate that the `DATE` (and `TIME`) are to be parsed into their
;                           proper EPOCH strings. This is the default for <= 2
;                           parameters. If set, individual time components will be parsed
;                           into a complete time string and `EPOCH_TIME` will be computed
;                           using the `cdf_parse_*` routines.
;                       If not set, `EPOCH_TIME` will be computed using the
;                           `cdf_epoch, /COMPUTE_EPOCH` (etc) routines and the individual
;                           time components (either provided or returned with the
;                           `DISSECT` keyword).
;       TO_EPOCH16:     in, optional, type=Boolean, default=0.
;                       Convert to EPOCH16 instead of EPOCH
;       TO_TT2000:      in, optional, type=Boolean, default=0
;                       Convert to EPOCH_TT2000 instead of EPOCH
;
; :Returns:
;       EPOCH_TIME:     out, type=dblarr/dcomplex/lon64arr
;                       The EPOCH, EPOCH16, or TT2000 times of the converted DATE-TIME.
;                           Output has same number of elements as DATE and is of the
;                           type: EPOCH=dblarr, EPOCH16=dcomplex_arr, TT2000=lon64arr
;
; :Uses:
;   Uses the following external programs::
;       dissectDateTime.pro
;       dissectDate.pro
;       dissectTime.pro
;       monthName.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;       
; :History:
;   Modification History::
;       Written by:     Matthew Argall 30 March 2012
;       2013-10-23  -   Fixed various bugs so that all examples work. - MRA
;-
function datetime_to_epoch, date, time, day, hour, minute, second, milli, micro, nano, pico, $
DISSECT=dissect, $
PARSE=parse, $
TO_EPOCH16 = to_epoch16, $
TO_TT2000=to_tt2000
	compile_opt strictarr

;-----------------------------------------------------------------------------------------
;CATCH AN ERROR //////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------

    catch, error
    if error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif

;-----------------------------------------------------------------------------------------
;CHECK INPUTS ////////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------

    ;check inputs
    ndate = n_elements(date)
    ntime = n_elements(time)
    dissect = keyword_set(dissect)
    parse = keyword_set(parse)
    to_epoch16 = keyword_set(to_epoch16)
    to_tt2000 = keyword_set(to_tt2000)

    ;If both DATE and TIME were given, they must have the same number of elements.
    if n_params() eq 2 and ndate ne ntime $
        then message, 'DATE and TIME must have same # elements.'
    
    ;If TIME was given, but not DAY, and DISSECT=0, then parse DATE and TIME together
    if (ntime gt 0) && (n_elements(day) eq 0) && (dissect eq 0) then parse = 1
    
    ;If a valid date string was given, then parse it.
    if n_params() eq 1 and (dissect eq 0) then parse = 1

;-----------------------------------------------------------------------------------------
;DISSECT DATE (AND TIME) IF REQUESTED ////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    
    ;if the date and time are to be dissected first, then do that.
    if (dissect eq 1) then begin
        ;PARSE = 1 -- YEAR, MONTH, DAY, etc. must be combined together to form a valid
        ;             epoch string. Thus, the dissected date and time must return strings.
        ;PARSE = 0 -- CDF times will be computed using integer values of YEAR, MONTH, etc.
        if parse eq 1 then type=7 else type=2
    
        ;If no time values were given, then the DATE string also contains the time.
        if ntime eq 0 then begin
            ;Extract the year, month, day, hour, minute, second, and fractional second
            ;There may be a delimeter, check for that, too.
            dissectDateTime, date, year, month, day, $
                                   hour, minute, second, $
                                   milli, micro, nano, pico, TYPE=type
                            
        ;otherwise separate the date and time individually
        endif else begin
            ;extract the year, month, and day
            dissectDate, date, year, month, day, TYPE=type
            
            ;extract the hour, minute, second, and fractional seconds
            dissectTime, time, hour, minute, second, $
                               milli, micro, nano, pico, TYPE=type
        endelse
    endif

;-----------------------------------------------------------------------------------------
;PARSE PROPERLY-FORMATTED EPOCH STRING ///////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    
    if (parse eq 1) then begin    
        ;if DISSECT is set, then construct the parse string
        if (dissect eq 1) then begin
            
            ;TT2000 strings have a different format from EPOCH and EPOCH16
            ;TT2000: YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp
            ;EPOCH, EPOCH16: DD-MMM-YYYY HH:MM:SS.mmm.uuu.nnn.ppp
            if keyword_set(tt2000) then begin
                
                ;Because DISSECT is set, date info are strings. Concatenate them together.
                parse_string = year + '-' + month + '-' + day + 'T' + $
                                    hour + ':' + minute + ':' + second + '.' + $
                                    milli + micro + nano + pico
            endif else begin
                ;EPOCH and EPOCH16 requre 3-character month names
                month = monthName(month)

                parse_string = day + '-' + month + '-' + year + ' ' + $
                               hour + ':' + minute + ':' + second + '.' + $
                               milli + '.' + micro + '.' + nano + '.' + pico
            endelse
        
        ;if TIME was given, then combine the DATE and TIME strings
        endif else if ntime ne 0 then begin
            ;Parsed strings must have decimal seconds. Fix those that do not.
            fixThese = where(strpos(time, '.') eq -1, count)
            if count ne 0 then time[fixThese] += '.0'
            
            ;create the parse string
            if keyword_set(tt2000) $
                then parse_string = date + 'T' + time $
                else parse_string = date + ' ' + time
        
        ;otherwise, just parse DATE
        endif else begin
            parse_string = date
        endelse

        ;Parse to the desired EPOCH type
        case 1 of
            to_epoch16: epoch_time = cdf_parse_epoch16(parse_string)
            to_tt2000:  epoch_time = cdf_parse_tt2000(parse_string)
            else:       epoch_time = cdf_parse_epoch(parse_string)
        endcase
        
        return, epoch_time
    endif
    
;-----------------------------------------------------------------------------------------
;CONVERT USING YEAR, MONTH, DAY, ETC. ////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    ;If we have gotten to here, then PARSE=0. If YEAR and MONTH are still undefined, then
    ;DISSECT=0. This implies that DATE and TIME are actually YEAR and MONTH values
    if (dissect eq 0) then begin
        if n_elements(date) gt 0 then year = date
        if n_elements(time) gt 0 then month = time
    endif
    
    ;Compute the epoch value
    case 1 of
        ;CDF_EPOCH16
        to_epoch16: cdf_epoch16, epoch_time, year, month, day, $
                                             hour, minute, second, $
                                             milli, micro, nano, pico, /COMPUTE_EPOCH
                                 
        ;CDF_TIME_TT2000
        to_tt2000:  cdf_tt2000, epoch_time, year, month, day, $
                                            hour, minute, second, $
                                            milli, micro, nano, /COMPUTE_EPOCH
                                
        ;CDF EPOCH
        else: cdf_epoch, epoch_time, year, month, day, $
                                     hour, minute, second, $
                                     milli, /COMPUTE_EPOCH
    endcase
    
    return, epoch_time
end





;-----------------------------------------------------------------------------------------
;%MAIN EXAMPLE DEMONSTRATION PROGRAM (.r datetime_to_epoch) //////////////////////////////
;-----------------------------------------------------------------------------------------
print, ''
print, '------------------------------------------------------------'
;Calculate the EPOCH time of a properly formatted epoch string
datetime = '25-Dec-2001 23:59:59.000.000.000.000'
epoch_time = datetime_to_epoch(datetime)
cdf_epoch, epoch_check, 2001, 12, 25, 23, 59, 59, 0, /COMPUTE_EPOCH
epoch_diff = cdf_epoch_diff(epoch_check, epoch_time)
print, 'Date-Time: ' + datetime
print, 'Difference with CDF_EPOCH: ' + strtrim(epoch_diff, 2)


print, ''
print, '------------------------------------------------------------'
;Calculate the EPOCH16 time of a properly formatted epoch string
datetime = '25-Dec-2001 23:59:59.000.000.000.000'
epoch_time = datetime_to_epoch(datetime, /TO_EPOCH16)
cdf_epoch16, epoch_check, 2001, 12, 25, 23, 59, 59, 0, /COMPUTE_EPOCH
epoch_diff = cdf_epoch_diff(epoch_check, epoch_time)
print, 'Date-Time: ' + datetime
print, 'Difference with CDF_EPOCH16: ' + strtrim(epoch_diff, 2)


print, ''
print, '------------------------------------------------------------'
;Calculate the TT2000 time of a properly formatted epoch string
datetime = '2001-12-25T23:59:59.000000000000'
epoch_time = datetime_to_epoch(datetime, /TO_TT2000)
cdf_tt2000, epoch_check, 2001, 12, 25, 23, 59, 59, 0, /COMPUTE_EPOCH
epoch_diff = cdf_epoch_diff(epoch_check, epoch_time)
print, 'Calculate the EPOCH TT20000 time of a properly formatted epoch string'
print, 'Date-Time: ' + datetime
print, 'Difference with CDF_TT2000: ' + strtrim(epoch_diff, 2)


print, ''
print, '------------------------------------------------------------'
;Calculate the EPOCH time of an improperly formatted epoch string
datetime = ['2001-12-25T23:59:59.000000000000', '20011225 235959', '20011225T235959.00', $
            '2001-12/25?2359:59.000000']
epoch_time = datetime_to_epoch(datetime, /DISSECT, /PARSE)
cdf_epoch, epoch_check, 2001, 12, 25, 23, 59, 59, 0, /COMPUTE_EPOCH
epoch_diff = cdf_epoch_diff(replicate(epoch_check, n_elements(epoch_time)), epoch_time)
print, 'Calculate the EPOCH time of an array of improperly formatted epoch strings'
print, 'Date-Time: ' + transpose(datetime)
print, 'Difference with CDF_EPOCH: ' + transpose(strtrim(epoch_diff, 2))


print, ''
print, '------------------------------------------------------------'
;Using year, month, day, ... make a TT2000 number
epoch_time = datetime_to_epoch(2001, 12, 25, 23, 59, 59, 0, 0, 0, 0, /TO_TT2000)
cdf_tt2000, epoch_check, 2001, 12, 25, 23, 59, 59, 0, /COMPUTE_EPOCH
epoch_diff = cdf_epoch_diff(epoch_check, epoch_time)
print, 'Calculate the EPOCH TT2000 time numeric year, month, day, hour, ... '
print, 'Date-Time: ' + '2001-12-25 23:59:59'
print, 'Difference with CDF_TT2000: ' + strtrim(epoch_diff, 2)


end
