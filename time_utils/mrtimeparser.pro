; docformat = 'rst'
;
; NAME:
;       MrTimeParser
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   The purpose of this function is to breakdown time strings in any format and optionally
;   rebuild them into any other format. Components of time strings are recognized by
;   tokens, each of which begin with "%". The defined tokens are below. Furthermore, there
;   are pre-defined patterns, listed below the tokens, that can be used.
;
;   LIST OF TOKENS::
;       %Y      -   Four-digit year: 2012, 2013, etc.
;       %y      -   Two-digit year: 60-59
;       %M      -   Two-digit month: 01-12
;       %C      -   Calendar month: January, Feburary, etc.
;       %c      -   Abbreviated calendar month: Jan, Feb, etc.
;       %d      -   Day of month: 01-31
;       %D      -   Day of year: 000-366
;       %W      -   Week day: Monday, Tuesday, etc.
;       %w      -   Abbreviated week day: Mon, Tue, etc.
;       %H      -   Hour on a 24-hour clock: 00-24
;       %h      -   Hour on a 12-hour clock: 01-12
;       %m      -   Minute: 00-59
;       %S      -   Seconds: 00-59
;       %f      -   Fractions of a second: Decimal point followed by any number of digits.
;       %1      -   Milli-seconds: 000-999
;       %2      -   Micro-seconds: 000-999
;       %3      -   Nano-seconds: 000-999
;       %4      -   Pico-seconds: 000-999
;       %A      -   A.M. or P.M. on a 12-hour clock
;       %z      -   Time Zone, abbreviated name
;       %o      -   Offset from UTC
;       %?      -   Any single, unknown character
;       %(      -   Anything between "%(" and "%)" is ignored.
;       %)      -   Anything between "%(" and "%)" is ignored.
;       \%      -   The "%" character.
;
;   NOTES:
;       If "z" or "o" appear in the input pattern, but not in the output pattern, the
;       offset from UTC is lost. No coversion between time zones takes place.
;
;       The tokens "%(" and "%)" can be used, for example, to encase regular expressions.
;       As such, the MrTimeTokensToRegex will produce a regular expression that matches
;       your criteria while allowing easy extraction of date and time information via
;       MrTimeParser (by not generating an unknown number of subexpressions if "(" and ")"
;       are included in the regular expression string). See the examples.
;
;   PRE-DEFINED PATTERNS::
;        1: 1951-01-09T08:21:10Z                    ISO-8601
;        2: 09-Jan-1951 08:21:10.000                CDF_EPOCH
;        3: 09-Jan-1951 08:21:10.000.000.000.000    CDF_EPOCH16
;        4: 1951-01-09T08:21:10.000000000           CDF_TIME_TT2000
;        5: 09 Jan 1951
;        6: 09 Jan 1951 08:21:10
;        7: 09 Jan 1951 08h 21m 10s
;        8: 09 January 1951
;        9: 09 January 1951 08:21:10
;       10: 09 January 1951 08h 21m 10s
;       11: Jan 09, 1951
;       12: Jan 09, 1951 08:21:10
;       13: Jan 09, 1951 08h 21m 10s
;       14: Jan 09, 1951
;       15: January 09, 1951 08:21:10
;       16: January 09, 1951 08h 21m 10s
;       17: January 09, 1951 08:21:10
;       18: 1951-009
;       19: 1951-009 08:21:10
;       20: 1951-009 08h 21m 10s
;       21: 009-1951
;       22: 009-1951 08:21:10
;       23: 009-1951 08h 21m 10s
;       24: 19510109
;       25: 09011951
;       26: 01091951
;       27: Tuesday, January 09, 1951
;       28: Tuesday, January 09, 1951 08:21:10
;       29: Tuesday, January 09, 1951 08h 21m 10s
;       30: Tue, Jan 09, 1951
;       31: Tue, Jan 09, 1951 08:21:10
;       32: Tue, Jan 09, 1951 08h 21m 10s
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       MrIsMember.pro
;       MG_StReplace.pro
;       MonthNameToNumber.pro
;       MonthNumberToName.pro
;       MrDayOfWeek.pro
;       MrTimeTokensToRegex.pro
;       MrTimeZoneNameToOffset.pro
;       MrTimeZoneOffsetToName.pro
;       MrWeekDayToName.pro
;       MrWeekNameToDay.pro
;       Year_Day.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2014-03-15  -   Written by Matthew Argall.
;       2014-03-21  -   Removed function MrTimeParser_Token2Regex to its own file
;                           (named MrTimeTokensToRegex.pro as of this date). - MRA
;       2014-04-29  -   Added support for the "%f" token. Weekdays and Wkdays are now
;                           handled. Added helper functions MrTimeParser_YearToYr and
;                           MrTimeParser_DissectDOY. Added patterns 27-32. - MRA
;       2014-04-03  -   Typo when converting %c to %W or %w. Fixed. - MRA
;       2014-05-10  -   Added ISMATCH keyword to return the match status without issuing
;                           an error message. - MRA.
;       2014-05-11  -   Added support for "%z" and "%o". New TIME_ZONE and OFFSET keywords. - MRA
;       2014-06-29  -   Added support for "\%", "%(" and "%)". - MRA
;       2014-06-30  -   Removed MrTimeParser_ExtractTokens and incorporated MrTokens_Extract. - MRA
;       2015-04-30  -   Was dissecting DOY incorrectly. Fixed. - MRA
;       2015-08-21  -   Fixed logic that caused error when converting %D -> %D. - MRA
;-
;*****************************************************************************************
;+
;   Given a pattern with tokens, breakdown a time string into its components.
;
; :Private:
;
; :Params:
;       TIME:               in, required, type=string/strarr
;                           Time strings (without tokens) that match `PATTERN`.
;       PATTERN:            in, required, type=string
;                           Pattern containing tokens that identify different date and
;                               time elements within `TIME`.
;       ISMATCH:            out, optional, type=boolean
;                           Returns 1 if `TIME` matches `PATTERN` and 0 otherwise. If
;                               ISMATCH=0, `TIME` will not be broken down and all
;                               keywords will be empty. If, in addition, no variable is
;                               present, an error will be issued.
;
; :Keywords:
;       YEAR:               out, optional, type=strarr
;                           4-digit year that matches a %Y token.
;       YR:                 out, optional, type=strarr
;                           2-digit year that matches a %y token.
;       DOY:                out, optional, type=strarr
;                           3-digit day-of-year that matches a %D.
;       MONTH:              out, optional, type=strarr
;                           2-digit month that matches a %M token.
;       CMONTH:             out, optional, type=strarr
;                           Calendar month name (e.g. January, February, etc.) that
;                               matches a %C tokenl.
;       CALMO:              out, optional, type=strarr
;                           3-character abbreviated calendar month name (e.g. Jan, Feb, ...)
;                               that matches a %c token.
;       WEEKDAY:            out, optional, type=strarr
;                           Weekday (e.g. Monday, Tuesday, etc.) that matches a %W token
;                               for the [start, end] of the file interval.
;       WKDAY:              out, optional, type=strarr
;                           3-character abbreviated week day (e.g. Mon, Tue, etc.) that
;                               matches a %M token.
;       DAY:                out, optional, type=strarr
;                           2-digit day that matches a %d token.
;       HOUR:               out, optional, type=strarr
;                           2-digit hour on a 24-hour clock that matches a %H token.
;       HR:                 out, optional, type=strarr
;                           2-digit hour on a 12-hour clock that matches a %h token.
;       MINUTE:             out, optional, type=strarr
;                           2-digit minute that matches a %m token.
;       SECOND:             out, optional, type=strarr
;                           2-digit second that matches a %S token.
;       DECIMAL:            out, optional, type=strarr
;                           Fraction of a second that matches the %f token.
;       MILLI:              out, optional, type=strarr
;                           3-digit milli-second that matches a %1 token.
;       MICRO:              out, optional, type=strarr
;                           3-digit micro-second that matches a %2 token.
;       NANO:               out, optional, type=strarr
;                           3-digit nano-second that matches a %3 token.
;       PICO:               out, optional, type=strarr
;                           3-digit pico-second that matches a %4 token.
;       AM_PM:              out, optional, type=strarr
;                           "AM" or "PM" string that matches a %A.
;       OFFSET:             out, optional, type=string
;                           Offset from UTC. (+|-)hh[:][mm]. Matches %o.
;       TIME_ZONE:          out, optional, type=string
;                           Time zone abbreviated name. Matches %z.
;-
pro MrTimeParser_Breakdown, time, pattern, tf_match, $
YEAR=year, $
YR=yr, $
DOY=doy, $
MONTH=month, $
CMONTH=cMonth, $
CALMO=calmo, $
WEEKDAY=weekday, $
WKDAY=wkday, $
DAY=day, $
HOUR=hour, $
HR=hr, $
MINUTE=minute, $
SECOND=second, $
DECIMAL=decimal, $
MILLI=milli, $
MICRO=micro, $
NANO=nano, $
PICO=pico, $
AM_PM=am_pm, $
OFFSET=offset, $
TIME_ZONE=time_zone
    compile_opt strictarr
    on_error, 2

    nTimes  = n_elements(time)
    boolean = keyword_set(boolean)
    
;-----------------------------------------------------
;Tokens and File Parts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Convert to a regular expression
    regex_str = MrTokens_ToRegex(pattern, /IGNORE_PARENS)
    
    ;Get the tokens
    tokens = MrTokens_Extract(pattern, COUNT=nTokens, REPLACE_PARENS='.*', OPATTERN=_pattern)
    
    ;Apply Regex
    ;   - Check the overall match to determine success, then remove it (only interested in the pieces).
    parts    = stregex(time, regex_str, /SUBEXP, /EXTRACT, /FOLD_CASE)
    tf_match = parts[0] eq '' ? 0 : 1
    parts    = parts[1:*,*]
    
    ;If no match was found, decide what to do.
    if tf_match eq 0 then begin
        if n_params() eq 3 $
            then return $
            else message, 'Could not match pattern "' + pattern + '" to time "' + time[0] + '".'
    endif

;-----------------------------------------------------
; Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    year      = strarr(nTimes)
    yr        = strarr(nTimes)
    doy       = strarr(nTimes)
    month     = strarr(nTimes)
    cmonth    = strarr(nTimes)
    calmo     = strarr(nTimes)
    weekday   = strarr(nTimes)
    wkday     = strarr(nTimes)
    day       = strarr(nTimes)
    hour      = strarr(nTimes)
    hr        = strarr(nTimes)
    minute    = strarr(nTimes)
    second    = strarr(nTimes)
    decimal   = strarr(nTimes)
    milli     = strarr(nTimes)
    micro     = strarr(nTimes)
    nano      = strarr(nTimes)
    pico      = strarr(nTimes)
    am_pm     = strarr(nTimes)
    offset    = strarr(nTimes)
    time_zone = strarr(nTimes)
    
;-----------------------------------------------------
; Match Tokens to Pattern Parts \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    for i = 0, nTokens - 1 do begin
        ;Extract the file name values
        case tokens[i] of
            'Y': year      = reform(parts[i,*])
            'y': yr        = reform(parts[i,*])
            'D': doy       = reform(parts[i,*])
            'M': month     = reform(parts[i,*])
            'C': cmonth    = reform(parts[i,*])
            'c': calMo     = reform(parts[i,*])
            'd': day       = reform(parts[i,*])
            'W': weekday   = reform(parts[i,*])
            'w': wkday     = reform(parts[i,*])
            'H': hour      = reform(parts[i,*])
            'h': hr        = reform(parts[i,*])
            'm': minute    = reform(parts[i,*])
            'S': second    = reform(parts[i,*])
            'f': decimal   = reform(parts[i,*])
            '1': milli     = reform(parts[i,*])
            '2': micro     = reform(parts[i,*])
            '3': nano      = reform(parts[i,*])
            '4': pico      = reform(parts[i,*])
            'A': am_pm     = reform(parts[i,*])
            'o': offset    = reform(parts[i,*])
            'z': time_zone = reform(parts[i,*])
            else: message, 'Token "' + tokens[i] + '" not recognized.', /INFORMATIONAL
        endcase
    endfor
    
    ;Return scalars
    if nTimes eq 1 then begin
        year       = year[0]
        yr        = yr[0]
        doy       = doy[0]
        month     = month[0]
        cmonth    = cmonth[0]
        calMo     = calMo[0]
        day       = day[0]
        weekday   = weekday[0]
        wkday     = wkday[0]
        hour      = hour[0]
        hr        = hr[0]
        minute    = minute[0]
        second    = second[0]
        decimal   = decimal[0]
        milli     = milli[0]
        micro     = micro[0]
        nano      = nano[0]
        pico      = pico[0]
        am_pm     = am_pm[0]
        offset    = offset[0]
        time_zone = time_zone[0]
    endif
end


;+
;   Given a pattern with tokens, build a time string given its components.
;
; :Private:
;
; :Params:
;       TIME:               out, required, type=string/strarr
;                           Result of combining the date and time elements via `PATTERN`.
;       PATTERN:            in, required, type=string
;                           Pattern describing how `TIME` should be built.
;
; :Keywords:
;       YEAR:               in, optional, type=strarr
;                           4-digit year that matches a %Y token.
;       YR:                 in, optional, type=strarr
;                           2-digit year that matches a %y token.
;       DOY:                in, optional, type=strarr
;                           3-digit day-of-year that matches a %D.
;       MONTH:              in, optional, type=strarr
;                           2-digit month that matches a %M token.
;       CMONTH:             in, optional, type=strarr
;                           Calendar month name (e.g. January, February, etc.) that
;                               matches a %C tokenl.
;       CALMO:              in, optional, type=strarr
;                           3-character abbreviated calendar month name (e.g. Jan, Feb, ...)
;                               that matches a %c token.
;       WEEKDAY:            in, optional, type=strarr
;                           Weekday (e.g. Monday, Tuesday, etc.) that matches a %W token
;                               for the [start, end] of the file interval.
;       WKDAY:              in, optional, type=strarr
;                           3-character abbreviated week day (e.g. Mon, Tue, etc.) that
;                               matches a %M token.
;       DAY:                in, optional, type=strarr
;                           2-digit day that matches a %d token.
;       HOUR:               in, optional, type=strarr
;                           2-digit hour on a 24-hour clock that matches a %H token.
;       HR:                 in, optional, type=strarr
;                           2-digit hour on a 12-hour clock that matches a %h token.
;       MINUTE:             in, optional, type=strarr
;                           2-digit minute that matches a %m token.
;       SECOND:             in, optional, type=strarr
;                           2-digit second that matches a %S token.
;       DECIMAL:            in, optional, type=strarr
;                           Fraction of a second that matches the %f token.
;       MILLI:              in, optional, type=strarr
;                           3-digit milli-second that matches a %1 token.
;       MICRO:              in, optional, type=strarr
;                           3-digit micro-second that matches a %2 token.
;       NANO:               in, optional, type=strarr
;                           3-digit nano-second that matches a %3 token.
;       PICO:               in, optional, type=strarr
;                           3-digit pico-second that matches a %4 token.
;       AM_PM:              in, optional, type=strarr
;                           "AM" or "PM" string that matches a %A.
;       OFFSET:             in, optional, type=string
;                           Offset from UTC. (+|-)hh[:][mm]. Matches %o.
;       TIME_ZONE:          in, optional, type=string
;                           Time zone abbreviated name. Matches %z.
;-
pro MrTimeParser_Compute, timeOut, pattern, $
YEAR=year, $
YR=yr, $
DOY=doy, $
MONTH=month, $
CMONTH=cMonth, $
CALMO=calmo, $
WEEKDAY=weekday, $
WKDAY=wkday, $
DAY=day, $
HOUR=hour, $
HR=hr, $
MINUTE=minute, $
SECOND=second, $
DECIMAL=decimal, $
MILLI=milli, $
MICRO=micro, $
NANO=nano, $
PICO=pico, $
AM_PM=am_pM, $
TIME_ZONE=time_zone, $
OFFSET=offset
    compile_opt strictarr
    on_error, 2

    ;Extract the tokens
    tokens = MrTokens_Extract(pattern, COUNT=nTokens, POSITIONS=positions)

    ;Step through each token
    curPos  = 0
    timeOut = ''
    for i = 0, nTokens - 1 do begin
    ;----------------------------------------------------------
    ; Replace Token with Time /////////////////////////////////
    ;----------------------------------------------------------
        case tokens[i] of
            ;Try to form a 4-digit year from a 2-digit year
            'Y': begin
                if year[0] eq '' then if yr[0] ne '' $
                    then year = MrTimeParser_YearToYr(yr, /FROM_YR) $
                    else message, 'Cannot form "%Y". Must give %Y or %y.'
                subStr = year
            endcase
            
            ;Try to form a 2-digit fom a 4-digit year
            'y': begin
                if yr[0] eq '' then if year[0] ne '' $
                    then yr = MrTimeParser_YearToYr(year) $
                    else message, 'Cannot form "%y". Must give %y, %Y.'
                subStr = yr
            endcase
            
            ;Try to form a 2-digit month from a calendar month name or the day-of-year
            'M': begin
                ;Month Name
                if month[0] eq '' then if cmonth[0] ne '' then begin
                    month = MonthNameToNumber(cmonth)
                
                ;Abbreviated Month Name
                endif else if calmo[0] ne '' then begin
                    month = MonthNameToNumber(calmo, /ABBR)
                
                ;Day of Year
                endif else if doy[0] ne '' then begin
                    if year[0] eq '' $
                        then MrTimeParser_DissectDOY, doy, month, YEAR=yr $
                        else MrTimeParser_DissectDOY, doy, month, YEAR=year
                endif else begin
                    message, 'Cannot form "%M". Must give (%M, %C, or %c) or %D.'
                endelse
                subStr = month
            endcase
            
            ;Try to form calendar month name from the abbreviated name, month number,
            ;or day-of-year
            'C': begin
                ;Month Number
                if cmonth[0] eq '' then if month[0] ne '' then begin
                    cmonth = MonthNumberToName(month)
                
                ;Abbreviated Month Name
                endif else if calmo[0] ne '' then begin
                    mo_temp = MonthNameToNumber(calmo, /ABBR)
                    cmonth  = MonthNumberToName(temporary(mo_temp))
                
                ;Day of Year
                endif else if doy[0] ne '' then begin
                    if year[0] eq '' $
                        then MrTimeParser_DissectDOY, doy, month, YEAR=yr $
                        else MrTimeParser_DissectDOY, doy, month, YEAR=year
                    
                    ;Take the month portion
                    month = monthNumberToName(month)
                endif else begin
                    message, 'Cannot form "%C". Must give %M, %C, %c or %D.'
                endelse
                subStr = cmonth
            endcase
            
            ;Try to form the abbreviated calendar month name from the full name,
            ;month number, or day-of-year
            'c':  begin
                ;Month Number
                if calmo[0] eq '' then if month[0] ne '' then begin
                    calmo = MonthNumberToName(month, /ABBR)
                    
                ;Month Name
                endif else if cmonth[0] ne '' then begin
                    mo_temp = MonthNameToNumber(cmonth)
                    calmo  = MonthNumberToName(temporary(mo_temp), /ABBR)
                
                ;Day of Year
                endif else if doy[0] ne '' then begin
                    if year[0] eq '' $
                        then MrTimeParser_DissectDOY, doy, month, YEAR=yr $
                        else MrTimeParser_DissectDOY, doy, month, YEAR=year
                    
                    ;Take the month portion
                    month = monthNumberToName(month, /ABBR)
                endif else begin
                    message, 'Cannot form "%c". Must give %M and, %C, %c or %D.'
                endelse
                subStr = calmo
            endcase
            
            ;Day of month
            'd': begin
                ;Day of year
                if day[0] eq '' then if doy[0] ne '' then begin
                    if year[0] eq '' $
                        then MrTimeParser_DissectDOY, doy, month, day, YEAR=yr $
                        else MrTimeParser_DissectDOY, doy, month, day, YEAR=year
                endif else begin
                    message, 'Cannot form "%d". Must give %d or %D.'
                endelse
                subStr = day
            endcase
            
            ;Day of year
            'D': begin
                ;Year, Month, and Day
                if doy[0] eq '' then begin
                    ;Get the month numer
                    if month[0] eq '' then if cmonth[0] ne '' then begin
                        month = MonthNameToNumber(cmonth)
                    endif else if calmo[0] ne '' then begin
                        month = MonthNameToNumber(calmo, /ABBR)
                    endif else begin
                        message, 'Cannot form "%D". Must give %D or [(%M, %C or %c) and %d with optional (%Y or %y)].'
                    endelse
                    
                    ;Get the day number
                    if day[0] eq '' then begin
                        message, 'Cannot form "%D". Must give %D or [(%M, %C or %c) and %d with optional (%Y or %y)].'
                    endif
                    
                    ;Year
                    if year[0] eq '' then if yr[0] ne '' then begin
                        year = MrTimeParser_YearToYr(yr, /FROM_YR)
                    endif else begin
                        message, 'Year not given. Finding day-of-year with non-leap year.', /INFORMATIONAL
                    endelse
                    
                    ;Compute the day of year.
                    if n_elements(year) gt 0 $
                        then doy = year_day(year + month + day) $
                        else doy = year_day('2001' + month + day)
                endif
                
                subStr = string(doy, FORMAT='(i03)')
            endcase
            
            ;Days of week.
            'W': begin
                ;Abbreviated Week Day
                if weekday[0] eq '' then if wkday[0] ne '' then begin
                    wkno    = MrWeekDayToNumber(wkday, /ABBR)
                    weekday = MrWeekNumberToDay(wkno)
                
                ;Date
                endif else begin
                    ;Year
                    if year[0] eq '' then if yr[0] ne '' $
                        then year = MrTimeParser_YearToYr(yr, /FROM_YR) $
                        else message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Month
                    if month[0] eq '' then if cmonth[0] ne '' then begin
                        month = monthNameToNumber(cmonth)
                    endif else if calmo[0] ne '' then begin
                        month = monthNameToNumber(calmo)
                    endif else begin
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    endelse
                    
                    ;DOY
                    if doy[0] ne '' then if month[0] eq '' || day[0] eq '' then begin
                        if year[0] eq '' $
                            then MrTimeParser_DissectDOY, doy, month, day, YEAR=yr $
                            else MrTimeParser_DissectDOY, doy, month, day, YEAR=year
                    endif
                    
                    ;Day
                    if day[0] eq '' then $
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Calculate the week day
                    weekday = MrDayOfWeek(year, month, day)
                endelse
                subStr = weekday
            endcase
            
            
            'w': begin
                ;Week day
                if wkday[0] eq '' then if weekday[0] ne '' then begin
                    wkno    = MrWeekDayToNumber(wkday)
                    weekday = MrWeekNumberToDay(wkno, /ABBR)
                
                ;Date
                endif else begin
                    ;Year
                    if year[0] eq '' then if yr[0] ne '' $
                        then year = MrTimeParser_YearToYr(yr, /FROM_YR) $
                        else message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Month
                    if month[0] eq '' then if cmonth[0] ne '' then begin
                        month = monthNameToNumber(cmonth)
                    endif else if calmo[0] ne '' then begin
                        month = monthNameToNumber(calmo)
                    endif else begin
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    endelse
                    
                    ;DOY
                    if doy[0] ne '' then if month[0] eq '' || day[0] eq '' then begin
                        if year[0] eq '' $
                            then MrTimeParser_DissectDOY, doy, month, day, YEAR=yr $
                            else MrTimeParser_DissectDOY, doy, month, day, YEAR=year
                    endif
                    
                    ;Day
                    if day[0] eq '' then $
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Calculate the week day
                    wkday = MrDayOfWeek(year, month, day, /ABBR)
                endelse
                subStr = wkday
            endcase
            
            ;24-Hour Clock
            'H': begin
                ;12-Hour clock?
                if hour[0] eq '' then if hr[0] ne '' then begin
                    hour = hr
                    ;AM/PM -- assume AM if not given
                    if am_pm[0] eq '' then begin
                        message, '%A not given. Assuming AM.', /INFORMATIONAL
                    ;Convert to 24-hour clock by adding 12 to the hours 1-11pm
                    endif else begin
                        iPM = where(am_pm eq 'PM', nPM)
                        if nPM gt 0 then begin
                            iPNoon = where(fix(hr[iPM]) lt 12, nPNoon)
                            if nPNoon gt 0 then hour[iPM[iPNoon]] = string(fix(hr[iPM[iPNoon]]) + 12, FORMAT='(i02)')
                        endif
                    endelse
                endif else begin
                    message, 'Cannot form "%H". Must give %H or %h.'
                endelse
                subStr = hour
            endcase
            
            ;12-Hour Clock.
            'h': begin
                ;24-hour clock?
                if hr[0] eq '' then if hour[0] ne '' then begin
                    hr = hour
                    iPM = where(fix(hour) ge 12, nPM)
                    if nPM gt 0 then hr[iPM] = string(fix(hour[iPM]) - 12, FORMAT='(i02)')
                    ;AM or PM?
                    if am_pm[0] eq '' then begin
                        am_pm = strarr(n_elements(hr)) + 'AM'
                        if nPM gt 0 then am_pm[iPM] = 'PM'
                    endif
                endif else begin
                    message, 'Cannot form "%h". Must give %H or %h (with optional %A).'
                endelse
                subStr = hr
            endcase
            
            ;Minutes, Seconds, Milli, Micro, Nano, Pico, AM_PM
            'm': if minute[0] eq '' then message, 'Cannot form "%m". Must give %m.' else subStr = minute
            'S': if second[0] eq '' then message, 'Cannot form "%S". Must give %S.' else subStr = second
            
            ;Fraction of a second
            'f': begin
                ;Milli, etc. given?
                if decimal[0] eq '' then if milli[0] ne '' then begin
                    decimal = strmid(milli + '000', 0, 3)
                    if micro[0] ne '' then decimal += micro
                    if nano[0]  ne '' then decimal += nano
                    if pico[0]  ne '' then decimal += pico
                endif else begin
                    decimal = '0'
                endelse
                subStr = '.' + decimal
            endcase
            
            ;Milliseconds
            '1': begin
                if milli[0] eq '' then if decimal[0] ne '' $
                    then milli = strmid(decimal + '000', 0, 3) $
                    else milli = '000'
                subStr = milli
            endcase
            
            ;Microseconds
            '2': begin
                if micro[0] eq '' then if decimal[0] ne '' $
                    then micro = strmid(decimal + '000000', 3, 3) $
                    else micro = '000'
                subStr = micro
            endcase
            
            ;Nanoseconds
            '3': begin
                if nano[0] eq '' then if decimal[0] ne '' $
                    then nano = strmid(decimal + '000000000', 6, 3) $
                    else nano = '000'
                subStr = nano
            endcase
            
            ;Picoseconds
            '4': begin
                if pico[0] eq '' then if decimal[0] ne '' $
                    then pico = strmid(decimal + '000000000000', 9, 3) $
                    else pico = '000'
                subStr = pico
            endcase
            
            ;AM/PM
            'A': if am_pm[0]  eq '' then message, 'Cannot form "%A". Must give %A.' else subStr = am_pm
            
            ;Time Zone
            'z': begin
                if time_zone[0] eq '' then if offset[0] ne '' $
                    then time_zone = MrTimeZoneOffsetToName(offset) $
                    else time_zone = 'Z'
                
                subStr = time_zone
            endcase
            
            ;Offset
            'o': begin
                if offset[0] eq '' then begin
                    if time_zone[0] ne '' $
                        then offset = MrTimeZoneNameToOffset(time_zone) $
                        else offset = '+00:00'
                endif
                
                subStr = offset
            endcase
            
            ;Ignore parentheses
            '(': subStr = strmid(pattern, positions[i]+2, positions[i+1]-positions[i]-2)
            
            else: message, 'Token "' + tokens[i] + '" not recognized.'
        endcase
        
    ;----------------------------------------------------------
    ; Piece Together Result ///////////////////////////////////
    ;----------------------------------------------------------
        ;Allocate memory
        ;   - Do not know initially which keywords were given (to check number of elements).
        ;   - If the first token is "%(", SUBSTR does not have any time information and
        ;       is a scalar. Must wait for a different token.
        if n_elements(timeOut) eq 1 && n_elements(subStr) gt 1 $
            then timeOut = replicate(timeOut, n_elements(subStr))

        ;Take pieces between tokens from the pattern
        ;   - CURPOS is the position after the previous token
        ;   - POSITIONS[i]-CURPOS are the number of characters trailing the previous token.
        timeOut += strmid(pattern, curPos, positions[i]-curPos) + subStr

        ;Skip over the current token
        ;   - Must also skip over the characters after the last token.
        curPos += 2 + positions[i] - curPos
        
        ;Skip over ')'
        if tokens[i] eq '(' then begin
            i += 1
            if i lt nTokens-1 then curPos = positions[i] + 2
        endif
    endfor
    
    ;Include the substring trailing the final token
    tail = strmid(pattern, positions[i-1]+2)
    if tail ne '' then timeOut += tail

    if n_elements(timeOut) eq 1 then timeOut = timeOut[0]
end

;+
;   Given a pattern with tokens, build a time string given its components.
;
; :Private:
;
; :Params:
;       TIME:               out, required, type=string/strarr
;                           Result of combining the date and time elements via `PATTERN`.
;       PATTERN:            in, required, type=string
;                           Pattern describing how `TIME` should be built.
;
; :Keywords:
;       YEAR:               in, optional, type=strarr
;                           4-digit year that matches a %Y token.
;       YR:                 in, optional, type=strarr
;                           2-digit year that matches a %y token.
;       DOY:                in, optional, type=strarr
;                           3-digit day-of-year that matches a %D.
;       MONTH:              in, optional, type=strarr
;                           2-digit month that matches a %M token.
;       CMONTH:             in, optional, type=strarr
;                           Calendar month name (e.g. January, February, etc.) that
;                               matches a %C tokenl.
;       CALMO:              in, optional, type=strarr
;                           3-character abbreviated calendar month name (e.g. Jan, Feb, ...)
;                               that matches a %c token.
;       WEEKDAY:            in, optional, type=strarr
;                           Weekday (e.g. Monday, Tuesday, etc.) that matches a %W token
;                               for the [start, end] of the file interval.
;       WKDAY:              in, optional, type=strarr
;                           3-character abbreviated week day (e.g. Mon, Tue, etc.) that
;                               matches a %M token.
;       DAY:                in, optional, type=strarr
;                           2-digit day that matches a %d token.
;       HOUR:               in, optional, type=strarr
;                           2-digit hour on a 24-hour clock that matches a %H token.
;       HR:                 in, optional, type=strarr
;                           2-digit hour on a 12-hour clock that matches a %h token.
;       MINUTE:             in, optional, type=strarr
;                           2-digit minute that matches a %m token.
;       SECOND:             in, optional, type=strarr
;                           2-digit second that matches a %S token.
;       DECIMAL:            in, optional, type=strarr
;                           Fraction of a second that matches the %f token.
;       MILLI:              in, optional, type=strarr
;                           3-digit milli-second that matches a %1 token.
;       MICRO:              in, optional, type=strarr
;                           3-digit micro-second that matches a %2 token.
;       NANO:               in, optional, type=strarr
;                           3-digit nano-second that matches a %3 token.
;       PICO:               in, optional, type=strarr
;                           3-digit pico-second that matches a %4 token.
;       AM_PM:              in, optional, type=strarr
;                           "AM" or "PM" string that matches a %A.
;       OFFSET:             in, optional, type=string
;                           Offset from UTC. (+|-)hh[:][mm]. Matches %o.
;       TIME_ZONE:          in, optional, type=string
;                           Time zone abbreviated name. Matches %z.
;-
pro MrTimeParser_Compute_v2, timeOut, pattern, $
YEAR=year, $
YR=yr, $
DOY=doy, $
MONTH=month, $
CMONTH=cMonth, $
CALMO=calmo, $
WEEKDAY=weekday, $
WKDAY=wkday, $
DAY=day, $
HOUR=hour, $
HR=hr, $
MINUTE=minute, $
SECOND=second, $
DECIMAL=decimal, $
MILLI=milli, $
MICRO=micro, $
NANO=nano, $
PICO=pico, $
AM_PM=am_pM, $
TIME_ZONE=time_zone, $
OFFSET=offset
    compile_opt strictarr
    on_error, 2

    ;Extract the tokens
    tokens = MrTokens_Extract(pattern, COUNT=nTokens, POSITIONS=positions)

    ;Step through each token
    curPos  = 0
    timeOut = ''
    for i = 0, nTokens - 1 do begin
    ;----------------------------------------------------------
    ; Replace Token with Time /////////////////////////////////
    ;----------------------------------------------------------
        case tokens[i] of
            'Y': substr = MrTimeParser_GetYear(year, yr)
            'y': substr = MrTimeParser_GetYr(yr, year)
            'M': substr = MrTimeParser_GetMonth(month, cmonth, calmo, doy)
            'C': substr = MrTimeParser_GetCMonth(cmonth, month, calmo, doy)
            'c': substr = MrTimeParser_GetCalMo(calmo, month, cmonth, doy)
            'd': substr = MrTimeParser_GetDay(day, doy)
            'D': substr = MrTimeParser_GetDOY(doy, year, yr, month, cmonth, calmo)
            
            ;Days of week.
            'W': begin
                ;Abbreviated Week Day
                if weekday[0] eq '' then if wkday[0] ne '' then begin
                    wkno    = MrWeekDayToNumber(wkday, /ABBR)
                    weekday = MrWeekNumberToDay(wkno)
                
                ;Date
                endif else begin
                    ;Year
                    if year[0] eq '' then if yr[0] ne '' $
                        then year = MrTimeParser_YearToYr(yr, /FROM_YR) $
                        else message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Month
                    if month[0] eq '' then if cmonth[0] ne '' then begin
                        month = monthNameToNumber(cmonth)
                    endif else if calmo[0] ne '' then begin
                        month = monthNameToNumber(calmo)
                    endif else begin
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    endelse
                    
                    ;DOY
                    if doy[0] ne '' then if month[0] eq '' || day[0] eq '' then begin
                        if year[0] eq '' $
                            then MrTimeParser_DissectDOY, doy, month, day, YEAR=yr $
                            else MrTimeParser_DissectDOY, doy, month, day, YEAR=year
                    endif
                    
                    ;Day
                    if day[0] eq '' then $
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Calculate the week day
                    weekday = MrDayOfWeek(year, month, day)
                endelse
                subStr = weekday
            endcase
            
            
            'w': begin
                ;Week day
                if wkday[0] eq '' then if weekday[0] ne '' then begin
                    wkno    = MrWeekDayToNumber(wkday)
                    weekday = MrWeekNumberToDay(wkno, /ABBR)
                
                ;Date
                endif else begin
                    ;Year
                    if year[0] eq '' then if yr[0] ne '' $
                        then year = MrTimeParser_YearToYr(yr, /FROM_YR) $
                        else message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Month
                    if month[0] eq '' then if cmonth[0] ne '' then begin
                        month = monthNameToNumber(cmonth)
                    endif else if calmo[0] ne '' then begin
                        month = monthNameToNumber(calmo)
                    endif else begin
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    endelse
                    
                    ;DOY
                    if doy[0] ne '' then if month[0] eq '' || day[0] eq '' then begin
                        if year[0] eq '' $
                            then MrTimeParser_DissectDOY, doy, month, day, YEAR=yr $
                            else MrTimeParser_DissectDOY, doy, month, day, YEAR=year
                    endif
                    
                    ;Day
                    if day[0] eq '' then $
                        message, 'Cannot form %W. Must give %W or provice [%Y], [%M, %C, %C], [%D, %d].'
                    
                    ;Calculate the week day
                    wkday = MrDayOfWeek(year, month, day, /ABBR)
                endelse
                subStr = wkday
            endcase
            
            ;24-Hour Clock
            'H': begin
                ;12-Hour clock?
                if hour[0] eq '' then if hr[0] ne '' then begin
                    hour = hr
                    ;AM/PM -- assume AM if not given
                    if am_pm[0] eq '' then begin
                        message, '%A not given. Assuming AM.', /INFORMATIONAL
                    ;Convert to 24-hour clock by adding 12 to the hours 1-11pm
                    endif else begin
                        iPM = where(am_pm eq 'PM', nPM)
                        if nPM gt 0 then begin
                            iPNoon = where(fix(hr[iPM]) lt 12, nPNoon)
                            if nPNoon gt 0 then hour[iPM[iPNoon]] = string(fix(hr[iPM[iPNoon]]) + 12, FORMAT='(i02)')
                        endif
                    endelse
                endif else begin
                    message, 'Cannot form "%H". Must give %H or %h.'
                endelse
                subStr = hour
            endcase
            
            ;12-Hour Clock.
            'h': begin
                ;24-hour clock?
                if hr[0] eq '' then if hour[0] ne '' then begin
                    hr = hour
                    iPM = where(fix(hour) ge 12, nPM)
                    if nPM gt 0 then hr[iPM] = string(fix(hour[iPM]) - 12, FORMAT='(i02)')
                    ;AM or PM?
                    if am_pm[0] eq '' then begin
                        am_pm = strarr(n_elements(hr)) + 'AM'
                        if nPM gt 0 then am_pm[iPM] = 'PM'
                    endif
                endif else begin
                    message, 'Cannot form "%h". Must give %H or %h (with optional %A).'
                endelse
                subStr = hr
            endcase
            
            ;Minutes, Seconds, Milli, Micro, Nano, Pico, AM_PM
            'm': if minute[0] eq '' then message, 'Cannot form "%m". Must give %m.' else subStr = minute
            'S': if second[0] eq '' then message, 'Cannot form "%S". Must give %S.' else subStr = second
            
            ;Fraction of a second
            'f': begin
                ;Milli, etc. given?
                if decimal[0] eq '' then if milli[0] ne '' then begin
                    decimal = strmid(milli + '000', 0, 3)
                    if micro[0] ne '' then decimal += micro
                    if nano[0]  ne '' then decimal += nano
                    if pico[0]  ne '' then decimal += pico
                endif else begin
                    decimal = '0'
                endelse
                subStr = '.' + decimal
            endcase
            
            ;Milliseconds
            '1': begin
                if milli[0] eq '' then if decimal[0] ne '' $
                    then milli = strmid(decimal + '000', 0, 3) $
                    else milli = '000'
                subStr = milli
            endcase
            
            ;Microseconds
            '2': begin
                if micro[0] eq '' then if decimal[0] ne '' $
                    then micro = strmid(decimal + '000000', 3, 3) $
                    else micro = '000'
                subStr = micro
            endcase
            
            ;Nanoseconds
            '3': begin
                if nano[0] eq '' then if decimal[0] ne '' $
                    then nano = strmid(decimal + '000000000', 6, 3) $
                    else nano = '000'
                subStr = nano
            endcase
            
            ;Picoseconds
            '4': begin
                if pico[0] eq '' then if decimal[0] ne '' $
                    then pico = strmid(decimal + '000000000000', 9, 3) $
                    else pico = '000'
                subStr = pico
            endcase
            
            ;AM/PM
            'A': if am_pm[0]  eq '' then message, 'Cannot form "%A". Must give %A.' else subStr = am_pm
            
            ;Time Zone
            'z': begin
                if time_zone eq '' then if offset ne '' $
                    then time_zone = MrTimeZoneOffsetToName(offset) $
                    else time_zone = 'Z'
                
                subStr = time_zone
            endcase
            
            ;Offset
            'o': begin
                if offset eq '' then begin
                    if offset ne '' $
                        then offset = MrTimeZoneNameToOffset(time_zone) $
                        else offset = '+00:00'
                endif
                
                subStr = offset
            endcase
            
            ;Ignore parentheses
            '(': subStr = strmid(pattern, positions[i]+2, positions[i+1]-positions[i]-2)
            
            else: message, 'Token "' + tokens[i] + '" not recognized.'
        endcase
        
    ;----------------------------------------------------------
    ; Piece Together Result ///////////////////////////////////
    ;----------------------------------------------------------
        ;Allocate memory
        ;   - Do not know initially which keywords were given (to check number of elements).
        ;   - If the first token is "%(", SUBSTR does not have any time information and
        ;       is a scalar. Must wait for a different token.
        if n_elements(timeOut) eq 1 && n_elements(subStr) gt 1 $
            then timeOut = replicate(timeOut, n_elements(subStr))

        ;Take pieces between tokens from the pattern
        ;   - CURPOS is the position after the previous token
        ;   - POSITIONS[i]-CURPOS are the number of characters trailing the previous token.
        timeOut += strmid(pattern, curPos, positions[i]-curPos) + subStr

        ;Skip over the current token
        ;   - Must also skip over the characters after the last token.
        curPos += 2 + positions[i] - curPos
        
        ;Skip over ')'
        if tokens[i] eq '(' then begin
            i += 1
            if i lt nTokens-1 then curPos = positions[i] + 2
        endif
    endfor
    
    ;Include the substring trailing the final token
    tail = strmid(pattern, positions[i-1]+2)
    if tail ne '' then timeOut += tail

    if n_elements(timeOut) eq 1 then timeOut = timeOut[0]
end


;+
;   Get (or calculate) the 4-digit year.
;
; :Returns:
;       YEAR:       The 4-digit year
;-
function MrTimeParser_GetYear
	compile_opt idl2
	on_error, 2
	
	;Do not have YEAR
	if year[0] eq '' then begin
		;Must have YR
		if yr[0] eq '' then message, 'Cannot form "%Y". Must give %Y or %y.'
		
		;2-Digit year
		;   00-59 -> 1900-1959
		;   60-99 -> 2060-2099
		i19 = where(fix(yr) ge 60, n19, COMPLEMENT=i20, NCOMPLEMENT=n20)
		year = yr
		if n19 gt 0 then year[i19] = '19' + year[i19]
		if n20 gt 0 then year[i20] = '20' + year[i20]
	endif
	
	return, year
end


;+
;   Get (or calculate) the 2-digit year.
;
; :Returns:
;       YR:         The 2-digit year
;-
function MrTimeParser_GetYr
	compile_opt idl2
	on_error, 2

	;Do not have YEAR
	if yr[0] eq '' then begin
		;Must have YEAR
		if year[0] eq '' then message, 'Cannot form "%y". Must give %Y or %y.'
		
		;Issue error (losing information)
		MrPrintF, 'LogWarn', 'Converting 4-digit year to 2-digit year.', /INFORMATIONAL
		
		;Parse YEAR
		yr = stregex(year, '[0-9]{2}([0-9]{2})', /SUBEXP, /EXTRACT)
		yr = reform(yr[1,*])
	endif
	
	return, yr
end


;+
;   Get (or calculate) the 2-digit month number.
;
; :Returns:
;       MONTH:      The 2-digit year
;-
function MrTimeParser_GetMonth
	compile_opt idl2
	on_error, 2

	;Calculate from (abbreviated) calendar month name or day-of-year
	if month[0] eq '' then begin
		case 1 of
			cmonth[0] ne '': month = MonthNameToNumber(cmonth)
			calmo[0]  ne '': month = MonthNameToNumber(calmo, /ABBR)
			doy[0]    ne '': MrTimeParser_ParseDOY, month, day
			else: message, 'Cannot form "%M". Must give (%M, %C, %c or %D).'
		endcase
	endif
	
	return, month
end


;+
;   Get (or calculate) the calendar month name.
;
; :Returns:
;       CMONTH:     The calendar month name.
;-
function MrTimeParser_GetCMonth
	compile_opt idl2
	on_error, 2

	;Calculate from 2-digit month number, abbreviated calendar month name, or day-of-year
	if cmonth[0] eq '' then begin
		case 1 of
			month[0]  ne '': cmonth = MonthNumberToName(month)
			calmo[0]  ne '': cmonth = MonthNameToNumber( MonthNumberToName(calmo), /ABBR )
			doy[0]    ne '': begin
				MrTimeParser_ParseDOY, month, day
				cmonth = MonthNumberToName(month)
			endcase
			else: message, 'Cannot form "%C". Must give %M, %C, %c or %D.'
		endcase
	endif
	
	return, cmonth
end


;+
;   Get (or calculate) the abbreviated calendar month name.
;
; :Returns:
;       CALMO:      The abbreviated calendar month name.
;-
function MrTimeParser_GetCalMo
	compile_opt idl2
	on_error, 2

	;Calculate from 2-digit month number, abbreviated calendar month name, or day-of-year
	if cmonth[0] eq '' then begin
		case 1 of
			month[0]  ne '': cmonth = MonthNumberToName(month, /ABBR)
			calmo[0]  ne '': cmonth = MonthNameToNumber( MonthNumberToName(calmo) )
			doy[0]    ne '': begin
				MrTimeParser_ParseDOY, month, day
				cmonth = MonthNumberToName(month, /ABBR)
			endcase
			else: message, 'Cannot form "%c". Must give %M and, %C, %c or %D.'
		endcase
	endif
	
	return, cmonth
end


;+
;   Get (or calculate) the day-of-month.
;
; :Returns:
;       DAY:        The abbreviated calendar month name.
;-
function MrTimeParser_GetDay
	compile_opt idl2
	on_error, 2

	;Calculate from day-of-year
	if day[0] eq '' then begin
		case 1 of
			doy[0] ne '': MrTimeParser_ParseDOY, month, day
			else: message, 'Cannot form "%d". Must give %d or %D.'
		endcase
	endif
	
	return, day
end


;+
;   Get (or calculate) the day-of-year.
;
; :Returns:
;       DOY:        The day-of-year.
;-
function MrTimeParser_GetDOY
	compile_opt idl2
	on_error, 2

	if doy[0] eq '' then begin
		
		;Get the month and day
		catch, the_error
		if the_error eq 0 then begin
			month = MrTimeParser_GetMonth()
			day   = MrTimeParser_GetDay()
		endif else begin
			;Redirect error to parent
			catch, /CANCEL
			on_error, 2
			message, 'Cannot form "%D". Must give %D or [(%M, %C or %c) and %d with optional (%Y or %y)].'
		endelse
		
		;Get the year
		catch, the_error
		if the_error eq 0 then begin
			year = MrTimeParser_YearToYr(yr, /FROM_YR)
			date = year + month + day
		endif else begin
			MrPrintF, 'LogWarn', 'Year not given. Finding DOY with non-leap year.'
			date = '2001' + month + day
		endelse
		catch, /CANCEL
		on_error, 2
		
		;Finally, create DOY
		doy = year_day(date)
	endif
	
	return, day
end


;+
;   Parse the day-of-year into month and day.
;
; :Params:
;       MONTH:      out, optional, type=string
;                   Month corresponding to `DOY`.
;       DAY:        out, optional, type=string
;                   Day of the `MONTH` corresponding to `DOY`.
;-
pro MrTimeParser_ParseDOY, month_out, day_out
	compile_opt strictarr
	on_error, 2

	;Check if DOY was given
	if doy[0] eq '' then message, 'DOY not given. Cannot parse.'

	;Try to get the year
	catch, the_error
	if the_error eq 0 then begin
		year = MrTimeParser_GetYear()
		catch, /CANCEL
	endif else begin
		catch, /CANCEL
		MrPrintF, 'LogWarn', 'No year given. Converting %D to %M for non-leap year.'
		year = replicate('2001', n_elements(doy))
	endelse
	on_error, 2

	;Get the month and day
	monthday = year_day(doy, YEAR=year, /TO_MODAY)
	month    = strmid(monthday, 0, 2)
	day      = strmid(monthday, 3, 2)
end


;+
;   Convert a four-digit year to a two-digit year and vice versa
;
; :Params:
;       DOY:        in, required, type=string
;                   Day of the year, from 0-366
;       MONTH:      out, optional, type=string
;                   Month corresponding to `DOY`.
;       DAY:        out, optional, type=string
;                   Day of the `MONTH` corresponding to `DOY`.
;
; :Keywords:
;       YEAR:       in, optional, type=string/strarr, default='2001'
;                   Year in which `DOY` occurs. Necessary for determining leap year.
;                       Non-leap year is assumed.
;-
pro MrTimeParser_DissectDOY, doy, month, day, $
YEAR=year
    compile_opt strictarr
    on_error, 2
    
    ;Year given?
    if n_elements(year) gt 0 then begin
        _year = year
        
        ;Was a year given?
        iBlank = where(_year eq '', count)
        if count gt 0 then begin
            _year[iBlank] = '2001'
            message, 'No year given. Converting %D to %M for non-leap year.', /INFORMATIONAL
        endif
        
        ;Two digit years?
        iTwo = where(strlen(_year) eq 2, count)
        if count gt 0 $
            then _year[iTwo] = MrTimeParser_YearToYr(_year[iTwo], /FROM_YR)
            
    ;No year given?
    endif else begin
        message, 'No year given. Converting %D to %M for non-leap year.', /INFORMATIONAL
        _year = replicate('2001', n_elements(doy))
    endelse

    ;Get the month and day
    monthday = year_day(doy, YEAR=year, /TO_MODAY)
    month = strmid(monthday, 0, 2)
    day   = strmid(monthday, 3, 2)
end


;+
;   Look-up function for commonly used patterns.
;
; :Private:
;
; :Params:
;       OPTION:         in, required, type=integer
;                       Number corresponding to the desired pattern.
;
; :Returns:
;       PATTERN:        The pre-defined pattern.
;-
function MrTimeParser_Patterns, option
    compile_opt strictarr
    on_error, 2
    
    ;Was one of the pre-defined patterns given?
    case option of
         1: pattern = '%Y-%M-%dT%H:%m:%SZ'            ;ISO-8601:        1951-01-09T08:21:10Z
         2: pattern = '%d-%c-%Y %H:%m:%S.%1'          ;CDF_EPOCH:       09-Jan-1951 08:21:10.000
         3: pattern = '%d-%c-%Y %H:%m:%S.%1.%2.%3.%4' ;CDF_EPOCH16:     09-Jan-1951 08:21:10.000.000.000.000
         4: pattern = '%Y-%M-%dT%H:%m:%S.%1%2%3'      ;CDF_TIME_TT2000: 1951-01-09T08:21:10.000000000
         5: pattern = '%d %c %Y'                      ;09 Jan 1951
         6: pattern = '%d %c %Y %H:%m:%S'             ;09 Jan 1951 08:21:10
         7: pattern = '%d %c %Y %Hh %mm %Ss'          ;09 Jan 1951 08h 21m 10s
         8: pattern = '%d %C %Y'                      ;09 January 1951
         9: pattern = '%d %C %Y %H:%m:%S'             ;09 January 1951 08:21:10
        10: pattern = '%d %C %Y %Hh %mm %Ss'          ;09 January 1951 08h 21m 10s
        11: pattern = '%c %d, %Y'                     ;Jan 09, 1951
        12: pattern = '%c %d, %Y %H:%m:%S'            ;Jan 09, 1951 08:21:10
        13: pattern = '%c %d, %Y %Hh %mm %Ss'         ;Jan 09, 1951 08h 21m 10s
        14: pattern = '%c %d, %Y'                     ;Jan 09, 1951
        15: pattern = '%C %d, %Y %H:%m:%S'            ;January 09, 1951 08:21:10
        16: pattern = '%C %d, %Y %Hh %mm %Ss'         ;January 09, 1951 08h 21m 10s
        17: pattern = '%C %d, %Y %H:%m:%S'            ;January 09, 1951 08:21:10
        18: pattern = '%Y-%D'                         ;1951-009
        19: pattern = '%Y-%D %H:%m:%S'                ;1951-009 08:21:10
        20: pattern = '%Y-%D %Hh %mm %Ss'             ;1951-009 08h 21m 10s
        21: pattern = '%D-%Y'                         ;009-1951
        22: pattern = '%D-%Y %H:%m:%S'                ;009-1951 08:21:10
        23: pattern = '%D-%Y %Hh %mm %Ss'             ;009-1951 08h 21m 10s
        24: pattern = '%Y%M%d'                        ;19510109
        25: pattern = '%d%M%Y'                        ;09011951
        26: pattern = '%M%d%Y'                        ;01091951
        27: pattern = '%W, %C %d, %Y'                 ;Tuesday, January 09, 1951
        28: pattern = '%W, %C %d, %Y %H:%m:%S'        ;Tuesday, January 09, 1951 08:21:10
        29: pattern = '%W, %C %d, %Y %Hh %mm %Ss'     ;Tuesday, January 09, 1951 08h 21m 10s
        30: pattern = '%w, %c %d, %Y'                 ;Tue, Jan 09, 1951
        31: pattern = '%w, %c %d, %Y %H:%m:%S'        ;Tue, Jan 09, 1951 08:21:10
        32: pattern = '%w, %c %d, %Y %Hh %mm %Ss'     ;Tue, Jan 09, 1951 08h 21m 10s
        else: message, 'Pattern option ' + strtrim(option, 2) + ' not recognized.'
    endcase
    
    return, pattern
end


;+
;   The purpose of this method is to retreive the order of the embedded date and time
;   codes.
;
; :Private
;
; :Params:
;       TOKEN:          in, required, type=string
;                       A character referencing a portion of a date or time.
;
; :Returns:
;       ORDER:          The order of the date/time code.
;-
function MrTimeParser_TokenOrder, token
    compile_opt strictarr
    on_error, 2
    
    case token of
        'y': order = 0
        'Y': order = 0
        'M': order = 1
        'C': order = 1
        'c': order = 1
        'D': order = 1
        'd': order = 2
        'W': order = 2
        'w': order = 2
        'H': order = 3
        'h': order = 3
        'm': order = 4
        'S': order = 5
        'f': order = 6
        '1': order = 6
        '2': order = 7
        '3': order = 8
        '4': order = 9
        'A': order = 10
        else: order = -1
    endcase
    
    return, order
end


;+
;   Convert a four-digit year to a two-digit year and vice versa
;
; :Params:
;       YR:         in, required, type=string
;                   The 4-digit year to be converted to a 2-digit year.
;
; :Keywords:
;       FROM_TWO:   in, optional, type=boolean, default=0
;                   If set, convert from a 2-digit year to a 4-digit year.
;-
function MrTimeParser_YearToYr, yr, $
FROM_YR=from_yr
    compile_opt strictarr
    on_error, 2
    
    ;2-Digit year to 4-Digit year
    if keyword_set(from_yr) then begin
        ;2-Digit year
        ;   00-59 -> 1900-1959
        ;   60-99 -> 2060-2099
        i19 = where(fix(yr) ge 60, n19, COMPLEMENT=i20, NCOMPLEMENT=n20)
        year = yr
        if n19 gt 0 then year[i19] = '19' + year[i19]
        if n20 gt 0 then year[i20] = '20' + year[i20]
        
    ;4-Digit year to 2-Digit year
    endif else begin
        message, 'WARNING: Converting 4-digit year to 2-digit year.', /INFORMATIONAL
        yr = stregex(year, '[0-9]{2}([0-9]{2})', /SUBEXP, /EXTRACT)
        yr = reform(yr[1,*])
    endelse
    
    return, year
end


;+
;   The initialization method. Here, a directory and filename can be given with a date
;   and time code embedded.
;
;   Calling Sequence::
;       MrTimeParser, timeIn, patternIn[, /BEAKDOWN], ...
;       MrTimeParser, timeIn, patternIn, ISMATCH=isMatch, ...
;       MrTimeParser, timeOut[, /COMPUTE], ...
;       MrTimeParser, timeIn, patternIn, patternOut, timeOut, ...
;       
;
; :Params:
;       TIME:           in, out, required, type=string/strarr
;                       Time strings. If COMPUTE is set, this is an output and the
;                           keywords below will be used to build it. If BREAKDOWN is
;                           set, then TIME will be broken into its date and time
;                           components.
;       PATTERNIN:      in, required, type=string
;                       The pattern used to breakdown or compute `TIME`. See the file
;                           header for token options.
;       PATTERNOUT:     in, optional, type=string
;                       If `TIME` is to be converted from one format to another, then this
;                           pattern defines the output format. See the `BOOLEAN` keyword
;                           for exceptions.
;       TIMEOUT:        out, optional, type=string/strarr
;                       Reformatted `TIME` based on `PATTERNOUT`. Required if `PATTERNOUT`
;                           is present.
;
; :Keywords:
;       BOOLEAN:            in, optional, type=boolean, default=0
;                           If set, `TIME` will be compared to `PATTERNIN`. If the
;                               the pattern matches the time, 1 is returned. 0 is returned
;                               otherwise.
;       BREAKDOWN:          in, optional, type=boolean
;                           If set, `TIME` will be broken down into its components.
;                               Automatically set if `TIME` is given.
;       COMPUTE:            in, optional, type=boolean
;                           If set, `TIME` will be built from components given.
;                               Automatically set if `TIME` is not give.
;       ISMATCH:            out, optional, type=boolean
;                           Returns 1 if `TIME` matches `PATTERN` and 0 otherwise. If
;                               ISMATCH=0, `TIME` will not be broken down and all
;                               keywords will be empty. If, in addition, no variable is
;                               present, an error will be issued.
;       YEAR:               in, out, optional, type=strarr
;                           4-digit year that matches a %Y token.
;       YR:                 in, out, optional, type=strarr
;                           2-digit year that matches a %y token.
;       DOY:                in, out, optional, type=strarr
;                           3-digit day-of-year that matches a %D.
;       MONTH:              in, out, optional, type=strarr
;                           2-digit month that matches a %M token.
;       CMONTH:             in, out, optional, type=strarr
;                           Calendar month name (e.g. January, February, etc.) that
;                               matches a %C tokenl.
;       CALMO:              in, out, optional, type=strarr
;                           3-character abbreviated calendar month name (e.g. Jan, Feb, ...)
;                               that matches a %c token.
;       WEEKDAY:            in, out, optional, type=strarr
;                           Weekday (e.g. Monday, Tuesday, etc.) that matches a %W token
;                               for the [start, end] of the file interval.
;       WKDAY:              in, out, optional, type=strarr
;                           3-character abbreviated week day (e.g. Mon, Tue, etc.) that
;                               matches a %M token.
;       DAY:                in, out, optional, type=strarr
;                           2-digit day that matches a %d token.
;       HOUR:               in, out, optional, type=strarr
;                           2-digit hour on a 24-hour clock that matches a %H token.
;       HR:                 in, out, optional, type=strarr
;                           2-digit hour on a 12-hour clock that matches a %h token.
;       MINUTE:             in, out, optional, type=strarr
;                           2-digit minute that matches a %m token.
;       SECOND:             in, out, optional, type=strarr
;                           2-digit second that matches a %S token.
;       DECIMAL:            in, optional, type=strarr
;                           Fraction of a second that matches the %f token.
;       MILLI:              in, out, optional, type=strarr
;                           3-digit milli-second that matches a %1 token.
;       MICRO:              in, out, optional, type=strarr
;                           3-digit micro-second that matches a %2 token.
;       NANO:               in, out, optional, type=strarr
;                           3-digit nano-second that matches a %3 token.
;       PICO:               in, out, optional, type=strarr
;                           3-digit pico-second that matches a %4 token.
;       AM_PM:              in, out, optional, type=strarr
;                           "AM" or "PM" string that matches a %A.
;       OFFSET:             in, out, optional, type=string
;                           Offset from UTC. (+|-)hh[:][mm]. Matches %o.
;       TIME_ZONE:          in, out, optional, type=string
;                           Time zone abbreviated name. Matches %z.
;-
pro MrTimeParser, time, patternIn, patternOut, timeOut, $
ISMATCH=isMatch, $
BREAKDOWN=breakdown, $
COMPUTE=compute, $
YEAR=year, $
YR=yr, $
DOY=doy, $
MONTH=month, $
CMONTH=cMonth, $
CALMO=calmo, $
WEEKDAY=weekday, $
WKDAY=wkday, $
DAY=day, $
HOUR=hour, $
HR=hr, $
MINUTE=minute, $
SECOND=second, $
DECIMAL=decimal, $
MILLI=milli, $
MICRO=micro, $
NANO=nano, $
PICO=pico, $
AM_PM=am_pm, $
OFFSET=offset, $
TIME_ZONE=time_zone, $

UTC=utc, $
TZ_OUT=tz_out
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Default to converting to an ISO time string
    _patternOut = n_elements(patternOut) eq 0 ? '' : patternOut
    compute     = keyword_set(compute)
    breakdown   = keyword_set(breakdown)
    tz_out      = keyword_set(utc)
    if keyword_set(utc) $
        then tz_out = 'Z' $
        else tz_out = n_elements(tz_out) eq 0 ? '' : tz_out
    
    ;If a pattern was given, set BREAKDOWN and COMPUTE
    ;If no pattern was given, only one of BREAKDOWN or COMPUTE may be set.
    ;   - Determine which one automatically
    ;   - If both were set, it was probably an accident. Otherwise TIME will not change.
    if _patternOut ne '' then begin
        compute   = 1
        breakdown = 1
    endif else if (breakdown + compute) eq 0 then begin
        if n_elements(time) eq 0 then compute = 1 else breakdown = 1
    endif else if (breakdown + compute) eq 2 then begin
        message, 'BREAKDOWN and COMPUTE are mutually exclusive.'
    endif
    
    ;Was one of the pre-defined patterns given?
    if MrIsA(patternIn, /INTEGER) $
        then inPattern = MrTimeParser_Patterns(patternIn) $
        else inPattern = patternIn
        
    if MrIsA(patternOut, /INTEGER) $
        then outPattern = MrTimeParser_Patterns(_patternOut) $
        else outPattern = _patternOut
        
    
;-----------------------------------------------------
; Breakdown \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if breakdown then begin
        MrTimeParser_Breakdown, time, inPattern, isMatch, $
                                YEAR=year, YR=yr, DOY=doy, MONTH=month, CMONTH=cMonth, $
                                CALMO=calmo, WEEKDAY=weekday, WKDAY=wkday, DAY=day, $
                                HOUR=hour, HR=hr, MINUTE=minute, SECOND=second, $
                                DECIMAL=decimal, MILLI=milli, MICRO=micro, NANO=nano, $
                                PICO=pico, AM_PM=am_pm, OFFSET=offset, TIME_ZONE=time_zone

        ;Check for matches.
        if isMatch eq 0 then $
            if arg_present(isMatch) eq 0 then message, 'Could not match pattern "' + inPattern + '" to time "' + time[0] + '".'
    endif

;-----------------------------------------------------
; Compute \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if compute then begin
        MrTimeParser_Compute, timeOut, outPattern, $
                              YEAR=year, YR=yr, DOY=doy, MONTH=month, CMONTH=cMonth, $
                              CALMO=calmo, WEEKDAY=weekday, WKDAY=wkday, DAY=day, $
                              HOUR=hour, HR=hr, MINUTE=minute, SECOND=second, $
                              DECIMAL=decimal, MILLI=milli, MICRO=micro, NANO=nano, $
                              PICO=pico, AM_PM=am_pm, OFFSET=offset, TIME_ZONE=time_zone
        
        ;Make sure we return values properly
        if outPattern eq '' then begin
            time = temporary(timeOut)
        endif else begin
            if n_params() ne 4 then message, 'Incorrect number of parameters.'
        endelse
    endif
end



;---------------------------------------------------
; Main Level Example Program (.r MrTimeParser) /////
;---------------------------------------------------
;EXAMPLE 1
;   Create a fake time with all of the time codes in it. Break down the time to
;   check if all components are parsed correctly.
timeIn = '2014-14-03-March-Mar-078-19-Saturday-SatT23:11:26:31.123456789000PM'
patternIn  = '%Y-%y-%M-%C-%c-%D-%d-%W-%wT%H:%h:%m:%S.%1%2%3%4%A'
MrTimeParser, timeIn, patternIn, /BREAKDOWN, $
              YEAR=year, YR=yr, MONTH=month, CMONTH=cmonth, CALMO=calmo, DOY=doy, $
              DAY=day, WEEKDAY=weekday, WKDAY=wkday, $
              HOUR=hour, HR=hr, MINUTE=minute, SECOND=second, $
              MILLI=milli, MICRO=micro, NANO=nano, PICO=pico, AM_PM=am_pm

;Display the results.
print, '--------------------------------------------'
print, 'Test Time:    ', timeIn
print, 'Test Pattern: ', patternIn
print, 'Parsed:'
print, year,    FORMAT='(%"   Year         = %i")'
print, yr,      FORMAT='(%"   2-Digit Year = %i")'
print, month,   FORMAT='(%"   Month        = %i")'
print, cmonth,  FORMAT='(%"   Month Name   = %s")'
print, calmo,   FORMAT='(%"   Abbr. Month  = %s")'
print, doy,     FORMAT='(%"   Day-of-Year  = %i")'
print, day,     FORMAT='(%"   Day          = %i")'
print, weekday, FORMAT='(%"   Weekday      = %s")'
print, wkday,   FORMAT='(%"   Wkday        = %s")'
print, hour,    FORMAT='(%"   24-Hour      = %i")'
print, hr,      FORMAT='(%"   12-Hour      = %i")'
print, minute,  FORMAT='(%"   Minute       = %i")'
print, second,  FORMAT='(%"   Second       = %i")'
print, milli,   FORMAT='(%"   Milliseconds = %i")'
print, micro,   FORMAT='(%"   Microseconds = %i")'
print, nano,    FORMAT='(%"   Nanoseconds  = %i")'
print, pico,    FORMAT='(%"   Picoseconds  = %i")'
print, am_pm,   FORMAT='(%"   AM/PM        = %s")'
print, ''
      
;EXAMPLE 2
;   Take a time string and convert it to all of the different pre-defined formats.
timeIn    = '19-March-2014 23:26:31.12'
patternIn = '%d-%C-%Y %H:%m:%S.%1'
timeOut   = strarr(32)
for i = 0, 31 do begin
    MrTimeParser, timeIn, patternIn, i+1, tOut
    timeOut[i] = tOut
endfor

print, '---------------------------------------'
print, 'Starting Time String:'
print, '    ' + timeIn
print, 'Results:'
print, '    ' + transpose(timeOut)
print, ''
      
      
;EXAMPLE 3
;   Convert an array of times
timeIn    = ['2001-12-03T10:55:00', '2001-12-03T11:00:00']
patternIn = '%Y-%M-%dT%H:%m:%S'
MrTimeParser, timeIn, patternIn, 2, timeOut

print, '---------------------------------------'
print, 'Starting Time String:'
print, timeIn, FORMAT='(%"   [%s, %s]")'
print, 'Results:'
print, timeOut, FORMAT='(%"   [%s, %s]")'
print, ''
      
      
;EXAMPLE 4
;   Example using the %f token
timeIn     = '2001-12-03T10:55:00.123456'
patternIn  = '%Y-%M-%dT%H:%m:%S%f'
patternOut = '%D-%Y %hh %mm %S.%1%2%3s %A'
MrTimeParser, timeIn, patternIn, patternOut, timeOut

print, '---------------------------------------'
print, 'Starting Time String:'
print, timeIn, FORMAT='(%"   %s")'
print, 'Results:'
print, timeOut, FORMAT='(%"   %s")'
print, ''
      
      
;EXAMPLE 4
;   Example using the ISMATCH keyword.
timeIn     = '2001-12-03T10:55:00.123456'
patternIn  = '%Y-%D %Hh %mm %Ss'
MrTimeParser, timeIn, patternIn, ISMATCH=isMatch

print, '---------------------------------------'
print, 'Starting Time String:    ' + timeIn
print, 'Pattern to match:        ' + patternIn
print, 'Successful Match?        ' + (isMatch ? 'Yes' : 'No')
      
;EXAMPLE 4
;   Example using %z and %o.
timeIn     = '2001-12-03 10:55:00 GMT'
patternIn  = '%Y-%M-%d %H:%m:%S %z'
patternOut = '%Y-%D %Hh %mm %Ss %o'
MrTimeParser, timeIn, patternIn, patternOut, timeOut

print, '---------------------------------------'
print, 'Starting Time String:'
print, FORMAT='(%"   %s")', timeIn
print, 'Results:'
print, FORMAT='(%"   %s")', timeOut
print, ''


;EXAMPLE 5
;   - Ignore the contents of %( and %)
filenames = ['rbspa_def_MagEphem_TS04D_20140331_v1.1.1.h5', $
             'rbspb_def_MagEphem_TS04D_20140331_v1.1.1.h5']
pattern    = 'rbsp%(%Y(a|b)%o%)_def_MagEphem_TS04D_%Y%M%d_v*.h5'
patternOut = 'rbsp%(%y(a|b)%z%)_def_MagEphem_TS04D_%Y-%D_v*.h5'
MrTimeParser, filenames, pattern, YEAR=year, MONTH=month, DAY=day, /BREAKDOWN
MrTimeParser, filenames, pattern, patternOut, fileOut

;Print results
print, '---------------------------------------'
print, 'Pattern: ' + pattern
print, 'Filenames:'
print, '  ' + filenames[0]
print, '  ' + filenames[1]
print, 'Time Components:'
print, FORMAT='(%"  Year:  [%s, %s]")', year
print, FORMAT='(%"  Month: [  %s,   %s]")', month
print, FORMAT='(%"  Day:   [  %s,   %s]")', day
print, 'Out Pattern: ' + patternOut
print, 'Output:'
print, '  ' + fileOut[0]
print, '  ' + fileOut[1]


end


