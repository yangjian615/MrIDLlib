; docformat = 'rst'
;
; NAME:
;       MRTIMESTAMP
;
; PURPOSE:
;+
;       IDL 8.2 has a function called TIMESTAMP that converts times to ISO-8601 values.
;       I do not have IDL 8.2, so try to recreate it here.
;
; :Categories:
;   Time Utility, Time Conversion
;
; :Keywords:
;       DAY:            in, optional, type=integer, default=current day
;                       2-digit day of month.
;       HOUR:           in, optional, type=integer, default=0
;                       2-digit hour of day (0-23)
;       MINUTE:         in, optional, type=integer, default=0
;                       2-digit minute (0-59)
;       MONTH:          in, optional, type=integer, default=current month
;                       2-digit month (1-12)
;       OFFSET:         in, optional, type=float 
;                       Offset in decimal hours from Coordinated Universal Time (UTC).
;                           Values range from -12 to 14.
;       SECOND:         in, optional, type=integer/float/double, default=0
;                       Number of seconds (0-59)
;       UTC:            in, optional, type=boolean, default=0
;                       If set, times are considered to be in UTC already. If `OFFSET` is
;                           also provided, then times will be converted to UTC using the
;                           offset. If not set, but `OFFSET` is provided, `RESULT` will
;                           be returned with the offset appended. If `OFFSET`=0 or is not
;                           given, then UTC=1 is assumed.
;       YEAR:           in, optional, type=integer, default=current year
;                       The years.
;       ZERO:           in, optional, type=integer
;                       `RESULT` will be initialized to '000-00-00T00:00:00Z'. All other
;                           keywords will be ignored. 
;
; :Returns:
;       RESULT:         ISO-1806 times representing the input parameters.
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
;       2013-10-24  -   Written by Matthew Argall
;       2014-03-03  -   Default to UTC=0 to mimic IDL's default behavior. Added examples.
;                           ZERO keyword returns scalar string when no keywords present. - MRA
;-
function mrtimestamp, $
DAY=day, $
HOUR=hour, $
MINUTE=minute, $
MONTH=month, $
OFFSET=offset, $
SECOND=second, $
UTC=utc, $
YEAR=year, $
ZERO=zero
    compile_opt strictarr
    on_error, 2
    
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    utc  = keyword_set(utc)
    zero = keyword_set(zero)

    nDay     = n_elements(day)
    nHour    = n_elements(hour)
    nMinute  = n_elements(minute)
    nMonth   = n_elements(month)
    nSecond  = n_elements(second)
    nYear    = n_elements(year)
    elements = [nDay, nHour, nMinute, nMonth, nSecond, nYear]
    nMax     = max(elements)
    
    ;Make sure the correct number of elements were given for each input.
    if min((elements eq 0) or (elements eq 1) or (elements eq nMax)) eq 0 then $
        message, 'Inputs, if defined, must be scalars or arrays with the same number of elements.'
        
;-----------------------------------------------------
;ZERO \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;RETURN -- if ZERO is set, return 
    if (zero eq 1) then $
        if nMax le 1 $
            then return, '0000-00-00T00:00:00Z' $
            else return, replicate('0000-00-00T00:00:00Z', max(elements))
    
;-----------------------------------------------------
;Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Compute the system time in case we need it
    current_time = systime(/JULIAN)
    caldat, current_time, current_day, current_month, current_year, $
                          current_hour, current_minute, current_second
    
    if nYear  eq 0 then year  = current_year
    if nDay   eq 0 then day   = current_day
    if nMonth eq 0 then month = current_month
    
    ;RETURN -- If no time was given, return the dates, only.
    if nHour + nMinute + nSecond eq 0 then begin
        result = string(FORMAT='(i4, 5(a1, i02))', year, '-', month, '-', day, 'T', $
                        current_hour, ':', current_minute, ':', current_second)
        return, result
    endif
    
    if nHour   eq 0 then hour   = 0
    if nMinute eq 0 then minute = 0
    if nSecond eq 0 then second = 0
    
    ;Output arrays
    strYear   = strarr(nMax)
    strMonth  = strarr(nMax)
    strDay    = strarr(nMax)
    strHour   = strarr(nMax)
    strMinute = strarr(nMax)
    strSecond = strarr(nMax)
    newSecond = make_array(nMax, TYPE=size(second, /TYPE))

;-----------------------------------------------------
;Convert to UTC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;If an offset was given and we are to convert to UTC, then we have to check if we
    ;crossed the dateline. If we did, add or subtract a day.
    if (n_elements(offset) gt 0) and (utc eq 1) then begin
        if array_equal(-12 > offset < 14, offset) eq 0 then message, 'OFFSET must be between -12 and 14'
        decHr = (hour + minute/60.0 + second/3600.0) - offset
        
        ;Add or subtract a day?
        dateLine = -1*(decHr lt 0) + (decHr ge 24)
        iDiff = where(dateLine ne 0, nDiff, COMPLEMENT=iSame, NCOMPLEMENT=nSame)
        
        ;Convert to Julian date, add or subtract one, then convert back to gregorian
        jul = julday(month, day, year) + dateLine
        caldat, jul, newMonth, newDay, newYear

        ;Make strings out of the year, month and day
        strYear   = string(newYear,  FORMAT='(i04)')
        strMonth  = string(newMonth, FORMAT='(i02)')
        strDay    = string(newDay,   FORMAT='(i02)')
        
        ;Subtract the offset from the hour. If we moved into a different day, add or
        ;subtract 24 hours, accordingly.
        strHour   = string((-24*dateLine) + hour - fix(offset), FORMAT='(i02)')
        
        ;Subtract the offset from the number of minutes.
        strMinute = string(minute - fix((offset mod 1)*100), FORMAT='(i02)')
        newSecond = 60 - second
        
        strOffset = 'Z'
        
;-----------------------------------------------------
;Leave In Local Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        strYear = string(year, FORMAT='(i4.4)')
        strMonth = string(month, FORMAT='(i02)')
        strDay = string(day, FORMAT='(i02)')
        strHour = string(hour, FORMAT='(i02)')
        strMinute = string(minute, FORMAT='(i02)')
        newSecond = second

        ;Append the offset: e.g., "-01:30"
        if n_elements(offset) ne 0 then begin
            min_offset = fix((offset mod 1) * 60)
            strHrOffset = string(fix(offset), FORMAT='(i+03)')
            strMinOffset = string(min_offset, FORMAT='(i02)')
            strOffset = strHrOffset + ':' + StrMinOffset

        ;Already in UTC? Append a 'Z'
        endif else strOffset = 'Z'
    endelse
        
;-----------------------------------------------------
;Deal with Seconds \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    strSecond = string(fix(second), FORMAT='(i02)')

    ;Get the precision correct for the seconds.
    case 1 of
        MrIsA(second, 'FLOAT'): begin
            ;Turn SECOND into a string accurate to 1e-6.
            strDecimal = string(FORMAT='(f8.6)', second)
            
            ;Get rid of the leading full seconds, the decimal point, and trailing zeros
            subStr = stregex(strDecimal, '[.]([0-9]+[1-9])0*$', /SUBEXP, /EXTRACT)
            strDecimal = reform(subStr[1,*])
            
            ;Add a decimal point to seconds with non-zero decimal places.
            iDec = where(strDecimal ne '', nDec)
            if nDec gt 0 then strDecimal[iDec] = '.' + strDecimal[iDec]
        endcase
        
        ;Same as above, but accurate to 1e-12
        MrIsA(second, /DOUBLE): begin
            strDecimal = string(FORMAT='(f14.12)', second)
            subStr = stregex(strDecimal, '[.]([0-9]+[1-9])0*$', /SUBEXP, /EXTRACT)
            strDecimal = reform(subStr[1,*])
            iDec = where(strDecimal ne '', nDec)
            if nDec gt 0 then strDecimal[iDec] = '.' + strDecimal[iDec]
        endcase
        
        ;If an integer type was given, do not append a decimal point.
        else: strDecimal = ''
    endcase
        
;-----------------------------------------------------
;Concatenate Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    result = strYear + '-' + strMonth + '-' + strDay + 'T' + $
             strHour + ':' + strMinute + ':' + strSecond + strDecimal + strOffset

	return, result
end




;-----------------------------------------------------
;Main Level Example Program: IDL> .r MrCmpVersion) \\\
;-----------------------------------------------------
;These follow the exmples on the TimeStamp help page
;http://exelisvis.com/docs/timestamp.html


;Scalar time, no offset
timeStr1 = MrTimeStamp(YEAR=2012, MONTH=9, DAY=4, HOUR=11, MINUTE=25, SECOND=15)
print, timeStr1

;Array of times with an 8-hour offset
timeStr2 = MrTimeStamp(YEAR=[2011, 2012, 2012], MONTH=[9,9,6], DAY=1, $
                       HOUR=[11,12,1], MINUTE=[30,55,0], SECOND=[0.0,1.2345,2], OFFSET=8)
print, timeStr2

;Set UTC to 0 so that results are local time +/- offset from UTC
timeStr3 = MrTimeStamp(YEAR=2012, MONTH=9, DAY=[4,5,6], $
                       HOUR=[11,12,13], OFFSET=[-4,-3,-2], UTC=0)
print, timeStr3

;Similar to the previous example, but show results in UTC.
timeStr4 = MrTimeStamp(YEAR=2012, MONTH=9, DAY=[4,5,6], $
                       HOUR=[9,12,16], OFFSET=[-4,-3,-2], /UTC)
print, timeStr4

;This example uses an offset of 12 hours with the returned string in UTC.
;The result falls back to the previous day.
timeStr5 = MrTimeStamp(YEAR=2012, MONTH=9, DAY=4, $
                       HOUR=11, MINUTE=25, SECOND=15, OFFSET=12, /UTC)
PRINT, timeStr5

end
