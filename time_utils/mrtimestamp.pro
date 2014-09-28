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
; :Examples:
;    Try the main level program at the end of this document.
;       IDL> .r MrTimeStamp
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
;       OFFSET:         in, optional, type=float/string
;                       Offset in decimal hours from Coordinated Universal Time (UTC).
;                           Values range from -12 to 14. Strings in the format "(+|-)hh[:mm]"
;                           are also accepted.
;       SECOND:         in, optional, type=integer/float/double, default=0
;                       Number of seconds (0-59).
;       TIME_ZONE:      in, optional, type=string
;                       The abbreviated time zone name. If present, `OFFSET` is ignored.
;       TZ_OUT:         in, optional, type=string/float
;                       A string of the form "(+|-)hh[:mm]" representing the offset from
;                           UTC or an abbreviated time zone name of the output sting.
;       UTC:            in, optional, type=boolean, default=0
;                       If set, times are considered to be in UTC already. If `OFFSET` is
;                           also provided, then times will be converted to UTC using the
;                           offset. If not set, but `OFFSET` is provided, `RESULT` will
;                           be returned with the offset appended. If `OFFSET`=0 or is not
;                           given, then UTC=1 is assumed. Setting UTC=1 automatically
;                           sets `TZ_OUT`="Z"
;       YEAR:           in, optional, type=integer, default=current year
;                       The years.
;       ZERO:           in, optional, type=integer
;                       `RESULT` will be initialized to '000-00-00T00:00:00Z'. All other
;                           keywords will be ignored. 
;
; :Returns:
;       RESULT:         ISO-1806 times representing the input parameters.
;
; :Uses:
;       MrTimeZoneNameToOffset.pro
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
;       2014-05-10  -   When no parameters are given, append the offset to UTC. Added
;                           the TIME_ZONE keyword. - MRA
;       2014-05-12  -   Convert between time zones using TZ_OUT. Offset can be a string
;                           of the form "(+|-)hh[:mm]". - MRA
;       2014-06-06  -   The systime function uses a 1-digit date for dates between 1 and 9.
;                           This is handled now.
;-
function MrTimeStamp, $
DAY=day, $
HOUR=hour, $
MINUTE=minute, $
MONTH=month, $
OFFSET=offset, $
SECOND=second, $
UTC=utc, $
TIME_ZONE=time_zone, $
TZ_OUT=tz_out, $
YEAR=year, $
ZERO=zero
    compile_opt strictarr
    on_error, 2
    
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    utc  = keyword_set(utc)
    zero = keyword_set(zero)
    
    ;Are we converting to a particular time zone?
    ;   All TZ_OUT elements are assumed to be formatted the same. Check the first element
    if MrIsA(tz_out, 'STRING') then begin
        isOffset = stregex(tz_out[0], '^(\+|-)?[0-9]', /BOOLEAN)
        tz_offset = isOffset ? tz_out : MrTimeZoneNameToOffset(tz_out)
        
        ;Is the offset given in hh:mm format?
        ;   Convert to decimal hours
        ;   Use 4 decimal places because 1/60.0 = 0.016666
        if strpos(tz_offset[0], ':') ne -1 then begin
            str = stregex(tz_offset, '^(\+|-)?([0-9]{2}):([0-9]{2})', /SUBEXP, /EXTRACT)
            hh = str[2]
            mm = str[3]
            tz_offset = string(float(hh) + float(mm)/60.0, FORMAT='(f+08.4)')
        endif
    endif
    if keyword_set(utc) then tz_offset = 0.0

    ;Convert time zone to offset?
    ;   If TIME_ZONE was given, ignore OFFSET
    if n_elements(offset)    gt 0 then _offset = offset
    if n_elements(time_zone) gt 0 then _offset = MrTimeZoneNameToOffset(time_zone)
    
    ;Is the offset given in 'hh:mm' format?
    ;   All OFFSET elements are assumed to be formatted the same.
    if MrIsA(_offset, 'STRING') then begin
        if strpos(_offset, ':') ne -1 then begin
            str = stregex(_offset, '^(\+|-)?([0-9]{2}):([0-9]{2})', /SUBEXP, /EXTRACT)
            hh = str[2]
            mm = str[3]
            _offset = string(float(hh) + float(mm)/60.0, FORMAT='f+08.4')
        endif
    endif

    nDay     = n_elements(day)
    nHour    = n_elements(hour)
    nMinute  = n_elements(minute)
    nMonth   = n_elements(month)
    nOffset  = n_elements(_offset)
    nSecond  = n_elements(second)
    nYear    = n_elements(year)
    elements = [nDay, nHour, nMinute, nMonth, nOffset, nSecond, nYear]
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
; Current Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if nYear + nDay + nMonth + nHour + nMinute + nSecond eq 0 then begin
        ;Compute the system time in case we need it
        t_utc = systime(/UTC)
        t_sys = systime(/JULIAN)
        caldat, t_sys, current_month, current_day, current_year, $
                       current_hour, current_minute, current_second
    
        ;Get the offset from UTC
        ;   Extract the hours and minutes from the UTC system time.
        ;   Compute the partial hour of the current time in both local and UTC times.
        ;   Compute the offset from UTC (one minute is 0.01666 hours).
        regex     = '([A-Za-z]{3}) ([A-Za-z]{3}) ( [0-9]|[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}) ([0-9]{4})'
        utc_parts = stregex(t_utc, regex, /SUBEXP, /EXTRACT)
        
        ;Make sure the UTC day-of-month is two digits
        ;   Fri Jun  6 15:54:29      becomes        Fri Jun 06 15:54:29
        utc_parts[3] = string(utc_parts[3], FORMAT='(i02)')
        t_utc        = strjoin(utc_parts[1:*], ' ' )

        ;Get the necessary pieces to calculate the local time offset.
        MrTimeParser, t_utc, '%w %c %d %H:%m:%S %Y', '%Y-%M-%dT%H:%m:%SZ', t_utc_out
        MrTimeStampToValues, t_utc_out, TYPE='STRING', $
                             YEAR=utc_year, MONTH=utc_month, DAY=utc_day, $
                             HOUR=utc_hour, MINUTE=utc_minute, SECOND=utc_second
        
        ;Days between times, in case the offset crosses midnight
        utc_date = utc_year + '-' + utc_month + '-' + utc_day
        sys_date = string(FORMAT='(%"%04i-%02i-%02i")', current_year, current_month, current_day)
        nDays    = days_between(sys_date, utc_date)
        
        ;Calculate the offset
        ;   Round to the nearest hundredth of an hour
        utc_hrs   = fix(utc_hour) + fix(utc_minute)/60.0D
        sys_hrs   = current_hour + current_minute/60.0
        current_offset = round( ((sys_hrs - utc_hrs) - 24.0D*nDays) * 100 ) / 100.0

        ;Create (+|-)hh:mm format
        offset_minute = fix((current_offset mod 1) * 100)
        if current_offset lt 0 $
            then offset_hour = string(fix(current_offset), FORMAT='(i+03)') $
            else offset_hour = string(fix(current_offset), FORMAT='(i+02)')
        
        ;Set the time values
        year    = current_year
        day     = current_day
        month   = current_month
        hour    = current_hour
        minute  = current_minute
        second  = fix(current_second)
        _offset = offset_hour + ':' + offset_minute
        nMax    = 1
    
;-----------------------------------------------------
; Date \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if nHour + nMinute + nSecond eq 0 then begin
        result = string(FORMAT='(i4, 2(a1, i02))', year, '-', month, '-', day)
        if nMax eq 1 then result = result[0]
        return, result
    
;-----------------------------------------------------
; Default to 0 Hours \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        if nHour   eq 0 then hour   = 0
        if nMinute eq 0 then minute = 0
        if nSecond eq 0 then second = 0
    endelse
    
    ;Output arrays
    strYear   = strarr(nMax)
    strMonth  = strarr(nMax)
    strDay    = strarr(nMax)
    strHour   = strarr(nMax)
    strMinute = strarr(nMax)
    strSecond = strarr(nMax)
    newSecond = make_array(nMax, TYPE=size(second, /TYPE))

;-----------------------------------------------------
;Convert to TZ_OUT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;If an offset was given and we are to convert to UTC, then we have to check if we
    ;crossed the dateline. If we did, add or subtract a day.
    if n_elements(_offset) gt 0 && n_elements(tz_offset) gt 0 then begin
        if array_equal(-12 > _offset   < 14, _offset)   eq 0 then message, 'OFFSET must be between -12 and 14'
        if array_equal(-12 > tz_offset < 14, tz_offset) eq 0 then message, 'TZ_OUT must be between -12 and 14'
        theOffset = float(_offset) - float(tz_offset)
        decHr = (hour + minute/60.0 + second/3600.0) - theOffset

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
        strHour   = string((-24*dateLine) + hour - fix(theOffset), FORMAT='(i02)')
        
        ;Subtract the offset from the number of minutes.
        strMinute = string(minute - fix((theOffset mod 1)*100), FORMAT='(i02)')
        newSecond = 60 - second

        ;Offset to append.
        ;   TZ_OUT was converted to decimal hours and renamed TZ_OFFSET
        if MrIsA(tz_out, 'STRING') $
            then strOffset = tz_out $
            else if tz_offset eq 0.0 $
                then strOffset = 'Z' $
                else strOffset = string(tz_offset, FORMAT='(f+06.2)')

        if stregex(strOffset, '^([A-Y])', /BOOLEAN) then strOffset = ' ' + strOffset

;-----------------------------------------------------
;Leave In Local Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        strYear   = string(year,   FORMAT='(i4.4)')
        strMonth  = string(month,  FORMAT='(i02)')
        strDay    = string(day,    FORMAT='(i02)')
        strHour   = string(hour,   FORMAT='(i02)')
        strMinute = string(minute, FORMAT='(i02)')
        newSecond = second

        ;Get the offset
        if n_elements(_offset) ne 0 then begin
            ;Is it already a string?
            if size(_offset, /TNAME) eq 'STRING' then begin
                if stregex(_offset, '[.:]', /BOOLEAN) eq 0 $
                    then strOffset = _offset + ':00' $
                    else strOffset = _offset
                
            ;Convert to string
            endif else begin
            
                ;Hour offset
                ;   Negative and positive numbers required different format strings
                strHrOffset = strarr(n_elements(_offset))
                iNeg = where(_offset le 0, nNeg, COMPLEMENT=iPos, NCOMPLEMENT=nPos)
                if nNeg gt 0 then strHrOffset[iNeg] = string(fix(_offset[iNeg]), FORMAT='(i+03)')
                if nPos gt 0 then strHrOffset[iPos] = string(fix(_offset[iPos]), FORMAT='(i+02)')
                
                ;Minute offset
                min_offset = fix((_offset mod 1) * 60)
                strMinOffset = string(min_offset, FORMAT='(i02)')
                
                ;Total offset
                strOffset    = strHrOffset + ':' + StrMinOffset
            endelse

        ;Assume in UTC? Append a 'Z'
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
        MrIsA(second, 'DOUBLE'): begin
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

    ;Return a scalar?
    if nMax eq 1 then result = result[0]
	return, result
end




;-----------------------------------------------------
;Main Level Example Program: IDL> .r MrCmpVersion) \\\
;-----------------------------------------------------
;EXAMPLE 1: Scalar time
;   If UTC is not set (i.e. UTC=0) and OFFSET is not given, UTC is assumed. A "Z"
;   is appended to the end of the time string.
year   = 2012
month  =    9
day    =    4
hour   =   11
minute =   25
second =   15
utc    =    0
timeStr1 = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, UTC=utc)
print, '----------------------------------------'
print, 'UTC assumed (Z appended) -- no offset provided.'
print, FORMAT='(%"Year    = %i")', year
print, FORMAT='(%"Month   = %i")', month
print, FORMAT='(%"Day     = %i")', day
print, FORMAT='(%"Hour    = %i")', hour
print, FORMAT='(%"Minute  = %i")', minute
print, FORMAT='(%"Second  = %i")', second
print, FORMAT='(%"UTC     = %i")', utc
print, FORMAT='(%"RESULTS = %s")', timeStr1
print, ''


;EXAMPLE 2: Scalar time, no offset
;   If UTC is set (i.e. UTC=1), but OFFSET is not given, a UTC time is created (i.e.
;   with a "Z" appended to the end of the time string).
year   = 2012
month  =    9
day    =    4
hour   =   11
minute =   25
second =   15
utc    =    1
timeStr1 = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, UTC=utc)
print, '----------------------------------------'
print, 'UTC requested, no offset given -- time left as-is, "Z" appended.'
print, FORMAT='(%"Year    = %i")', year
print, FORMAT='(%"Month   = %i")', month
print, FORMAT='(%"Day     = %i")', day
print, FORMAT='(%"Hour    = %i")', hour
print, FORMAT='(%"Minute  = %i")', minute
print, FORMAT='(%"Second  = %i")', second
print, FORMAT='(%"UTC     = %i")', utc
print, FORMAT='(%"RESULTS = %s")', timeStr1
print, ''


;EXAMPLE 3: Scalar time, offset
;   If UTC=0 and an offset is given, the offset is appended to the
;   end of the time string.
year   = 2012
month  =    9
day    =    4
hour   =   11
minute =   25
second =   15
offset =   -3
utc    =    0
timeStr1 = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, OFFSET=offset, UTC=utc)
print, '----------------------------------------'
print, 'Local time with offset from UTC appended -- offset provided.'
print, FORMAT='(%"Year    = %i")', year
print, FORMAT='(%"Month   = %i")', month
print, FORMAT='(%"Day     = %i")', day
print, FORMAT='(%"Hour    = %i")', hour
print, FORMAT='(%"Minute  = %i")', minute
print, FORMAT='(%"Second  = %i")', second
print, FORMAT='(%"Offset  = %i")', offset
print, FORMAT='(%"UTC     = %i")', utc
print, FORMAT='(%"RESULTS = %s")', timeStr1
print, ''


;EXAMPLE 4: Scalar time, no offset
;   If UTC=1 and an offset is given, time is converted to UTC.
year   = 2012
month  =    9
day    =    4
hour   =   11
minute =   25
second =   15
offset =   -3
utc    =    1
timeStr1 = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, OFFSET=offset, UTC=utc)
print, '----------------------------------------'
print, 'Converted to UTC -- /UTC, offset provided.'
print, FORMAT='(%"Year    = %i")', year
print, FORMAT='(%"Month   = %i")', month
print, FORMAT='(%"Day     = %i")', day
print, FORMAT='(%"Hour    = %i")', hour
print, FORMAT='(%"Minute  = %i")', minute
print, FORMAT='(%"Second  = %i")', second
print, FORMAT='(%"Offset  = %i")', offset
print, FORMAT='(%"UTC     = %i")', utc
print, FORMAT='(%"RESULTS = %s")', timeStr1
print, ''


;EXAMPLE 5: Array of times with different offsets.
;   Mix of scalars and vectors
;   UTC=0, so the offset will be appended to the end of the string.
year   = 2012
month  = 9
day    = [ 4,  5,  6]
hour   = [11, 12, 13]
offset = [-4, -3, -2]
utc    = 0
timeStr3 = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, OFFSET=offset, UTC=utc)
print, '----------------------------------------'
print, 'Local Time + Offset --> UTC=0. Mix of scalars and vectors.'
print, FORMAT='(%"Year    = %4i")', year
print, FORMAT='(%"Month   = %4i")', month
print, FORMAT='(%"Day     = [%4i, %4i, %4i]")', day
print, FORMAT='(%"Hour    = [%4i, %4i, %4i]")', hour
print, FORMAT='(%"Offset  = [%4i, %4i, %4i]")', offset
print, FORMAT='(%"UTC     = %i")', utc
print, FORMAT='(%"RESULTS = [%s, %s, %s]")', timeStr3
print, ''


;EXAMPLE 6: Array of times with different offsets.
;   Mix of scalars and vectors
;   UTC=1, so times will be converted to UTC.
year   = 2012
month  = 9
day    = [ 4,  5,  6]
hour   = [11, 12, 13]
offset = [-4, -3, -2]
utc    = 1
timeStr3 = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, OFFSET=offset, UTC=utc)
print, '----------------------------------------'
print, 'Local Time + Offset --> Convert to UTC. Mix of scalars and vectors.'
print, FORMAT='(%"Year    = %4i")', year
print, FORMAT='(%"Month   = %4i")', month
print, FORMAT='(%"Day     = [%4i, %4i, %4i]")', day
print, FORMAT='(%"Hour    = [%4i, %4i, %4i]")', hour
print, FORMAT='(%"Offset  = [%4i, %4i, %4i]")', offset
print, FORMAT='(%"UTC     = %i")', utc
print, FORMAT='(%"RESULTS = [%s, %s, %s]")', timeStr3
print, ''


;EXAMPLE 7
;   Use the TIME_ZONE keyword.
year      = 2012
month     =    9
day       =    4
hour      =   11
minute    =   25
second    =   15
time_zone = 'EST'
utc       =    0
timeStr1 = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, TIME_ZONE=time_zone, UTC=utc)
print, '----------------------------------------'
print, 'Provide a Time Zone -- Time Zone converted to Offset (UTC=0).'
print, FORMAT='(%"Year      = %i")', year
print, FORMAT='(%"Month     = %i")', month
print, FORMAT='(%"Day       = %i")', day
print, FORMAT='(%"Hour      = %i")', hour
print, FORMAT='(%"Minute    = %i")', minute
print, FORMAT='(%"Second    = %i")', second
print, FORMAT='(%"Time Zone = %s")', time_zone
print, FORMAT='(%"UTC       = %i")', utc
print, FORMAT='(%"RESULTS   = %s")', timeStr1
print, ''


;EXAMPLE 8:
;   Print the current time with an offset and in UTC
print, '----------------------------------------'
print, FORMAT='(%"The currrent local time is          %s")', MrTimeStamp()
print, FORMAT='(%"The currrent local time in UTC is   %s")', MrTimeStamp(/UTC)
print, ''


;EXAMPLE 9:
;   Convert the current time offset to another
tz_out = string(indgen(25)-12, FORMAT='(i+03)') + ':00'
nTimes = n_elements(tz_out)
tOut   = strarr(nTimes)
for i = 0, nTimes - 1 do tOut[i] = MrTimeStamp(TZ_OUT=tz_out[i])
print, '-------------------------------------------'
print, 'Convert between offsets.'
print, FORMAT='(%"   %s,   %s")', 'TZ', 'Time'
for i = 0, nTimes - 1 do $
    print, FORMAT='(2x, a6, 2x, a0)', tz_out[i], tOut[i]
print, ''


;EXAMPLE 10:
;   Convert from one time zone to another
time = '2012-09-04T11:25:15 EST'
year   = 2012
month  =    9
day    =    4
hour   =   11
minute =   25
second =   15
time_zone = 'EST'
tz_out    = 'PST'
tOut = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, $
                   HOUR=hour, MINUTE=minute, SECOND=second, $
                   TIME_ZONE=time_zone, TZ_OUT=tz_out)
print, '----------------------------------------'
print, FORMAT='(%"Eastern Standard Time:   %s")', time
print, FORMAT='(%"Pacific Standard Time:   %s")', tOut
print, ''


;EXAMPLE 11:
;   Convert current time to another time zone
local = MrTimeStamp()
t_pst = MrTimeStamp(TZ_OUT='PST')
print, '----------------------------------------'
print, FORMAT='(%"Current Local Time:      %s")', local
print, FORMAT='(%"Pacific Standard Time:   %s")', t_pst
print, ''

end
