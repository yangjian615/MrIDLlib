; docformat = 'rst'
;
; NAME:
;       MRTIMESTAMP
;
; PURPOSE:
;+
;       IDL 8.2 has a function called TIMESTAMPTOVALUE breaks down ISO-8601 values into
;       its components. I do not have IDL 8.2, so try to recreate it here. It is the
;       inverse of MRTIMESTAMP.
;
;   Accepted formats::
;       2014-11-05T10:22:38Z            ;UTC
;       2014-11-05T10:22:38 EST         ;Time Zone 
;       2014-11-05T10:22:38-04:00       ;Offset
;       2014-11-05T10:22:38             ;UTC Assumed
;
; :Categories:
;   Time Utility, Time Conversion
;
; :Examples:
;   Try the example program at the end of this document::
;       IDL> MrTimeStampToValues
;
; :Params:
;       TIMESTAMP:      in, required, type=iso-8601 string
;                       The ISO-8601 string to be decomposed.
;
; :Keywords:
;       DAY:            out, optional, type=integer, default=current day
;                       2-digit day of month.
;       HOUR:           out, optional, type=integer, default=0
;                       2-digit hour of day (0-23)
;       MINUTE:         out, optional, type=integer, default=0
;                       2-digit minute (0-59)
;       MONTH:          out, optional, type=integer, default=current month
;                       2-digit month (1-12)
;       OFFSET:         out, optional, type=float 
;                       Offset in hours from Coordinated Universal Time (UTC).
;                           Values range from -12 to 14.
;       SECOND:         out, optional, type=integer/float/double, default=0
;                       Number of seconds (0-59)
;       TIME_ZONE:      out, optional, type=string
;                       Abbreviated time zone name.
;       TYPE:           in, optional, type=integer, default=2
;                       The type-code or type-name indicating which data type the
;                           should be returned as. The default is 2 to return the same
;                           type as was given initially.
;       UTC:            out, optional, type=boolean, default=1
;                       Returns 1 if the time given was UTC (i.e. it contains "Z") and
;                           0 otherwise.
;       YEAR:           out, optional, type=integer, default=current year
;                       The years.
;
; :Returns:
;       RESULT:         ISO-1806 times representing the input parameters.
;
; :Uses:
;   Uses the following external programs::
;       TypeCode2Name.pro
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
;       2014/03/08  -   TYPE can now be a type-name. - MRA
;       2014/05/05  -   Added the TIME_ZONE keyword and an example program. - MRA
;       2014/05/29  -   Typo when changing types was causing conversion error. Fixed. - MRA
;-
pro MrTimeStampToValues, timeStamp, $
DAY=day, $
HOUR=hour, $
MINUTE=minute, $
MONTH=month, $
OFFSET=offset, $
SECOND=second, $
TIME_ZONE=time_zone, $
TYPE=type, $
UTC=utc, $
YEAR=year
    compile_opt strictarr
    on_error, 2
    
    ;Convert to integers by default.
    if n_elements(type) eq 0 then type = 'INT'
    typecode = (size(type, /TNAME) eq 'STRING') ? typenametocode(type) : type
    
    ;Extract the pieces
    parts = stregex(timeStamp, '([0-9]{4})-' + $                        ;Year
                               '([0-9]{2})-' + $                        ;Month
                               '([0-9]{2})T?' + $                       ;Day
                               '([0-9]{2})?:?' + $                      ;Hour
                               '([0-9]{2})?:?' + $                      ;Minute
                               '([0-9]{2})?' + $                        ;Second
                               '([.0-9]*)?' + $                         ;Decimal Second
                               '(Z)?' + $                               ;UTC?
                               '(-[0-9]{2}:[0-9]{2})?' + $              ;Offset
                               '( ?([A-Z][A-Z])?[A-Z0-9]?[A-Z0-9]?)', $ ;Time Zone
                               /EXTRACT, /SUBEXP)

    ;Pick out the substrings
    year      = reform(parts[1,*])
    month     = reform(parts[2,*])
    day       = reform(parts[3,*])
    hour      = reform(parts[4,*])
    minute    = reform(parts[5,*])
    second    = reform(parts[6,*]) + reform(parts[7,*])
    utc       = reform(   (parts[8,*] eq 'Z') or $                          ;Is UTC
                        ( (parts[9,*] eq '')  and (parts[10,*] eq '') ) $   ;Assume UTC
                      )
    offset    = reform(parts[9,*])
    time_zone = reform(parts[10,*])
    
    ;Convert to numeric types.
    if typecode ne 7 then begin
        year   = fix(year,   TYPE=typecode)
        month  = fix(month,  TYPE=typecode)
        day    = fix(day,    TYPE=typecode)
        hour   = fix(hour,   TYPE=typecode)
        minute = fix(minute, TYPE=typecode)
        second = fix(second, TYPE=5)      ;Always a double
    endif
    
    ;Scalars?
    if n_elements(timeStamp) eq 1 then begin
        year      = year[0]
        month     = month[0]
        day       = day[0]
        hour      = hour[0]
        minute    = minute[0]
        second    = second[0]
        utc       = utc[0]
        offset    = offset[0]
        time_zone = time_zone[0]
    endif
end




;-----------------------------------------------------------
;Main Level Example Program: IDL> .r MrTimeStampToValues \\\
;-----------------------------------------------------------
;Create a set of strings and parse them.
timeStamps = ['2014-11-05T10:22:38Z', $
              '2014-11-05T10:22:38 EST', $
              '2014-11-05T10:22:38-04:00', $
              '2014-11-05T10:22:38']
MrTimeStampToValues, timeStamps, TYPE='STRING', $
                     YEAR=year, MONTH=month, DAY=day, $
                     HOUR=hour, MINUTE=minute, SECOND=second, $
                     UTC=utc, OFFSET=offset, TIME_ZONE=time_zone

;Print the results
print, '-------------------------------------------------------------'
print, 'Input:'
print, '  ' + transpose(timeStamps)

;Header
print, FORMAT='(%"%4s  %5s  %3s  %4s  %6s  %6s  %3s  %6s  %9s")', $
       'Year', 'Month', 'Day', 'Hour', 'Minute', 'Second', 'UTC', 'Offset', 'TimeZone'
       
;Results
for i = 0, n_elements(timeStamps) - 1 do begin
    print, FORMAT='(%"%4s    %2s    %2s   %2s     %2s      %2s     %1i   %6s   %4s")', $
           year[i], month[i], day[i], hour[i], minute[i], second[i], $
           utc[i], offset[i], time_zone[i]
endfor
print, ''

end
