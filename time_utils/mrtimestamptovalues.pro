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
; :Categories:
;   Time Utility, Time Conversion
;
; :Params:
;       TIMESTAMP:      in, required, type=iso-8601 string
;                       The iso-8601 string to be decomposed.
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
;       TYPE:           in, optional, type=integer, default=2
;                       The type-code or type-name indicating which data type the
;                           should be returned as. The default is 2 (integer) for
;                           everything except seconds, which is always returned as a
;                           double unless TYPE=7 (string)
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
;-
pro MrTimeStampToValues, timeStamp, $
DAY=day, $
HOUR=hour, $
MINUTE=minute, $
MONTH=month, $
OFFSET=offset, $
SECOND=second, $
TYPE=type, $
UTC=utc, $
YEAR=year
    compile_opt strictarr
    on_error, 2
    
    ;Convert to integers by default.
    if n_elements(type) eq 0 then type = 'INT'
    typecode = (size(type, /TNAME) eq 'STRING') ? typename2code(type) : type
    
    ;Extract the pieces
    parts = stregex(timeStamp, '([0-9]{4})-' + $            ;Year
                               '([0-9]{2})-' + $            ;Month
                               '([0-9]{2})T?' + $           ;Day
                               '([0-9]{2})?:?' + $          ;Hour
                               '([0-9]{2})?:?' + $          ;Minute
                               '([0-9]{2})?' + $            ;Second
                               '([.0-9]*)?' + $             ;Decimal Second
                               '(Z)?' + $                   ;UTC?
                               '(-[0-9]{2}:[0-9]{2})?', $   ;Offset
                               /EXTRACT, /SUBEXP)
    
    ;Pick out the substrings
    year   = reform(parts[1,*])
    month  = reform(parts[2,*])
    day    = reform(parts[3,*])
    hour   = reform(parts[4,*])
    minute = reform(parts[5,*])
    second = reform(parts[6,*]) + reform(parts[7,*])
    utc    = reform(parts[8,*] ne '')
    offset = reform(parts[9,*])
    
    ;Convert to numeric types.
    if typecode ne 7 then begin
        year   = fix(year,   TYPE=type)
        month  = fix(month,  TYPE=type)
        day    = fix(day,    TYPE=type)
        hour   = fix(hour,   TYPE=type)
        minute = fix(minute, TYPE=type)
        second = fix(second, TYPE=5)      ;Always a double
    endif
end
