; docformat = 'rst'
;
; NAME:
;       SSM_TO_HMS
;
; PURPOSE:
;+
;       Convert time from a seconds since midnight value to hour-minute-second
;       (HHMMSS.mmmuuunnnddd) format.
;
; :Params:
;       SSM_TIME:       in, required, type=Numeric [array]
;                       The time in seconds since midnight to be converted.
;       HOUR:           out, optional, type=Same as `HMS_TIME`
;                       The hour of SSM_TIME
;       MINUTE:         out, optional, type=Same as `HMS_TIME`
;                       The minute of SSM_TIME
;       SECOND:         out, optional, type=Same as `HMS_TIME`
;                       The fractional seconds of SSM_TIME
;
; :Keywords:
;       NUMBER:         in, optional, type=Boolean, default=0
;                       If set, `HMS_TIME` will be a float of the form HHMMSS.dddd.
;       NUMVEC:         in, optional, type=Boolean, default=0
;                       If set, output will be a 3xN vector of the form
;                           [hour, minutes, seconds].
;       DELIMETER:      in, optional, type=string, default=':'
;                       If used with `TO_STRING`, it is the delimiter that separates the
;                           hour, minute, and second values (e.g. HH:MM:SS)
;
; :Returns:
;       SSM_TIME:       The time in seconds since midnight
;
; :Author:
;   Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;		Durham, NH, 03824
;       mry27@wildcats.unh.edu
;
; :History:
;   Modification History::
;
;       Written by:     Matthew Argall 08 March 2012
;       03/08/2012:     Added optional outputs hour, minute, second
;       11/26/2012:     Moved ssm_to_hms to a separate function. Added keywords TO_STRING
;                           and DELIMETER.
;       12/14/2012:     Added ON_ERROR, 2. Appended a leading zero to a second value less
;                           than 10. -MRA
;       07/07/2013:     Made defaults for TO_STRING and DELIMETER be 1 and ':ssm'. - MRA
;       2013-10-25  -   Return a scalar if a scalar was given. - MRA
;       2014/10/29  -   Removed TO_STRING and added NUMBER and NUMVEC keywords. - MRA
;-
function ssm_to_hms, ssm_time, hour, minute, second, decimal, $
NUMBER=number, $
NUMVEC=numvec, $
DELIMETER=delimiter
    compile_opt idl2
    on_error, 2
    
    number = keyword_set(number)
    numvec = keyword_set(numvec)
    if n_elements(delimeter) eq 0 then delimeter = ':'
    if number + numvec gt 1 then message, 'NUMBER and NUMVEC are mutually exclusive.'
    
    nTimes = n_elements(ssm_time)

	;get the hour, minute, and second
    hour   = floor(ssm_time/3600L)
    minute = floor((ssm_time mod 3600L)/60L)
    second = (ssm_time mod 3600.0D) mod 60.0

    ;Number of the form HHMMSS
    if number then begin
        hms_time = hour*10000D + minute*100D + second
        
    ;Vector of the form [hour, minute, second]
    endif else if numvec then begin
        ;Need to transpose?
        if array_equal(size(ssm_time, /DIMENSIONS), [1,nTimes]) then begin
            hour   = reform(hour)
            minute = reform(minute)
            second = reform(second)
        endif
        hms_time = transpose([[hour], [minute], [second]])
        
    ;'String of the form 'HH:MM:SS.ddd'
    endif else begin
        ;convert to string
        hour   = string(hour, format='(i02.2)')
        minute = string(minute, format='(i02.2)')
        
        ;Append a zero to numbers less than 10 so that they read ":01", ":02", etc.
        ;This is necessary because we do not know the precision of the seconds.
        append_zero = strarr(ntimes)
        iltTen = where(second lt 10, count)
        if count gt 0 then append_zero[iltTen] = '0'
        second = append_zero + string(second, format='(f0)')
        
        ;combine to form the HMS string
        hms_time = hour + delimeter + minute + delimeter + second
    endelse
    
    if ntimes eq 1 $
        then return, hms_time[0] $
        else return, hms_time

end
  
 