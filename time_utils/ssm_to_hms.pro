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
;                       The second of SSM_TIME
;       SECOND:         out, optional, type=Same as `HMS_TIME`
;                       The fractional seconds of SSM_TIME
;
; :Keywords:
;       TO_STRING:      in, optional, type=Boolean, default=1
;                       Convert the output to a string.
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
;-
function ssm_to_hms, ssm_time, hour, minute, second, decimal, $
TO_STRING=to_string, $
DELIMETER=delimiter
    compile_opt idl2
    on_error, 2
    
    if n_elements(to_string) eq 0 then to_string = 1
    if n_elements(delimeter) eq 0 then delimeter = ':'
    
    ntimes = n_elements(ssm_time)

	;get the hour, minute, and second
    hour = floor(ssm_time/3600L)
    minute = floor((ssm_time mod 3600L)/60L)
    second = (ssm_time mod 3600.0D) mod 60.0

    ;convert to HHMMSS string
    if keyword_set(to_string) then begin

        ;convert to string
        hour = string(hour, format='(i02.2)')
        minute = string(minute, format='(i02.2)')
        
        ;Append a zero to numbers less than 10 so that they read ":01", ":02", etc.
        ;This is necessary because we do not know the precision of the seconds.
        append_zero = strarr(ntimes)
        iltTen = where(second lt 10, count)
        if count gt 0 then append_zero[iltTen] = '0'
        second = append_zero + string(second, format='(f0)')
        
        ;combine to form the HMS string
        hms_time = hour + delimeter + minute + delimeter + second

    ;otherwise convert to a numeric value
    endif else begin
        hms_time = hour*10000D + minute*100D + second
    endelse
    
    if ntimes eq 1 $
        then return, hms_time[0] $
        else return, hms_time

end
  
 