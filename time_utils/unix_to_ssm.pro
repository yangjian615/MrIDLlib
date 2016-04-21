; docformat = 'rst'
;
; NAME:
;       UNIX_TO_SSM
;
; PURPOSE:
;+
;       Convert time from UNIX time -- the number of seconds since 1970, not including
;       leap seconds -- to seconds since midnight (ssm). Note that all date information
;       is lost in the conversion.
;
; :Categories:
;   Time Conversion
;
; :Params:
;       UNIX_TIME           in, required, type=dblarr
;                           UNIX times to convert to SSM.
;
; :Returns:
;       SSM_TIME:           Unix time converted to seconds since midnight.
;
; :Author:
;    Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       02/20/2013  -   Written by Matthew Argall
;-
function unix_to_ssm, unix_time
    compile_opt idl2
    on_error, 2
    
    ;Unix time is the number of seconds since 1970 without leap seconds. Dividing by the
    ;number of seconds per day will give a remainder of the number of seconds into the
    ;current day.
    ssm_time = unix_time mod 86400.0D
    
    return, ssm_time
end