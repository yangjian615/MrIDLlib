; docformat = 'rst'
;
; NAME:
;       UNIX2SSE
;
; PURPOSE:
;+
;       Convert time from UNIX time -- the number of seconds since 1970, not including
;       leap seconds -- to seconds since epoch (sse). Note that all date information
;       is lost in the conversion.
;
; :Categories:
;   Time Conversion
;
; :Params:
;       UNIX_TIME:          in, required, type=dblarr
;                           UNIX times to convert to SSM.
;       UNIX_EPOCH:         in, optional, type=double, default=midnight on UNIX_TIME[0]
;                           Epoch from which `SSE_TIME` is referenced.
;
; :Returns:
;       SSE_TIME:           Unix time converted to seconds since the given epoch.
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
;       2016/04/03  -   Written by Matthew Argall
;-
function unix2sse, unix_time, unix_epoch
	compile_opt idl2
	on_error, 2

	;Default epoch time
	;   - Midnight of the first given data point.
	if n_elements(unix_epoch) eq 0 then begin
		unix_epoch = unix_time[0] - (unix_time[0] mod 86400.0D)
	endif

	;Subtract the epoch time.
	sse_time = unix_time - unix_epoch

	return, sse_time
end