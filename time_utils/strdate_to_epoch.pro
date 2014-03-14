; docformat = 'rst'
;
; NAME:
;       STRSSM_TO_HMS
;
; PURPOSE:
;+
;       Convert time from a seconds since midnight value to hour-minute-second
;       (HHMMSS.mmmuuunnnddd) format.
;
; :Params:
;       DATE:       in, required, type=string
;                   The date to be converted. 'YYYYMMDD'
;       TIME:       in, required, type=string
;                   The time to be converted. 'HHMMSS'
;
; :Keywords:
;       EPOCH16:        in, optional, type=Boolean, default=0
;                       Convert to an EPOCH16 value instead of EPOCH.
;
; :Returns:
;       EPOCH:          the CDF epoch value corresponing to the input date
;
; :Restrictions:
;   This function is now obsolete. See::
;           datetime_to_epoch.pro OR
;           convert_time.pro
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
;       Written by:     Matthew Argall 08 November 2011
;-
function strdate_to_epoch, date, time, $
EPOCH16 = epoch16
    compile_opt idl2
    on_error, 2

	;make sure date is a string of the form 'YYYYMMDD'
	is_date = stregex(date, '[0-9]{6}')
	if is_date eq -1 then message, 'date must be string of form "YYMMDD"'
	
	;ensure that time is a string of the form 'HHMMSS'
	is_time = stregex(time, '^[0-9]{6}[.]?[0-9]*$?')
	if is_time eq -1 then message, 'time must be string of form "HHMMSS[.DDD]"'
	
	;break the date into year, month, and day. convert to integer
	year = long(strmid(date, 0, 4))
	month = long(strmid(date, 4, 2))
	day =  long(strmid(date, 6, 2))
	
	;break the time into hour, minute, second. convert to integer
	len = strlen(time)
	hour = long(strmid(time, 0, 2))
	minute = long(strmid(time, 2, 2))
	second = strmid(time, 4, 2)
	
	deci = stregex(time, '[.][0-9]*$')
	if deci eq -1 then milli = 0 else milli = float(strmid(time, deci, len)) * 1000.0
		
	;compute the epoch or epoch16 value
	if keyword_set(epoch16) then begin
        cdf_epoch16, epoch, year, month, day, hour, minute, second, milli, /compute_epoch
    endif else begin
        cdf_epoch, epoch, year, month, day, hour, minute, second, milli, /compute_epoch
    endelse
	
	return, epoch

end
