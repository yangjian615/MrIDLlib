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
;       T_IN:           in, required, type=string/strarr
;                       The time, of form 'HHMMSS[.DDD]', to be converted to seconds since
;                           midnight.
;
; :Keywords:
;       BACKWARD:       in, optional, type=Boolean, default=0
;                       Indicate that T_IN represents a time in HMS and that it should be
;                           converted to STRSSM.
;
; :Returns:
;       T_SSM:          `T_IN` converted to seconds since midnight. Alternatively, if
;                           `BACKWARD` is set, then it is string HMS time.
;
; :Restrictions:
;   This function is now obsolete. See::
;           hms_to_ssm.pro
;           ssm_to_hms.pro
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
;       03/14/2012:       implemented keyword BACKWARD to convert from SSM to
;			                StrHMS, made array inputs possible. MRA.
;       03/26/2012:       added keyword DELIMETER. MRA.
;       03/27/2012:       DELIMETER can now be inserted into the time with /BACKWARD. MRA.
;       04/28/2012:       use STREGEX to remove need to specify DELIMETER. MRA.
;-
function strhms_to_ssm, t_in, $
BACKWARD = backward
    compile_opt idl2
    on_error, 2

;---------------------------------------------------------------------
;Created by Matthew Argall:: 01/25/2012
;
;STRHMS_TO_SSM converts a string in the form 'HHMMSS[.DDD]' (HMS) to
;seconds since midnight (SSM) or vice versa.
;
;INPUTS
; str_hms:       string, 'HHMMSS[.DDD]', time of day
;
;OUTPUTS
; ssm:       the CDF epoch value corresponing to the input date
;
;MODIFICATIONS
; 03/14/2012:       implemented keyword BACKWARD to convert from SSM to
;			   StrHMS, made array inputs possible
; 03/26/2012:       added keyword DELIMETER
; 03/27/2012:       DELIMETER can now be inserted into the time with /BACKWARD
; 04/28/2012:       use STREGEX to remove need to specify DELIMETER
;---------------------------------------------------------------------

	;convert from SSM to StrHMS
	if keyword_set(backward) then begin
		
		t_hms = hms_to_ssm(t_in, /backward)
		decimal = t_hms mod 1D
		t_strhms = string(long(t_hms), format='(i6.6)')

        if n_elements(delimeter) ne 0 then begin
            hour = strmid(t_strhms, 0, 2)
            minute = strmid(t_strhms, 2, 2)
            second = strmid(t_strhms, 4, 2)
            t_strhms = temporary(hour) + delimeter + temporary(minute) + delimeter + temporary(second)
        endif else delimeter = ''

		;extract the first three decimal places. will return *** if t is negative
		decimal = stregex(string(decimal, format='(f5.3)'), '[.][1-9][1-9]?[1-9]?', /extract)
		
		;make the resultant string of hms time
		t_strhms = t_strhms + decimal
		
		;ensure that time is a string of the form 'HHMMSS[.DDD]' (with optional delimeter)
		check = '^[0-9]{2}' + delimeter + '[0-9]{2}' + delimeter + '[0-9]{2}' + '[.]?[0-9]*$?'
		is_time = stregex(t_strhms, check)
		if min(is_time) eq -1 then message, 'time must be string of form "HHMMSS[.DDD]"'
	
		return, t_strhms

	;convert StrHMS to SSM	
	endif else begin
    
        ;make sure the start date is a string of the form 'HHMMSS'
        ;one character delimeter possible between hour, minute, and second (e.g. 'HH:MM:SS')
        if min(stregex(t_in, '[0-9]{2}.?[0-9]{2}.?[0-9]{2}')) eq -1 then $
            message, 'DATE must be string of form "HHMMSS" (1 char delimeters possible)'
            
        ;extract the year, month and day
        ;empty strings returned by stregex are converted to 0 with LONG() and DOUBLE()
        parts = stregex(t_in, '([0-9]{2}).?([0-9]{2}).?([0-9]{2})([.]?[0-9]*$?)', /subexpr, length=len)
        hour = long(strmid(t_in, parts[1,*], len[1,*]))
        minute = long(strmid(t_in, parts[2,*], len[2,*]))
        second = long(strmid(t_in, parts[3,*], len[3,*]))
        milli = double(strmid(t_in, parts[4,*], len[4,*]))
		
        ;calculate the time in seconds since midnight and check that it is valid (20864 sec/day)
		t_ssm = hour*3600D + minute*60D + second + milli
		if max(t_ssm gt 86400D) then message, 'Invalid input time'
	
		return, t_ssm
	endelse
end
