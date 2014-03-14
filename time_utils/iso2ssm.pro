; docformat = 'rst'
;
; NAME:
;       DATATIME_TO_EPOCH
;
; PURPOSE:
;+
;       The purpose of this program is to convert an ISO-1806 string to seconds elapsed
;       since midnight.
;
; :Params:
;       ISOTIME:        in, required, type=string/strarr
;                       ISO-1806 times.
;
; :Keywords:
;       DATE:           out, optional, type=strarr
;                       The date of each element in `ISOTIME`
;
; :Returns:
;       T_SSM:          out, type=dblarr
;                       The number of seconds since midnight.
;
; :Uses:
;   Uses the following external programs::
;       MrTimeStampToValues.pro
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
;       2014/03/03  -   Written by Matthew Argall
;-
function iso2ssm, isoTime, $
DATE = date
	compile_opt strictarr
    on_error, 2

;-----------------------------------------------------------------------------------------
;Calculate Seconds Since Midnight ////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------

    ;Dissect the date into integers
    MrTimeStampToValues, isoTime, YEAR=year, MONTH=month, DAY=day, $
                                  HOUR=hour, MINUTE=minute, SECOND=second, TYPE=2

    ;Calculate the seconds since midnight
    t_ssm = hour*3600D + minute*60.0D + second

    ;Return the date, too, if asked.
    if arg_present(date) then $
        date = string(year, '-', month, '-', day, FORMAT='(i04, a1, i02, a1, i02)')
    
    if n_elements(isoTime) eq 1 $
        then return, t_ssm[0] $
        else return, t_ssm
end
