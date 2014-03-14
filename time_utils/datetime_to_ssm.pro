; docformat = 'rst'
;
; NAME:
;       DATATIME_TO_EPOCH
;
; PURPOSE:
;+
;       The purpose of this program is to convert date-time strings in the format
;       'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp' to seconds since midnight on the date indicated
;       by 'YYYY-MM-DD'. Here, the dilimeters and decimal places are optional.
;
; :Params:
;       DATETIME:       in, required, type=strarr
;                       An array of date-times in the form 'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp',
;                           where the delimiters and decimal places are optional, of the
;                           times to be converted to seconds since midnight.
;
; :Keywords:
;       DATE:           out, optional, type=strarr
;                       The date of each element in `DATETIME`
;
; :Returns:
;       T_SSM:          out, type=dblarr
;                       The number of seconds since midnight.
;
; :Uses:
;   Uses the following external programs::
;       dissectDateTime.pro
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
;       05/22/2013  -   Written by Matthew Argall
;       2014/01/21  -   Return a scalar if a scalar was given. - MRA
;-
function datetime_to_ssm, dateTime, $
DATE = date
	compile_opt strictarr
    on_error, 2

;-----------------------------------------------------------------------------------------
;Calculate Seconds Since Midnight ////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------

    ;Dissect the date into integers
    dissectDateTime, dateTime, year, month, day, $
                               hour, minute, second, $
                               milli, micro, nano, pico, TYPE=2
    
    ;Calculate the seconds since midnight
    t_ssm = hour*3600. + minute*60.0 + second + milli*1d-3 + micro*1d-6 + nano*1d-9

    ;Return the date, too, if asked.
    if arg_present(date) then $
        date = string(year, '-', month, '-', day, FORMAT='(i04, a1, i02, a1, i02)')
    
    if n_elements(dateTime) eq 1 $
        then return, t_ssm[0] $
        else return, t_ssm
        
end
