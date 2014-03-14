; docformat = 'rst'
;
; NAME:
;       SSM_TO_DATETIME
;
; PURPOSE:
;+
;   Convert time from a seconds since midnight value to a date-time format.
;
; :Params:
;       DATE:               in, required, type=string/strarr
;                           The date of `SSM_TIME[0]`. Or the date of each `SSM_TIME` value.
;       SSM_TIME:           in, required, type=Numeric [array]
;                           The time in seconds since midnight to be converted.
;
; :Returns:
;       DATETIME:           out, type=string
;                           The converted date-time value 'YYYY-MM-DDTHH:MM:SS.sssssssss'.
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
;       2013-10-24  -   Written by Matthew Argall
;-
function ssm_to_datetime, date, ssm_time
    compile_opt idl2
    on_error, 2
    
    ;Was a single date given?
    if n_elements(date) eq 1 and n_elements(ssm_time) gt 1 then begin
        ;If SSM_TIME > 86400, then it lies in the following day. Find how many days
        ;ahead each time is.
        iDay = fix(ssm_time / 86400)
        
        ;Get the maximum number of days into the future that SSM_TIME extends
        maxDays = max(iDay)
        
        ;Generate dates between DATE and the last date in SSM_TIME.
        datearr = dateGen(date, maxDays+1)

        ;Concatenate the date onto the time.
        datetime = datearr[iDay] + 'T' + ssm_to_hms(ssm_time - iDay*86400.0)
    
    ;Array of dates
    endif else begin
        datetime = date + time_string
    endelse
    
    return, datetime
end
  
 