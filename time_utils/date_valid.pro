; docformat = 'rst'
;
; NAME:
;       DATE_VALID
;
; PURPOSE:
;+
;       Determines if the date provided is a valid date. There are two levels of date
;       checking.
;           1. That the format is correct (i.e. has the form 'YYYYMMDD' with optional
;               delimeters.
;           2. That the date is an actual date (i.e. has the form 'YYYYMMDD' with optional
;               delimeters, that 1 < MM < 12 and that 1 < DD <= # days in MM.
;       This only works for A.D., not B.C.
;
; :Categories:
;       Utilities
;
; :Examples:
;       See also the main level program after the main program::
;
;           .r date_valid
;
; :Params:
;
;       DATE:           in, required, type=Strarr
;                       The dates to be checked for valid format ('YYYYMMDD' with optional
;                           delimeters).
;
; :Keywords:
;       CHECK_CAL:      in, optional, type=Boolean, default=0
;                       Check that the date provided is a calendar date, not simply that
;                           it has the proper format.
;
; :Returns:
;
;       ISDATE:         Returns 1 if the `DATE` is valid, 0 otherwise. Has the same number
;                           of elements as `DATE`.
;
; :Restrictions:
;       Used the following programs::
;           dissectDate.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Midification History::
;       Written by:  Matthew Argall 13 January 2013
;-
function date_valid, date, $
CHECK_CAL=check_cal
    compile_opt idl2
    on_error, 2
    
    ;allocate space for the ISDATE array.
    ndates = n_elements(date)
    isdate = intarr(ndates)
    
    ;Check to see if the month and day are valid, too
    if keyword_set(check_cal) then begin
        days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        days_per_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        ;Separate the date into year, month, and day. Return them as integers.
        dissectDate, date, year, month, day, TYPE=2
        
        ;check if the month is valid
        ismonth = month lt 12
        
        ;check for leap years
        isleap = year mod 4 eq 0
        leap = where(isleap eq 1, leap_count, COMPLEMENT=notLeap, NCOMPLEMENT=notLeap_count)
        
        ;check if the day is valid
        isday = intarr(ndates)
        if leap_count gt 0 $
            then isday[leap] = day[leap] gt 0 and day[leap] lt days_per_month_leap[month[leap]]
            
        if notLeap_count gt 0 $
            then isday[notLeap] = day[notLeap] gt 0 and day[notLeap] lt days_per_month[month[notLeap]]
        
        isdate = ismonth and isday
    
    endif else begin
    
        ;Check the date string for the year, month, and day subexpressions.
        pos = stregex(date, '([0-9]{4})[^0-9]?([0-9]{2})[^0-9]?([0-9]{2})', len=len, /subexp)
        
        ;Check which dates were valid
        valid = where(pos[0,*] ne -1, valid_count)
        
        ;Set the valid date bit in isdate
        if valid_count gt 0 then isdate[valid] = 1
    endelse
    
    return, isdate
end

;---------------------------------------------------
; Main Level Example Program (.r year_day) /////////
;---------------------------------------------------
;Create a list of dates of various formats. Some are actual dates, some are not.
date = ['20120323', '2012-03-23', '2012/03/23', '2012/03-23', '12/03/23', '2012/03/42', $
        '2012/13/23']
        
;Figure out which are properly formatted, then which are actual dates.
isdate = date_valid(date)
iscaldate = date_valid(date, /CHECK_CAL)

;Print the results.
print, 'The following dates have a valid format:'
print, 'CAL   FORMAT   DATE'
for i=0, n_elements(date)-1 $
    do print, format='(1x, i1, 6x, i1, 6x, a0)', iscaldate[i], isdate[i], date[i]
end