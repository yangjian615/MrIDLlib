; docformat = 'rst'
;
; NAME:
;       LEAP_DAYS_BETWEEN
;
; PURPOSE:
;+
;       Calculate the number of leap days between two dates.
;
; :Categories:
;
;       Utilities, Time
;
; :Params:
;       START_DATE:     in, required, type=Strarr.
;                       The date from which the number of leap days will be calculated.
;
;       END_DATE:       in, required, type=Strarr.
;                       The date to which the number of leap days will be calculated.
;
; :Returns:
;       NLEAPS:         The number of leap days between START_DATE and END_DATE. Includes
;                           the start and end dates if they are leap days.
;
; :Examples:
;       See the main level program at end of file::
;
;           .r leap_days_between
;
; :Uses:
;       Calls the following routines from my library::
;           dissectDate.pro
;           year_day.pro
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
;       Written by:     Matthew Argall 26 Mar 2012
;       04/28/2012:     Used STREGEX to remove need for DELIMETER keyword. MRA.
;       01/13/2012:     Simplified calculation of NLEAPS, result is now correct. MRA.
;-
function leap_days_between, start_date, end_date
	compile_opt idl2
	on_error, 2
      
    ;Ensure START_DATE and END_DATE have the same number of elements.  
    if n_elements(start_date) ne n_elements(end_date) then $
        message, 'START_DATE and END_DATE must have same # elements.'
    
    ;Make sure the start date is a string of the form 'YYYYMMDD'
    ;one character delimeter possible between year, month, and day (e.g. 'YYYY-MM-DD')
	if min(date_valid([start_date, end_date]) eq 0) then $
	    message, 'START_DATE and END_DATE must be string of form "YYYYMMDD" ' + $
	             '(1 char delimeters possible)'
    
    ;extract the year, month and day. 
    dissectDate, start_date, syear, TYPE=2
    dissectDate, end_date, eyear, TYPE=2
	
	;Calculate the day of the year. Also get which years are leap years.
	sdoy = year_day(start_date, YEAR=syear, LEAP=sleap)
	edoy = year_day(end_date, YEAR=eyear, LEAP=eleap)	
	
	;Calculate the number of years between start year and end year.
	;Count the number of years from start year to the next leap year.
	;Divide by 4 to get the number of leap days
	nleaps = eyear/4 - syear/4 + sleap

	;If the first year is a leap year and we already passed Feb 29
	;then we have to subtract a leap day (leap day is day 60).
	;in all other places, subtract 0
	nleaps -= sdoy gt 60 and sleap
	
	;If the end year is a leap year and we have not reached Feb 29
	;AND the start and end years are different, then subtract a leap day.
	;in all other places, subtract 0
	nleaps -= eleap and (edoy lt 60) and (eyear ne syear)
	
	return, nleaps	
end

;---------------------------------------------------
; Main Level Example Program (.r year_day) /////////
;---------------------------------------------------
start_date = ['20120228', '19920228', '20100228', '2008-02-29', '2009/08/23', '1996/02/29']
end_date =   ['20120301', '20120301', '20120301', '2012/03/01', '2012/02/29', '2012/02/29']
nleap_days = leap_days_between(start_date, end_date)
print, '    START        END       LEAP_BETWEEN'
for i=0, n_elements(start_date)-1 $
	do print, start_date[i], end_date[i], nleap_days[i], format='(a10, 3x, a10, 8x, i03)'
end