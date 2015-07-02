; docformat = 'rst'
;
; NAME:
;       MrDOY2Date
;
; PURPOSE:
;+
;   Convert day-of-year to a date.
;
; :Categories:
;   Time Utility
;
; :Params:
;       DOY:    in, required, type=integer/intarr
;               The day-of-year. Ranges from 1-355 during common years, and 1-366
;                   during leap years.
;       YEAR:   in, optional, type=integer/intarr, default=1991
;               Year in which `DOY` resides. Used to calculate. If not given, a
;                   non-leap year will be used.
;
; :Returns:
;       DATE:   A 2xN integer array indicating the day and month.
;
; :Author::
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History
;       2015-06-29:     Written by Matthew Argall
;-
function MrDOY2Date, doy, year
	compile_opt idl2
	on_error, 2
	
	;Default to non-leap year
	_year = n_elements(year) eq 0 ? 1991 : year
	
	;Calculate the Julian year
	jul_newyear = julday(1, 1, _year)
	
	;Add the number of days (do not count Jan 1 twice!)
	jul_date = jul_newyear + doy - 1
	caldat, jul_date, month, day
	
	return, transpose([[day], [month]])
end