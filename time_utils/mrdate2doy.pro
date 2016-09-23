; docformat = 'rst'
;
; NAME:
;       MrDOY2Date
;
; PURPOSE:
;+
;   Convert day-of-year to a date.
;
;   Calling Sequence:
;       doy = MrDate2DOY(month, day[, year])
;       doy = MrDate2DOY(date)
;
; :Categories:
;   Time Utility
;
; EXAMPLES:
;       See main level program at end::
;           .run MrDate2DOY
;
;
; :Params:
;       MONTH:  in, required, type=integer/intarr/string
;               Month. Ranges from 1-12, or a date-string formatted as 'YYYY-MM-DD'.
;       DAY:    in, optional, type=integer/intarr
;               Day of month. Can range from 1-31.
;       YEAR:   in, optional, type=integer/intarr, default=1991
;               Year in which `DOY` resides. Used to calculate. If not given, a
;                   non-leap year will be used.
;
; :Returns:
;       DOY:    The day-of-year. Ranges from 1-355 during common years, and 1-366
;                   during leap years.
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
;       2015-07-19:     A date-string may be given.
;-
function MrDate2DOY, month, day, year
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Accept a month, formatted as 'YYYY-MM-DD'
	if size(month, /TNAME) eq 'STRING' then begin
		;Make sure it fits
		if ~array_equal(stregex(month, '[0-9]{4}-[0-9]{2}-[0-9]{2}', /BOOLEAN), 1) $
			then message, 'DATE must be formatted as "YYYY-MM-DD".'
		
		;Extract the date
		year   = fix(strmid(month, 0, 4))
		_month = fix(strmid(month, 5, 2))
		day    = fix(strmid(month, 8, 2))
	endif else begin
		_month = month
	endelse
	
	;Default to non-leap year
	_year = n_elements(year) eq 0 ? 1991 : year

;-----------------------------------------------------
; Compute DOY \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Calculate the Julian year
	jul_newyear  = julday(1, 1, _year)
	jul_thisyear = julday( temporary(_month), day, temporary(_year) )
	
	;Subtract to get the day-of-year (add 1 to get Jan 1)
	doy = jul_thisyear - jul_newyear + 1
	
	return, doy
end

;---------------------------------------------------
; Main Level Example Program (.r year_day) /////////
;---------------------------------------------------

;TABLE 1
;   - Year-day calendar for common years
year  = 2015
month = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
ndays = [  31,    28,    31,    30,    31,    30,    31,    31,    30,    31,    30,    31 ]

;Print the day-of-month
print, ''
print, 'COMMON YEAR -- Day-of-Year Table'
print, FORMAT='(6x, 31(i2, 3x))', indgen(31)+1
print, strjoin(replicate('-', 158))

;Step through each month
for imonth = 0, 11 do begin
	;Calculate the days of year
	doy = MrDate2DOY(imonth+1, indgen(ndays[imonth]) + 1, year)
	
	;Print the results
	str_ndays = strtrim(ndays[imonth], 2)
	print, FORMAT='(a3, 2x, ' + str_ndays + '(i3, 2x))', month[imonth], doy
endfor

;TABLE 2
;   - Year-day calendar for leap years
year      = 2016
ndays[1] += 1

;Print the day-of-month
print, ''
print, ''
print, 'LEAP YEAR -- Day-of-Year Table'
print, FORMAT='(6x, 31(i2, 3x))', indgen(31)+1
print, strjoin(replicate('-', 158))

;Step through each month
for imonth = 0, 11 do begin
	;Calculate the days of year
	doy = MrDate2DOY(imonth+1, indgen(ndays[imonth]) + 1, year)
	
	;Print the results
	str_ndays = strtrim(ndays[imonth], 2)
	print, FORMAT='(a3, 2x, ' + str_ndays + '(i3, 2x))', month[imonth], doy
endfor


end