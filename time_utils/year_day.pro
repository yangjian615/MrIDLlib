; docformat = 'rst'
;
; NAME:
;       YEAR_DAY
;
; PURPOSE:
;+
;       Calculate the day of the year. January 1st = 1, December 31st = 365 or 366
;       Alternatively, calculate the month + day ('MMDD') of a day-of-year
;
; :Categories:
;   Time Utility
;
; EXAMPLES:
;
;       See main level program at end::
;
;           .r year_day
;
;
; :Params:
;       DATE:   in, required, type=string/strarr/int/intarr
;               If TO_MODAY is /not/ set, DATE is a string (or array of strings) of the
;               form 'YYYYMMDD' (1 char delimeter possible -- e.g. 'YYYY-MM-DD')
;               for which the day of year is to be calculated.
;
;               If TO_MODAY is set, then date represents a year day. The return value
;               will be the month + day of the input date.
;
; :Keywords:
;       DELIMITER:      in, optional, type=string, default='-'
;                       if `TO_MODAY` is set, then this is the delimiter separating the
;                           month from the year.
;
;       LEAP:   in, out, optional, type=intarr, default=0
;               Is an input only if `TO_MODAY` is set. Indicate that the day(s) of year
;                   given by DATE occurs during a leap year.
;
;               Is an output only if `TO_MODAY` is /not/ set. Indicates whether the input 
;                   year is a leap year. Will have same number of elements as `DATE`.::
;
;                       1 = leap year
;                       0 = not a leap year 
;
;       YEAR:   in, optional, type=string/strarr.
;               If `DATE` represents the day-of-year, then this represents the years
;                   in which the day-of-year falls ('YYYYMMDD' with optional delimeter).
;
; :Returns:
;
;       MODAY:      Returned if `TO_MODAY` is set. 
;                   A string of the month + day of `DATE`. Will have same # elements as `DATE`
;
;       YEAR_DAY:   returned if `TO_MODAY` is /not/ set. 
;                   The day of the year since January 1st. Will have same # elements as `DATE`
;
; :Uses:
;   Uses the following external programs::
;       dissectDate.pro
;
; :Author::
;   Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;		Durham, NH, 03824
;       mry27@wildcats.unh.edu
;
; :History:
;   Modification History
;       Written by:     Matthew Argall:: 26 Mar 2012
;       04/27/2012:     removed DELIMETER.
;                           extract year, month and day with STREGEX
;                           DATE can be an array of dates
;       04/28/2012:     added TO_MODAY keyword and associated code
;       01/11/2013:     added YEAR keyword, doys now converted to dates properly.
;       01/14/2013:     added ON_ERROR, 2
;       05/21/2013:     Added the DELIMITER keyword. - MRA
;       2014/03/03:     Default delimiter is now "-". - MRA
;-
function year_day, date, $
DELIMITER = delimiter, $
LEAP = leap, $
YEAR = year, $
TO_MODAY = to_moday
	compile_opt idl2
	on_error, 2
	
	;Days in each month (index = month-1s). Will add leap day later if necessary
	days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	first_of_month = [0, total(days_in_month[0:-2], /CUMULATIVE)] + 1
	first_of_month_leap = [first_of_month[0:1], first_of_month[2:*] + 1]
		
    ;convert from year-day to month-day if requested
    if keyword_set(to_moday) then begin
    
        ;check the inputs
        if n_elements(leap) gt 0 and n_elements(year) gt 0 $
            then message, 'LEAP and YEAR are mutually exlusive'
        if max(date gt 366) or max(date lt 1) then message, 'DATE must range from 1 to 366'
        if n_elements(year) gt 0 then leap = (year mod 4) eq 0
        if n_elements(delimiter) eq 0 then delimiter = '-'
        
        ndates = n_elements(date)
        nleap = n_elements(leap)
        case nLeap of
            0: leap = replicate(0, ndates)
            1: leap = replicate(leap, ndates)
            else: if nLeap ne nDates then message, 'LEAP must have same # elements as DATE.'
        endcase
        
        ;Find which month the day-of-year value corresponds to. Add 1 to make it 1-based.
        ;subtract the year day of the first of the month from date to get the day
        month = lonarr(ndates)
        day = lonarr(ndates)
        isleap = where(leap eq 1, count, COMPLEMENT=notleap, NCOMPLEMENT=notcount)
        if count gt 0 then begin
            month[isleap] = value_locate(first_of_month_leap, date[isleap]) + 1
            day[isleap] = date[isleap] - first_of_month_leap[month[isleap]-1] + 1
        endif

        ;Do the same for non-leap-years
        if notcount gt 0 then begin
            month[notleap] = value_locate(first_of_month, date[notleap]) + 1
            day[notleap] = date[notleap] - first_of_month[month[notleap]-1] + 1
        endif

        ;Make a string on the form 'MMDD'.
        moday = string(month, format='(i02.2)') + delimiter + string(day, format='(i2.2)')

	    return, moday
	    
	;otherwise convert a date to the day of year
	endif else begin
	    ;Dissect the date-string into year, month, and day. Return as integers.
	    dissectDate, date, year, month, day, TYPE=2
        
        ;check for leap days. leap will be an array of 1s and 0s
        leap = (year mod 4) eq 0
        
        ;check if the day and month are valid
        ;increase the days in February by 1 if it is a leap year
        if max(month gt 12) then message, 'Month not valid'
        if max(day gt (days_in_month[month-1] + (month eq 2 and leap))) then $
            message, 'Day not valid'
        
        ;calculate the year day by totalling up the days in the previous months
        ;then adding the current day of the month. careful of negative indexing
        ndays = lonarr(n_elements(year))
        ndays = first_of_month[month-1]-1
        year_day = fix(ndays + day)
        
        ;if MONTH is greater than 2, then add LEAP. leap year will already be included in
        ;DAY if MONTH is February, so do not add it again.
        year_day += (month gt 2) and leap
    
        if n_elements(date) eq 1 then year_day = year_day[0]
        return, year_day
    endelse
end

;---------------------------------------------------
; Main Level Example Program (.r year_day) /////////
;---------------------------------------------------
date = '20120101'
doy = year_day(date)
print, format='(%"The year-day of %s is %i")', date, doy

date = ['20110301', '20120301']
leap = [0, 1]
doy = year_day(date, LEAP=leap)
print, format='(%"The year-days of [%s, %s] are %i and %i")', date, doy

doy = 152
month_day = year_day(doy, /LEAP, /TO_MODAY)
print, format='(%"The date (MMDD) of day %i of a leap year is %s")', doy, month_day
end