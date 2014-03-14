; docformat = 'rst'
;
; NAME:
;       UNIX_TO_DATETIME
;
; PURPOSE:
;+
;       Convert time from UNIX time -- the number of seconds since 1970, not including
;       leap seconds -- to date-time (YYYY-MM-DDTHH:MM:SS.dddddd).
;
; :Categories:
;   Time Conversion
;
; :Example:
;   See the main level program at the end of this file::
;
;       IDL> .r unix_to_datetime
;
; :Params:
;       UNIX_TIME           in, required, type=dblarr
;                           UNIX times to convert to date-time format.
;
; :Keywords:
;       DATE_DELIM:         in, optional, type=string, default='-'
;                           If used with `TO_STRING`, it is the delimeter that separates
;       TIME_DELIM:         in, optional, type=string, default=':'
;                           If used with `TO_STRING`, it is the delimeter that separates
;                               the hour, minute, and second values (e.g. HH:MM:SS)
;       SEPARATOR:          in, optional, type=string, default='T'
;                           If used with `TO_STRING`, it is the delimeter that separates
;                               the date and time (e.g. dateTtime)
;
; :Returns:
;       DATETIME:           Unix time converted to seconds since midnight.
;
; :Uses:
;   Uses the following external programs::
;       year_day.pro
;       hms_to_ssm.pro
;
; :Author:
;    Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;
;       02/20/2013  -   Written by Matthew Argall
;-
function unix_to_datetime, unix_time, $
DATE_DELIM = date_delim, $
TIME_DELIM = time_delim, $
SEPARATOR = separator
    compile_opt idl2
    on_error, 2
    
    ;Make a copy of the input array and count the number of elements.
    unix_temp = unix_time
    ntimes = n_elements(unix_time)

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Create default delimiters
    if n_elements(date_delim) eq 0 then date_delim = '-'
    if n_elements(time_delim) eq 0 then time_delim = ':'
    if n_elements(separator) eq 0 then separator = 'T'

;-----------------------------------------------------
;Start of Each Year, In Seconds \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------  
    
    ;Calculate UNIX times out to 3170AD
    years_into_future = 2000
    
    ;Create an array of years
    ref_years = indgen(years_into_future) + 1970
    
    ;figure out which years are leap years
    ileap_years = where((ref_years mod 4) eq 0)
    
    ;Number of days per year, including leap days
    days_per_year = lonarr(years_into_future) + 365
    days_per_year[ileap_years] += 1
    
    ;Calculate the number of days past Jan. 1, 1970 on which each year starts.
    start_day = [0, total(days_per_year, /CUMULATIVE)]
    
    ;Calculate the number of seconds past Jan. 1, 1970 at which each year begins
    ;Note that 365 days of 86400 seconds will put you at the start of the new year.
    year_start_ssm = start_day * 86400.0D

;-----------------------------------------------------
;Start of Month, in Seconds \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    month_numbers = intarr(12) + 1

    ;Day of year of the start of each month for non-leap years
    start_of_month = year_day(['19700101', '19700201', '19700301', '19700401', '19700501', $
                               '19700601', '19700701', '19700801', '19700901', '19701001', $
                               '19701101', '19701201'])
                               
    ;For leap years
    start_of_month_leap = start_of_month
    start_of_month_leap[2:*] += 1
    
    ;Convert days to seconds. We want to start at the beginning of the day, not the end,
    ;so subtract 1.
    month_ssm = (start_of_month-1) * 86400L
    month_ssm_leap = (start_of_month_leap-1) * 86400L

;-----------------------------------------------------
;Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    datetime = strarr(ntimes)
    year = intarr(ntimes)
    month = intarr(ntimes)
    day = intarr(ntimes)
    hsm = strarr(ntimes)

;-----------------------------------------------------
;Calculate Year \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Figure out into which years the unix times fall
    theseYears = value_locate(year_start_ssm, unix_time)
    
    ;Store the years.
    year = ref_years[theseYears]
    
    ;Subtract off the number of seconds to the beginning of the year. Must add a day
    ;because, e.g. 16*86400 is the start of the 17th day, but (16*86400 mod 86400) = 16
    unix_temp = unix_temp - year_start_ssm[theseYears] + 86400

;-----------------------------------------------------
;Calculate Month \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Check for leap years
    iLeap = where((year mod 4) eq 0, nleaps, COMPLEMENT=iyear, NCOMPLEMENT=nyears)
    
    ;Find the month and subract that number of seconds
    if nleaps ne 0 then begin
        month[iLeap] = month_numbers[value_locate(month_ssm_leap, unix_temp[iLeap])]
        unix_temp -= month_ssm_leap[month[ileap]-1]
    endif
    
    if nyears ne 0 then begin
        month[iyear] = month_numbers[value_locate(month_ssm, unix_temp[iyear])]
        unix_temp -= month_ssm[month[iyear]-1]
    endif

;-----------------------------------------------------
;Calculate Day, Hour, Minute, Second \\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Locate the month
    day = floor(unix_temp / 86400)
    
    ;Only the seconds within the day should remain. Convert to hours-minutes-seconds.
    hms = ssm_to_hms((unix_temp mod 86400D), /TO_STRING, DELIMITER=time_delim)
    
;-----------------------------------------------------
;Put Everythin Together \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    datetime = string(year, FORMAT='(i4.4)') + date_delim + $
               string(month, FORMAT='(i2.2)') + date_delim + $
               string(day, FORMAT='(i2.2)') + separator + $
               hms

    return, datetime
end


    
;-----------------------------------------------------
;Main Level Program \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
unix_time = 1.35838081406d * 1e+09
datetime = unix_to_datetime(unix_time)

print, FORMAT='(%"The unix time corresponding to 1.35838081406e+09 is %s")', datetime
end