; docformat = 'rst'
;
; NAME:
;       DATEGEN
;
; PURPOSE:
;+
;   Generate a string array of sequential dates formatted as 'YYYY-MM-DD', where
;   the delimiters are optional.
;
; :Categories:
;       Utility, Time
;
; :Examples:
;       See also the main level program after the main program::
;
;           .r date_gen
;
; :Params:
;       START_DATE:     in, required, type=String
;                       Date ('YYYYMMDD') on which to start generating dates.
;       NDAYS:          in, required, type=int/string
;                       Number of days to generate. Days are sequential and start on
;                           `START_DATE`. If NDAYS is a string, then it is the last day
;                           in the sequence to be generated.
;
; :Keywords:
;       DELIMITER:      in, optional, type=string, default=''
;                       The delimiter separating the year, month, and day.
;
; :Returns:
;       DATEARR:        out, type=Strarr
;                       A string array of sequential dates ('YYYMMDD') starting on
;                           `START_DATE` and extending for `NDAYS`.
;
; :Uses:
;   Uses the following external programs::
;       days_between.pro
;       year_day.pro
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
;       Written by:  Matthew Argall 27 April 2012
;       05/21/2013  -   The second parameter can now be a date. If it is, the number of
;                           days between START_DATE and NDAYS is calculated. Added the
;                           DELIMITER keyword. - MRA.
;       10/23/2013  -   DELIMITER='-' is now the default. - MRA
;-
function dategen, start_date, ndays, $
DELIMITER = delimiter
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, -1
    endif
    
    ;The string separating the year, month, and day.
    if n_elements(delimiter) eq 0 then delimiter = '-'
    
    ;Was an end date given instead of the number of days?
    dtype = size(ndays, /TYPE)
    if dtype eq 7 then daysTot = days_between(start_date, ndays) + 1 $
                  else daysTot = ndays
    
    ;get the day of year and the year of the starting date
    sdoy = year_day(start_date, leap=leap)
    syr = strmid(start_date, 0, 4)
    
    ;convert the start year to long, initialize a YEARS array, and
    ;create an array of year days extending from the start day-of-year to
    ;start-day-of-year + ndays.
    yr = long(syr)
    years = lonarr(daysTot) + yr
    doys = lindgen(daysTot) + sdoy
    
    ;some leap days might be greater than 365 (366). Find them
    if leap then days_per_year = 366 else days_per_year = 365
    gtDPY = where(doys gt days_per_year, count)
    
    ;while there are some
    while count ne 0 do begin
        
        ;subtract the number of days in a single year and increase the year by one
        doys[gtDPY] -= days_per_year
        years[gtDPY] += 1
        yr += 1
        
        ;figure out of the new year is a leap year, update DPY, and find the DOYS
        ;that are still greater than 365 (366)
        if yr mod 4 eq 0 then days_per_year = 366 else days_per_year = 365
        gtDPY = where(doys gt days_per_year, count)
    endwhile
    
    ;convert day-of-year to month-day, then append it to the year
    moday = year_day(doys, LEAP=(years mod 4 eq 0), /TO_MODAY, DELIMITER=delimiter)
    datearr = string(years, format='(i4.4)') + delimiter + moday

    return, datearr
end


;---------------------------------------------------
; Main Level Example Program (.r dategen) //////////
;---------------------------------------------------
;Define the start date and number of days.
;Include leap day and new year.
start_date = '20111214'
ndays = 70
datearr = dategen(start_date, ndays)

print, datearr, format='(4(a10, 3x))'
end