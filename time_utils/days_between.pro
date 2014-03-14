; docformat = 'rst'
;
; NAME:
;       DAYS_BETWEEN
;
; PURPOSE:
;+
;   Calculate the number of days between two dates
;
;   Note: The total count does not include the start date.
;
; :Categories:
;       Utility
;
; :Params:
;       SDATE:              in, required, type=Strarr
;                           The first date from which NDAYS will be calulated. Format is
;                               'YYYY-MM-DD', where delimiters are optional.
;       EDATE:              in, required, type=Strarr
;                           The second date to which NDAYS will be calculated. Format is
;                               'YYYY-MM-DD', where delimiters are optional.
;
; :Returns:
;       NDAYS:              Number of days between `START_DATE` and `END_DATE`, not counting
;                               the start dates.
;
; :Examples:
;   See the main level program::
;       IDL> .r days_between
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall, Room 113
;        Durham, NH, 03824
;        mry27@wildcats.unh.edu       
;
; :History:
;   Modification History::
;       Written by:     Matthew Argall 26 March 2012
;       04/28/2012:     Used STREGEX to remove need for DELIMETER keyword.
;                           START_DATE and END_DATE can now be arrays. MRA.
;       01/14/2013:     Added a CATCH block. MRA.
;       2014/01/23:     Greatly simplified things by subtracting two JulDays. - MRA
;-
function days_between, sDate, eDate
    compile_opt idl2

    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !null
    endif

    ;Get the year, month and day
    dissectDate, sDate, sYear, sMonth, sDay, TYPE=2
    dissectDate, eDate, eYear, eMonth, eDay, TYPE=2
    
    ;Calculate the Julian Day Number
    sJulDay = JulDay(sMonth, sDay, sYear)
    eJulDay = JulDay(eMonth, eDay, eYear)
    
    ;Subtract
    nDaysBtwn = eJulDay - sJulDay
    
    return, nDaysBtwn
    
    ;------------------------------;
    ; My previous version is below ;
    ;------------------------------;

;    ;Ensure sDate and sDate have the same number of elements.  
;    if n_elements(sDate) ne n_elements(sDate) then $
;        message, 'sDate and sDate must have same # elements.'
;    
;    ;Make sure the start date is a string of the form 'YYYYMMDD'
;    ;one character delimeter possible between year, month, and day (e.g. 'YYYY-MM-DD')
;    if min(date_valid([sDate, sDate]) eq 0) then $
;        message, 'sDate and sDate must be string of form "YYYYMMDD" ' + $
;                 '(1 char delimeters possible)'
;    
;    ;extract the year
;    syear = fix(strmid(sDate, 0, 4))
;    eyear = fix(strmid(sDate, 0, 4))
;    
;    ;get the year day of the start and end dates and see if they are leap years
;    sdoy = year_day(sDate, leap=sleap)
;    edoy = year_day(sDate, leap=eleap)
;    
;    ;calculate the number of leap days between two dates
;    ;DOY already contains the leap day information, so remove it from nleaps
;    ;(Feb. 29th is DOY 60)
;    nleaps = leap_days_between(sDate, sDate)
;    nleaps -= sdoy le 60 and sleap
;    nleaps -= edoy ge 60 and eleap
;    
;    ;find where SYEAR and EYEAR are the same and where they are different
;    diff_years = where(syear ne eyear, diff_count, $
;                       complement=same_year, ncomplement=same_count)
;    
;    ;initialize the result array
;    days_between = lonarr(diff_count + same_count)
;    
;    ;at each index at where the dates are different...
;    ;[To keep array size limited, all arrays created /before/ this point will be accessed
;    ;with DIFF_YEARS indices so that only the necessary number of array entries are
;    ;created/accessed. Arrays created within the IF statement will have DIFF_COUNT
;    ;number of elements]
;    if diff_count ne 0 then begin
;        ;calculate the number of days to the end of the year
;        days_left = (365+sleap[diff_years]) - sdoy[diff_years]
;        
;        ;number of years between the end of start_year and the beginning of end_year
;        ;turn it into days (leap years added later)
;        nyears = (eyear[diff_years] - syear[diff_years]) - 1
;        if nyears ne -1 then nmid_days = nyears * 365 else nmid_days = 0
;        
;        ;total the number of days between the two dates
;        ;DOY already contains the leap days, so subtract them out of nleaps
;        days_between[diff_years] = long(days_left + nmid_days + $
;                                        nleaps[diff_years] + edoy[diff_years])
;    endif
;    
;    ;at each index where the dates are the same...
;    if same_count ne 0 then begin
;        ;just calculate the difference between year days.
;        days_between = long(edoy[same_year] - sdoy[same_year])
;    endif
;    
;    return, days_between
    
end

;---------------------------------------------------
; Main Level Example Program (.r year_day) /////////
;---------------------------------------------------
start_date = ['20120326', '20120323', '20120630', '20120101', '2012/08/23', '2012-10/14']
end_date =   ['20120328', '20120501', '20120822', '20120514', '2012/09/11', '2012/12-25']
ndays = days_between(start_date, end_date)
print, '    START        END       DAYS_BETWEEN'
for i=0, n_elements(start_date)-1 $
    do print, start_date[i], end_date[i], ndays[i], format='(a10, 3x, a10, 8x, i03)'
end
