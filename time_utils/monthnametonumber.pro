;+
;   The purpose of this method is to convert month names to 2-digit months.
;   E.g. 'January' -> '01'
;
; :Categories:
;   Time Utility
;
; :Params:
;       NAMES:          in, required, type=string/strarr
;                       The names of the months.
;
; :Keywords:
;       ABBR:           in, otpional, type=boolean, default=0
;                       Indicate that `NAMES` are 3-character abbreviated month names.
;
; :Returns:
;       MONTH_NUMBERS:  The number of the month corresponding to `NAMES`.
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
;       2014/02/19  -   Written by Matthew Argall
;-
function MonthNameToNumber, names, $
ABBR = abbr, $
STRING=string
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Names of the months
    if keyword_set(abbr) $
        then month_names = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', $
                            'SEP', 'OCT', 'NOV', 'DEC'] $
        else month_names = ['JANUARY', 'FEBRUARY', 'MARCH', 'APRIL', 'MAY', 'JUNE', 'JULY', $
                            'AUGUST', 'SEPTEMBER', 'OCTOBER', 'NOVEMBER', 'DECEMBER']
    
    ;Get the month numbers. Month names must be sorted alphabetically.
    iSorted = sort(month_names)
    iMonths = value_locate(month_names[iSorted], strupcase(names))
    month_numbers = iSorted[iMonths] + 1

    ;Return strings?
    if keyword_set(string) then month_numbers = string(month_numbers, FORMAT='(i02)')
    
    return, month_numbers
end