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
;       ABBR:           in, optional, type=boolean, default=0
;                       Indicate that `NAMES` are 3-character abbreviated month names.
;       TYPE:           in, optional, type=string/integer, default='STRING'
;                       Variable type-name or type-code of the resulting `MONTH_NUMBERS`.
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
;       2014/03/19  -   Replace STRING with TYPE keyword. Strings are returned by default. - MRA
;-
function MonthNameToNumber, names, $
ABBR = abbr, $
TYPE = type
    compile_opt strictarr
    on_error, 2

    ;Default to strings
    if n_elements(type) eq 0 then type = 7
    if size(type, /TNAME) eq 'STRING' $
        then tcode = typenameToCode(type) $
        else tcode = type

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
    case tcode of
        2: ;Do nothing
        7: month_numbers = string(month_numbers, FORMAT='(i02)')
        else: month_numbers = fix(month_numbers, TYPE=tcode)
    endcase
    
    return, month_numbers
end