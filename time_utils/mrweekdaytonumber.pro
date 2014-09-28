;+
;   The purpose of this method is to convert names of week days to -- Sunday = 1 and
;   Saturday = 7. It is the inverse operation of MrWeekNumberToDay.pro.
;
; :Examples:
;   See the main level program at the end of this document.
;       IDL> .r MrWeekDayToNumber
;
; :Categories:
;   Time Utility
;
; :Params:
;       NAMES:          in, required, type=string/strarr
;                       The names of the week days.
;
; :Keywords:
;       ABBR:           in, optional, type=boolean, default=0
;                       Indicate that `NAMES` are 3-character abbreviated week day names.
;       TYPE:           in, optional, type=string/integer, default='STRING'
;                       Variable type-name or type-code of the resulting `WKDAY_NUMBERS`.
;
; :Returns:
;       WKDAY_NUMBERS:  The number of the day corresponding to `NAMES`.
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       typeNameToCode.pro
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
;       2014/04/29  -   Written by Matthew Argall
;-
function MrWeekDayToNumber, names, $
ABBR = abbr, $
TYPE = type
    compile_opt strictarr
    on_error, 2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Default to strings
    if n_elements(type) eq 0 then type = 7
    if size(type, /TNAME) eq 'STRING' $
        then tcode = typenameToCode(type) $
        else tcode = type

    ;Names of the months
    if keyword_set(abbr) $
        then wkday_names = ['SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT'] $
        else wkday_names = ['SUNDAY', 'MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', $
                            'FRIDAY', 'SATURDAY']
    
    ;Get the month numbers. Month names must be sorted alphabetically.
    iSorted = sort(wkday_names)
    iMonths = value_locate(wkday_names[iSorted], strupcase(names))
    wkday_numbers = iSorted[iMonths] + 1

    ;Return strings?
    case tcode of
        2: ;Do nothing
        7: wkday_numbers = string(wkday_numbers, FORMAT='(i02)')
        else: wkday_numbers = fix(wkday_numbers, TYPE=tcode)
    endcase
    
    return, wkday_numbers
end


;---------------------------------------------------------
; Main Level Example Program (.r MrWeekDayToNumber) //////
;---------------------------------------------------------
days = ['SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT']
nos  = MrWeekDayToNumber(days, /ABBR)

print, '--------------------------------------'
print, 'Days:    [' + strjoin(days, ', ') + ']'
print, 'Numbers: [' + strjoin(nos, ', ')  + ']'
print, ''

end
