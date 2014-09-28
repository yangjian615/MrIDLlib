; docformat = 'rst'
;
; NAME:
;       MrWeekNumberToDay.pro
;
;+
;   The purpose of this method is to convert the number of a weekday to the name of
;   the day. Numbers range from 1 on Sunday to 7 on Satruday.
;
; :Categories:
;   Time Utility
;
; :Params:
;       WKDAY_NUMBERS:  In, required, type=integer/string/intarr/strarr
;                       The day of week numbers to be converted to a names.
;
; :Keywords:
;       ABBR:           in, otpional, type=boolean, default=0
;                       If set, 3-character abbreviated month names are returned.
;       UPPERCASE:      in, optional, type=boolean, default=0
;                       Return `NAMES` in all uppercase.
;       LOWERCASE:      in, optional, type=boolean, default=0
;                       Return `NAMES` in all lowercase.
;
; :Returns:
;       NAMES:          Names of the monthS corresponding to `MONTH_NUMBERS`.
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
function MrWeekNumberToDay, wkday_number, $
ABBR = abbr, $
UPPERCASE=uppercase, $
LOWERCASE=lowercase
    compile_opt strictarr
    on_error, 2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Names of the months
    if keyword_set(abbr) $
        then wkday_names = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'] $
        else wkday_names = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', $
                            'Friday', 'Saturday']
    
    ;Get the requested names
    names = wkday_names[wkday_number-1]
    if keyword_set(uppercase) then names = strupcase(names)
    if keyword_set(lowercase) then names = strlowcase(names)
    
    return, names
end


;---------------------------------------------------------
; Main Level Example Program (.r MrWeekDayToNumber) //////
;---------------------------------------------------------
days = [1, 2, 3, 4, 5, 6, 7]
nos  = MrWeekNumberToDay(days)

print, '--------------------------------------'
print, 'Days:    [' + strjoin(string(days, FORMAT='(i1)'), ', ') + ']'
print, 'Names:   [' + strjoin(nos, ', ') + ']'
print, ''

end
