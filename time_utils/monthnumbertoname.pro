; docformat = 'rst'
;
; NAME:
;       MonthNumberToName.pro
;
;+
;   The purpose of this method is to convert 2-digit months to a name.
;   E.g. '01' -> "January'.
;
; :Categories:
;   Time Utility
;
; :Params:
;       MONTH_NUMBERS:  In, required, type=string/strarr
;                       The 2-digit month number to be converted to a name.
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
;       2014/02/19  -   Written by Matthew Argall
;       2014/03/05  -   Changed the default case to first-character chapitalized and
;                           added the UPPERCASE and LOWERCASE keywords. - MRA
;-
function MonthNumberToName, month_number, $
ABBR = abbr, $
UPPERCASE=uppercase, $
LOWERCASE=lowercase
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
        then month_names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', $
                            'Sep', 'Oct', 'Nov', 'Dec'] $
        else month_names = ['January', 'February', 'March', 'April', 'May', 'June', 'July', $
                            'August', 'September', 'October', 'November', 'December']
    
    ;Get the requested names
    names = month_names[month_number-1]
    if keyword_set(uppercase) then names = strupcase(names)
    if keyword_set(lowercase) then names = strlowcase(names)
    
    return, names
end
