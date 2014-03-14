; docformat = 'rst'
;
; NAME:
;       MONTHNAME
;
; PURPOSE:
;+
;       Turn a month number or an array of month numbers into the names of their
;       corresponding months. If the months are already names, they are returned with
;       the first letter uppercase and the other letters lowercase.
;
; :Params:
;       MONTH:              in, required, type=STRING/NUMERIC [array]
;                           The number of the month.
;
; :Keywords:
;       FULL_NAME:          in, optional, type=Boolean, default=0
;                           Return the full name of the month. The default is to return
;                               the three-letter abbreviation.
;
; :Returns:
;       NAME_OF_MONTH:      The name (string) that corresponds to `MONTH`::
;                               01 = 'Jan[urary]'   05 = 'May'      09 = 'Sep[tember]'
;                               02 = 'Feb[ruary]'   06 = 'Jun[e]'   10 = 'Oct[ober]'
;                               03 = 'Mar[ch]'      07 = 'Jul[y]'   11 = 'Nov[ember]'
;                               04 = 'Apr[il]'      08 = 'Aug[ust]' 12 = 'Dec[ember]'
;                                   
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       Durham, NH, 03824
;       mry27@wildcats.unh.edu
;
; :History:
;   Modification History::
;
;       Written by: Matthew Argall 12 November 2012
;-
function monthName, month, $
FULL_NAME=fullname
    compile_opt idl2
    on_error, 2

    ;The names of the month
    if keyword_set(fullname) then begin    
        names = ['January', 'February', 'March', 'April', 'May', 'June', $
                 'July', 'August', 'September', 'October', 'November', 'December']
    endif else begin    
        names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
                 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    endelse

    ;if MONTH is a string, it could be a number or a name
    if size(month, /TYPE) eq 7 then begin
        ;check the first entry to see if it is name
        thisMonth = where(strcmp(names, month[0], /FOLD_CASE) eq 1, count)
    
        ;if it is not a name
        if count eq 0 then begin
            ;then it must be a number. Turn the string into a number
            num_month = fix(month)
            
            ;get the name using the month number
            monthName = names[num_month-1]
            
        ;if it is a name, make sure the first letter is capital
        endif else begin
            monthName = strupcase(strmid(month, 0, 1)) + strlowcase(strmid(month, 1, 2))
        endelse
        
    ;Otherwise MONTH is a number. Get the name of the month.
    endif else monthName = names[month-1]

    return, monthName
end