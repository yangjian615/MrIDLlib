; docformat = 'rst'
;
; NAME:
;       TRUE_TO_HEX_COLOR
;
;
; PURPOSE:
;+
;       Convert a[n array of] 24-bit True Color value[s] and to a 6-character
;       hexidecimal string.
;
; :Categories:
;
;       Type Conversion, Color
;
; :Params:
;
;       TRUE_COLOR:         in, required, type=int/intarr
;                           A decimal number (or array of numbers) to convert to
;                               hexidecimal strings
;
; :Returns:
;
;       HEX_COLOR:          out, type=str/strarr
;                           The 6 character hexidecimal string value of DECIMAL_VALUE
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
;	Modification History::
;       11/01/2012      -   Written by Matthew Argall
;-
function true_to_hex_color, true_color
    compile_opt idl2
    
    hex_color = strtrim(string(format = '(z06)', long(value)), 2)
    
    return, hex_color
end