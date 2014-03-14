;+
; NAME:
;       DEC_TO_HEX
;
; PURPOSE:
;+
;       Convert a[n array of] decimal integer[s] to a hexidecimal string. Fractional
;       numbers are truncated.
;
; :Categories:
;
;       Type Conversion
;
; :Params:
;
;       DECIMAL_VALUE:      in, required, type=int/intarr
;                           A decimal number (or array of numbers) to convert to
;                               hexidecimal strings
;
; :Returns:
;
;       HEX_STRING:         out, type=str/strarr
;                           The hexidecimal value of DECIMAL_VALUE
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History::
;   Modification History::
;       11/01/2012      -   Written by Matthew Argall
;-
function dec_to_hex, value
    compile_opt idl2
    on_error, 2
    
    hex_string = strtrim(string(format = '(z06)', long(value)), 2)
    
    return, hex_string
end