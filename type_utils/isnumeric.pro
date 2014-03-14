; docformat = 'rst'
;
; NAME:
;       TYPE_TO_FORMAT_CODE
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;       The purpose of this program is to determine if the input is of numeric type.
;
; :Categories:
;
;       Type Utility
;
; :Examples:
;
;       See the example program at the end of this file::
;
;           IDL> .r isnumeric
;
; :Params:
;
;       X:                      in, required, type=int
;                               The IDL type code of a variable, returned by
;                                   `SIZE(x, /TYPE)`
;
; :Returns:
;
;       TF_NUMERIC:             Is 1 if `X` is of a numeric type, 0 otherwise.
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
;       03/03/2013  -   Written by Matthew Argall
;-
function isnumeric, x
    compile_opt idl2
    on_error, 2
    
    ;Convert the type code to a format code character.
    case size(x, /TYPE) of
         0: tf_numeric = 0      ;undetermined type
         1: tf_numeric = 1      ;byte
         2: tf_numeric = 1      ;integer
         3: tf_numeric = 1      ;long
         4: tf_numeric = 1      ;float
         5: tf_numeric = 1      ;double
         6: tf_numeric = 1      ;complex
         7: tf_numeric = 0      ;character
         8: tf_numeric = 0      ;structure
         9: tf_numeric = 1      ;double complex
        10: tf_numeric = 0      ;pointer
        11: tf_numeric = 0      ;object
        12: tf_numeric = 1      ;uint
        13: tf_numeric = 1      ;ulong
        14: tf_numeric = 1      ;long64
        15: tf_numeric = 1      ;ulong64
        else: tf_numeric = 0    ;undefined type
    endcase
    
    return, tf_numeric
end

;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Create variables of different types
int = 5
str = 's'
dbl = 1D

;Determine if they are of a numeric type
int_isnum = isnumeric(int)
str_isnum = isnumeric(str)
dbl_isnum = isnumeric(dbl)

;Print the results
print, format='(%"x = %3i     Is Numeric? %i")', int, int_isnum
print, format='(%"x = %3s     Is Numeric? %i")', str, str_isnum
print, format='(%"x = %3.1f     Is Numeric? %i")', dbl, dbl_isnum

end