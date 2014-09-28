; docformat = 'rst'
;
; NAME:
;       MrSigNum
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
; PURPOSE
;+
;   A wrapper for IDL's SigNum function introduced in IDL 8.3.
;
; :Examples:
;   See the example program at the end of this document::
;       IDL> .r MrSigNum
;
; :Params:
;       X:          in, required, type=numeric
;                   Determine the sign of each element. The sign is defined as::
;                        1          - X > 0
;                        0          - X = 0
;                       -1          - X < 0
;                        X / Abs(X) - X is complex
;                        NaN        - X is NaN
;                   
;
; :Returns:
;       RESULT:     Sign of each element of `X`.
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
;       2014/05/31  -   Written by Matthew Argall
;       2014/09/02  -   Return the same type as X, if possible. - MRA
;-
function MrSignum, x
    compile_opt strictarr
    on_error, 2
    
    ;Use IDL's function introduced in IDL 8.3
    if MrCmpVersion('8.3') le 0 then return, signum(x)
    
    ;Compute the sign.
    ;   - Preserve datatype if possible
    type  = size(x, /TYPE)
    if MrIsA(x, /COMPLEX) $
        then sign_x = x / abs(x) $
        else sign_x = -(fix(x lt 0, TYPE=type)) + fix(x gt 0, TYPE=type)
    
    return, sign_x
end


;---------------------------------------------------
; Main Level Example Program (.r MrSigNum) /////////
;---------------------------------------------------

;EXAMPLE 1
;   Compute the mean of a vector
x    = [-0.5, 0, 1.5]
sign = MrSigNum(x) 

;Print Results
print, '---------------------------------------------------------'
print, FORMAT='(%"Sign of [%0.1f, %0.1f %0.1f]:  [%i, %i, %i]")', x, sign
print, ''

end