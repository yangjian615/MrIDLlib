; docformat = 'rst'
;
; NAME:
;       MrMean
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
;   A wrapper for IDL's Mean() function that contains a DIMENSION keyword for IDL
;   versions pre-8.0.
;
; :Examples:
;   See the example program at the end of this document::
;       IDL> .r MrMean
;
; :Params:
;       X:              in, required, type=numeric
;                   
; :Keywords:
;       DIMENSION:      in, optional, type=integer, default=0
;                       The dimension, starting with 1, along which the mean should
;                           be computed. The default, 0, averages over all dimensions. 
;       DOUBLE:         in, optional, type=boolean, default=0
;                       If set, all computations will be performed in double-precision.
;       NAN:            in, optional, type=boolean, default=0
;                       If set, NaNs will be excluded from the results.
;
; :Returns:
;       RESULT:         Mean of `X`.
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
;       2014/04/05  -   Written by Matthew Argall
;-
function MrMean, x, $
DIMENSION=dimension, $
DOUBLE=double, $
NAN=nan
    compile_opt strictarr
    on_error, 2
    
    ;Use IDL's mean function for versions 8.0 and higher or if the DIMENSION keyword
    ;was not given.
    if MrCmpVersion('8.0') le 0 || n_elements(dimension) eq 0 $
        then return, mean(x, DIMENSION=dimension, DOUBLE=double, NAN=nan)

    ;Mean over all dimensions?
    if dimension eq 0 then return, mean(x, DOUBLE=double, NAN=nan)

    ;Mean over a specific dimension
    dims = size(x, /DIMENSIONS)
    N    = dims[dimension-1]
    
    ;Compute the mean
    result = total(x, dimension, DOUBLE=double, NAN=nan) / N

    return, result
end


;---------------------------------------------------
; Main Level Example Program (.r MrMean) ///////////
;---------------------------------------------------

;EXAMPLE 1
;   Compute the mean of a vector
x = indgen(10)
xMean = MrMean(x)

;EXAMPLE 2
;   Mean over the second dimension of a matrix
y = indgen(3,5)
yMean = MrMean(y, DIMENSION=2)

;Print Results
print, '---------------------------------------------------------'
print, FORMAT='(%"Mean of a Vector:     %5.1f")', xMean
print, FORMAT='(%"Mean of Dimension 2: [%5.1f, %5.1f, %5.1f]")', yMean
print, ''

end