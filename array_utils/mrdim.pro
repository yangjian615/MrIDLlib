; docformat = 'rst'
;
; NAME:
;       MrDims
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
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
;   Get the dimension sizes of a variable. Same as Size(X, /DIMENSIONS), but with
;   the added ability to return the size of a single dimension.
;
;   NOTE:
;       The dimensions of an undefined variable and scalar are both 0
;           IDL> print, size(undefinedVariable, /DIMENSIONS)
;                      0
;           IDL> print, size(3, /DIMENSIONS)
;                      0
;
; :Categories:
;   Coordinate Systems
;
; :Params:
;       X:              in, required, type=any
;                       Variable for which the dimension sizes are desired.
;       DIM:            in, optional, type=integer
;                       The dimension for which the size is retured. If DIM is greater
;                           than the number of dimensions in `X`, then zero is returned.
;
; :Returns:
;       DIMSIZE:        out, required, type=integer/intarr
;                       Dimension size(s).
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/08/29  -   Written by Matthew Argall
;-
function MrDim, x, dim
	compile_opt idl2
	on_error, 2
	
	;Get the dimensions
	dimSize = size(x, /DIMENSIONS)
	
	;Return a single dimension
	nDim = n_elements(dim)
	if nDim gt 0 then begin
		if nDim gt 1 then message, 'DIM must be a scalar integer.'
		if dim  le 0 then message, 'DIM must be > 0.'
		if dim  gt 8 then message, 'DIM must be <= 8.'
		dimSize = dim gt n_elements(dimSize) ? 0 : dimSize[dim-1]
	endif
	
	;Return
	return, dimSize
end