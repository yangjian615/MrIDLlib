; docformat = 'rst'
;
; NAME:
;       MrNearestNeighbor
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
;   Find the nearest neighbor of Y in X
;
; :Params:
;       X:              in, required, type=numarr
;                       A vector of monotonically increasing or decreasing values.
;       Y:              in, required, type=numarr
;                       Values for which the location is required.
;
; :Keywords:
;       INDEX:          out, optional, type=long
;                       Index into X of the nearest neighbor to each value in Y.
;-
function MrNearestNeighbor, x, y
	compile_opt idl2
	on_error, 2

	nx = n_elements(x)
	ny = n_elements(y)

	;Locate y in x
	;   - Make sure they are all within range
	if nx eq 1 $
		then index = replicate(0, n_elements(y)) $
		else index = value_locate(x, y) > 0
	
	;Always return a row vector (Nx1)
	if size(index, /N_DIMENSIONS) eq 2 then index = reform(index)

	;Find the neighboring index
	;   - Make sure they are all in range
	iup = (index + 1) < (nx - 1)
	
	;Which is closer?
	ichange = where( abs(x[iup] - y) lt abs(x[index] - y), nchange)
	if nchange gt 0 then index[ichange] = index[ichange] + 1
	
	return, index
end