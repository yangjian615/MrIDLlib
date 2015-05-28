; docformat = 'rst'
;
; NAME:
;       MrMedian
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may  be used to endorse or promote products derived from this     ;
;         software without specific prior written permission.                            ;
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
;       A wrapper for the Median function. Changes include::
;           - Return the index of the median value.
;
; :Categories:
;       Wrapper, Array Utilities
;
; :Params:
;       ARRAY:              in, required, type=numeric array
;                           Array in which the median is sought.
;       SUB:                in, optional, type=intarr
;                           Subscript of the median value. If /EVEN is set and `ARRAY`
;                               has an even number of points, then the subscripts of the
;                               two values used to compute the average are returned.
;
; :Keywords:
;       EVEN:               in, optional, type=integer, default=0
;                           If set, then when Array contains an even number of points
;                               (i.e. there is no middle number), MEDIAN returns the
;                               average of the two middle numbers.
;
; :Returns:
;       MED:                out, required, type=numeric scalar
;                           The median value of `ARRAY`
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Matthew Argall 2015
;
; :History:
;   Modification History::
;       2015/05/20  -   Written by Matthew Argall
;-
function MrMedian, array, sub, $
EVEN=even
	compile_opt idl2
	on_error, 2
	
	even = keyword_set(even)
	
	;Only works for 1D arrays
	if size(array, /N_DIMENSIONS) ne 1 then message, 'ARRAY must be a 1D array.'

	;Sort the array into ascending order
	isort = sort(array)
	
	;Number of elements
	n = n_elements(array)
	iseven = (n mod 2) eq 0

	;Find the median
	if n eq 1 then begin
		sub = 0
		med = array[0]
	
	;Even number of points with average
	endif else if iseven && even then begin
		;Subscripts of two median values
		sub = [n/2, n/2 + 1]
		sub = isort[sub]
		
		;Average median value
		med = ( array[sub[0]] + array[sub[1]] ) / 2.0
		
	;Other cases
	endif else begin
		;Subscript and value of median
		sub = isort[ ceil(n / 2.0) ]
		med = array[sub]
	endelse

	return, med
end