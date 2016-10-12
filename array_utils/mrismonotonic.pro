; docformat = 'rst'
;
; NAME:
;       MrIsMonotonic
;
;*****************************************************************************************
;   Copyright (c) 2016, University of New Hampshire                                      ;
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
;         contributors may be used to endorse or promote products derived from this      ;
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
;   Determine if elements in an array are monotonic. An array is monotonically
;   increasing (decreasing) if the difference between adjacent elements is always
;   positive (negative). Equal values are not strictly monotonic.
;
; :Categories:
;   Array utilities
;
; :Params:
;       ARRAY:      in, required, type=numeric
;                   Array for which to determine monotonicity
;
; :Keywords:
;       DECREASE:   in, required, type=boolean, default=0
;                   If set, check for monotonically decreasing.
;       NAN:        in, required, type=boolean, default=0
;                   If set, NaN values are ignored.
;
; :Returns:
;       TF_MONO:    out, required, type=boolean
;                   Returns true (1) if monotonic and 0 otherwise.
;
; :Author:
;    Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016-07-25  -   Written by Matthew Argall
;-
function MrIsMonotonic, array, $
NAN=nan, $
DECREASE=decrease
	compile_opt idl2
	on_error, 2
	
	;Make sure the array is a column or row vector
	sz = size(array)
	if ~(sz[0] le 1) && ~(sz[0] eq 2 && sz[1] eq 1) then message, 'ARRAY must be 1xN or Nx1.'
	
	;If there is only one value
	if sz[sz[0]+2] eq 1 then tf_mono = finite(array)
	
	;Defaults
	tf_dec  = keyword_set(decrease)
	tf_nan  = keyword_set(nan)
	tf_mono = 0B
	
	;Ignore NaNs
	if tf_nan then begin
		;Look for non-NaN values
		iGood = where(finite(array), nGood)
		if nGood gt 0 then begin
			;Pick out the good points
			temp = array[iGood]
			
			;Check for monotonicity
			if tf_dec $
				then tf_mono = array_equal( (temp[1:*] - temporary(temp)) lt 0, 1 ) $
				else tf_mono = array_equal( (temp[1:*] - temporary(temp)) gt 0, 1 )
		endif
	
	;Keep NaNs
	endif else begin
		;Check for monotonicity
		if tf_dec $
			then tf_mono = array_equal( (array[1:*] - array) lt 0, 1 ) $
			else tf_mono = array_equal( (array[1:*] - array) gt 0, 1 )
	endelse
	
	;Return the monotonicity result
	return, tf_mono
end