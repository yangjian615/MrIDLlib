;+
; docformat = 'rst'
;
; NAME:
;       MrIntervalsX
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
;   Find continuous data intervals within an evenly spaced, monotonic
;   vector of points.
;
; :Categories:
;       Array Utilities
;
; :Params:
;       X:          in, required, type=string/long
;                   Monotonically increasing, uniformly spaced array.
;       DX:         in, optional, type=float, default=Median(X[1:*] - X)
;                   Spacing between elements of `X`.
;
; :Keywords:
;       COUNT:      in, optional, type=intarr
;                   Number of intervals found.
;       
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2014/10/15  -   Written by Matthew Argall
;-
function MrIntervalsX, X, delta_x, $
COUNT=nIntervals

;------------------------------------;
; Time and Sampling Intervals        ;
;------------------------------------;

	; Take the difference between adjacent points
	nx = n_elements(x)
	dx = x[1:*] - x

	; Must know the sampling interval
	if n_elements(delta_x) eq 0 then delta_x = median(dx)

;------------------------------------;
; Data Intervals                     ;
;------------------------------------;

	; Number of sampling intervals between data points.
	;   - Round to remove the effect of small systematic noise.
	ndx = round(dx / delta_x)

	; Locations of the data gaps
	igaps = where(ndx gt 1, ngaps)

	; Number of data intervals
	;   - One more than the number of data gaps.
	nIntervals = ngaps + 1
	
	; Allocate memory
	;   - First column holds the beginning of an interval
	;   - The first data interval begins at 1, the last ends at index "end"
	idata         = lonarr(2, nIntervals)
	idata[0,0]    = 1
	idata[1,nx-1] = nx - 1

	; Other data intervals begin one point after.
	;   - The first point just prior to a data gap.
	;   - The first point just after a data gap.
	idata[1, 2:nIntervals] = igaps+1
	idata[2, 1:end-1]      = igaps
end
