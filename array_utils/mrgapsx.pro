; docformat = 'rst'
;
; NAME:
;       MrGapsX
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
;   Find gaps in a monotoically increasing, evenly spaced array.
;
; :Categories:
;       Array Utilities
;
; :Params:
;       ARRAY:              in, required, type=string/long
;                           Monotonically increasing, uniformly spaced array.
;       DX:                 in, optional, type=float, default=Median(ARRAY[1:*] - ARRAY)
;                           Spacing between elements of `ARRAY`. If not provided, the
;                               median separation of data points is used.
;
; :Keywords:
;       COUNT:              in, optional, type=intarr
;                           Number of gaps found.
;       GAPSIZE:            out, optional, type=integer/intarr
;                           Number of data points in each gap, in units of `DX`. If
;                               `COUNT` is 0 then GAPSIZE=0.
;
; :Returns:
;       IGAPS:              Indices into `ARRAY` where data gaps begin and end.
;                               IGAPS[*,0] indicate the last point in `ARRAY` before
;                               a gap, while `IGAPS[*,1] indicate the first point in
;                               `ARRAY` after the data gap. If `ARRAY` does not contain
;                               any gaps, -1 is returned and `COUNT`=0.
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
;       2014/04/30  -   Written by Matthew Argall
;       2016/02/11  -   Fixed indexing error when retrieving gap size. - MRA
;       2016/07/24  -   GAPSIZE is now an integer, not a float. - MRA
;-
function MrGapsX, array, dx, $
GAPSIZE=gapsize, $
COUNT=count
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Sampling Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Take the difference between adjacent points
	;   - The last point will be truncated
	darr = array[1:*] - array
	
	;Sampling interval
	if n_elements(dx) eq 0 then dx = median(darr)

;-----------------------------------------------------
; Find Gaps \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Number of samples between points.
	;  - Round to remove systematic noise
	gapSize = floor(darr / dx)
	
	;Location of the data gaps
	igaps = where(gapSize gt 1, count)

	;Start and stop of each gap
	if count gt 0 then begin
		gapSize = gapSize[iGaps]
		igaps = [[igaps], [igaps+1]]
	endif else begin
		gapsize = 0
	endelse

	return, igaps
end
