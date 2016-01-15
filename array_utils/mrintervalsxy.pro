
; docformat = 'rst'
;
; NAME:
;       MrIntervalsXY
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
;   Find continuous, overlapping segments of data among two monotonic,
;   uniformly spaced datasets.
;
; :Categories:
;       Array Utilities
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
;*****************************************************************************************
;+
;
;   Intervals in X that fall entirely between intervals in Y, so that there is no
;   overlapping data, are removed from X.
;
; :Params:
;       IX:         in, required, type=2xN lonarr
;                   Indices into a monotonic, evenly spaced array that define the
;                       start IX[1,:] and end IX[2,*] of continuous subsets of data.
;       IY:         in, required, type=2xN lonarr
;                   Indices into a monotonic, evenly spaced array that define the
;                       start IY[1,:] and end IY[2,*] of continuous subsets of data.
;       X:          in, required, type=2xN array
;                   Array values at locations `IX`.
;       Y:          in, required, type=2xN array
;                   Array values at locations `IY`.
;
; Parameters:
;   IX:           in, required, type = 2xN double
;   IY:           in, required, type = 2xN double
;   X:            in, required, type = 2xN double
;   Y:            in, required, type = 2xN double
;
; :Returns:
;   IX_OUT:       out, required, type = 2xN double array
;-
function MrIntervalsXY_Remove, ix, iy, x, y

	nx = n_elements( ix[0, *] )
	ny = n_elements( iy[0, *] )
	x_out  = x
	ix_out = ix

	; Remove X intervals that fall entirely within a Y data gap (and
	; vice versa).
	ii = 0
	jj = 0
	while ii le nx && jj le ny
		;Does X begin in a data gap?
		ibegin = where( x[1,ii] gt y[2,jj:*], nbegin)
		
		if nbegin gt 0 then begin
			;Does the X-interval end before the gap finished?
			;   - Mark it for deletion
			if x[2,ii] lt y[1, jj+ibegin[0]] then ix_out[*,ii] = -1
		
			;X and Y are monotinically increasing, so there is
			;no need to check X against the first JJ points in
			;Y anymore. Leave them out of our next search.
			jj = jj + ibegin[0] + 1
		endif
		
		;Check next interval
		ii += 1
	endwhile
	
	;Prune
	igood = where(ix_out[0,*] ne -1, ngood)
	if ngood gt 0 then ix_out = [0,igood] else ix_out = -1
	
	return, ix_out
end


;+
;   Find continuous, overlapping segments of data among two monotonic,
;   uniformly spaced datasets.
;
; :Categories:
;       Array Utilities
;
; :Params:
;       X:          in, required, type=array
;                   Monotonically increasing, uniformly spaced array.
;       Y:          in, required, type=array
;                   Monotonically increasing, uniformly spaced array.
;
; :Keywords:
;       REMOVE:     in, optional, type=boolean, default=0
;                   If set, non-overlapping data will be removed.
;       SYNC:       in, optional, type=boolean, default=0
;                   If set, the start and stop index of each interval will
;                       be adjusted so that the corresponding values in `X`
;                       and `Y` are as close as possible.
;       TOLERANCE:  in, optional, type=boolean, default=0
;                   If true, and `SYNC` is in use, specify the number of
;                     look-ahead and look-behind points.
;-
pro MrIntervalsXY, x, y, ix, iy, $
REMOVE=remove, $
SYNC=sync, $
TOLERANCE=tolerance
	compile_opt idl2
	on_error, 2

	;Defaults
	tf_remove = keyword_set(remove)
	tf_syn    = keyword_set(sync)
	if n_elements(tolerance) eq 0 then tolerance = 1

;------------------------------------;
; Time and Sampling Intervals        ;
;------------------------------------;

	; Find indices within the time arrays that bracket continuous data
	; intervals.
	ix = MrIntervalsX(x)
	iy = MrIntervalsX(y)

;------------------------------------;
; Remove Intervals                   ;
;------------------------------------;

	; Remove from FGM
	if tf_remove
		; Extract the data values the define the interval
		xx = x[ix]
		yy = x[iy]
		
		; Remove intervals
		ix = MrIntervalsXY_Remove( ix, iy, xx, yy )
		iy = MrIntervalsXY_Remove( iy, ix, xx, yy )
	end

;------------------------------------;
; Sync Times                         ;
;------------------------------------;

	; Remove from FGM
	if tf_sync
		ix = MrIntervalsXY_Sync( ix, iy, X(ix), Y(iy), TOLERANCE=tolerance )
		iy = MrIntervalsXY_Sync( iy, ix, Y(iy), X(ix), TOLERANCE=tolernace )
	end
end