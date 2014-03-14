; docformat = 'rst'
;
; NAME:
;
;       NFFT_INTERVALS
;
; PURPOSE:
;+
;       The purpose of this program is to compute how many fft windows fit within a
;       given interval.
;
; :Categories:
;
;       FFT Utility
;
; :Params:
;
;       NPTS                in, required, type=long
;                           The number of points in the data interval to be analized.
;       NFFT                in, required, type=int
;                           The number of points to use per FFT
;       NSHIFT              in, optional, type=int, default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;                           
; :Returns:
;
;       N_INTERVALS         The number of FFT intervals of length NFFT that fit within
;                               NPTS (with a shift of NSHIFT).
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
;       Written by:     Matthew Argall 27 November 2012
;       08/01/2013  -   If NSHIFT=0, then N_INTERVALS = 1. - MRA
;-
;*****************************************************************************************
function nfft_intervals, npts, nfft, nshift
	compile_opt idl2
	
	;Default to shifting by 1 point
	if n_params() lt 2 then nshift = 1
	if nshift eq 0 then return, 1
	
	;Calculate the number of fft intervals that fit inside NPTS.
	n_intervals = floor(1 + (npts - nfft) / nshift)
	
	return, n_intervals
end