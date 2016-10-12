; docformat = 'rst'
;
; NAME:
;
;       MrPSD2
;
; PURPOSE:
;+
;       The purpose of this program is to calculate the power spectral density of a
;       vector.
;
; :Categories:
;
;       Math Utility, Spectral Analysis
;
; :Params:
;       DATA:               in, required, type=NxM fltarr
;                           The data for which a spectrogram is desired.
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;       DIMENSION:          in, optional, type=int, default=0
;                           The dimension over which to take the FFT. If 0, then the FFT
;                               is taken over all dimensions (this is the default). As an
;                               example, say DATA is an N1xN2 array. If DIMENSION=2, then
;                               the N1 FFTs will be taken along each DATA[i,*]
;       FREQUENCIES:        out, type=fltarr(NFFT)
;                           The frequency bins of the spectrogram
;       TIME:               out, type=dblarr
;                           The time associated with each Power Spectral Density slice. It
;                               is taken at the beginning of each FFT.
;      _REF_EXTRA:          in, type=structure
;                           Any keyword accepted by MrFFT.pro is also accepted for keyword
;                               inheritance.
;                           
; :Returns:
;
;       PSD:                Power spectral density of `DATA`
;
; :Uses:
;   Uses the following external programs::
;       MrFFT.pro
;       MrTS_Diff.pro
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
;       Matthew Argall 2016
;
; :History::
;   Modification History::
;       2016/08/29  -   Written by Matthew Argall
;-
function MrPSD2, data, nfft, nshift, $
DIMENSION = dimension, $
DT = dt, $
FREQUENCIES = frequencies, $
FMAX = fmax, $
FMIN = fmin, $
NDIFF = nDiff, $
T0 = t0, $
TIME = time, $
_REF_EXTRA = extra
	compile_opt idl2

;-----------------------------------------------------
; Take the FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Take the FFT
	data_fft = MrFFT2( data, dt, $
	                   NFFT        = nfft, $
	                   NSHIFT      = nshift, $
	                   DIMENSION   = dimension, $
	                   DT_MEDIAN   = dt_median, $
	                   FMAX        = norm_fmax, $
	                   FMIN        = norm_fmin, $
	                   FREQUENCIES = frequencies, $
	                   TIME        = time, $
	                  _EXTRA       = extra )

	;If FREQUENCES is >= 2D, IPOS below will inflate PSD enormously.
	;   - Use Array_Indices to reduce dimensions (?)
	if size(frequencies, /N_DIMENSIONS) gt 1 then begin
		message, 'FREQUENCIES has more than 1 dimension. Stop to investigate effects.'
		stop
	endif

;-----------------------------------------------------
; Compute the PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ipos = where(frequencies gt 0, npos)

	;The FFT is returned with dimensions arranged as (time, frequency[, component]).
	;IDL ignores the trailing "*" if there is no 3rd dimension.
	time        = time + t0
	frequencies = frequencies[ipos]
	psd         = abs(data_fft[*, ipos, *])^2 ;* (dt * nfft) ;* dt ;* (dt/nfft)

	return, psd
end