; docformat = 'rst'
;
; NAME:
;
;       MrPSD
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
; :Examples:
;   See the main level program at the end of this file::
;
;       IDL> .r MrSpectrogram
;
; :Params:
;
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
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2012
;
; :History::
;   Modification History::
;       04/12/2013  -   Written by Matthew Argall
;       07/30/2013  -   Change _EXTRA to _REF_EXTRA. - MRA
;       09/30/2013  -   Added the O1_DIFFERENCE keyword. - MRA
;       10/04/2013  -   Renamed from MrSpectrogram to MrPSD. Renamed the O1_DIFFERENCE
;                           keyword to NDIFF and allow differences of order N with the
;                           use of MrTS_Diff. Remove the TIME keyword and let it be passed
;                           out in the _REF_EXTRA keyword if it is present. - MRA
;       2014/09/10  -   Now properly return (dt/N)*|fft|^2, instead of |fft|. - MRA
;       2014/10/20  -   Did not anticipate how previous changes would affect computation
;                           of frequencies. Fixed. - MRA
;-
function MrPSD, data, nfft, dt, nshift, $
DIMENSION = dimension, $
FREQUENCIES = frequencies, $
FMAX = fmax, $
FMIN = fmin, $
NDIFF = nDiff, $
T0 = t0, $
TIME = time, $
_REF_EXTRA = extra
    compile_opt idl2

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ds = size(data, /STRUCTURE)
    if ds.n_dimensions gt 2 then message, 'DATA must be a vector or array of vectors.'
    
    ;Dimension to take the FFT of
    if n_elements(dimension) eq 0 then if ds.n_dimensions eq 2 then begin
        void = max(ds.dimensions, imax)
        dimension = imax+1
    endif else dimension = 1
    if n_elements(dt) eq 0 then dt = 1.0
    if n_elements(t0) eq 0 then t0 = 0.0
    
    ;Differencing
    if n_elements(nDiff) eq 0 then nDiff = 0

;-----------------------------------------------------
;Difference? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if nDiff ne 0 $
        then data_temp = MrTS_Diff(data, nDiff, DIMENSION=dimension) $
        else data_temp = data

;-----------------------------------------------------
;Calculate Spectrogram \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Normalize the frequencies.
    ;   - Ensures proper normalization of PSD later (no erreneous factors of dt). 
    if n_elements(fmax) gt 0 then norm_fmax = fmax * dt
    if n_elements(fmin) gt 0 then norm_fmin = fmin * dt

    ;Take the FFT
    data_fft = MrFFT(temporary(data_temp), nfft, 1.0, nshift, $
                     DIMENSION   = dimension, $
                     FMAX        = norm_fmax, $
                     FMIN        = norm_fmin, $
                     FREQUENCIES = frequencies, $
                     TIME        = time, $
                    _EXTRA       = extra)

    ;Un-normalize the frequencies
    frequencies /= dt

    ;Select only the positive frquencies
    posFreqs    = where(frequencies gt 0)
    frequencies = frequencies[posFreqs]

    ;The FFT is returned with dimensions arranged as (time, frequency[, component]).
    ;IDL ignores the trailing "*" if there is no 3rd dimension.
    time = dt*time + t0
    psd  = abs(data_fft[*, posFreqs, *])^2 ;* (dt * nfft) ;* dt ;* (dt/nfft)

    return, psd
end