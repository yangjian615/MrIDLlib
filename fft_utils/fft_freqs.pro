; docformat = 'rst'
;
; NAME:
;
;       FFT_FREQS
;
; PURPOSE:
;+
;       Compute the positive and negative FFT frequencies.
;
; :Categories:
;
;       FFT Utility
;
; :Params:
;
;       NFFT:               in, required, type=int
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float, default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;
; :Keywords:
;
;       FNYQUIST:           out, type=float
;                           The nyquist frequency
;       INYQUIST:           out, type=long
;                           The index value within FREQS at which to find the nyquist
;                               frequency
;                           
; :Returns:
;
;       FREQUENCIES:        The FFT frequencies
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
;-
function fft_freqs, nfft, dt, $
FNYQUIST = fnyquist, $
INYQUIST = inyquist
	compile_opt strictarr
	on_error, 2
    
    if n_elements(dt) eq 0 then dt = 1.0
    if nfft mod 2 ne 0 then message, 'NPTS must be even.'
    
    ;calculate the index of the nyquist frequency
    inyquist = nfft/2
    
    ;create an array of frequency indices
    ;the most negative frequency will be one index beyond the nyquist
    ;find the negative index value of the most negative frequency (inyquist+1 -> -inyquist+1)
    ifreqs = lindgen(nfft)
    imost_negf = inyquist + 1
    imost_neg = -inyquist + 1
    
    ;make an array of indices from the most negative to least negative index (-inyquist+1 to -1)
    ;to do so, create an array of imost_neg points ranging from 0 to imost_neg-1 (two away from inyquist)
    ;then subtract imost_neg so that it ranges from -imost_neg to -1
    neg_freqs = lindgen(-imost_neg) + imost_neg
    ifreqs[imost_negf:nfft-1] = neg_freqs

    ;divide by the number of points, nfft, and the sampling interval, dt, to get the 
    ;frequencies
    frequencies = float(ifreqs) / (nfft * dt)
    fnyquist = frequencies[inyquist]
    
    return, frequencies
end