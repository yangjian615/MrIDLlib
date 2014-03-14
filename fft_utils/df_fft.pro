;+
; NAME:
;       DF_FFT
;
; PURPOSE:
;
;       The purpose of this program is to compute the frequency bin spacing, df, of a
;       Fast Fourier Transform.
;
; :Categories:
;       FFT Utility
;
; :Params:
;       NFFT                in, required, type=int
;                           The number of points to use per FFT
;       DT                  in, optional, type=float, default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;                           
; :Returns:
;       DF                  The difference in frequency between frequency bins.
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
;       08/09/2013  -   Default to sampling period of 1.0.
;-
function df_fft, nfft, dt
	compile_opt idl2
	
	if n_elements(dt) eq 0 then dt = 1.0
	
	;calculate the frequency bin size.
	df = 1.0 / (nfft * dt)
	
	return, df
end