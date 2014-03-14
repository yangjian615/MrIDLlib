;+
; NAME:
;       DT_FFT
;
; PURPOSE:
;
;       The purpose of this program is to calculate the time between samples of a time
;       array.
;
; :Categories:
;       FFT Utility
;
; :Params:
;       TIME                in, required, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT                  in, optional, type=float, default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;                           
; :Returns:
;       DF                  The difference in frequency between frequency bins.
;
; :Uses:
;   Uses the following external programs::
;       MrMode
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
function dt_fft, time
	compile_opt idl2
	on_error, 2
	
	;calculate the frequency bin size.
	npts = n_elements(time)
	dt = MrMode(time[1:npts-1] - time[0:npts-2])
	
	return, dt
end