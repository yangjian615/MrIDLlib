;+
; NAME:
;       DT_FFT
;
; PURPOSE:
;
;       Compute the sampling interval of a dataset.
;
; :Categories:
;       FFT Utility
;
; :Params:
;       TIME                in, required, type=numeric array
;                           Array of times.
;                           
; :Returns:
;       DT                  The sampling interval.
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
;       Written by:     written by Matthew Argall
;-
function dt_fft, time
	compile_opt idl2
	on_error, 2 

    mode = keyword_set(mode)
    
    ;Use the mode.
    if mode then begin
        npts = n_elements(time)
        dt = MrMode(time[1:npts-1] - time[0:npts-2])
    
    ;Use the average time between samples.
    endif else begin

        ;Calculate the mean different between points.
        dt = moment(time[1:-1] - time[0:-2], MAXMOMENT=1, SDEV=sdev, _STRICT_EXTRA=extra)
        dt = dt[0]
    
        ;Make sure the standard deviation is not of the same order as dt.
        if sdev ge 0.1*dt then $
            message, 'Standard deviation is > 10% of the sample period.', /INFORMATIONAL
    endelse
    
    return, dt
end