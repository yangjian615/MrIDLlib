; docformat = 'rst'
;
; NAME:
;   MRFFTPHASE
;
;+
;   The purpose of this program is to take the 2D results of an FFT created with a sliding
;   window, extract a subset of the time interval at specific frequenies, and calculate
;   the phase of those cuts.
;
; :Categories:
;   FFT
;
; :Params:
;       DATA:           in, required, type=NxM or NxMxL numeric
;                       Results of a sliding FFT (e.g. those returned by MrFFT). Its
;                           leading 2 dimensions are associated with [time, frequency].
;       FREQS:          in, optional, type=fltarr(N)
;                       The frequencies associated with dimension 1 of `DATA`. FREQS is
;                           required if `F` is provided. It also must be monotonically
;                           increasing or decreasing.
;       F:              in, optional, type=fltarr
;                       The frequencies at which the phase is to be calculated. If provided,
;                           `FREQS` must also be given.
;       TIME:           in, optional, type=float(M)
;                       The time tags associated with dimension 2 of `DATA`. TIME is
;                           required if `TRANGE` is provided.
;       TRANGE:         in, optional, type=fltarr(2)
;                       A range of times falling within `TIME` from which the phase cuts
;                           are to to be made. If provided, then `TIME` is required.
;
; :Returns:
;       PHASE:          The phase of `DATA` at the given `F` and `TRANGE`. If `F` and
;                           `TRANGE` are not provided, or they lie beyond the range of
;                           `FREQS` and `TIME`, then the phase of all points in `DATA`
;                           will be returned.
;                       
;
; :Uses:
;   Uses the following external programs::
;       getindexrange.pro
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
;       09/22/2013  -   Written by Matthew Argall
;       09/30/2013  -   Properly returns the subset of
;-
function MrFFTPhase, data, freqs, f, time, tRange
    compile_opt idl2
    on_error, 2
    
    dims = size(data, /DIMENSIONS)
    nFreqs = n_elements(freqs)
    nTime = n_elements(time)

    ;Get the subset of time
    if (nTime ne 0) and (n_elements(tRange) ne 0) then begin
        if nTime ne dims[0] then message, 'TIME and 1st dimension of DATA must have the same size.'
        itRange = getIndexRange(time, tRange)
    endif

    ;Get the subset of frequencies
    if (nFreqs ne 0) and (n_elements(f) ne 0) then begin
        if nFreqs ne dims[1] then message, 'FREQS and 2nd dimension of DATA must have the same size.'
        ifreqs = value_locate(freqs, f)
        
        ;Instead of rounding down, take the closest value
        ibump = where(abs(f - freqs[ifreqs]) lt abs(f - freqs[ifreqs+1]), nBump)
        if nBump gt 0 then ifreqs[iBump] += 1
    endif

    ;Calculate the phase
    phase = atan(data, /PHASE)
    
    ;Return the subset of PHASE
    if n_elements(itRange) eq 0 and n_elements(ifreqs) eq 0 $
        then return, phase $
        else if n_elements(itRange) eq 0 then return, phase[*, ifreqs, *] $
        else return, phase[itRange[0]:itRange[1], ifreqs, *]
end