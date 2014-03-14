; docformat = 'rst'
;
; NAME:
;   MRFFTPHASE
;
;+
;   The purpose of this program is to take the 2D results of an FFT created with a sliding
;   window, extract a subset of the frequency interval at specific times, and return the
;   power along those cuts.
;
; :Categories:
;   FFT
;
; :Params:
;       DATA:           in, required, type=NxMxL numeric
;                       The power of an FFT computed with a sliding window
;                           (c.f. MrSpectrogram). leading dimension is associated with
;                           [time, frequency].
;       FREQS:          in, optional, type=fltarr(N)
;                       The frequencies associated with dimension 1 of `DATA`. FREQS is
;                           required if `FRANGE` is provided.
;       TIME:           in, optional, type=float(M)
;                       The time tags associated with dimension 2 of `DATA`. TIME is
;                           required if `TRANGE` is provided.
;       T:              in, optional, type=fltarr(2)
;                       A set of times falling within `TIME` from which the power cuts
;                           are to to be made. If provided, then `TIME` is required.
;       FRANGE:         in, optional, type=fltarr
;                       The range of frequencies over which the power is to be returned.
;                           If provided, `F` must also be given.
;
; :Returns:
;       POWER:          The power of `DATA` at the given `T` and `FRANGE`. If `T` and
;                           `FRANGE` are not provided, or they lie beyond the range of
;                           `TIME` and `FREQS`, then the power of all points in `DATA`
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
;-
function MrFFTPower, data, time, t, freqs, fRange
    compile_opt idl2
    
    dims = size(data, /DIMENSIONS)
    nFreqs = n_elements(freqs)
    nTime = n_elements(time)
    
    ;Get the subset of frequencies
    if (nFreqs ne 0) and (n_elements(fRange) ne 0) then begin
        if nTime ne dims[1] then message, 'FREQS and 2nd dimension of DATA must have the same size.'
        ifRange = getIndexRange(freqs, frange)
    endif
        
    ;Get the subset of frequencies
    if (nTime ne 0) and (n_elements(t) ne 0) then begin
        if nTime ne dims[0] then message, 'FREQS and 1st dimension of DATA must have the same size.'
        itRange = value_locate(time, t)
        
        ;Make sure none were out of range
        ibump = where(itRange eq -1, nbump)
        if nbump ne 0 then itRange[ibump] += 1
        
        ;Select only uniq indices
        itRange = itRange[uniq(itRange, sort(itRange))]
    endif
    
    ;Extract the subset of DATA
    if (n_elements(itRange) ne 0) and (n_elements(ifrange) ne 0) $
        then power = data $
        else if n_elements(ifRange) eq 0 then power = data[*, itRange, *] $
        else power = data[itRange, ifRange, *]
    
    return, power
end