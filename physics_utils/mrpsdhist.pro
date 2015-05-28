; docformat = 'rst'
;
; NAME:
;       MrPSDHist
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may  be used to endorse or promote products derived from this     ;
;         software without specific prior written permission.                            ;
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
;   Read fluxgate quicklook data.
;
; :Categories:
;   MMS, DFG, AFG
;
; :Params:
;       DATA:               in, required, type=numeric array
;                           Data for which the power spectral density is to be histogrammed.
;       NFFT:               in, required, type=integer
;                           Number of points per PSD interval.
;       SAMPLE_RATE:        in, required, type=float
;                           Sample rate at which data was acquired.
;       NSHIFT:             in, required, type=integer
;                           Number of points to shift between PSD intervals.
;
; :Keywords:
;       DIMENSION:          in, optional, type=integer, default=0
;                           Dimension over which to compute the PSD.
;       NBINS:              in, optional, type=integer, default=100
;                           Number of bins in which to subdivide the power.
;       FREQUENCY:          out, optional, type=fltarr
;                           Frequencies at which the PSD was computed.
;       POWER:              out, optional, type=fltarr
;                           Power of each of the histogrammed bins.
;       PEAK_POWER:         out, optional, type=fltarr
;                           Power level of the histogram bin with the most counts.
;
; :Returns:
;       HCOUNT:             Number of occurrences of each `POWER` in each `FREQUENCY` bin.
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
;       2015/05/19  -   Written by Matthew Argall
;-
function MrPSDHist, data, nfft, sample_rate, nshift, $
DIMENSION=dimension, $
NBINS=nbins, $
FREQUENCY=frequencies, $
POWER=power, $
PEAK_POWER=peak_power
	compile_opt idl2
	on_error, 2
	
	if n_elements(dimension) eq 0 then dimension = 0
	if n_elements(nbins)     eq 0 then nbins     = 100
	if n_elements(nshift)    eq 0 then nshift    = long(nfft / 2)
	
;-----------------------------------------------------
; FFT Parameters \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check dimensions
	if dimension ne 0 then begin
		;Cannot have more than 2 dimensions
		sz    = size(data)
		ndims = sz[0]
		dims  = ndims eq 0 ? 0 : sz[1:sz[0]]
		if ndims gt 2 then message, 'DATA must have 2 or fewer dimensions.'
		
		;How many "components" were given?
		nComps = dimension eq 1 ? dims[1] : dims[0]
	endif else nComps = 1

	;Compute the sample interval and frequency resolution
	dt = 1.0 / sample_rate
	df = 1.0 / (nfft * dt)

;-----------------------------------------------------
; Compute PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	psd = MrPSD(data, nfft, dt, noverlap, $
	            DIMENSION   = dimension, $
	            FREQUENCIES = frequencies, $
	            _REF_EXTRA  = extra)
	
	;Log-scale the power spectra
	psd = MrLog(psd)

;-----------------------------------------------------
; Histogram Power \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of frequencies
	nFreqs = n_elements(frequencies)
	
	;Deterimine histogram bin sizes
	iFinite  = where(finite(psd))
	pmin     = min(psd[iFinite])
	pmax     = max(psd[iFinite])
	binsize  = (pmax - pmin) / (nbins - 1)
	
	;Power levels of the bins
	power      = MrMake_Array(nbins, START=pmin, INCREMENT=binsize)
	peak_power = fltarr(3, nFreqs)

	;Allocate memory
	if nComps gt 0 $
		then hcount = fltarr( nbins, nFreqs, nComps ) $
		else hcount = fltarr( nbins, nFreqs )

	;Histogram
	for jj = 0, nComps - 1 do begin
		for ii = 0,  nFreqs - 1 do begin
			hcount[*, ii, jj] = cgHistogram(psd[*, ii, jj], $
			                                MIN     = pmin, $
			                                MAX     = pmax, $
			                                BINSIZE = binsize)
		endfor
	endfor

;-----------------------------------------------------
; Median Frequency \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; We want the psd bin with the median number of counts. We can then
	; get the median psd from the POWER array.
	;

	;Find the maximum power
	peak_power = fltarr(nComps, nFreqs)
	for ii = 0, nComps - 1 do begin
		;Index of median occurrence of power
		!Null    = max(hcount[*,*,ii], ix_floor, DIMENSION=1)
		ix_floor = array_indices([nbins, nfreqs], ix_floor, /DIMENSIONS)

		;Median power
		peak_power[ii,*] = power[ix_floor[0,*]]
	endfor

	;Reorder as [frequency, nbins, nComps]
	if nComps eq 1 $
		then hcount = transpose(hcount) $
		else hcount = transpose(hcount, [1,0,2])
	
	return, hcount
end
