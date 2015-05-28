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
;   Make a plot of histogrammed power spectral density.
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
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the graphic will be added to the current MrGraphics
;                               window.
;       NBINS:              in, optional, type=integer, default=100
;                           Number of bins in which to subdivide the power.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrImage
;
; :Returns:
;       GIM:                A MrImage graphics object containing the plot.
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
function MrPSDHistPlot, data, nfft, sample_rate, nshift, $
CURRENT=current, $
NBINS=nbins, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; FFT Parameters \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Compute the sample rate
	hcount = MrPSDHist(data, nfft, sample_rate, nshift, $
	                   NBINS      = nbins, $
	                   FREQUENCY  = frequency, $
	                   POWER      = power, $
	                   PEAK_POWER = peak_power)

;-----------------------------------------------------
; Plot Histogram \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Make a window if one does not exist -- make room for the colorbar
	if ~keyword_set(current) then win = MrWindow(YGAP=0, OXMARGIN=[10, 12])
	
	;Plot
	gIm = MrImage(hcount, frequency, power, $
	              /AXES, $
	              /CURRENT, $
	              /SCALE, $
	              CTINDEX       = 13, $
	              MISSING_COLOR = 'white', $
	              MISSING_VALUE = 0, $
	              YTITLE        = 'Power!C(Log$\down10$)', $
	              _STRICT_EXTRA = extra)

	;Overplot the median power at each frequency
	gOPlot = MrPlot(frequency, peak_power, $
	                OVERPLOT = gIm, $
	                COLOR    = 'magenta')
	
	;Colorbar
	gCB = MrColorbar(TARGET = gIm, $
	                 TITLE  = 'Counts')

	return, gIm
end
