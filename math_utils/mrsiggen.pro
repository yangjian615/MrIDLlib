; docformat = 'rst'
;
; NAME:
;       mrSigGen
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
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
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
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
;       Generate a time-series signal via a linear comination of complex exponentials::
;
;           x(t)  = c + \sum_{j=1}^{\left\lfloor N/2 \right\rfloor} \left[ A_{j} e^{i \left(2 \pi f_{j} + \phi \right) }
;
;       or sinusoids::
;
;           x(t)  = c + \sum_{j=1}^{\left\lfloor N/2 \right\rfloor} \left[ A_{j} \cos { \left(2 \pi f_{j} t\right)} + B_{j} \sin { \left(2 \pi f_{j} t\right)}  \right] 
;
;       or a square wave::
;
;           x(t) = { 0    for [0,   T/2)
;                  { 1    for [T/2,   T)
;                   
;
;       NOTE::
;           Phase shifts are not applied to square waves.
;
; :Examples:
;   A DC wave with sampling rate of 1Hz and duration 60s::
;       x = MrSigGen(TIME=y)
;       plot, x, y, TITLE='DC Signal', XTITLE='Time (s)', YRANGE=[-1,1], YTITLE='Amplitude'
;
;   A DC wave with amplitude 0.5 sampled at 10Hz for 60s::
;       y = MrSigGen(10, 60, DC=0.5, TIME=x)
;       plot, x, y, TITLE='DC Signal', XTITLE='Time (s)', YRANGE=[-1,1], YTITLE='Amplitude'
;
;       y = MrSigGen(10, 60, FREQUENCIES=0.0, AMP_A=0.5, TIME=x)
;       plot, x, y, TITLE='DC Signal', XTITLE='Time (s)', YRANGE=[-1,1], YTITLE='Amplitude'
;
;   A 0.25Hz wave, sampled 100 times the nyquist frequency (fN = 0.5Hz) for 10 periods::
;       y = MrSigGen(FREQUENCIES=0.25, TIME=x)
;       plot, x, y, TITLE='0.25Hz Wave', XTITLE='Time (s)', YRANGE=[-1,1], YTITLE='Amplitude'
;
;   A wave with two frequency components, each with their own amplitude and phase::
;       y = MrSigGen(30, 16, FREQUENCIES=[0.25, 1.5], AMP_A=[1.0, 0.25], PHASE_A=[-90, 45], /ANGLES, TIME=x)
;       plot, x, y, TITLE='Mulit-Component Wave', XTITLE='Time (s)', YRANGE=[-1,1], YTITLE='Amplitude'
;
;   Add noise to the last example::
;       y = MrSigGen(30, 16, FREQUENCIES=[0.25, 1.5], AMP_A=[1.0, 0.25], PHASE_A=[-90, 45], /ANGLES, NOISE_LEVEL=0.8, TIME=x)
;       plot, x, y, TITLE='Mulit-Component Wave with Noise', XTITLE='Time (s)', YRANGE=[-1,1], YTITLE='Amplitude'
;
; :Categories:
;       Math Utilities
;
; :Params:
;       SAMPLE_RATE:    in, optional, type=float, default=1.0
;                       Number of samples per unit time. If `FREQUENCIES` is given, the
;                           defualt is 100 * (2*max(`FREQUENCIES`) -- 100 * the nyquist
;                           frequency.
;       DURATION:       in, optional, type=float, default=10.0
;                       Length of the sampling interval in units of time. If `FREQUENCIES`
;                           is given, the defualt is 10 * 1 / min(`FREQUENCIES`) --
;                           ten times the longest wave period.
;
; :Keywords:
;       AMP_A:          in, optional, type=numeric, default="randu(1, n_elements(T)"
;                       The scaling coefficients of the even sinusoid components. It can
;                           have the following number of elements ("nf" and "nt" are the
;                           number of elements in `FREQUENCIES` and `TIME`)
;                               0:     1.0 or if `SEED` is provided, an nf-element array of random coeffiecients.
;                               1:     A single time-independent coefficient used for all frequencies.
;                               nf:    A unique, time-independent coefficient for each frequency.
;                               nt:    A time-dependent coefficient used for all frequencies.
;                               nt*nf: A time-dependent coeffieicent for each frequency.
;       AMP_B:          in, optional, type=numeric, default="randu(1, n_elements(T)"
;                       The scaling coefficients of the odd sinusoid components. If `F`
;                           has "nf" number of points, then the following rules apply to 
;                           B, depending on how many elements it has::
;                               0:     Left undefined unless `REAL` is set, then 1.0 or
;                                       if `SEED` is provided, an nf-element array of random coeffiecients.
;                               1:     A single time-independent coefficient used for all frequencies.
;                               nf:    A unique, time-independent coefficient for each frequency.
;                               nt:    A time-dependent coefficient used for all frequencies.
;                               nt*nf: A time-dependent coeffieicent for each frequency.
;                           If provided, the signal will be created using real sines and
;                           cosines instead of complex exponentials.
;       DC:             in, optional, type=int, default=0
;                       The DC offset of the resulting sinusoid.
;       DEGREES:        in, optional, type=int, default=0
;                       If set, `PHASE_A` and `PHASE_B` have units of degrees, not radians.
;       FREQUENCIES:    in, optional, type=int, default=0.0
;                       The frequencies to use when generating `SIGNAL`. A frequency
;                           of 0Hz is equivalent to setting `DC`=AMP_A.
;       NOISE_LEVEL:    in, optional, type=int, default=0.0
;                       A fraction of the Min([`A`, `B`]) amplitude to be used for the
;                           noise. If not provided, no noise will be added to the signal.
;       NOISE_SEED:     in, optional, type=int, default=1
;                       The seed supplied to RandomU() when generating noise.
;       NORMALIZE:      in, optional, type=boolean, default=0
;                       Normalize the resulting wave amplitude to 1.
;       NPERIODS:       in, optional, type=integer
;                       A positive integer indicating the number of periods of the longest
;                           period wave to sample. If `SAMPLE_RATE` is lower than the
;                           Nyquist frequency, it will be increased to 2.0*max(`FREQUENCIES`).
;                           `DURATION` will be set equal to NPERIODS / min(`FREQUENCIES`)
;       PHASE_A:        in, optional, type=numeric, default=0.0
;                       The phase of the "in-phase" component of the signal.
;       PHASE_B:        in, optional, type=numeric, default=0.0
;                       The phase of the "in-quadrature" component of the signal.
;       REAL:           in, optional, type=boolean, default=0
;                       If set, use real sines and cosines to generate the signal.
;       SAMPLE_RATE:    in, optional, type=float, default=max(`F`)*2*100
;                       Samples per seconds. Default is 200*Nyquist frequency
;       SEED:           in, optional, type=integer, default=1
;                       The seed to use when generating random coefficients and phases.
;       SQUARE:         in, optional, type=boolean, default=0
;                       Indicate that a square wave is to be made.
;       TIME:           out, optional, type=fltarr()
;                       The time stamps, in seconds, of the output signal.
;
; :Returns:
;       SIGNAL:         The time-series signal.
;
; :Uses:
;   Uses the following external functions::
;       MrMake_Array.pro
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
;
;       02/22/2013  -   Written by Matthew Argall
;       03/18/2013  -   Added PHASE_A and PHASE_B. Output X renamed to SIGNAL. - MRA
;       03/20/2013  -   Square waves now created properly. - MRA
;       09/04/2013  -   Phase and amplitude can be specified for all points in time and
;                           for each frequency. - MRA
;       09/13/2013  -   Added the NOISE_LEVEL and NOISE_SEED keywords. - MRA
;       09/13/2013  -   Added the SEED keyword. - MRA
;       2015/03/02  -   F, A, B parmeters renamed to FREQUENCIES, AMP_A, and AMP_B and
;                           are now keywords. SAMPLE_RATE and DURATION are now the inputs.
;                           PERIODS is no longer accepted. N_PERIODS was renamed to
;                           NPERIODS and repurposed. SEED is used only if provided and
;                           does not restart the seed sequence when used. Now a purely DC
;                           signal can be created with the DC keyword. - MRA
;       2015/04/22  -   ANGLES keyword renamed to DEGREES and documented. - MRA
;-
function mrSigGen, sample_rate, duration, $
DC = DC, $
FREQUENCIES = f, $
AMP_A = amp_a, $
AMP_B = amp_b, $
DEGREES = degrees, $
NOISE_LEVEL = noise_level, $
NOISE_SEED = noise_seed, $
NORMALIZE = normalize, $
NPERIODS = nPeriods, $
PHASE_A = phase_a, $
PHASE_B = phase_b, $
REAL = real, $
SEED = seed, $
SQUARE = square, $
TIME = time
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
;CHECK INPUTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    nf = n_elements(f)

	;Sample rate
	;   - Sample at 100 times the nyquist frequency.
	if n_elements(sample_rate) eq 0 then begin
		if nf gt 0 $
			then sample_rate = max(f)*2.0 * 100.0 $
			else sample_rate = 1.0
	endif
	
	;Duration
	;   - Sample for 10 of the longest wave periods, or for 60 time units.
	if n_elements(duration) eq 0 then begin
		if nf gt 0 $
			then duration = 10.0 / min(f) $
			else duration = 60.0
	endif
	
	;Number of periods
	if n_elements(nPeriods) gt 0 then begin
		if nPeriods le 0 then message, 'NPERIODS must be a positive integer.'
		if nf eq 0 || max(f eq 0.0) eq 1 then message, 'To use NPERIODS, FREQUENCIES must be defined and must not contain 0.0.'
		sample_rate >= max(f)*2.0
		duration     = nPeriods / min(f)
	endif

	;Other defaults.
	nSeed     = n_elements(seed)
	degrees   = keyword_set(degrees)
	normalize = keyword_set(normalize)
	real      = keyword_set(real)
	square    = keyword_set(square)
	if n_elements(DC)          eq 0 then DC          = 0
	if n_elements(noise_level) eq 0 then noise_level = 0.0
	
	;0-Hz frequency
	if nf eq 0 then begin
		frequencies = 0.0
		nf = 1
	endif

;-----------------------------------------------------
; Time Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	time = MrMake_Array(START=0, INCREMENT=1.0/sample_rate, LAST=duration)
	nt   = n_elements(time)

;-----------------------------------------------------
; Coefficients \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Coefficients for the cosines.
	case n_elements(amp_A) of
		0:     A_coeff = nSeed eq 0 ? replicate(1.0, nt, nf) : rebin(randomu(seed, 1, nf), nt, nf)
		1:     A_coeff = replicate(amp_A, nt, nf)
		nf:    A_coeff = rebin(transpose(amp_A), nt, nf)
		nt:    A_coeff = rebin(amp_A, nt, nf)
		nf*nt: A_coeff = amp_A
		else: message, 'A: Incorrect number of elements.'
	endcase

	;Coefficients for the sines.
	case n_elements(amp_B) of
		0:     if keyword_set(real) then B_coeff = nSeed eq 0 ? replicate(1.0, nt, nf) : rebin(randomu(seed, 1, nf), nt, nf)
		1:     B_coeff = replicate(amp_B, nt, nf)
		nf:    B_coeff = rebin(transpose(amp_BB), nt, nf)
		nt:    B_coeff = rebin(amp_B, nt, nf)
		nf*nt: B_coeff = amp_B
		else: message, 'B: Incorrect number of elements.'
	endcase

;-----------------------------------------------------
; Normalize Coefficients \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if normalize then begin
		A_coeff /= sqrt(total(A_coeff^2))
		if n_elements(B_coeff) gt 0 then B_coeff /= sqrt(total(B_coeff^2))
	endif

;-----------------------------------------------------
; Phase \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Phase for the cosines.
	case n_elements(phase_A) of
		0:     phi_A = nSeed eq 0 ? replicate(0.0, nt, nf) : rebin(randomu(seed, 1, nf), nt, nf)
		1:     phi_A = replicate(phase_A, nt, nf)
		nf:    phi_A = rebin(transpose(phase_A), nt, nf)
		nt:    phi_A = rebin(phase_A, nt, nf)
		nf*nt: phi_A = phase_A
		else: message, 'PHASE_A: Incorrect number of elements.'
	endcase

	;Phase for the cosines.
	case n_elements(phase_B) of
		0:     if keyword_set(real) then phi_B = nSeed eq 0 ? replicate(0.0, nt, nf) : rebin(randomu(seed, 1, nf), nt, nf)
		1:     phi_B = replicate(phase_B, nt, nf)
		nf:    phi_B = rebin(transpose(phase_B), nt, nf)
		nt:    phi_B = rebin(phase_B, nt, nf)
		nf*nt: phi_B = phase_B
		else: message, 'PHASE_B: Incorrect number of elements.'
	endcase
	
	;Convert to radians.
	if degrees then begin
		phi_A *= !dtor
		if n_elements(phi_b) gt 0 then phi_B *= !dtor
	endif

;-----------------------------------------------------
; Square Wave \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if square then begin
		;allocate memory to the signal
		signal   = fltarr(nt)
		sig_temp = fltarr(nt)
	
		;The last half of each period will be 1. The first half will be 0.
		for i = 0, nf - 1 do begin

			;Generate a signal the length of out time interval, but with different numbers
			;of wavelengths to accomodate
			period    = 1.0 / f[i]
			n_periods = floor(time[-1] / period)
			nT        = period / dt
			nT_half   = period / (2*dt)
		
			;Create the square wave by making 
			;   - [0, T/2) = 0 
			;   - [T/2, T) = A_coeff[i]
			;   - Only need to fill the latter half of the array.
			for j = 0, n_periods-1 do sig_temp[j*nT+nT_half:j*nT+nT-1] = A_coeff[*,i]
			
			;Fill the remainder of the array on the high end.
			if npts - ((j-1)*nT+nT-1) gt nT_half then sig_temp[j*nT+nT_half:npts-1] = A_coeff[*,i]

			;Sum the components.
			signal += sig_temp
		endfor

;-----------------------------------------------------
; Sinusoidal Wave \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if n_elements(B) gt 0 then begin
	;-----------------------------------------------------
	; Sines and Cosines \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		signal = fltarr(nt)
		
		;Step through each frequency
		for i = 0, nf-1 do begin
			;At each frequency, accumulate the results of
			;   s = A*cos(w*t + phi) + B*sin(w*t + phi)
			signal += A_coeff[*,i] * cos(2*!pi*f[i]*time + phi_A[*,i]) + $
			          B_coeff[*,i] * sin(2*!pi*f[i]*time + phi_B[*,i])
		endfor
	
	;-----------------------------------------------------
	; Complex Exponentials \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
	endif else begin
		signal = complexarr(nt)
	
		;Step through each frequency
		for i = 0, nf-1 do begin
			;At each frequency, accumulate the results of
			;   s = A * exp(i*phi) * exp(i*w*t)
			signal += A_coeff[*,i]*exp(complex(0,phi_A[*,i])) * exp(complex(fltarr(nt), 2*!pi*f[i]*time))
		endfor
		
		;Only a DC signal?
		if nf eq 1 && f[0] eq 0.0 then signal = real_part(signal)
	endelse

;-----------------------------------------------------
; DC Component \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if DC ne 0.0 then signal += DC

;-----------------------------------------------------
; Noise \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Add noise to the signal at 10% of the minimum signal amplitude.
	if noise_level ne 0.0 then begin
		min_amplitude = min(A_coeff)
		if n_elements(B) ne 0 then min_amplitude = min([min_amplitude])
		signal += 2.0 * (randomu(noise_seed, nt) - 0.5) * noise_level * min_amplitude
	endif
	
	;Return the seed initialization sequence
	if nSeed gt 0 then seed = theSeed

	return, signal
end