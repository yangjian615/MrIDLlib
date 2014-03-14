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
;       The purpose of this program is to generate a signal by taking a linear combination
;       of sines and cosines according to::
;
;           x(t)  = c + \sum_{j=1}^\left\lfloor N/2 \right\rfloor \left[ A_{j} \cos { \left(2 \pi f_{j} t\right)} + B_{j} \sin { \left(2 \pi f_{j} t\right)}  \right] 
;
;       NOTE::
;           Phase shifts are not applied to square waves.
;
; :Categories:
;       Math Utilities
;
; :Params:
;       F:              in, out, optional, type=int
;                       The frequencies to use when generating `SIGNAL`.
;       A:              in, optional, type=numeric, default="randu(1, n_elements(T)"
;                       The scaling coefficients of the even sinusoid components. If `F`
;                           has "nt" number of points, then the following rules apply to 
;                           A, depending on how many elements it has::
;                               0:  A = randu(1, nt) -- random coefficients with seed 1
;                               nt: A = A            -- A are the coefficients
;                               1:  A = randu(A, nt) -- random coefficients with seed A
;       B:              in, optional, type=numeric, default="randu(1, n_elements(T)"
;                       The scaling coefficients of the odd sinusoid components. If `F`
;                           has "nt" number of points, then the following rules apply to 
;                           B, depending on how many elements it has::
;                               0:  `A` * exp(i2pi`F`) will be used. (not `A`cos + i`B`sin)
;                               nt: B = B            -- B are the coefficients
;                               1:  B = randu(B, nt) -- random coefficients with seed B
;
; :Keywords:
;       DC:             in, optional, type=int, default=0
;                       The DC offset of the resulting sinusoid.
;       N_PERIODS:      in, optional, type=10
;                       Number of periods the output signal should have.
;       NOISE_LEVEL:    in, optional, type=int
;                       A fraction of the Min([`A`, `B`]) amplitude to be used for the
;                           noise. If not provided, no noise will be added to the signal.
;       NOISE_SEED:     in, optional, type=int, default=1
;                       The seed supplied to RandomU() when generating noise.
;       NORMALIZE:      in, optional, type=boolean, default=0
;                       Normalize the resulting wave amplitude to 1.
;       PERIOD:         in, optional, type=fload, default=1.0/min(`F`)
;                       The number of seconds of each period.
;       PHASE_A:        in, optional, type=numeric, default=0.0
;                       The phase of the "in-phase" component of the signal.
;       PHASE_B:        in, optional, type=numeric, default=0.0
;                       The phase of the "in-quadrature" component of the signal.
;       SAMPLE_RATE:    in, optional, type=float, default=max(`F`)*2*100
;                       Samples per seconds. Default is 200*Nyquist frequency
;       SQUARE:         in, optional, type=boolean, default=0
;                       Indicate that a square wave is to be made.
;       TIME:           out, optional, type=fltarr()
;                       The time stamps, in seconds, of the output signal.
;       
;
; :Returns:
;       SIGNAL:         The signal made by the linear combination of sines and cosines
;
; :Uses:
;   Uses the following external functions::
;       linspace.pro
;       normalize.pro
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
;-
function mrSigGen, f, A, B, $
DC = DC, $
N_PERIODS = n_periods, $
NOISE_LEVEL = noise_level, $
NOISE_SEED = noise_seed, $
NORMALIZE = normalize, $
PERIOD = period, $
PHASE_A = phase_a, $
PHASE_B = phase_b, $
REAL = real, $
SAMPLE_RATE = sample_rate, $
SQUARE = square, $
TIME = time
    compile_opt idl2
;    on_error, 2
    
;-----------------------------------------------------
;CHECK INPUTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if min(f) eq 0 then message, 'f=0 must be specified with DC keyword.'
    nf = n_elements(f)

    ;If no DC component was specified, make it zero.
    if n_elements(DC) eq 0 then DC = 0
    if n_elements(period) eq 0 then period = 1.0 / min(f)
    if n_elements(n_periods) eq 0 then n_periods = 10
    if n_elements(noise_seed) eq 0 then noise_seed = 1
    if n_elements(sample_rate) eq 0 then sample_rate = max(f)*2.0 * 100.0
    if keyword_set(normalize) then A_coeff = normalize(A_coeff)
    if keyword_set(normalize) and n_elements(B_coeff) ne 0 then B_coeff = normalize(B_coeff)
    
;-----------------------------------------------------
;CREATE THE TIME ARRAY \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Generate the times
    dt = 1 / sample_rate
    time = linspace(0, n_periods*period, dt, /INTERVAL)
    npts = n_elements(time)
    nt = npts
    
;-----------------------------------------------------
;COEFFICIENTS AND PHASE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Get the coefficients for the cosines. If A is a scalar, then it is the randomu seed.
    case n_elements(A) of
        0:     A_coeff = randomu(1, nt, nf)
        1:     A_coeff = replicate(A, nt, nf)
        nf:    A_coeff = rebin(transpose(A), nt, nf)
        nt:    A_coeff = rebin(A, nt, nf)
        nf*nt: A_coeff = A
        else: message, 'A: Incorrect number of elements.'
    endcase

    ;Get the coefficients for the sines. If B is a scalar, then it is the randu seed.
    case n_elements(B) of
        0:     if keyword_set(real) then B_coeff = randomu(1, nt, nf)
        1:     B_coeff = replicate(B, nt, nf)
        nf:    B_coeff = rebin(transpose(B), nt, nf)
        nt:    B_coeff = rebin(B, nt, nf)
        nf*nt: B_coeff = B
        else: message, 'B: Incorrect number of elements.'
    endcase
    
    ;Set the phase for the cosines. If PHASE_A is a scalar, then it is the randomu seed
    case n_elements(phase_A) of
        0:     phi_A = randomu(1, nt, nf)
        1:     phi_A = replicate(phase_A, nt, nf)
        nf:    phi_A = rebin(transpose(phase_A), nt, nf)
        nt:    phi_A = rebin(phase_A, nt, nf)
        nf*nt: phi_A = phase_A
        else: message, 'PHASE_A: Incorrect number of elements.'
    endcase
    
    ;Set the phase for the cosines. If PHASE_B is a scalar, then it is the randomu seed
    case n_elements(phase_B) of
        0:     if keyword_set(real) then phi_B = randomu(1, nt, nf)
        1:     phi_B = replicate(phase_B, nt, nf)
        nf:    phi_B = rebin(transpose(phase_B), nt, nf)
        nt:    phi_B = rebin(phase_B, nt, nf)
        nf*nt: phi_B = phase_B
        else: message, 'PHASE_B: Incorrect number of elements.'
    endcase
    
;-----------------------------------------------------
;MAKE A SQUARE WAVE? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if keyword_set(square) then begin
        ;allocate memory to the signal
        signal = fltarr(npts)
        sig_temp = fltarr(npts)
        
        ;The last half of each period will be 1. The first half will be 0.
        for i = 0, nf - 1 do begin
    
            ;Generate a signal the length of out time interval, but with different numbers
            ;of wavelengths to accomodate
            period = 1.0 / f[i]
            n_periods = floor(time[-1] / period)
            nT = period / dt
            nT_half = period / (2*dt)
            
            ;create the square wave by making 
            ;   [0, T/2) = 0 
            ;   [T/2, T) = A_coeff[i]
            for j = 0, n_periods-1 do sig_temp[j*nT+nT_half:j*nT+nT-1] = A_coeff[*,i]
            if npts - ((j-1)*nT+nT-1) gt nT_half then sig_temp[j*nT+nT_half:npts-1] = A_coeff[*,i]

            ;Sum the components.
            signal += sig_temp
        endfor
        
        if DC ne 0 then signal += DC
        return, signal
    endif
    
;-----------------------------------------------------
;MAKE SINUSOIDAL SIGNALS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;-----------------------------------------------------
    ;USE SINES AND COSINES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
    if n_elements(B) gt 0 then begin
        signal = fltarr(npts)
            
        ;Step through each frequency
        for i = 0, nf-1 do begin
            ;sum the contributions from each frequency
            signal += A_coeff[*,i] * cos(2*!pi*f[i]*time + phi_A[*,i]) + $
                      B_coeff[*,i] * sin(2*!pi*f[i]*time + phi_B[*,i])
        endfor
        
    ;-----------------------------------------------------
    ;USE COMPLEX EXPONENTIALS \\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
    endif else begin
        signal = complexarr(npts)
        
        ;Step through each frequency
        for i = 0, nf-1 do begin
            ;sum the contributions from each frequency
            signal += A_coeff[*,i]*exp(complex(0,phi_A[*,i])) * exp(complex(fltarr(npts), 2*!pi*f[i]*time))
        endfor
    endelse   
    
    signal += DC   
    
;-----------------------------------------------------
;ADD NOISE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Add noise to the signal at 10% of the minimum signal amplitude.
    if n_elements(noise_level) ne 0 then begin
        min_amplitude = min(A)
        if n_elements(B) ne 0 then min_amplitude = min([min_amplitude])
        signal += randomu(noise_seed, size(signal, /DIMENSIONS)) * noise_level * min_amplitude
    endif
    
    return, signal
end