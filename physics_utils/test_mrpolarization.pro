; docformat = 'rst'
;
; NAME:
;
;       TEST_MRPOLARIZATION
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
;   The purpose of this program is to test the MrPolarization routines to see if it is
;   performing correctly.
;
; :Categories:
;
;       Test Program
;
; :Uses:
;   Uses the following external programs::
;       MrWindow.pro
;       MrSigGen.pro
;       MrPolarization.pro
;       MrFFT.pro
;       MrLog.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History::
;   Modification History::
;       09/12/2013  -   Written by Matthew Argall
;-
function test_mrpolarization
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(MyWin)  then obj_destroy, MyWin
        if obj_valid(polWin) then obj_destroy, polWin
        void = cgErrorMsg()
        return, obj_new()
    endif

;-----------------------------------------------------
;Create the Signal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;
    ;The signal will be made in 3 phases, and will have x-, y-, and z-components. The z-
    ;component will be zero. The phase of the y-component will begin 90 degrees ahead of
    ;the x-component, then transition to 90 degrees behind the x-component.
    ;

    ;Frequencies, Amplitudes, and Phases
    f = [0.1, 0.3, 0.45, 1.0, 1.23, 1.6, 2.1, 3.0]
    sr = 64.0       ;Sample rate
    nl = 1.0        ;Noise level
    nT = 100        ;Number of periods to generate
    Ax = 1.0        ;Amplitude of Bx
    Ay = 1.0        ;Amplitude of By
    phix = 0        ;Phase of Bx
    phiy = !pi/2.0  ;Phase of By
    
    ;First leg of the signal
    Hx1 = MrSigGen(f, Ax, Ax, PHASE_A=phix, PHASE_B=phix, SAMPLE_RATE=sr, N_PERIODS=nT, NOISE_LEVEL=nl, TIME=t1)
    Hy1 = MrSigGen(f, Ay, Ay, PHASE_A=phiy, PHASE_B=phiy, SAMPLE_RATE=sr, N_PERIODS=nT, NOISE_LEVEL=nl)

    ;Transition region -- Transition Y from 90 degrees ahead to 90 degrees behind
    npts = n_elements(Hx1)
    to_zero = cos(!pi/2.0*findgen(npts)/(npts-1))^2
    to_one = reverse(to_zero)
    phix = to_one*!pi/2.0
    phiy = to_zero*!pi/2.0
    
    ;Second leg of the signal
    Hx2 = MrSigGen(f, Ax, Ax, PHASE_A=phix, PHASE_B=phix, SAMPLE_RATE=sr, N_PERIODS=nT, NOISE_LEVEL=nl, TIME=t2)
    Hy2 = MrSigGen(f, Ay, Ay, PHASE_A=phiy, PHASE_B=phiy, SAMPLE_RATE=sr, N_PERIODS=nT, NOISE_LEVEL=nl)
    t2 += t1[1] + t1[-1]

    ;X-component is 90 degrees ahead in phase
    phix = !pi/2.0
    phiy = 0.0
    
    ;Final leg of the signal
    Hx3 = MrSigGen(f, Ax, Ax, PHASE_A=phix, PHASE_B=phix, SAMPLE_RATE=sr, N_PERIODS=nT, NOISE_LEVEL=nl, TIME=t3)
    Hy3 = MrSigGen(f, Ay, Ay, PHASE_A=phiy, PHASE_B=phiy, SAMPLE_RATE=sr, N_PERIODS=nT, NOISE_LEVEL=nl)
    t3 += t2[-1] + t1[1]

    ;Combine everything together
    t = [temporary(t1), temporary(t2), temporary(t3)]
    H = transpose([[temporary(Hx1), temporary(Hx2), temporary(Hx3)], $
                   [temporary(Hy1), temporary(Hy2), temporary(Hy3)], $
                   [replicate(0, n_elements(t))]])

;-----------------------------------------------------
;Phase and Power Calculations \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;FFT and Power Parameters
    nfft = 10*64*4
    nshift = nfft/2
    dt = 1.0/64.0
    fmin = 0.0
    fmax = 3.5
    
    ;FFT
    test_fft   = MrFFT(H, nfft, dt, nshift, $
                       DIMENSION=2, $
                       FMIN=fmin, $
                       FMAX=fmax, $
                       FREQUENCIES=Ff, $
                       TIME=Ft, $
                       /WINDOW)
                       
    ;Spectrogram
    test_spect = MrSpectrogram(H, nfft, dt, nshift, $
                               DIMENSION=2, $
                               FMAX=fmax, $
                               FREQUENCIES=Sf, $
                               TIME=St, $
                               /WINDOW)
    test_spect = MrLog(test_spect)

    ;Take only the positive frequencies of the FFT and calculate the phase difference
    ;between the x- and y-components.
    fpos = where(Ff ge 0)
    test_fft = test_fft[*,fpos,*]
    Ff = Ff[fpos]
    test_phase = (atan(test_fft[*,*,1], /PHASE) - atan(test_fft[*,*,0], /PHASE)) * !radeg

    ;Index of the frequency whose phase is to be checked.
    void = min(abs(Ff - f[5]), ifreq)

;-----------------------------------------------------
;FFT for Phase Information \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;FFT
    test_fft = MrFFT(H, nfft, dt, nshift, $
                     DIMENSION=2, $
                     FMIN=fmin, $
                     FMAX=fmax, $
                     FREQUENCIES=Ff, $
                     TIME=Ft, $
                     /WINDOW)

    ;Take only the positive frequencies of the FFT and calculate the phase difference
    ;between the x- and y-components.
    fpos = where(Ff ge 0)
    test_fft = test_fft[*,fpos,*]
    Ff = Ff[fpos]
    test_phase = (atan(test_fft[*,*,1], /PHASE) - atan(test_fft[*,*,0], /PHASE)) * !radeg

    ;Index of the frequency whose phase is to be checked.
    void = min(abs(Ff - f[5]), ifreq)

;-----------------------------------------------------
;Polarization Calculations \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Polarization Parameters
    pol = MrPolarization(H, nfft, dt, nshift, $
                         FMIN=fmin, $
                         FMAX=fmax, $
                         DIMENSION=2, $
                         TIME=tPol, $
                         FREQUENCIES=fPol, $
                         KDOTB_ANGLE=angPol, $
                         ELLIPTICITY=ePol, $
                         INTENSITY=IPol, $
                         COHERENCY=CPol, $
                         SPECTRAL_MATRIX=smPol, $
                         WAVE_NORMAL_MATRIX=wnPol)

;-----------------------------------------------------
;Plot Power Spectra Results \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Create a window for viewing the results
    mywin = MrWindow(XSIZE=500, YSIZE=700, YGAP=6, XMARGIN=[10,15], REALIZE=0)

    ;X-component of the signal and its power
    plx = mywin -> Plot(t, H[0,*], $
                        TITLE='Test Data: Bx (One Period)', $
                        XTITLE='Time (s)', $
                        YTITLE='Amplitude', $
                        XRANGE=[0, 10], $
                        DRAW=0)
                        
    imx = mywin -> Image(reform(wnPol[0,0,*,*]), tPol, fPol, $
                         CTINDEX=13, $
                         /AXES, $
                         TITLE='Test Data: Bx Power', $
                         XTITLE='Time (s)', $
                         YTITLE='Frequency!C(Hz)', $
                         DRAW=0)

    ;Y-component of the signal and its power
    ply = mywin -> Plot(t, H[1,*], $
                        TITLE='Test Data: By (One Period)', $
                        XTITLE='Time (s)', $
                        YTITLE='Amplitude', $
                        XRANGE=[0, 10], $
                        DRAW=0)
                        
    imy = mywin -> Image(reform(wnPol[1,1,*,*]), tPol, fPol, $
                         CTINDEX=13, $
                         /AXES, $
                         TITLE='Test Data: By Power', $
                         XTITLE='Time (s)', $
                         YTITLE='Frequency!C(Hz)', $
                         DRAW=0)

    ;Phase difference between X and Y at all frequencies and at a single frequency
    imp = mywin -> Image(test_phase, Ft, Ff, $
                         CTINDEX=13, $
                         /AXES, $
                         TITLE='Test Data: Phase Diff', $
                         XTITLE='Time (s)', $
                         YTITLE='Frequency!C(Hz)', $
                         DRAW=0)
                         
    pla = mywin -> Plot(Ft, test_phase[*,ifreq], $
                        TITLE='Phase Difference (f = 1.6Hz)', $
                        XTITLE='Time (s)', $
                        YTITLE='Phase Diff', $
                        DRAW=0)

    ;Add colorbars to the image data
    cbx = mywin -> Colorbar(GRAPHIC=imx, $
                            RANGE=[min(test_spect[*,*,0], max=tmax), tmax], $
                            CBLOCATION='RIGHT', $
                            /RIGHT, $
                            TLOCATION='RIGHT', $
                            TITLE='Power!C(nT^2 * Hz)', $
                            CTINDEX=13, $
                            DRAW=0)
                            
    cby = mywin -> Colorbar(GRAPHIC=imy, $
                            RANGE=[min(test_spect[*,*,1], max=tmax), tmax], $
                            CBLOCATION='RIGHT', $
                            /RIGHT, $
                            TLOCATION='RIGHT', $
                            TITLE='Power!C(nT^2 * Hz)', $
                            CTINDEX=13, $
                            DRAW=0)
                            
    cbp = mywin -> Colorbar(GRAPHIC=imp, $
                            RANGE=[min(test_phase, max=pmax), pmax], $
                            CBLOCATION='RIGHT', $
                            /RIGHT, $
                            TLOCATION='RIGHT', $
                            TITLE='Phase Diff!C(Deg)', $
                            CTINDEX=13, $
                            DRAW=0)

;-----------------------------------------------------
;Plot Polarization Results \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Create a window for viewing the results
    polWin = MrWindow(XSIZE=500, YSIZE=700, YGAP=6, XMARGIN=[10,15], REALIZE=0)

    ;INTENSITY
    IPol = MrLog(IPol)
    irange = [min(IPol, /NAN, max=imax), imax]
    imInt = polWin -> Image(IPol, tPol, fPol, $
                            CTINDEX=13, $
                            /AXES, $
                            RANGE=irange, $
                            TITLE='Test Data: Intensity', $
                            XTITLE='Time (s)', $
                            YTITLE='Frequency!C(Hz)', $
                            DRAW=0)

    ;POLARIZATION
    prange = [min(pol, /NAN, max=pmax), pmax]
    imPol = polWin -> Image(pol, tPol, fPol, $
                            CTINDEX=0, $
                            /AXES, $
                            RANGE=prange, $
                            TITLE='Test Data: % Polarization', $
                            XTITLE='Time (s)', $
                            YTITLE='Frequency!C(Hz)', $
                            DRAW=0)

    ;Create a color palette for Ellipticity that goes from red -> white -> blue
    ;   [255,0,0] -> [255,255,255] -> [0,0,255]
    red   = [linspace(255, 255, 128), linspace(255, 0,   128)]
    green = [linspace(0,   255, 128), linspace(255, 0,   128)]
    blue  = [linspace(0,   255, 128), linspace(255, 255, 128)]
    MrCT_red_white_blue = [[red], [green], [blue]]

    ;ELLIPTICITY
    erange = [min(ePol, /NAN, max=pmax), pmax]
    imEll = polWin -> Image(ePol, tPol, fPol, $
                            /AXES, $
                            PALETTE=MrCT_red_white_blue, $
                            RANGE=erange, $
                            TITLE='Test Data: Ellipticity', $
                            XTITLE='Time (s)', $
                            YTITLE='Frequency!C(Hz)', $
                            DRAW=0)

    ;ANGLE BETWEEN K AND B
    angPol *= !radeg
    arange = [min(angPol, /NAN, max=pmax), pmax]
    imAng = polWin -> Image(angPol, tPol, fPol, $
                            /AXES, $
                            CTINDEX=0, $
                            RANGE=arange, $
                            TITLE='Test Data: Polarization Angle', $
                            XTITLE='Time (s)', $
                            YTITLE='Frequency!C(Hz)', $
                            DRAW=0)

    ;COHERENCY
    crange = [min(CPol, /NAN, max=pmax), pmax]
    imCoh = polWin -> Image(CPol, tPol, fPol, $
                            /AXES, $
                            CTINDEX=0, $
                            RANGE=[0,1], $
                            TITLE='Test Data: Coherency', $
                            XTITLE='Time (s)', $
                            YTITLE='Frequency!C(Hz)', $
                            DRAW=0)

    ;Colorbar: Intensity
    cbInt = polWin -> Colorbar(GRAPHIC=imInt, $
                               RANGE=irange, $
                               CBLOCATION='RIGHT', $
                               /RIGHT, $
                               TLOCATION='RIGHT', $
                               TITLE='Intensity!C(nT^2 * Hz)', $
                               CTINDEX=13, $
                               DRAW=0)

    ;Colorbar: Polarization
    cbPol = polWin -> Colorbar(GRAPHIC=imPol, $
                               RANGE=prange, $
                               CBLOCATION='RIGHT', $
                               /RIGHT, $
                               TLOCATION='RIGHT', $
                               TITLE='% Polarization', $
                               CTINDEX=0, $
                               DRAW=0)

    ;Colorbar: Ellipticity
    cbEll = polWin -> Colorbar(GRAPHIC=imEll, $
                               CBLOCATION='RIGHT', $
                               PALETTE=MrCT_red_white_blue, $
                               RANGE=erange, $
                               /RIGHT, $
                               TLOCATION='RIGHT', $
                               TITLE='Ellipticity', $
                               DRAW=0)

    ;Colorbar: k dot b angle
    cbAng = polWin -> Colorbar(GRAPHIC=imAng, $
                               CBLOCATION='RIGHT', $
                               CTINDEX=0, $
                               RANGE=arange, $
                               /RIGHT, $
                               TLOCATION='RIGHT', $
                               TITLE='Angle!C(Deg)', $
                               DRAW=0)

    ;Colorbar: Coherency
    cbCoh = polWin -> Colorbar(GRAPHIC=imCoh, $
                               CBLOCATION='RIGHT', $
                               CTINDEX=0, $
                               RANGE=crange, $
                               /RIGHT, $
                               TLOCATION='RIGHT', $
                               TITLE='Coherency', $
                               DRAW=0)

    ;Realize and draw    
    MyWin -> RealizeGUI
    polWin -> RealizeGUI
    
    return, [mywin, polWin]
end