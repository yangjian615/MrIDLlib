; docformat = 'rst'
;
; NAME:
;
;       MrPoynting_Spectra
;
; PURPOSE:
;+
;   The purpose of this program is to calculate the Poynting Vector, in micro-Watts per
;   square meter (W/m^2 * 1e-6), in the spectral domain::
;       \vec{S}_{avg} = \frac{1}{4 \mu} \left(\delta \vec{E}^{*} \times \delta \vec{B} +
;                       \delta \vec{E} \times \delta \vec{B}^{*} \right)
;
;   References::
;       Loto'aniu, T, M., et. al., Propagation of electromagnetic ion cyclotron wave
;           energy in the magnetosphere, J. Geophys. Res., 110, 2005.
;           doi:10.1029/2004JA010816
;
; :Categories:
;
;       Physics Utility, Spectral Analysis
;
; :Params:
;
;       E:                  in, required, type=3xN numeric
;                           Electric field waveform data in units of milli-volts per meter (mV/m)
;       B:                  in, required, type=3xN numeric
;                           Magnetic field waveform data in units of nano-Tesla (nT)
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;
;       B_MIN:              in, optional, type=float
;                           If this is specified, any value of Bi < B_min will cause
;                               !values.f_nan to be used as the result. This keyword is
;                               only used if `COMPUTE_EX`, `COMPUTE_EY`, or `COMPUTE_EZ`
;                               are set.
;       COMPUTE_EX:         in, optional, type=boolean, default=0
;                           If set, the X-component of `E` will be computed. In this case
;                               E[0,*] = Ey, E[1,*] = Ez.
;       COMPUTE_EY:         in, optional, type=boolean, default=0
;                           If set, the Y-component of `E` will be computed. In this case
;                               E[0,*] = Ex, E[1,*] = Ez.
;       COMPUTE_EZ:         in, optional, type=boolean, default=0
;                           If set, the Z-component of `E` will be computed. In this case
;                               E[0,*] = Ex, E[1,*] = Ey.
;       DIMENSION:          in, optional, type=int, default=0
;                           The dimension over which to take the FFT. If 0, then the FFT
;                               is taken over all dimensions (this is the default).
;       FREQUENCIES:        out, type=fltarr(NFFT)
;                           The frequency bins of the spectrogram
;       MKS:                in, optional, type=boolean, default=0
;                           Indicate the the electric and magnetic fields have units of
;                               Volts per meter (V/m) and Tesla (T), respectively.
;                               Millivolts per meter (mV/m) and nanoTesla (nT) are assumed.
;       S_MIN:              in, optional, type=float
;                           Values of `S_poynting` below this value will be set to NaN
;       TIME:               out, type=dblarr
;                           The time associated with each Power Spectral Density slice. It
;                               is taken at the beginning of each FFT.
;       _EXTRA:             in, type=structure
;                           Any keyword accepted by MrFFT.pro
;                           
; :Returns:
;
;       S_poynting:         The poynting vector in the spectral domain. Resulting units
;                           are in Watts per square meter (W / m^2).
;
; :Uses:
;   Uses the following external programs::
;       MrFFT
;       EdotB_zero
;       MrInterp_TS
;       constants
;       
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2012
;
; :History::
;   Modification History::
;       04/12/2013  -   Written by Matthew Argall
;       09/20/2013  -   Fixed typos in calculation. - MRA
;       10/06/2013  -   Added the COMPUTE_E[XYZ], TMARTRIX, TE, and TB keywords.
;-
function MrPoynting_Spectra, E, B, nfft, dt, nshift, $
B_MIN = b_min, $
COMPUTE_EX = compute_Ex, $
COMPUTE_EY = compute_Ey, $
COMPUTE_EZ = compute_Ez, $
DIMENSION = dimension, $
FREQUENCIES = frequencies, $
NDETREND = nDetrend, $
NFAR = nFAR, $
NFAS = nFAS, $
POSITION = position, $
S_MIN = s_min, $
T0 = t0, $
TB = tB, $
TE = tE, $
TIME = time, $
TMATRIX = TMatrix, $
MKS = mks, $
_REF_EXTRA = extra
    compile_opt idl2
    on_error, 2

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ndims = size(E, /N_DIMENSIONS)
    if ndims gt 2 then message, 'DATA must be a vector or array of vectors.'
    
    mks = keyword_set(mks)
    if n_elements(tE) gt 0 and n_elements(tB) gt 0 then interp = 1 else interp = 0
    if n_elements(t0) eq 0 then t0 = 0.0

    compute_Ex = keyword_set(compute_Ex)
    compute_Ey = keyword_set(compute_Ey)
    compute_Ez = keyword_set(compute_Ez)
    
    if n_elements(nDetrend) eq 0 then nDetrend = 0
    if n_elements(nFAR) eq 0 then nFAR = 0
    if n_elements(nFAS) eq 0 then nFAS = 0
;-----------------------------------------------------
;Compute the 3rd Component of E? \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if (compute_Ex + compute_Ey + compute_Ez gt 0) then begin
        if (interp eq 1) then begin
            E_pynt = EdotB_zero(E, B, tE, tB, void, B_pynt, t_pynt, dt_pynt, $
                                X=compute_Ex, Y=compute_Ey, Z=compute_Ez, $
                                B_MIN=b_min, TMATRIX=TMatrix, /COMBINE)
            t0 = t_pynt[0]
        endif else begin
            E_pynt  = EdotB_zero(E, B, X=compute_Ex, Y=compute_Ey, Z=compute_Ez)
            B_pynt  = B
            dt_pynt = dt
        endelse

;-----------------------------------------------------
;Interpolate E and B? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if n_elements(tB) gt 0 then begin
        MrInterp_TS, E, B, tE, tB, E_pynt, B_pynt, t_pynt, DT_OUT=dt_pynt
        t0 = t_pynt[0]

;-----------------------------------------------------
;Leave E and B Alone? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
        
    endif else begin
        E_pynt = E
        B_pynt = B
        dt_pynt = dt
    endelse

;-----------------------------------------------------
;Detrend and Rotate the Fields? \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    nSystem = nFAR > 0 ? nFAR : nFAS
    if nSystem + nDetrend gt 0 then begin
        B_pynt = MrDetrendRotate(B_pynt, nDetrend, nSystem, $
                                 POSITION=position, $
                                 DIMENSION=dimension, $
                                 RMATRIX=rMatrix)
        E_pynt = MrDetrendRotate(E_pynt, nDetrend, $
                                 DIMENSION=dimension, $
                                 RMATRIX=rMatrix)
    endif

;-----------------------------------------------------
;FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Take the FFT
    E_fft = MrFFT(temporary(E_pynt), nfft, dt_pynt, nshift, T0=t0, TIME=time, FREQUENCIES=frequencies, $
                  DIMENSION=dimension, _STRICT_EXTRA=extra)
    
    B_fft = MrFFT(temporary(B_pynt), nfft, dt_pynt, nshift, T0=t0, $
                  DIMENSION=dimension, _STRICT_EXTRA=extra)
;-----------------------------------------------------
;Calculate Poynting Vector \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;The coefficient 1 / (4*mu_0)
    ;   Poynting Flux strengths reported in Loto'aniu ranged from 1-150 uW/m^2, so
    ;   convert to microWeber per square meter.
    ;
    ;   In MKS units, to convert to micro-W/m^2, multiply by 1e-6
    ;
    ;   If not in MKS units, mV/m * nT results an answer 1e+12 too big. Therefore, multiply
    ;   by 1e-12 to convert to W/m^2 and another 1e+6 to convert to micro-W/m^2. In total,
    ;   that means we have to multiply by 1e-6.
    if keyword_set(mks) $
        then coeff = 1.0 / (4.0 * constants('mu_0') * 1.0e6) $
        else coeff = 1.0 / (4.0 * constants('mu_0') * 1.0e6) 

    ;Allocate Memory
    S_poynting = B_fft

    ;Sx = 1/(4u) * [(Ey*Bz - Ez*By) + (EyBz* - EzBy*)]
    S_poynting[*,*,0] = conj(E_fft[*,*,1])*B_fft[*,*,2] - conj(E_fft[*,*,2])*B_fft[*,*,1] + $
                        E_fft[*,*,1]*conj(B_fft[*,*,2]) - E_fft[*,*,2]*conj(B_fft[*,*,1])
                        
    ;Sy = 1/(4u) * [(Ez*Bx - Ex*Bz) + (EzBx* - ExBz*)]
    S_poynting[*,*,1] = conj(E_fft[*,*,2])*B_fft[*,*,0] - conj(E_fft[*,*,0])*B_fft[*,*,2] + $
                        E_fft[*,*,2]*conj(B_fft[*,*,0]) - E_fft[*,*,0]*conj(B_fft[*,*,2])

    ;Sz = 1/(4u) * [(Ex*By - Ey*Bx) + (ExBy* - EyBx*)]
    S_poynting[*,*,2] = conj(E_fft[*,*,0])*B_fft[*,*,1] - conj(E_fft[*,*,1])*B_fft[*,*,0] + $
                        E_fft[*,*,0]*conj(B_fft[*,*,1]) - E_fft[*,*,1]*conj(B_fft[*,*,0])

    ;Multiply by the coefficient
    S_poynting = coeff * S_poynting

;-----------------------------------------------------
;Real, Positive \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Select only the positive frquencies
    posFreqs = where(frequencies gt 0)
    frequencies = frequencies[posFreqs]

    ;Take only the positive frequencies and the real part.
    S_poynting = real_part(S_poynting[*,posFreqs,*])
    
    ;Set values |S_poynting| < S_MIN to !values.f_nan
    if n_elements(s_min) gt 0 then begin
        replaceThese = where(abs(S_poynting) lt s_min, count)
        if count ne 0 then S_poynting[replaceThese] = !values.f_nan
    endif

    return, S_poynting
end