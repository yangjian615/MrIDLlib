; docformat = 'rst'
;
; NAME:
;       MrEFW__Define
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
; PURPOSE
;+
;   The purpose of this class is to provide FFT, PSD, Spectrogram, and other frequency-
;   domain-related operations.
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg (Coyote Graphics)
;       MrEFW
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
;	Modification History::
;       2013/12/27  -   Written by Matthew Argall.
;       2013/12/30  -   Added the PoyntingSpectra and PoyntingFlux methods. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this program is to create a tranformation matrix that will transform
;   the Modified GSE (MGSE) system into the GSE system.
;
;   MGSE is defined as:
;       Y_MGSE=-W_SC(GSE) x Z_GSE
;       Z_MGSE= W_SC(GSE) x Y_MGSE
;       X_MGSE= Y_MGSE    x Z_MGSE
;
;   where W_SC(GSE) is the spin axis direction in GSE. UVW coordinates in GSE can be
;   retrieved from the `<https://emfisis.physics.uiowa.edu/tools/spacecraft_attitude
;   EMFISIS Spacecraft Attitude Tool>`
;
; :Params:
;       R_UVW2GSE:         in, optional, type=fltarr(3,3)
;                           A transformation matrix from the UVW frame to the GSE frame::
;                               | Ux Uy Uz |  | Au_UVW |   | Ax_GSE |
;                               | Vx Vy Vz |  | Av_UVW | = | Ay_GSE |
;                               | Wx Wy Wz |  | Aw_UVW |   | Az_GSE |
;
; :Keywords:
;       INVERSE:            in, optional, type=boolean, default=0
;                           If set, the transformation matrix from GSE to MSGE will be
;                               returned.
;
; :Returns:
;       R_MGSE2GSE:         The transformation matrox from MGSE to GSE.
;-
function MrEFW::MGSE2GSE, R_UVW2GSE, $
INVERSE = inverse
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    ;How the MGSE unit vectors are seen in GSE
    Y_MGSE = -cross_product(R_UVW2GSE[*,2], [0.0, 0.0, 1.0])
    Z_MGSE =  cross_product(R_UVW2GSE[*,2], Y_MGSE)
    X_MGSE =  cross_product(Y_MGSE, Z_MGSE)
    
    ;Form the transformation matrix
    R_MGSE2GSE = [[X_MGSE], [Y_MGSE], [Z_MGSE]]
    
    ;Return the inverse transform?
    if keyword_set(inverse) $
        then return, transpose(R_MGSE2GSE) $
        else return, R_MGSE2GSE
end


;+
;   The purpose of this mehtod is to calculate the third component of the electric
;   field assuming \vec{E} \cdot \vec{B} = 0::
;       E_{z} = -\frac{( E_{x} B_{x} + E_{y} B_{y})} {B_{z}}
;
;   NOTE:
;       Do not assign two output parameters to variables with the same name. This will
;       cause unexpected results.
;
; :Params:
;       BFIELD:             in, required, type=object
;                           A 'MrDataAnalysis' object containing the magnetic field and
;                               its time stamps.
;
; :Keywords:
;       B_MIN:              in, optional, type=float
;                           If this is specified, any value of Bz < min_Bz will cause
;                               !values.f_nan to be used as the result.
;       INTERP:             in, optional, type=boolean, default=0
;                           Interpolate the electric field so that is has the same time
;                               stamps as the magnetic field. 
;       TMATRIX:            in, optional, type=3x3 float
;                           A matrix that transforms B into the same frame as E. Typically,
;                               this is from an inertial system into the spacecraft frame.
;                               If provided and `COMBINE` is set, the 3D electric field
;                               will be transformed into the `B`'s original frame.
;       X:                  in, optional, type=boolean, default=0
;                           If set, the X-component of `E` will be computed. In this case
;                               E[0,*] = Ey, E[1,*] = Ez.
;       Y:                  in, optional, type=boolean, default=0
;                           If set, the Y-component of `E` will be computed. In this case
;                               E[0,*] = Ex, E[1,*] = Ez.
;       Z:                  in, optional, type=boolean, default=0
;                           If set, the Z-component of `E` will be computed. In this case
;                               E[0,*] = Ex, E[1,*] = Ey. If none of `X`, `Y`, or `Z` are
;                               set, then we will assume you want the z-component.
;-
pro MrEFW::EdotBzero, BField, $
B_MIN = B_min, $
INTERP = interp, $
TMATRIX=TMatrix, $
X=x, $
Y=y, $
Z=z
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Default to interpolating.
    interp = keyword_set(interp)

    ;Make sure a valid object was given.
    if obj_valid(BField) eq 0 || obj_class(BField) ne 'MRDATAANALYSIS' then $
        message, 'BFIELD must be a valid "MrDataAnalysis" object.'

    ;Interpolate?
    if interp then self -> Interp_TS, BField

    ;Get the data
    self -> GetData, E
    BField -> GetData, B

    *self.y = edotb_zero(E, B, /COMBINE, B_MIN=B_min, TMATRIX=TMatrix, X=x, Y=y, Z=z)
end



;+
;   The purpose of this method is to perform simple VxB subtraction to bring the electric
;   field into an inertial frame.
;-
function MrEFW::VxB_Remove, t, X, B
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    ;Compute the spacecraft velocity
    v = (X[1:*] - X[0:-2]) / (t[1:*] - t[0:-2])

    ;Calculate VxB
    Em = cross_product(v, B[2:*])
    
    return, Em
end



;+
;   Get class properties.
;
; :Keywords:
;   _REF_EXTRA:             out, optional, type=any
;                           Any keyword accepted by the superclasses is also accepted for
;                               keyword inheritance.
;-
pro MrEFW::GetProperty, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    self -> MrDataAnalysis::GetProperty, _STRICT_EXTRA=extra
end


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
; :Params:
;       BField:             in, required, type=object
;                           A "MrDataAnalysis" object containing magnetic field waveform
;                               data in units of nano-Tesla (nT)
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
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
;       FREQUENCIES:        out, type=fltarr(NFFT)
;                           The frequency bins of the spectrogram
;       INTERP:             in, optional, type=boolean, default=0
;                           If set, the electric field and `BFIELD` will be interpolated
;                               to have the same time stamps.
;       NDETREND:           in, optional, type=integer, default=0
;                           Number of points that make up the sliding, box-car average
;                               to be removed from the data.
;       NSYSTEM:            in, optional, type=boolean, default=0
;                           If greater than 0, data will be rotated into a field-aligned
;                               coordinate system, where NSYSTEM number of points is used
;                               to calculate the direction of the background field. See
;                               the FAR and FAS methods.
;       POSITION:           in, optional, type=fltarr
;                           Position at which each `BFIELD` data point was taken. If
;                               provided, data will be rotated into the field-aligned,
;                               radial coordinate system. Use with `NSYSTEM`.
;       MKS:                in, optional, type=boolean, default=0
;                           Indicate the the electric and magnetic fields have units of
;                               Volts per meter (V/m) and Tesla (T), respectively.
;                               Millivolts per meter (mV/m) and nanoTesla (nT) are assumed.
;       NAN:                in, optional, type=boolean, default=0
;                           If set, treat NaN's as missing values while detrending.
;       S_MIN:              in, optional, type=float
;                           Values of `S_poynting` below this value will be set to NaN
;       T0:                 in, optional, type=boolean, default=0.0
;                           Time offset, in seconds, at which `TIME` is to begin.
;       TIME:               out, optional, type=dblarr
;                           The time associated with each Power Spectral Density slice. It
;                               is taken at the beginning of each FFT.
;       TMATRIX:            in, optional, type=fltarr(9)
;                           A 3x3 transformation matrix that rotates `BFIELD` into the
;                               frame of the electric field.
;       _EXTRA:             in, type=structure
;                           Any keyword accepted by MrFFT.pro
;-
pro MrEFW::PoyntingSpectra, BField, nfft, dt, nshift, $
B_MIN = b_min, $
COMPUTE_EX = compute_Ex, $
COMPUTE_EY = compute_Ey, $
COMPUTE_EZ = compute_Ez, $
FREQUENCIES = frequencies, $
INTERP = interp, $
MKS = mks, $
NAN=NaN, $
NDETREND = nDetrend, $
NSYSTEM = nSystem, $
POSITION = position, $
S_MIN = s_min, $
T0 = t0, $
TIME = time, $
TMATRIX = TMatrix, $
_REF_EXTRA = extra
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    mks = keyword_set(mks)
    interp = keyword_set(interp)
    if n_elements(t0) eq 0 then t0 = 0.0

    compute_Ex = keyword_set(compute_Ex)
    compute_Ey = keyword_set(compute_Ey)
    compute_Ez = keyword_set(compute_Ez)
    
    if n_elements(nDetrend) eq 0 then nDetrend = 0
    if n_elements(nSystem)  eq 0 then nSystem = 0
    
;-----------------------------------------------------
;Compute the 3rd Component of E? \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Rotate the magnetic field, if need be
    if n_elements(TMATRIX) gt 0 then BField -> RotateVector, TMatrix
    
    ;Interpolate the fields?
    if interp $
        then self -> Interp_TS, BField, DX_OUT=dt_pynt $
        else dt_pynt = self -> SamplePeriod()

    ;Compute the third component of the electric field?
    if (compute_Ex + compute_Ey + compute_Ez gt 0) $
        then self -> EdotBzero, BField, X=compute_Ex, Y=compute_Ey, Z=compute_Ez

;-----------------------------------------------------
;Detrend and Rotate the Fields? \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if nSystem + nDetrend gt 0 then begin
        ;Transform the magnetic field
        BField -> DetrendRotate, nDetrend, nSystem, position, DIMENSION=self.dimension, $
                                 NAN=NaN, RMATRIX=rMatrix
        
        ;Use the same transformation matrix
        self -> DetrendRotate, nDetrend, DIMENSION=self.dimension, NAN=NaN, RMATRIX=rMatrix
    endif

;-----------------------------------------------------
;FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    self -> GetData, E
    BField -> GetData, B

    ;Take the FFT
    oFFT = obj_new('MrFFT')
    E_fft = oFFT -> FFT(E, nfft, dt_pynt, nshift, T0=t0, TIME=time, FREQUENCIES=frequencies, $
                        DIMENSION=self.dimension, _STRICT_EXTRA=extra)
    
    B_fft = oFFT -> FFT(B, nfft, dt_pynt, nshift, T0=t0, DIMENSION=self.dimension, $
                        _STRICT_EXTRA=extra)
    obj_destroy, oFFT
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

    ;Store the data
    self -> SetData, time, S_poynting, frequencies
end


;+
;   Display the Poynting vector spectral density in Fourier space.
;
;   Names of the window and graphics objects are::
;       Display Window                      -   'Poynting Flux Window'
;       X-Component of the Poynting Flux    -   'Sx'
;       Y-Component of the Poynting Flux    -   'Sy'
;       Z-Component of the Poynting Flux    -   'Sz'
;       Colorbars from each image           -   'CB: ' + name
;
; :Params:
;       BField:         in, optional, type=object
;                       A "MrDataAnalysis" object containing magnetic field waveform
;                           data in units of nano-Tesla (nT)
;       NFFT:           in, optional, type=int, default=1024
;                       The number of points to use per FFT
;       DT:             in, optional, type=float. default=1.0
;                       The time between data samples. If not present, unit spacing
;                           is assumed.
;       NSHIFT:         in, optional, type=int. default=`NFFT`/2
;                       The number of points to shift ahead after each FFT.
;
; :Keywords:
;       T0:             in, optional, type=float, default=0.0
;                       The time offset at which `DATA` begins, in seconds.
;       TITLE:          in, optional, type=strarr(), default='Power Spectral Density'
;                       An easier way of specifying the title for each spectral plot.
;       _REF_EXTRA:     in, optional, type=structure
;                       Any keyword accepted by the PoyntingSpectra method is also accepted
;                           for keyword inheritance.
;
; :Returns:
;       POYNTWIN:       out, required, type=object/objarr
;                       An object reference to a MrWindow widget containing the Poynting
;                           spectra graphics.
;-
function MrEFW::PoyntingFlux, BField, nfft, dt, nshift, $
CURRENT = current, $
T0 = t0, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    dims = size(data, /DIMENSION)
    components = ['X', 'Y', 'Z']

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Defaults -- NFFT, NSHIFT, DT, and DIMENSION will be handled by MrPSD.
    if arg_present(cbArr) then keep_cb = 1 else keep_cb = 0
    if n_elements(t0) eq 0 then t0 = 0.0
    current = keyword_set(current)
    
    ;Create the window
    refresh_in = 1
    if current then begin
        poyntWin = GetMrWindows(/CURRENT)
        refresh_in = poyntWin.refresh
        poyntWin -> Refresh, /DISABLE
    endif else begin
        poyntWin = MrWindow(XSIZE=500, YSIZE=650, REFRESH=0, XMARGIN=[8,13], $
                            NAME='Poynting Flux Window')
    endelse
    
;-----------------------------------------------------
;Calculate PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Compute the spectrogram
    if n_params() gt 0 then begin
        self -> PoyntingSpectra, BField, nfft, dt, nshift, $
                                 T0 = t0, $
                                _EXTRA = extra
    endif
                           
    ;Retrieve the data
    self -> GetData, time, poynt, frequencies

;-----------------------------------------------------
;Create Images and Colorbars \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Create the spectrogram plots.
    for i = 0, 2 do begin
        name = 'S' + components[i]
    
        ;Create the images
        !Null = MrImage(poynt[*,*,i], time, frequencies, /CURRENT, $
                        /AXES, $
                        CTINDEX=13, $
                        MISSING_COLOR='Black', $
                        NAME=name, $
                        TITLE='Poyting-Flux: S$\down' + components[i] + '$', $
                        XTICKFORMAT='time_labels', $
                        XTITLE='Time (UT)', $
                        YTITLE='Frequency (Hz)')

        ;Create the colorbar        
        !Null = MrColorbar(TITLE='S$\down' + components[i] + '$!C($\mu$W/m^2 * Hz)', $
                           TARGET=poyntWin[name], NAME='CB: ' + name, /CURRENT)

        ;Bind the colorbar to the image.
        poyntWin -> Bind, [poyntWin['CB: ' + name], poyntWin[name]], /CAXIS
    endfor
    
    ;Bind the X-axes together
    Ims = poyntWin -> Get(/ALL, ISA='MrImage')
    poyntWin -> Bind, Ims, /XAXIS

;-----------------------------------------------------
;Return Window or Objects? \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    poyntWin -> Refresh, DISABLE=~refresh_in
    return, poyntWin
end


;+
;   Set class properties.
;
; :Keywords:
;   _REF_EXTRA:             in, optional, type=any
;                           Any keyword accepted by the superclasses is also accepted for
;                               keyword inheritance.
;-
pro MrEFW::SetProperty, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Superclass properties
    self -> MrDataAnalysis::SetProperty, _STRICT_EXTRA=extra
end


;+
;   Clean up after the object is destroyed
;-
pro MrEFW::cleanup
    compile_opt strictarr
    
    self -> MrDataAnalysis::Cleanup
end


;+
;   The initialization method.
;-
function MrEFW::init, tE, E, $
DIMENSION=dimension
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Superclass
    case n_params() of
        1: success = self -> MrDataAnalysis::Init(E, DIMENSION=dimension)
        2: success = self -> MrDataAnalysis::Init(tE, E, DIMENSION=dimension)
        else: message, 'Incorrect number of parameters.'
    endcase
    
    return, success
end


;+
;   The class definition statement.
;-
pro MrEFW__define
    compile_opt strictarr
    
    class = { MrEFW, $
              inherits MrDataAnalysis $
             }
end