; docformat = 'rst'
;
; NAME:
;
;       MrPoynting_Vector
;
; PURPOSE:
;+
;   The purpose of this program is to calculate the Poynting Vector, in micro-Watts per
;   square meter (W/m^2 * 1e-6), in the time domain::
;       \vec{S} = \frac{1}{\mu_{0}} \left( \vec{E} \times \vec{B} \right)
;
;   References::
;       Griffiths, D. J., Introduction to Electrodynamics, 3rd. Ed., Prentice Hall,
;           1999, pg. 347
;
; :Categories:
;
;       Physics Utility, Time Series
;
; :Params:
;
;       E:                  in, required, type=3xN numeric
;                           Electric field waveform data in millivolts per meter (mV/m)
;       B:                  in, required, type=3xN numeric
;                           Magnetic field waveform data in nanoTelsa (nT). Must have the
;                               same number of elements as `E`.
;
; :Keywords:
;       MKS:                in, optional, type=boolean, default=0
;                           Indicate the the electric and magnetic fields have units of
;                               Volts per meter (V/m) and Tesla (T), respectively.
;                               Millivolts per meter (mV/m) and nanoTesla (nT) are assumed.
;                           
; :Returns:
;
;       S_poynting:         The poynting vector in the spectral domain. Resulting units
;                           are in Watts per square meter (W / m^2).
;
; :Uses:
;   Uses the following external programs::
;       cross_product.pro
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
;       04/18/2013  -   Written by Matthew Argall
;-
function MrPoynting_Vector, E, B, $
MKS = mks
    compile_opt idl2
    on_error, 2

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
    ;   by 1e-12 to convert to W/m^2 and another 1e+6 to convert to micro-W/m^2
    if keyword_set(mks) $
        then coeff = 1.0e-6 / constants('mu_0') $
        else coeff = 1.0e-6 / constants('mu_0') 
    
    ;Allocate Memory
    S_poynting = coeff * cross_product(E, B)

    return, S_poynting
end