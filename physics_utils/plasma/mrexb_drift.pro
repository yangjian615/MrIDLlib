; docformat = 'rst'
;
; NAME:
;   MrExB_Drift
;
; PURPOSE:
;+
;   The purpose of this program is to calculate the ExB drift velocity::
;
;       v_{d} = \frac{ \vec{E} \times \vec{B} } { |B|^{2} }
;
; :Categories:
;   Plasma Physics Utilties
;
; :Params:
;       E:              in, required, type=3xN real
;                       Three-component electric field data in units of milli-Volts per
;                           meter (mV/m).
;       B:              in, required, type=3xN real
;                       Three-component magnetic field data in units of nanoTesla (nT).
;                           B must be the same size as `E`.
;
; :Returns:
;       v_ExB:          The ExB drift velocity in units of kilometers per second (km/s)
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
; :History:
;   Modification History::
;       07/17/2014  -   Written by Matthew Argall
;-
function MrExB_Drift, E, B, $
MAGNITUDE=magnitude
    compile_opt strictarr
    on_error, 2

;---------------------------------------------------------------------
;Check inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    dimsE = size(E, /DIMENSIONS)
    dimsB = size(B, /DIMENSIONS)
    
    ;Make sure they are the same size.
    if array_equal(dimsE, dimsB) eq 0 $
        then message, 'E and B must be the same size.'
    
    ;Make sure they are 3xN
    if dimsE[0] ne 3 $
        then message, 'E and B must be 3xN arrays.'
;---------------------------------------------------------------------
;Calculate the ExB drift velocity ////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Units
    ;   mV/m * nT / (nT)^2 = m/s * 1e6
    ;   => Must multiply the result by 1e6 in order to have units of m/s
    v_ExB = cross_product(E, B)
    Bmag_squared = total(B^2, 1)
    
    ;Divide by |B|^2
    v_ExB[0,*] = v_ExB[0,*] / Bmag_squared
    v_ExB[1,*] = v_ExB[1,*] / Bmag_squared
    v_ExB[2,*] = v_ExB[2,*] / temporary(Bmag_squared)
    
    ;Convert km/s (1e6 * 1e-3)
    v_ExB *= 1e3
    
    ;Return the magnitude?
    if arg_present(magnitude) then magnitude = sqrt(total(v_ExB^2, 1))
    
    return, v_ExB
end
