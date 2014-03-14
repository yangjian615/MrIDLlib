; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the E-to-B ratio::
;
;       r = \frac{|E|} {|B|}
;
; :Categories:
;
;   Plasma Physics Utilties
;
; :Params:
;       E;          in, required, type=numeric array
;                   Electric field values. Units are assumed to be milliVolts per meter (mV/m)
;       B:          in, required, type=numeric array
;                   Magnetic fieild values. Units are assumed to be nanoTesla (nT).
;
; :Returns:
;
;       ratio:      The alfven velocity in kilometers per second (km/s)
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
;       2013-10-28  -   Written by Matthew Argall
;-
function E_to_B_ratio, E, B
    compile_opt strictarr
    on_error, 2

;---------------------------------------------------------------------
;Check inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Units:: (mV/m) / (nT) = (V/m) / T * 1e6 = m/s * 1e6 --> Must multiply by 1e6
    ratio = (E / B) * 1e6

    return, ratio
end
