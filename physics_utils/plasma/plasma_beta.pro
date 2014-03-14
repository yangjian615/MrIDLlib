; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the plasma beta::
;
;       \Beta = \frac {P_{plasma}} {P_{magnetic}}
;
;   This is the ratio of the plasma pressure to the magnetic pressure.
;
; :Categories:
;
;   Plasma Physics Utilties
;
; :Params:
;       B:                  in, required, type=fltarr
;                           Magnitude of the magnetic field, |B|, in units of nano-Tesla (nT).
;       N:                  in, required, type=numeric array
;                           Plasma density in units of particles per centimeter cubed (cm^{-3}).
;       T:                  in, required, type=numeric array
;                           Plasma temperature in Mega-Kelvin (MK)
;
; :Returns:
;       Beta:               The plasma beta (unitless)
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
;       01/28/2012  -   Written by Matthew Argall
;       07/15/2013  -   Units are now assumed to be nT, cm^-3, and MK. - MRA
;-
function plasma_beta, B, n, T
    compile_opt idl2
    on_error, 2
    
	;compute the plasma beta
	plasma_beta = plasma_pressure(n, T) / magnetic_pressure(B)

	return, plasma_beta
end