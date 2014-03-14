; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the pressure of a plasma::
;
;       P = n k T
;
;   This is the equation of state for an ideal gas.
;
; :Categories:
;
;   Plasma Physics Utilties
;
; :Params:
;       N:                  in, required, type=numeric array
;                           Plasma density in units of particles per meter cubed (cm^{-3}).
;       T:                  in, required, type=numeric array
;                           Plasma temperature in kelvin (MK)
;
; :Returns:
;       PLASMA_PRESSURE:    The pressure of the plasma in nano-Pascals (nPa)
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
;       07/15/2013  -   Units are now assumed to be cm^-3, MK, and nPa. - MRA
;-
function plasma_pressure, n, T
    compile_opt idl2
    on_error, 2
    
	;compute the plasma pressure
	;   Units:      1e12*Pa, so must multiply by 1e12 to convert to Pa
	;   Convert:    Multiply by 1e9 to convert from Pa to nPa 
	plasma_pressure = n * constants('k_b') * 1e21 * T   ;Pa --> nPa

	return, plasma_pressure

end