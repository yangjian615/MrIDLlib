; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the magnetic pressure::
;
;       p = \frac{B^{2}} {2 \mu_{0}}
;
; :Categories:
;   Physics Utilties
;
; :Params:
;       B:                  in, required, type=3xN numeric array
;                           Magnetic field [Bx, By, Bz] in units of nano-Tesla (nT).
;
; :Returns:
;       MAGNETIC_PRESSURE:  The magnetic pressure in units of nano-Pascal
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
;       07/15/2013  -   Units are now assumed to be nT and nP.
;-
function magnetic_pressure, B
    compile_opt idl2
    on_error, 2
    
    ;Determine if B is a vector or scalar (magnitude) array
    if MrIsA(B, /ROW, /COLUMN) $
        then magnitude = 1 $
        else magnitude = 0

	;compute the magnetic pressure
	;units: nT^2 / (2 mu_0) = 10e-18 Pa = 1e-9 nPa
	if magnitude eq 0 $
	    then magnetic_pressure = dot_product(B, B) / (2 * constants('mu_0')) $
	    else magnetic_pressure = B^2 / (2 * constants('mu_0'))

	return, magnetic_pressure * 1e-9    ;Pa -> nPa

end