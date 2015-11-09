; docformat = 'rst'
;
;+
;   Compute the velocity of a relativistic particle with a given energy (does not
;   account for massless particles).
;
;   Starting with
;       E = m c^{2} ( \gamma - 1 )
;
;   solve for v in \gamma
;       v = sqrt{ c^{2} \left[ 1 - \left( \frac{1} { \frac{KE}{m c^{2}} + 1 } \right)^{2} \right] }
;
;   Pulling m c^{2} out of the denominator and noting that the rest energe E0 = mc^2.
;       v = c sqrt{ 1 - \left( \frac{ E_{0} } { E_{0} + E } \right)^{2} }
;
; :Categories:
;   Plasma Physics Utilties
;
; :Params:
;       E:                  in, required, type=fltarr
;                           Kinetic energy of the particle in electron volts (eV)
;       M:                  in, required, type=float
;                           The mass of a particle species in kilograms (Kg).
;
; :Returns:
;       V:                  Velocity of the particle. (m/s)
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/10/31  -   Written by Matthew Argall
;-
function mrrel_velocity, E, m
	compile_opt strictarr
	on_error, 2

	;Compute the gamma factor
	c    = constants('c')
	eV2J = 1.602176565e-19  ;converts eV to Joules
	E_J  = E * eV2J

	;Rest mass
	E0 = m * c^2
	
	;Relativistic velocity
	v = c * sqrt( 1.0 - ( E0 / (E_J + E0) )^2 )

	return, v
end
