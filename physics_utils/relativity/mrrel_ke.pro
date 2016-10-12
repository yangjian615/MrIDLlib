; docformat = 'rst'
;
;+
;   Compute the kinetic energy of a relativistic particle with a given velocity.
;       KE = m c^{2} (\gamma - 1)
;
; :Categories:
;   Plasma Physics Utilties
;
; :Params:
;       V:                  in, required, type=fltarr
;                           Velocity of the object (km/s)
;       M:                  in, required, type=float
;                           The mass of a particle species in kilograms (Kg).
;
; :Returns:
;       E:                  Energy of the particle in eV.
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
function mrrel_ke, v, m
	compile_opt strictarr
	on_error, 2
	
	;Conversion factor
	eV2Joule = 1.60218e-19
	km2m     = 1e3

	;Compute the gamma factor
	c    = constants('c')
	gam  = mrrel_gamma(v * km2m)

	;Kinetic energy
	KE = m * c^2 * (gam - 1.0)

	return, KE/eV2Joule
end
