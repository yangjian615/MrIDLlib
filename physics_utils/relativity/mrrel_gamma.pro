; docformat = 'rst'
;
;+
;   Compute the relativistic lorentz factor, gamma::
;     \gamma = \frac{1} { \sqrt{ 1 - \frac{v^{2}} {c^{2}} } }
;
; :Categories:
;   Plasma Physics Utilties
;
; :Params:
;       V:                  in, required, type=fltarr
;                           Velocity of the particle in meters per second (m/s)
;
; :Returns:
;       GAMMA:              The Lorentz factor (unitless)
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
function mrrel_gamma, v, $
DOUBLE=tf_double
	compile_opt idl2
	on_error, 2
	
	;Double precision?
	tf_double = keyword_set(double) || size(type, /TNAME) eq 'DOUBLE'
	
	;Speed of light
	c   = constants('c', DOUBLE=tf_double)
	
	;Gamma
	if tf_double $
		then gam = 1.0D / sqrt( 1.0D - v^2/c^2 ) $
		else gam = 1.0  / sqrt( 1.0  - v^2/c^2 )

	return, gam
end
