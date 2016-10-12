; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the plasma frequency::
;
;       \omega_{p} = \sqrt{\frac{n_{e} q^{2}} {m \epsilon_{0}}}
;
; :Categories:
;   Physics Utilties
;
; :Params:
;       N:                  in, required, type=numeric array
;                           Plasma density in units of particles per centimeter cubed (cm^{-3}).
;       PARTICLE:           in, optional, type=string, default='H+'
;                           The name of the particle for which the cyclotron frequency
;                               is to be calculated. Options are::
;                                   'H+'
;                                   'He+'
;                                   'O+'
;                                   'e-'
;
; :Returns:
;       OMEGA_P:            The plasma frequency in Hertz (Hz)
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
;       01/29/2012  -   Written by Matthew Argall
;       07/05/2013  -   PARTICLE is not a required parameter. - MRA
;       07/11/2013  -   Return angular frequency instead of frequncy. - MRA
;       07/15/2013  -   Units are assumed to be cm^-3 - MRA
;-
function w_plasma, n, particle
	compile_opt idl2
	on_error, 2

	;electron or ion cyclotron frequency?
	case particle of
		'H+': m = constants('m_p', /DOUBLE)
		'He+': m = constants('m_He', /DOUBLE)
		'O+': m = constants('m_O', /DOUBLE)
		'e-': m = constants('m_e', /DOUBLE)
		else: message, 'Particle type ' + particle + ' not recognized.'
	endcase

	;compute the plasma beta
	;units: sqrt(1e6 * m^-3) * sqrt(q/(m*epsilon_0)) --> 1e3*rad/s => Multiply result by 1e3
	omega_p = sqrt(n) * sqrt(constants('q', /DOUBLE)^2 / (m * constants('epsilon_0', /DOUBLE)))

	return, omega_p * 1e3   ;Unit correction

end