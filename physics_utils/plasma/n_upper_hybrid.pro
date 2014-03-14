; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the electron density from the upper
;   hybrid frequency::
;
;       \omega_{h}^{2} = \omega_{p}^2 + \omega_{c}^{2}
;                      = \left( \frac{n_{0} e^{2}} {\epsilon_{0} m} \right)^{2} + \left( \frac{q B} {m} \right^{2}
;       n_{e] = \left( \omega_{h}^{2} - \omega_{c}^{2} \right) * ( \frac{\epsilon_{0} m} {e^{2}} )
;
; :Categories:
;   Physics Utilties, Plasmas
;
; :Params:
;       f_h:                in, required, type=numeric array
;                           The upper hybrid frequency \f_{h} in cycles per second.
;       B:                  in, required, type=numeric array,
;                           The total magnetic field strength |B| in nanoTesla, nT.
;
; :Returns:
;       N:                  The electron density in particles*cm^{-3}
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
;       10/02/2012  -   Written by Matthew Argall
;-
function n_upper_hybrid, f_p, B
    compile_opt idl2
    on_error, 2

	;compute the electron density
	;Units: m^-3 want cm^-3: Multiply by 1e-6 m^3/cm^3
	n_e = ((2*!pi*f_p)^2 - w_cyclotron(B, 'e-')) * constants('epsilon_0') * (constants('m_e') / constants('q')^2)

	return, n_e * 1e-6   ;Unit correction

end