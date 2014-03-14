; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the cyclotron frequency::
;
;       f = \frac{\abs{q} B} {m}
;
; :Categories:
;   Physics Utilties
;
; :Params:
;       B:                  in, required, type=fltarr/fltarr(3\,N)
;                           Magnitude of the magnetic field, |B|, or the 3-component
;                               magnetic field. Units are nano-Testla (nT).
;       PARTICLE:           in, required, type=string, default='H+'
;                           The name of the particle for which the cyclotron frequency
;                               is to be calculated. Options are::
;                                   'H+'
;                                   'He+'
;                                   'O+'
;                                   'e-'
;
; :Returns:
;       F_C:                Cyclotron frequency in Hertz (Hz)
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
;       07/06/2013  -   Particle is now a required parameter. - MRA
;       07/11/2013  -   Return angular frequency instead of frequency. - MRA
;       07/15/2013  -   Units are now assumed to be nT. The vector or scalar magnetic
;                           field can be given. - MRA
;       2014/03/03  -   Fixed how 3D-vectors and 1D-vectors are distinguished. - MRA
;-
function w_cyclotron, B, particle
    compile_opt idl2
    on_error, 2
    
	;electron or ion cyclotron frequency?
	case particle of
	    ;Proton
	    'H+': begin
	        q = constants('q')
	        m = constants('m_p')
	    endcase
	    
	    ;Helium
	    'He+': begin
	        q = constants('q')
	        m = constants('m_He')
	    endcase
	    
	    ;Oxygen
	    'O+': begin
	        q = constants('q')
	        m = constants('m_O')
	    endcase
	    
	    ;Electron
	    'e-': begin
	        q = constants('q')
	        m = constants('m_e')
	    endcase
	    
	    else: message, 'Particle type ' + particle + ' not recognized.'
	endcase
	
	dims = size(B, /DIMENSIONS)
	nDims = size(B, /N_DIMENSIONS)

	;compute the cyclotron frequency
	;Units: 1e-9 * B * (q/m) = 1e-9 rad/s => must multiply result by 1e-9
	if nDims eq 2 && (max(dims eq 3) eq 1) $
	    then omega_c = magnitude_vec(B) * (q / m) $
	    else omega_c = B * (q / m)
	
	return, omega_c * 1e-9  ;Unit correction

end