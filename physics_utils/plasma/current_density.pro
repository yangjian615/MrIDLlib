; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate current density of a plasma::
;
;       J = q n v
;
; :Categories:
;
;   Plasma Physics Utilties
;
; :Params:
;       N:                  in, required, type=numeric array
;                           Plasma density in units of particles per meter cubed (cm^{-3}).
;       V:                  in, required, type=3xN numeric array
;                           Plasma velocity in units of kilometers per second (km/s).
;       Q:                  in, required, type=float
;                           The charge of the particle species in coulombs (C).
;
; :Returns:
;       J:                  The current density in units of nanoAmps per square meter (nA / m^{2})
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
;       09/06/2011  -   Written by Matthew Argall
;-
function current_density, n, v, q
    compile_opt idl2
    on_error, 2
    
    npts = n_elements(n)
    
;---------------------------------------------------------------------
;Calculate the Current Density ///////////////////////////////////////
;---------------------------------------------------------------------

    ;Current Density: J = q * n * v
    ;   units: C * cm^-3 * km/s = 1e9 * A/m^2 => must multiply result by 1e9
    ;   scale: (1e9 * A/m^2) * (1e9 * nA/A) = 1e18 * nA / m^2

    ;Alfven Speed (nA / m^2)
    J = (q*1e18) * rebin(reform(n, 1, npts), 3, npts) * v

    return, J
end
