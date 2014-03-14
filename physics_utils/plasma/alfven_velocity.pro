; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the Alfven velocity::
;
;       v = B (\mu_{0}*m*n)^{-1/2} = B (\mu_{0}*\rho)^{-1/2}
;
; :Categories:
;
;   Plasma Physics Utilties
;
; :Params:
;       B:                  in, required, type=N or 3xN numeric array
;                           If `B` is a row or column array, then it represents the
;                               magnitude of the magnetic field and the Alfven Speed will
;                               be calculated. If it is a 3xN array, it is assumed to
;                               represent the vector magnetic field and the Alfven velocity
;                               is calculated. Units are expected to be nanoTesla (nT).
;       N:                  in, required, type=numeric array
;                           Plasma density in units of particles per meter cubed (cm^{-3}).
;       M:                  in, required, type=float
;                           The mass of a particle species in kilograms (Kg).
;
; :Keywords:
;       SPEED:              in, optional, type=boolean, default=0
;                           Indicate that `B` is the magnitude of the magnetic field and
;                               that the scalar Alfven speed is to be calculated, vector
;                               velocity.
;
; :Returns:
;
;       v_A:                The alfven velocity in kilometers per second (km/s)
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
;       11/28/2011  -   Written by Matthew Argall
;       07/14/2013  -   Added check for magnitude or vector magnetic field. Units are
;                           now assumed to be nT and cm^-3 and km/s - MRA
;-
function alfven_velocity, B, n, m
    compile_opt strictarr
    on_error, 2

;---------------------------------------------------------------------
;Check inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Determine if B is a vector or scalar (magnitude) array
    if MrIsA(B, /ROW) || MrIsA(B, /COLUMN) || MrIsA(B, /SCALAR) $
        then magnitude = 1B $
        else magnitude = 0B

    npts = n_elements(n)
;---------------------------------------------------------------------
;Calculate the Alfven Velocity ///////////////////////////////////////
;---------------------------------------------------------------------

    ;the alfven velocity, v = B * (mu_0*m*n)^-(1/2)
    ;units: nT / sqrt(cm^3 * mu_0) = 1e-12 * m/s => must multiply result by 1e-12
    mu_0 = constants('mu_0')    ;permeability of free space
    rho = m * n                 ;mass density of the plasma

    ;Alfven Speed
    if magnitude eq 1 then begin
        v_A = B / sqrt(mu_0 * rho)
        
    ;Alfven Velocity
    endif else begin
        v_A = fltarr(3, npts)
        v_A[0,*] = B[0,*] / sqrt(mu_0 * rho)
        v_A[1,*] = B[1,*] / sqrt(mu_0 * rho)
        v_A[2,*] = B[2,*] / sqrt(mu_0 * rho)
    endelse

    return, v_A * 1e-15         ;m/s --> km/s (with factor of 1e-12 in there)
end
