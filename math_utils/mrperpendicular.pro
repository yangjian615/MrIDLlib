; docformat = 'rst'
;
; :NAME:
;   MrPerpendicular.pro
;
; :PURPOSE:
;+
;   Find the component of the vector X perpendicular to the vector Y::
;
;       X_{perp} = \vec{X} - (\vec{X} \cdot \vec{Y}) \hat{y}
;
; :Categories:
;   Math Untilities
;
; :Params:
;       X:              in, required, type=3xN real
;                       Three-component vector to be projected onto Y.
;       Y:              in, required, type=3xN real
;                       Three-component vector along which the parallel component of `X`
;                           is to be computed.
;
; :Keywords:
;       MAGNITUDE:      out, optional, type=Nx1 float
;                       A named variable that will contain the magnitude of the component
;                           of `X` parallel to `Y`.
;
; :Returns:
;       X_par:          The component of `X` parallel to the vector `Y`
;
; :Uses:
;   Uses the following external programs::
;       parallel_component.pro
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
;       2014/07/17  -   Written by Matthew Argall
;       2014/07/20  -   Properly returns a vector pointing in an arbitrary direction
;                           perpendicular to `Y`. Added the MAGNITUDE keyword. - MRA
;-
function MrPerpendicular, X, Y, $
MAGNITUDE=magnitude
    compile_opt strictarr
    on_error, 2
    
    ;Subtract X_par from X
    ;   MrParallel will check the inputs.
    X_perp = X - MrParallel(X, Y)
    
    ;Compute the magnitude?
    if arg_present(magnitude) then magnitude = sqrt(total(X_perp^2, 1))
    return, X_perp
end
