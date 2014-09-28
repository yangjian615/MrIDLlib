; docformat = 'rst'
;
; :NAME:
;   MrParallel.pro
;
; :PURPOSE:
;+
;   Find the component of the vector X parallel to the vector Y::
;
;       X_{par} = ( \vec{X} \cdot \vec{Y} ) \hat{y}
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
;       cross_product.pro
;       magnitude_vec.pro
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
;       07/17/2014  -   Written by Matthew Argall
;       07/20/2014  -   Forgot to multipy by y-hat to turn it into a vector. Added the
;                           MAGNITUDE keyword. - MRA
;-
function MrParallel, X, Y, $
MAGNITUDE=magnitude
    compile_opt strictarr
    on_error, 2

;---------------------------------------------------------------------
;Check inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    dimsX = size(X, /DIMENSIONS)
    dimsY = size(Y, /DIMENSIONS)
    
    ;Make sure they are the same size.
    if array_equal(dimsX, dimsY) eq 0 $
        then message, 'X and Y must be the same size.'
    
    ;Make sure they are 3xN
    if dimsX[0] ne 3 $
        then message, 'X and Y must be 3xN arrays.'
;---------------------------------------------------------------------
; Compute the Parallel Velocity //////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Normalize Y
    y_hat = normalize(Y)
    
    ;Dot X into Y
    ;   - Reform to create three identical columns.
    ;   - Multiply by y-hat
    x_par = rebin(reform(dot_product(X, y_hat), 1, dimsX[1]), 3, dimsX[1]) * y_hat
    
    ;Compute the magnitude?
    if arg_present(magnitude) then magnitude = sqrt(total(x_par^2, 1))
    return, x_par
end
