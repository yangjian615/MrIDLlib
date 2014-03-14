; docformat = 'rst'
;
; NAME:
;       CROSS_PRODUCT
;
; PURPOSE:
;+
;       The purpose of this program is to compute the cross product of two 3xN vector
;       arrays by crossing the individual vectors::
;           x[3x1] and y[3x1] then use `crosspn`
;           x[3x1] and y[3xN] then take the cross product of x with y[*,i]
;           x[3xN] and y[3x1] then take the cross product of x[*,i] with y
;           x[3xN] and y[3xN] then take the cross product of x[*,i] y[*,i]
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       X:              in, optional, type="fltarr(3,N)"
;                       An array of 3-dimensional vectors to be crossed with `Y`
;       Y:              in, optional, type="fltarr(3,N)"
;                       An array of 3-dimensional vectors to be crossed with `X`. Must
;                           contain either 1 vector or the same number of vectors as `X`.
;
; :Keywords:
;
; :Returns:
;       X_CROSS_Y:      The cross product of the vectors of x with the vectors of y.
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
;
;       11/03/2011  -   Written by Matthew Argall
;-
function cross_product, x, y
    compile_opt idl2
    on_error, 2

    sx = size(x)
    sy = size(y)

    nx = n_elements(x)
    ny = n_elements(y)

    ;check the size of each array and compute the corss procuct
    ;accordingly. if x and y are not compatible, then print a message
    if (nx eq 3) and (ny eq 3) then begin
        x_cross_y = crossp(x, y)
    endif else if (nx eq 3) and (sy[0] eq 2) and (sy[1] eq 3) then begin
        x_cross_y = [x[1]*y[2,*] - x[2]*y[1,*], $
                     x[2]*y[0,*] - x[0]*y[2,*], $
                     x[0]*y[1,*] - x[1]*y[0,*]]
    endif else if (sx[0] eq 2) and (sx[1] eq 3) and (ny eq 3) then begin
        x_cross_y = [x[1,*]*y[2] - x[2,*]*y[1], $
                     x[2,*]*y[0] - x[0,*]*y[2], $
                     x[0,*]*y[1] - x[1,*]*y[0]]
    endif else if (sx[0] eq 2) and (sx[1] eq 3) and $
                  (sy[0] eq 2) and (sy[1] eq 3) and (nx eq ny) then begin
        x_cross_y = [x[1,*]*y[2,*] - x[2,*]*y[1,*], $
                     x[2,*]*y[0,*] - x[0,*]*y[2,*], $
                     x[0,*]*y[1,*] - x[1,*]*y[0,*]]
    endif else print, 'x and y are not fit for crossing'

    return, x_cross_y
end
