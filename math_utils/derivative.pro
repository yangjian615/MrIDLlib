; docformat = 'rst'
;
; NAME:
;       DERIVATIVE
;
; PURPOSE:
;+
;       The purpose of this program is to take the derivative of each component of an
;       an array of vectors. The derivative is taken as::
;
;               y[*,N] - y[*,N-1] / (x[N] - x[N-1])
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;
;       Y:                  in, required, type="FLTARR(3,N)"
;                           Take the derivative of this array. The der
;       X:                  in, optional, type=FLTARR(N), default="LONARR(N_ELEMENTS(Y[0,*]))"
;                           Take the derivative of y with respect to this variable. If not
;                           given, unit spacing is assumed.
;
; :Returns:
;
;       DY_DX:              out, type=same as Y
;                           The derivative of y with respect to x using a 1-point shift
;                               along each column of y to get dy and along x for dx.
;                               dy_dx = dy / dx
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
;       Written by Matthew Argall 08 November 2012
;
;-
function derivative, y, x
    compile_opt idl2
    
    if n_elements(x) eq 0 then x = lonarr(n_elements(y[0,*]))
    
    ;each component of the result will have one less point than the input array
    sz = size(y, /DIMENSIONS)
    if sz[0] ne 3 then message, '"Y" Must be 3xN'
    dy_dx = fltarr(sz[0], sz[1]-1)
    
    ;Shift the columns of y one point and take the difference between it and y to get dy
    ;Do the same for dx, then divide for dy_dx.
    dy = y[*, 1:-1] - y[*, 0:-2]
    dx = x[1:-1] - x[0:-2]
    
    dy_dx[0,*] = dy[0,*] / dx
    dy_dx[1,*] = dy[1,*] / dx
    dy_dx[2,*] = dy[2,*] / dx

    ;return the derivative
    return, dy_dx
end



;----------------------------------------------------
;MAIN LEVEL PROGRAM FOR DEMONSTRATION (.r derivative)
;----------------------------------------------------

;make a line y = x
y = findgen(1,100)
dy_dx = derivative(y)

;plot the line y = x
window, /free
plot, y

;plot the dy_dx (a line at y = 1)
window, /free
plot, dy_dx

end