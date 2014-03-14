; docformat = 'rst'
;
; NAME:
;       DOT_PRODUCT
;
; PURPOSE:
;+
;       The purpose of this program is to compute the dot product of two 3xN vector
;       arrays by dotting the individual vectors::
;           x[3x1] and y[3x1] then take the dot product of x      with y
;           x[3x1] and y[3xM] then take the dot product of x      with y[*,i]
;           x[3xM] and y[3x1] then take the dot product of x[*,i] with y
;           x[NxM] and y[NxM] then take the dot product of x[*,i] with y[*,i]
;
; :Examples:
;       See the main level program at the end of the file::
;
;           IDL> .r dot_product
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       X:              in, optional, type="fltarr(N,M)"
;                       An array of M-dimensional vectors to be dotted with `Y`
;       Y:              in, optional, type="fltarr(N,M)"
;                       An array of M-dimensional vectors to be dotted with `X`. Must
;                           contain either 1 vector or the same number of vectors as `X`.
;
; :Keywords:
;
; :Returns:
;       X_DOT_Y:        The DOT product of the vectors of x with the vectors of y.
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
;       02/13/2013  -   Replaced the ([3xM] do [3xM]) case with the ([NxM] dot [NxM])
;                           case - MRA
;-
function dot_product, x, y
    compile_opt idl2
    on_error, 2

;---------------------------------------------------------------------
;Check Keywords \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------

	sx = size(x)
	sy = size(y)
	
	nx = sx[sx[0] + 2]
	ny = sy[sy[0] + 2]

;---------------------------------------------------------------------
;Compute the Dot Product \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------

    ;check if x and y are 1D, three element vectors
    ;if both are, do a simple dot product    
    if (nx eq 3) and (ny eq 3) then begin
        x_dot_y = total(x*y)

    ;is x a 3 element vector and y a 3xN array?
    endif else if (nx eq 3) and (sy[0] eq 2) and (sy[1] eq 3) then begin
        x_dot_y = x[0]*y[0,*] + x[1]*y[1,*] + x[2]*y[2,*]
  
    ;is x a 3xN array and y a 3 element vector?
    endif else if (sx[0] eq 2) and (sx[1] eq 3) and (ny eq 3) then begin
        x_dot_y = x[0,*]*y[0] + x[1,*]*y[1] + x[2,*]*y[2]

    ;are x and y both NxM?
    endif else if (sx[0] eq 2) and (sy[0] eq 2) and $
                  (sx[1] eq sy[1]) and (nx eq ny) then begin
        ;sum all of the elements in a single row
        x_dot_y = total(x * y, 1)

    ;if none of the above works, x and y are not sized properly and
    ;the dot product cannot be performed
    endif else message, 'x or y is improper size'
    
    return, x_dot_y

end



;---------------------------------------------------
; Main Level Example Program (.r dot_product) //////
;---------------------------------------------------
print, '--------------- Scheme 1: ---------------'
;Create arrays such that X=[3x1], Y=[3,1], then dot them together
X = findgen(3)
Y = findgen(3)
X_dot_Y = dot_product(X, Y)

;Print the results
help, X, Y, X_dot_Y
print, ''
print, format='(%"         X         dot          Y         = X_dot_Y")'
print, format='(%"[%4.1f, %4.1f, %4.1f] dot [%4.1f, %4.1f, %4.1f] = %4.1f")', X, Y, X_dot_Y
print, ''


print, '--------------- Scheme 2: ---------------'
;Create arrays such that X=[3x1], Y=[3,M], then dot them together
X = findgen(3)
Y = findgen(3,4)
X_dot_Y = dot_product(X, Y)

;Print the results
help, X, Y, X_dot_Y
print, ''
print, format='(%"         X         dot          Y         = X_dot_Y")'
print, format='(%"[%4.1f, %4.1f, %4.1f] dot [%4.1f, %4.1f, %4.1f] = %4.1f")', X, Y[*,0], X_dot_Y[0]
for i = 1, n_elements(Y[0,*]) - 1 do $
    print, format='(%"                       [%4.1f, %4.1f, %4.1f] = %4.1f")', Y[*,i], X_dot_Y[i]
print, ''


print, '--------------- Scheme 3: ---------------'
;Create arrays such that X=[3xM], Y=[3,1], then dot them together
X = findgen(3,4)
Y = findgen(3)
X_dot_Y = dot_product(X, Y)

;Print the results
help, X, Y, X_dot_Y
print, ''
print, format='(%"         X         dot          Y         = X_dot_Y")'
print, format='(%"[%4.1f, %4.1f, %4.1f] dot [%4.1f, %4.1f, %4.1f] = %4.1f")', X[*,0], Y, X_dot_Y[0]
for i = 1, n_elements(X[0,*]) - 1 do $
    print, format='(%"[%4.1f, %4.1f, %4.1f]                        = %4.1f")', X[*,i], X_dot_Y[i]
print, ''


print, '-------------- Scheme 4.1: --------------'
;Create arrays such that X=[3xM], Y=[3,M], then dot them together
X = findgen(3,4)
Y = findgen(3,4)
X_dot_Y = dot_product(X, Y)

;Print the results
help, X, Y, X_dot_Y
print, ''
print, format='(%"         X         dot          Y         = X_dot_Y")'
for i = 0, n_elements(X[0,*]) - 1 do $
    print, format='(%"[%4.1f, %4.1f, %4.1f] dot [%4.1f, %4.1f, %4.1f] = %5.1f")', X[*,i], Y[*,i], X_dot_Y[i]
print, ''


print, '-------------- Scheme 4.2: --------------'
;Create arrays such that X=[NxM], Y=[NxM], then dot them together
X = findgen(4,5)
Y = findgen(4,5)
X_dot_Y = dot_product(X, Y)

;Print the results
help, X, Y, X_dot_Y
print, ''
print, format='(%"             X         dot              Y             = X_dot_Y")'
for i = 0, n_elements(X[0,*]) - 1 do $
    print, format='(%"[%4.1f, %4.1f, %4.1f, %4.1f] dot [%4.1f, %4.1f, %4.1f, %4.1f] = %6.1f")', X[*,i], Y[*,i], X_dot_Y[i]
print, ''

end