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
;           x[Nx3] and y[Nx3] then take the cross product of x[i,*] y[i,*]
;
;   NOTE:
;       - A 3x3 vector will be interpreted as an [3xN] vector.
;       - x and y can have a different number of elements, so long as one of their
;           dimensions is of size 3. The result will be the 3xN cross product, where
;           N is the smaller x and y.
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
;       11/03/2011  -   Written by Matthew Argall
;       2014/09/10  -   Removed the condition that the number of points has to be
;                           equal. If `X` is 3xN and `Y` is 3xM, then the output will
;                           be 3xL, where L = N < M. - MRA
;       2015/01/20  -   Added the Nx3 cross 3 case. - MRA
;       2015/02/26  -   Added the 3 cross Nx3 case. - MRA
;-
function cross_product, x, y
	compile_opt idl2
	on_error, 2

	xdims  = size(x, /DIMENSIONS)
	ydims  = size(y, /DIMENSIONS)
	xndims = size(x, /N_DIMENSIONS)
	yndims = size(y, /N_DIMENSIONS)
	nx = n_elements(x)
	ny = n_elements(y)

	;3 cross 3
	if (nx eq 3) and (ny eq 3) then begin
		x_cross_y = crossp(x, y)
		if xdims[0] eq 1 then x_cross_y = transpose(x_cross_y)

	;3 cross 3xN
	endif else if (nx eq 3) and (yndims eq 2) and (ydims[0] eq 3) then begin
		x_cross_y = [x[1]*y[2,*] - x[2]*y[1,*], $
		             x[2]*y[0,*] - x[0]*y[2,*], $
		             x[0]*y[1,*] - x[1]*y[0,*]]
	
	;3 cross Nx3
	endif else if (nx eq 3) and (yndims eq 2) and (ydims[1] eq 3) then begin
		x_cross_y = [[x[1]*y[*,2] - x[2]*y[*,1]], $
		             [x[2]*y[*,0] - x[0]*y[*,2]], $
		             [x[0]*y[*,1] - x[1]*y[*,0]]]
		             
	;3xN cross 3
	endif else if (xndims eq 2 && xdims[0] eq 3) and (ny eq 3) then begin
		x_cross_y = [x[1,*]*y[2] - x[2,*]*y[1], $
		             x[2,*]*y[0] - x[0,*]*y[2], $
		             x[0,*]*y[1] - x[1,*]*y[0]]
		             
	;Nx3 cross 3
	endif else if (xndims eq 2 && xdims[1] eq 3) and (ny eq 3) then begin
		x_cross_y = [[x[*,1]*y[2] - x[*,2]*y[1]], $
		             [x[*,2]*y[0] - x[*,0]*y[2]], $
		             [x[*,0]*y[1] - x[*,1]*y[0]]]
		             
	;3xN cross 3xN
	endif else if (xndims eq 2 && xdims[0] eq 3) and $
	              (yndims eq 2 && ydims[0] eq 3) then begin
		x_cross_y = [x[1,*]*y[2,*] - x[2,*]*y[1,*], $
		             x[2,*]*y[0,*] - x[0,*]*y[2,*], $
		             x[0,*]*y[1,*] - x[1,*]*y[0,*]]
		             
	;Nx3 cross Nx3
	endif else if (xndims eq 2 && xdims[1] eq 3) and $
	              (yndims eq 2 && ydims[1] eq 3) then begin
		x_cross_y = [[x[*,1]*y[*,2] - x[*,2]*y[*,1]], $
		             [x[*,2]*y[*,0] - x[*,0]*y[*,2]], $
		             [x[*,0]*y[*,1] - x[*,1]*y[*,0]]]
	endif else begin
		message, string(FORMAT='(%"Cannot cross a [%i,%i] array with a [%i,%i] array")', $
		                xdims, ydims)
	endelse

	return, x_cross_y
end
