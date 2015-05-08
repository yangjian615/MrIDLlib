; docformat = 'rst'
;
; NAME:
;       MrVector_Cross
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
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/05/03  -   Written by Matthew Argall
;-
function MrVector_Cross, x, y
	compile_opt idl2
	on_error, 2
	
	xsz    = size(x)
	ysz    = size(y)
	xndims = xsz[0]
	yndims = ysz[0]
	xdims  = xndims eq 0 ? 0 : xsz[1:xsz[0]]
	ydims  = yndims eq 0 ? 0 : ysz[1:ysz[0]]
	nx     = xsz[xsz[0]+2]
	ny     = ysz[ysz[0]+2]

	;3 cross 3
	if (nx eq 3) and (ny eq 3) then begin
		x_cross_y = crossp(x, y)
		if xdims[0] eq 1 then x_cross_y = transpose(x_cross_y)

	;3 cross 3xN
	endif else if (nx eq 3) && (yndims eq 2) && (ydims[0] eq 3) then begin
		x_cross_y = [x[1]*y[2,*] - x[2]*y[1,*], $
		             x[2]*y[0,*] - x[0]*y[2,*], $
		             x[0]*y[1,*] - x[1]*y[0,*]]
	
	;3 cross Nx3
	endif else if (nx eq 3) && (yndims eq 2) && (ydims[1] eq 3) then begin
		x_cross_y = [[x[1]*y[*,2] - x[2]*y[*,1]], $
		             [x[2]*y[*,0] - x[0]*y[*,2]], $
		             [x[0]*y[*,1] - x[1]*y[*,0]]]
		             
	;3xN cross 3
	endif else if (xndims eq 2 && xdims[0] eq 3) && (ny eq 3) then begin
		x_cross_y = [x[1,*]*y[2] - x[2,*]*y[1], $
		             x[2,*]*y[0] - x[0,*]*y[2], $
		             x[0,*]*y[1] - x[1,*]*y[0]]
		             
	;Nx3 cross 3
	endif else if (xndims eq 2 && xdims[1] eq 3) && (ny eq 3) then begin
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
	endif else if (xndims eq 2 && xdims[1] eq 3) && $
	              (yndims eq 2 && ydims[1] eq 3) then begin
		x_cross_y = [[x[*,1]*y[*,2] - x[*,2]*y[*,1]], $
		             [x[*,2]*y[*,0] - x[*,0]*y[*,2]], $
		             [x[*,0]*y[*,1] - x[*,1]*y[*,0]]]
	
	;Vectors not given
	endif else begin
		xmsg = xndims eq 1 ? strtrim(xdims, 2) : '[' + strjoin(strtrim(xdims, 2), ',') + ']'
		ymsg = yndims eq 1 ? strtrim(ydims, 2) : '[' + strjoin(strtrim(ydims, 2), ',') + ']'
		message, 'Cannot cross X: ' + xmsg + ' with Y: ' + ymsg + '.'
	endelse

	return, x_cross_y
end
