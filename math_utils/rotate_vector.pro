; docformat = 'rst'
;
; NAME:
;       ROTATE_VECTOR
;
; PURPOSE:
;+
;       The purpose of this program is to provide a means of rotating an array of 3D 
;       vectors about a particular axis or by means of a single rotation matrix. The
;       method of rotation is as follows::
;           R[3x3] and x[3x1]   then take R##transpose(x)
;           R[3x3] and x[1x3]   then take R##x
;           R[3x3] and x[3xN]   then take R##transpose(x) for each 3-vector in x
;           R[3x3] and x[Nx3]   then take R##x for each 3-vector in x
;           R[3,3,N] and x[3xN] then take R##x for each 3x3 R and 3x1 x
;           R[3,3,N] and x[Nx3] then take R##transpose(x) for each 3x3 R and 1x3 x
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Examples:
;   See the main-level example program at the end of this document::
;       IDL> .run rotate_vector
;
; :Params:
;       X:              in, required, type=Numeric array [3xN]
;                       The array of 3D vectors to be rotated.
;       R:              in, required, type=number or 3x3 numeric array
;                       If R is a scalar number, then it must be used in conjunction with
;                           `ALPHA`, `BETA`, or `GAMMA` and represents the angle about
;                           the x-, y-, or z- axis, respectively, by which to rotate `X`.
;                       If R is a 3x3 numeric array, then it is the rotation matrix used
;                           to transform `X` into a new coordinate system. 
;
; :Keywords:
;       ALPHA:          in, optional, type=Boolean, default=0
;                       Indicate that R represents the counter-clockwise angle of rotation
;                           about the x-axis by which to rotate `X`.
;       BETA:           in, optional, type=Boolean, default=0
;                       Indicate that R represents the counter-clockwise angle of rotation
;                           about the y-axis by which to rotate `X`.
;       GAMMA:          in, optional, type=Boolean, default=0
;                       Indicate that R represents the counter-clockwise angle of rotation
;                           about the z-axis by which to rotate `X`.
;       EULER:          in, optional, type=Boolean, default=0
;                       If set then `ALPHA`, `BETA` and `GAMMA` represent the traditional
;                           Euler angles and that the rotation is though of as rotating
;                           the coordinate system, not the vectors.
;
; :Returns:
;       X_PRIME:        The result of rotating `X` using `R`.
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
;       11/14/2011  -   Written by Matthew Argall
;       13/02/2013  -   Changed order of inputs from "x, R" to "R, x" as it is more
;                           natural in a mathematical sense. - MRA
;       14/04/2013  -   Added cases for R begin 3x3xN. - MRA
;       2015/02/26  -   Clearified and cleaned up code. - MRA
;-
function rotate_vector, R, x, $
;KEYWORDS
ALPHA = alpha, $
BETA = beta, $
GAMMA = gamma, $
EULER = euler
	on_error, 2

	;Check R and X
	nx     = n_elements(x)
	nr     = n_elements(r)
	rDims  = size(R, /DIMENSIONS)
	xDims  = size(x, /DIMENSIONS)
	nrDims = size(R, /N_DIMENSIONS)
	nxDims = size(x, /N_DIMENSIONS)

	;determine whether r is an angle or a rotation matrix
	;if it is an angle, build the proper orthogonal rotation
	;matrix for rotating x around the chosen axis
	if nr eq 1 then begin
		if abs(R) gt 2.0*!pi then message, "R not a valid angle: -2*pi <= R <= 2*pi"

		;if alpha is set, rotate counter-clockwise about x
		if keyword_set(alpha) then begin
			cosalpha = cos(R)
			sinalpha = sin(R)

			R = [[0,        0,         1], $
		         [0, cosalpha, -sinalpha], $
		         [0, sinalpha, cosalpha]]
		;if beta is set, rotate counter-clockwise about y
		endif else if keyword_set(beta) then begin
			cosbeta = cos(R)
			sinbeta = sin(R)

			R = [[ cosbeta, 0, sinbeta], $
		         [       0, 1,       0], $
		         [-sinbeta, 0, sinbeta]]
		;if beta is set, rotate counter-clockwise about z
		endif else if keyword_set(gamma) then begin
			cosgamma = cos(R)
			singamma = sin(R)

			R = [[cosgamma, -singamma, 0], $
		         [singamma,  cosgamma, 0], $
		         [       0,         0, 1]]
		;if R is an angle, the rotation axis must be known
		endif else begin
			message, 'If R is an angle, then ALPHA, BETA, OR GAMMA must be set'
		endelse

		;if the angle provided is an Euler angle, take the transpose
		;of the rotation matrix to rotate the axes instead of the vector
		if keyword_set(euler) then R = transpose(R)
	endif

;The goal is to always perform mathematical transformations:
;
; | x' |   | Rxx  Rxy  Rxz | | x |
; | y' | = | Ryx  Ryy  Ryz | | y |
; | z' |   | Rzx  Rzy  Rzz | | z |
;
;   x'  =   R   ##   x
; [1,3] = [3,3] ## [1,3]
;
;-----------------------------------------------------
;R=[3,3], x=[3,1] \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if (nx eq 3) && (nxDims eq 1) && (nrDims eq 2) then begin
		x_prime = reform(matrix_multiply(x, R, /atranspose))

;-----------------------------------------------------
;R=[3,3], x=[1,3] \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if (nx eq 3) && (nxDims eq 2) && (nrDims eq 2) then begin
		x_prime = matrix_multiply(x, R)

;-----------------------------------------------------
;R=[3,3], x=[3xN] \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;R##transpose(x) for each 3x0 vector
	endif else if (nrDims eq 2) && (xDims[0] eq 3) then begin
		x_prime = [dot_product(R[*,0], x), $
		           dot_product(R[*,1], x), $
		           dot_product(R[*,2], x)]

;-----------------------------------------------------
;R=[3,3], x=[Nx3] \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;R##x for each 1x3 vector
	endif else if (nrDims eq 2) && (xDims[1] eq 3) then begin
		xT = transpose(x)
		x_prime = [dot_product(R[*,0], xT), $
		           dot_product(R[*,1], xT), $
		           dot_product(R[*,2], xT)]

;-----------------------------------------------------
;R=[3,3,N], x=[3,N] \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if (nrDims eq 3) && (xDims[0] eq 3) && (rDims[2] eq xDims[1]) then begin
        x_prime = transpose([[dot_product(reform(R[*,0,*]), x)], $
		                     [dot_product(reform(R[*,1,*]), x)], $
		                     [dot_product(reform(R[*,2,*]), x)]])

;-----------------------------------------------------
;R=[3,3,N], x=[N,3] \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if (nrDims eq 3) && (xDims[1] eq 3) && (rDims[2] eq xDims[0]) then begin
		xT = transpose(x)
		x_prime = transpose([[dot_product(reform(R[*,0,*]), xT)], $
		                     [dot_product(reform(R[*,1,*]), xT)], $
		                     [dot_product(reform(R[*,2,*]), xT)]])

	endif else begin
		message, 'R and x are not compatible for rotating'
	endelse

	return, x_prime
end


;-----------------------------------------------------
; Main-level Example Program: IDL> .r Rotate_Vector \\
;-----------------------------------------------------
;DATA
;   - Create a vector 45 degrees from the X-axis and 45 degrees up from the XY-plane.
;   - Rotate about z to align the xz-plane with the vector
;   - Rotate about y to align the x-axis with the vector
vec = [[1.0], [1.0], [1.41]]
mat = [[      0.5,           0.5,      sqrt(2.0)/2.0  ], $
       [-sqrt(2.0)/2.0, sqrt(2.0)/2.0,      0.0       ], $
       [     -0.5,          -0.5,      sqrt(2.0)/2.0  ]]

;EXAMPLE 1
;   - R=[3,3], x=[3,1]
temp_vec = transpose(vec)
vec_rot  = rotate_vector(mat, temp_vec)

print, ''
print, 'R=[3,3], x=[3,1]'
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                               mat[*,0]
print, FORMAT='(%" | %5.2f %5.2f %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f %5.2f |")', vec_rot, mat[*,1], temp_vec
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                               mat[*,2]
print, '----------------------------------------------'
print, ''


;EXAMPLE 2
;   - R=[3,3], x=[1,3]
vec_rot = rotate_vector(mat, vec)

print, 'R=[3,3], x=[3,1]'
print, FORMAT='(%" | %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_rot[0], mat[*,0], vec[0]
print, FORMAT='(%" | %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_rot[1], mat[*,1], vec[1]
print, FORMAT='(%" | %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_rot[2], mat[*,2], vec[2]
print, '----------------------------------------------'
print, ''


;EXAMPLE 3
;   - R=[3,3], x=[3,N]
temp_vec = rebin(reform(vec, 3, 1), 3, 2)
vec_rot  = rotate_vector(mat, temp_vec)

print, 'R=[3,3], x=[3,N]'
print, FORMAT='(%" | %5.2f %5.2f %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f %5.2f |")', vec_rot[*,0], mat[*,0], temp_vec[*,0]
print, FORMAT='(%" | %5.2f %5.2f %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f %5.2f |")', vec_rot[*,1], mat[*,1], temp_vec[*,1]
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                                    mat[*,2]
print, '----------------------------------------------'
print, ''


;EXAMPLE 2
;   - R=[3,3], x=[N,3]
temp_vec = rebin(vec, 2, 3)
vec_rot = rotate_vector(mat, temp_vec)

print, 'R=[3,3], x=[N,3]'
print, FORMAT='(%" | %5.2f %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f |")', vec_rot[0,*], mat[*,0], temp_vec[*,0]
print, FORMAT='(%" | %5.2f %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f |")', vec_rot[1,*], mat[*,1], temp_vec[*,1]
print, FORMAT='(%" | %5.2f %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f |")', vec_rot[2,*], mat[*,2], temp_vec[*,2]
print, '----------------------------------------------'
print, ''


;EXAMPLE 3
;   - R=[3,3,N], x=[3,N]
temp_vec = rebin(reform(vec, 3, 1), 3, 2)
temp_mat = rebin(mat, 3, 3, 2)
vec_rot  = rotate_vector(mat, temp_vec)

print, 'R=[3,3,N], x=[3,N] ... (only one rotation matrix shown)'
print, FORMAT='(%" | %5.2f %5.2f %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f %5.2f |")', vec_rot[*,0], mat[*,0,0], temp_vec[*,0]
print, FORMAT='(%" | %5.2f %5.2f %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f %5.2f |")', vec_rot[*,1], mat[*,1,0], temp_vec[*,1]
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                                    mat[*,2,0]
print, '----------------------------------------------'
print, ''


;EXAMPLE 2
;   - R=[3,3,N], x=[N,3]
temp_vec = rebin(vec, 2, 3)
temp_mat = rebin(mat, 3, 3, 2)
vec_rot = rotate_vector(temp_mat, temp_vec)

print, 'R=[3,3,N], x=[N,3] ... (only one rotation matrix shown)'
print, FORMAT='(%" | %5.2f %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f |")', vec_rot[0,*], mat[*,0], temp_vec[*,0]
print, FORMAT='(%" | %5.2f %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f |")', vec_rot[1,*], mat[*,1], temp_vec[*,1]
print, FORMAT='(%" | %5.2f %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f |")', vec_rot[2,*], mat[*,2], temp_vec[*,2]
print, '----------------------------------------------'
print, ''

end



