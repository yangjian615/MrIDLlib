; docformat = 'rst'
;
; NAME:
;   MrVector_Rotate
;
; PURPOSE:
;+
;   The purpose of this program is to provide a means of rotating an array of 3D 
;   vectors about a particular axis or by means of a single rotation matrix. The
;   method of rotation is as follows::
;       R[3x3] and x[3x1]   then take R##transpose(x)
;       R[3x3] and x[1x3]   then take R##x
;       R[3x3] and x[3xN]   then take R##transpose(x) for each 3-vector in x
;       R[3x3] and x[Nx3]   then take R##x for each 3-vector in x
;       R[3,3,N] and x[3xN] then take R##x for each 3x3 R and 3x1 x
;       R[3,3,N] and x[Nx3] then take R##transpose(x) for each 3x3 R and 1x3 x
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Examples:
;   See the main-level example program at the end of this document::
;       IDL> .run rotate_vector
;
; :Params:
;       R:              in, required, type=number or 3x3 numeric array
;                       The rotation matrix used to transform `X` into a new
;                           coordinate system. 
;       X:              in, required, type=Numeric array [3xN]
;                       The array of 3D vectors to be rotated.
;
; :Returns:
;       X_PRIME:        The result of rotating `X` using `R`.
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
;       2015-05-01  -   Written by Matthew Argall
;-
function MrVector_Rotate, R, x, $
	on_error, 2

	;Check R and X
	sr = size(r)
	sx = size(x)
	nr     = sr[sr[0]+2]
	nx     = sx[sx[0]+2]
	nrDims = sr[0]
	nxDims = sx[0]
	rDims  = nrDims eq 0 ? 0 : sr[1:sr[0]]
	xDims  = nxDims eq 0 ? 0 : sx[1:sx[0]]

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



