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
;   See the example program at the end of this document::
;       IDL> .run MrEulerMatrix
;
; :Params:
;       ALPHA:          in, optional, type=float, default=0.0
;                       Radians by which to rotate a coordinate system about the x-axis.
;       BETA:           in, optional, type=float, default=0.0
;                       Radians by which to rotate a coordinate system about the y-axis.
;       GAMMA:          in, optional, type=float, default=0.0
;                       Radians by which to rotate a coordinate system about the z-axis.
;
; :Keywords:
;       ANGLES:         in, optional, type=Boolean, default=0
;                       If set then `ALPHA`, `BETA` and `GAMMA` are angles, not radians.
;       ORDER:          in, optional, type=intarr(3), default=[3\,2\,1]
;                       Order in which `ALPHA`, `BETA` and `GAMMA` should be multiplied.
;                           The default is `GAMMA` ## `BETA` ## `ALPHA`.
;       MATH:           in, optional, type=boolean, default=0
;                       If set, `EULER_MATRIX` will be math-like instead of IDL-like.
;
;                           MATH:
;                                   v = [[vx], [vy], [vz]]
;                                   A = [[ Axx,  Axy,  Axz], $
;                                        [ Ayx,  Ayy,  Ayz], $
;                                        [ Azx,  Azy,  Azz]]
;
;                                     v'  =    A   ##   v
;                                   [1,3] =  [3,3] ## [1,3]
;                                             ^          ^
;
;                                   | vx' |   | Axx  Axy  Axz |  | vx |
;                                   | vy' | = | Ayx  Ayy  Ayz |  | vy |
;                                   | vz' |   | Azx  Azy  Azz |  | vz |
;
;                           IDL:
;                                   v = [vx, vy, vz]
;                                   A = [[ Axx,  Ayx,  Azx], $
;                                        [ Axy,  Ayy,  Azy], $
;                                        [ Axz,  Ayz,  Azz]]
;
;                                    v' =   A   #   v
;                                   [3] = [3,3] #  [3]
;                                            ^      ^
;
;                                                     | Axx  Ayx  Azx |  
;                                   | vx' vy' vz' | = | Axy  Ayy  Azy |  | vx vy vz |
;                                                     | Axz  Ayz  Azz |
;
; :Returns:
;       EULER_MATRIX:   The result of the three Euler rotations.
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
;       2015/02/19  -   Written by Matthew Argall
;-
function MrEulerMatrix, alpha, beta, gamma, $
ANGLES=angles, $
ORDER=order, $
MATH=math
	compile_opt idl2
	on_error, 2

	;Default to no rotation
	angles = keyword_set(angles)
	math   = keyword_set(math)
	_alpha = n_elements(alpha) eq 0 ? 0.0 : alpha
	_beta  = n_elements(beta)  eq 0 ? 0.0 : beta
	_gamma = n_elements(gamma) eq 0 ? 0.0 : gamma
	if n_elements(order) eq 0 then order = [1, 2, 3]

	;Convert to radians?
	if angles then begin
		_alpha *= !dtor
		_beta  *= !dtor
		_gamma *= !dtor
	endif

	;Sines and Cosines
	sin_alpha = sin(_alpha)
	cos_alpha = cos(_alpha)
	sin_beta  = sin(_beta)
	cos_beta  = cos(_beta)
	sin_gamma = sin(_gamma)
	cos_gamma = cos(_gamma)

	;Rotation about X-axis
	euler_x = [[  1.0,    0.0,        0.0], $
	           [  0.0,  cos_alpha,  sin_alpha], $
	           [  0.0, -sin_alpha,  cos_alpha]]

	;Rotation about Y-axis
	euler_y = [[  cos_beta,   0.0, -sin_beta], $
	           [    0.0,      1.0,    0.0], $
	           [  sin_beta,   0.0,  cos_beta]]

	;Rotation about Z-axis
	euler_z = [[  cos_gamma,  sin_gamma,  0.0], $
	           [ -sin_gamma,  cos_gamma,  0.0], $
	           [    0.0,        0.0,      1.0]]

	;Form the complete rotation
	; #  - Columns * Rows        -- IDL's outer product
	; ## - Rows    * Columns     -- IDL's inner product
	case 1 of
		array_equal(order, [1, 2, 3]): euler_matrix = euler_x ## euler_y ## euler_z
		array_equal(order, [2, 1, 3]): euler_matrix = euler_y ## euler_x ## euler_z
		array_equal(order, [1, 3, 2]): euler_matrix = euler_x ## euler_z ## euler_y
		array_equal(order, [3, 1, 2]): euler_matrix = euler_z ## euler_x ## euler_y
		array_equal(order, [2, 3, 1]): euler_matrix = euler_y ## euler_z ## euler_x
		array_equal(order, [3, 2, 1]): euler_matrix = euler_z ## euler_y ## euler_x
		else: message, 'Invalid ORDER.'
	endcase

	;Return IDL-like instead of math-like?
	if math eq 0 then euler_matrix = transpose(euler_matrix)

	return, euler_matrix
end

;-----------------------------------------------------
; Main level program: IDL> .run MrEulerMatrix \\\\\\\\
;-----------------------------------------------------
;EXAMPLE 1
;   - Demonstrate the difference between MATH and IDL matrices
;   - Form a row and column vector, both 45 degrees from the X-axis.
;   - Rotate about z by 45 degrees to bring the x-axis in-line with the vector
vec_idl  = [1.0, 1.0, 0.0]
vec_math = [[1.0], [1.0], [0.0]]
alpha    = 0.0
beta     = 0.0
gamma    = 45.0

;Create the rotation matrix
mat_idl  = MrEulerMatrix(alpha, beta, gamma, /ANGLES)
mat_math = MrEulerMatrix(alpha, beta, gamma, /ANGLES, /MATH)

;Multiply
vec_idl_rot  = mat_idl   # vec_idl
vec_math_rot = mat_math ## vec_math

;Print results
print, 'Results of:'
print, FORMAT='(%"   vec    = [%5.2f, %5.2f, %5.2f]")', vec_idl
print, FORMAT='(%"   angles = [%5.2f, %5.2f, %5.2f]")', [alpha, beta, gamma]
print, '   matrix = MrEulerMatrix(alpha, beta, gamma, /ANGLES)'
print, '   result = matrix # vec'
print, ''
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                                   mat_idl[*,0]
print, FORMAT='(%" | %5.2f %5.2f %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f %5.2f |")', vec_idl_rot, mat_idl[*,1], vec_idl[*]
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                                   mat_idl[*,2]
print, ''
help, mat_idl, vec_idl, vec_idl_rot
print, ''
print, ''
print, 'Results of:'
print, FORMAT='(%"   vec    = [[%5.2f], [%5.2f], [%5.2f]]")', vec_math
print, FORMAT='(%"   angles = [%5.2f, %5.2f, %5.2f]")', [alpha, beta, gamma]
print, '   matrix = MrEulerMatrix(alpha, beta, gamma, /ANGLES, /MATH)'
print, '   result = matrix ## vec'
print, ''
print, FORMAT='(%" | %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_math_rot[0], mat_math[*,0], vec_math[0]
print, FORMAT='(%" | %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_math_rot[1], mat_math[*,1], vec_math[1]
print, FORMAT='(%" | %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_math_rot[2], mat_math[*,2], vec_math[2]
print, ''
help, mat_math, vec_math, vec_math_rot
print, '----------------------------------------'
print, ''



;EXAMPLE 2
;   - Create a vector 45 degrees from the X-axis and 45 degrees up from the XY-plane.
;   - Rotate about z to align the xz-plane with the vector
;   - Rotate about y to align the x-axis with the vector
vec     = [[1.0], [1.0], [1.41]]
alpha   = 0.0
beta    = -45.0
gamma   = 45.0
mat     = MrEulerMatrix(alpha, beta, gamma, /ANGLES, /MATH)
vec_rot = mat ## vec

;Print results
print, 'Results of:'
print, FORMAT='(%"   vec    = [[%5.2f], [%5.2f], [%5.2f]]")', vec
print, FORMAT='(%"   angles = [%5.1f, %5.1f, %5.1f]")', [alpha, beta, gamma]
print, '   matrix = MrEulerMatrix(alpha, beta, gamma, /ANGLES, /MATH)'
print, '   result = matrix ## vec'
print, ''
print, FORMAT='(%" | %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_rot[0], mat[*,0], vec[0]
print, FORMAT='(%" | %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_rot[1], mat[*,1], vec[1]
print, FORMAT='(%" | %5.2f |   | %5.2f, %5.2f, %5.2f | | %5.2f |")', vec_rot[2], mat[*,2], vec[2]
print, ''
help, mat, vec, vec_rot
print, '----------------------------------------'
print, ''

end