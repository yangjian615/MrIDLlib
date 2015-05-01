; docformat = 'rst'
;
; NAME:
;       MrEulerMatrix
;
; PURPOSE:
;+
;   Create a coordinate system tranformation matrix out of any number of pure
;   Euler transformations. Note: results are intended to rotate a coordinate
;   system, not a vector. 
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Examples:
;   See the example program at the end of this document::
;       IDL> .run MrEulerMatrix
;
; :Params:
;       EULER_ANGLES:   in, required, type=float/fltarr
;                       Euler angles of rotation, specified in radians.
;       SEQUENCE:       in, optional, type=string/strarr, default=['Z'\, 'Y'\, 'X']
;                       The sequence in which the rotations are applied. If V is a
;                           column vector, and SEQUENCE has its default value, then
;                           the result is obtained as R = Z ## Y ## X ## V, where
;                           Z, Y and X are rotations about said axes.
;                       
;
; :Keywords:
;       ROTATE_VECTOR:  in, optional, type=boolean, default=0
;                       If set, the angles are intended to rotate a vector. By default
;                           the coordinate system is rotated.
;       DEGREES:        in, optional, type=Boolean, default=0
;                       If set then `ALPHA`, `BETA` and `GAMMA` are degrees, not radians.
;       MATH:           in, optional, type=boolean, default=0
;                       If set, `ROTM` will be math-like instead of IDL-like.
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
;       ROTM:           The result of the three Euler rotations.
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
;       2015/02/19  -   Written by Matthew Argall
;       2015/04/30  -   Generalzied. Accept any number of rotation angles. Added
;                           the ROTATE_VECTOR keyword. - MRA
;-
function MrEulerMatrix, euler_angles, sequence, $
DEGREES=degrees, $
MATH=math, $
ROTATE_VECTOR=rotate_vector
	compile_opt idl2
	on_error, 2

	;Default to no rotation
	degrees       = keyword_set(degrees)
	math          = keyword_set(math)
	rotate_vector = keyword_set(rotate_vector)
	if n_elements(sequence) eq 0 then sequence = ['Z', 'Y', 'X']
	
	;Check for conflicts
	if n_elements(euler_angles) ne n_elements(sequence) $
		then message, 'EULER_ANGLES and SEQUENCE must have the same number of elements.'

	;Convert to radians?
	angles = degrees ? euler_angles * !dpi / 180.0D : euler_angles
	if rotate_vector then angles = -angles

	;Step through each rotation.
	rotm = identity(3)
	for i = n_elements(sequence) - 1, 0, -1 do begin
		if euler_angles[i] eq 0 then continue
		
		;Sines and Cosines
		sinA = sin(angles[i])
		cosA = cos(angles[i])
		
		;Create the matrices so they appear math-like when written.
		;  - This requires the ## operator
		case strupcase(sequence[i]) of
			;Rotation about X-axis
			'X': temp_rotm = [[  1.0,   0.0,   0.0 ], $
			                  [  0.0,  cosA,  sinA ], $
			                  [  0.0, -sinA,  cosA ]]

			;Rotation about Y-axis
			'Y': temp_rotm = [[  cosA,   0.0, -sinA ], $
			                  [   0.0,   1.0,   0.0 ], $
			                  [  sinA,   0.0,  cosA ]]

			;Rotation about Z-axis
			'Z': temp_rotm = [[  cosA,  sinA,  0.0 ], $
			                  [ -sinA,  cosA,  0.0 ], $
			                  [   0.0,   0.0,  1.0 ]]
			
			;Unknown axis of rotation
			else: message, 'Rotation axis not recognized: "' + sequence[i] + '".'
		endcase
		
		;Combine the rotations
		rotm = temp_rotm ## rotm
	endfor

	;Return IDL-like instead of math-like?
	if math eq 0 then rotm = transpose(rotm)

	return, rotm
end

;-----------------------------------------------------
; Main level program: IDL> .run MrEulerMatrix \\\\\\\\
;-----------------------------------------------------
;EXAMPLE 1
;   - Demonstrate the difference between MATH and IDL matrices
;   - Form a row and column vector, both 45 degrees from the X-axis.
;   - Rotate about z by 45 degrees to bring the x-axis in-line with the vector
vec_idl      = [1.0, 1.0, 0.0]
vec_math     = [[1.0], [1.0], [0.0]]
euler_angles = 45.0
sequence     = 'Z'

;Create the rotation matrix
mat_idl  = MrEulerMatrix(euler_angles, sequence, /DEGREES)
mat_math = MrEulerMatrix(euler_angles, sequence, /DEGREES, /MATH)

;Multiply
vec_idl_rot  = mat_idl   # vec_idl
vec_math_rot = mat_math ## vec_math

;Print results
print, 'Results of:'
print, FORMAT='(%"   vec      = [%5.2f, %5.2f, %5.2f]")', vec_idl
print, FORMAT='(%"   angles   = %5.2f")',                 euler_angles
print, FORMAT='(%"   sequence = %s")',                    sequence
print, '   matrix = MrEulerMatrix(angles, sequence, /DEGREES)'
print, '   result = matrix # vec'
print, ''
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                                   mat_idl[*,0]
print, FORMAT='(%" | %5.2f %5.2f %5.2f | = | %5.2f, %5.2f, %5.2f | | %5.2f %5.2f %5.2f |")', vec_idl_rot, mat_idl[*,1], vec_idl[*]
print, FORMAT='(%"                         | %5.2f, %5.2f, %5.2f | ")',                                   mat_idl[*,2]
print, ''
help, mat_idl, vec_idl, vec_idl_rot
print, '----------------------------------------'
print, ''
print, 'Results of:'
print, FORMAT='(%"   vec      = [[%5.2f], [%5.2f], [%5.2f]]")', vec_math
print, FORMAT='(%"   angles   = %5.2f")',                 euler_angles
print, FORMAT='(%"   sequence = %s")',                    sequence
print, '   matrix = MrEulerMatrix(angles, sequence, /DEGREES, /MATH)'
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
;   - Create a vector 45 degrees from the X-axis and 45 degrees down from the Z-axis.
;   - Rotate ccw about z to align the xz-plane with the vector
;   - Rotate  cw about y to align the x-axis with the vector
vec          = [[1.0], [1.0], [1.41]]
euler_angles = [-45, 45]
sequence     = ['Y', 'Z']
mat          = MrEulerMatrix(euler_angles, sequence, /DEGREES, /MATH)
vec_rot      = mat ## vec

;Print results
print, 'Results of:'
print, FORMAT='(%"   vec    = [[%5.2f], [%5.2f], [%5.2f]]")', vec
print, FORMAT='(%"   angles   = [%6.2f, %6.2f]")',            euler_angles
print, FORMAT='(%"   sequence = [%s, %s]")',                  sequence
print, '   matrix = MrEulerMatrix(angles, sequence, /DEGREES, /MATH)'
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