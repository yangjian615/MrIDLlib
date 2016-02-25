; docformat = 'rst'
;
; NAME:
;
;       MrMatrix_Multiply
;
; PURPOSE:
;+
;   Multiply one matrix by another. This function is equivalent to the ## operator,
;   but works for two Nx3x3 or two 3x3xN matrices.
;
;   Hint: If you print a matrix, and it looks like it would if you wrote it for a math
;         class, then your matrices are suitable for this function.
;
; :Examples:
;   Interchange row order from {1, 2, 3} to {3, 1, 2}
;       A = [[0, 0, 1], [1, 0, 0], [0, 1, 0]]
;       B = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
;       C = MrMatrix_Multiply(A, B)
;           4       5       6
;           7       8       9
;           1       2       3
;       C = A ## B
;           4       5       6
;           7       8       9
;           1       2       3
;
; :Categories:
;       Math Utilities
;
; :Params:
;       A:              in, required, type=Nx3x3 or 3x3xN real numeric array
;                       The matrix by which to multiply `B`.
;       B:              in, required, type=Nx3x3 or 3x3xN real numeric array
;                       The matrix to be multiplied by `A`.
;
; :Returns:
;       AB:             The result of the matrix multiplication.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History::
;   Modification History::
;       2015/01/23  -   Written by Matthew Argall
;-
function MrMatrix_Multiply, A, B
	compile_opt idl2
	on_error, 2

	;Determine dimension sizes
	dimsA  = size(A, /DIMENSIONS)
	dimsB  = size(B, /DIMENSIONS)
	ndimsA = size(A, /N_DIMENSIONS)
	ndimsB = size(B, /N_DIMENSIONS)
	typeA  = size(A, /TYPE)

	;Allocate memory
	AB = make_array(dimsA, TYPE=typeA)

	;3x3xN and 3x3xN
	if array_equal(dimsA[0:1], [3,3]) && array_equal(dimsB[0:1], [3,3]) then begin
		;Fill the First Row                                                     ;A_[row,col] * B_[row,col]
		AB[0,0,*] = A[0,0,*]*B[0,0,*] + A[1,0,*]*B[0,1,*] + A[2,0,*]*B[0,2,*]   ;Axx*Bxx + Axy*Byx + Azx*Bzx
		AB[0,1,*] = A[0,1,*]*B[0,0,*] + A[1,1,*]*B[0,1,*] + A[2,1,*]*B[0,2,*]   ;Ayx*Bxx + Ayy*Byx + Ayz*Bzx
		AB[0,2,*] = A[0,2,*]*B[0,0,*] + A[1,2,*]*B[0,1,*] + A[2,2,*]*B[0,2,*]   ;Azx*Bxx + Azy*Byx + Azz*Bzx 

		;Fill the Second Row
		AB[1,0,*] = A[0,0,*]*B[1,0,*] + A[1,0,*]*B[1,1,*] + A[2,0,*]*B[1,2,*]   ;Axx*Bxy + Axy*Byy + Axz*Bzy
		AB[1,1,*] = A[0,1,*]*B[1,0,*] + A[1,1,*]*B[1,1,*] + A[2,1,*]*B[1,2,*]   ;Ayx*Bxy + Ayy*Byy + Ayz*Bzy
		AB[1,2,*] = A[0,2,*]*B[1,0,*] + A[1,2,*]*B[1,1,*] + A[2,2,*]*B[1,2,*]   ;Azx*Bxy + Azy*Byy + Azz*Bzy

		;Fill the Third Row
		AB[2,0,*] = A[0,0,*]*B[2,0,*] + A[1,0,*]*B[2,1,*] + A[2,0,*]*B[2,2,*]   ;Axx*Bxz + Axy*Byz + Axz*Bzz
		AB[2,1,*] = A[0,1,*]*B[2,0,*] + A[1,1,*]*B[2,1,*] + A[2,1,*]*B[2,2,*]   ;Ayx*Bxz + Ayy*Byz + Ayz*Bzz
		AB[2,2,*] = A[0,2,*]*B[2,0,*] + A[1,2,*]*B[2,1,*] + A[2,2,*]*B[2,2,*]   ;Azx*Bxz + Azy*Byz + Azz*Bzz
	
	;Nx3x3 and Nx3x3
	endif else if array_equal(dimsA[1:2], [3,3]) && array_equal(dimsB[1:2], [3,3]) then begin
		;Fill the First Row                                                     ;A_[row,col] * B_[row,col]
		AB[*,0,0] = A[*,0,0]*B[*,0,0] + A[*,1,0]*B[*,0,1] + A[*,2,0]*B[*,0,2]   ;Axx*Bxx + Axy*Byx + Azx*Bzx
		AB[*,0,1] = A[*,0,1]*B[*,0,0] + A[*,1,1]*B[*,0,1] + A[*,2,1]*B[*,0,2]   ;Ayx*Bxx + Ayy*Byx + Ayz*Bzx
		AB[*,0,2] = A[*,0,2]*B[*,0,0] + A[*,1,2]*B[*,0,1] + A[*,2,2]*B[*,0,2]   ;Azx*Bxx + Azy*Byx + Azz*Bzx 

		;Fill the Second Row
		AB[*,1,0] = A[*,0,0]*B[*,1,0] + A[*,1,0]*B[*,1,1] + A[*,2,0]*B[*,1,2]   ;Axx*Bxy + Axy*Byy + Axz*Bzy
		AB[*,1,1] = A[*,0,1]*B[*,1,0] + A[*,1,1]*B[*,1,1] + A[*,2,1]*B[*,1,2]   ;Ayx*Bxy + Ayy*Byy + Ayz*Bzy
		AB[*,1,2] = A[*,0,2]*B[*,1,0] + A[*,1,2]*B[*,1,1] + A[*,2,2]*B[*,1,2]   ;Azx*Bxy + Azy*Byy + Azz*Bzy

		;Fill the Third Row
		AB[*,2,0] = A[*,0,0]*B[*,2,0] + A[*,1,0]*B[*,2,1] + A[*,2,0]*B[*,2,2]   ;Axx*Bxz + Axy*Byz + Axz*Bzz
		AB[*,2,1] = A[*,0,1]*B[*,2,0] + A[*,1,1]*B[*,2,1] + A[*,2,1]*B[*,2,2]   ;Ayx*Bxz + Ayy*Byz + Ayz*Bzz
		AB[*,2,2] = A[*,0,2]*B[*,2,0] + A[*,1,2]*B[*,2,1] + A[*,2,2]*B[*,2,2]   ;Azx*Bxz + Azy*Byz + Azz*Bzz

	endif else begin
		message, 'R cannot be used to rotate A.'
	endelse

	return, AB
end