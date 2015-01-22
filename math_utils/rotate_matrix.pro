; docformat = 'rst'
;
; NAME:
;
;       MATRIX_ROTATION
;
; PURPOSE:
;+
;       The purpose of this program is to rotate a 3x3 or 3x3xN matrix via a standard
;       rotation::
;
;           A' = R A R^T
;
;       Where R^T is the transpose of the rotation matrix R.
;
;   The tensor product of two fields is::
;                | HxKx  HxKy  HxKz |
;       P = HK = | HyKx  HyKy  HyKz | = HiKj
;                | HzKx  HzKy  HzKz |
;
;   In a different coordinate system, H and K are given by::
;       H'i = Aij Hj     K'l = Alm Km
;
;   The tensor H'K', then, is::
;       P' = H'i K'j = (Aij Hj) (Alm Km)
;                    = Aij Alm (HK)jm
;                    = Aij (Aml)^T (HK)jm
;                    = Aij (HK)jm (Aml)^T
;                    = A HK A^T
;
; :Categories:
;
;       Math Utilities, Rotations
;
; :Params:
;
;       R:                  in, required, type=3x3 or 3x3xN real numeric array
;                           The rotation matrix, or vector of rotation matrices, by which
;                               `A` is to be rotated
;       A:                  in, required, type=3x3 or 3x3xN real numeric array
;                           The matrix, or vector of matrices, to be rotated.
;
; :Keywords:
;
;       TRANS:              in, optional, private, type=Boolean, default=0
;                           The rotation R A is performed first, then `rotation_matrix.pro`
;                               is called again, recursively, to compute RA R^T. This
;                               keyword indicates that the recursive part is being
;                               performed and no more recursions should be done.
;                           
; :Returns:
;
;       A_PRIME:            The result of the matrix rotation.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2012
;
; :History::
;   Modification History::
;       03/17/2013  -   Written by Matthew Argall
;       03/19/2013  -   Must transpose differently if A is 3x3 - MRA
;       05/25/2013  -   Allowing A to be 3x3 caused complications with dimension checking.
;                           Fixed. - MRA
;-
function rotate_matrix, R, A, $
TRANS = trans
    compile_opt idl2
    on_error, 2
    
    dimsA  = size(A, /DIMENSIONS)
    dimsR  = size(R, /DIMENSIONS)
    ndimsA = size(A, /N_DIMENSIONS)
    ndimsB = size(B, /N_DIMENSIONS)
    typeA  = size(A, /TYPE)

    RA = make_array(dimsA, TYPE=typeA)
    
    ;3x3xN and 3x3xN
    if array_equal(dimsR[0:1], [3,3]) && array_equal(dimsA[0:1], [3,3]) then begin
        ;Fill the First Row                                                     ;R_[row,col] * A_[row,col]
        RA[0,0,*] = R[0,0,*]*A[0,0,*] + R[1,0,*]*A[0,1,*] + R[2,0,*]*A[0,2,*]   ;Rxx*Axx + Rxy*Ayx + Rzx*Azx
        RA[0,1,*] = R[0,1,*]*A[0,0,*] + R[1,1,*]*A[0,1,*] + R[2,1,*]*A[0,2,*]   ;Ryx*Axx + Ryy*Ayx + Ryz*Azx
        RA[0,2,*] = R[0,2,*]*A[0,0,*] + R[1,2,*]*A[0,1,*] + R[2,2,*]*A[0,2,*]   ;Rzx*Axx + Rzy*Ayx + Rzz*Azx 
    
        ;Fill the Second Row
        RA[1,0,*] = R[0,0,*]*A[1,0,*] + R[1,0,*]*A[1,1,*] + R[2,0,*]*A[1,2,*]   ;Rxx*Axy + Rxy*Ayy + Rxz*Azy
        RA[1,1,*] = R[0,1,*]*A[1,0,*] + R[1,1,*]*A[1,1,*] + R[2,1,*]*A[1,2,*]   ;Ryx*Axy + Ryy*Ayy + Ryz*Azy
        RA[1,2,*] = R[0,2,*]*A[1,0,*] + R[1,2,*]*A[1,1,*] + R[2,2,*]*A[1,2,*]   ;Rzx*Axy + Rzy*Ayy + Rzz*Azy
    
        ;Fill the Third Row
        RA[2,0,*] = R[0,0,*]*A[2,0,*] + R[1,0,*]*A[2,1,*] + R[2,0,*]*A[2,2,*]   ;Rxx*Axz + Rxy*Ayz + Rxz*Azz
        RA[2,1,*] = R[0,1,*]*A[2,0,*] + R[1,1,*]*A[2,1,*] + R[2,1,*]*A[2,2,*]   ;Ryx*Axz + Ryy*Ayz + Ryz*Azz
        RA[2,2,*] = R[0,2,*]*A[2,0,*] + R[1,2,*]*A[2,1,*] + R[2,2,*]*A[2,2,*]   ;Rzx*Axz + Rzy*Ayz + Rzz*Azz

        ;Now compute the other half of the rotation.
        if keyword_set(trans) eq 0 then begin
            case ndimsA of
                2: A_prime = rotate_matrix(RA, transpose(R, [1,0]), /TRANS)
                3: A_prime = rotate_matrix(RA, transpose(R, [1,0,2]), /TRANS)
                else: message, 'A must be a 3x3xN matrix.'
            endcase
        endif else return, RA
        
    ;Nx3x3 and Nx3x3
    endif else if array_equal(dimsR[1:2], [3,3]) && array_equal(dimsA[1:2], [3,3]) then begin
        ;Fill the First Row                                                     ;R_[row,col] * A_[row,col]
        RA[*,0,0] = R[*,0,0]*A[*,0,0] + R[*,1,0]*A[*,0,1] + R[*,2,0]*A[*,0,2]   ;Rxx*Axx + Rxy*Ayx + Rzx*Azx
        RA[*,0,1] = R[*,0,1]*A[*,0,0] + R[*,1,1]*A[*,0,1] + R[*,2,1]*A[*,0,2]   ;Ryx*Axx + Ryy*Ayx + Ryz*Azx
        RA[*,0,2] = R[*,0,2]*A[*,0,0] + R[*,1,2]*A[*,0,1] + R[*,2,2]*A[*,0,2]   ;Rzx*Axx + Rzy*Ayx + Rzz*Azx 
    
        ;Fill the Second Row
        RA[*,1,0] = R[*,0,0]*A[*,1,0] + R[*,1,0]*A[*,1,1] + R[*,2,0]*A[*,1,2]   ;Rxx*Axy + Rxy*Ayy + Rxz*Azy
        RA[*,1,1] = R[*,0,1]*A[*,1,0] + R[*,1,1]*A[*,1,1] + R[*,2,1]*A[*,1,2]   ;Ryx*Axy + Ryy*Ayy + Ryz*Azy
        RA[*,1,2] = R[*,0,2]*A[*,1,0] + R[*,1,2]*A[*,1,1] + R[*,2,2]*A[*,1,2]   ;Rzx*Axy + Rzy*Ayy + Rzz*Azy
    
        ;Fill the Third Row
        RA[*,2,0] = R[*,0,0]*A[*,2,0] + R[*,1,0]*A[*,2,1] + R[*,2,0]*A[*,2,2]   ;Rxx*Axz + Rxy*Ayz + Rxz*Azz
        RA[*,2,1] = R[*,0,1]*A[*,2,0] + R[*,1,1]*A[*,2,1] + R[*,2,1]*A[*,2,2]   ;Ryx*Axz + Ryy*Ayz + Ryz*Azz
        RA[*,2,2] = R[*,0,2]*A[*,2,0] + R[*,1,2]*A[*,2,1] + R[*,2,2]*A[*,2,2]   ;Rzx*Axz + Rzy*Ayz + Rzz*Azz

        ;Now compute the other half of the rotation.
        if keyword_set(trans) eq 0 then begin
            case ndimsA of
                2: A_prime = rotate_matrix(RA, transpose(R, [1,0]), /TRANS)
                3: A_prime = rotate_matrix(RA, transpose(R, [0,2,1]), /TRANS)
                else: message, 'A must be a Nx3x3 matrix.'
            endcase
        endif else return, RA
    
    endif else begin
        message, 'R cannot be used to rotate A.'
    endelse

    return, A_prime
end