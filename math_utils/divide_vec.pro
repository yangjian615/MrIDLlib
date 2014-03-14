; docformat = 'rst'
;
; NAME:
;       DIVIDE_VEC
;
; PURPOSE:
;+
;       The purpose of this program is to vectorize the division of arrays of vectors.
;       Additional element-by-element matrix division schemes have also been implemented,
;       but are not vectorized::
;           1. A[1xM] and B[1xM] then A / B
;           2. A[3xM] and B[1x3] then A[*,i] / B
;           3. A[3xM] and B[1xM] then vectorize A[i,*] / B
;           4. A[3xM] and B[3xM] then vectorize A[i,*] / B[i,*]
;           5. A[NxM] and B[1xM] then loop through A[i,*] / B
;           6. A[NxM] and B[NxM] then loop through A[i,*] / B[i,*]
;
; :Examples:
;       See the main level program at the end of the file::
;
;           IDL> .r divide_vec
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       A:              in, optional, type=real numeric array(NxM)
;                       The array to be divided by `B`.
;       B:              in, optional, type=real numeric array
;                       The array by which to divide `A`. Must have the same number of
;                           rows as `A`. 
;
; :Keywords:
;
; :Returns:
;       A_DIVIDEDBY_B:  The result of dividing the columns of A by (the columns of) B.
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
;       01/31/2012  -   Written by Matthew Argall
;       02/13/2013  -   Added cases for 3xM - MRA
;-
function divide_vec, A, B
    compile_opt idl2
    on_error, 2

	sz_A = size(A)
	sz_B = size(B)
	
	n_A = sz_A[sz_A[0]+2]
	n_B = sz_B[sz_B[0]+2]	
	
	;1. if A and B are 1D and have the same number of elements, then divide A by B
	if sz_A[0] eq 1 && sz_B[0] eq 1 and n_A eq n_B then begin
		A_dividedby_B = A / B
		
	;2. if A is 3xM and B has 3 elements
	endif else if sz_A[0] eq 3 and n_B eq 3 then begin
	    A_dividedby_B = dblarr(sz_A[1:2])
	    A_dividedby_B[0,*] = A[0,*] / B[0]
	    A_dividedby_B[1,*] = A[1,*] / B[1]
	    A_dividedby_B[2,*] = A[2,*] / B[2]
		
	;3. if A is 3xM and B has M elements
	endif else if sz_A[0] eq 3 and sz_A[2] eq n_B then begin
	    A_dividedby_B = dblarr(sz_A[1:2])
	    A_dividedby_B[0,*] = A[0,*] / B
	    A_dividedby_B[1,*] = A[1,*] / B
	    A_dividedby_B[2,*] = A[2,*] / B
		
	;4. if A is 3xM and B is 3xM
	endif else if sz_A[0] eq 3 and sz_B[3] eq 3 and n_A eq n_B then begin
	    A_dividedby_B = dblarr(sz_A[1:2])
	    A_dividedby_B[0,*] = A[0,*] / B[0,*]
	    A_dividedby_B[1,*] = A[1,*] / B[1,*]
	    A_dividedby_B[2,*] = A[2,*] / B[2,*]
		
	;5. if A is NxM and B has M elements, then divide A by B
	endif else if sz_A[2] eq n_B then begin
		A_dividedby_B = dblarr(sz_A[1:2])
		for i = 0, sz_A[1] - 1 do A_dividedby_B[i,*] = A[i,*] / B
		
	;6. if A is NxM and B is NxM then divide A by B
	endif else if array_equal(sz_A[0:sz_A[0]], sz_B[0:sz_B[0]]) then begin
		A_dividedby_B = dblarr(sz_A[1:2])
		for i = 0, sz_A[1] - 1 do A_dividedby_B[i,*] = A[i,*] / B[i,*]
		
	endif else message, 'A and B are not compatible for dividing'
	;there could be a problem with sz_A[2] = n_B if A is NxM and B is IxJxKx... but n_B = M

	return, A_dividedby_B

end

;---------------------------------------------------
; Main Level Example Program (.r divide_vec) ///////
;---------------------------------------------------
print, '--------------- Scheme 1: ---------------'
A = findgen(5)
B = findgen(5) + 1
help, A, B
A_dividedby_B = divide_vec(A, B)
help, A_dividedby_B
print, ''


print, '--------------- Scheme 2: ---------------'
A = findgen(3, 4)
B = findgen(4) + 1
help, A, B
A_dividedby_B = divide_vec(A, B)
help, A_dividedby_B
print, ''


print, '--------------- Scheme 3: ---------------'
A = findgen(3, 5)
B = findgen(5) + 1
help, A, B
A_dividedby_B = divide_vec(A, B)
help, A_dividedby_B
print, ''


print, '--------------- Scheme 4: ---------------'
A = findgen(3,2)
B = findgen(3,2) + 1
help, A, B
A_dividedby_B = divide_vec(A, B)
help, A_dividedby_B
print, ''


print, '--------------- Scheme 5: ---------------'
A = findgen(7,4)
B = findgen(4) + 1
help, A, B
A_dividedby_B = divide_vec(A, B)
help, A_dividedby_B
print, ''


print, '--------------- Scheme 6: ---------------'
A = findgen(6,5)
B = findgen(6,5) + 1
help, A, B
A_dividedby_B = divide_vec(A, B)
help, A_dividedby_B

end