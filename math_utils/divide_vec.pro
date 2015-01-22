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
;       QUIET:          in, optional, type=boolean, default=0
;                       If set, math error reporting will be suppressed (e.g. divide by
;                           zero, over/underflow errors, etc.).
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
;       2015/01/20  -   Improved logic. Added more cases. - MRA
;       2015/01/21  -   Added the QUIET keyword. Capture errors if !Except = 1 - MRA
;-
function divide_vec, A, B, $
QUIET=quiet
    compile_opt idl2
    on_error, 2

    ;Suppress divide by zeros?
    ;   - !Except = 2 will report error immediately then clears the math exceptions list.
    ;   - !Except = 1 will hold errors until the end of the program, so we can capture them.
    quiet    = keyword_set(quiet)
    inExcept = !except
    if quiet $
        then !except = 0 $
        else !except = !except > 1

    ;Sizes of inputs
    Andims = size(A, /N_DIMENSIONS)
    Bndims = size(B, /N_DIMENSIONS)
    Adims  = size(A, /DIMENSIONS)
    Bdims  = size(B, /DIMENSIONS)
	nA     = n_elements(A)
	nB     = n_elements(B)
		
	;1. if A and B have the same dimensions (including [3xN / 3xN] and [Nx3 / Nx3])
	if array_equal(Adims, Bdims) then begin
	    A_over_B = A / B
		
	;2. if A is 3xM and B has 3 elements
	endif else if (Andims eq 2 and Adims[0] eq 3) and (nB eq 3) then begin
	    A_over_B = dblarr(sz_A[1:2])
	    A_over_B[0,*] = A[0,*] / B[0]
	    A_over_B[1,*] = A[1,*] / B[1]
	    A_over_B[2,*] = A[2,*] / B[2]
		
	;3. if A is Mx3 and B has 3 elements
	endif else if (Andims eq 2 && Adims[1] eq 3) and (nB eq 3) then begin
	    A_over_B = dblarr(sz_A[1:2])
	    A_over_B[0,*] = A[*,0] / B[0]
	    A_over_B[1,*] = A[*,1] / B[1]
	    A_over_B[2,*] = A[*,2] / B[2]
		
	;4. if A is 3xM and B has M elements
	endif else if (Andims eq 2 and Adims[0] eq 3) and (Adims[1] eq nB) then begin
	    A_over_B = dblarr(Adims)
	    A_over_B[0,*] = A[0,*] / B
	    A_over_B[1,*] = A[1,*] / B
	    A_over_B[2,*] = A[2,*] / B

	;5. if A is Mx3 and B has M elements
	endif else if (Andims eq 2 and Adims[0] eq 3) and (Adims[1] eq nB) then begin
	    A_over_B = dblarr(Adims)
	    A_over_B[0,*] = A[*,0] / B
	    A_over_B[1,*] = A[*,1] / B
	    A_over_B[2,*] = A[*,2] / B
		
	;6. if A is NxM and B has M elements, then divide A by B
	endif else if (Andims eq 2 and Bndims eq 1) && (Adims[1] eq nB) then begin
		A_over_B = dblarr(Adims)
		for i = 0, Adims[0] - 1 do A_over_B[i,*] = A[i,*] / B
		
	;7. if A is NxM and B has N elements, then divide A by B
	endif else if (Andims eq 2) and (Bndims eq 1) and (Adims[0] eq nB) then begin
		A_over_B = dblarr(Adims)
		for i = 0, Adims[1] - 1 do A_over_B[*,i] = A[*,i] / B
				
	endif else message, 'A and B are not compatible for dividing'

    ;Report errors?
    if ~quiet && !except eq 1 then begin
        mathErr = reverse(cgBitGet(check_math()))
        if mathErr[0] then message, 'Divide by zero: Integer', /INFORMATIONAL
        if mathErr[1] then message, 'Overflow: Integer',       /INFORMATIONAL
        if mathErr[4] then message, 'Divide by zero: Float',   /INFORMATIONAL
        if mathErr[5] then message, 'Underflow: Float',        /INFORMATIONAL
        if mathErr[6] then message, 'Overflow: Float',         /INFORMATIONAL
        if mathErr[7] then message, 'Math error: Float',       /INFORMATIONAL
    endif

    ;Clear the error list and turn reporting back on.
    if quiet && inExcept ne 0 && !error_state.name eq 'IDL_M_MATHERROR_DETECTED' then void = check_math()
    !except = inExcept

	return, A_over_B
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