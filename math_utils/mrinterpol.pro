; docformat = 'rst'
;
; NAME:
;       MrInterpol
;
; PURPOSE:
;+
;       The purpose of this program is to generalize the INTERPOL procedure to arrays.
;       Each column of the given array, `A`, will be interpolated indepentently at points
;       `Xout`::
;           A[1xM] and X[M] then use `interpol`
;           A[3xM] and X[M] then use `interpol` 3 times
;           A[Mx3] and X[M] then use `interpol` 3 times
;           A[NxM] and X[M] then loop over each A[i,*], interpolating each column
;           A[MxN] and X[M] then loop over each A[*,i], interpolating each row
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       A:              in, required, type=Numeric array [NxM]
;                       The array to be interpolated. Can be a 1-D vector.
;       X:              in, required, type=Numeric array [Mx1]
;                       The abcissa values at which A are found; must have M elements
;       Xout:           in, required, type=Numeric array
;                       The new abcissa values.
;
; :Keywords:
;
; :Returns:
;       RESULT:         The values of `A` interpolated to the locations of `Xout`.
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
;       04/12/2013  -   Added cases for 3xN, Nx3, and MxN, where M is the number of
;                           elements in `X`. - MRA
;-
function MrInterpol, A, X, Xout
    compile_opt idl2
    on_error, 2

	sz_A = size(A)
	sz_X = size(X)
	
	;Number of elements
	n_A = sz_A[sz_A[0] + 2]
	n_X = sz_X[sz_X[0] + 2]
	n_Xout = n_elements(Xout)
	
	;Determine output type. Maintain Double and Complex types, make everything else a float
	type_A = sz_A[sz_A[0] + 1]
	case type_A of
	    5: out_type = 5
	    6: out_type = 6
	    9: out_type = 9
	    else: out_type = 4
	endcase
	
        
;---------------------------------------------------------------------
;A and X are 1xN or Nx1 //////////////////////////////////////////////
;---------------------------------------------------------------------
	if n_A eq n_X and sz_A[0] eq 1 then begin
		result = interpol(A, X, Xout)
        
;---------------------------------------------------------------------
;A is 3xM, X is an M element vector //////////////////////////////////
;---------------------------------------------------------------------
	endif else if sz_A[0] eq 2 && sz_A[1] eq 3 && sz_A[2] eq n_X then begin
	    result = make_array(3, n_Xout, TYPE=out_type)
	    result[0,*] = interpol(A[0,*], X, Xout)
	    result[1,*] = interpol(A[1,*], X, Xout)
	    result[2,*] = interpol(A[2,*], X, Xout)
        
;---------------------------------------------------------------------
;A is Mx3, X is an M element vector //////////////////////////////////
;---------------------------------------------------------------------
	endif else if sz_A[0] eq 2 && sz_A[1] eq 3 && sz_A[2] eq n_X then begin
	    result = make_array(n_Xout, 3, TYPE=out_type)
	    result[*,0] = interpol(A[*,0], X, Xout)
	    result[*,1] = interpol(A[*,1], X, Xout)
	    result[*,2] = interpol(A[*,2], X, Xout)
        
;---------------------------------------------------------------------
;A is NxM, X is an M element vector //////////////////////////////////
;---------------------------------------------------------------------
	endif else if sz_A[0] eq 2 && sz_A[2] eq n_X then begin
		result = make_array(sz_A[1], n_Xout, TYPE=out_type)
		for i = 0, sz_A[1] - 1 do result[i,*] = interpol(A[i,*], X, Xout)
        
;---------------------------------------------------------------------
;A is MxN, X is an M element vector //////////////////////////////////
;---------------------------------------------------------------------
	endif else if sz_A[0] eq 2 && sz_A[1] eq n_X then begin
		result = make_array(n_Xout, sz_A[2], TYPE=out_type)
		for i = 0, sz_A[2] - 1 do result[*,i] = interpol(A[*,i], X, Xout)
        
;---------------------------------------------------------------------
;Other Cases /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else message, '[A,X]: Incorrect dimension sizes.'

	return, result

end