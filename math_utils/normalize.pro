; docformat = 'rst'
;
; NAME:
;       NORMALIZE
;
; PURPOSE:
;+
;       The purpose of this program is to normalize an array of 3-dimensional vectors such
;       that::
;           A[1,*]^2 + A[2,*]^2 + A[3,*]^2 = 1
;
; :Examples:
;       See main level program at end of file::
;           IDL> .r normalize
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       A:              in, optional, type=Numeric array 3xN
;                       The array of 3-dimensional vectors to be normalized.
;
; :Returns:
;       N:              An array of unit vectors corresponding to each vector in `A`.
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
;       02/06/2013  -   Now uses `divide_vec` instead of a loop in the normalization
;                           process.
;-
function normalize, A
    compile_opt idl2
    on_error, 2

	dims  = size(A, /DIMENSIONS)
	ndims = size(A, /N_DIMENSIONS)
	nA    = n_elements(A)
	
	;normalize A
	if nA eq 3 then begin
		N = A / sqrt(dot_product(A, A))
		
	;normalize each vector in A
	endif else if (ndims eq 2) && (dims[1] eq 3 or dims[0] eq 3) then begin
		N     = dblarr(ndims)
		A_mag = sqrt(dot_product(A, A))
		N     = divide_vec(A, A_mag)
		
	;A must be a 3 element vector or a 3xN array
	endif else message, 'A must be a 3 element vector or [3,N] or [N,3] array'
	
	;return the normal of A
	return, N
end

;---------------------------------------------------
; Main Level Example Program (.r normalize) ////////
;---------------------------------------------------
;Create an array of vectors to normalize
vector_array = rebin([4, 6, 16], 3, 5)
normalized_array = normalize(vector_array)
print, 'Vector Array:'
print, vector_array
print, ''
print, 'Normalized Array:'
print, normalized_array
print, ''
print, 'Normalized Magnitude:'
print, transpose(magnitude(normalized_array))

end