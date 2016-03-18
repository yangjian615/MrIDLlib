; docformat = 'rst'
;
; NAME:
;       MrVector_Normalize
;
; PURPOSE:
;+
;       The purpose of this program is to normalize an array of 3-dimensional vectors such
;       that::
;           sqrt( A[1,*]^2 + A[2,*]^2 + A[3,*]^2 ) = 1
;
; :Examples:
;       See main level program at end of file::
;           IDL> .r MrVector_Normalize
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       A:              in, optional, type=Numeric array 3xN
;                       The array of 3-dimensional vectors to be normalized.
;
; :Returns:
;       B:              An array of unit vectors corresponding to each vector in `A`.
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
;       2015/05/03  -   Written by Matthew Argall
;       2016/03/12  -   Copy paste problem on the Nx3 case.
;-
function MrVector_Normalize, A, $
DOUBLE=double
	compile_opt idl2
	on_error, 2

	;Describe input
	sz    = size(A)
	ndims = sz[0]
	dims  = ndims eq 0 ? 0 : sz[1:ndims]
	n     = sz[sz[0]+2]
	type  = sz[sz[0]+1]

	;Describe output
	if type eq 5 || keyword_set(double) $
		then B = double(A) $
		else B = float(A)
	
	;3x1 or 1x3
	if n eq 3 then begin
		B = B / sqrt(total(B^2))
		
	;3xN
	endif else if ndims eq 2 && dims[0] eq 3 then begin
		mag    = sqrt(total(B^2, 1))
		B[0,*] = B[0,*] / mag
		B[1,*] = B[1,*] / mag
		B[2,*] = B[2,*] / mag
		
	;Nx3
	endif else if ndims eq 2 && dims[1] eq 3 then begin
		mag    = sqrt(total(B^2, 2))
		B[*,0] = B[*,0] / mag
		B[*,1] = B[*,1] / mag
		B[*,2] = B[*,2] / mag
		
	;A must be a 3 element vector or a 3xN array
	endif else message, 'Input must be 3xN or Nx3.'
	
	;return the norm of A
	return, B
end

;---------------------------------------------------
; Main Level Example Program (.r normalize) ////////
;---------------------------------------------------
;Create an array of vectors to normalize
vector_array = rebin([4, 6, 16], 3, 5)
normalized_array = mrvector_normalize(vector_array)
print, 'Vector Array:'
print, vector_array
print, ''
print, 'Normalized Array:'
print, normalized_array
print, ''
print, 'Normalized Magnitude:'
print, transpose(sqrt(total(normalized_array^2, 1)))

end