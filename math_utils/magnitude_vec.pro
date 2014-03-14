; docformat = 'rst'
;
; NAME:
;       MAGNITUDE_VEC
;
; PURPOSE:
;+
;       The purpose of this program is to calculate the magnitude of an array of M-D
;       vectors::
;           Magnitude = |A[i,*]|
;           Magnitude = sqrt( A[0,*]^2 + A[1,*]^2 + ... + A[N,*]^2 )
;           Magnitude = sqrt( total(A^2, 1) )
;           Magnitude = sqrt( dot_product(A) ) 
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       A:              in, optional, type=Numeric array NxM
;                       The array of M-dimensional vectors to be normalized.
;
; :Returns:
;       A_MAG:          The magnitude of each vector in `A`.
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
;       02/13/2013  -   Written by Matthew Argall
;-
function magnitude_vec, A
    compile_opt idl2
    on_error, 2
	
	;Dot A with itself and take the square root.
	A_mag = sqrt( dot_product(A, A) )
	
	;return the normal of A
	return, A_mag
end

;---------------------------------------------------
; Main Level Example Program (.r magnitude) ////////
;---------------------------------------------------
