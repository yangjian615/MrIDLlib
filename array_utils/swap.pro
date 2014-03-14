; docformat = 'rst'
;
; NAME:
;
;       SWAP
;
; PURPOSE:
;+
;       The purpose of this program is to swap the contents of two variables.
;
; :Categories:
;
;       Array Utility
;
; :Params:
;
;       A:                  in, required, type=any
;                           Its value will be transferred to `B`.
;       B:                  in, required, type=any
;                           Its value will be transferred to `A`.
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
;       Written by:     Matthew Argall 08 September 2011
;-
pro swap, a, b
	
	;make a=b and b=a
	temp_a = a
	a = b
	b = temporary(temp_a)
end