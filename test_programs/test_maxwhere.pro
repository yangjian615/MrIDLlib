; docformat = 'rst'
;
;+
;   Test to see if it is faster to find a single value within an array using
;   the where or max function.
;
;   Solution::
;       Max is a factor of 2 faster when N is small (<= 10000). However, Where
;       is much faster for N >> 10000.
;      
;       Test 1:
;          10000 iterations of where on a 26-element STRING array: 0.01271605 seconds.
;          10000 iterations of max:  on a 26-element STRING array: 0.00642705 seconds.
;
;       Test 2:
;          10000 iterations of where on a 10,000-element LONG array: 0.24382591 seconds.
;          10000 iterations of max:  on a 10,000-element LONG array: 0.20062113 seconds.
;      
;       Test 3:
;          10000 iterations of where on a 100,000-element LONG array: 1.30535007 seconds.
;          10000 iterations of max:  on a 100,000-element LONG array: 1.41289210 seconds.
;      
;       Test 3:
;          10000 iterations of where on a 10,000,000-element LONG array: 95.49853706 seconds.
;          10000 iterations of max:  on a 10,000,000-element LONG array: 129.24299908 seconds.
;
; :Categories:
;   Test Program
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;-
pro test_maxwhere
	compile_opt idl2

	;An array and a value to find within the array
	array  = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', $
	          'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
	value  = 't'
	maxitr = 10000
	
	tic
	for i = 0, maxitr-1 do iloc = where(array eq value, nloc)
	t_where = toc()

	tic
	for i = 0, maxitr-1 do void = max(array eq value, iloc)
	t_max = toc()
	
	
	;Print results
	npts = n_elements(array)
	type = size(array, /TNAME)
	print, maxitr, npts, type, t_where, FORMAT='(%"%i iterations of where on a %i-element %s array: %0.8f seconds.")'
	print, maxitr, npts, type, t_max,   FORMAT='(%"%i iterations of max   on a %i-element %s array: %0.8f seconds.")'
end