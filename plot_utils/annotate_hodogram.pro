; docformat = 'rst'
;
; NAME:
;       ANNOTATE_HODOGRAM
;
; PURPOSE:
;+
;   The purpose of this program is to display the eigenvalues and eigenvectors returned by
;   a minimum variance analysis below a hodogram on the currently active window.
;
; :Categories:
;   Graphics Utility, Multi-Spacecraft Methods
;
; :Params:
;       EIGVALS:        in, required, type=fltarr(3)
;                       The eigenvalues of the MVA frame
;       EIGVECS:        in, required, type="fltarr(3,3)"
;                       The eigenvectors of the MVA frame. EIGVECS(*,0) represent the
;                           eigenvector for EIGVAL(0).
;       B_AVG:          in, required, type=fltarr(3)
;                       The x-, y-, and z- components of the average field during the MVA
;                           interval
;
; :Keywords:
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
;       02/25/2012  -   Created by Matthew Argall
;-
pro annotate_hodogram, eigvals, eigvecs, B_avg

;---------------------------------------------------------------------
;Check Keywords \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
	
	;check that all 3 eigenvalues are present
	if n_elements(eigvals) ne 3 then message, 'eigvals must contain 3 eigenvalues'
	
	;check that all 3 eigenvectors are present
	sz = size(eigvecs)
	if sz[0] ne 2 || sz[1] ne 3 || sz[2] ne 3 then message, 'eigvecs must be a 3x3 matrix of eigenvectors'
	
;---------------------------------------------------------------------
;Annotate the Hodogram \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------	

	offset = float(!d.y_ch_size * 1.5)/float(!d.y_size)
	bottom = 5 * offset

	lambda = greek('lambda')
	line1 = string(' ', format='(8x, a1)') + $
			string('x', 'y', 'z', 'B', format='(4(10x, a1))')
	line2 = string('i', eigvals[0], eigvecs(*,0), B_avg[0], format='(a1, 3x, f8.4, 3x, 3(f7.4, 3x), f6.2)')
	line3 = string('j', eigvals[1], eigvecs(*,1), B_avg[1], format='(a1, 3x, f8.4, 3x, 3(f7.4, 3x), f6.2)')
	line4 = string('k', eigvals[2], eigvecs(*,2), B_avg[2], format='(a1, 3x, f8.4, 3x, 3(f7.4, 3x), f6.2)')
	
	xyouts, 0.25, bottom-0.5*offset, line1, /normal
	xyouts, 0.25, bottom-0.5*offset - offset, line2, /normal
	xyouts, 0.25, bottom-0.5*offset - offset*2.0, line3, /normal
	xyouts, 0.25, bottom-0.5*offset - offset*3.0, line4, /normal
	
end