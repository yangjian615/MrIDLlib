; docformat = 'rst'
;
;+
;   Compute the velocity of a 1D boundary via the Constant Velocity Approach (CVA)
;
; METHOD:
;    The velocity of the boundary can be expressed as a polynomial in time
;
;       V = A0 + A1*t + A2*t^2 + A3*t^3
;
;    If the velocity is constant, then A1 = A2 = A3 = 0. The distance traveled
;    along the normal from spacecraft 0 to spacecraft i can then be expressed
;    as the velocity multiplied by the delay times:
;
;       dR.n = A0 * dt
;
;    To solve, let m = n/A0 and invert the dR matrix. Noting that n is a unit
;    vector, |n| = 1, we find |m| = 1/A0 and
;
;      V = A0 = 1/|m|
;
;
; :Categories:
;   Physics Utilties
;
; :Params:
;       R:          in, required, type=3x4 float
;                   Position vectors of four spacecraft.
;       T:          in, required, type=fltarr(4)
;                   Times at which each spacecraft crossed the boundary.
;
; :Keywords:
;       N:          out, optional, type=fltarr(4)
;                   Components of the unit vector normal to the boundary.
;
; :Returns:
;       V:          Velocity of the boundary.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       01/29/2012  -   Written by Matthew Argall
;-
function MrCVA, R, t, $
N=n
	compile_opt idl2
	on_error, 2
	
	;Check input sizes
	szR = size(R)
	szT = size(t)
	if szR[0] ne 2 || szR[1] ne 3 || szR[2] ne 4 then message, 'R must be 3x4.'
	if szt[0] ne 1 && szR[1] ne 4                then message, 'T must be a 4-element row vector.'

	;Positions and crossing times relative to the first
	;spacecraft to encounter the boundary.

	dR = R[*,1:*] - rebin(R[*,0], 3, 4)
	dt = t[1:*] - t[0]

	;Invert the matrix dR
	;   - LA_INVERT expects the separation vectors to be along the
	;     rows, like a normal math matrix.
;	dRinv = invert(dR)
	dRinv = la_invert(dR)

	;Compute the normal vector
	;   - Multiply the rows of DT by the columns of DRINV
	;   - dRinv ## transpose(dt)
	m = matrix_multiply(dt, dRinv, /ATRANSPOSE)
	A = 1.0 / sqrt(total(m^2))
	n = m * A
	
	;Constant velocity
	V = temporary(A)
	return, V
end