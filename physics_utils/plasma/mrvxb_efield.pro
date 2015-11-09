; docformat = 'rst'
;
; NAME:
;   MrVxB_EField
;
; PURPOSE:
;+
;   Calculate the convective electric field.
;
;       E_{C} = - \vec{V} \times \vec{B}
;
; :Categories:
;   Plasma Physics Utilties
;
; :Params:
;       V:              in, required, type=3xN real
;                       Three-component velocity data in units of kilometers per
;                           second (km/s).
;       B:              in, required, type=3xN real
;                       Three-component magnetic field data in units of nanoTesla (nT).
;                           B must be the same size as `V`.
;
; :Returns:
;       E_VxB:          The VxB Convective electricfield in units of milli-Volts
;                           per meter (mV/m)
;
; :Uses:
;   Uses the following external programs::
;       cross_product.pro
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
;       2015/09/13  -   Written by Matthew Argall
;-
function MrVxB_EField, V, B, $
MAGNITUDE=magnitude
	compile_opt strictarr
	on_error, 2

;---------------------------------------------------------------------
;Check inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	dimsV = size(V, /DIMENSIONS)
	dimsB = size(B, /DIMENSIONS)

	;Make sure they are the same size.
	if array_equal(dimsV, dimsB) eq 0 $
		then message, 'V and B must be the same size.'

	;Make sure they are 3xN
	if dimsV[0] ne 3 $
		then message, 'V and B must be 3xN arrays.'
;---------------------------------------------------------------------
;Calculate the ExB drift velocity ////////////////////////////////////
;---------------------------------------------------------------------

	;Units
	;   km/s * nT = m/s * T * 1e-6 = m/s * (Vs/m^2) * 1e-6 = V/m * 1e-6 = mV/m * 1e-3
	;   => Must multiply the result by 1e-3 in order to have units of mV/m
	E_VxB = -1e-3 * cross_product(V, B) 

	;Return the magnitude?
	if arg_present(magnitude) then magnitude = sqrt(total(E_VxB^2, 1))

	return, E_VxB
end
