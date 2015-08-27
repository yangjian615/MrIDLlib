;+
; NAME:
;       ht_velocity
;
; PURPOSE:
;
;       Calculate the velocity of the deHoffmann-Teller frame
;       using the convective electric field as a proxy for the measured
;       electric field.
;
; INPUT POSITIONAL PARAMETERS
;
;   v           -   units: meters, type: fltarr(3,N)
;                       3 components of the plasma velocity
;   B           -   units: Tesla. type: fltarr(3,N)
;                       3 components of the magnetic field
;
; RETURN VALUE
;
;   v_ht        -   units: m/s the three components of the deHoffmann-Teller velocity
;
; EXAMPLE
;
;   An example is included at the end and is a main level program
;   IDL> .r ht_velocity
;
; MODIFICATION HISTORY:
;
;       Written by:  Matthew Argall 04 April 2012
;       02/16/2012 - changed all operations do double precision
;
;SOURCE
;   Paschmann, G., Daly, P.W., Analysis Methods for Mulit-Spacecraft
;       Data, ISSI Scientific Report, chp 9, pg. 226
;-
;******************************************************************************************;
function ht_velocity, v, B
;---------------------------------------------------------------------
;Check Parameters ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	v_sz = size(v)
	if v_sz[0] ne 2 || v_sz[1] ne 3 then  message, 'v must be a 3xN array'
	
	B_sz = size(B)
	if B_sz[0] ne 2 || B_sz[1] ne 3 then message, 'B must be a 3xN array'
	
	if v_sz[v_sz[0]+2] ne B_sz[B_sz[0]+2] then message, 'v and B must have the same number of elements'
	
;---------------------------------------------------------------------
;deHoffman Teller Velocity ///////////////////////////////////////////
;---------------------------------------------------------------------
	npts = n_elements(v[0,*])
	
	;Here, the deHoffmann-Teller frame is determined as outlined in
	;section 9.3. 
	Pij = dblarr(3, 3, npts)
	Kij = dblarr(3, 3, npts)
	Kv = dblarr(3, npts)
	
	;B^2
;	BB = dot_product(B,B)
	BB = B[0,*]*B[0,*] + B[1,*]*B[1,*] + B[2,*]*B[2,*]
	
	;Evaluate equations 9.9 - 9.11. Pij is the projection matrix of B
	;onto a plane perpendicular to B. Kij and Kv are a result of
	;minimizing the square of the residual electric field.
	for i = 0, npts - 1 do begin
		BiBj = rebin(transpose(B[*, i]), 3, 3) * rebin(B[*, i], 3, 3)
		Pij[*,*,i] = identity(3, /double) - BiBj/BB[i]
		Kij[*,*,i] = BB[i] * Pij[*,*,i]
		
		Kv[*, i] = rotate_vector(Kij[*,*,i], v[*,i])
	endfor

	;average value of Kv
	Kv_avg = transpose([mean(Kv[0,*], /double), $
                        mean(Kv[1,*], /double), $
                        mean(Kv[2,*], /double)])
	
	;average value of Kij
	K0 = [[mean(Kij[0,0,*], /double), mean(Kij[1,0,*], /double), mean(Kij[2,0,*], /double)], $
		  [mean(Kij[0,1,*], /double), mean(Kij[1,1,*], /double), mean(Kij[2,1,*], /double)], $
		  [mean(Kij[0,2,*], /double), mean(Kij[1,2,*], /double), mean(Kij[2,2,*], /double)]]

	;deHoffmann-Teller velocity
;   v_ht = rotate_vector(Kv_avg, invert(K0, /double))
	v_ht = invert(K0, /double) ## Kv_avg
	
	return, v_ht
end

;
;EXAMPLE PROGRAM
;
;   This example program uses data from the Appendix at the end of Chapter 8 in ISSI 
;   "Analysis Methods for Multi-Spacecraft Data". Data points are from AMPTE/IRM, 
;   from 05:18:20–05:19:26 UT on October 19, 1984 and for use in benchmark tests
;   of MVA and other programs
;
;   The procedure that follows will duplicates the results from the text.
;

;---------------------------------------------------------------------
;Copy the Sample Data ////////////////////////////////////////////////
;---------------------------------------------------------------------
;time (s) seconds from 15:18:20.49
t = [0.00,4.38,8.75,13.12,17.49,21.87,26.24,30.61,34.98,39.36,43.73,48.10,52.47,56.85,61.22,65.59]

;density (#/cm^3)
N = [12.18,9.22,9.92,18.08,20.39,15.00,20.19,23.53,24.31,25.91,26.17,24.49,22.20,22.86,17.56,17.86]

;B(x,y,z) (nT)
Bx = transpose([-13.6,-14.8,-13.4,-14,-7.1,-0.9,-10,-6.1,1.2,-3.4,-0.9,-1,11,19.1,24.9,29.2])
By = transpose([-24.7,-24.9,-17.2,-25,-4.5,-5,-0.4,-4.8,1.6,-3.9,1.2,-1.5,13.2,34.4,50.1,47.1])
Bz = transpose([54.6,58.7,62.4,43.8,33.5,44.4,44.6,21.1,21,4.1,5,12.3,29.7,20.1,1.9,-10.6])

;Velocity (km/s)
Vx = transpose([-111,-102,-111,-135,-128,-82,-139,-143,-132,-117,-112,-98,-90,-83,-82,-93])
Vy = transpose([-211,-213,-196,-229,-252,-237,-228,-241,-226,-217,-210,-212,-186,-168,-129,-123])
Vz = transpose([57, 41, 34, 54, 54, 51, 77, 57, 80, 79, 93, 92, 104, 121, 88, 53])

;combine the components of the magnetic field and of the velocity
B = double([Bx, By, Bz])
v = double([Vx, Vy, Vz])

;---------------------------------------------------------------------
;Compute the Convective Electric Field ///////////////////////////////
;---------------------------------------------------------------------
;Compute the convective electric field: E = -v x B
E = -[v[1,*]*B[2,*] - v[2,*]*B[1,*], $      ;Ex
      v[2,*]*B[0,*] - v[0,*]*B[2,*], $      ;Ey
      v[0,*]*B[1,*] - v[1,*]*B[0,*]]        ;Ez
E = E * 1.0e-3                              ;convert to mV/m

;---------------------------------------------------------------------
;Compute the deHoffmann-Teller Velocity //////////////////////////////
;---------------------------------------------------------------------
v_ht = ht_velocity(v, B)

;---------------------------------------------------------------------
;Print the Results ///////////////////////////////////////////////////
;---------------------------------------------------------------------
;print the deHoffmann-Teller Velocity
print, '-----------------------------------------------------'
print, 'deHoffmann-Teller Velocity'
print, 'Compare with the result on page 227, section 9.3.1'
print, 'x', 'y', 'z', format='(13x, 2(a1, 10x), a1)'
print, 'v_ht =', transpose(v_ht), format='(a6, 3(3x, f8.3))'

end