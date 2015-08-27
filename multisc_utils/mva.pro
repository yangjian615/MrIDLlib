function mva, t, field, $
;KEYWORDS
  COVARIANCE = M, $
  EIGVALS = eigvals, $
  MAXIMUM = maximum, $
  TSTART = tstart, $
  TEND = tend


;---------------------------------------------------------------------
;Created by Matthew Argall 9/8/2011
;
;MIN_VARIANCE computes the covariant matrix of the three components
;of the magnetic field within the time range [tstart, tend], then
;calculates the eigenvalues and eigenvectors, organizing them in such a
;way that Ax = lx, x being a column vector, the minimum variance direction
;corresponds to +x in GSM coordinates, maximum variance with +z GSM, and
;mid-variance completes the right-handed coordinate system.
;
;
;INPUTS
;   t - time array (seconds since midnight) corresponding to the data points in B
;   field - 3xN magnetic field matrix
;
; KEYWORDS
;   COVARIANCE - covariant matrix of M (unaltered from output, unlike lamda and the output)
;   EIGVALS - solution to the eigenvalue-eigenvector equation Mx = lx. The eigenvalues are
;              ordered from minimum to maximum.
;	MAXIMUM - order the eigenvalues from largest to smallest, orient the maximum varying
;			  eigenvector along +GSEx, and the minimum varying eigenvector along +GSEz.
;			  The default is to do a minimum variance analysis.
;   TSTART  - starting time of the minimum variance algorithm. Units: seconds since midnight
;   TEND	- end time of the minimum variance algorithm. Units: seconds since midnight
;
;
;OUTPUT
;   EIGVECS - The eigenvectors of the covariant matrix, as described above. Eigvec(*,0) corresponds
;              to eigval[0], etc.
;
;REFERENCES
;	Bengt, Sonnerup, Scheible, chp. 8, Minimum and Maximum Variance Analysis
;	Sonnerup, "Magnetopause Properties from AMPTE/IRM Observations of the
;		Convection Electric Field: Method Development", JGR 1987
;
;USAGE
;   eigvecs = min_variance(time, field)
;	eigvecs = min_variance(time, field, eigvals=eigvals, /maximum)
;
;REFERENCES
;	Bengt, Sonnerup, Scheible. Chp.8 Minimum and Maximum Variance Analysis
;	Fetter & Walecka "Theoretical Mechanics of Particles and Coninua"
;
;MODIFICATIONS
;	10/19/2011 - Added MAXIMUM and MINIMUM keywords,
;				 changed TSTART and TEND to keywords,
;				 commented out the last sort. it happens automatically with ASCENDING keyword - MRA
;   07/08/2013 - Changing the signs of the N, M, and L components to be "GSE-like" ended
;                up producing a magnetospheric field that points southward. Commenting
;                out those lines produced the proper field orientation. - MRA
;---------------------------------------------------------------------
	;check whether to perform minimum or maximum variance. default is minimum variance
	if keyword_set(maximum) then ascending=0 else ascending=1

	if n_elements(tstart) ne 0 and n_elements(tend) ne 0 then begin
	    indx = getIndexRange(t, [tstart, tend])
		field = field[*,indx[0]:indx[1]]
	endif

	;compute the covariant matrix for the magnetic field
	;then find the eigenvalues and eigenvectors of the covariance matrix
	M = correlate(field, /covariance, /double)
	eigvals = eigenql(M, eigenvectors=eigvecs, /double, ascending=ascending)

	;Check that the modal matrix, A, returns the eigenvalues and that the
	;eigenvectors are orthoganal:
;	A = transpose(eigvecs)
;	print, diag_matrix(transpose(A)##M##A)     ;should return the eigenvalues
;	print, transpose(A)##A                     ;should be the identity matrix

	;Arrange the eigenvalues in ascending order to correspond to the day-side
	;magnetosphere, where the minimum variance direction is along x and the
	;maximum is along z (i.e. eigvecs[2] -> +x1, eigvecs[1] -> x2, eigvecs[0] -> +x3)
	;
	;Note that there are only 3 eigenvalues and their inices sum to 3 (0+1+2).
	;To find the median subract the max and min indices from 3.

;	lamda_max = max(eigvals, imax, min=lamda_min, subscript_min=imin)
;	imid = 3 - imax - imin

	;The eigenvectors are stored in rows. If the x component of the minimum
	;variance direction is negative, make it positive to correspond with GSE x.
	;Likewise, if the z component of the maximum variance direction is negative,
	;make it positive to correspond with GSE z.
	;
	;maximum <-> minimum in previous comment if MAXIMUM is set
	;
	;Note that because the eigenvalues and eigenvectors are sorted according
	;properly with the /ascending keyword in EIGENQL, the process is the same
	;for maximum and minimum variance

 	if eigvecs[0,0] lt 0 then eigvecs[*,0] = -eigvecs[*,0]
 	if eigvecs[2,2] lt 0 then eigvecs[*,2] = -eigvecs[*,2]
;	if keyword_set(maximum) then begin
;		if eigvecs(imax,0) lt 0 then eigvecs(*,imax) = -eigvecs(imax,*)
;		if eigvecs(imin,2) lt 0 then eigvecs(imin,*) = -eigvecs(imin,*)
;	endif else begin
;		if eigvecs(imin,0) lt 0 then eigvecs(imin,*) = -eigvecs(imin,*)
;		if eigvecs(imax,2) lt 0 then eigvecs(imax,*) = -eigvecs(imax,*)
;	endelse

	;if the determinant of eigvecs is negative, this indicates a left-handed coordinate
	;system. To remedy this, take the negative of the eigenvector corresponding to
	;lamda_mid.
 	if determ(eigvecs) lt 0 then eigvecs[*,1] = -eigvecs[*,1]

	;If maximum variance, rearrange the eigenvalues so that they are in descending order.
	;Their eigenvectors change with them, as well as the covariant matrix
;	eigvals = [eigvals[imin], eigvals[imid], eigvals[imax]]
;	eigvecs = [[eigvecs(*,imin)], [eigvecs(*,imid)], [eigvecs(*,imax)]]
;	M = [[M(*,imin)], [M(*,imid)], [M(*,imax)]]


return, eigvecs

end








;
;EXAMPLE PROGRAM
;
;   This example program uses data from the Appendix at the end of Chapter 8 in ISSI 
;   Analysis Methods for Multi-Spacecraft Data. Data points are from AMPTE/IRM, 
;   from 05:18:20–05:19:26 UT on October 19, 1984 for use in benchmark tests
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
;Perform MVAB and MVAE ///////////////////////////////////////////////
;---------------------------------------------------------------------
hel        = mva(B, EIGVALS=eigvals_min)
eigvec_max = mva(E, EIGVALS=eigvals_max, /MAXIMUM)

;---------------------------------------------------------------------
;Perform Another Iteration of MVAE ///////////////////////////////////
;---------------------------------------------------------------------
;use the normal component of MVAE to adjust the particle velocity into 
;a frame moving with the discontinuity and do another iteration of MVAE (section 8.4.1-2)
;E' = E + (un x B) where un is the velocity of the discontinuity in the normal direction
;and E is the electric field in the spacecraft frame.
;In section 8.4.2, a value of (u dot n) = -5 is chosen
un = -5 * eigvec_max[*,0]
unxB = [un[1]*B[2,*] - un[2]*B[1,*], $      ;unxB_x
        un[2]*B[0,*] - un[0]*B[2,*], $      ;unxB_y
        un[0]*B[1,*] - un[1]*B[0,*]]        ;unxB_z

;calculate the electric field in the frame of the discontinuity. Convert to mV/m
E_prime = E + unxB * 1.0e-3

;perform the next iteration of MVAE
eigvec_max2 = mva(E_prime, EIGVALS=eigvals_max2, /MAXIMUM)

;---------------------------------------------------------------------
;Compute the Average Fields //////////////////////////////////////////
;---------------------------------------------------------------------
;calculate the average field
Bavg = mean(B, DIMENSION=2)
Eavg = mean(E, DIMENSION=2)
Ep_avg = mean(E_prime, DIMENSION=2)

;and rotate the average values into the new MVA frame
;Note that the results published in Multi-Spacecraft methods are actually a left-handed
;coordinate system. To get the same signs, we would have to negate the M component
Bavg_nlm = [Bavg[0]*eigvec_min[0,0] + Bavg[1]*eigvec_min[1,0] + Bavg[2]*eigvec_min[2,0], $
            Bavg[0]*eigvec_min[0,1] + Bavg[1]*eigvec_min[1,1] + Bavg[2]*eigvec_min[2,1], $
            Bavg[0]*eigvec_min[0,2] + Bavg[1]*eigvec_min[1,2] + Bavg[2]*eigvec_min[2,2]]

Eavg_nlm = [Eavg[0]*eigvec_max[0,0] + Eavg[1]*eigvec_max[1,0] + Eavg[2]*eigvec_max[2,0], $
            Eavg[0]*eigvec_max[0,1] + Eavg[1]*eigvec_max[1,1] + Eavg[2]*eigvec_max[2,1], $
            Eavg[0]*eigvec_max[0,2] + Eavg[1]*eigvec_max[1,2] + Eavg[2]*eigvec_max[2,2]]

Ep_avg_nlm = [Ep_avg[0]*eigvec_max2[0,0] + Ep_avg[1]*eigvec_max2[1,0] + Ep_avg[2]*eigvec_max2[2,0], $
              Ep_avg[0]*eigvec_max2[0,1] + Ep_avg[1]*eigvec_max2[1,1] + Ep_avg[2]*eigvec_max2[2,1], $
              Ep_avg[0]*eigvec_max2[0,2] + Ep_avg[1]*eigvec_max2[1,2] + Ep_avg[2]*eigvec_max2[2,2]]

;---------------------------------------------------------------------
;Print the Results ///////////////////////////////////////////////////
;---------------------------------------------------------------------
;create labels for the eigenvectors
lambda = '!4k!X'
nlm = ['N', 'M', 'L']

;print the results of MVAB.
print, '-----------------------------------------------------'
print, 'MVAB eigenvalues and eigenvectors'
print, 'Compare with the table in Figure 8.2'
print, lambda, 'x', 'y', 'z', 'Bavg', format='(4(9x, a1), 8x, a4)'
for i=0, 2 do print, strtrim(string(nlm[i], eigvals_min[i], eigvec_min[*,i], Bavg_nlm[i], $
                                    format='(a1, 2x, f10.4, 3(3x, f7.4), 2x, f7.2)'), 1)

;print the results of MVAE.
print, '-----------------------------------------------------'
print, 'MVAE: Iteration I; eigenvalues and eigenvectors'
print, 'Compare with Table 8.3'
print, lambda, 'x', 'y', 'z', 'Eavg', format='(4(9x, a1), 8x, a4)'
for i=0, 2 do print, strtrim(string(nlm[i], eigvals_max[i], eigvec_max[*,i], Eavg_nlm[i], $
                                    format='(a1, 2x, f10.4, 3(3x, f7.4), 2x, f7.2)'), 1)

;print the results of the second iteration of MVAE.
print, '-----------------------------------------------------'
print, 'MVAE: Iteration II, eigenvalues and eigenvectors'
print, 'Compare with the table in Figure 8.10'
print, lambda, 'x', 'y', 'z', 'Eavg', format='(4(9x, a1), 8x, a4)'
for i=0, 2 do print, strtrim(string(nlm[i], eigvals_max2[i], eigvec_max2[*,i], Ep_avg_nlm[i], $
                                    format='(a1, 2x, f10.4, 3(3x, f7.4), 2x, f7.2)'), 1)

end
