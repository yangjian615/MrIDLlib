;+
; NAME:
;       variance_analysis
;
; PURPOSE:
;
;       Compute the covariant matrix of the three components
;       of the magnetic (minimum variance) or electric (maximum variance) field, then
;       calculates the eigenvalues and eigenvectors.
;
;       The eigenvalues, X, are determined in such a way as to find the day-side magnetopause
;       normal direction. For minimum variance (MVAB), the normal direction corresponds to the
;       least-varying (in time) component of the magnetic field. For maximum variance (MVAE),
;       this corresponds to the maximally varying (in time) component of the electric field.
;
;       Because the subsolar magnetopause normal generally points in the X-GSE direction,
;       while the maximally-varying magnetic field is the reconnecing component, Bz, the
;       eigenvectors are ordered in the following manner:
;           X[0,*] = N = Normal component (N)
;           X[1,*] = M = Intermediately varying component (M)
;           X[2,*] = L = Maximally (minimally) varying component for MVAB (MVAE) 
;
;       The magnitude of the eigenvalues, Y, indicate which eigenvector is which.
;           min(Y) -> Normal component
;           mid(Y) -> Intermediate component
;           max(Y) -> Maximum (minimum) component for MVAB (MVAE)
;
;
; INPUT POSITIONAL PARAMETERS
;
;   FIELD           -   3xN magnetic field matrix
;
; KEYWORD INPUTS
;
;  MAXIMUM          -   in. optional. type: boolean. default: 0
;                           order the eigenvalues from largest to smallest, orient
;                           the  maximum varying eigenvector along +GSEx, and the
;                           minimum  varying eigenvector along +GSEz. The default is
;                           to do a minimum variance analysis.
;
; KEYWORD OUTPUTS
;
;   COVARIANCE      -   out. optional. type:fltar(3,3)
;                           covariant matrix of M (unaltered from output, unlike
;                           lamda and the output)
;   EIGVALS         -   out. optional. type=fltarr(3)    
;                           solution to the eigenvalue-eigenvector equation Mx =
;                           lx. The eigenvalues are ordered from minimum to maximum.
;
; RETURN VALUE
;
;   EIGVECS         -   The eigenvectors of the covariant matrix, as described
;                       above.  Eigvec(*,0) corresponds to eigval[0], etc.
;
; REFERENCES
;	Bengt, Sonnerup, Scheible, chp. 8, Minimum and Maximum Variance Analysis from the
;       ISEE "Analysis Methods for Multi-Spacecraft Data" book, 2000
;	Sonnerup, "Magnetopause Properties from AMPTE/IRM Observations of the
;		Convection Electric Field: Method Development", JGR 1987
;
; USAGE
;   eigenvectors = oRef -> minimum_variance, field [, COVARIANCE=covariance] 
;                                            [, EIGVALS=eigvals] [, MAXIMUM=maximum]
;
; EXAMPLES
;
;   An example main level program is included at the end.
;   IDL> .r variance_analysis
;
; MODIFICATION HISTORY:
;
;   Written by:  Matthew Argall 08 Sept. 2011
;   10/19/2011 - Added MAXIMUM and MINIMUM keywords,
;                changed TSTART and TEND to keywords,
;                commented out the last sort. it
;                   happens automatically with ASCENDING keyword - MRA
;   11/03/2011 - Removed TSTART, TEND, and other useless parts -MRA
;-
;*****************************************************************************************
function variance_analysis, field, $
;KEYWORDS
COVARIANCE = M, $
EIGVALS = eigvals, $
MAXIMUM = maximum

	;check whether to perform minimum or maximum variance. default is minimum variance
	;Arrange the eigenvalues in ascending order for MVAB and descending order for MVAE
	if keyword_set(maximum) then ascending=0 else ascending=1
	
	;compute the covariant matrix for the magnetic field
	;then find the eigenvalues and eigenvectors of the covariance matrix
	M = correlate(field, /covariance, /double)
	eigvals = eigenql(M, eigenvectors=eigvecs, /double, ascending=ascending)

	;The eigenvectors are stored in rows. 
	;If the x component of N is negative, make it positive to correspond with +X-GSE.
	;If the z component of L is negative, make it positive to correspond with +Z-GSE.
	if eigvecs[0,0] lt 0 then eigvecs(*,0) = -eigvecs(*,0)
	if eigvecs[2,2] lt 0 then eigvecs(*,2) = -eigvecs(*,2)
	
	;If the determinant of EIGVECS is negative, this indicates a left-handed coordinate
	;system. To remedy this, take the negative of the M-component
	if determ(eigvecs) lt 0 then eigvecs(*,1) = -eigvecs(*,1)

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
E *= 1.0e-3                                 ;convert to mV/m

;---------------------------------------------------------------------
;Perform MVAB and MVAE ///////////////////////////////////////////////
;---------------------------------------------------------------------
eigvec_min = variance_analysis(B, EIGVALS=eigvals_min)
eigvec_max = variance_analysis(E, EIGVALS=eigvals_max, /MAXIMUM)

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
eigvec_max2 = variance_analysis(E_prime, EIGVALS=eigvals_max2, /MAXIMUM)

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