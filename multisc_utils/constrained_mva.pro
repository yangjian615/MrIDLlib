;+
; NAME:
;       oMultiSpacraftMethods::constrained_mva
;
; PURPOSE:
;
;       Performs a constrained Minimum/Maximum Variance Analysis (MVAB/MVAE)
;
;       Constrained MVA (CMVA) uses a vector, e, to constrain the determination of the
;       eigenvectors and eigenvalues of the MVA covariant matrix by finding a direction
;       that fits the MVA criteria AND forces (e dot n) = 0, where n is resulting normal
;       direction.
;       
;       The method followed here is that P*M*P*n = l*n, where M is the covariant matrix
;       of the field, n is the boundary normal, l are the eignevalues of
;       the constrained variance matrix (PMP), and P is the projection matrix
;       describing the projection of a vector onto the plane perpendicular to
;       the constraint, e. 
;       P = identity_matrix[i,j] - e[i]*e[j]
;
;***IMPORTANT NOTE****
;       The formulation of the algorithm below was undertaken assuming that
;       the constraint's components were ordered LMN. In all of the other programs
;       created by me (Matthew Argall), I have ordered the components NML to
;       correspond with XYZ GSE at the magnetopause. In order to use this code
;       with my other programs, you will have to build your constraint, then
;       reverse it... constraint = reverse(constraint)
;
; ARGUMENTS
;   M               -   the covariant matrix obtained from the unconstrained MVA process
;   e               -   the constraint dot_product(n, e) = 0 where n and e are
;                       described above
;
; KEYWORD INPUTS
;
;   maximum         -   specify that the covariant matrix is a result of MVAE
;
; KEYWORD INPUTS
;
;   eigenvalues     -   optionally return the eigenvalues of the constrained
;                       variance analysis frame
;
; RETURN VALUE
;
;   eigvecs         -   the direction of the variance axes as viewed from the
;                       initial frame of reference. The eigenvectors are along the
;                       rows where row 1 corresponds to the first eigenvector, etc.
;
;REFERENCES
;   Paschmann, G., Daly, P.W., Analysis Methods for Multi-Spacecraft
;       Data. ISSI Scientific Report, Chp. 8, Pg. 195.
;
; MODIFICATION HISTORY:
;
;       Written by:  Matthew Argall 22 November 2011
;       03/04/2012 - added keyword MAXIMUM and ordered eigenvalues and
;			            eigenvectors properly, depending on whether MVAE or MVAB
;			            was performed to get the covariant matrix, M
;-
;******************************************************************************************;

function constrained_mva, M, e, $
;KEYWORDS
EIGENVALUES = eigenvalues, $
MAXIMUM = maximum

	;the constraint that makes dot_product(e, n) = 0
	e_ij = rebin(e, 3, 3) * transpose(rebin(e, 3, 3))
	
	;the projection matrics that describes a vector projected onto the
	;plane perpendicular to e.
	P = identity(3) - e_ij
	
	;the constrained problem: PMP*n = lambda*n
	PMP = P ## M ## P
	
	;the variance basis and its eigenvalues
	eigenvalues = hqr(elmhes(PMP, /double), /double)
	
	;sort the eigenvalues -- ascending for MVAB, descending for MVAE
	;such that they are ordered n, m, l (reverse of Multi-Spacecraft Methods)
	;hqr returns a row vector, so no need to specify a dimension in reverse()
	if keyword_set(maximum) then eigenvalues = reverse(eigenvalues[sort(eigenvalues)]) $
							else eigenvalues = eigenvalues[sort(eigenvalues)]
	
	;compute the eigenvectors of PMP 
	eigenvectors = eigenvec(PMP, eigenvalues, /double)
	
	;return the basis vectors of the variance frame
	return, eigenvectors

end
