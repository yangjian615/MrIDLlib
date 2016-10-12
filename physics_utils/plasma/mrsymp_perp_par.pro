; docformat = 'rst'
;
; NAME:
;       MrSymP_perp_par
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Diagonalize the pressure tensor using the magnetic field. Assumes the
;   pressure tensor is symmetric.
;
; :Categories:
;       Plasma Utilities
;
; :Params:
;       V:          in, required, type=Nx3 fltarr
;                   A unit vector that defines the parallel directions.
;       P:          in, required, type=Nx3x3 fltarr
;                   Symmetric pressure tensor. The first dimension must be the same
;                       size as that of `V`.
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
;       2016-04-04  -   Written by Matthew Argall
;-
function MrSymP_perp_par, B, P, $
P_TOTAL=P_total
	compile_opt idl2
	on_error, 2

	;Check inputs
	szB = size(B)
	szP = size(P)
	if szP[0] ne 3 || szP[2] ne 3 || szP[3] ne 3 then message, 'P must be an Nx3x3 array.'
	if szB[0] ne 2 || szB[2] ne 3 then message, 'B must be Nx3.'
	if szB[1] ne szP[1] then message, 'B and P must have the same number of points.'
	nPts = szB[1]
	
	;Rotate P about Z-axis by the angle between X and the projection of B on the XY plane
	ph            = atan(B[*,1], B[*,0])
	rot_ph        = fltarr(nPts, 3, 3)
	rot_ph[*,0,0] =  cos(ph)
	rot_ph[*,1,0] = -sin(ph)
	rot_ph[*,0,1] =  sin(ph)
	rot_ph[*,1,1] =  cos(temporary(ph))
	rot_ph[*,2,2] =  1.0
	P1            = MrMatrix_Rotate( temporary(rot_ph), P)
	
	;Then rotate P about Y-axis by the angle between Bz and B 
	th            = !pi/2.-atan(B[*,2], sqrt(B[*,0]^2 + B[*,1]^2))
	rot_th        = fltarr(nPts, 3, 3)
	rot_th[*,0,0] =  cos(th)
	rot_th[*,2,0] =  sin(th)
	rot_th[*,1,1] =  1.0
	rot_th[*,0,2] = -sin(th)
	rot_th[*,2,2] =  cos(temporary(th))
	P2            = MrMatrix_Rotate(temporary(rot_th), temporary(P1))
	
	;Finally diagonalize Pxx and Pyy
	;   - Eigenvalues of:
	;         | Pxx  Pxy   0  |
	;         | Pyx  Pyy   0  |
	;         |  0    0   Pzz |
	l1 = 0.5 * ( p[*,0,0]+p[*,1,1] + sqrt(p[*,0,0]^2+p[*,1,1]^2-2.*p[*,0,0]*p[*,1,1]+4.0*p[*,0,1]*p[*,0,1]) )
	l2 = 0.5 * ( p[*,0,0]+p[*,1,1] - sqrt(p[*,0,0]^2+p[*,1,1]^2-2.*p[*,0,0]*p[*,1,1]+4.0*p[*,0,1]*p[*,0,1]) )
	
	;Rotation angle to diagonalize
	;   - Works only for symmetric 2x2 matrices (see reference).
	;   - Note that cos(ph)^2 = 0.5 * [cos(2ph) + 1]
	;   - In eq. 10, denominator is (l1 - l2)
	;   - Multiply top and bottom by (a+b) noting that (a+b) = (l1+l2)
	;   - (a-l1)^2 - (b-l2)^2 = 0 after substituting in eigenvalues as written above.
	ph = fltarr(nPts)
	i  = where(l1 eq l2, n)
	if n gt 0    then ph[i] = 0.0
	if n lt nPts then ph[i] = acos( (( l1[i]*P2[i,0,0] - l2[i]*P2[i,1,1]) / sqrt(l1[i]^2 - l2[i]^2)) )
	
	;Rotate about the z-axis
	rot_ph = fltarr(nPts, 3, 3)
	rot_ph[*,0,0] =  cos(ph)
	rot_ph[*,1,0] = -sin(ph)
	rot_ph[*,0,1] =  sin(ph)
	rot_ph[*,1,1] =  cos(temporary(ph))
	rot_ph[*,2,2] =  1.0
	
	;Diagonalize
	;     | P-perp1     0       0   |
	;     |    0     P-perp2    0   |
	;     |    0        0     P-par |
	P_diag = MrMatrix_Rotate(temporary(rot_ph), temporary(P2))
	P_diag = (reform(P_diag, nPts, 9))[*,[0,4,8]]

	;Total pressure
	P_total = mean(P_diag, DIMENSION=2)
	
	;Return the diagonal components
	return, P_diag
END