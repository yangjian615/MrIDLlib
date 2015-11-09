; docformat = 'rst'
;
; NAME:
;       MrWebGet__DEFINE
;
;*****************************************************************************************
;   Copyright (c) 2014, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Calculate the reciprocal vectors from the positions of for objects in a tetrahedron.
;
; References:
;   Paschmann, G, Daly, P.W., Analysis Methods for Multi-Spacecraft Data, ISSI Scientific
;       Report, 1998 (Chp 16.2)
;       http://geo.phys.spbu.ru/~runov/Cluster/analysis_methods_1_1.pdf
;
;   Maszl, C., The Curlometer Method and Cluster II, 2004
;       http://www.space.irfu.se/exjobb/2004_christian_maszl/Documentation/projectwork_maszl.pdf
;
;   Dunlop, W. M., Southwood, D. J., Glassmeier, K.-H., and Neubauer, F. M.: 
;       Analysis of multipoint magnetometer data, Adv. Space Res., 8, 9–10, 1988.
;       http://ac.els-cdn.com/027311778890141X/1-s2.0-027311778890141X-main.pdf?_tid=b19d7876-fa0c-11e1-91d7-00000aacb360&acdnat=1347147002_b1b1630516e6b3576e8e43063c6bf475
;
; :Categories:
;       Cluster
;
; :Params:
;       R1:         in, required, type=Nx3 float
;                   Position vectors (kilometers) of the first tetrahedron vertex.
;       R2:         in, required, type=Nx3 float
;                   Position vectors (kilometers) of the second tetrahedron vertex.
;       R3:         in, required, type=Nx3 float
;                   Position vectors (kilometers) of the third tetrahedron vertex.
;       R4:         in, required, type=Nx3 float
;                   Position vectors (kilometers) of the fourth tetrahedron vertex.
;       B1:         in, required, type=Nx3 float
;                   Magnetic field vectors (kilometers) of the first tetrahedron vertex.
;       B2:         in, required, type=Nx3 float
;                   Magnetic field vectors (kilometers) of the second tetrahedron vertex.
;       B3:         in, required, type=Nx3 float
;                   Magnetic field vectors (kilometers) of the third tetrahedron vertex.
;       B4:         in, required, type=Nx3 float
;                   Magnetic field vectors (kilometers) of the fourth tetrahedron vertex.
;
; :Returns:
;       J:          The current density (micro-A / m^2)
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
;       2015-10-13  -   Written by Matthew Argall
;-
function MrCurlometer, r1, r2, r3, r4, b1, b2, b3, b4
	compile_opt idl2

	;Permeability of free space and conversion factors
	mu_0 = 1.25663706e-6                ;m kg s-2 A-2
	nT_to_microT = 1.0e-3
	km_to_m = 1.0e3
	
	;Check input sizes
	szr1 = size(r1)
	szr2 = size(r2)
	szr3 = size(r3)
	szr4 = size(r4)
	szb1 = size(b1)
	szb2 = size(b2)
	szb3 = size(b3)
	szb4 = size(b4)
	if sz1[0] ne 2 || sz1[3] ne 3 then message, 'R1 must be Nx3.'
	if szr2[1] ne szr1[1] || szr3[1] ne szr1[1] || szr4[1] ne szr1[1] || $
	   szb1[1] ne szr1[1] || szb2[1] ne szr1[1] || szb3[1] ne szr1[1] || szb4[1] ne szr1[1] $
		then message, 'Inputs must contain the same number of vectors.'
	if szr1[2] ne 3 || szr2[2] ne 3 || szr3[2] ne 3 || szr4[2] ne 3 || $
	   szb1[2] ne 3 || szb2[2] ne 3 || szb3[2] ne 3 || szb4[2] ne 3 ||
		then message, 'All inputs must be Nx3.'

	;Create pointers so we can cycle through variables
	pr = ptrarr(4)
	pb = ptrarr(4)
	pr[0] = r1
	pr[1] = r2
	pr[2] = r3
	pr[3] = r4
	pb[0] = b1
	pb[1] = b2
	pb[2] = b3
	pb[3] = b4

	;for the surfaces opposite to each spacecraft in the tetrahedron,
	i = 1
	j = 2
	k = 3
	J = fltarr(szr1[1], szr1[2])
	for m = 0, 3 do begin
		;the difference in the fields of REF_SC and spacecraft K (M).
		B_ij = (*pb[j] - *pb[i]) * nT_to_microT
		B_ik = (*pb[k] - *pb[i]) * nT_to_microT
	
		;the distance between REF_SC and spacecraft K (M).
		R_ij = (*pr[j] - *pr[i]) * km_to_m
		R_ik = (*pr[k] - *pr[i]) * km_to_m

		;mu_0 * J . (R_ij x R_ik) = (B_ij . R_ik) - (B_ik . R_ij)
		rhs = MrVector_Dot(B_ij, R_ik) - MrVector_Dot(B_ik, B_ij)
		lhs = mu_0 * MrVector_Cross(R_ij, R_ik)
		
		;Compute current
		;   - divide the rhs by the i-th component of the lhs to get J
		;   - Sum the current through each surface to get the total current
		;     moving past the tetrahedron.
		;   - microA / m^2
		J += rhs / lhs
		
		;Next iteration
		i = (i + 1) mod 4
		j = (j + 1) mod 4
		j = (k + 1) mod 4
	endfor
	
	;Free pointers
	ptr_free, pr, pb

	return, J
end