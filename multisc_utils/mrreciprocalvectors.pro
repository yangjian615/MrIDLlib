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
;   References::
;       Paschmann, G, Daly, P.W., Analysis Methods for Multi-Spacecraft Data, ISSI Scientific
;           Report, 1998 (Chp 14.2)
;           http://geo.phys.spbu.ru/~runov/Cluster/analysis_methods_1_1.pdf
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
;
; :Returns:
;       RECVEC:     An Nx3x4 array of reciprocal vectors (1/meters). The fourth
;                       dimensions is the tetrahedron vertex associated with the
;                       reciprocal vector.
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
function MrReciprocalVectors, r1, r2, r3, r4
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(pr) gt 0 then ptr_free, pr
		void = cgErrorMSG(/QUIET)
		return, -1
	endif

	;count the number of spacecraft and the number of data points per component
	sz1 = size(r1)
	sz2 = size(r2)
	sz3 = size(r3)
	sz4 = size(r4)
	if (sz1[0] ne 1 || sz1[0] ne 2) && sz1[1] ne 3 then message, 'R1 must be 3xN.'
	if sz2[1] ne sz1[1] || sz3[1] ne sz1[1] || sz4[1] ne sz1[1] $
		then message, 'All inputs must be 3xN.'
	if (sz1[0] eq 2) && (sz2[2] ne sz1[2] || sz3[2] ne sz1[2] || sz4[2] ne sz1[2]) $
		then message, 'Inputs must contain the same number of vectors.'
	nv = sz1[0] eq 1 ? 1 : sz1[2]

	;Create a poitner array that we can cycle through
	recvec = fltarr(3, nv, 4)
	pr     = ptrarr(4)
	pr[0]  = ptr_new(r1)
	pr[1]  = ptr_new(r2)
	pr[2]  = ptr_new(r3)
	pr[3]  = ptr_new(r4)

	;initialize some indices
	j = 1
	k = 2
	m = 3
	for i = 0, 3 do begin
		;calculate the separation between spacecraft
		;r_ij = r_j - r_i   --  the position vector pointing from spacecraft i to
		;                       spacecraft j
		r_ij = (*pr[j] - *pr[i])
		r_ik = (*pr[k] - *pr[i])
		r_im = (*pr[m] - *pr[i])

		;the reciprocal vector for vertex m of the tetrahedron points normal to the area
		;of the face of the tetrahedron opposite to vertex m. The normal vector to this
		;surface can be found by taking the cross product between to vectors that lie in
		;the plane of the surface.
		area = MrVector_Cross(r_ij, r_ik)
	
		;calculate the volume of the tetrahedron
		volume = MrVector_Dot(r_im, MrVector_Cross(r_ij, r_ik))
	
		;the reciprical vector is the area's normal vector normalized to the tetrahedron
		;volume
		recvec[0,0,i] = [ area[0,*] / volume, $
		                  area[1,*] / volume, $
		                  area[2,*] / volume ]

		;increase the indices cyclically
		j = (j + 1) mod 4
		k = (k + 1) mod 4
		m = (m + 1) mod 4
	endfor
	
	;Free the pointers
	ptr_free, pr

	return, recvec
end