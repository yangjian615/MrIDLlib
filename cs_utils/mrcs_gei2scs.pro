; docformat = 'rst'
;
; NAME:
;       mms_fdoa_xgei2despun
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
;+
;   Return a transformation matrix from GEI to a despun satellite coordinate system.
;
; :Params:
;       YEAR:           in, required, type=intarr
;                       4-digit integer year
;       MONTH:          in, required, type=intarr
;                       2-digit integer month
;       DAY:            in, required, type=intarr
;                       2-digit integer day
;       SEC:            in, required, type=fltarr
;                       Seconds elapsed since midnight on `DAY`
;       RA:             in, required, type=fltarr
;                       Right-ascension of spacecraft z-axis as viewed from GEI.
;       DEC:            in, required, type=fltarr
;                       Declination of spacecraft z-axis as viewed from GEI.
;
; :Returns:
;       GEI2SCS:        Transformation from GEI to spacecraft coordinates
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
;       2015-09-25  -   Written by Matthew Argall
;       2015-11-26  -   Inputs can be row or column vectors - MRA
;-
function MrCS_gei2scs, year, month, day, sec, ra, dec
	compile_opt idl2
	on_error, 2
	
	;Convert to radians
	ra_rad  = ra * !dtor
	dec_rad = dec * !dtor

	;Fractional number of days since the beginning of the year of interest.
	iday = julday(month, day, year) - julday(1, 1, year) + 1

	;Location of the sun
	r_sun = MrCS_sun_position(year, iday, sec)   ; in what coords? normalized?

	;RA and DEC form a spherical coordinate system.
	;   - RA  = number of hours past the vernal equinox (location on the
	;           celestial equator of sunrise on the first day of spring).
	;   - DEC = degrees above or below the equator
	cosDec = cos(dec_rad)

	;[x y z] components of the unit vector pointing in the direction of
	;the spin axis.
	;   - The spin axis points to a location on the suface of the celestial sphere.
	;   - RA and dec are the spherical coordinates of that location,
	;     with the center of the earth as the origin.
	;   - Transforming GEI to SCS transforms [0 0 1] to [x y z] = OMEGA
	;   - Already normalized: spherical to cartesian with r = 1.
	scsz      = fltarr(3, n_elements(ra_rad))
	scsz[0,*] = cos(ra_rad) * cosDec
	scsz[1,*] = sin(ra_rad) * cosDec
	scsz[2,*] = sin(dec_rad)

	;Form the X- and Y-vectors
	;   - X must point in the direction of the sun.
	;   - To ensure this, Y' = Z' x Sun
	;   - X' = Y' x Z'
	scsy = mrvector_cross(scsz, r_sun)
	scsy = mrvector_normalize(scsy)
	scsx = mrvector_cross(scsy, scsz)

	;Transformation from GEI to SCS.
	gei2scs        = fltarr(3, 3, n_elements(ra))
	gei2scs[*,0,*] = scsx
	gei2scs[*,1,*] = scsy
	gei2scs[*,2,*] = scsz

	return, gei2scs
end