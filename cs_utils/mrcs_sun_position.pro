; docformat = 'rst'
;
; NAME:
;       mrcs_sun_position
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
;   Calculate the position of the sun in GEI coordinates.
;
; :Params:
;       IYEAR:          in, required, type=intarr
;                       4-digit integer year
;       DOY:            in, required, type=intarr
;                       Day of year
;       SSM:            in, required, type=fltarr
;                       Seconds elapsed since midnight on `DOY`
;
; :Returns:
;       R_SUN:          Position of sun in GEI coordinates.
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
;       2015-09-25  -   Written by Matthew Argall.
;-
function mrcs_sun_position, IYR, IDAY, SECS
	compile_opt idl2
	on_error, 2

	;Make sure the date is within range.
	if total( IYR lt 1901 ) gt 0 || total( IYR gt 2099 ) gt 0 $
		then message, 'Year must be between 1901 and 2099.'

	;Constants
	RAD = 57.29578         ; 180/pi

	;Convert seconds to days.
	FDAY=SECS/86400

	;Number of days since noon on 1 Jan 1900.
	DDJ = 365 * (IYR-1900) + fix((IYR-1901) / 4) + IDAY - 0.5
	DJ  = DDJ * make_array(n_elements(SECS), VALUE=1.0, /FLOAT) + FDAY

	; Convert to Julian centuries from 1900
	;   - Julian Year:    Exactly 365.25 days of 86,400 SI seconds each.
	;   - Julian Century: 36,525 days
	T   = DJ / 36525

	; Degrees per day
	;   - It takes 365.2422 days to complete a revolution about the sun.
	;   - There are 360 degrees in a circle.
	;  => 360.0 / 365.2422 = 0.9856 degrees/day

	; Keep degrees between 0 and 360
	;   mod(..., 360) will force answer to be in range [0, 360).


	; Mean longitude of the sun
	VL     = 279.696678 + 0.9856473354 * DJ mod 360

	; Greenwhich sidereal time.
	GST    = 279.690983 + 0.9856473354 * DJ + 360 * FDAY + 180. mod 360

	; Mean anomaly
	G      = (358.475845 + 0.985600267 * DJ mod 360) / RAD

	; Ecliptic longitude
	SLONG  = VL + (1.91946 - 0.004789 * T) * sin(G) + 0.020094 * sin(2 * G)

	; Obliquity (Axial tilt)
	OBLIQ  = (23.45229 - 0.0130125 * T) / RAD


	SLP    = (SLONG - 0.005686) / RAD
	SIND   = sin(OBLIQ) * sin(SLP)
	COSD   = sqrt(1 - SIND^2)

	; Solar declination
	SDEC   = RAD * atan(SIND / COSD)

	; Solar right ascension
	SRASN  = 180 - RAD * atan(SIND / (tan(OBLIQ) * COSD), -cos(SLP) / COSD)

	; Equatorial rectangular coordinates of the sun
	r_sun      = fltarr(3, n_elements(SECS))
	r_sun[0,*] = cos(SRASN/RAD) * COSD
	r_sun[1,*] = sin(SRASN/RAD) * COSD
	r_sun[2,*] = SIND
	
	return, r_sun
end