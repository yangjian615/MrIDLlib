; docformat = 'rst'
;
; NAME:
;       COLOR_TRIP
;
; PURPOSE:
;+
;       The purpose of this program is to convert a color triple [red, gree, blue] to a
;       true color, 24-bit number. If the TO_TRIPLE keyword is set, then the color triple
;       of a 24-bit number is calculated.
;
; :Categories:
;       Graphics Utility, Color
;
; :Params:
;   COLOR:          in, required, type="lonarr(3,*) / lonarr(*)"
;                   A 3xN array of color triples to be converted to 24-bit numbers. If
;                       the TO_TRIPLE keyword is set, then COLOR is an array of 24-bit 
;                       numbers.
;
; :Keywords:
;   TO_TRIPLE:      in, optional, type=Boolean, default=0,
;                   Indicate that 24-bit numbers were given and that they are to be
;                       converted to color triples.
;
; :Returns:
;   COLORS_OUT:     The 8-bit color triple or 24-bit true color number
;			            of the desired colors.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:

;	Modification History::
;
;       Written by  -   Matthew Argall 08 September 2011
;       11/19/2012  -   TO_TRIPLE calculations now correct.
;-
function color_trip, color, $
;KEYWORD
TO_TRIPLE = to_triple

	if keyword_set(to_triple) then begin
		c24 = color

		b = c24 / 2L^16
		g = (c24 - b*2L^16) / 2L^8
		r = c24 - b*2L^16 - g*2L^8
		colors_out = [r, g, b]
	endif else begin
		colors_out = color[0,*]*2L^0 + color[1,*]*2L^8 + color[2,*]*2L^16
	endelse
	
	return, colors_out

end
