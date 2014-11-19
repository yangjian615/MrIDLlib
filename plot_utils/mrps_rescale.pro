; docformat = 'rst'
;
;+
;   Rescale character sizes and thicknesses and line thicknesses for postscript output.
;
; :Params:
;       VALUE:          in, optional, type=numeric
;                       The value to be rescaled. If not provided, one of the defaults
;                           will be used::
;                               Charsize    -   1.0
;                               CharThick   -   3.0
;                               Thick       -   3.0
;                               XThick      -   3.0
;                               YThick      -   3.0
;                               ZThick      -   3.0
;
; :Keywords:
;       CHARSIZE:       in, optional, type=boolean=
;                       Indicate that `VALUE` is a character size. If no other keywords
;                           are set, this is assumed.
;       CHARTHICK:      in, optional, type=boolean, default=0
;                       Indicate that `VALUE` is a character thickness.
;       THICK:          in, optional, type=boolean, default=0
;                       Indicate that `VALUE` is a line thickness.
;       XTHICK:         in, optional, type=boolean, default=0
;                       Indicate that `VALUE` is a line thickness for the x-axis.
;       YTHICK:         in, optional, type=boolean, default=0
;                       Indicate that `VALUE` is a line thickness for the y-axis.
;       ZTHICK:         in, optional, type=boolean, default=0
;                       Indicate that `VALUE` is a line thickness for the z-axis.
;
; :Returns:
;       OUT:            Rescaled number for `VALUE` suitable for postscript output.
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
;   Modification History::
;       2014/11/18  -   Written by Matthew Argall.
;-
function MrPS_Rescale, value, $
CHARSIZE=charsize, $
CHARTHICK=charthick, $
THICK=thick, $
XTHICK=xthick, $
YTHICK=ythick, $
ZTHICK=zthick
	compile_opt idl2
	on_error, 2
	
	case 1 of
		keyword_set(charsize):  out = n_elements(value) eq 0 ? 1.0 : value / 1.5
		keyword_set(charthick): out = n_elements(value) eq 0 ? 3.0 : value * 3.0
		keyword_set(thick):     out = n_elements(value) eq 0 ? 3.0 : value * 3.0
		keyword_set(xthick):    out = n_elements(value) eq 0 ? 3.0 : value * 3.0
		keyword_set(ythick):    out = n_elements(value) eq 0 ? 3.0 : value * 3.0
		keyword_set(zthick):    out = n_elements(value) eq 0 ? 3.0 : value * 3.0
		else:                   out = n_elements(value) eq 0 ? 1.0 : value / 1.5
	endcase
	
	return, out
end

