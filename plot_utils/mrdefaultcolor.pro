; docformat = 'rst'
;
; NAME:
;       MrDefaultColor
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   Choose a default color.
;
;   cgDefaultColor is used directly if::
;       COLOR is provided
;       DEFAULT is provided
;       NCOLORS = 1, 3
;
;   In all other cases, a color table is loaded and colors are taken at equally spaced
;   intervals throughout the available color range (see `CLIP` and `BOTTOM`). If `MODE`=0
;   (indexed color mode), the colors are left in the color table and their indices are
;   returned. If `MODE`=1, no lasting changes to the color table are made and 24-bit
;   decompsed colors are returned (unless otherwise specified).
;   
; :Categories:
;    Plot Utilities, Wrapper, Coyote Graphics
;
; :Params:
;       COLOR:          in, optional, type=string/integer/long, default='black'
;                       Name, color table index, or 24-bit decomposed color of the
;                           desired color.
;
; :Keywords:
;       BACKGROUND:     in, optional, type=boolean, default=0
;                       If set, the colors are treated as background colors. Otherwise,
;                           they are drawing colors. Used only with cgDefaultColor().
;       BOTTOM:         in, optional, type=integer, default=1
;                       Indicate at which index the first color will be located. If
;                           BOTTOM > !d.table_size-`NCOLORS`, then the latter value is
;                           used.
;       CLIP:           in, optional, type=integer/intarr(2), default=[0,255]
;                       The [bottom, top] color table indices from which colors will
;                           be drawn. If a scalar integer is given, it indicates the
;                           the bottom and the top is taken to be 255. Colors in the
;                           color table below "bottom" and above "top" are ignored.
;                           Only used when `COLOR` is undefined.
;       CTINDEX:        in, optional, type=integer, default=13
;                       Color table from which default colors should be drawn.
;       DEFAULT:        in, optional, type=byte/int/long/string (array)
;                       Default colors to use if `CTINDEX` is not specified. If `NCOLORS`
;                           is three, then the DEFAULT=['Blue', 'Forest Green', 'Red'].
;       MODE:           in, optional, type=int, default=Current color mode
;                       The color mode. A 0 means indexed color mode. A 1 means decomposed
;                           color mode. If not supplied in the call, the color mode is
;                           determined at run-time with `cgGetColorState`.
;       NCOLORS:        in, optional, type=int
;                       Desired number of output colors. If not given, output will have
;                           the same number of elements as `COLOR` or `DEFAULT` or 1,
;                           depending on which are defined.
;       RGB_TABLE:      out, optional, type=Nx3 bytarr
;                       Named variable into which the resulting color table is returned.
;       ROW:            in, optional, type=boolean, default=0
;                       Direct graphics require Nx3 color tables. Object graphics require
;                           3xN color tables. If ROW is set, a 3xN table is returned. This
;                           applies to `RGB_TABLE` and `COLORSOUT` when `TRIPLE`=1.
;       TRADITIONAL:    in, optional, type=boolean, default=0
;                       If set, IDL traditional colors are used (white for drawing color
;                           and black for background color).
;       TRIPLE:         out, optional, type=Nx3 or 3xN bytarr
;                       If set, color triples are returned, regardless of `MODE`. See
;                           `ROW` for output options.
;
; :Returns:
;       COLORSOUT:      Default colors. If `MODE`=0, indices into the current color table
;                           are returned (unless `TRIPLE`=1). If `MODE`=1 and `COLOR` is
;                           a byte or integer, values are regarded as indices into the
;                           current color table. Colors are extracted and converted to
;                           24-bit decomposed colors. If longs are given, they are
;                           considered 24-bit decomposed colors.
;
; :Uses:
;   Uses the following external programs::
;       cgColor24.pro
;       cgDefaultColor.pro
;       cgErrorMSG.pro
;       cgGetColorState.pro
;       cgLoadCT.pro
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
;   Change History::
;       2014/11/01  -   Written by Matthew Argall
;-
function MrDefaultColor, color, $
BACKGROUND=background, $
BOTTOM=bottom, $
CLIP=clip, $
CTINDEX=ctIndex, $
DEFAULT=default, $
MODE=mode, $
NCOLORS=nColors, $
RGB_TABLE=rgb_table, $
ROW=row, $
TRADITIONAL=traditional, $
TRIPLE=triple
	compile_opt strictarr

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(r) gt 0 then tvlct, r, g, b
		void = cgErrorMSG()
		return, -1
	endif

	;Defaults
	if n_elements(ctIndex) eq 0 then ctIndex = 13
	if n_elements(default) eq 0 then default = cgDefaultColor(TRADITIONAL=traditional, BACKGROUND=background)
	nDefaults = n_elements(default)
	
	;Number of requested colors is taken from::
	;   1. Color
	;   2. NColors
	;   3. Defaults
	;   4. 1
	nColorsIn = n_elements(color)
	if n_elements(nColors) eq 0 then nColors = nColorsIn
	if nColors             eq 0 then nColors = nDefaults
	
	;If the number of colors requested is less than the number of colors given, error
	if nColors lt nColorsIn then message, 'COLOR must have <= NCOLORS number of elements.'

	;If a single default was given, it is the default for all colors.
	if nDefaults eq 1 && nColors gt 1 then begin
		_default  = replicate(default, nColors)
		nDefaults = nColors

	;The number of elements in DEFAULT must be the same as tne number of colors requested.
	endif else begin
		if nDefaults ne nColors then $
			message, 'DEFAULTS must have NCOLORS number of elements.'
		_default = default
	endelse

;---------------------------------------------------------------------
; cgDefaultColor /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Use cgDefaultColor straight away if:
	;   - COLOR was provided.
	;   - Default colors were provided.
	;   - One or three colors were requested.
	if (nColorsIn gt 0) || (nDefaults gt 0) || (nColors eq 1 || nColors eq 3) then begin
		if nColors eq 3 && nDefaults eq 0 then _default = ['Blue', 'Forest Green', 'Red']

		;Use cgDefaultColor.
		colorsOut = cgDefaultColor(color, BACKGROUND=background, DEFAULT=_default, $
		                           MODE=mode, TRADITIONAL=traditional)
		
		;Make sure the output colors have NCOLORS number of elements.
		nColorsOut = n_elements(colorsOut)
		if nColorsOut ne nColors then begin
			temp_out                       = make_array(nColors, TYPE=size(colorsOut, /TYPE))
			temp_out[0:nColorsOut-1]       = colorsOut
			temp_out[nColorsOut:nColors-1] = _default[nColorsOut:nColors-1]
			colorsOut                      = temporary(temp_out)
		endif
		
		return, colorsOut
    endif

;---------------------------------------------------------------------
; Take from Color Table //////////////////////////////////////////////
;---------------------------------------------------------------------

	;More defaults
	row     = keyword_set(row)
	triple  = keyword_set(triple)
	_bottom = n_elements(bottom) eq 0 ? 1 : !d.table_size-nColors < bottom

	;Color mode
	;   - In PS, drawing is always DECOMPOSED=0
	;   - Must over-ride this to return requested type.
	thisState = cgGetColorState()
	thisMode  = n_elements(mode) eq 0 ? thisState : keyword_set(mode)

	;Save the color table
	tvlct, r, g, b, /GET

	;Load the desired index
	;   - Load only the colors that will be used as defaults.
	cgLoadCT, ctIndex, NCOLORS=nColors, BOTTOM=_bottom, CLIP=clip

	;Get the colors
	tvlct, r_out, g_out, b_out, /GET
	rgb_table = [[r_out], [g_out], [b_out]]

	;Index color mode?
	if thisMode eq 0 then begin
		;Maintain the new color table & return indices of loaded colors
		if triple then begin
			colorsOut = rgb_table[1:nColors, *]
			if row then colorsOut = transpose(colorsOut)
		endif else begin
			colorsOut = bindgen(nColors) + _bottom
		endelse
	
	;Decomposed color mode
	endif else begin

		;Return the color table?
		if arg_present(rgb_table) then begin
			if row then rgb_table = transpose(rgb_table)
		endif

		;Reduce colors
		colorsOut = rgb_table[1:nColors, *]

		;Return color triples or decomposed colors?
		if triple then begin
			if row then colorsOut = transpose(colorsOut)
		endif else begin
			colorsOut = cgColor24(colorsOut)
		endelse
	
		;Restore the color table
		tvlct, r, g, b
	endelse

	return, colorsOut
end