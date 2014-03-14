; docformat = 'rst'
;
; NAME:
;       LOAD_COLOR
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
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
;       LOAD_COLOR returns either an 8-bit color index or a 24-bit true
;       color number of the requested color, depending on the decomposed
;       state of the device.
;
;       If 8-bit color is being used, the input colors are loaded into
;       the current color table. If 24-bit color mode is in effect, then
;       the 24-bit number associated with the color is returned.
;
;       To obtain a list of available colors, call LOAD_COLOR without any
;       arguments.
;
; :Categories:
;
;       Graphics Utility, Color
;
; :Params:
;
;	COLORS:         in, optional, type=String/strarr.
;                   The names of the desired colors. If not given, the names of the
;                       available colors will be returned and printed to the command window.
;
; :Keywords:
;
;   BOTTOM:         in, optional, type=Int, default=1
;                   The lowest index value at which to start loading colors.
;   TOP:            in, optional, type=Int, default=255
;                   The highest index into which a color should be loaded.
;   ITOP:           out, type=Long
;                   The highest index into which a color was loaded.
;
; :Returns:
;
;	INDEX:          The 8 bit color index or 24 bit true color number
;			            of the desired colors. If no argument is given, then the list of
;                       available colors is returned.
;
; :Uses:
;   Uses the following external programs::
;       getdecomposedstate.pro (Coyote Graphics)
;       ismember.pro
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
;       Written by  -   Matthew Argall 2 October 2011
;	    02/10/2011  -   added LIST_COLORS keyword - MRA
;       01/04/2012  -   made COLOR can now be an array
;                           turned color list into a hash variable
;                           removed keyword LIST_COLORS
;                       if no positional parameter is given, the color 
;                           table list is printed
;       02/06/2012  -   added keywords BOTTOM, TOP, and ITOP
;       08/14/2012  -   return !Null and print message if color is not found
;       11/02/2012  -   Made the default value of TOP=255
;       11/19/2012  -   Total revision to use the !COLOR system variable
;       17/03/2013  -   Simplified color-matching with call to "ismember" - MRA
;-
function load_color, colors, $
BOTTOM = bottom, $
ITOP = itop, $
TOP = top
    compile_opt idl2

    ;get the list of colors from the !COLOR system variable
    color_list = transpose(tag_names(!COLOR))

    ;Make a hash variable to augment the list of colors (old way of doing it)
    ;color_table = hash() ;e.g., 'red', [255, 0, 0]
    
    ;if a list of colors is desired, print the keys of the color table hash
    if n_params() eq 0 then begin
        print, color_list, format='(a-20, 5x, a-20, 5x, a-20)'
        return, color_list
    endif
    
    ;find the colors in the color list
    isColor = ismember(color_list, colors, /FOLD_CASE, $
                       N_MATCHES=n_matches, A_INDICES=theseColors, $
                       NONMEMBER_INDS=notColors)
    
    ;If not found, display the unavailable colors and return
    if n_matches eq 0 then begin
        print, 'Color Not Found: ' + colors[notColors]
        return, !NULL
    endif
    
    ;set the bottom index at which to load the colors
    if n_elements(bottom) eq 0 then bottom = 1
    if n_elements(top) eq 0 then top = 255
    
    ;get the decomposed state of the device
    decomposed_state = getdecomposedstate()

    ncolors = n_elements(colors)
    color_index = lonarr(ncolors)
    index = bottom

    ;if the decomposed state is PSEUDOCOLOR then return a color table index
    if decomposed_state eq 0 then begin
        ;load the colors into the color table, starting at index 1
        ;(index 0 is a primary/pure color, so I avoid using it)
        foreach icolor, theseColors do begin
            ;break out of the loop once INDEX > TOP
            if index gt top then break
            
            ;load the colors into the color table at index=INDEX
            tvlct, !COLOR.(icolor)[0], !COLOR.(icolor)[1], !COLOR.(icolor)[2], index
            color_index[index-bottom] = index
            index += 1
        endforeach
        
    ;if the decomposed state is TRUE COLOR then return a true color 24-bit number
    endif else foreach icolor, theseColors do begin
        color_index[index-bottom] = color_trip(!COLOR.(icolor))
        index += 1
    endforeach
    itop = index - 1

    ;return the color index and destroy the color_table hash object
    if ncolors eq 1 then color_index = color_index[0]
    return, color_index
end
