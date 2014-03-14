; docformat = 'rst'
;
; NAME:
;   MRCREATECT
;
;+
;   The purpose of this program is to create different colors tables that can be loaded
;   via TVLCT
;
; :Categories:
;   Plot Utility
;
; :Keywords:
;       CLIP:           in, optional, type=integer/intarr(2)
;                       The lower and upper bound on the color table indices to load. If
;                           CLIP is a scalar, it is the lower bound.
;       NCOLORS:        in, optional, type=boolean, default=256
;                       The number of colors used to create the color table.
;       REVERSE:        in, optional, type=boolean, default=0
;                       Reverse the selected colortable.
;       ROW:            in, optional, type=boolean, default=0
;                       TVLCT takes an Nx3 array whereas object graphics take a 3xN array.
;                           Set this keyword to return a 3xN array
;
;       BLUE:           in, optional, type=boolean, default=0
;                       A color table that ranges linearly from white to blue.
;       GREEN:          in, optional, type=boolean, default=0
;                       A color table that ranges linearly from white to green.
;       RED:            in, optional, type=boolean, default=0
;                       A color table that ranges linearly from white to red.
;       RWB:            in, optional, type=boolean, default=0
;                       A color table that ranges evenly from red to white to blue.
;
; :Returns:
;       color_table:    The color table specified by one of the keywords.
;
; :Uses:
;   Uses the following external programs::
;       linspace.pro
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
;       09/23/2013  -   Written by Matthew Argall
;       10/02/2013  -   Added the RED, GREEN, BLUE, NCOLORS and CLIP keywords.
;-
function MrCreateCT, $
CLIP = clip, $
NCOLORS = nColors, $
REVERSE = reverse, $
ROW = row, $
;Color Table Names
RED = red, $
GREEN = green, $
BLUE = blue, $
RWB = rwb
    compile_opt idl2
    on_error, 2
    
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    blue  = keyword_set(blue)
    green = keyword_set(green)
    red   = keyword_set(red)
    rwb   = keyword_set(rwb)
    if (blue + green + red + rwb ne 1) then message, 'One and only one colortable is allowed.'
    
    reverse = keyword_set(reverse)
    row = keyword_set(row)
    if n_elements(ncolors) eq 0 then ncolors = 256
    if n_elements(clip) eq 0 then clip = [0, 255]
    if n_elements(clip) eq 1 then clip = [clip, 255]
    clip = 0 > clip < 255
    
;---------------------------------------------------------------------
;Create Color Table //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    case 1 of
        ;WHITE->BLUE
        blue: begin
            r = linspace(255, 0, 256)
            g = linspace(255, 0, 256)
            b = bytarr(256) + 255
        endcase
        
        ;WHITE->GREEN
        green: begin
            r = linspace(255, 0, 256)
            g = bytarr(256) + 255
            b = linspace(255, 0, 256)
        endcase
        
        ;WHITE->RED
        red: begin
            r = bytarr(256) + 255
            g = linspace(255, 0, 256)
            b = linspace(255, 0, 256)
        endcase
        
        ;RED->WHITE->BLUE
        rwb: begin
            r = [bytarr(128)+255, linspace(255, 0, 128)]
            g = [linspace(0, 255, 128), linspace(255, 0, 128)]
            b = [linspace(0, 255, 128), bytarr(128)+255]
        endcase
    endcase
    
;---------------------------------------------------------------------
;Select Color Range //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Clip the color table
    r = r[clip[0]:clip[1]]
    g = g[clip[0]:clip[1]]
    b = b[clip[0]:clip[1]]
    nClip = clip[1] - clip[0] + 1
    
    ;Select the NCOLORS
    if nColors ne nClip then begin
        iColors = (lindgen(nColors) * nClip) / (nColors-1)
        r = r[iColors]
        g = g[iColors]
        b = b[iColors]
    endif
    
    ;Create the color table and reverse it, if necessary.
    color_table = [[r], [g], [b]]
    if (reverse eq 1) then color_table = reverse(color_table, 1)
    if (row eq 1) then color_table = transpose(color_table)
    
    return, color_table
end