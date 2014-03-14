; docformat = 'rst'
;
; NAME:
;       MraOPlot
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
;+
; PURPOSE:
;
;   The purpose of this program is to serve as a wrapper for the cgOPlot command.
;   Additions to cgPlot include:
;       - Plot multiple columns or rows of data on the same axes with a single call.
;           o Each column/row of Y is plotted against X (which is 1D)
;       - Each line can have its own
;           o Color
;           o Linestyle
;           o Symbol
;           o Symbol Color
;
; :Categories:
;    Graphics
;    
; :Params:
;       X:              in, required, type=any
;                       If X is provided without Y, a vector representing the dependent values to be 
;                           plotted If both X and Y are provided, X is the independent parameter and 
;                           Y is the dependent parameter to be plotted.
;       Y:              in, optional, type=any
;                       A vector representing the dependent values to be plotted.
;
; :Keywords:
;       COLOR:          in, optional, type=string/integer, default='black'
;                       If this keyword is a string, the name of the data color. By default, 'black'.
;                           Color names are those used with cgColor. Otherwise, the keyword is assumed 
;                           to be a color index into the current color table.
;       DIMENSION:      in, optional, type=boolean, default=0
;                       The dimension of data over which to plot. This applies only if the dependent
;                           variable is 2D. Say a time series vector V is [3,N]. To plot V[0,*], V[1,*], and
;                           V[2,*] independently within the same set of axes, set dimension=2.
;       LINESTYLE:      out, optional, type=integer/intarr, default=0
;                       The line style for drawing each line.
;       PSYM:           in, optional, type=integer, default=0
;                       Any normal IDL PSYM values, plus any value supported by the Coyote Library
;                           routine cgSYMCAT. An integer between 0 and 46.
;       SYMCOLOR:       in, optional, type=string/integer, default='black'
;                       If this keyword is a string, the name of the symbol color. By default, 'black'.
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       SYMSIZE:        in, optional, type=float/fltarr, default=1.0
;                       The size of each symbol determined by `PSYM`.
;       THICK:          in, optional, type=float/fltarr, default=1.0
;                       The thickness of the line being plotted. 
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword appropriate for the cgOPlot command is allowed in the program.
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
;       05/04/2013  -   Written by Matthew Argall
;       09/06/2013  -   Added the DIMENSION and THICK keyword. - MRA
;       2014/01/22  -   Keywords are now cyclic. Now calls cgPlot directly with the overplot
;                           keyword set (instead of cgOPlot). This meant adding the CHARSIZE
;                           keyword. - MRA
;       2014/01/24  -   Renamed from weOPlot to MraOPlot. - MRA
;-
PRO MraOPlot, x, y, $
CHARSIZE=charsize, $
COLOR = color, $
DIMENSION = dimension, $
LINESTYLE = linestyle, $
PSYM = psym, $
SYMCOLOR = symcolor, $
SYMSIZE = symsize, $
THICK = thick, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2

    dims = Size(x, /DIMENSIONS)
    setDefaultValue, dimension, 0

;---------------------------------------------------------------------
;Dependent and Independent Variables /////////////////////////////////
;---------------------------------------------------------------------
    ;Figure out the dependent variable
    IF N_PARAMS() EQ 1 $
        THEN dep = x $
        ELSE dep = y

    ;Make the independent variable index the chosen DIMENSION of Y.
    IF N_Params() EQ 1 THEN BEGIN
        IF dimension EQ 0 $
            THEN indep = LindGen(N_Elements(dep)) $
            ELSE indep = LindGen(dims[dimension-1])
    
    ;The independent variable was given.
    ENDIF ELSE BEGIN
        dims = Size(y, /DIMENSIONS)
        indep = x
    ENDELSE
    
    ;Make sure arrays were given, not scalars
    IF N_Elements(indep) EQ 1 THEN indep = [indep]
    IF N_Elements(dep) EQ 1 THEN dep = [dep]

;---------------------------------------------------------------------
;Set Defaults ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;The dimension not being plotted.
    case dimension of
        0: xdim = 0
        1: xdim = 2
        2: xdim = 1
    endcase
        
    ;Number of defaults to use.
    if xdim eq 0 then nDefaults = 1 else nDefaults = dims[xdim-1]

    ;Pick a set of default colors so not everything is the same color.
    default_colors = ['opposite', 'Blue', 'Forest_Green', 'Red', 'Magenta', 'Orange']
    if nDefaults eq 1 then d_color = default_colors[0] else d_color = default_colors[1:nDefaults]
    
    ;Set Defaults
    setDefaultValue, color, d_color
    setDefaultValue, linestyle, 0
    setDefaultValue, psym, 0
    setDefaultValue, symcolor, color
    setDefaultValue, symsize, 1.0
    setDefaultValue, thick, 1.0

;---------------------------------------------------------------------
;Normal Plotting /////////////////////////////////////////////////////
;---------------------------------------------------------------------

    IF dimension EQ 0 THEN BEGIN
        cgPlot, indep, dep, $
                CHARSIZE      = charsize, $
                COLOR         = color, $
                LINESTYLE     = linestyle, $
                OVERPLOT      = 1B, $
                PSYM          = psym, $
                SYMCOLOR      = symcolor, $
                SYMSIZE       = symsize, $
                THICK         = thick, $
                _STRICT_EXTRA = extra
        RETURN
    ENDIF

;---------------------------------------------------------------------
;Multiple Overplots //////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Get number of elements to make cyclic
    nColor = n_elements(color)
    nLineStyle = n_elements(linestyle)
    nPSym = n_elements(psym)
    nSymColor = n_elements(symcolor)
    nSymSize = n_elements(symsize)
    nThick = n_elements(thick)

    ;Plot each vector of data.
    FOR j = 0, dims[xdim-1]-1 DO BEGIN
        CASE xdim OF
            1: cgPlot, indep, dep[j,*], $
                       CHARSIZE      = charsize, $
                       COLOR         = color[j mod nColor], $
                       LINESTYLE     = linestyle[j mod nLineStyle], $
                       OVERPLOT      = 1B, $
                       PSYM          = psym[j mod nPSym], $
                       SYMCOLOR      = symcolor[j mod nSymColor], $
                       SYMSIZE       = symsize[j mod nSymSize], $
                       THICK         = thick[j mod nThick], $
                       _STRICT_EXTRA = extra
                        
            2: cgPlot, indep, dep[*,j], $
                       CHARSIZE      = charsize, $
                       COLOR         = color[j mod nColor], $
                       LINESTYLE     = linestyle[j mod nLineStyle], $
                       OVERPLOT      = 1B, $
                       PSYM          = psym[j mod nPSym], $
                       SYMCOLOR      = symcolor[j mod nSymColor], $
                       SYMSIZE       = symsize[j mod nSymSize], $
                       THICK         = thick[j mod nThick], $
                       _STRICT_EXTRA = extra
        ENDCASE
    ENDFOR
END
    
