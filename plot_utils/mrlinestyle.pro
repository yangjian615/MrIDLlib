; docformat = 'rst'
;
; NAME:
;   MrLinstyle
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
;   Given a linestyle name or number, determine the number accepted by the LINESTYLE
;   direct graphics keyword. Note that a linestyle of 6 is not accepted by the keyword.
;
; :Categories:
;    Graphics
;
; :Params:
;       LINESTYLE:          in, required, type=string/integer
;                           The name or number of the linestyle to use. Possible values are::
;                               0   '-'     Solid
;                               1   '.'     Dotted
;                               2   '--'    Dashed
;                               3   '-.'    Dash-dot
;                               4   '-:'    Dash-dot-dot
;                               5   '__'    Long dash
;                               6   ' '     None
;
; :Keywords:
;       NAMES:              in, optional, type=boolean, default=0
;                           If set, the names of the linestyles will be returned.
;
; :Returns:
;       STYLE:              Number representing the style of line to be used. Note that
;                               6 (" ") is included to work with function graphics. This
;                               number is not accepted by the direct graphics LINESTYLE
;                               keyword and you must check for it.
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
;     Change History::
;       2014/09/19  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the ColorFill object to the display window.
;-
function MrLinestyle, linestyle, $
NAMES=names
    compile_opt strictarr
    on_error, 2
    
    ;Return the names?
    if keyword_set(names) then begin
        names = ['-', $
                 '.', $
                 '--', $
                 '-.', $
                 '-:', $
                 '--', $
                 ' ']
        return, names
    endif
    
    ;Was a symbol used for the linestyle?
    if size(linestyle, /TNAME) eq 'STRING' then begin
        case linestyle of
            '-':  style = 0
            '.':  style = 1
            '--': style = 2
            '-.': style = 3
            '-:': style = 4
            '__': style = 5
            ' ':  style = 6
            else: message, 'Symbol name "' + linestyle + '" not recognized.'
        endcase
    endif else style = linestyle
    
    ;Valid linestyle?
    if style lt 0 || style gt 6 then message, 'LINESTYLE is out of range.'
    
    return, style
end