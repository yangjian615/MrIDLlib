; docformat = 'rst'
;
; NAME:
;       MrGetWindow
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
;   The purpose of this program is to create a new window and return its window ID.
;
; :Categories:
;       Graphics Utility, Window
;
; :Examples:
;   See mail level program at end of file::
;
;       IDL> .r MrGetWindow
;
; :Keywords:
;       _REF_EXTRA:             in, optional, type=structure
;                               Any keyword accepted by the WINDOW procedure
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       04/23/2013  -   Created by Matthew Argall, adapted from FREE_WINDOW.PRO
;       2013/12/21  -   IDL now provides backing store for UNIX family OS. - MRA
;-
function MrGetWindow, $
RETAIN = retain, $
_REF_EXTRA=extra
    compile_opt idl2

    ;Allow IDL to provide the backing store for UNIX machines. This includes Linux and
    ;Mac OS X. Mac OS X, Mavericks, requires this.
    if n_elements(retain) eq 0 then begin
        if !version.os_family eq 'unix' then retain = 2
    endif

    ;Create a new window. Its index will be stored in the !D system variable
    window, RETAIN=retain, _STRICT_EXTRA=extra
    win_index = !d.window
    
    ;return the index of the window
    return, win_index 
end

;
;EXAMPLE PROGRAM
;
;   Uses free_window to open two graphics windows and get their IDs. Then, the IDs are
;   used to set the current window and plot a different graph in each of them. 
;

;create two arrays and generate two windows
x = findgen(100)
y = x
win_id1 = MrGetWindow()
win_id2 = MrGetWindow(/FREE, XSIZE=200, YSIZE=500)

;plot to the first window
wset, win_id1
plot, x, y

;plot to the second window
wset, win_id2
plot, x, reverse(y)

;print the IDs of the windows returned by free_window
print, format='(a0, i0)', 'The index of the first window is: ', win_id1
print, format='(a0, i0)', 'The index of the first window is: ', win_id2

end