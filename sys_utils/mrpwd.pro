; docformat = 'rst'
;
; NAME:
;       MrPWD
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
;   Print the present working directory, or return it into a named variable.
;
; :Examples:
;   See the main level program at the end of this file::
;
;       IDL> .r pwd
;
; :Categories:
;
;       System Utility
;
; :Params:
;	    DIRECTORY:          out, optional, type=string
;                           A named variable into which the present working directory will
;                               be returned.
;
; :Keywords:
;       PATH_SEP:           in, optional, type=boolean, default=0
;                           If set, the file-system path separator will be appended to
;                               the end of the directory.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History::
;   Modification History::
;       09/26/2013  -   Written by Matthew Argall
;-
pro MrPWD, directory, $
PATH_SEP=path_sep
    compile_opt idl2
    
    ;get the current directory.
    cd, current=pwd
    if keyword_set(path_sep) then pwd += path_sep()
    
    ;Print the current directory?
    if n_params() eq 0 $
        then print, pwd $
        else directory = pwd
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
print, 'The present working directory is:'
MrPWD

print, ''
print, 'The present working directory, with a path separator, is:'
MrPWD, directory, /PATH_SEP
print, directory
end