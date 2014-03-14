; docformat = 'rst'
;
; NAME:
;       PWD
;
; PURPOSE:
;+
;       Get the present working directory.
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
; :Returns:
;
;	    PWD:            The present working directory terminating in a path separator
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
;       Written by  -   Matthew Argall 19 November 2012
;       2014/02/23  -   Changed from a function to a procedure.
;-
pro pwd
    compile_opt idl2
    
    ;get the current directory and add a path separator to it.
    cd, current=pwd
    pwd += path_sep()
    
    print, pwd
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
print, 'The present working directory is:'
pwd
end