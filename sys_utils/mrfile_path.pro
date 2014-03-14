; docformat = 'rst'
;
; NAME:
;
;       mrFile_Path
;
; PURPOSE:
;+
;       The purpose of this program is to return the path to a given file.
;       The file path is taken to be the first character of `FILENAME` up to the last
;       file path separator [path_sep()] in the file name. If no path separators are
;       found, then the empty string is returned.
;
; :Categories:
;
;       System Utility
;
; :Examples:
;   See the main level program at the end of this file::
;
;       IDL> .r file_path
;
; :Params:
;       FILENAME:           in, required, type=string
;                           The filename for which the extension is to be returned.
;
; :Returns:
;       PATH:               The path of the file name.
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
;       02/26/2013  -   Written by Matthew Argall
;-
function mrFile_Path, filename
    compile_opt idl2
    on_error, 2

    ;Check that a filename was provided.
    if n_elements(filename) eq 0 then message, 'A filename must be given.'

    ;Check the file extension
    path_pos = strpos(filename, path_sep(), /REVERSE_SEARCH)
    
    ;If no path separator is found, return the empty string
    if path_pos eq -1 $
        then path = '' $
        else path = strmid(filename, 0, path_pos)
    
    return, path
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
filename = '/users/username/blah/file.something.1=0?\|{}blah.txt'
file_path = mrFile_Path(filename)
print, format='(%"The file name is: %s")', filename
print, format='(%"Its extension is: %s")', file_path
end