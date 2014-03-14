; docformat = 'rst'
;
; NAME:
;
;       FILE_EXTENSION
;
; PURPOSE:
;+
;       The purpose of this program is to return the file extension of a filename.
;       The file extension is expected to be the last set of characters in the file
;       name and preceeded by a dot, '.'.
;
; :Categories:
;
;       System Utility
;
; :Examples:
;   See the main level program at the end of this file::
;
;       IDL> .r file_extension
;
; :Params:
;
;       FILENAME:           in, required, type=string
;                           The filename for which the extension is to be returned.
;
; :Returns:
;       EXTENSION:          The file extension of the file name. If none exists, the empty
;                               string is returned.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History::
;   Modification History::
;       02/22/2013  -   Written by Matthew Argall
;       06/24/2013  -   If no extension is found, the empty string is returned. - MRA
;-
function file_extension, filename
    compile_opt idl2
    on_error, 2

    ;Check that a filename was provided.
    if n_elements(filename) eq 0 then message, 'A filename must be given.'

    ;Check the file extension
    extension_pos = strpos(filename, '.', /REVERSE_SEARCH)
    if extension_pos eq -1 then return, ''
    
    ;Extract the file extension
    extension_len = strlen(filename) - extension_pos - 1
    extension = strmid(filename, extension_pos+1, extension_len)
    
    return, extension
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
filename = '/users/username/blah/file.something.1=0?\|{}blah.txt'
extension = file_extension(filename)
print, format='(%"The file name is: %s")', filename
print, format='(%"Its extension is: %s")', extension
end