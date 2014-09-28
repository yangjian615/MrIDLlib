; docformat = 'rst'
;
; NAME:
;       TEST_CATCH_BLOCK
;
; PURPOSE:
;+
;   The purpose of this program is to see if hash(data, /EXTRACT) causes an error when
;   compiled.
;       % Keyword parameters not allowed in call.
;
;   Results::
;       No, an error does not occur. I had a typo in the other program.
;       
;
; :Examples:
;       IDL> .r test_hash_compile
;
; :Categories:
;       Test Program
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
;       2014/05/25  -   Written by Matthew Argall
;-
function test_hash_compile, struct
    compile_opt idl2
    on_error, 2
    
    return, hash(struct, /EXTRACT)
end