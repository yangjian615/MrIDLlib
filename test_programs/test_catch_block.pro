; docformat = 'rst'
;
; NAME:
;       TEST_CATCH_BLOCK
;
; PURPOSE:
;+
;   The purpose of this program is to test whether or not two catch statements can
;   exist in the same procedure.
;
;   Results::
;       It IS possible to have more than one catch block!
;       
;
; :Examples:
;       IDL> .r test_catch_block
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
;       2014/02/09  -   Written by Matthew Argall
;-
pro test_catch_block
    
    ;First catch block
    catch, error1
    if error1 ne 0 then begin
        ;Cancel catch, reset the error state, fix the error.
        catch, /CANCEL
        Message, /RESET
        Message, 'Error1 was caught!', /INFORMATIONAL
        a = 1
    endif

    ;Generate first error. "a" is undefined.
    c = 5 + a
    
    ;Second catch block
    catch, error2
    if error2 ne 0 then begin
        ;Cancel catch, reset the error state, fix the error.
        catch, /cancel
        Message, /RESET
        Message, 'Error2 was caught!', /INFORMATIONAL
        b = 3
    endif
    
    ;Generate another error. "b" is undefined.
    c += b
end

;Main Level Program
test_catch_block
end