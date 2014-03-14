; docformat = 'rst'
;
; NAME:
;       TEST_REF_EXTRA
;
; PURPOSE:
;+
;       The purpose of this program is to test the various types of keyword inheritance
;       mechanisms. I have a set of classes that follow this exact same structre, but is
;       not working.
;
;       Result::
;           Keywords present in the keyword list of the subroutine that are also present
;           in the inheritance structure/array (heretofore referred to as "extra") get
;           populated by their "extra" value and removed from "extra". Keywords that are
;           present in "extra", but not in the keyword list are removed from "extra" and
;           not present within the subroutine. An exception occurrs if _EXTRA or
;           _REF_EXTRA are present in the keyword list of the ubroutine. Then all unused
;           keywords remain in "extra" and can be used later.
;
;           _STRICT_EXTRA is foiled when _EXTRA or _REF_EXTRA appear in the keyword list.
;               
;
; :Categories:
;
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
;       07/05/2013  -   Written by Matthew Argall
;-
function test_keyword_2, $
KEY4 = key4, $
KEY5 = key5, $
KEY6 = key6
    compile_opt strictarr
    on_error, 3

    print, ''
    print, 'Test_Keyword_2'    
    if size(extra, /TNAME) eq 'STRING' $
        then print, extra $
        else help, extra
    
    return, 1
    
end


function test_keyword_1, $
KEY1 = key1, $
KEY2 = key2, $
KEY3 = key3, $
_EXTRA = extra
    compile_opt strictarr
    on_error, 3

    print, ''
    print, 'Test_Keyword_1'    
    if size(extra, /TNAME) eq 'STRING' $
        then print, extra $
        else help, extra
        
    result = test_keyword_2(_EXTRA=extra)

    return, 1
end

function test_keyword_inheritance, $
_EXTRA = extra
    compile_opt strictarr
    on_error, 3
    
    result = test_keyword_1(_STRICT_EXTRA=extra)

    print, ''
    print, 'Test_Keyword_Inheritance'
    if size(extra, /TNAME) eq 'STRING' $
        then print, extra $
        else help, extra

    result = test_keyword_2(_EXTRA=extra)

    print, ''
    print, 'Test_Keyword_Inheritance'
    if size(extra, /TNAME) eq 'STRING' $
        then print, extra $
        else help, extra

    return, result

end