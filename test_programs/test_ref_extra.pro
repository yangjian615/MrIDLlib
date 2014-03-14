; docformat = 'rst'
;
; NAME:
;       TEST_REF_EXTRA
;
; PURPOSE:
;+
;       The purpose of this program is to test different features of the _REF_EXTRA
;       keyword. Namely,
;           1) Can it pass keyword strings, as in the documentation
;           2) Do the keyword strings have to be defined in the call to TEST_REF_EXTRA?
;           3) If I define an array of strings, does that limit the number of keywords
;               that are inherited?
;           4) Are the keyword strings still found in the "extra" array after their
;               first use?
;
;       Test data::
;           x = findgen(100)
;           y = sin(x)
;
;       Results::
;           1) Yes, it can. No error is thrown.
;               test_ref_extra, x, y
;           2) No, they do not. XTITLE and XSTYLE are applied, but no error is thrown
;               for YSTYLE and YRANGE.
;                   test_ref_extra, x, y, XRANGE=[20,80], XSTYLE=1
;           3) Yes, it does. XTITLE does not work.
;                   test_ref_extra, x, y, XRANGE=[20,80], XSTYLE=1, XTITLE='X Title'
;           4) Yes, it is. The following prints 1.
;                   test_ref_extra, x, y, XRANGE=[20,80], XSTYLE=1, XTITLE='X Title'
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
;       03/05/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
pro test_ref_extra, x, y, $
_REF_EXTRA = extra
    compile_opt idl2

    ;Test to see if we can pass the strings, even if they are not found in _REF_EXTRA
    plot, x, y, _EXTRA=['xrange', 'yrange', 'xstyle', 'ystyle']

    ;Check to see if the string is still there.
    print, max(extra eq strupcase('xrange'))
    
end