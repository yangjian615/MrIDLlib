; docformat = 'rst'
;
; NAME:
;       TEST_WIDGET_TREE
;
; PURPOSE:
;+
;       The purpose of this program is to try to recreate a segmentation fault. This
;       happens when I am inside a program and decide to compile it without first
;       returning.
;
;       Result::
;           <This `https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/dVkKvKWjkkM`>
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
;*****************************************************************************************
pro compile_no_return_crash
    index = intarr(200)
    
    stop
end