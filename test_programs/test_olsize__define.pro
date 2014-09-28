; docformat = 'rst'
;
; NAME:
;       TEST_REF_EXTRA
;
; PURPOSE:
;+
;   The purpose of this program is to figure out how to get the _OverloadSize
;   method to return for
;       IDL> size(!Null, /N_DIMENSIONS)
;
;   Result:
;       Turns out, its a bug. Will be fixed in IDL 8.3.1.
;           https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/-f8Cxlp5cxQ
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
;       07/09/2013  -   Written by Rob Klooster
;-
function test_olSize::_OverloadSize
    return, size(*self.value, /DIMENSIONS)
end

function test_olSize::Init
    compile_opt strictarr
    self.value = Ptr_New(/ALLOCATE_HEAP)
    return, 1
end

pro test_olSize__define
    class = {test_olSize, $
             inherits IDL_Object, $
             value: ptr_new()}
end


;Main level test program
myObj = Obj_New('Test_olSize')
print, 'Dimensions:   ', '[' + strjoin(strtrim(size(myObj, /DIMENSIONS), 2), ', ') + ']'
print, 'N_Elements:   ', strtrim(size(myObj, /N_ELEMENTS), 2)
print, 'N_Dimensions: ', strtrim(size(myObj, /N_DIMENSIONS), 2)
obj_destroy, myObj

end