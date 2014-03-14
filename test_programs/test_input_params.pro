; docformat = 'rst'
;
; NAME:
;       TEST_INPUT_PARAMS
;
; PURPOSE:
;+
;       The purpose of this program is to test why CalDat returns wrong answers when
;       supplied "filler" variables as inputs. This is in response to a question posed
;       on the Google Groups page
;       <https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/QuluTk7IfZ8>`.
;
;   Results::
;       If the output positional prameters are passed in by value.
;
; :Examples:
;       IDL> .r test_input_params
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

function test_input_params, a, b
    a = 5
    b = 4
    return, a + b   
end

print, test_input_params(null, null)
end