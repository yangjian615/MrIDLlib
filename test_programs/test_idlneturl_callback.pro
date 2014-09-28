; docformat = 'rst'
;
; NAME:
;       Test_IDLnetURL_Callback
;
; PURPOSE:
;+
;   If an error occurs in the IDLnetURL callback function, CCurl subsequently cannot
;   initialize properly, sending the following errors:
;
;       % IDLNETURL::GET:  CCurlException:  Error: Http Get Request Failed. Error = easy handled already used in multi handle, Curl Error Code = 2 
;
;       % IDLNETURL::GET:  CCurlException:  Error: Http Get Request Failed. Error = , Curl Error Code = 2.. 
;
;   This program tries to reproduce the error in a simple manner.
;
;   Results::
;
; :Examples:
;       IDL> .r Test_IDLnetURL_Callback
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
;       07/09/2013  -   Written by Rob Klooster
;-
function Test_IDLnetURL_Callback, status, progressInfo, callbackData
    compile_opt strictarr
    on_error, 2
    
    ;Throw an error by trying to access a field of a non-structure variable
    cancel = callbackData.progressbar -> CheckCancel()
    
    return, ~cancel
end



;Create the IDLnetURL object
callbackData = {percent: 0D, progressbar: obj_new(), cancel: 0B}
myNet = obj_new('IDLnetURL')
for i = 0, 500 do str = myNet -> Get(URL='https://www.google.com/', /STRING_ARRAY)

;Set the callback function
myNet -> SetProperty, CALLBACK_FUNCTION='Test_IDLnetURL_Callback', CALLBACK_DATA=callbackData

;Try to download something, but cause an error in the callback function
str = myNet -> Get(URL='https://www.google.com/', /STRING_ARRAY)

;Wait
wait, 1

;Try to fix things
myNet -> SetProperty, CALLBACK_FUNCTION='', CALLBACK_DATA=!Null
myNet -> CloseConnections

;Try to download again
str = myNet -> Get(URL='https://www.google.com/', /STRING_ARRAY)
obj_destroy, myNet

end