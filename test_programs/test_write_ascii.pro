; docformat = 'rst'
;
; NAME:
;       TEST_CATCH_BLOCK
;
; PURPOSE:
;+
;   The purpose of this program is to see how the output from Read_Ascii and Ascii_Template
;   can be used to write to a file.
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
pro test_write_ascii
    compile_opt strictarr
    
    ;Information available from Ascii_Template and Read_Ascii
    str_fmt = ['i', 'a']
    nLines = 4
    nFields = 2

    ;Structure returned by Read_Ascii()
    struct = {field0: [0,1,2,3], $
              field1: ['a', 'b', 'c', 'd']}

    ;Open a file
    openw, lun, '/Users/argall/Desktop/test.txt', /get_lun

    ;Loop through each data point
    for i = 0, nLines - 1 do begin
        
        ;Concatenate the separate field values into a string
        str_field = ''
        for j = 0, nFields - 1 do str_field += String( struct.(j)[i], FORMAT='(' + str_fmt[j] + ')' ) + ','
    
        ;Print the string to the file
        printf, lun, str_field
    endfor
    
    free_lun, lun
end

;Main Level Program
test_write_ascii
end