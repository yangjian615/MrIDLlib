;+
; NAME:
;       MrIsNull
;
; PURPOSE:
;
;       The purpose of this program is to test if the given input is !NULL. For versions
;       of IDL earlier than 8.0, test the given input against a given value that should
;       represent !NULL.
;
;       This is to be used with the MrNull function.
;
; :Examples:
;   Create a version-independent null value, then test for that null value::
;       myNull  = MrNull(-1)
;       tf_Null = MrIsNull(myNull, -1)
;
; :Categories:
;       Wrapper
;
; :Params:
;       INPUT:              in, required, type=any, default='INT'
;                           Test if this is equal to !NULL (IDL v8.0 or later) or `VALUE`
;                               (earlier than IDL v8.0).
;       NULL:               in, required, type=any
;                           Test if `INPUT` is equal to NULL. For versions of IDL later
;                               than v8.0, this parameter is ignored and the !Null system
;                               variable is used as the test.
;                           
;                           
; :Returns:
;       TF_NULL:            Returns true (1) if `INPUT` is equal to `NULL`, false (0)
;                               otherwise.
;
; :Uses:
;   Uses the following external programs::
;       MrCmpVersion
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
;       2013/11/11  -   Written by Matthew Argall
;-
function MrIsNull, input, null
	compile_opt idl2
	on_error, 2
	
	;If the current version is IDL 8.0 or later, compare with !Null
	if MrCmpVersion('8.0') le 0 then begin
	    if input eq !Null then tf_null = 1 else tf_null = 0
	
	;Otherwise, compare INPUT to NULL
	endif else begin
	    nin = n_elements(input)
	    nnul = n_elements(null)
	    
	    ;If nothing was provided, then return true
	    if nin eq 0 then begin
	        tf_null = 1
	        
	    ;if INPUT and NULL have the same number of elements, then they can be compared
	    endif else if nin eq nnul then begin
	        if nin gt 1 $
	            then tf_null = array_equal(input, null) $
	            else tf_null = input eq null
	    
	    ;if INPUT and NULL have disparate number of elements, then INPUT is not NULL.
	    endif else tf_null = 0
	endelse
	
	return, tf_null
end