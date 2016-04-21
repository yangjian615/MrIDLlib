; docformat = 'rst'
;
; NAME:
;   Test_Destroy__Define
;
; PURPOSE
;+
;   Test whether a procedure or function method can destroy self, but still return
;   data to the user.
;
;   Results:
;      Both FunGet and ProGet are able to return data and destroy the object.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/02/03  -   Written by Matthew Argall.
;-
;*****************************************************************************************
;+
;   Function to retrieve data.
;
; :Params:
;       DATA:               out, optional, type=any
;                           Array to be stored internally.
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, the array will be removed frome the object and return.
;                               The object will also be destroyed.
;-
function Test_Destroy::FunGet, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Delete?
	if keyword_set(no_copy) then begin
		;Get the data
		data = temporary(*self.data)
		
		;Destroy the object
		obj_destroy, self
	endif else begin
		data = *self.data
	endelse
	
	;Return the data
	return, data
end


;+
;   Procedure to retrieve data.
;
; :Params:
;       DATA:               out, optional, type=any
;                           Array to be stored internally.
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, the array will be removed frome the object and return.
;                               The object will also be destroyed.
;-
pro Test_Destroy::ProGet, data, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Delete?
	if keyword_set(no_copy) then begin
		;Get the data
		data = temporary(*self.data)
		
		;Destroy the object
		obj_destroy, self
	endif else begin
		data = *self.data
	endelse
end


;+
;   Procedure to set data.
;
; :Params:
;       DATA:               out, optional, type=any
;                           Array to be stored internally.
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, the array will be removed frome the object and return.
;                               The object will also be destroyed.
;-
pro Test_Destroy::SetData, data, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Delete?
	if keyword_set(no_copy) $
		then *self.data = temporary(data) $
		else *self.data = data
end


;+
;   Clean up after the object is destroyed
;-
pro Test_Destroy::cleanup
	compile_opt strictarr
	on_error, 2

	;Free pointers
	ptr_free, self.data
end


;+
;   The initialization method.
;
; :Params:
;       DATA:   in, optional, type=any
;               Array of data
;-
function Test_Destroy::init, data
NO_COPY=no_copy
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		message, !error_state.msg, /INFORMATIONAL
		return, 0
	endif
	
	;Allocate memory
	self.data = ptr_new(/ALLOCATE_HEAP)

	;X must be given
	if n_elements(data) eq 0 then $
		message, 'DATA must be defined.'
		
	;Save the object property
	self -> SetData, data, NO_COPY=no_copy

	return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;
; :Fields:
;       DATA:       Data to be accessed via bracket overloading.
;-
pro Test_Destroy__define
    compile_opt strictarr
    
    class = { Test_Destroy, $
              data: Ptr_New() $
            }
end