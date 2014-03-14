;+
; NAME:
;       MrNull
;
; PURPOSE:
;
;       The purpose of this program is to return a value that will represent the !Null
;       value of IDL 8.0 for earlier versions. If the IDL version is 8.0 or later, then
;       !Null will be returned.
;
;       To check if future variables are equal to the value returned by MrNull, use the
;       MrIsNull function with the `INPUT` defined here.
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
;       INPUT:              in, optional, type=string/any, default='INT'
;                           For versions of IDL earlier than 8.0, if the input is a string
;                               that matches one of the variable type names, then the
;                               default null value of that data type will be returned.
;                               Otherwise, the given value of INPUT will be returned.
;                           
; :Returns:
;       Null                The !Null value.
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
;       2013/11/13  -   Unsigned types now return the greatest allowable value. - MRA
;-
function MrNull, input
	compile_opt idl2
	on_error, 2
	
	;If the current version is IDL 8.0 or later, then return !Null
	if MrCmpVersion('8.0') le 0 then return, !Null
	
	;If a string was provided, return a Null value of the proper type
	type = size(input, /TNAME)
	if type eq 'STRING' then begin
	    case strupcase(input) of
	        'UNDEFINED': message, 'Cannot declare an undefined Null value.'
	        'BYTE':      null = -1B
	        'INT':       null = fix(-1)
	        'LONG':      null = -1L
	        'FLOAT':     null = -1.0
	        'DOUBLE':    null = -1D
	        'COMPLEX':   null = complex(-1,-1)
	        'STRING':    null = ''
	        'STRUCT':    message, 'Cannot create a Null structure value.'
	        'DCOMPLEX':  null = dcomplex(-1,-1)
	        'POINTER':   null = ptr_new()
	        'OBJREF':    null = obj_new()
	        'UINT':      null = uint(-1)
	        'ULONG':     null = ulong(-1)
	        'LONG64':    null = long64(-1)
	        'ULONG64':   null = ulong64(-1)
	        else: null = input
	    endcase
	    
	;Otherwise, return the given value
	endif else null = input
	
	return, null
end