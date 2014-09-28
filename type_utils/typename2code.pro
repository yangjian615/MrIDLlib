; docformat = 'rst'
;
; NAME:
;   MrArray__Define
;
; PURPOSE
;+
;   Convert a type-name to a type-code. See the `Size http://exelisvis.com/docs/SIZE.html`
;   function
;
; :Keywords:
;       TYPENAME:           in, required, type=string
;                           The type-name to be converted to a type-code
;
; :Returns:
;       TYPECODE:           The type-code belonging to `TYPENAME`
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
;	Modification History::
;       2014/03/03  -   Written by Matthew Argall.
;       2015/04/19  -   Vector of type names can be given.
;-
function TypeName2Code, type
    compile_opt strictarr
    on_error, 2
    
    ;Make sure a string array was given.
    if size(type, /TNAME) ne 'STRING' then $
        message, 'TYPE must be a STRING'
        
    ;Uppercase
    _type = strupcase(type)
    
    ;Names of all IDL datatypes
    type_names = ['UNDEFINED', 'BYTE', 'INT', 'LONG', 'FLOAT', 'DOUBLE', 'COMPLEX', $
                  'STRING', 'STRUCT', 'DCOMPLEX', 'POINTER', 'OBJREF', 'UINT', 'ULONG', $
                  'LONG64', 'ULONG64']
    
    ;Sort them alphabetically. Locate matches
    iSort     = sort(type_names)
    iMatch    = value_locate(type_names[iSort], _type)
    type_code = iSort[iMatch]

    ;Make sure no rounding occured
    tf_pass = min(_type eq type_names[type_code])
    if tf_pass eq 0 then message, 'Invalid type name given.'
    
    return, type_code
end
