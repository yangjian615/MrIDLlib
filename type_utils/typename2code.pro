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
;-
function TypeName2Code, type
    compile_opt strictarr
    on_error, 2
    
    ;Type-Name?
    case strupcase(type) of
        'UNDEFINED': typeCode = 0
        'BYTE':      typeCode = 1
        'INT':       typeCode = 2
        'LONG':      typeCode = 3
        'FLOAT':     typeCode = 4
        'DOUBLE':    typeCode = 5
        'COMPLEX':   typeCode = 6
        'STRING':    typeCode = 7
        'STRUCT':    typeCode = 8
        'DCOMPLEX':  typeCode = 9
        'POINTER':   typeCode = 10
        'OBJREF':    typeCode = 11
        'UINT':      typeCode = 12
        'ULONG':     typeCode = 13
        'LONG64':    typeCode = 14
        'ULONG64':   typeCode = 15
        else: message, 'Type name does not exist: "' + type + '".'
    endcase
    
    return, typeCode
end
