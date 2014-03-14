; docformat = 'rst'
;
; NAME:
;       MrObj_Class
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;       The purpose of this program is to serve as a wrapper for IDL's "Obj_Class".
;       Additional functionality includes::
;           - Accepts arrays of objects
;
; :Catetories:
;       Wrapper, Type Utility
;
; :Examples:
;       See the example program at the end of this file::
;           IDL> .r MrObj_Class
;
; :Params:
;       ARG:                    in. optional. type=objarr/strarr
;                               Object references or string variables for which the object
;                                   class names are desired. If not present, a string array
;                                   of the currently loaded object classes will be returned.
;
; :Keywords:
;       COUNT:                  out, optional, type=intarr
;                               A named variable that will contain the number of names
;                                   returned for each element in `ARG`.
;       SUPERCLASS:             in, optional, type=boolean, default=0
;                               Set this keyword to cause MrObj_Class to return the names
;                                   of the objects' direct superclasses as a string array,
;                                   one element per superclass. If ARG has more than one
;                                   element, a structure will be returned whos tags are
;                                   'OBJECT#', where # is the index of `ARG`, and whose
;                                   values are the string arrays of superclass names. If
;                                   set, `ARG` must be present.
;
; :Returns:
;       RESULT:                 The names of the classes or superclasses
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
;       10/11/2013  -   Written by Matthew Argall
;-
function MrObj_Class, arg, $
COUNT=count, $
SUPERCLASS=superclass
    compile_opt strictarr
    on_error, 2
    
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    nArgs = n_elements(arg)
    superclass = keyword_set(superclass)
    getCount = arg_present(count)
    
    ;Output a better error message than Obj_Class
    if nArgs eq 0 and (superclass eq 1) then message, 'ARG must be supplied with SUPERCLASS.'
    
;-----------------------------------------------------
;nArg = 0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Call Obj_Class directly
    if nArgs eq 0 then begin
        if getCount $
            then return, Obj_Class(COUNT=count) $
            else return, Obj_Class()
    endif
    
;-----------------------------------------------------
;nArg = 1 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Call Obj_Class directly
    if nArgs eq 1 then begin
        if getCount $
            then return, Obj_Class(arg, COUNT=count, SUPERCLASS=superclass) $
            else return, Obj_Class(arg, SUPERCLASS=superclass)
    endif
    
;-----------------------------------------------------
;nArg > 1 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if (superclass eq 0) then result = strarr(nArgs)
    if (getCount   eq 1) then count = intarr(nArgs)

    ;Step through each argument
    for i = 0, nArgs - 1 do begin
        ;Call Obj_Class
        tempResult = Obj_Class(Arg[i], COUNT=tempCount, SUPERCLASS=superclass)
        if getCount then count[i] = tempCount
    
    ;-----------------------------------------------------
    ;Superclasses? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        if (superclass eq 1) then begin
            ;Create a tag name that is "OBJECT" + index-into-ARG
            iObj = string(FORMAT='(%"OBJECT%i")', i)
            
            ;Create a structure with the tag and the string of superclass names.
            if i eq 0 $
                then result = create_struct(iObj, tempResult) $
                else result = create_struct(result, iObj, tempResult)
    
    ;-----------------------------------------------------
    ;Class names? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        endif else begin
            result[i] = tempResult
        endelse
    endfor
    
    return, result
end



;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Create a couple objects
obj1 = obj_new('IDLanROIGroup')
obj2 = obj_new('IDLanROI')

;Find the names of the classes and superclasses
classnames = MrOBJ_Class([obj1, obj2])
superclass = MrOBJ_Class([obj1, obj2], /SUPERCLASS)

;Print the results
print, ''
print, FORMAT='(%"Objects 1 and 2 are of class [%s, %s]")', classnames
print, ''
print, 'Superclasses are stored in a structure:'
help, superclass

obj_destroy, obj1
obj_destroy, obj2

end