; docformat = 'rst'
;
; NAME:
;       MrIsA
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
;       The purpose of this program is to serve as a wrapper for IDL's "isa" function.
;       In addition to the types checked by "ISA", MrIsA will also check for::
;           - Decimal
;           - Integer
;           - Number
;           - Real
;           - Complex
;           - Double
;           - Column   (array is a N-row, 1-column vector)
;           - Row      (array is a 1-row, N-column vector)
;
;       Furthermore, if `X` is a structure and `TYPE` is given, `TYPE` will be checked
;       against the name of the structure.
;
;   Note:
;       Multiple keywords can be set, but if the keywords conflict, the result will be
;       false. An example of conflicting keywords would be SCALAR and ARRAY. In other
;       words, keyword combinations are taken as AND cases, not OR.
;
; :Categories:
;       Wrapper, Type Utility
;
; :Examples:
;
;       See the example program at the end of this file::
;
;           IDL> .r MrIsA
;
; :Params:
;       X:                      in, required, type=any
;                               Check if this is of type `TYPE`
;       TYPE:                   in, required, type=optional
;                               Determine if `X` is this type. Options are:
;                                   'NUMERIC', 'REAL', 'COMPLEX', 'DOUBLE', 'COLUMN'
;                                   'ROW', or any other type accepted by IDL's ISA function.
;
; :Keywords:
;       DECIMAL:                in, optional, type=boolean, default=0
;                               If set, determine if `X` is a decimal number (non-integer)
;       INTEGER:                in, optional, type=boolean, default=0
;                               If set, determine if `X` is any integer class.
;       NUMBER:                 in, optional, type=boolean, default=0
;                               If set, determine if `X` is a numeric type.
;       REAL:                   in, optional, type=boolean, default=0
;                               If set, determine if `X` is a real numeric type.
;       COMPLEX:                in, optional, type=boolean, default=0
;                               If set, determine if `X` is a complex numeric type.
;       DOUBLE:                 in, optional, type=boolean, default=0
;                               If set, determine if `X` is a double numeric type.
;       COLUMN:                 in, optional, type=boolean, default=0
;                               If set, determine if `X` is a column vector (i.e. 1xN).
;       ROW:                    in, optional, type=boolean, default=0
;                               If set, determine if `X` is a row vector (i.e. Nx0)
;       FILE:                   in, optional, type=boolean, default=0
;                               If set, determine if `X` is a file (with ASSOC).
;       ARRAY:                  in, optional, type=boolean, default=0
;                               If set, return 1 if `X` is an array.
;       SCALAR:                 in, optional, type=boolean, default=0
;                               If set, return 1 if `X` is a scalar.
;
; :Returns:
;       TF_ISA:                 Is 1 if `X` is of type `TYPE` and optionally qualifies as
;                                   one of the set keywords, and 0 otherwise.
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
;       04/13/2013  -   Written by Matthew Argall
;       07/15/2013  -   Changed the functionality to work via keywords. This way, multiple
;                           keywords can be set at the same time.
;       2013/11/14  -   IDL's ISA function was introduced in 8.0. Fixed for earlier versions. - MRA
;       2014/01/14  -   In keeping with the IsA function, returns true if `X` is defined.
;                           Fixed errors with combinations of keywords from MrIsA and IsA. - MRA
;       2014/01/21  -   Conflicting cases return False immediately (e.g. /ROW, /COLUMN) - MRA
;-
function MrIsA, x, type, $
 DECIMAL = decimal, $
 INTEGER = integer, $
 NUMBER  = number, $
 REAL    = real, $
 COMPLEX = complex, $
 DOUBLE  = double, $
 COLUMN  = column, $
 ROW     = row, $
 ;Keywords for IDL's IsA function
 FILE    = file, $
 ARRAY   = array, $
 SCALAR  = scalar
    compile_opt strictarr
    on_error, 2
    
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Get the input type
    x_type = size(x, /TNAME)
    x_size = size(x)
    
    ;Assume false.
    tf_isa = 0
    
    ;Defaults
    if n_elements(type) gt 0 then type = strupcase(type) else type = ''
    decimal = keyword_set(decimal)
    integer = keyword_set(integer)
    number  = keyword_set(number)
    real    = keyword_set(real)
    complex = keyword_set(complex)
    double  = keyword_set(double)
    column  = keyword_set(column)
    row     = keyword_set(row)
    file    = keyword_set(file)
    array   = keyword_set(array)
    scalar  = keyword_set(scalar)
    
    ;Count the number of keywords that are set.
    nKey = decimal + integer + number + real + complex + double + column + $
           row + file + array + scalar
    
    ;Take care of conflicting cases
    if real + complex    gt 1 then return, 0
    if array + scalar    gt 1 then return, 0
    if column + row      gt 1 then return, 0
    if decimal + integer gt 1 then return, 0
    
;-----------------------------------------------------
;Integer \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if integer then begin
        case x_type of
            'BYTE':      tf_isa = 1
            'INT':       tf_isa = 1
            'LONG':      tf_isa = 1
            'UINT':      tf_isa = 1
            'ULONG':     tf_isa = 1
            'LONG64':    tf_isa = 1
            'ULONG64':   tf_isa = 1
            else:        ;do nothing
        endcase
    endif
    
;-----------------------------------------------------
;Decimal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if decimal then begin
        case x_type of
            'FLOAT':     tf_isa = 1
            'DOUBLE':    tf_isa = 1
            'COMPLEX':   tf_isa = 1
            'DCOMPLEX':  tf_isa = 1
            else:        ;do nothing
        endcase
    endif
    
;-----------------------------------------------------
;A Number? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if number then begin
        case x_type of
            'UNDEFINED': ;Do nothing
            'BYTE':      tf_isa = 1
            'INT':       tf_isa = 1
            'LONG':      tf_isa = 1
            'FLOAT':     tf_isa = 1
            'DOUBLE':    tf_isa = 1
            'COMPLEX':   tf_isa = 1
            'STRING':    ;Do nothing
            'STRUCT':    ;Do nothing
            'DCOMPLEX':  tf_isa = 1
            'POINTER':   ;Do nothing
            'OBJREF':    ;Do nothing
            'UINT':      tf_isa = 1
            'ULONG':     tf_isa = 1
            'LONG64':    tf_isa = 1
            'ULONG64':   tf_isa = 1
            else:        ;do nothing
        endcase
    endif
        
;-----------------------------------------------------
;Real \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if real then begin
        case x_type of
            'UNDEFINED': ;Do nothing
            'COMPLEX':   ;Do nothing
            'STRING':    ;Do nothing
            'STRUCT':    ;Do nothing
            'DCOMPLEX':  ;Do nothing
            'POINTER':   ;Do nothing
            'OBJREF':    ;Do nothing
            else: tf_isa = 1    ;any other non-complex numeric type
        endcase
    endif
        
;-----------------------------------------------------
;Complex \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if complex then begin
        case x_type of
             'COMPLEX':  tf_isa = 1
             'DCOMPLEX': tf_isa = 1
            else: ;Not complex
        endcase
    endif
            
;-----------------------------------------------------
;Double \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if double then begin    
        case x_type of
             'DOUBLE':  tf_isa = 1      ;double
             'DCOMPLEX': tf_isa = 1      ;dcomplex
            else: ;Not a double
        endcase
    endif
            
;-----------------------------------------------------
;Column \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if column then if x_size[0] eq 2 && x_size[1] eq 1 then tf_isa = 1
        
;-----------------------------------------------------
;Row \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if row then if x_size[0] eq 1 then tf_isa = 1
        
;-----------------------------------------------------
;Versions Before IDL 8.0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;They did not have an IsA function
    if MrCmpVersion('8.0') ge 0 then begin
        tf_type = 0
        
        case x_type of
            'UNDEFINED': tf_type = 1
            'BYTE':      tf_type = 1
            'INT':       tf_type = 1
            'LONG':      tf_type = 1
            'FLOAT':     tf_type = 1
            'DOUBLE':    tf_type = 1
            'COMPLEX':   tf_type = 1
            'STRING':    tf_type = 1
            'STRUCT':    tf_type = type eq '' ? 1 : (size(x, /SNAME) eq type)
            'DCOMPLEX':  tf_type = 1
            'POINTER':   tf_type = type eq '' ? 1 : ptr_valid(x)
            'OBJREF':    tf_type = type eq '' ? 1 : MrObj_Class(x, type)
            'UINT':      tf_type = 1
            'ULONG':     tf_type = 1
            'LONG64':    tf_type = 1
            'ULONG64':   tf_type = 1
            else:        ;Do nothing
        endcase
        
        ;Have not implemented these yet.
        if file   then message, 'The FILE keyword is not available before IDL 8.0', /INFORMATIONAL
        if array  then if x_size[0] ge 1 then tf_isa = 1
        if scalar then if x_size[0] eq 0 && x_size[x_size[0]+2] eq 1 then tf_isa = 1
        
;-----------------------------------------------------
;Versions after 8.0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        ;Call IDL's IsA function. Do not call IsA with X alone. This will be taken care
        ;of later.
        if type ne '' $
            then tf_type = isa(x, type, FILE=file, ARRAY=array, SCALAR=scalar) $
            else if file + array + scalar gt 0 then tf_isa = isa(x, FILE=file, ARRAY=array, SCALAR=scalar)
    endelse

    ;Combine the results.
    if nKey eq 0 and type eq '' then tf_isa = n_elements(x) gt 0 ? 1 : 0    ;i.e. IsA(X)
    if nKey eq 0 and type ne '' then tf_isa = tf_type
    if nKey gt 0 and type ne '' then tf_isa = tf_type || tf_isa
    
    return, tf_isa
end

;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Create variables of different types
test1 = 5
test2 = 's'
test3 = 1D
test4 = ['a', 'b']

;Determine if they are of a numeric type
tf_test0 = MrIsA(test0)
tf_test1 = MrIsA(test1, /INTEGER)
tf_test2 = MrIsA(test2, /SCALAR)
tf_test3 = MrIsA(test3, /DOUBLE)
tf_test4 = MrIsA(test4, 'STRING', /ARRAY)

;Print the results
print, FORMAT='(%"x           Is defined?        %i")',          tf_test0
print, FORMAT='(%"x = %3i     Is an integer?     %i")',   test1, tf_test1
print, FORMAT='(%"x = %3s     Is a scalar?       %i")',   test2, tf_test2
print, FORMAT='(%"x = %3.1f     Is a double?       %i")', test3, tf_test3
print, FORMAT='(%"x = [%s, %s]  Is a String Array? %i")', test4, tf_test4

end