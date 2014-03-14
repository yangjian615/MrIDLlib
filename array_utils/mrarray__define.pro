; docformat = 'rst'
;
; NAME:
;   MrArray__Define
;
; PURPOSE
;+
;   The purpose of this class is to an object that hosts an array and behaves exactly
;   like an array would (a test of IDL_Object and operator overloading).
;
;   Notes:
;       * The first use of an operator (such as AND, EQ, ^ *, etc.) calls the overload
;           method for the object on the left side first. So, for two MrArray objects,
;           A and B::
;
;               print, A [operator] B
;
;           will call A's _Overload first.
;
;       * If two arrays are compared, results will be truncated to have the same number
;           of elements as the shorter array.
;
;       * If an array and a scalar are compared, each elements of the array is compared
;           against the scalar value.
;
; :Uses:
;   Uses the following external programs::
;       MrReformIndices.pro
;       MrConcatenate.pro
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
;       2014/03/01  -   Written by Matthew Argall.
;       2014/03/03  -   Added the Append and Extend methods. - MRA
;       2014/03/04  -   Added the Min and Max methods. - MRA
;-
;*****************************************************************************************
;+
;   Helper method to determine if "self" was given on the left or right side of an
;   operator.
;
; :Private:
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       SIDE:               Returns 'RIGHT' if "self" was provided on the right side of
;                               the operator and 'LEFT' if it was provided on the left.
;-
function MrArray::_LeftOrRight, left, right
    compile_opt strictarr
    on_error, 2
    
    ;Information about inputs
    lType = size(left, /TNAME)
    rType = size(right, /TNAME)
    heapID = obj_valid(self, /GET_HEAP_IDENTIFIER)

    ;Was "self" given on the left of the AND operator?
    theSelf = ''
    if lType eq 'OBJREF' then begin
        lHeapID = obj_valid(left, /GET_HEAP_IDENTIFIER)
        if lHeapID eq heapID then side = 'LEFT'
    endif
    
    ;Was "self" given on the right of the AND operator? Treat the special case
    ;where "self" is given on both sides.
    if rType eq 'OBJREF' then begin
        rHeapID = obj_valid(right, /GET_HEAP_IDENTIFIER)
        if rHeapID eq heapID then if theSelf ne 'LEFT' then side = 'RIGHT'
    endif

    return, side
end


;+
;   The purpose of this method is to perform a bit-wise comparison between two
;   integer, longword or byte expressions. For other types, `RIGHT` is returned unless
;   `LEFT` is zero or the empty string, in which case 0 (zero) is returned.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             Returns 1 if there is a current element to retrieve, and 0 if
;                               there are no more elements.
;-
function MrArray::_OverloadAnd, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;If {right | left} is also an object with _OverloadAND, get the results recursively.
    ;   The first use of AND calls the _OverloadAND method for the object on the left
    ;   side of AND first. So, for two MrArray objects, A and B,
    ;
    ;       print, A and B
    ;
    ;   will call A -> _OverloadAND first.
    case side of
        'LEFT':  result = *self.array and right
        'RIGHT': result = left and *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to multiply two expressions together.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrArray::_OverloadAsterisk, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Multiply
    case side of
        'LEFT':  result = *self.array * right
        'RIGHT': result = left * *self.array
    endcase
    
    return, result
end


;+
;   Allow square-bracket array indexing from the left side of an operator.
;
; :Params:
;       OBJREF:             in, required, type=ObjRef
;                           The object reference variable that is being indexed (i.e. "self")
;                               Use when you want to replace "self" with `VALUE`.
;       VALUE:              in, required, type=numeric array
;                           The value specified on the right-hand side of the equal sign.
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT1:         in, required, type=integer/intarr(3)
;                           Index subscripts. Either a scalar, an index array, or a 
;                               subscript range in the form [start, stop, step_size]
;       SUBSCRIPT2:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT3:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT4:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT5:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT6:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT7:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT8:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;-
pro MrArray::_OverloadBracketsLeftSide, objRef, value, isRange, subscript1, subscript2, $
                                        subscript3, subscript4, subscript5, $
                                        subscript6, subscript7, subscript8
    compile_opt strictarr
    on_error, 2
    
    ;Reform VALUE into a 1D array
    nValues = n_elements(value)
    valueOut = reform(value, nValues)
    
    ;Get the implicit array dimensions and reform the subscripts into a 1D array
    ;of indices.
    self -> GetProperty, DIMENSIONS=dims
    indices = MrReformIndices(dims, isRange, subscript1, subscript2, subscript3, $
                                             subscript4, subscript5, subscript6, $
                                             subscript7, subscript8)
    
    ;Set the values of the implicit array
    (*self.array)[indices] = valueOut
end


;+
;   Allow square-bracket array indexing from the right side of an operator.
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT1:         in, required, type=integer/intarr(3)
;                           Index subscripts. Either a scalar, an index array, or a 
;                               subscript range in the form [start, stop, step_size]
;       SUBSCRIPT2:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT3:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT4:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT5:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT6:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT7:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT8:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;
; :Returns:
;       RESULT:             in, required, type=numeric array
;                           The subarray accessed by the input parameters.
;-
function MrArray::_OverloadBracketsRightSide, isRange, subscript1, subscript2, $
                                              subscript3, subscript4, subscript5, $
                                              subscript6, subscript7, subscript8
    compile_opt strictarr
    on_error, 2

    ;Make sure an array has been provided.
    if n_elements(*self.array) eq 0 then return, !Null

    ;How many subscripts were given?
    ;   Note: !Null can be given as a subscript: array[0, 0:3, !Null]
    nSub = n_elements(isRange)

    ;Create a copy of the array
    result = *self.array

    ;Step through the subscripts, reducing the result as necessary. Work from
    ;highest to lowest dimension in case the dimension becomes shallow and truncated
    ;by IDL.
    for i = nSub-1, 0, -1 do begin
        if isRange[i] eq 0 then begin
            case i of
                7: result = result[*,*,*,*,*,*,*,Subscript8]
                6: result = result[*,*,*,*,*,*,Subscript7,*]
                5: result = result[*,*,*,*,*,Subscript6,*,*]
                4: result = result[*,*,*,*,Subscript5,*,*,*]
                3: result = result[*,*,*,Subscript4,*,*,*,*]
                2: result = result[*,*,Subscript3,*,*,*,*,*]
                1: result = result[*,Subscript2,*,*,*,*,*,*]
                0: result = result[Subscript1,*,*,*,*,*,*,*]
            endcase
        endif else begin
            case i of
                7: result = result[*,*,*,*,*,*,*,Subscript8[0]:Subscript8[1]:Subscript8[2]]
                6: result = result[*,*,*,*,*,*,Subscript7[0]:Subscript7[1]:Subscript7[2],*]
                5: result = result[*,*,*,*,*,Subscript6[0]:Subscript6[1]:Subscript6[2],*,*]
                4: result = result[*,*,*,*,Subscript5[0]:Subscript5[1]:Subscript5[2],*,*,*]
                3: result = result[*,*,*,Subscript4[0]:Subscript4[1]:Subscript4[2],*,*,*,*]
                2: result = result[*,*,Subscript3[0]:Subscript3[1]:Subscript3[2],*,*,*,*,*]
                1: result = result[*,Subscript2[0]:Subscript2[1]:Subscript2[2],*,*,*,*,*,*]
                0: result = result[Subscript1[0]:Subscript1[1]:Subscript1[2],*,*,*,*,*,*,*]
            endcase
        endelse
    endfor
    
    ;Return the results    
    return, result
end


;+
;   The purpose of this method is to apply exponentiation with the caret operator.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of raising `LEFT` to the power of `RIGHT`.
;-
function MrArray::_OverloadCaret, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Exponentiate
    case side of
        'LEFT':  result = *self.array ^ right
        'RIGHT': result = left ^ *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to check for equality. See IDL's online help for
;   `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             True (1) if `LEFT` equals `RIGHT`. Otherwise false (0)
;-
function MrArray::_OverloadEQ, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Equal?
    case side of
        'LEFT':  result = *self.array eq right
        'RIGHT': result = left eq *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to provide iterations to the FOREACH operator. Iterate
;   over all of the elements in the array.
;
; :Params:
;       VALUE:              out, required, type=scalar
;                           Set to the value of the current object element. If `KEY` is
;                               undefined, then set this to the first object element.
;       KEY:                out, required, type=long
;                           Set to the index (or key) associated with the current element.
;                               In the first iteration, KEY is undefined.
;
; :Returns:
;       NEXT:               Returns 1 if there is a current element to retrieve, and 0 if
;                               there are no more elements.
;-
function MrArray::_OverloadForeach, value, key
    compile_opt strictarr
    on_error, 2
    
    nPts = n_elements(*self.array)
    if n_elements(key) eq 0 then key = 0

    ;Get the array element if the index is in range
    if key lt nPts then begin
        next = 1
        value = (*self.array)[key]
        
    ;Otherwise, stop iterating 
    endif else next = 0
    
    ;Next element to retrieve
    key += 1
    
    return, next
end


;+
;   The purpose of this method is to check if `LEFT` is greater than or equal to `RIGHT`.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             True (1) if `LEFT` greater than or equal to `RIGHT`.
;                               Otherwise false (0).
;-
function MrArray::_OverloadGE, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Compare
    case side of
        'LEFT':  result = *self.array ge right
        'RIGHT': result = left ge *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to check if `LEFT` is greater than `RIGHT`. See IDL's
;   online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             True (1) if `LEFT` greater than or equal to `RIGHT`.
;                               Otherwise false (0).
;-
function MrArray::_OverloadGT, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Compare
    case side of
        'LEFT':  result = *self.array gt right
        'RIGHT': result = left gt *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to return the element-by-element maximum between
;   `LEFT` and `RIGHT`. See IDL's online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             Returns the element of `LEFT` if it is greater than `RIGHT`,
;                               and vice versa.
;-
function MrArray::_OverloadGreaterThan, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Get maximum
    case side of
        'LEFT':  result = *self.array > right
        'RIGHT': result = left > *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to provide information when the HELP procedure
;   is called.
;
; :Params:
;       VARNAME:        in, required, type=string
;                       Name of the variable supplied to the HELP procedure.
;-
function MrArray::_OverloadHelp, varname
    compile_opt strictarr
    on_error, 2

    ;Info about the object
    heapnum = obj_valid(self, /GET_HEAP_IDENTIFIER)
    type    = size(self, /TNAME)
    class   = obj_class(self)
    
    ;Normal help string
    str = string(varname, type, '<ObjHeapVar', heapnum, '(', class, ')>', $
                 FORMAT='(a-12, a-12, a11, i0, a1, a0, a2)')

    ;Help about the dat    
    help, *self.array, OUTPUT=arrHelp
    arrHelp = stregex(arrHelp, '<.+>[ ]+([A-Z]+.+)', /SUBEXP, /EXTRACT)

    ;Concatenate all of the help together
    str = [[str], $
           ['  ARRAY       ' + strtrim(arrHelp[1], 1)]]
    
    return, str
end


;+
;   The purpose of this method is to provide information when implied print is used.
;-
function MrArray::_OverloadImpliedPrint
    compile_opt strictarr
    on_error, 2

    ;Print the array
    return, self -> _OverloadPrint()
end


;+
;   The purpose of this method is to evaluate if the array is true or not. Called when
;   logical operators are used (&&, ||, etc.)
;
; :Returns:
;       RESULT:         True (1) if the implicit array is a scalar or 1 element array
;                           not equal to zero and false if a scalar not equal to zero.
;                           Arrays with more than one element result in an error.
;-
function MrArray::_OverloadIsTrue
    compile_opt strictarr
    on_error, 2

    ;Make sure the array is a scalar
    if self.n_elements gt 1 then message, 'Array must be a scalar or 1 element array.'
    
    ;Is it true?
    result = (*self.array)[0] ne 0
    
    ;Print the array
    return, result
end


;+
;   The purpose of this method is to return the element-by-element minimum between
;   `LEFT` and `RIGHT`.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             Returns the element of `LEFT` if it is less than `RIGHT`,
;                               and vice versa.
;-
function MrArray::_OverloadLessThan, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Minimum
    case side of
        'LEFT':  result = *self.array < right
        'RIGHT': result = left < *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to check if `LEFT` is less than or equal to `RIGHT`.
;   See IDL's online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             True (1) if `LEFT` less than or equal to `RIGHT`.
;                               Otherwise false (0).
;-
function MrArray::_OverloadLE, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Compare
    case side of
        'LEFT':  result = *self.array le right
        'RIGHT': result = left le *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to check if `LEFT` is less than `RIGHT`. See IDL's
;   online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             True (1) if `LEFT` less than to `RIGHT`. Otherwise false (0).
;-
function MrArray::_OverloadLT, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Compare
    case side of
        'LEFT':  result = *self.array lt right
        'RIGHT': result = left lt *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to subract two expressions.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of subtracting `RIGHT` from `LEFT`.
;-
function MrArray::_OverloadMinus, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Subtract
    case side of
        'LEFT':  result = *self.array - right
        'RIGHT': result = left - *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to negate the implicit array.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             Negation of the implicit array.
;-
function MrArray::_OverloadMinusUnary
    compile_opt strictarr
    on_error, 2
    
    ;Negate the array, making positive values negative, and vice versa
    return, -(*self.array)
end


;+
;   The purpose of this method is to take the MOD of `LEFT` and `RIGHT`.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The remainder of `LEFT` divided by `RIGHT` (i.e. the MOD).
;-
function MrArray::_OverloadMOD, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Find the remainder
    case side of
        'LEFT':  result = *self.array mod right
        'RIGHT': result = left mod *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to check for inequality. See IDL's online help for
;   `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             True (1) if `LEFT` is not equal to `RIGHT`. Otherwise false (0).
;-
function MrArray::_OverloadNE, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Compare
    case side of
        'LEFT':  result = *self.array ne right
        'RIGHT': result = left ne *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to perform a logical NOT of the implicit array. See
;   the IDL Help page for `Bitwise Operators <http://exelisvis.com/docs/Bitwise_Operators.html>`
;   for more information.
;
; :Returns:
;       RESULT:             The logical NOT of the implicit array.
;-
function MrArray::_OverloadNOT, left, right
    compile_opt strictarr
    on_error, 2
    
    ;Bit-wise NOT
    return, not *self.array
end


;+
;   The purpose of this method is to perform an inclusive OR between `LEFT` and `RIGHT`.
;   SEE the IDL Help page for `Bitwise Operators <http://exelisvis.com/docs/Bitwise_Operators.html>`
;   for more information.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of `LEFT` OR `RIGHT`.
;-
function MrArray::_OverloadOR, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Bitwise OR
    case side of
        'LEFT':  result = *self.array OR right
        'RIGHT': result = left OR *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to add two expressions.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of adding `RIGHT` to `LEFT`.
;-
function MrArray::_OverloadPlus, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Add
    case side of
        'LEFT':  result = *self.array + right
        'RIGHT': result = left + *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to multiply the columns of `LEFT` by the rows of `RIGHT`.
;   (i.e., an IDL matrix multiplication, not a mathematical matrix multiplication). See
;   IDL's page for `Matrix Operators <http://exelisvis.com/docs/Matrix_Operators.html>`
;   for more information.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of matrix multiplying `LEFT` by `RIGHT` in the IDL
;                               sense.
;-
function MrArray::_OverloadPound, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Multiply columns by rows.
    case side of
        'LEFT':  result = *self.array # right
        'RIGHT': result = left # *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to multiply the rows of `LEFT` by the columns of `RIGHT`.
;   (i.e., a mathematical matrix multiplication, not an IDL matrix multiplication). See
;   IDL's page for `Matrix Operators <http://exelisvis.com/docs/Matrix_Operators.html>`
;   for more information.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of matrix multiplying `LEFT` by `RIGHT` in the
;                               mathematical sense.
;-
function MrArray::_OverloadPoundPound, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Multiply rows by columns
    case side of
        'LEFT':  result = *self.array ## right
        'RIGHT': result = left ## *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to provide information when implied print is used.
;   is called.
;-
function MrArray::_OverloadPrint
    compile_opt strictarr
    on_error, 2

    ;Print the array
    return, *self.array
end


;+
;   The purpose of this method is to obtain the results of IDL's Size() function when
;   applied to the applicit array.
;
; :Returns:
;       RESULT:             Results of the Size() function on the implicit array
;-
function MrArray::_OverloadSize
    compile_opt strictarr
    on_error, 2

    ;Return the dimensions of the array. The Size and N_Elements functions
    ;will know what to do with them.
    return, size(*self.array, /DIMENSIONS)
end


;+
;   The purpose of this method is to divide `LEFT` by `RIGHT`.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of dividing `LEFT` by `RIGHT`.
;-
function MrArray::_OverloadSlash, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Divide
    case side of
        'LEFT':  result = *self.array / right
        'RIGHT': result = left / *self.array
    endcase
    
    return, result
end


;+
;   The purpose of this method is to take the logical NOT (~) of the implicit array. See
;   IDL's online help page for `Logical Operators <http://exelisvis.com/docs/Logical_Operators.html>`
;
; :Returns:
;       RESULT:             The logical not of the implicit array.
;-
function MrArray::_OverloadTilde, left, right
    compile_opt strictarr
    on_error, 2

    ;Return the logical not.    
    return, ~(*self.array)
end


;+
;   The purpose of this method is to perform an eXlusive OR between `LEFT` and `RIGHT`.
;   SEE the IDL Help page for `Bitwise Operators <http://exelisvis.com/docs/Bitwise_Operators.html>`
;   for more information.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of `LEFT` XOR `RIGHT`.
;-
function MrArray::_OverloadXOR, left, right
    compile_opt strictarr
    on_error, 2
    
    ;On which side of the operator was "self" given?
    side = self -> _LeftOrRight(left, right)

    ;Bitwise XOR
    case side of
        'LEFT':  result = *self.array XOR right
        'RIGHT': result = left XOR *self.array
    endcase
    
    return, result
end


;+
;   Concatenate an array to the implicit array.
;
; :Params:
;       ARRAY:          in, required, type=intarr
;                       Array to be concatenated to the current array.
;       DIMENSION:      in, optional, type=integer, default=1
;                       The dimension, starting with 1, along which the two arrays are
;                           to be concatenated.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `ARRAY` will be appended to the beginning of the implicit
;                           array. The default is to append to the end.
;-
pro MrArray::Append, index, $
BEFORE=before
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    before = keyword_set(before)
    if n_elements(dimension) eq 0 then dimension = 1
    
    ;Concatenate
    if before eq 1 $
        then *self.array = MrConcatenate(array, *self.array, dimension) $
        else *self.array = MrConcatenate(*self.array, array, dimension)
end


;+
;   Convert 1-Dimensional indices to their multi-dimensional counterparts.
;
; :Params:
;       INDEX:          in, required, type=intarr
;                       Array of 1D indices.
;
; :Returns:
;       IMULTI:         Multi-dimensional indices.
;-
function MrArray::Array_Indices, index
    compile_opt strictarr
    on_error, 2
    
    ;Get the dimensions of the implicit array
    self -> GetProperty, DIMENSIONS=dims
    
    ;Convert to multi-dimensional indices
    iMulti = array_indices(dims, index, /DIMENSIONS)
    
    return, iMulti
end


;+
;   The prupose of this method is to erase the data from the array.
;-
pro MrArray::Clear
    compile_opt strictarr
    on_error, 2
    
    *self.array = !Null
end


;+
;   Extend a dimension of the implicit array by appending the desired number of
;   elements.
;
; :Params:
;       NELEMENTS:      in, required, type=intarr
;                       Number of elements to extend the `DIMENSION` of the implicit
;                           array.
;       DIMENSION:      in, optional, type=integer, default=1
;                       The dimension, starting with 1, to be extended.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `NELEMENTS` will be appended to the beginning of the
;                           implicit array. The default is to append to the end.
;       VALUE:          in, optional, type=any
;                       The value that the extended elements will take on.
;-
pro MrArray::Extend, nElements, dimension, $
BEFORE=before, $
VALUE=value
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    before = keyword_set(before)
    if n_elements(dimension) eq 0 then dimension = 1
    
    ;Create the 
    self -> GetProperty, DIMENSIONS=ExtendDims, TYPE=type
    ExtendDims[dimension-1] = nElements
    ExtendArray = Make_Array(DIMENSION=ExtendDims, TYPE=type, VALUE=value)

    ;Concatenate
    if before eq 1 $
        then *self.array = MrConcatenate(ExtendArray, *self.array, dimension) $
        else *self.array = MrConcatenate(*self.array, ExtendArray, dimension)
end


;+
;   Get class properties.
;
; :Keywords:
;       ARRAY:              out, optional, type=any
;                           Array to be stored internally.
;       DIMENSIONS:         out, optional, type=lonarr
;                           Sizes of each dimension of `ARRAY`.
;       MAXIMUM:            out, optional, type=any
;                           Maximum value of `ARRAY`.
;       MINIMUM:            out, optional, type=any
;                           Minimum value of `ARRAY`.
;       N_DIMENSIONS:       out, optional, type=int
;                           Number of dimensions of `ARRAY`.
;       N_ELEMENTS:         out, optional, type=long
;                           Number of elements in `ARRAY`.
;       TNAME:              out, optional, type=string
;                           Type-name of `ARRAY`.
;       TYPE:               out, optional, type=int
;                           Type-code of `ARRAY`.
;-
pro MrArray::GetProperty, $
ARRAY=array, $
DIMENSIONS=dimensions, $
N_DIMENSIONS=n_dimensions, $
N_ELEMENTS=n_elements, $
MINIMUM=minimum, $
MAXIMUM=maximum, $
TNAME=tname, $
TYPE=type
    compile_opt strictarr
    on_error, 2
    
    if arg_present(array)        then array        = *self.array
    if arg_present(dimensions)   then dimensions   = size(*self.array, /DIMENSIONS)
    if arg_present(n_dimensions) then n_dimensions = size(*self.array, /N_DIMENSIONS)
    if arg_present(n_elements)   then n_elements   = size(*self.array, /N_ELEMENTS)
    if arg_present(tname)        then tname        = size(*self.array, /TNAME)
    if arg_present(type)         then type         = size(*self.array, /TYPE)
    if arg_present(maximum)      then maximum      = max(*self.array)
    if arg_present(minimum)      then minimum      = min(*self.array)
end


;+
;   Make an array and store it internally.
;
; :Params:
;       D1:                 in, optional, type=any/integer
;                           Size of the second dimension.
;       D2:                 in, optional, type=integer
;                           Size of the second dimension.
;       D3:                 in, optional, type=integer
;                           Size of the third dimension.
;       D4:                 in, optional, type=integer
;                           Size of the fourth dimension.
;       D5:                 in, optional, type=integer
;                           Size of the fifth dimension.
;       D6:                 in, optional, type=integer
;                           Size of the sixth dimension.
;       D7:                 in, optional, type=integer
;                           Size of the seventh dimension.
;       D8:                 in, optional, type=integer
;                           Size of the eight dimension.
;
; :Keywords:
;       TYPE:               in, optional, type=int/float, default='FLOAT'
;                           The type-name or type-code of the array to be created.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's Make_Array procedure.
;-
pro MrArray::Make_Array, D1, D2, D3, D4, D5, D6, D7, D8, $
TYPE=type, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    if n_elements(type) eq 0 then type='FLOAT'
    
    ;Get a type-code if a name was given.
    tcode = size(type, /TNAME) ne 'STRING' ? type : self -> TypeName2Code(type) 

    ;Make the array
    case 1 of
        n_elements(D8) gt 0: *self.array = make_array(D1, D2, D3, D4, D5, D6, D7, D8, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        n_elements(D7) gt 0: *self.array = make_array(D1, D2, D3, D4, D5, D6, D7, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        n_elements(D6) gt 0: *self.array = make_array(D1, D2, D3, D4, D5, D6, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        n_elements(D5) gt 0: *self.array = make_array(D1, D2, D3, D4, D5, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        n_elements(D4) gt 0: *self.array = make_array(D1, D2, D3, D4, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        n_elements(D3) gt 0: *self.array = make_array(D1, D2, D3, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        n_elements(D2) gt 0: *self.array = make_array(D1, D2, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        n_elements(D1) gt 0: *self.array = make_array(D1, $
                                                      TYPE=tcode, _STRICT_EXTRA=extra)
        else: message, 'No dimensions specified for result.' 
    endcase
end


;+
;   Find the maximum value of the implicit array.
;
; :Params:
;       SUBSCRIPT_MAX:      in, optional, type=integer
;                           Index of the maximum value.
;
; :Keywords:
;       ABSOLUTE:           in, optional, type=boolean, default=0
;                           Find the minimum of the absolute value of the implicit array.
;       DIMENSION:          in, optional, type=integer
;                           Find the minimum along a particular dimension (begins with 1).
;       MIN:                out, optional, type=any
;                           Name of a variable to recieve the minimum array element.
;       NAN:                in, optional, type=boolean, default=0
;                           If set, NaN's will be excluded from the result.
;       SUBSCRIPT_MIN:      out, optional, type=integer
;                           Name of a variable to recieve the index of the `MIN`.
;-
function MrArray::Max, subscript_max, $
ABSOLUTE=absolute, $
DIMENSION=dimension, $
MIN=minimum, $
NAN=nan, $
SUBSCRIPT_MIN=subscript_min
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif
    
    ;Find the minimum.
    
    ;Find the maximum?
    if (arg_present(maximum) eq 0) && (arg_present(iMax) eq 0) then begin
        maximum = min(*self.array, subscript_max, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension)
    endif else begin
        maximum = min(*self.array, subscript_max, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension, $
                                                  MIN=minimum, SUSBSCRIPT_MIN=iMin)
    endelse
    
    return, maximum
end


;+
;   Find the minimum value of the implicit array.
;
; :Params:
;       SUBSCRIPT_MIN:      in, optional, type=integer
;                           Index of the minimum value.
;
; :Keywords:
;       ABSOLUTE:           in, optional, type=boolean, default=0
;                           Find the minimum of the absolute value of the implicit array.
;       DIMENSION:          in, optional, type=integer
;                           Find the minimum along a particular dimension (begins with 1).
;       MAX:                out, optional, type=any
;                           Name of a variable to recieve the maximum array element.
;       NAN:                in, optional, type=boolean, default=0
;                           If set, NaN's will be excluded from the result.
;       SUBSCRIPT_MAX:      out, optional, type=integer
;                           Index of the maximum value.
;-
function MrArray::Min, iMin, $
ABSOLUTE=absolute, $
DIMENSION=dimension, $
MAX=maximum, $
NAN=nan, $
SUBSCRIPT_MAX=iMax
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif
    
    ;Find the minimum.
    if (arg_present(maximum) eq 0) && (arg_present(iMax) eq 0) then begin
        minimum = min(*self.array, subscript_min, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension)
    endif else begin
        minimum = min(*self.array, subscript_min, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension, $
                                                  MAX=maximum, SUBSCRIPT_MAX=iMax)
    endelse
    
    return, minimum
end


;+
;   Set class properties.
;
; :Keywords:
;       ARRAY:              in, optional, type=any
;                           Array to be accessed via bracket overloading.
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, `ARRAY` will be copied directly into the object and
;                               will be left undefined.
;-
pro MrArray::SetProperty, $
ARRAY=array, $
NO_COPY=no_copy
    compile_opt strictarr
    on_error, 2

    ;Set the data
    if n_elements(array) gt 0 then begin
        if keyword_set(no_copy) $
            then *self.array = temporary(array) $
            else *self.array = array
    
    ;Was the !Null system variable given (not an undefined variable)? 
    endif else begin
        help, array, OUTPUT=helpTxt
        if strpos(helpTxt[0], '!NULL') ne -1 then *self.array = !Null
    endelse
end


;+
;   Convert the implicit array to the desired type.
;
; :Keywords:
;       TYPE:               in, optional, type=string/integer, default='INT'
;                           The or type-code type-name to which the implicit array will
;                               be converted.
;-
pro MrArray::SetType, type
    compile_opt strictarr
    on_error, 2
    
    ;Default to an integer
    if n_elements(type) eq 0 then type = 'INT'
    
    ;Type-Name?
    tt = size(type, /TNAME)
    typeCode = tt ne 'STRING' ? type : self -> TypeName2Code(tt)
    
    ;Fix the array type
    *self.array = fix(*self.array, TYPE=typeCode)
end


;+
;   Convert the implicit array to the desired type.
;
; :Keywords:
;       TYPENAME:           in, required, type=string
;                           The type-name to be converted to a type-code
;
; :Returns:
;       TYPECODE:           The type-code belonging to `TYPENAME`
;-
function MrArray::TypeName2Code, type
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


;+
;   Indices of the unique elements within the implicit array.
;
; :Keywords:
;       COMPLEMENT:         out, optional, type=intarr
;                           The index values of the non-unique elements of the implicit array.
;       COUNT:              out, optional, type=integer
;                           The number of unique elements in the implicit array.
;       NAN                 in, optional, type=boolean, default=0
;                           If set, all NaN values will be excluded from `IUNIQ`. Instead,
;                               they will be included in `COMPLEMENT` and `NCOMPLEMENT`.
;       NCOMPLEMENT:        out, optional, type=intarr
;                           The number of non-unique values in the implicit array.
;       VALUES:             out, optional, type=any
;                           Unique values of the imipicit array.
;
; :Returns:
;       IUNIQ:              Indices of the unique elements of the implicit array.
;-
function MrArray::Uniq, $
COMPLEMENT=complement, $
COUNT=count, $
NAN=nan, $
NCOMPLEMENT=nComplement, $
VALUES=values
    compile_opt strictarr
    on_error, 2
    
    ;Find uniq indices
    iSort = sort(*self.array)
    iUniq = uniq(*self.array, iSort)
    
    ;Remove NaNs
    if keyword_set(nan) then begin
        iFinite = where(finite(array[iUniq]) eq 1, nFinite)
        if nFinite gt 0 then begin
            iUniq = iUniq[iFinite]
        endif else begin
            iUniq = !Null
            count = 0
        endelse
    endif
    
    ;Count the number of uniq values
    if arg_present(count) then count = n_elements(unique_inds)
    
    ;Get the index values of the non-unique elements
    if arg_present(complement) or arg_present(ncomplement) then begin
        complement = where(histogram(iUniq, MIN=0, MAX=n_elements(array), BINSIZE=1) eq 0, nComplement, /NULL)
        if nComplement gt 0 then complement = iSort[complement]
    endif
    
    ;Return the uniq values?
    if arg_present(values) then values = (*self.array)[iSort[iUniq]]

    ;Return the uniq indices in the correct order.
    return, iSort[iUniq]
end


;+
;   Finds the intervals within a given monotonic vector that brackets a given set of
;   one or more search values. See IDL's `Value_Locate <http://exelisvis.com/docs/VALUE_LOCATE.html>`
;
; :Params:
;       VALUE:              in, required, type=any
;                           Values to be located in the implicit array.
;
; :Keywords:
;       L64:                in, optional, type=boolean, default=0
;                           If set, indices will be returned as type Long64.
;       SORT:               in, optional, type=boolean, default=0
;                           If set, the implicit array will first be sorted into
;                               ascending order, as required by the Value_Locate function.
;
; :Returns:
;       RESULT:             Indices into the implicit array.
;-
function MrArray::Value_Locate, value, $
L64=l64, $
SORT=sort
    compile_opt strictarr
    on_error, 2
    
    ;Sort first?
    if keyword_set(sort) then begin
        iSort = sort(*self.array)
        result = value_locate((*self.array)[iSort], value, L64=l64)
        result = result[iSort]
    endif else begin
        result = value_locate(*self.array, value, L64=l64)
    endelse
    
    return, result
end


;+
;   Compare the implicit array with a particular value and returns the indices that
;   the make the comparison true.
;
; :Params:
;       VALUE:              in, required, type=any
;                           Value to be the subject of the Where search.
;
; :Keywords:
;       COMPLEMENT:         out, optional, type=intarr
;                           Indices where the comparison failed.
;       COUNT:              out, optional, type=integer
;                           Number of true comparisons.
;       L64:                in, optional, type=boolean, default=0
;                           If set, indices will be returned as type Long64.
;       NCOMPLEMENT:        out, optional, type=integer
;                           Number of false comparisons.
;       EQUAL:              in, optional, type=boolean
;                           If set, use the EQ operator for the comparison. If no other
;                               input keyords are given, this is assumed.
;       GREATER:            in, optional, type=boolean, default=0
;                           If set, use the GT operator for the comparison.
;       GEQ:                in, optional, type=boolean, default=0
;                           If set, use the GE operator for the comparison.
;       NOTEQ:              in, optional, type=boolean, default=0
;                           If set, use the NE operator for the comparison.
;       LESS:               in, optional, type=boolean, default=0
;                           If set, use the LT operator for the comparison.
;       LEQ:                in, optional, type=boolean, default=0
;                           If set, use the LE operator for the comparison.
;
; :Returns:
;       RESULT:             Indices into the implicit array where the comparison returned
;                               true. If `COUNT`=0, !Null is returned.
;-
function MrArray::Where, value, $
COMPLEMENT=complement, $
COUNT=count, $
L64=l64, $
NCOMPLEMENT=n_complement, $
;Relational Operators
EQUAL=equal, $
GREATER=greater, $
GEQ=GEQ, $
NOTEQ=notEQ, $
LESS=lessThan, $
LEQ=LEQ
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    equal   = keyword_set(equal)
    greater = keyword_set(greater)
    geq     = keyword_set(geq)
    noteq   = keyword_set(notEQ)
    less    = keyword_set(less)
    leq     = keyword_set(leq)
    
    ;Resolve conflicts
    nKeys = equal + less + leq + greater + geq
    if nKeys gt 1 then message, 'Conflicting keywords. Only one input keyword is allowed.'
    if nKeys eq 0 then equal = 1
    
    ;Check where
    case 1 of
        equal:   result = where(*self.array eq value, count, COMPLEMENT=complement, N_COMPLEMENT=ncomplement, L64=l64, /NULL)
        greater: result = where(*self.array ge value, count, COMPLEMENT=complement, N_COMPLEMENT=ncomplement, L64=l64, /NULL)
        geq:     result = where(*self.array gt value, count, COMPLEMENT=complement, N_COMPLEMENT=ncomplement, L64=l64, /NULL)
        noteq:   result = where(*self.array ne value, count, COMPLEMENT=complement, N_COMPLEMENT=ncomplement, L64=l64, /NULL)
        less:    result = where(*self.array le value, count, COMPLEMENT=complement, N_COMPLEMENT=ncomplement, L64=l64, /NULL)
        leq:     result = where(*self.array lt value, count, COMPLEMENT=complement, N_COMPLEMENT=ncomplement, L64=l64, /NULL)
    endcase
    
    return, result
end


;+
;   Clean up after the object is destroyed
;-
pro MrArray::cleanup
    compile_opt strictarr
    on_error, 2
    
    ;Free pointers
    ptr_free, self.array
end


;+
;   The initialization method.
;
; :Params:
;       ARRAY:              in, optional, type=any/integer
;                           If an array, then it is the array to be stored and all other
;                               parameters are ignored. If a scalar integer, the size
;                               if the first dimension in the resulting array.
;       D2:                 in, optional, type=integer
;                           Size of the second dimension. Ignored unless `ARRAY` is scalar.
;       D3:                 in, optional, type=integer
;                           Size of the third dimension. Ignored unless `ARRAY` is scalar.
;       D4:                 in, optional, type=integer
;                           Size of the fourth dimension. Ignored unless `ARRAY` is scalar.
;       D5:                 in, optional, type=integer
;                           Size of the fifth dimension. Ignored unless `ARRAY` is scalar.
;       D6:                 in, optional, type=integer
;                           Size of the sixth dimension. Ignored unless `ARRAY` is scalar.
;       D7:                 in, optional, type=integer
;                           Size of the seventh dimension. Ignored unless `ARRAY` is scalar.
;       D8:                 in, optional, type=integer
;                           Size of the eight dimension. Ignored unless `ARRAY` is scalar.
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, `ARRAY` will be copied directly into the object and
;                               will be left undefined.
;       TYPE:               in, optional, type=int/float, default='FLOAT'
;                           The type of array to be created. Ignored unless `ARRAY` is scalar
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's Make_Array procedure. Ignored
;                               unless `ARRAY` is scalar.
;-
function MrArray::init, array, D2, D3, D4, D5, D6, D7, D8, $
NO_COPY=no_copy, $
TYPE=type, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Allocate heap to pointers.
    self.array = ptr_new(/ALLOCATE_HEAP)
    
    ;Set the filename for known filetypes
    if IsA(array, /ARRAY) $
        then self -> SetProperty, ARRAY=array, NO_COPY=no_copy $
        else self -> Make_Array, array, D2, D3, D4, D5, D6, D7, D8, TYPE=type, _REF_EXTRA=extra
    
    return, 1
end


;+
;   The class definition statement.
;
; :Fields:
;       DATA:       Data to be accessed via bracket overloading.
;-
pro MrArray__define
    compile_opt strictarr
    
    class = { MrArray, $
              inherits IDL_Object, $
              array: Ptr_New() $
            }
end