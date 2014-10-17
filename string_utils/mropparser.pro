; docformat = 'rst'
;
; NAME:
;    MrOpParser
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Determine the order of operations.
;
; :Categories:
;    String Utilities
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 113
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@wildcats.unh.edu
;
; :History:
;    Modification History::
;       2014/10/14  -   Written by Matthew Argall
;       2014/10/16  -   Handle parentheses. - MRA
;-
;*****************************************************************************************
;+
;   Parse the next value from the string of operations.
;
; :Private:
;
; :Params:
;       OPERATOR:       in, required, type=string
;                       The operator for which the associativity is to be determined.
;
; :Returns:
;       ASSOCIATIVITY:  Associativity of the given operator. Returns 1 for left-associative
;                           and 2 for right-associative.
;-
function MrOpParser_GetAssoc, operator
    on_error, 2

    ;Define order of operations.
    case operator of
        '==': associativity = 'LEFT'
        '+':  associativity = 'LEFT'
        '-':  associativity = 'LEFT'
        '*':  associativity = 'LEFT'
        '/':  associativity = 'LEFT'
        '^':  associativity = 'LEFT'
        '':   associativity = ''
        else: message, 'Operator not recognized: "' + operator + '".'
    endcase

    return, associativity
end


;+
;   Parse the next value from the string of operations.
;
; :Private:
;
; :Params:
;       OPERATOR:       in, required, type=string
;                       The operator for which the precedence is to be determined.
;
; :Returns:
;       PRECEDENCE:     Precedence of the given operator.
;-
function MrOpParser_GetPrecedence, operator
    on_error, 2

    ;Define order of operations.
    case operator of
        '==': precedence =  0
        '+':  precedence =  1
        '-':  precedence =  1
        '*':  precedence =  2
        '/':  precedence =  2
        '^':  precedence =  3
        '':   precedence = -1
        else: message, 'Operator not recognized: "' + operator + '".'
    endcase

    return, precedence
end


;+
;   Parse the next value from the string of operations.
;
; :Private:
;
; :Params:
;       OPSTR:          in, required, type=string
;                       A string from which the value is extracted. The operator is
;                           expected to begin at the first character in the string.
;       REMAINDER:      out, optional, type=string
;                       The remainder of `OPSTR` after the operator has been extracted.
;
; :Returns:
;       ATOM:           The extracted value.
;-
function MrOpParser_NextAtom, opStr, remainder, $
COUNT=count, $
OPERATIONS=operations
    on_error, 2

    _opStr = strtrim(opStr, 2)
;-------------------------------------------------------
; Subexpression ////////////////////////////////////////
;-------------------------------------------------------
    ;Is the first character a parentheses?
    char = strmid(_opStr, 0, 1)

    if char eq '(' then begin
        pos   = 1
        nOpen = 1
        nChars = strlen(_opStr)
    
        ;Find the matching close parens
        ;   - Continue until
        ;       o The parenthesis is closed
        ;       o We reach the end of the string
        while (char ne ')' && nOpen gt 0) || (pos lt nChars) do begin
            ;Get the next character
            char = strmid(_opStr, pos, pos+1)
            
            ;A parenthesis?
            case char of
                '(': nOpen += 1
                ')': nOpen -= 1
                else: ;Ignore
            endcase
            
            ;Next character
            pos += 1
        endwhile
        
        ;Parentheses not balanced?
        if nOpen ne 0 then message, 'Parentheses are not balanced.'

        ;Evaluate the subexpression within parentheses
        ;   - Extract the expression
        ;   - Evaluate the expression -- it is the next atom.
        expression = strmid(_opStr, 1, pos-2)
        atom       = MrOpParser(expression, COUNT=count, OPERATIONS=operations)

;-------------------------------------------------------
; Any Non-Operator Sequence ////////////////////////////
;-------------------------------------------------------
    endif else begin
        ;Find the next operator
        opRegEx = '(\+|-|\*|/|==|\^)'
        pos     = stregex(_opStr, opRegEx)
    
        ;Extract the atom
        ;   - Anything that is not an operator
        ;   - If no operator was found, take the whole string
        ;   - Otherwise, take up to the next operator.
        if pos eq -1 $
            then atom = _opStr $
            else atom = strtrim(strmid(_opStr, 0, pos), 2)
    endelse
    
    
    ;Extract the remainder
    remainder = strtrim(strmid(_opStr, pos), 2)
    return, atom
end


;+
;   Parse the next operation from the string.
;
; :Private:
;
; :Params:
;       OPSTR:          in, required, type=string
;                       A string from which the operator is extracted. The operator is
;                           expected to begin at the first character in the string.
;       REMAINDER:      out, optional, type=string
;                       The remainder of `OPSTR` after the operator has been extracted.
;
; :Returns:
;       NEXTOP:         The extracted operator.
;-
function MrOpParser_NextOp, opStr, remainder, $
PRECEDENCE=precedence, $
ASSOCIATIVITY=associativity
    on_error, 2
    
    ;Extract the next operator
    opRegEx = '^(\+|-|\*|/|==|\^)'
    pos     = stregex(strtrim(opStr, 2), opRegEx, LEN=len)

    ;Extract the operator
    if pos eq -1 $
        then nextOp = '' $
        else nextOp = strmid(opStr, pos, len)
        
    ;Extract the remainder
    remainder = strmid(opStr, len)
    
    ;Get other features?
    if arg_present(precedence)    then precedence    = MrOpParser_GetPrecedence(nextOp)
    if arg_present(associativity) then associativity = MrOpParser_GetAssoc(nextOp)

    return, nextOp
end


;+
;   Evaluate the expression.
;
; :Params:
;       EXPRESSION:     in, required, type=string
;                       The expression to be evaluated.
;       MIN_PREC:       in, private, required, type=string
;                       Minimum precedence at which operations should be evaluated.
;       REMAINDER:      in, out, private, required, type=string
;                       When recursing, the part of `EXPRESSION` yet to be evaluated.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of operations evaluated.
;       OPERATIONS:     out, optional, type=strarr(3\,`COUNT`)
;                       The [lhs, op, rhs] of each operation, in the order they are to
;                           be evaluated. "ans" refers to the answer computed in the
;                           previous operation.
;
; :Returns:
;       LHS:            A string joining each operation, parenthesized to demonstrate
;                           the order in which operations should be executed.
;-
function MrOpParser, expression, min_prec, remainder, $
COUNT=count, $
OPERATIONS=operations
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG(/QUIET)
        return, ''
    endif

;-------------------------------------------------------
; Initial Conditions ///////////////////////////////////
;-------------------------------------------------------
    ;Initial conditions of recursion.
    ;   - Extract the left-hand side of the first expression.
    ;   - Precedence starts at 0.
    if n_elements(min_prec) eq 0 then begin
        lhs      = MrOpParser_NextAtom(expression, remainder)
        min_prec = 0
    endif else begin
        lhs = expression
    endelse
    
    ;Initial conditions of while loop
    ;   - First operator and the value on which it acts.
    this_op = MrOpParser_NextOp(remainder, tempRemain, $
                                PRECEDENCE    = this_prec, $
                                ASSOCIATIVITY = this_assoc)
    rhs     = MrOpParser_NextAtom(tempRemain, rhs_remain, $
                                  COUNT=count, OPERATIONS=operations)

;-------------------------------------------------------
; Evaluate Expressions /////////////////////////////////
;-------------------------------------------------------
    ;Example: 2 + 1 + 2
    ;   - Since the minimum precedence of all operators is 0, we will
    ;       loop through all operators until we find one that is < 0
    ;       (i.e. the end-of-expression empty string).
    while this_prec ge min_prec do begin
        ;Look ahead to the next operator.
        next_op = MrOpParser_NextOp(rhs_remain, $
                                    PRECEDENCE    = next_prec, $
                                    ASSOCIATIVITY = next_assoc)

    ;-------------------------------------------------------
    ; Next Operation Takes Precedence to Current? //////////
    ;-------------------------------------------------------
        ;Example: 2 + 1 * 2
        ;   - Evaluate as 2 + (1 * 2)
        while next_prec gt this_prec do begin
            ;Example: 1 * 2 * 3
            ;   - 'LEFT':  => ((2 * 3) * 4) = 24
            ;   - 'RIGHT': => (2 * (3 * 2)) = 12
            ;   - Bump up the precedence for left-associative operators so that
            ;       a look-ahead addition does not supercede an earlier addition.
            if this_assoc eq 'LEFT' $
                then next_min_prec = this_prec + 1 $
                else next_min_prec = this_prec
        
            ;Recurse and evaluate the next operation
            rhs = MrOpParser(rhs, next_min_prec, rhs_remain, $
                             COUNT=count, OPERATIONS=operations)

            ;Next iteration
            ;   - "Look Ahead" to the next operator.
            la_op   = MrOpParser_NextOp(rhs_remain, $
                                        PRECEDENCE    = la_prec, $
                                        ASSOCIATIVITY = la_assoc)

            ;Example: 2 + 3^2 * 6
            ;   - Both ^ and * must be evaluated before returning to +
            this_prec = next_prec
            next_prec = la_prec
            if la_prec gt next_prec then lhs = rhs
        endwhile

    ;-------------------------------------------------------
    ; Record Results ///////////////////////////////////////
    ;-------------------------------------------------------
        ;Count and store operations in order.
        if n_elements(operations) eq 0 then begin
            operations = [lhs, this_op, rhs]
            count      = 1
        endif else begin
            ;Substitute "ans" for the part of the expression that is already evaluated.
            if stregex(rhs, '[()]', /BOOLEAN) $
                then operations = [[operations], [ lhs,  this_op, 'ans']] $
                else operations = [[operations], ['ans', this_op,  rhs ]]
            count++
        endelse

        ;Form the operation
        lhs = '(' + lhs + ' ' + this_op + ' ' + rhs + ')'

    ;-------------------------------------------------------
    ; Next Iteration ///////////////////////////////////////
    ;-------------------------------------------------------
        ;Update the remainder to chop of parts that have been evaluated.
        remainder = rhs_remain

        ;Next iteration
        ;   - Get the next operator and value
        this_op   = MrOpParser_NextOp(rhs_remain, tempRemain, $
                                      PRECEDENCE    = this_prec, $
                                      ASSOCIATIVITY = this_assoc)
        rhs       = MrOpParser_NextAtom(tempRemain, rhs_remain, $
                                        COUNT=count, OPERATIONS=operations)
    endwhile
    
    return, lhs
end




;-------------------------------------------------------
; Main-Level Example Program (.r MrOpParser) ///////////
;-------------------------------------------------------
;Example 1
;   - Recurse once: * trumps +
opStr  = '2 + 3 * 4 + 5 == 19'
result = MrOpParser(opStr, COUNT=count, OPERATIONS=operations)
print, 'Operation: ' + opStr
print, 'Result:    ' + result
print, 'Order:'
for i = 0, count-1 do print, FORMAT='(%"  %3s %3s %3s")', operations[*,i]
void = temporary(operations)
print, ''

;Example 2
;   - Recurse twice: (* trumps +) and (^ trumps *)
opStr  = '1 + 5 * 2^2'
result = MrOpParser(opStr, COUNT=count, OPERATIONS=operations)
print, '-----------------------------------'
print, 'Operation: ' + opStr
print, 'Result:    ' + result
print, 'Order:'
for i = 0, count-1 do print, FORMAT='(%"  %3s %3s %3s")', operations[*,i]
void = temporary(operations)
print, ''

;Example 3
;   - Skip ahead: (^ and * trump +) but (^ trumps *)
opStr  = '1 + 2^2 * 5'
result = MrOpParser(opStr, COUNT=count, OPERATIONS=operations)
print, '-----------------------------------'
print, 'Operation: ' + opStr
print, 'Result:    ' + result
print, 'Order:'
for i = 0, count-1 do print, FORMAT='(%"  %3s %3s %3s")', operations[*,i]
void = temporary(operations)
print, ''

;Example 4
;   - Subexpression
opStr  = '1 * (2 + 3)'
result = MrOpParser(opStr, COUNT=count, OPERATIONS=operations)
print, '-----------------------------------'
print, 'Operation: ' + opStr
print, 'Result:    ' + result
print, 'Order:'
for i = 0, count-1 do print, FORMAT='(%"  %3s %3s %3s")', operations[*,i]
void = temporary(operations)
print, ''


end