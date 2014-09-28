; docformat = 'rst'
;
; NAME:
;       MrIsBalanced
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   Test if a string containing '(', '[', '{' are properly balanced by ')', ']', '}'.
;   Custom open and close characters are allowed.
;
; :Examples:
;   Try the main-level example program at the end of this document.
;       IDL> .r MrIsBalanced
;                           
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
;       2014-06-24  -   Written by Matthew Argall
;       2014-06-29  -   Added the ESCAPE_CHAR keyword. - MRA
;-
;*****************************************************************************************
;+
;   Recursively balance parens. In this approach, the call stack is used as the open-close
;   paren stack. This was found to be much slower than the non-recursive search. See the
;   example program for profiling.
;
; :Examples:
;   See the main level program at the end of this document::
;       IDL> .r MrIsBalanced
;
; :Params:
;       STR:            in, required, type=string
;                       String to be tested for balanced parentheses, square brackets,
;                           curly brackets, etc.
;       CHAROPEN:       in, optional, type=strarr, default="['(', '[', '{']"
;                       Characters that indicate a sequence has been opened. Must be
;                           used with `CHARCLOSE`.
;       CHARCLOSE:      in, optional, type=strarr, default="[')', ']', '}']"
;                       Characters that indicate a sequence has been closed. All characters
;                           must be the same length. Each element of CHARCLOSE must match
;                           1-to-1 with the open character in `CHAROPEN`.
;
; :KEYWORDS:
;       START_POS:      in, optional, type=integer, default=0
;                       Location in `STR` at which to begin balancing.
;       POS:            in, private, optional, type=integer, default=`START_POS`
;                       Current position within the string where balance in being tested.
;       LEN:            in, private, optional, type=integer, default=(strlen(`CHAROPEN`))[0]
;                       Length of the open and close characters.
;
; :Returns:
;       RESULT:         If RESULT equals strlen(`STR`), then the string is balanced. If
;                           not, then RESULT gives the location of the first character
;                           at which balancing failed.
;-
function MrIsBalanced_Recursive, str, charOpen, charClose, $
START_POS=start_pos, $
LEN=len, $
POS=pos
    compile_opt strictarr
    on_error, 2

;---------------------------------------------------------------------
; Initial Conditions /////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(pos) eq 0 then begin
        ;Open and Close characters
        if n_elements(charOpen)  eq 0 then begin
            charOpen  = ['(', '[', '{']
            charClose = [')', ']', '}']
        endif
        
        ;Make sure the open and close are of the same length
        if min( [strlen(charOpen), strlen(charClose)] eq strlen(charOpen[0]) ) eq 0 then $
            message, 'All open and close characters must be the same length.'
        len = strlen(charOpen[0])
        
        ;Start at the beginning of the string
        if n_elements(start_pos) eq 0 then start_pos = 0
        pos = start_pos
        
        ;No Open has been found yet
        next_close = ''
    endif
    
;---------------------------------------------------------------------
; Process String /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;The current character being tested.
    thisChar = strmid(str, pos, len)

    ;End or Close?
    ;   - Return if any close character is matched.
    if thisChar eq '' || max(thisChar eq charClose) then return, pos
    
    ;Open?
    if max(thisChar eq charOpen) then begin
        iLastOpen = where(charOpen eq thisChar)
        nextClose = charClose[iLastOpen[0]]
    
        ;Check the next character
        ;   - Search for matching Close
        ;   - Non-Close and non-End characters will be eaten
        nextPos  = MrIsBalanced_Recursive(str, charOpen, charClose, POS=pos+len, LEN=len)
        nextChar = strmid(str, nextPos, len)

        ;Is the next character a close?
        ;   - Successful Open-Close pair was found.
        ;   - Proceed to next Open-Close pair or the End.
        ;   - Closes without a matching Open will be caught here as well.
        if nextChar eq nextClose then return, MrIsBalanced_Recursive(str, charOpen, charClose, $
                                                           POS=nextPos+len, LEN=len)

        ;If we get here, then an Open was found without a Close
        ;   - Return the failing Open character
        return, nextPos
    endif
    
    ;No Open, Close, or End character was found.
    ;   - Move to the next character.
    ;   - Continue eating characters until an Open, Close, or End is found.
    return, MrIsBalanced_Recursive(str, charOpen, charClose, POS=pos+1, LEN=len)
end


;+
;   Balance parentheses (brackets, braces, etc.) by stepping through each character in
;   the string.
;
; :Params:
;       STR:            in, required, type=string
;                       String to be tested for balanced parentheses, square brackets,
;                           curly brackets, etc.
;       CHAROPEN:       in, optional, type=strarr, default="['(', '[', '{']"
;                       Characters that indicate a sequence has been opened. Must be
;                           used with `CHARCLOSE`.
;       CHARCLOSE:      in, optional, type=strarr, default="[')', ']', '}']"
;                       Characters that indicate a sequence has been closed. All characters
;                           must be the same length. Each element of CHARCLOSE must match
;                           1-to-1 with the open character in `CHAROPEN`.
;
; :KEYWORDS:
;       CONTENT:        out, optional, type=strarr
;                       Contents of each Open-Close set, returned in the order that the
;                           open character was found.
;       COUNT:          out, optional, type=long
;                       Number of open-close pairs found. If `STR` is unbalanced, -1
;                           is returned.
;       ESCAPE_CHAR:    in, optional, type=string, default='\'
;                       An escape character used to indicate that a particular open or
;                           close character should be ignored. Two escape characters
;                           ("\\") indicate that the actual character is intended ("\")
;                           and is not meant to be an escape.
;       FAIL_POS:       out, optional, type=integer
;                       If parentheses are unbalanced, this is the position in the string
;                           at which the failure occurred. If the string is balanced, then
;                           FAIL_POS will equal the length of the string.
;       POSITIONS:      out, optional, type=2x`COUNT` lonarr
;                       Position of the [open, close] character pairs within the string.
;                           If `STR` is unbalanced, [0,0] is returned.
;       RECURSIVE:      in, optional, type=boolean, default=0
;                       If set, a recurive approach will be taken.
;       START_POS:      in, optional, type=integer, default=0
;                       Location in `STR` at which to begin balancing.
;
; :Returns:
;       TF_BALANCED:    Returns 1 (one) if the string is balanced and 0 otherwise.
;-
function MrIsBalanced, str, charOpen, charClose, $
CONTENT=content, $
COUNT=openCount, $
ESCAPE_CHAR=escape_char, $
FAIL_POS=pos, $
POSITIONS=positions, $
RECURSIVE=recursive, $
START_POS=start_pos
    compile_opt strictarr
    on_error, 2
    
;---------------------------------------------------------------------
; Defaults ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    recursive = keyword_set(recursive)
    if n_elements(escape_char) eq 0 then escape_char = '\'
    if n_elements(start_pos) eq 0 then start_pos = 0
    if n_elements(charOpen) eq 0 then begin
        charOpen  = ['(', '[', '{']
        charClose = [')', ']', '}']
    endif
    
    ;The double escape
    twoEsc = escape_char + escape_char
    
    ;Validate inputs
    if max( [strlen(charOpen), strlen(charClose)] ne strlen(charOpen[0]) ) then $
        message, 'All Open and Close characters must be the same length.'
    len = strlen(charOpen[0])
    
    ;Output?
    if arg_present(positions) then doPos  = 1 else doPos  = 0
    if arg_present(content)   then doCont = 1 else doCont = 0
    if doCont then doPos = 1
    
    ;Use the recursive method?
    if recursive then begin
        return, MrIsBalanced_Recursive(str, charOpen, charClose, START_POS=start_pos)
    endif
    
;---------------------------------------------------------------------
; Process String /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Simple case
    nChars = strlen(str)
    if nChars eq 0 then return, 0
    
    ;Allocate memory
    stack      = strarr(2, nChars)
    pos        = start_pos
    iStack     = 0L
    openCount  = 0L
    
    if doPos then begin
        tempPos    = lonarr(nChars)
        positions  = lonarr(2, nChars)
        closeCount = 0L
    endif
    
    ;Step through each character
    while pos lt strlen(str) do begin
        thisChar = strmid(str, pos, len)

        ;Open character?
        if max(thisChar eq charOpen, iMax) then begin
            ;Was it escaped?
            If strmid(str, pos-1, 1) eq escape_char && strmid(str, pos-2, 2) ne twoEsc then begin
                pos += 1
                continue
            endif
        
            ;Store the open characters.
            ;   - Pair it with its matching close character
            ;   - Store the position
            stack[*,iStack] = [thisChar, charClose[iMax]]
            if doPos then tempPos[iStack] = pos
            openCount += 1
            iStack    += 1

        ;Close character?
        endif else if max(thisChar eq charClose, iMax) then begin
            ;Was it escaped?
            If strmid(str, pos-1, 1) eq escape_char && strmid(str, pos-2, 2) ne twoEsc then begin
                pos += 1
                continue
            endif
        
            ;Must match the last close character
            ;   - There must have been an open
            ;   - A ")" must match a "(", not a "[" etc.
            if iStack eq 0 || thisChar ne stack[1,iStack-1] then return, pos

            ;Store the open-close pair of positions
            if doPos then begin
                positions[*,closeCount] = [tempPos[iStack-1], pos]
                closeCount += 1
            endif
            
            ;Move backward in the stack to the previous unclosed open character.
            iStack -= 1
        endif
        
        ;Next character
        pos += 1
    endwhile

    ;Is the stack empty?
    if iStack ne 0 then pos = pos-1
    
;---------------------------------------------------------------------
; Post-Processing ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Was it balanced?
    tf_balanced = pos eq strlen(str)

    ;Not Balanced?
    if tf_balanced eq 0 then begin
        positions = [0,0]
        count     = -1
        
    ;Return positions/content?
    endif else if doPos then begin
        ;Trim results
        positions = positions[*,0:closeCount-1]

        ;Sort
        iSort = sort(positions[0,*])
        positions[0,*] = positions[0,iSort]
        positions[1,*] = positions[1,iSort]

        ;Get the content?
        if doCont then begin
            ;Do not take the encompassing parentheses.
            length = reform(positions[1,*] - positions[0,*] - 1)
            content = strmid(str, reform(positions[0,*]+1), length)
        endif
    endif

    return, tf_balanced
end


;---------------------------------------------------
; Main Level Example Program (.r MrStrCount) ///////
;---------------------------------------------------
;EXAMPLE 1
;   - Create some test strings
testStrings = ['()', '(', ')', '', '(()))', '(((())))', '()()(()())', $
               '(() ( hi))) (())()(((( ))))', 'abcd', '[]', '{}', '()[]{}', '{[()]}', $
               '{[()([])]()}', '[([](()]]']
               
;Check if they match
print, 'Case Tests:'
longest = strtrim(max(strlen(testStrings)), 2)
for i = 0, n_elements(testStrings) - 1 do begin
    ;Balance them
    tf_balanced = MrIsBalanced(testStrings[i])
    
    ;Print results
    if tf_balanced $
        then print, testStrings[i], 'Pass!', FORMAT='(2x, a-' + longest + ', 3x, a0)' $
        else print, testStrings[i], 'Failed at ', result, FORMAT='(2x, a-' + longest + ', 3x, a10, i0)'
endfor

;EXAMPLE 2
;   - Use recursive method with same test strings
print, ''
print, '---------------------------------------------------------------'
print, 'Case Tests:'
longest = strtrim(max(strlen(testStrings)), 2)
for i = 0, n_elements(testStrings) - 1 do begin
    ;Balance them
    pos = !Null
    result = MrIsBalanced(testStrings[i])
    
    ;Print results
    if result eq strlen(testStrings[i]) $
        then print, testStrings[i], 'Pass!', FORMAT='(2x, a-' + longest + ', 3x, a0)' $
        else print, testStrings[i], 'Failed at ', result, FORMAT='(2x, a-' + longest + ', 3x, a10, i0)'
endfor


;EXAMPLE 3
;   - Use custom open and close strings
testString  = '%(the(brown]fox%)'
tf_balanced = MrIsBalanced(testString, '%(', '%)')
pass        = tf_balanced ? 'Balanced!  ' : 'Not Balanced:  '

;print the results
print, ''
print, '-----------------------------------------------'
print, 'Search for custom open "%(" and close "%)":'
print, pass, testString, FORMAT='(2x, 2(a0))'

;EXAMPLE 4
;   - Extract the contents of each paren
testStr = '(hk(lumberjack)ad(black[forest]ham)fajh)'
tf_balanced = MrIsBalanced(testStr, COUNT=count, CONTENT=content)

;Print results
print, ''
print, '--------------------------------------------------'
print, 'String: ' + testStr
print, '  # Found: ' + strtrim(count, 2)
print, '  Contents:'
for i = 0, count-1 do print, '    ', content[i]
               

;EXAMPLE 5
;   - Use the escape characters
theString = '5 * ( 6 + \)3 \\)'
tf_balanced = MrIsBalanced(theString)

;Print results
print, ''
print, '---------------------------------------------------'
print, FORMAT='(%"%s is balanced?   %s")', theString, (tf_balanced ? 'Yes' : 'No')


;EXAMPLE 6
;   - Speed test of normal vs. /RECURSIVE
;   - Test on a 100 character string.
testStr = strjoin(replicate('(([]{()[{{}}]})', 6)) + '{[([]){}]}'

;Run test
print, ''
print, '--------------------------------------------------'
nTrials = 100
profiler
print, 'Normal Method:'
for i = 0, nTrials-1 do result = MrIsBalanced(testStr)
profiler, /REPORT
profiler, /RESET
print, ''
print, 'Normal Method with Content:'
for i = 0, nTrials-1 do result = MrIsBalanced(testStr, CONTENT=content)
profiler, /REPORT
profiler, /RESET
print, ''
print, 'Recursive Method:'
for i = 0, nTrials-1 do result = MrIsBalanced_Recursive(testStr)
profiler, /REPORT
profiler, /RESET
profiler, /CLEAR



end