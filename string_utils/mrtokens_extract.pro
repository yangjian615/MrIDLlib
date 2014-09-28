; docformat = 'rst'
;
; NAME:
;       MrTimeTokensToRegex
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
;   Replace tokens with regex expressions that can be used to pull information out of
;   the file names.
;
; :Example:
;   See the example program at the end of this document::
;       IDL> .run MrTokens_Extract
;
; :Params:
;       PATTERN:            in, required, type=string
;                           Pattern containing tokens that identify different date and
;                               time elements within the string it represents.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of tokens found in `PATTERN`. If `GET_TOKENS` is set,
;                               then COUNT represents the number of tokens returned.
;       POSITIONS:          out, optioinal, type=lonarr
;                           Positions within `PATTERN` at which "%[token]" can be found.
;                               The positions point to the "%" character, not the token
;                               that follows it.
;       OPATTERN:           out, optional, type=string
;                           If `IGNORE_PARENS` or `REPLACE_PARENS` is set, then this will
;                               be the resulting `PATTERN` with parentheses and their
;                               contents removed.
;       IGNORE_PARENS:      in, optional, type=boolean, default=0
;                           If set, "%(" and "%)" tokens will be excluded from `TOKENS`.
;                               Anything between "%(" and "%)" is always ignored.
;       REPLACE_PARENS:     in, optional, type=string, default=''
;                           A string that will replace the %( and %) tokens and their
;                               contents. Setting this keyword automatically sets
;                               `IGNORE_PARENS`=1.
;
; :Returns:
;       TOKENS:         A string that can be passed to the STREGEX function to extract
;                           time elements from the string that `PATTERN` represents.
;                           It may be necessary to set the /FOLD_CASE keyword in STREGEX.
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/06/30  -   Written by Matthew Argall
;-
function MrTokens_Extract, pattern, $
COUNT=count, $
IGNORE_PARENS=ignore_parens, $
OPATTERN=pattern_out, $
POSITIONS=positions, $
REPLACE_PARENS=replace_parens
    compile_opt strictarr
    on_error, 2
    
    ;Create an editable copy
    _pattern = pattern
    ignore_parens = keyword_set(ignore_parens)
    if n_elements(replace_parens) gt 0 $
        then ignore_parens = 1 $
        else replace_parens = ''
    
    ;Get all known tokens
    allTokens = MrTokens()
    allTokens = strjoin(allTokens)

    ;Find all of the tokens in the file pattern
    count     = 0L
    curPos    = 0
    len       = strlen(_pattern)
    positions = lonarr(len)
    tokens    = strarr(len)
    while curPos lt len do begin
        pre  = strmid(_pattern, 0, curPos)
        post = strmid(_pattern, curPos)
        
        ;Get the position and length of the token. This has to be done each time
        ;because the length of the regex string changes with each iteration.
        ;   - Avoid "\%"
        ;   - If "%" appears at the beginning of the string, get the character that follows
        ;   - If not, search for the first "%" not preceded by "\"
        if strmid(post, 0, 1) eq '%' then begin
            theToken = stregex(post, '%([' + allTokens + '])', /EXTRACT, /SUBEXP)
            pos      = stregex(post, '%([' + allTokens + '])', /SUBEXP)
        endif else begin
            theToken = stregex(post, '[^\]%([' + allTokens + '])', /EXTRACT, /SUBEXP)
            pos      = stregex(post, '[^\]%([' + allTokens + '])', /SUBEXP)
        endelse
        pos = pos[1]
        theToken = theToken[1]

        ;Get the parts of the string surrounding the token
        before_token = pre + strmid(post, 0, pos-1)
        after_token  = strmid(post, pos+1)

        ;Which token?
        case theToken of
            ;Open paren
            '(': begin
                ;Find closure
                closePos = strpos(after_token, '%)')
                if closePos eq -1 then message, 'Token "%(" must eventually be closed by "%)".'
            
                ;Count the pair?
                if ignore_parens then begin
                    _pattern = before_token + replace_parens + strmid(after_token, closePos+2)
                    len = strlen(_pattern)
                endif else begin
                    positions[count:count+1] = [curPos + pos - 1, curPos + pos + closePos + 1]
                    tokens[count:count+1]    = [theToken, ')']
                    count += 2
                endelse
            
                ;Move to next interval
                curPos = strlen(before_token) + 2 + closePos + 2
            endcase
            
            ;Close Paren
            ')': message, 'Token "%)" found without preceding "%(".'
            
            ;No tokens remain
            '': curPos = len
            
            ;Any other valid token
            else: begin
                tokens[count]    = theToken
                positions[count] = curPos + pos - 1
                count           += 1
                curPos           = strlen(before_token) + 1
            endcase
        endcase
    endwhile
    
    ;Trim results
    if arg_present(pattern_out) then pattern_out = temporary(_pattern)
    positions = positions[0:count-1]
    return, tokens[0:count-1]
end




;----------------------------------------------------------
; Main Level Example Program (.r MrTimeTokensToRegex) /////
;----------------------------------------------------------
;EXAMPLE 1
;   Extract tokens from a string.
testString  = 'rbsp%((a|b)%)_\%_magnetometer_%Y%M%d_%H%m%S_%(%D%z%o%).cdf'
tokens = MrTokens_Extract(testString, COUNT=nTokens)

print, '---------------------------------------------'
print, 'Test String: ', testString
print, '  nTokens:   ', strtrim(nTokens, 2)
print, '  tokens:    ', strjoin(tokens, ', ')


;EXAMPLE 2
;   Use the POSITIONS keyword to extract the tokens
testString  = 'rbsp%((a|b)%)_\%_magnetometer_%Y%M%d_%H%m%S_%(%D%z%o%).cdf'
tokens = MrTokens_Extract(testString, COUNT=nTokens, POSITIONS=positions)

print, ''
print, '---------------------------------------------'
print, 'Test String: ', testString
print, '  nTokens:   ', strtrim(nTokens, 2)
print, '  tokens:    ', strjoin(strmid(testString, positions, 2), ', ')


;EXAMPLE 3
;   Use the IGNORE_PARENS keyword
testString  = 'rbsp%((a|b)%)_\%_magnetometer_%Y%M%d_%H%m%S_%(%D%z%o%).cdf'
tokens = MrTokens_Extract(testString, COUNT=nTokens, REPLACE_PARENS='.*', OPATTERN=pOut)

print, ''
print, '---------------------------------------------'
print, 'Test String:   ', testString
print, '  nTokens:     ', strtrim(nTokens, 2)
print, '  tokens:      ', strjoin(tokens, ', ')
print, '  Pattern Out: ', pOut
end