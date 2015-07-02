; docformat = 'rst'
;
; NAME:
;       MrTokens_ToRegex
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
;   LIST OF TOKENS::
;       %Y      -   Four-digit year: 2012, 2013, etc.
;       %y      -   Two-digit year: 60-59
;       %M      -   Two-digit month: 01-12
;       %C      -   Calendar month: January, Feburary, etc.
;       %c      -   Abbreviated calendar month: Jan, Feb, etc.
;       %d      -   Day of month: 01-31
;       %D      -   Day of year: 000-366
;       %W      -   Week day: Monday, Tuesday, etc.
;       %w      -   Abbreviated week day: Mon, Tue, etc.
;       %H      -   Hour on a 24-hour clock: 00-24
;       %h      -   Hour on a 12-hour clock: 01-12
;       %m      -   Minute: 00-59
;       %S      -   Seconds: 00-59
;       %f      -   Fraction of a second. Decimal point followed by any number of digits.
;       %z      -   Time Zone, abbreviated name (e.g. EST, CST)
;       %o      -   Offset from UTC: (+|-)hh[:mm] (e.g. +00, +00:00, -01:30)
;       %1      -   Milli-seconds: 000-999
;       %2      -   Micro-seconds: 000-999
;       %3      -   Nano-seconds: 000-999
;       %4      -   Pico-seconds: 000-999
;       %A      -   A.M. or P.M. on a 12-hour clock
;       %?      -   A single, unknown character
;       \%      -   The "%" character
;       %(      -   Text is copied verbatim from "%(" until "%)"
;       %)      -   Text is copied verbatim from "%(" until "%)"
;
;   EXTRA::
;       *       -   Replaced with '.*'
;       .       -   Replaced with '\.'
;
;   NOTES::
;       "%o"    Is too liberal. It will also match (+|-)00: and (+|-)00:0.
;       "%z"    1- and 2-character time zone abbreviations are hard-coded because there
;                   are few of them. If I missed one, then it will not be caught by the regex.
;       "%1-4"  Match 0 or 00 or 000.
;
; :Example:
;   See the example program at the end of this document::
;       IDL> .run MrTokens_ToRegex
;
; :Params:
;       PATTERN:        in, required, type=string
;                       Pattern containing tokens that identify different date and
;                           time elements within the string it represents.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of tokens found in `PATTERN`. If `GET_TOKENS` is set, then
;                           COUNT represents the number of tokens returned.
;       IGNORE_PARENS:  in, optional, type=boolean, default=0
;                       If set, the contents of "%(...%)" will be replaced with ".*". This
;                           is helpful when the contents contain subexpressions that would
;                           interfere with the extraction of date and time information.
;                           See the examples at the end.
;
; :Returns:
;       REGEX_STR:      A string that can be passed to the STREGEX function to extract
;                           time elements from the string that `PATTERN` represents.
;                           It may be necessary to set the /FOLD_CASE keyword in STREGEX.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/03/22  -   Written by Matthew Argall
;       2014/04/02  -   Added the GET_TOKENS keyword and the "%?" token. - MRA
;       2014/04/28  -   Added the "\%", "%f", "%(", and "%)" tokens.
;       2014/05/11  -   Added the "%z" and "%o" tokens.
;       2014/06/06  -   Search for "%)" is no longer greedy. - MRA
;       2014/06/23  -   Added the IGNORE_PARENS keyword. %M, %D, %d, %H, %h, and %S are
;                           are more exclusive (e.g. %M is now ([0-2][0-9]|3[0-1])
;                           instead of [0-9]{2}). - MRA
;       2014/06/29  -   Renamed from MrTimeTokensToRegex to MrTokens_ToRegex. Removed
;                           the GET_TOKENS keyword. Use the MrTokens function instead. - MRA
;       2014/06/30  -   Rewritten to use MrTokens_Extract. - MRA
;       2014/07/20  -   Made the %z token more exlusive. See notes above. - MRA
;       2015/06/05  -   Month (%M) and day (%d) can be expressed as 1-digit numbers. - MRA
;-
function MrTokens_ToRegex, pattern, $
COUNT=nTokens, $
IGNORE_PARENS=ignore_parens
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    ignore_parens = keyword_set(ignore_parens)
    
    ;Create an editable copy
    ;   - Replace "." with "\."
    ;   - Replace "*" with ".*"
    ;   - Must do before extracting tokens (so that positions are correct)!
    _pattern = pattern
    if strpos(_pattern, '.') ne -1 then _pattern = strjoin(strsplit(_pattern, '.', /EXTRACT), '\.')
    if strpos(_pattern, '*') ne -1 then _pattern = strjoin(strsplit(_pattern, '*', /EXTRACT), '.*')
    
    ;Extract the tokens
    tokens = MrTokens_Extract(_pattern, COUNT=nTokens, POSITIONS=positions)

    ;Step through each token
    regex  = ''
    curPos = 0
    for i = 0, nTokens - 1 do begin
        ;Match token to regular expression.
        case tokens[i] of
            'Y': regex_str = '([0-9]{4})'         
            'y': regex_str = '([0-9]{2})'         
            'M': regex_str = '(0?[0-9]|1[0-2])'    
            'C': regex_str = '(January|February|March|April|May|June|' + $
                              'July|August|September|October|November|December)'
            'c': regex_str = '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)'
            'd': regex_str = '(0?[0-9]|[1-2][0-9]|3[0-1])'
            'D': regex_str = '([0-2][0-9]{2}|3[0-5][0-9]|36[0-6])'
            'W': regex_str = '(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)'
            'w': regex_str = '(Sun|Mon|Tue|Wed|Thu|Fri|Sat)'
            'H': regex_str = '([0-1][0-9]|2[0-4])'
            'h': regex_str = '(0[0-9]|1[0-2])'    
            'm': regex_str = '([0-5][0-9])'       
            'S': regex_str = '([0-5][0-9]|60)'       
            'f': regex_str = '\.([0-9]+)'         
            '1': regex_str = '([0-9][0-9]?[0-9]?)'
            '2': regex_str = '([0-9][0-9]?[0-9]?)'
            '3': regex_str = '([0-9][0-9]?[0-9]?)'
            '4': regex_str = '([0-9][0-9]?[0-9]?)'
            'A': regex_str = '(AM|PM)'            
            'z': regex_str = '(Z|AT|BT|NT|UT|[A-Z]{2}[0-9]|[A-Z]{3}|[A-Z]{4})'  ;'([A-Z][A-Z]?[A-Z0-9]?[A-Z0-9]?)'
            'o': regex_str = '([+-]?[0-9]{2}:?[0-9]?[0-9]?)'
            '?': regex_str = '?'                  
            '(': begin
                ;Use the contents or ".*"?
                if ignore_parens then begin
                    regex_str = '.*'
                    nTokens  -= 2
                endif else begin
                    regex_str = strmid(_pattern, positions[i]+2, positions[i+1]-positions[i]-2)
                endelse
            endcase
            
            ;Unknown token
            else: begin
                message, 'Token "' + tokens[i] + '" not recognized. Replacing with ".*".', /INFORMATIONAL
                regex_str = '.*'
            endcase
        endcase
        
        ;Piece together the regular expression
        ;   - Take anything between the previous token and the current token
        regex += strmid(_pattern, curPos, positions[i]-curPos) + regex_str

        ;Skip over current token
        ;   - Must skip over characters between the previous token and current token
        curPos += 2 + positions[i] - curPos
        
        ;Skip over ')'
        if tokens[i] eq '(' then begin
            i += 1
            if i lt nTokens-1 then curPos = positions[i]+2
        endif
    endfor

    ;Add any characters after the last token.
    regex += strmid(_pattern, positions[i-1]+2)

    ;Search for "\%" and replace with "%"
    if strpos(regex, '\%') ne -1 $
        then regex = strjoin(strsplit(regex, '\\%', /REGEX, /EXTRACT), '%')
        
    return, regex
end


;----------------------------------------------------------
; Main Level Example Program (.r MrTimeTokensToRegex) /////
;----------------------------------------------------------
;EXAMPLE 1
;   Parse a time string
timeString  = '2014-03-21 20:02:23'
tokenString = '%Y-%M-%d %H:%m:%S'
regexString = MrTokens_ToRegex(tokenString)
confirm     = stregex(timeString, regexString, /BOOLEAN)
parsed      = stregex(timeString, regexString, /SUBEXP, /EXTRACT)

;Print results
print, '----------------------------------------------------------'
print, 'Parse the time:   ' + timeString
print, '  Token String:   ' + tokenString
print, '  Regex Pattern:  ' + regexString
print, '  Passed Test?    ' + (confirm ? 'Yes' : 'No')
print, '  Parsed:         ' + "['" + strjoin(parsed, "', '"), "']"
print, ''


;EXAMPLE 2
;   Parse a time string with custom precision, with some "%" in there for some reason.
timeString  = '2014-03-21%20:02:23.12345678900987654321'
tokenString = '%Y-%M-%d\%%H:%m:%S%f'
regexString = MrTokens_ToRegex(tokenString)
confirm     = stregex(timeString, regexString, /BOOLEAN)
parsed      = stregex(timeString, regexString, /SUBEXP, /EXTRACT)

;Print results
print, '----------------------------------------------------------'
print, 'Parse the time:   ' + timeString
print, '  Token String:   ' + tokenString
print, '  Regex Pattern:  ' + regexString
print, '  Passed Test?    ' + (confirm ? 'Yes' : 'No')
print, '  Parsed:         ' + "['" + strjoin(parsed, "', '"), "']"
print, ''

;EXAMPLE 3
;   Parse a time string with an offset from UTC
timeString  = 'Sun, 11 May 2014 14:31:14 +0000'
tokenString = '%w, %d %c %Y %H:%m:%S %o'
regexString = MrTokens_ToRegex(tokenString)
confirm     = stregex(timeString, regexString, /BOOLEAN)
parsed      = stregex(timeString, regexString, /SUBEXP, /EXTRACT)

;Print results
print, '----------------------------------------------------------'
print, 'Parse the time:   ' + timeString
print, '  Token String:   ' + tokenString
print, '  Regex Pattern:  ' + regexString
print, '  Passed Test?    ' + (confirm ? 'Yes' : 'No')
print, '  Parsed:         ' + "['" + strjoin(parsed, "', '"), "']"
print, ''

;EXAMPLE 3
;   Parse a time string with a time zone
timeString  = 'Sun, 11 May 2014 14:31:14 GMT'
tokenString = '%w, %d %c %Y %H:%m:%S %z'
regexString = MrTokens_ToRegex(tokenString)
confirm     = stregex(timeString, regexString, /BOOLEAN)
parsed      = stregex(timeString, regexString, /SUBEXP, /EXTRACT)

;Print results
print, '----------------------------------------------------------'
print, 'Parse the time:   ' + timeString
print, '  Token String:   ' + tokenString
print, '  Regex Pattern:  ' + regexString
print, '  Passed Test?    ' + (confirm ? 'Yes' : 'No')
print, '  Parsed:         ' + "['" + strjoin(parsed, "', '"), "']"
print, ''


;EXAMPLE 5
;   - Use the IGNORE_PARENS keyword to extract only the time information.
;   - Normally the (a|b) would add an extra element to the extracted regex results.
;   - Additional parentheses would add additional elements, making it difficult to know
;       which elements are related to time and which result from unrelated parentheses.
filenames = ['rbspa_def_MagEphem_TS04D_20140331_v1.1.1.h5', $
             'rbspb_def_MagEphem_TS04D_20140331_v1.1.1.h5']
pattern   = 'rbsp%((a|b)%)_def_MagEphem_TS04D_%Y%M%d_v*.h5'
regex     = MrTokens_ToRegex(pattern, /IGNORE_PARENS)
parts     = stregex(filenames, regex, /SUBEXP, /EXTRACT)
print, '----------------------------------------------------------'
print, 'Extract only the date information from two files:'
print, '  File 1:  ' + filenames[0]
print, '  File 2:  ' + filenames[1]
print, '  Pattern: ' + pattern
print, 'Results:'
print, FORMAT='(%"  Regex Output:  %s")',             regex
print, FORMAT='(%"  Regex Matches: [   %i,    %i]")', stregex(filenames, regex, /BOOLEAN)
print, FORMAT='(%"  Year:          [%s, %s]")',       parts[1,*]
print, FORMAT='(%"  Month:         [  %s,   %s]")',   parts[2,*]
print, FORMAT='(%"  Day:           [  %s,   %s]")',   parts[3,*]

end