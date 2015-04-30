; docformat = 'rst'
;
; NAME:
;       MrTokens.pro
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
;   Return the official token list. Definitions of tokens and their intended use are
;   below, followed by a select list of programs and utility routines that make use of
;   the tokens.
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
;   PROGRAMS & ROUTINES THAT USE TOKENS::
;       MrFileFinder__Define.pro
;       MrTimeParser.pro
;       MrTokens_ToRegex.pro
;       MrTokens_Extract.pro
;       MrFile_Search.pro
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
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2014/06/29  -   Written by Matthew Argall
;-
function MrTokens, pattern, $
COUNT=nTokens, $
IGNORE_PARENS=ignore_parens
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    get_tokens    = keyword_set(get_tokens)
    ignore_parens = keyword_set(ignore_parens)
    
    ;Tokens that are recognized
    tokens = ['Y', 'y', 'M', 'C', 'c', 'd', 'D', 'W', 'w', 'z', 'o', $
              'H', 'h', 'm', 'S', 'f', '1', '2', '3', '4', 'A', '?', '(', ')']

    ;Return the recognized tokens?
    nTokens = n_elements(tokens)
    
    ;Ignore parentheses?
    if ignore_parens then begin
        tokens = tokens[0:nTokens-3]
        nTokens -= 2
    endif
    
    return, tokens
end