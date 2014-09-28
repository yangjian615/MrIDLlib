; docformat = 'rst'
;
; NAME:
;       MrTimeParser
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
;   Count the number of times a sub-string appears within a string.
;
;   NOTE:
;       The start of each consective substring begins at then end of the previously-
;       found substring. This may affect REGEX search patterns.
;
; :Examples:
;   Try the main-level example program at the end of this document.
;       IDL> .r MrStrCount
;
; :Params:
;       STR:            in, required, type=string
;                       String in which the substring is to be found.
;       SEARCH_STRING:  in, required, type=string
;                       Substring to be found within `STR`.
;
; :Keywords:
;       REGEX:          in, optional, type=boolean, default=0
;                       If set, `SEARCH_STRING` is a regular expression to be matched.
;
; :Returns:
;       COUNT:          Number of times `SEARCH_STRING` appears within `STR`.
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
;-
function MrStrCount, str, search_string, $
REGEX=regex
    compile_opt strictarr
    on_error, 2
    
    ;Check input
    if n_elements(str) ne 1 then message, 'STR must be a scalar string.'
    
    ;Defaults
    regex = keyword_set(regex)
    count = 0
    start = 0
    
    ;Loop through the string
    while start lt strlen(str) do begin
        pre  = strmid(str, 0, start)
        post = strmid(str, start)

        ;Find the substring
        if regex then begin
            start  = stregex(post, search_string, LENGTH=length)
        endif else begin
            start  = strpos(post, search_string)
            length = strlen(search_string)
        endelse 
        
        ;If no match was found, end the loop.
        if start eq -1 then begin
            start = strlen(str)
        endif else begin
            count += 1
            start += strlen(pre) + length
        endelse
    endwhile
    
    return, count
end



;---------------------------------------------------
; Main Level Example Program (.r MrStrCount) ///////
;---------------------------------------------------

;EXAMPLE 1
;   Find how many "%" characters are in a string
str           = 'rbspa_magnetometer_%Y%M%d.cdf'
search_string = '%'
count         = MrStrCount(str, search_string)
print, '------------------------------------------'
print, FORMAT='(%"String:          %s")', str
print, FORMAT='(%"Search String:   %s")', search_string
print, FORMAT='(%"Number Found:    %i")', count

;EXAMPLE 2
;   - Use a regular expression to search for a pattern
;   - Note that StrSplit(str, search_string, /REGEX, COUNT=count) does not return
;       the correct number of pieces in this case. The middle "%M" is ignored.
str           = 'rbspa_magnetometer_%Y%M%d.cdf'
search_string = '%[YMd]'
count         = MrStrCount(str, search_string, /REGEX)
print, '------------------------------------------'
print, FORMAT='(%"String:          %s")', str
print, FORMAT='(%"Regex Pattern:   %s")', search_string
print, FORMAT='(%"Number Found:    %i")', count

;EXAMPLE 3
;   Extract the any [YMd] that immediately follows the "%" character
str           = 'rbsp%((a|b)%)_magnetometer_%Y%M%d.cdf'
search_string = '%[YMd]'
count         = MrStrCount(str, search_string, /REGEX)
subexp        = stregex(str, strjoin(replicate('%([YMd]).*', count)), /SUBEXP, /EXTRACT)
print, '------------------------------------------'
print, FORMAT='(%"String:          %s")', str
print, FORMAT='(%"Regex Pattern:   %s")', search_string
print, FORMAT='(%"Number Found:    %i")', count
print, FORMAT='(%"Tokens Found:    [%s, %s, %s]")', subexp[1:*]

end