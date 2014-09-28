; docformat = 'rst'
;
; NAME:
;       MrTimeZoneOffsetToName.pro
;
;*****************************************************************************************
;   Copyright (c) 2014, University of New Hampshire                                      ;
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
;   Convert an offset in hours from Grenich Mean Time to an abbreviated time code.
;
; :Examples:
;   Try the example program at the end of this document::
;       IDL> .r MrTimeZoneOffsetToName
;
; :Categories:
;   Data Utilities
;
; :Params:
;       OFFSET:         in, required, type=string
;                       The offset from Grenich Mean Time, starting with a "+" or "-'
;                           and followed by a number "0" through "12".
;
; :Keywords:
;       MILITARY:       in, optional, type=boolean, default=0
;                       If set, `ABBR` will be the military time code A-Z.
;
; :Returns:
;       ABBR:           The abbreviated time zone name corresponding to offset.
;
; :Uses:
;       MrTimeZoneNameToOffset.pro
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
;       05/10/2014  -   Written by Matthew Argall
;-
function MrTimeZoneOffsetToName, offset, $
COUNT=count, $
MILITARY=military
    compile_opt idl2
    on_error, 2
    
    military = keyword_set(military)
    
    ;Get the key from MrTimeZoneNameToOffset
    ;   [Military Letter, Abbreviated Time Zone Name, Offset, Time Zone Name]
    void = MrTimeZoneNameToOffset(KEY=key)
    
    ;Make sure the input offset is in the format '+hh'
    if min(strlen(offset) eq 3) ne 1 $
        then _offset = string(fix(offset), FORMAT='(i+03)') $
        else _offset = offset
    
    ;Alphabetize by offset
    iSort = sort(key[2,*])

    ;Find the offset
    iAbbr = value_locate(key[2,iSort], offset)
    iAbbr = iSort[iAbbr]
    
    ;Get the abbreviated name
    if military $
        then abbr = key[0,iAbbr] $
        else abbr = key[1,iAbbr]
    
    return, abbr
end



;--------------------------------------------------------------
;Main Level Example Program: IDL> .r MrTimeZoneOffsetToName \\\
;--------------------------------------------------------------
;EXAMPLE 1
;   Create an array of all offsets, then convert them to time zone names
offsets = string(indgen(25)-12, FORMAT='(i+03)')
abbr    = MrTimeZoneOffsetToName(offsets)

;Print the results
print, '-----------------------------------------------'
print, 'Offset -> Abbreviated Time Zone:'
for i = 0, n_elements(offsets) - 1 do $
    print, FORMAT='(%"  %s  %s")', offsets[i], abbr[i]
print, ''


;EXAMPLE 2
;   Create an array of all offsets, then convert them to military time zone names
offsets = string(indgen(25)-12, FORMAT='(i+03)')
abbr    = MrTimeZoneOffsetToName(offsets, /MILITARY)

;Print the results
print, '-----------------------------------------------'
print, 'Offset -> Military Time Zone:'
for i = 0, n_elements(offsets) - 1 do $
    print, FORMAT='(%"  %s  %s")', offsets[i], abbr[i]
print, ''


end

