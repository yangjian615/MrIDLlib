; docformat = 'rst'
;
; NAME:
;       MrTimeZoneNameToOffset.pro
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
;   Convert an abbreviated time zone name to an offset in hours from UTC.
;
; :Examples:
;   Show all of the time zones::
;       IDL> void = MrTimeZoneNameToOffset(/SHOW_TZ)
;
;
;   Print the offset of Eastern Standard Time (EST) from UTC::
;       IDL> print, MrTimeZoneNameToOffset('EST')
;               -5
;
;   Print the offset of 'PST', 'MST', 'CST' and 'EST' from UTC::
;       IDL> print, MrTimeZoneNameToOffset(['PST', 'MST', 'CST', 'EST'])
;               -8 -7 -6 -5
;
;   Return the time zone information::
;       IDL> void = MrTimeZoneNameToOffset(KEY=key)
;
;
; :Categories:
;   Time Utilities
;
; :Params:
;       ABBR:           The abbreviated time zone name corresponding to offset.
;
; :Keywords:
;       KEY:            out, optional, type=4xN strarr
;                       An array of military letter designations, time zone abbreviations,
;                           offsets, and time zone names. The same information is printed
;                           when the `SHOW_TZ` keyword is set.
;       MILITARY:       in, optional, type=boolean, default=0
;                       If set, `ABBR` will be the military time code A-Z.
;       SHOW_TZ:        in, optional, type=boolean, default=0
;                       If set, time zone information will be printed to the command window.
;
; :Returns:
;       RESULT:         in, required, type=string
;                       The offset from Grenich Mean Time, starting with a "+" or "-'
;                           and followed by a number "0" through "12".
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
;       2014/05/10  -   Written by Matthew Argall
;-
function MrTimeZoneNameToOffset, abbr, $
KEY=key, $
MILITARY=military, $
SHOW_TZ=show_tz
    compile_opt idl2
    on_error, 2
    
    show_tz  = keyword_set(show_tz)
    military = keyword_set(military)
    
    key = [['A', 'CET',  '+01', 'Central European Time'], $
           ['A', 'FWT',  '+01', 'French Winter Time'], $
           ['A', 'MET',  '+01', 'Middle Europe Time'], $
           ['A', 'MEWT', '+01', 'Middle Europe Winter Time'], $
           ['A', 'SWT',  '+01', 'Swedish Winter Time'], $
           ['B', 'EET',  '+02', 'Eastern Eropean Time, USSR Zone 1'], $
           ['C', 'BT',   '+03', 'Baghdad Time, USSR Zone 2'], $
           ['D', 'ZP4',  '+04', 'USSR Zone 3'], $
           ['E', 'ZP5',  '+05', 'USSR Zone 4'], $
           ['F', 'ZP6',  '+06', 'USSR Zone 5'], $
           ['G', 'CXT',  '+07', 'Christmas Island Time'], $
           ['H', 'CCT',  '+08', 'China Coast Time, USSR Zone 7'], $
           ['H', 'AWST', '+08', 'Australian Western Standard Time'], $
           ['H', 'WST',  '+08', 'Western Standard Time'], $
           ['I', 'JST',  '+09', 'Japan Standard Time, USSR Zone 8'], $
           ['K', 'EAST', '+10', 'East Australian Standard Time'], $
           ['K', '',     '+10', 'East Australian Time (Use EAST)'], $
           ['K', 'GST',  '+10', 'Guam Standaard Time, USSR Zone 9'], $
           ['L', '',     '+11', 'N/A'], $
           ['M', 'IDLE', '+12', 'International Date Line East'], $
           ['M', 'NZST', '+12', 'New Zealand Standard Time'], $
           ['M', 'NZT',  '+12', 'New Zealand Time'], $
           ['N', 'WAT',  '-01', 'West Africa Time'], $
           ['O', 'AT',   '-02', 'Azores Time'], $
           ['P', '',     '-03', 'N/A'], $
           ['Q', 'AST',  '-04', 'Atlantic Standard Time'], $
           ['R', 'EST',  '-05', 'Eastern Standard Time'], $
           ['S', 'CST',  '-06', 'Central Standard Time'], $
           ['T', 'MST',  '-07', 'Mountain Standard Time'], $
           ['U', 'PST',  '-08', 'Pacific Standard Time'], $
           ['V', 'AKST', '-09', 'Alaska Standard Time'], $
           ['V', 'YST',  '-09', 'Yukon Standard Time'], $
           ['W', 'HST',  '-10', 'Hawaii Standard Time'], $
           ['W', 'HAST', '-10', 'Hawaii-Aleutian Standard Time'], $
           ['W', 'AHST', '-10', 'Alaska-Hawaii Standard Time'], $
           ['W', 'CAT',  '-10', 'Central Alaska Time'], $
           ['X', 'NT',   '-11', 'Nome Time'], $
           ['Y', 'IDLW', '-12', 'International Date Line West'], $
           ['Z', 'GMT',  '+00', 'Greenwich Mean Time'], $
           ['Z', 'UT',   '+00', 'Universal Time'], $
           ['Z', 'UTC',  '+00', 'Universal Coordinated Time'], $
           ['Z', 'WET',  '+00', 'Western European Time']]
    
    ;Print all of the time zones?
    if show_tz then begin
        print, 'Military', 'Abbr', 'Offset', 'Name', $
                    FORMAT='(a8, 3x, a4, 3x, a6, 3x, a4)'
        print, key, FORMAT='(3x, a1, 7x, a-4, 5x, a3, 4x, a0)'
        return, ''
    endif
    
    ;Was something given?
    if n_elements(abbr) eq 0 then begin
        if arg_present(key) $
            then return, '' $
            else message, 'Usage: offset = MrTimeZoneNameToOffset(abbr)'
    endif

    ;Military
    if military then begin
        ;Search for the letter
        iOffset = value_locate(key[0,*], strupcase(abbr))
        result  = reform(key[2, iOffset])
        
    ;Standard Abbrs
    endif else begin
        ;Alphabetize by abbreviated name
        iSort   = sort(key[1,*])
        
        ;Search for the offsets
        iOffset = value_locate(key[1,iSort], strupcase(abbr))
        iOffset = iSort[iOffset]
        
        ;Pick the offsets
        result  = reform(key[2,iOffset])
    endelse

    if n_elements(abbr) eq 1 then result = result[0]
    return, result
end







