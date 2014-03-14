; docformat = 'rst'
;
; NAME:
;       DISSECTTIME
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
;   Break a date and time string of the form 'HH:MM:SS.mmmuuunnnppp' up into hour,
;   minute, second, millisecond, microsecond, nanosecond, and picosecond.
;
;   May have 1-character non-digit delimeters and does not have to include decimal 
;   seconds.
;
; :Categories:
;   Utility, Time Conversion
;
; :Params:
;   TIME:   in, required, type=string/strarr
;           The time to break into aprts
;   HOUR:   out, optional, type=string/strarr
;           The hour of the `TIME` string
;   MINUTE: out, optional, type=string/strarr
;           The minute of the `TIME` string
;   SECOND: out, optional, type=string/strarr
;           The second of the `TIME` string
;   MILLI:  out, optional, type=string/strarr
;           The millisecond of the `TIME` string
;   MICRO:  out, optional, type=string/strarr
;           The microsecond of the `TIME` string
;   NANO:   out, optional, type=string/strarr
;           The nanosecond of the `TIME` string
;   PICO:   out, optional, type=string/strarr
;           The picosecond of the `TIME` string
;
; :Keywords:
;   TYPE: in, optional, type=Boolean, default=Same as `TIME`
;         IDL type code to assign to the outputs
;
; :Uses:
;   Uses the following extrnal programs::
;           MrIsA.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       Written by:     Matthew Argall 11 November 2012
;       2012-12-14:     Added ON_ERROR, 2. MRA.
;       2013-10-23  -   If DATE was a column, StRegEx was returning a 3D array. Simplified
;                           things by using the EXTRACT keyword to StRegEx. - MRA
;-
pro dissectTime, time, hour, minute, second, milli, micro, nano, pico, $
TYPE=type
    compile_opt idl2
    on_error, 2

    ;If a type was not given, then make it the same type as TIME
    time_type = size(time, /type)
    if n_elements(type) eq 0 then type = time_type
                        

;----------------------------------------------------
;CONVERT NUMERIC TIME ///////////////////////////////
;----------------------------------------------------

    ;if TIME is of numeric type, do math to extract the info
    if time_type ne 7 then begin
        hour = fix(floor(time_in / 10000), TYPE=type)
        minute = fix(floor((a mod 10000) / 100), TYPE=type)
        second = fix(floor(time_in / 100), TYPE=type)
        decimal = fix(time_in mod 1, TYPE=type)
        
        milli = fix(floor(decimal * 1e3), TYPE=type)
        micro = fix(floor((decimal * 1e6) mod 1e3), TYPE=type)
        nano = fix(floor((decimal * 1e9) mod 1e6), TYPE=type)
        pico = fix(floor((decimal * 1e12) mod 1e9), TYPE=type)
        
        ;conver to string unless specified otherwise
        if type eq 7 then begin
            hour = string(hour, '(i02.2)')
            minute = string(minute, '(i02.2)')
            second = string(second, 'i02.2)')
            milli = string(milli, '(i03.3)')
            micro = string(micro, '(i03.3)')
            nano = string(nano, '(i03.3)')
            pico = string(pico, '(i03.3)')
        endif
    endif

;----------------------------------------------------
;CONVERT STRING TIME ////////////////////////////////
;----------------------------------------------------
        
    ;create an editable version of TIME and get the number of elements
    ntimes = n_elements(time)
    
    ;Get the position and length of the hour, minute, second, and decimal second
    ;Allow for 1-character, non-digit delimeters
    if MrIsA(time, /COLUMN) $
        then subStrings = stregex(reform(time), '([0-9]{2})[^0-9]?([0-9]{2})[^0-9]?([0-9]{2})[.]?([0-9]*)', /EXTRACT, /SUBEXP) $
        else subStrings = stregex(time, '([0-9]{2})[^0-9]?([0-9]{2})[^0-9]?([0-9]{2})[.]?([0-9]*)', /EXTRACT, /SUBEXP)
    
    ;Extract the hour, minute, and second.
    hour    = reform(subStrings[1,*])
    minute  = reform(subStrings[2,*])
    second  = reform(subStrings[3,*])
    decimal = reform(subStrings[4,*])

    ;If there are times without a decimal, then add a pico-second decimal
    nonDec = where(strcmp(decimal, '') eq 1, count)
    if count gt 0 then decimal[nonDec] = '000000000000'
    
    ;Get the number of 0's to add to the decimal to convert it to picoseconds, then add
    ;that many 0's
    decPut = 12 - strlen(decimal)
    putThese = where(decPut ne 0, count)
    if count ne 0 then $
        for i = 0, count-1 do decimal[putThese[i]] += strjoin(replicate('0', decPut[putThese[i]]))
    
    ;Extract the number of milli-, micro-, nano- and picoseconds
    milli = strmid(decimal, 0, 3)
    micro = strmid(decimal, 3, 3)
    nano = strmid(decimal, 6, 3)
    pico = strmid(decimal, 9, 3)
    
    ;convert to numeric type if requested.
    if type ne 7 then begin
        hour = fix(hour, TYPE=type)
        minute = fix(minute, TYPE=type)
        second = fix(second, TYPE=type)
        milli = fix(milli, TYPE=type)
        micro = fix(micro, TYPE=type)
        nano = fix(nano, TYPE=type)
        pico = fix(pico, TYPE=type)
    end
    
end