; docformat = 'rst'
;
; NAME:
;       DISSECTDATETIME
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
;       Break a date and time string of the form 'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp' up
;       into year, month, day, hour, minute, second, millisecond, microsecond, nanosecond,
;       and picosecond.
;
;       Date and time substrings can have 1-character non-digit delimeters and the time
;       string does not have to include decimal seconds.
;
; :Categories:
;       Utility, Time Conversion
;
; :Params:
;       DATE_TIME:      in, required, type=string/strarr
;                       The date-time string to be broken down into parts. 
;       YEAR:           out, optional, type=string/strarr
;                       The year of the `DATE_TIME` string
;       MONTH:          out, optional, type=string/strarr
;                       The month of the `DATE_TIME` string
;       DAY:            out, optional, type=string/strarr
;                       The day of the `DATE_TIME` string
;       HOUR:           out, optional, type=string/strarr
;                       The hour of the `DATE_TIME` string
;       MINUTE:         out, optional, type=string/strarr
;                       The minute of the `DATE_TIME` string
;       SECOND:         out, optional, type=string/strarr
;                       The second of the `DATE_TIME` string
;       MILLI:          out, optional, type=string/strarr
;                       The millisecond of the `DATE_TIME` string
;       MICRO:          out, optional, type=string/strarr
;                       The microsecond of the `DATE_TIME` string
;       PICO:           out, optional, type=string/strarr
;                       The picosecond of the `DATE_TIME` string
;       NANO:           out, optional, type=string/strarr
;                       The nanosecond of the `DATE_TIME` string
;
; :Keywords:
;       TYPE:           in, optional, type=Boolean, default=Same type as `DATE_TIME`
;                       IDL type code to assign to the outputs
;       SEPARATE:       in, optional, type=boolean, default=0
;                       If set, do not dissect the date time. Instead, just separate the
;                           date from the time. In this case `YEAR` will be the date
;                           and `MONTH` will be the time. The `TYPE` keyword is ignored.
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
;       05/21/2013  -   Was accidentally looking for commas instead of decimal
;                           points. Fixed. - MRA
;       05/22/2013  -   Added the SEPARATE keyword. - MRA
;       10/23/2013  -   Save a few steps by using the EXTRACT keyword in StRegEx. DATE_TIME
;                           must be a row, otherwise StRegEx returns a 3D array. - MRA
;       2014/03/03  -   Return scalars if a single DATE_TIME was given. - MRA
;-
pro dissectDateTime, date_time, year, month, day, $
                                hour, minute, second, $
                                milli, micro, nano, pico, $
SEPARATE=separate, $
TYPE=type
    compile_opt strictarr
    on_error, 2
    
    nDateTime = n_elements(date_time)
    
    ;DATE_TIMEs must be strings
    if size(date_time, /TYPE) ne 7 then message, 'DATE_TIME must be a string/strarr.'

    ;Get the position and length of the date and time substrings within DATE_TIME.
    ;Allow for 1-character, non-digit delimeters
    if MrIsA(date_time, /COLUMN) $
        then subStrings = stregex(reform(date_time), '([0-9]{4}[^0-9]?[0-9]{2}[^0-9]?[0-9]{2})[^0-9]?' + $
                                                     '([0-9]{2}[^0-9]?[0-9]{2}[^0-9]?[0-9]{2}[.]?[0-9]*)', $
                                                     /EXTRACT, /SUBEXP) $
        else subStrings = stregex(date_time, '([0-9]{4}[^0-9]?[0-9]{2}[^0-9]?[0-9]{2})[^0-9]?' + $
                                             '([0-9]{2}[^0-9]?[0-9]{2}[^0-9]?[0-9]{2}[.]?[0-9]*)', $
                                             /EXTRACT, /SUBEXP)

    ;If all we are to do is separate the date from the time, then our work is done.
    if keyword_set(separate) then begin
        year = reform(subStrings[1,*])
        month = reform(subStrings[2,*])
        
        ;Return scalars
        if nDateTime eq 1 then begin
            year = year[0]
            month = month[0]
        endif
    endif else begin
        ;Dissect the date and time into their pieces
        dissectDate, subStrings[1,*], year, month, day, TYPE=type
        dissectTime, subStrings[2,*], hour, minute, second, milli, micro, nano, pico, TYPE=type
    endelse
end