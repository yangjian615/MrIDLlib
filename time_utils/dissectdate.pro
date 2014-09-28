; docformat = 'rst'
;
; NAME:
;       DISSECTDATE
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
;       Break a date and time string of the form 'YYYY-MM-DD' up into year, month, day.
;       May contain 1-character non-digit delimeters.
;
;       Date can also be of numeric type, e.g., 20050125. Data is returned in the same'
;       type as DATE unless TYPE is specified.
;
; :Categories:
;       Utility, Time Conversion
;
; :Params:
;       DATE:       in, required, type=strarr/numeric array
;                   The date to be dissected.
;       YEAR:       out, optional, type=string/strarr
;                   The year of the `DATE` string
;       MONTH:      out, optional, type=string/strarr
;                   The month of the `DATE` string
;       DAY:        out, optional, type=string/strarr
;                   The day of the `DATE` string
;
; :Keywords:
;       TYPE:       in, optional, type=Boolean, default=`SIZE(DATE, /TYPE)`
;                   IDL type code to assign to the outputs.
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
; :Copyright:
;       Matthew Argall 2012
;
; :History:
;   Modification History::
;       Written by:     Matthew Argall 11 November 2012
;       2013-01-14  -   Date converted to desired TYPE properly. MRA.
;       2013-10-23  -   If DATE was a column, StRegEx was returning a 3D array. Simplified
;                           things by using the EXTRACT keyword to StRegEx. - MRA
;       2014-04-03  -   Return scalars if a scalar is given. - MRA
;-
pro dissectDate, date, year, month, day, $
TYPE=type
    compile_opt strictarr
    on_error, 2

    ;If a type was not given, then make it the same type as DATE
    date_type = size(date, /TYPE)
    nDates = n_elements(date)
    if n_elements(type) eq 0 then type = date_type
    
;----------------------------------------------------
;CONVERT NUMERIC DATE ///////////////////////////////
;----------------------------------------------------
    
    ;if date is not a string, then math out the date and time
    if date_type ne 7 then begin
        year = fix(floor(date / 10000), TYPE=type)
        month = fix(floor((date mod 10000) / 100), TYPE=type)
        day = fix(floor(date mod 100), TYPE=type)
        
        ;convert to a string unless requested otherwise
        if type eq 7 then begin
            year = string(year, format='(i4.4)')
            month = string(month, format='(i02.2)')
            day = string(day, format='(i02.2)')
        endif
        
        return
    endif

;----------------------------------------------------
;CONVERT STRING DATE ////////////////////////////////
;----------------------------------------------------
    
    ;Get the position and length of the year, month, and day substrings within DATE.
    ;Allow for 1-character, non-digit delimeters
    if MrIsA(date, /COLUMN) $
        then subStrings = stregex(reform(date), '([0-9]{4})[^0-9]?([0-9]{2})[^0-9]?([0-9]{2})', /EXTRACT, /SUBEXP) $
        else subStrings = stregex(date, '([0-9]{4})[^0-9]?([0-9]{2})[^0-9]?([0-9]{2})', /EXTRACT, /SUBEXP)

    ;Extract the year, month, and date    
    year  = reform(subStrings[1,*])
    month = reform(subStrings[2,*])
    day   = reform(subStrings[3,*])
    
    ;Convert to numeric type if requested.
    if type ne 7 then begin
        year = fix(year, TYPE=type)
        month = fix(month, TYPE=type)
        day = fix(day, TYPE=type)
    endif
    
    ;Return scalars
    if nDates eq 1 then begin
        year  = year[0]
        month = month[0]
        day   = day[0]
    endif
end