; docformat = 'rst'
;
; NAME:
;       MrDayOfWeek
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
;   Compute the day of the week.
;
; :Examples:
;   Try running the main level program at the end of this document::
;       IDL> .r MrDayOfWeek
;
; :Params:
;       ARG0:           in, optional, type=long/double, default=SysTime(/JULIAN)
;                       Either the Julian date of the week day to be returned or the
;                           year in which `MONTH` and `DAY` reside.
;       MONTH:          in, optional, type=int/intarr/str/strarr
;                       Month of `YEAR` in which `DAY` resides. Can be month name.
;       DAY:            in, optional, type=int/intarr
;                       Day within `YEAR` and `MONTH` of the weekday to be computed.
;
; :Keywords:
;       ABBR:           in, optional, type=boolean, default=0
;                       If set, the abbreviated day of the week will be returned. Ignored if
;                           `NUMBER` is set.
;       MONTH_ABBR:     in, optional, type=boolean, default=0
;                       If set and `MONTH` is a name, then it will be taken to be the
;                           3-character abbreviated month name.
;       NUMBER:         in, optional, type=boolean, default=0
;                       If set, the number of days since Sunday will be returned.
;
; :Returns:
;       DAYOFWEEK:      Name of the week day.
;
; :Uses:
;   Uses the following external programs::
;       MonthNameToNumber.pro
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
;       2014-04-24  -   Written by Matthew Argall with suggestions from the `IDL news group
;                           <https://groups.google.com/forum/#!searchin/comp.lang.idl-pvwave/day$20of$20week/comp.lang.idl-pvwave/2CNe1-RwFIk/_tjugp6nfNgJ>`
;-
function MrDayOfWeek, arg0, month, day, $
ABBR=abbr, $
MONTH_ABBR=month_abbr, $
NUMBER=NUMBER
    compile_opt strictarr
    on_error, 2

    ;Defualts
    abbr       = keyword_set(abbr)
    month_abbr = keyword_set(month_abbr)
    number     = keyword_set(number)

    ;Get the Julian date
    case n_params() of
        3: begin
            year = arg0
            _month = size(month, /TNAME) eq 'STRING' ? MonthNameToNumber(month, ABBR=month_abbr) : month
            julian_date = julday(_month, day, year)
        endcase
        1: julian_date = arg0
        0: julian_date = systime(/JULIAN)
        else: message, 'Incorrect number of parameters.'
    endcase
    
    ;Julday of any old Sunday, say Julday(3,24,2002)
    jdate_sunday = 2452358L

    ;What day of the week is it? (note the use of round)
    dow = ( (round(julian_date) - jdate_sunday ) mod 7 )
    dow = (dow + 7) mod 7

    ;Name of the day
    if number EQ 0  then begin
        if abbr $
            then daysOfWeek = ['Sun', 'Mon', 'Tues', 'Wed', 'Thur', 'Fri', 'Sat'] $
            else daysOfWeek = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', $
                               'Thursday', 'Friday', 'Saturday']
        dow = daysOfWeek[dow]
    endif
    
    return, dow
end



;---------------------------------------------------
; Main Level Example Program (.r MrDayOfWeek) //////
;---------------------------------------------------
;EXAMPLE 1
;   The day I wrote this program is Tuesday, April 29, 2014
dow = MrDayOfWeek(2014, 4, 29)
print, '----------------------------------'
print, 'Date:          2014-04-29'
print, FORMAT='(%"Day of Week:   %s")', dow
print, ''


;EXAMPLE 2
;   Today's date and day of week
dow = MrDayOfWeek()
print, '----------------------------------'
print, FORMAT='(%"Date:          %s")', systime(/UTC)
print, FORMAT='(%"Day of Week:   %s")', dow
print, ''

;EXAMPLE 3
;   Array of dates
year  = [2014, 1984, 2009]
month = [  03,   06,   05]
day   = [  23,   30,   14]
dow = MrDayOfWeek(year, month, day)
print, '----------------------------------'
print, 'Dates:         [2014-03-23, 1984-06-30, 2009-05-14]'
print, FORMAT='(%"Day of Week:   [%s, %s, %s]")', dow
end