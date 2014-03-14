; docformat = 'rst'
;
; NAME:
;   epochtickformat
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
; PURPOSE
;+
;   The purpose of this program is to convert the tick-lables of a variable with units of
;   CDF Epoch, Epoch16 or TT2000 to a string with format of hours-minutes-seconds
;   ('HH:MM:SS').
;
; :Categories:
;   Plot Utilities
;
; :Uses:
;   Uses the following external functions::
;       dissectDate
;       dissectTime
;       ssm_to_hms
;       dateGen
;       isMember
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
;       09/22/2013  -   Created by Matthew Argall
;       09/23/2013  -   Removed decimal seconds from time. - MRA
;-
function Label_Seconds_Apply_Format, date, value, format
    compile_opt strictarr
    on_error, 2

    ;Make a copy of the format string
    result = format
    
    ;Get the parts of the date and time as strings.
    dissectDate, date, year, month, day, TYPE=7
    dissectTime, value, hour, minute, second, milli, micro, nano, TYPE=7
    
    nTokens = 1
    while nTokens eq 1 do begin
        pos = stregex(result, '(%[MNDYZWAHIS%])', LEN=len, /SUBEXP)
        
        ;No token found?
        if pos[1] eq -1 then begin
            nTokens = 0
            token = ''
            
        ;Token found
        endif else begin
            token = strmid(result, pos[1]+1, 1)

            ;Get the parts of the result before and after the token
            sLen = strlen(result)
            before_token = strmid(result, 0, pos[1])
            after_token = strmid(result, pos[1]+2, sLen-pos[1]-1)
        endelse
        
        ;Replace the token by the proper value.
        case token of
            'M': result = before_token + monthname(month) + after_token
            'N': result = before_token + month + after_token
            'D': result = before_token + day + after_token
            'Y': result = before_token + year + after_token
            'Z': result = before_token + strmid(year, 2, 2) + after_token
            'W': 
            'A': begin
                if fix(hour) gt 12 then begin
                    AMPM = 'PM'
                    hour = string(fix(hour)-12, FORMAT='(i02)')
                endif else AMPM = 'AM'
                
                result = before_token + AMPM + after_token
            endcase
            'H': result = before_token + hour + after_token
            'I': result = before_token + minute + after_token
            'S': begin
                ;See if %n was given next. Append it before seconds so that POS does not
                ;change.
                if strmid(result, pos[1]+2, 2) eq '%n' then begin
                    ;Extract the 'n'
                    numberChar = strmid(result, pos[1]+3, 1)
                    number = strpos('0123456789', numberPos)
                    if number le 0 $
                        then decimals = '' $
                        else decimals = '.' + strmid(milli+micro+nano, 0, number)
                    
                    ;Cut out the %n
                    result = strmid(result, 0, pos[1]+2) + strmid(result, pos[1]+4, sLen-pos[1]-4)
                endif else decimal = ''
                
                ;Put the seconds and decimals in.
                result = before_token + second + decimal + after_token
            endcase
            '%': result = before_token + '%' + after_token
            '': ;Do nothing
            else: message, 'Unknown format code: "%' + token + '"'
        endcase
    endwhile
    
    return, result
end

;-
;   The purpose of this program is to convert the tick-lables of a variable with units of
;   CDF Epoch, Epoch16 or TT2000 to a string with format of hours-minutes-seconds
;   ('HH:MM:SS').
;
;   To use, call LABEL_SECONDS with any of the keywords before creating the plot. This
;   step is necessary to initialize the LABEL_SECONDS_COMMON common block. Then when
;   making the plot, set XTICKFORMAT=['label_seconds', 'label_seconds'] and
;   XTICKUNITS=['Numeric', 'Numeric'].
;
;   The input parameters are not to be given except IDL's direct graphics procedures.
;
; :Params:
;       AX:             in, required, type=string
;                       the axis on which the labels will be placed
;       INDEX:          in, required, type=int
;                       Tick mark index
;       VALUES:         in, required, type=fltarr
;                       Data value at the tick mark
;       LEVEL:          in, optional, type=int
;                       The current axis level
;
; :Keywords:
;       FORMAT:         in, optional, type=strarr, default='%H:%I:%S'
;                       The format codes to be applied to `VALUE`. Options are::
;                           %M - Month name
;                           %N - Month number
;                           %D - Day of month
;                           %Y - Year
;                           %Z - 2-digit year
;                           %W - Day of week
;                           %H - Hours
;                           %I - Minutes
;                           %S - Seconds
;                           %n - If this immediately follow $S, then "n" is a digit 0-9
;                                specifyting the number of places after the decimal to keep
;                           %% - The "%" character
;                              - All other characters in the format string will be preserved.
;       DATE:           in, required, type=string
;                       When LABEL_SECONDS is initialized, a date must be supplied. `VALUE`
;                           is referenced from this date.
;       UNIQ_LABELS:    in, optional, type=boolean, default=0
;                       If set, only unique labels will be placed on the axis.
;       LABELS:         hidden, type=strarr
;                       A list of the labels already placed on the axis. Used internally
;                           if `UNIQ_LABELS` is set.
;-
function Label_Seconds, ax, index, value, level, $
FORMAT=format, $
DATE=date, $
UNIQ_LABELS=uniq_labels, $
LABELS=labels
    compile_opt strictarr
    on_error, 2
    
    common label_seconds_common, cDate, cFormat, cUniq_Labels, cLabels

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Reset the date
    if arg_present(date) or n_elements(date) gt 0 then void = temporary(cDate)
    if n_elements(date) eq 1 then cDate = date
    
    ;Keep only uniq labels?
    if n_elements(uniq_labels) eq 0 then begin
        if n_elements(cUniq_Labels) eq 0 then cUniq_Labels = keyword_set(uniq_labels)
    endif else cUniq_Labels = keyword_set(uniq_labels)
    
    ;Reset the format
    if arg_present(format) or n_elements(format) eq 0 then void = temporary(format)
    if n_params() lt 3 then if n_elements(format) eq 0 then format = '%H:%I:%S'
    if n_elements(format) gt 0 then cFormat = format
    
    ;RETURN -- If this the is set-up call
    if n_params() lt 3 then return, 0

;-----------------------------------------------------
;Create Axis Label \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if n_elements(level) eq 0 then level = 0
    nFormat = n_elements(cFormat)
    formatLevel = cFormat[level mod nFormat]

    ;If VALUE > 86400, then we need to add a day to DATE and subtract one from VALUE
    nFuture = fix(value / 86400)
    newDate = dateGen(cDate, nFuture+1)
    newDate = newDate[nFuture]
    newValue = ssm_to_hms(value - 86400*nFuture)

    result = Label_Seconds_Apply_Format(newDate, newValue, formatLevel)

    ;Clear the list of labels
    if index eq 0 and cUniq_Labels then begin
        void = temporary(cLabels)
        cLabels = result
    endif else if cUniq_Labels then begin
        if isMember(cLabels, result) $
            then result = '' $
            else cLabels = [cLabels, result]
    endif
    
    return, result[0]
end