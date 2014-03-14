; docformat = 'rst'
;
; NAME:
;       DATATIME_TO_EPOCH
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
;       Convert properly formatted DATE (or DATE and TIME) strings to the desired EPOCH
;       type using the CDF_PARSE functions. Optionally, DATE (and TIME) can be broken into
;       pieces to be used with the CDF_EPOCH, CDF_EPOCH, or CDF_TT2000 programs.
;
;       With this program "Properly Formatted" is now uniquely defined for all CDF types.
;       This format is 'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp' where Y=year, M=month, D=day,
;       H=hour, M=minute, S=second, m=millisecond, u=microsecond
;
;       Recall that EPOCH and EPOCH16 parsers require the format::
;
;           '25-Dec-2001 23:59:59.000.000.000.000'
;
;       So, if the input date and time are of the "Proper" format, as defined above, use
;       keyword /DISSECT (and, optionally, /PARSE).
;
;       Year, month, day, hour, ..., can also be given, in which case the EPOCH_TIME is
;       computed from those, not from a parsed string.
;
; :Params:
;       EPOCH_STRING:       in, required, type=string/strarr
;                           A properly formatted epoch string. Choices are::
;                               CDF_EPOCH:          'DD-Mon-YYYY hh:mm:ss.ccc'
;                               CDF_EPOCH16:        'DD-Mon-YYYY hh:mm:ss.ccc.uuu.nnn.ppp'
;                               CDF_TIME_TT2000:    'yyyy-mm-ddThh:mm:ss.cccuuunnn' (iso)
;                           Note that all decimal places are required.
;
; :Keywords:
;       ISO:            in, optional, type=boolean, default=0
;                       If set, `EPOCH_STRING` is an ISO-8601 string. It will be broken
;                           down and formatted correctly.
;       TO_CDF_EPOCH:   in, optional, type=boolean
;                       If set, `EPOCH_STRING` will be parsed into "CDF_EPOCH" values.
;       TO_EPOCH16:     in, optional, type=boolean
;                       If set, `EPOCH_STRING` will be parsed into "CDF_EPOCH16" values.
;       TO_EPOCH16:     in, optional, type=boolean
;                       If set, `EPOCH_STRING` will be parsed into "CDF_TIME_TT2000" values.
;       EPOCH_TYPE:     in, optional, type=string
;                       An alternate way of specifying the `TO_CDF_EPOCH`, `TO_EPOCH16`
;                           and `TO_TT2000` keywords. Possible values include::
;                               "CDF_EPOCH"
;                               "CDF_EPOCH16"
;                               "CDF_TIME_TT2000"
;
; :Returns:
;       EPOCH_TIME:     out, type=dblarr/dcomplex/lon64arr
;                       The EPOCH, EPOCH16, or TT2000 times of the converted DATE-TIME.
;                           Output has same number of elements as DATE and is of the
;                           type: EPOCH=dblarr, EPOCH16=dcomplex_arr, TT2000=lon64arr
;
; :Uses:
;   Uses the following external programs::
;       MrTimeStampToValues.pro
;       MonthNumber2Name.pro
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
;       2014/03/08  -   Written by Matthew Argall
;-
function MrCDF_Epoch_Parse, epoch_string, $
ISO          = iso, $
TO_CDF_EPOCH = to_epoch, $
TO_EPOCH16   = to_epoch16, $
TO_TT2000    = to_tt2000, $
EPOCH_TYPE   = epoch_type
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    iso        = keyword_set(iso)
    to_epoch   = keyword_set(to_epoch)
    to_epoch16 = keyword_set(to_epoch16)
    to_tt2000  = keyword_set(to_tt2000)
    
    if n_elements(epoch_type) gt 0 then begin
        case epoch_type of
            'CDF_EPOCH':       to_epoch   = 1
            'CDF_EPOCH16':     to_epoch16 = 1
            'CDF_TIME_TT2000': to_tt2000  = 1
            else: message, 'Epoch type "' + epoch_type + '" not recognized.'
        endcase
    endif
    
    ;Guess the epoch type
    if (to_epoch + to_epoch16 + to_tt2000 eq 0) && iso eq 0 then begin
        to_epoch = stregex(epoch_string[0], '[0-9]{2}-[A-Z][a-z]{2}-[0-9]{4} ' + $
                                            '[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$', $
                                            /BOOLEAN)
        
        to_epoch16 = stregex(epoch_string[0], '[0-9]{2}-[A-Z][a-z]{2}-[0-9]{4} ' + $
                                              '[0-9]{2}:[0-9]{2}:[0-9]{2}\.' + $
                                              '[0-9]{3}\.[0-9]{3}\.[0-9]{3}\.[0-9]{3}', $
                                              /BOOLEAN)
        
        to_tt2000 = stregex(epoch_string[0], '[0-9]{4}-[0-9]{2}-[0-9]{2}T' + $
                                             '[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{9}', $
                                             /BOOLEAN)
    endif
    
    ;Only one can bese
    if to_epoch + to_epoch16 + to_tt2000 ne 1 then $
        message, 'Exactly one of TO_CDF_EPOCH, TO_EPOCH16, and TO_TT2000 can be set.'

;-----------------------------------------------------------------------------------------
;Iso Time? ///////////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    ;Was an iso-time given?
    If iso eq 1 then begin
        MrTimeStampToValues, epoch_string, YEAR=year, MONTH=month, DAY=day, HOUR=hour, $
                             MINUTE=minute, SECOND=second, TYPE='STRING'
        
        ;Break-up the seconds into milli-, micro-, nano-, and pico-seconds
        decimal = stregex(second, '([0-9]{2})\.?' + $           ;seconds
                                  '([0-9]?[0-9]?[0-9]?)' + $    ;milliseconds
                                  '([0-9]?[0-9]?[0-9]?)' + $    ;microseconds
                                  '([0-9]?[0-9]?[0-9]?)' + $    ;nanoseconds
                                  '([0-9]?[0-9]?[0-9]?)',  $    ;picoseconds
                                  /SUBEXP, /EXTRACT)
        
        ;Store the parts as 3-digit strings
        second = string(reform(decimal[1,*]), FORMAT='(i02)')
        milli  = string(reform(decimal[2,*]), FORMAT='(i03)')
        micro  = string(reform(decimal[3,*]), FORMAT='(i03)')
        nano   = string(reform(decimal[4,*]), FORMAT='(i03)')
        pico   = string(reform(decimal[5,*]), FORMAT='(i03)')

        ;Scalars
        if n_elements(epoch_string) eq 1 then begin
            second = second[0]
            milli  = milli[0]
            micro  = micro[0]
            nano   = nano[0]
            pico   = pico[0]
        endif
        
        ;Parse together
        case 1 of
            to_epoch:   epoch_out = day + '-' + MonthNumberToName(month, /ABBR) + '-' + year + $
                                    ' ' + hour + ':' + minute + ':' + second + '.' + milli
            to_epoch16: epoch_out = day + '-' + MonthNumberToName(month, /ABBR) + '-' + year + $
                                    ' ' + hour + ':' + minute + ':' + second + '.' + $
                                    milli + '.' + micro + '.' + nano + '.' + pico
            to_tt2000:  epoch_out = year + '-' + month + '-' + day + 'T' + hour + ':' + $
                                    minute + ':' + second + '.' + milli + micro + nano
        endcase
    endif else begin
        epoch_out = epoch_string
    endelse

;-----------------------------------------------------------------------------------------
;Parse ///////////////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    case 1 of
        to_epoch:   epoch_time = cdf_parse_epoch(temporary(epoch_out))
        to_epoch16: epoch_time = cdf_parse_epoch16(temporary(epoch_out))
        to_tt2000:  epoch_time = cdf_parse_tt2000(temporary(epoch_out))
    endcase
    
    ;Return the epoch time
    return, epoch_time
end





;-----------------------------------------------------------------------------------------
;%MAIN EXAMPLE DEMONSTRATION PROGRAM (.r datetime_to_epoch) //////////////////////////////
;-----------------------------------------------------------------------------------------
;Example with CDF_TIME_TT2000
test_string  = '2005-12-04T20:19:18.176214648' 
parsed_value = MrCDF_Epoch_Parse(test_string) 

;Print results
print, '---------------------------------------------------'
print, 'CDF_TIME_TT2000 Example'
print, FORMAT='(%"Test TT2000 String:  %s")', test_string
print, FORMAT='(%"String -> Epoch16:   %i")', parsed_value 
print, FORMAT='(%"Epoch16  -> String:  %s")', CDF_ENCODE_TT2000(parsed_value)
print, ''

;Example with CDF_EPOCH16
test_string = '04-Dec-2005 20:19:18.176.214.648.000' 
parsed_value = MrCDF_Epoch_Parse(test_string)

;Print results
print, '---------------------------------------------------'
print, 'CDF_EPOCH16 Example'
print, FORMAT='(%"Test Epoch16 String:  %s")',    test_string
print, FORMAT='(%"String  -> Epoch:     %i.%i")', real_part(parsed_value), imaginary(parsed_value)
print, FORMAT='(%"Epoch16 -> String:    %s")',    CDF_ENCODE_EPOCH16(parsed_value)
print, ''

;Example with CDF_EPOCH
test_string  = '04-Dec-2005 20:19:18.176' 
parsed_value = MrCDF_Epoch_Parse(test_string)

;Print results
print, '---------------------------------------------------'
print, 'CDF_EPOCH Example'
print, FORMAT='(%"Test Epoch String:  %s")', test_string
print, FORMAT='(%"String -> Epoch:    %i")', parsed_value 
print, FORMAT='(%"Epoch  -> String:   %s")', CDF_ENCODE_EPOCH(parsed_value)
print, ''


;Example with ISO-8601 String
test_string    = '2005-12-04T20:19:18.176214648Z' 
parsed_epoch   = MrCDF_Epoch_Parse(test_string, /ISO, /TO_CDF_EPOCH)
parsed_epoch16 = MrCDF_Epoch_Parse(test_string, /ISO, /TO_EPOCH16) 
parsed_tt2000  = MrCDF_Epoch_Parse(test_string, /ISO, /TO_TT2000) 

;Print results
print, '---------------------------------------------------'
print, 'ISO-8601 Example'
print, FORMAT='(%"Test ISO-8601 String:  %s")',    test_string
print, FORMAT='(%"String  -> Epoch:      %i")',    parsed_epoch
print, FORMAT='(%"String  -> Epoch16:    %i.%i")', real_part(parsed_epoch16), imaginary(parsed_epoch16)
print, FORMAT='(%"String  -> TT2000:     %i")',    parsed_tt2000
print, FORMAT='(%"Epoch   -> String:     %s")',    CDF_ENCODE_TT2000(parsed_epoch)
print, FORMAT='(%"Epoch16 -> String:     %s")',    CDF_ENCODE_TT2000(parsed_epoch16)
print, FORMAT='(%"TT2000  -> String:     %s")',    CDF_ENCODE_TT2000(parsed_tt2000)
print, ''

end
