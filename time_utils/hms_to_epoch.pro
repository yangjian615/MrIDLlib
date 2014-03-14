; docformat = 'rst'
;
; NAME:
;       HMS_TO_EPOCH
;
; PURPOSE:
;+
;       Convert time from a time-of-day value (HHMMSS.mmmuuunnnddd) to the desired 
;       CDF_EPOCH type. Choices are::
;           EPOCH
;           EPOCH16
;           TT2000
;
; :Categories:
;   Utility, Time Conversion
;
; :Params:
;       DATE:               in, required, type=string/strarr
;                           The properly-formatted epoch string. The default format
;                               accepted by CDF_PARSE* is YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp.
;                               If HMS_TIME is also given, then this only needs to be the
;                               date portion ('YYYY-MM-DD')
;       TIME:               in, optional, type=Numeric [array]
;                           The properly-formatted time string. The default format
;                               accepted by CDF_PARSE* is HH:MM:SS.mmmuuunnnppp.
;
; :Keywords:
;       DISSECT:            in, optional, type=Boolean, default=0.
;                           Indicate that DATE and/or TIME is not properly formatted and
;                               that they should be dissected and rebuilt into the proper
;                               format. If this is selected, both can be of numeric type.
;       TO_EPOCH16:         in, optional, type=Boolean, default=0.
;                           Convert to EPOCH16 instead of EPOCH
;       TO_TT2000:          in, optional, type=Boolean, default=0
;                           Convert to EPOCH_TT2000 instead of EPOCH
;
; :Returns:
;
;       EPOCH_TIME:         out, type=CDF_EPOCH, EPOCH16, or TT2000 [array]
;                           The converted cdf epoch value.
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
;
;       Written by:     Matthew Argall 12/11/2012
;-
function hms_to_epoch, date, time, $
DISSECT=dissect, $
TO_EPOCH16=to_epoch16, $
TO_TT2000=to_tt2000
    compile_opt idl2
    on_error, 2

    ;if DISSECT is set, then DATE needs to be converted to 'YYYY-MM-DD' format
	if keyword_set(dissect) then begin
	    if n_params() eq 1 then begin
	        dissectDateTime, date, year, month, day, hour, minute, second, milli, micro, $
	                         nano, pico, TYPE=7
	    endif else begin
            dissectDate, date, year, month, day, TYPE=7
            dissectTime, time, hour, minute, second, milli, micro, nano, pico, TYPE=7
        endelse
        
	    epoch_string = year + '-' + month + '-' + day + 'T' + hour + ':' + minute + ':' + $
	                   second + '.' + milli + micro + nano + pico
	                   
	;otherwise, just combine the date (and time)
	endif else begin
	    if n_params() eq 1 $
	        then epoch_string = date $
	        else epoch_string = date + 'T' + time
    endelse
    
    ;EPOCH16
    if keyword_set(to_epoch16) then begin
        epoch_time = cdf_parse_epoch16(epoch_string)
    
    ;TT2000
    endif else if keyword_set(to_tt2000) then begin
        epoch_time = cdf_parse_tt2000(epoch_string)
        
    ;EPOCH
    endif else begin
        epoch_time = cdf_parse_epoch(epoch_string)
    endelse
    
    return, epoch_time
end
  
 