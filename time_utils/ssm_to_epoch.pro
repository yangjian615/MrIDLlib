; docformat = 'rst'
;
; NAME:
;       SSM_TO_EPOCH
;
; PURPOSE:
;+
;       Convert time from a seconds since midnight value to the desired CDF_EPOCH type.
;
; :Params:
;       DATE:               in, required, type=string/strarr
;                           The properly-formatted date of the SSM_TIME value.
;                               (See CDF_PARSE_*)
;       SSM_TIME:           in, required, type=Numeric [array]
;                           The time in seconds since midnight to be converted.
;
; :Keywords:
;       DISSECT:            in, optional, type=Boolean, default=0
;                           Indicate that DATE is not properly formatted and that it
;                               should be dissected and rebuilt into the proper format.
;                               If this is selected, DATE can be of numeric type.
;       TO_EPOCH16:         in, optional, type=Boolean, default=0
;                           Convert to EPOCH16 instead of EPOCH
;       TO_TT2000:          in, optional, type=Boolean, default=0
;                           Convert to EPOCH_TT2000 instead of EPOCH
;
; :Returns:
;       EPOCH_TIME:       out, type=CDF_EPOCH, EPOCH16, or TT2000 [array]
;                           The converted cdf epoch value.
;
; :Author:
;   Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;		Durham, NH, 03824
;       mry27@wildcats.unh.edu
;
; :History:
;   Modification History::
;
;       Written by: Matthew Argall 12/11/2012
;-
function ssm_to_epoch, date, ssm_time, $
DISSECT=dissect, $
TO_EPOCH16=to_epoch16, $
TO_TT2000=to_tt2000
    compile_opt idl2
    on_error, 2

    ;if DISSECT is set, then DATE needs to be converted to 'YYYY-MM-DD' format
	if keyword_set(dissect) then begin
	    dissectDate, date, year, month, day, TYPE=7
	    date_string = year + '-' + month + '-' + day
	endif else date_string = date
    
    ;convert from SSM to 'HH:MM:SS.mmmuuunnnddd' format
    time_string = ssm_to_hms(ssm_time, /TO_STRING, DELIMETER=':')
    
    ;combine the date and time into a properly-formated epoch string
    ;'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp
    epoch_string = date_string + 'T' + time_string
    
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
  
 