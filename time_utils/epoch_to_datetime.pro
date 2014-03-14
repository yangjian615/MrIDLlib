; docformat = 'rst'
;
; NAME:
;       EPOCH_TO_DATETIME
;
; PURPOSE:
;+
;       Convert EPOCH, EPOCH16 or TT2000 to [YYYYMMDD, HHMMSS.mmmuuunnnppp] values.
;
; :Categories:
;   Time Utility, CDF Utility, Time Conversion
;
; :Params:
;       EPOCH_TIME:         in, required, type=EPOCH/EPOCH16/TT2000 [array]
;                           The epoch value to be converted to seconds since midnight.
;                               Year, month, and day information is discarded.
;
; :Keywords:
;       DATEDELIM:          in, optional, type=string, default='-'
;                           If used with `TO_STRING`, it is the delimeter that separates
;       EPOCH:              in, optional, type=Int
;                           Returns `DATE_TIME` in the format {0 | 1 | 2 | 3}, according
;                               to the `cdf_parse_epoch`, `cdf_parse_epoch16`, and the
;                               `cdf_parse_tt2000` functions.
;       TIMEDELIM:          in, optional, type=string, default=':'
;                           If used with `TO_STRING`, it is the delimeter that separates
;                               the hour, minute, and second values (e.g. HH:MM:SS)
;       TO_STRING:          in, optional, type=Boolean, default=1
;                           Convert the output to a string.
;       SEPARATOR:          in, optional, type=string, default='T'
;                           If used with `TO_STRING`, it is the delimeter that separates
;                               the date and time (e.g. dateTtime)
;
; :Returns:
;       DATE_TIME:          A two column array. DATE_TIME[0,*] is the converted date
;                               and DATE_TIME[1,*] is the converted time
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2011
;
; :History:
;   Modification History::
;
;       Written by:     Matthew Argall 12 November 2012
;       11/19/2012:     Determine epoch type automatically; removed EPOCH16 and TT2000
;                           keywords. - MRA
;       02/12/2012:     Use `MrCDF_Epoch_Type.pro`, added SEPARATOR and EPOCH, changed
;                           default values to be DATEDELIM='-', TIMEDELIM=':', 
;                           TO_STRING=1 - MRA
;-
function epoch_to_datetime, epoch_time, $
DATEDELIM = datedelim, $
EPOCH = epoch, $
TIMEDELIM = timedelim, $
TO_STRING = to_string, $
SEPARATOR = separator
    compile_opt idl2
    on_error, 2

;---------------------------------------------------------------------
;SET SOME DEFAULTS AND DETERMINE EPOCH TYPE //////////////////////////
;---------------------------------------------------------------------
    npts = n_elements(epoch_time)
    
    ;Set defaults
    if n_elements(to_string) eq 0 then to_string = 1
        
    ;determine the epoch type
    epoch_type = MrCDF_Epoch_Type(epoch_time)

;---------------------------------------------------------------------
;PROCESS THE EPOCH KEYWORD ///////////////////////////////////////////
;---------------------------------------------------------------------

    ;Parse the values according to the format specified by EPOCH
    if n_elements(epoch) gt 0 then begin
        date_time = cdf_epoch_encode(epoch_time, EPOCH=epoch)
        return, date_time
    endif

;---------------------------------------------------------------------
;BREAK DOWN EPOCH TIMES //////////////////////////////////////////////
;---------------------------------------------------------------------
    case epoch_type of 
        'CDF_EPOCH': begin
            cdf_epoch, epoch_time, year, month, day, hour, minute, second, milli, $
                       /BREAKDOWN_EPOCH
            
            ;Set the micro-, nano-, and pico-second values to 000
            micro = intarr(npts)
            nano = intarr(npts)
            pico = intarr(npts)
        endcase
        
        'CDF_EPOCH16': cdf_epoch16, epoch_time, year, month, day, hour, minute, second, $
                                    milli, micro, nano, pico, /BREAKDOWN_EPOCH
                                    
        'CDF_TIME_TT2000': begin
            cdf_tt2000, epoch_time, year, month, day, hour, minute, second, $
                        milli, micro, nano, /BREAKDOWN_EPOCH
                        
            ;Set the pico-seconds to 000
            pico = intarr(npts)
        endcase
        
        else: message, 'Variable "epoch_time" is not a recognized epoch type.'
    endcase

;---------------------------------------------------------------------
;CONVERT TO STRING ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;convert to string if requested
    if keyword_set(to_string) then begin
        ;set the default delimeters if none were picked
        if n_elements(datedelim) eq 0 then datedelim = '-'
        if n_elements(timedelim) eq 0 then timedelim = ':'
        if n_elements(separator) eq 0 then separator = 'T'
        
        ;convert to string
        year = string(year, format='(i04.4)')
        month = string(month, format='(i02.2)')
        day = string(day, format='(i02.2)')
        hour = string(hour, format='(i02.2)')
        minute = string(minute, format='(i02.2)')
        second = string(second, format='(i02.2)')
        milli = string(milli, format='(i03.3)')
        micro = string(micro, format='(i03.3)')
        nano = string(nano, format='(i03.3)')
        pico = string(pico, format='(i03.3)')
        
        ;join the time and date parts
        date_time = year + datedelim + month + datedelim + day + separator + $
                    hour + timedelim + minute + timedelim + second + '.' + $
                    milli + micro + nano + pico

;---------------------------------------------------------------------
;CONVERT TO NUMBER ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        
    ;otherwise make numeric values
    endif else begin
        
        date = year*1e4 + month*1e2 + day
        time = double(hour)*1e4 + double(minute)*1e3 + double(second) + $
               double(milli)*1e-3 + double(micro)*1e-6 + double(nano)*1e-9 + $
               double(pico)*1e-12
    
        ;return the dates in the first column and the times in the second column
        date_time = transpose([[date], [time]])
    endelse
  
	return, date_time
end
