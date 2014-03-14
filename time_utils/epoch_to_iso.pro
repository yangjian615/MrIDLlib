; docformat = 'rst'
;
; NAME:
;       EPOCH_TO_DATETIME
;
; PURPOSE:
;+
;       Convert EPOCH, EPOCH16 or TT2000 to ISO-8601 values.
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
;       EPOCH:          in, optional, type=Int
;                       Returns `ISOTIME` in the format {0 | 1 | 2 | 3}, according
;                           to the `cdf_parse_epoch`, `cdf_parse_epoch16`, and the
;                           `cdf_parse_tt2000` functions.
;       OFFSET:         in, optional, type=float 
;                       Offset in decimal hours from Coordinated Universal Time (UTC).
;                           Values range from -12 to 14.
;       UTC:            in, optional, type=boolean, default=1
;                       If set, times are considered to be in UTC already. If `OFFSET` is
;                           also provided, then times will be converted to UTC using the
;                           offset. If not set, but `OFFSET` is provided, `ISOTIME` will
;                           be returned with the offset appended. If `OFFSET`=0 or is not
;                           given, then UTC=1 is assumed.
;
; :Returns:
;       ISOTIME:            An array of iso-8601 times.
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
;       2013-10-24  -   Written by Matthew Argall
;       2014/02/03  -   Make use of MrCDF_Epoch. - MRA
;-
function epoch_to_iso, epoch_time, $
 EPOCH = epoch, $
 OFFSET=offset, $
 UTC=utc, $
_REF_EXTRA = extra
    compile_opt strictarr
    on_error, 2

;---------------------------------------------------------------------
;SET SOME DEFAULTS AND DETERMINE EPOCH TYPE //////////////////////////
;---------------------------------------------------------------------
    npts = n_elements(epoch_time)
        
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
    MrCDF_Epoch, epoch_time, year, month, day, hour, minute, second, $
                             milli, micro, nano, pico, /BREAKDOWN_EPOCH

;---------------------------------------------------------------------
;CONVERT TO ISO TIME /////////////////////////////////////////////////
;---------------------------------------------------------------------
    isoTime = MrTimeStamp(YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, $
                          SECOND=double(second) + milli*1d-3 + micro*1d-6 + nano*1d-9 + pico*1d-12, $
                         _STRICT_EXTRA=extra)
  
	return, isoTime
end
