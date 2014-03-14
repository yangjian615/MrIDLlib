; docformat = 'rst'
;
; NAME:
;       EPOCH_TO_SSM
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
;       Convert EPOCH, EPOCH16 or TT2000 to seconds since midnight. Year, month, and day
;       information are discarded.
;
; :Categories:
;   Utility, Time Conversion
;
; :Params:
;       EPOCH_TIME:     in, required, type=EPOCH/EPOCH16/TT2000 [array]
;                       The epoch value to be converted to seconds since midnight.
;                           Year, month, and day information is discarded.
;
; :Returns:
;       SSM:            out, required, type=dblarr
;                       The time in seconds since midnight on the day of EPOCH_TIME
;
; :Uses:
;   Uses the following external programs::
;       MrCDF_Epoch_Type
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
;       Written by:     Matthew Argall 12 November 2012
;       2012-11-19:     Determine epoch type automatically; removed EPOCH16 and TT2000
;                           keywords.
;       2013-10-25  -   Added the RELATIVE keyword. Made output be double precision,
;                           simplified check of epoch type. - MRA
;-
function epoch_to_ssm, epoch_time
RELATIVE=relative
    compile_opt idl2
    on_error, 2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Default to calculating SSM relative to the earliest epoch time.
    if n_elements(relative) eq 0 then relative = 0
    
    ;determine the epoch type
    epoch_type = MrCDF_Epoch_Type(epoch_time[0])

;---------------------------------------------------------------------
;Relative to Minimum Epoch ///////////////////////////////////////////
;---------------------------------------------------------------------

    if relative then begin
        ;Epoch time of midnight (00:00:00) on the earliest day.
        epoch_0 = min(epoch_time)
        MrCDF_Epoch, epoch_0, year, month, day, /BREAKDOWN_EPOCH
        MrCDF_Epoch, epoch_0, year, month, day, EPOCH_TYPE=epoch_type, /COMPUTE_EPOCH
        
    
        ;CDF_EPOCH       -- # Double:   (Milliseconds from 0AD [leap year])
        ;CDF_EPOCH16     -- # DComplex: (Seconds from 0AD, Picoseconds within second)
        ;CDF_TIME_TT2000 -- # Long64:   (Nanoseconds since 2000 Jan 1, 12h TT)
        case epoch_type of
            'CDF_EPOCH': ssm = (epoch_time - epoch_0) * 1D-3
            
            'CDF_EPOCH16': begin
                ssm = epoch_time - epoch_0
                ssm = real_part(ssm) + imaginary(ssm)*1D-12
            endcase
            
            'CDF_TIME_TT2000': ssm = (epoch_time - epoch_0) * 1D-9
        endcase

        return, ssm
    endif

;---------------------------------------------------------------------
;Absolute ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;EPOCH16
    if epoch_type eq 'CDF_EPOCH16' then begin
        ;break down the epoch time
        cdf_epoch16, epoch_time, year, month, day, hour, minute, second, milli, micro, $
                                 nano, pico, /BREAKDOWN_EPOCH
        
        ;convert to seconds since midnight
        ssm = hour*3600D + minute*60D + second + $
              milli*1D-3 + micro*1D-6 + nano*1D-9 + pico*1D-12
    
    ;TT2000
    endif else if epoch_type eq 'CDF_TIME_TT2000' then begin
        ;break down the epoch time
        cdf_tt2000, epoch_time, year, month, day, hour, minute, second, milli, micro, $
                                nano, /BREAKDOWN_EPOCH
        
        ;convert to seconds since midnight
        ssm = hour*3600D + minute*60d + second + $
              milli*1D-3 + micro*1D-6 + nano*1D-9
    
    ;EPOCH16
    endif else begin
        ;convert from EPOCH time to seconds elapsed from beginning of the day
        cdf_epoch, epoch_time, year, month, day, hour, minute, second, milli, /BREAKDOWN_EPOCH
        ssm = hour*3600D + minute*60D + second + milli*1D-3
    endelse
  
	return, ssm
end
