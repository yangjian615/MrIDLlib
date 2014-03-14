; docformat = 'rst'
;
; NAME:
;       MRCDF_EPOCH_ENCODE
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
;       The purpose of this program is to create a wrapper for the `cdf_encode_epoch`, 
;       `cdf_encode_epoch16` and `cdf_encode_tt2000.pro` functions. For options not
;       available via the EPOCH keyword, see `epoch_to_datetime.pro`.
;
; :Categories:
;       Time Utility, CDF Utility, Time Conversion
;
; :Params:
;   T_EPOCH:            in, required, type=double, dcomplex, long64
;                       A[n array of] CDF times to be turned into strings. Accepted types
;                           are::
;                               EPOCH               -- Double
;                               EPOCH16             -- DComplex
;                               CDF_TIME_TT2000     -- Long64
;
; :Keywords:
;   EPOCH:              in. optional, type=int, default=0
;                       The format of the output::
;                           0 - dd-mmm-yyyy hh:mm:ss.ccc                -- CDF_EPOCH
;                             - dd-mmm-yyyy hh:mm:ss.ccc.uuu.nnn.ppp    -- CDF_EPOCH16
;                             - dd-mmm-yyyy hh:mm:ss.ccccccccc          -- CDF_TIME_TT2000
;                           1 - yyyymmdd.ttttttt                        -- CDF_EPOCH
;                             - yyyymmdd.ttttttttttttttt                -- CDF_EPOCH16
;                             - yyyymmdd.tttttttttt                     -- CDF_TIME_TT2000
;                           2 - yyyymmddss                              -- [ALL]
;                           3 - yyyy-mm-ddThh:mm:ss.cccZ                -- CDF_EPOCH
;                             - yyyy-mm-ddThh:mm:ss.ccc.uuu.nnn.pppZ    -- CDF_EPOCH16
;                             - yyyy-mm-ddThh:mm:ss.ccccccccc           -- CDF_TIME_TT2000
;
; :Returns:
;   ENCODED_STRING:     A date string representing each of `T_EPOCH`.
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
;	Modification History::
;       Written by  -   Matthew Argall 12 February 2012
;       2014/02/03  -   Renamed to MrCDF_Epoch_Encode from CDF_Epoch_Encode. - MRA
;-
function MrCDF_Epoch_Encode, t_epoch, $
EPOCH = epoch
    compile_opt strictarr
    on_error, 2
    
    ;Set default values. Make sure EPOCH is a valid option.
    if n_elements(epoch) eq 0 then epoch = 0
    if epoch lt 0 || epoch gt 3 then message, 'EPOCH must be {0 | 1 | 2 | 3}.'

    ;Determine the type of CDF epoch value given
    epoch_type = MrCDF_Epoch_Type(t_epoch)

    ;Encode epoch values
    case epoch_type of
        'CDF_EPOCH':       epoch_string = cdf_encode_epoch(t_epoch, EPOCH=epoch)
        'CDF_EPOCH16':     epoch_string = cdf_encode_epoch16(t_epoch, EPOCH=epoch)
        'CDF_TIME_TT2000': epoch_string = cdf_encode_tt2000(t_epoch, EPOCH=epoch)
    endcase
        
    return, epoch_string
end