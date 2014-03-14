; docformat = 'rst'
;
; NAME:
;       EPOCH_TO_ET
;
; PURPOSE:
;+
;       The purpose of this program is to convert CDF Epoch times to ephemeris time.
;
; :Categories:
;       Time Utility, CDF Utility, Time Conversion, SPICE Utility
;
; :Params:
;   T_EPOCH:        in, required, type=double, dcomplex, long64
;                   A[n array of] CDF times, either EPOCH, EPOCH16, or CDF_TIME_TT2000.
;
; :Keywords:
;   METHOD:         in, optional, type=int, default=1
;                   Method by which `T_EPOCH` is to be converted to `ET`::
;                       1. Epoch -> Date-Time string -> ET
;                       2. Epoch -> UTC -> ET (not very accurate. see below)
;
; :Returns:
;   ET:             Ephemeris time of each CDF Epoch value in `T_EPOCH`
;
; :Uses:
;   Uses the following external programs::
;       cspice_furnsh
;       cspice_unload
;       cspice_str2et
;       cspice_deltet
;       error_message.pro
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
;
;       Written by  -   Matthew Argall 12 February 2012
;       17/02/2013  -   Added the Epoch -> UTC -> ET method - MRA
;-
function epoch_to_et, t_epoch, $
METHOD = method, $
KERNEL = kernel
    compile_opt idl2

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        ;unload the kernel
        if n_elements(kernel) ne 0 then cspice_unload, kernel
        
        void = cgErrorMsg()
        return, !null
    endif

    ;Load the kernel if necessary.
    if n_elements(kernel) gt 0 then cspice_furnsh, kernel
    if n_elements(method) eq 0 then method = 1
    
    ;cpice_deltet requires an Nx1 vector, so if a 1xN vector was given,
    ;take the transpose.
    sz_epoch = size(t_epoch)
    case sz_epoch[0] of
        0: ;ok
        1: ;ok
        2: if sz_epoch[1] eq 1 $
                then t_epoch = transpose(t_epoch) $
                else message, 'T_EPOCH must be a scalar or an Nx1 array.'
        else: message, 'T_epoch must be a scalar or an Nx1 array.'
    endcase

;---------------------------------------------------------------------
;EPOCH -> UTC -> ET //////////////////////////////////////////////////
;---------------------------------------------------------------------
    if method eq 1 then begin
        ;Convert to UTC by subtracting the number of leap seconds and the difference
        ;beween TAI and TDT
        t_UTC = double(t_epoch)*1e-9 - 34.0 - 32.184
        
        ;Calculate the difference between UTC and ET
        cspice_deltet, t_UTC, 'UTC', t_deltaET
        
        ;Convert to ET
        et = t_UTC + t_deltaET

;---------------------------------------------------------------------
;EPOCH -> DATETIME -> ET /////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else if method eq 2 then begin
    ;
    ;CAUTION:
    ;   This mehtod produces jumps of +/- 1 second in ET times.
    ;

        ;Convert the epoch values to a string with format 
        ;'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp'
        epoch_string = epoch_to_datetime(t_epoch)
       
        ;Convert to ET
        cspice_str2et, epoch_string, et
    
    ;otherwise send an error message
    endif else begin
        message, 'Method must be {1 | 2}.'
    endelse

    
    ;Unload the kernel if necessary
    if n_elements(kernel) ne 0 then cspice_unload, kernel
    
    return, et
end