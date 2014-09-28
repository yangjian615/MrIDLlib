; docformat = 'rst'
;
; NAME:
;       HMS_TO_SSM
;
; PURPOSE:
;+
;       Convert from a string or numeric 'HHMMSS.mmmuuunnnppp' time value to seconds since
;       midnight.
;
; :Categories:
;   Utility, Time Conversion
;
; :Params:
;       HMS_TIME:           in, required, type=string/numeric [array]
;                           The time to be converted to SSM. If HMS_TIME is::
;                               1xN: HHMMSS.mmmuuunnnppp format (m-milli, u-micro, n-nano, p-pico).
;                               2-9xN: [hour, minute, second, milli, micro, nano, pico]
;                           If the leading dimension of `HMS_TIME` > 1, then the other
;                               inputs are ignored.
;       HOUR:               in, optional, type=int/intarr
;                           The hour of `HMS_TIME`
;       MINUTE:             in, optional, type=int/intarr
;                           The number of minutes to be converted.
;       SECOND:             in, optional, type=int/intarr/float/fltarr
;                           The number of seconds to be converted.
;       MILLI:              in, optional, type=int/intarr
;                           The number of milliseconds to be converted.
;       MICRO:              in, optional, type=int/intarr
;                           The number of microseconds to be converted
;       NANO:               in, optional, type=int/intarr
;                           The number of nanoseconds to be converted.
;       PICO:               in, optional, type=int/intarr
;                           The number of picoseconds to be converted.
;
; :Returns:
;       SSM_TIME:           out, type=double/dblarr
;                           The time in seconds since midnight
;
; :Author:
;    Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Copyright 2013 by Matthew Argall
;
; :History:
;   Modification History::
;
;       Written by:     Matthew Argall 08 March 2012
;       03/08/2012:     Added optional outputs hour, minute, second. MRA.
;       11/26/2012:     Removed BACKWORD keyword and created separate SSM_TO_HMS()
;                           function. HMS_TIME can be of string type. Math simplified - MRA
;       12/14/2012:     Added ON_ERROR, 2. MRA.
;       02/20/2013:     Allow for `HMS_TIME` to be given as a 3-9xN array. Changed
;                           parameter list to accept multiple inputs. Final computation
;                           is done in double precision. Do an in-depth check to see
;                           if an Nx1 array is really suppose to be 1xN (see below). - MRA
;       09/30/2013:     Return the HMS tims to their input array if an error occurs. 
;                           Also, transpose the output array upon exit, if necessary. - MRA
;       2014/03/03:     Return a scalar if only one time was given. - MRA
;       2014/07/19:     Return a row vector under all circumstances. - MRA
;-
function hms_to_ssm, hms_time_in, minute, second, milli, micro, nano, pico
    compile_opt idl2
    
    ;catch any errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if tf_trans eq 1 then hms_time_in = temporary(hms_time)
        void = cgErrorMsg()
        return, !null
    endif

;---------------------------------------------------------------------
;CHECK INPUTS ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    nTimes = n_elements(hms_time_in)
    
    ;Get the sizes of the dimensions
    hms_dims = size(hms_time_in, /DIMENSIONS)
    hms_type = size(hms_time_in, /TYPE)
    
    ;If HMS_TIME is the only parameter given and it has more than one column
    if n_params() eq 1 && hms_dims[0] ge 1 then begin
        ;Then we must check to see if it is an Nx1 ('HH:MM:SS') array of times or an
        ;MxN array of times (['HH', 'MM', SS']). If it is the former then one of the
        ;following will be true.
        ;   1. There are more than 7 columns (i.e. it is a column vector)
        ;   2. HMS_TIME is a string of length > 2 (i.e., not '0' - '24' -- it is "HH:MM:SS...").
        ;   3. HMS_TIME is a number > 24 (i.e. not 0-24 for the hour -- it is HHMMSS...).
        if (hms_dims[0] gt 7) || $
           (hms_type    eq 7 and strlen(hms_time_in[0]) gt 2) || $
           (hms_type    ne 7 and fix(hms_time_in[0], TYPE=3) gt 24) then begin
           
                ;if any of the above conditions are met, then we must take the transpose
                ;to make the array 1xN so as to fall within the proper condition below.
                hms_time = transpose(temporary(hms_time_in))
                tf_trans = 1
        endif
    endif 
    
    ;If none of the above was true, then copy over the input as is. It will be put back
    ;before returning.
    if n_elements(hms_time) eq 0 then begin
        hms_time = temporary(hms_time_in)
        tf_trans = 0
    endif

    ;Get info about HMS_TIME.
    hms_type = size(hms_time, /TYPE)
    hms_dims = size(hms_time, /DIMENSIONS)
    
    ;Check number of dimension and inputs here so that we do not have to use a "break"
    ;or an "else" statement in the switches below.
    if hms_dims[0] gt 7 then message, 'If HMS_TIME is an NxM array, ensure N <= 7.'
    if n_params() lt 1 || n_params() gt 7 then message, 'Incorrect number of arguments.'

    ;Set absent parameters to 0
    if n_elements(pico)   eq 0 and hms_dims[0] lt 7 then pico   = 0D
    if n_elements(nano)   eq 0 and hms_dims[0] lt 6 then nano   = 0D
    if n_elements(micro)  eq 0 and hms_dims[0] lt 5 then micro  = 0D
    if n_elements(milli)  eq 0 and hms_dims[0] lt 4 then milli  = 0D
    if n_elements(second) eq 0 and hms_dims[0] lt 3 then second = 0D
    if n_elements(minute) eq 0 and hms_dims[0] lt 2 then minute = 0D

;---------------------------------------------------------------------
;IF STRINGS WERE GIVEN ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if hms_type eq 7 then begin
    
    ;---------------------------------------------------------------------
    ;WERE MULTIPLE INPUTS GIVEN? /////////////////////////////////////////
    ;---------------------------------------------------------------------
        if n_params() gt 1 then begin
            ;Change the inputs from strings to integers
            switch n_params() of
                7: pico   = fix(pico)
                6: nano   = fix(nano)
                5: mico   = fix(micro)
                4: milli  = fix(milli)
                3: second = fix(second, TYPE=5)
                2: minute = fix(minute)
                1: hour   = fix(hms_time)
                else: message, 'Incorrect number of arguments.'
            endswitch
        
    ;---------------------------------------------------------------------
    ;OR WAS HMS_TIME GIVEN AS A MULTI-DIMENSIONAL ARRAY? /////////////////
    ;---------------------------------------------------------------------
        endif else if n_params() eq 1 and hms_dims[0] gt 1 then begin
            ;Change the inputs from strings to integers
            ;NOTE: adding an ELSE statement executed as part of the switch.
            switch hms_dims[0] of
                7: pico   = fix(reform(hms_time[6,*]))
                6: nano   = fix(reform(hms_time[5,*]))
                5: micro  = fix(reform(hms_time[4,*]))
                4: milli  = fix(reform(hms_time[3,*]))
                3: second = fix(reform(hms_time[2,*], TYPE=5))
                2: minute = fix(reform(hms_time[1,*]))
                1: hour   = fix(reform(hms_time[0,*]))
;                else: message, 'If HMS_TIME is an NxM array, ensure N <= 7. Now, N = ' + $
;                               strtrim(string(hms_dims[0]), 2)
            endswitch

    ;---------------------------------------------------------------------
    ;OR WAS HMS_TIME OF THE FORM 'HHMMSS.mmmuuunnnppp? ///////////////////
    ;---------------------------------------------------------------------
        endif else begin
            dissectTime, hms_time, hour, minute, second, milli, micro, nano, pico, TYPE=2
        endelse
        
;---------------------------------------------------------------------
;IF NUMBER WERE GIVEN ////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
    
    ;---------------------------------------------------------------------
    ;WAS HMS_TIME GIVEN AS A MULTI-DIMENSIONAL ARRAY? ////////////////////
    ;---------------------------------------------------------------------
        if n_params() eq 1 and hms_dims[0] gt 1 then begin
            ;Change the inputs from strings to integers
            ;NOTE: adding an ELSE statement executed as part of the switch.
            switch hms_dims[0] of
                7: pico   = reform(hms_time[6,*])
                6: nano   = reform(hms_time[5,*])
                5: micro  = reform(hms_time[4,*])
                4: milli  = reform(hms_time[3,*])
                3: second = reform(hms_time[2,*])
                2: minute = reform(hms_time[1,*])
                1: hour   = reform(hms_time[0,*])
;                else: message, 'If HMS_TIME is an NxM array, ensure N <= 7. Now, N = ' + $
;                               strtrim(string(hms_dims[0]), 2)
            endswitch

    ;---------------------------------------------------------------------
    ;WAS HMS_TIME A 1XN ARRAY OF NUMBERS? ////////////////////////////////
    ;---------------------------------------------------------------------
        endif else if n_params() eq 1 and hms_dims[0] le 1 then begin
            second = hms_time mod 100
            minute = fix((hms_time mod 1e4) / 1e2)
            hour   = fix((hms_time mod 1e7) / 1e4)
        endif
        
        ;The case of multiple inputs was taken care of already during the CHECK INPUTS
        ;phase when we set the absent parameters to 0. There is no need to manipulate them
        ;as there was in the case of strings (convert from string to integers)
    endelse
        
;---------------------------------------------------------------------
;CALCULATE THE NUMBER OF SECONDS SINCE MIDNIGHT //////////////////////
;---------------------------------------------------------------------
    ;Piece together the fractional second
    second = second + double(milli)*1e-3 + double(micro)*1e-6 + $
                      double(nano)*1e-9  + double(pico)*1e-12
    
    ;Calculate SSM
    t_ssm = hour*3600D + minute*60D + second

    ;replace the input data
    if tf_trans $
        then hms_time_in = transpose(temporary(hms_time)) $
        else hms_time_in = temporary(hms_time)

    ;Return the number of seconds since midnight.
    if nTimes eq 1 then t_ssm = t_ssm[0]
    return, t_ssm
end


;---------------------------------------------------------------------
;Main Level Example Program (IDL> .r hms_to_ssm)
;---------------------------------------------------------------------
;Create scalar times
time1 = '20:13:52.123456789'
time2 = 201352.123456789D
time3 = [20, 13, 52, 123, 456, 789]
time4 = ['20', '13', '52', '123', '456', '789']

;Create arrays of times
time5 = replicate('20:13:52.123456789', 4)
time6 = replicate(201352.123456789D, 4)
time7 = rebin([20, 13, 52, 123, 456, 789], 6, 4)
time8 = [[time4], [time4], [time4], [time4]]

;Compute the results for scalar times.
ssm_time1 = hms_to_ssm(time1)
ssm_time2 = hms_to_ssm(time2)
ssm_time3 = hms_to_ssm(time3)
ssm_time4 = hms_to_ssm(time4)

;Compute the results for arrays of times.
ssm_time5 = hms_to_ssm(time5)
ssm_time6 = hms_to_ssm(time6)
ssm_time7 = hms_to_ssm(time7)
ssm_time8 = hms_to_ssm(time8)

;print the results
print, ''
print, '-------------------------------------------------------------'

;scalar times
print, format='(%"%s has been used for all times computed")', time1
print, 'All are used as:'
print, '        result = hms_to_ssm(time#)'
help, time1, time2, time3, time4
print, format='(%"The result for time1: %f0")', ssm_time1
print, format='(%"The result for time2: %f0")', ssm_time2
print, format='(%"The result for time3: %f0")', ssm_time3
print, format='(%"The result for time4: %f0")', ssm_time4
print, ''
print, '-------------------------------------------------------------'
print, ''

;arrays of times
print, format='(%"%s has now been repeated four times in each of the following cases:")', time1
print, 'All are used as:'
print, '        result = hms_to_ssm(time#)'
help, time5, time6, time7, time8
print, format='(%"The result for time5: [%f0, %f0, %f0, %f0]")', ssm_time5
print, format='(%"The result for time6: [%f0, %f0, %f0, %f0]")', ssm_time6
print, format='(%"The result for time7: [%f0, %f0, %f0, %f0]")', ssm_time7
print, format='(%"The result for time8: [%f0, %f0, %f0, %f0]")', ssm_time8

end
