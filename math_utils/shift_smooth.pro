; docformat = 'rst'
;
; NAME:
;
;       SHIFT_SMOOTH
;
; PURPOSE:
;+
;       The purpose of this program is to take a boxcar sliding average of a data set,
;       allowing for a shift between each averaging interval.
;
;       NOTE: This is different from the SMOOTH function in that NAVG can be even, the
;             sliding average begins at index 0 and looks ahead NAVG-1 pts (i.e., not
;             centered), and the trailing edge of the array is truncated.
;
; :Categories:
;
;       Math Utility
;
; :Params:
;
;       DATA:               in, required, type=numeric array
;                           The data which for which the field-aligned coordinate system
;                               is to be calculated.
;       NAVG:               in, required, type=int/float
;                           The number of points to average when finding the background
;                               field strength. If `DT` is provided, then this is the
;                               number of seconds to average.
;       NSHIFT:             in, optional, type=int. default=1
;                           The number of points to shift ahead after each averaging
;                               interval.
;
; :Keywords:
;       CENTER_TIME:        in, optional, type=Boolean, default=1
;                           Indicate that the times returned in `TIME` are centered within
;                               their respective averaging interval. Set CENTER_TIME=0 to
;                               return the times at the beginning of each interval.
;       DIMENSION:          in, optional, type=int, default=1
;                           The dimension along which to average, ranging from 1 to 8.
;       DT:                 in, optional, type=float, default=1.0
;                           The time between data samples. If not provided and `TIME` is
;                               returned, unit spacing is assumed.
;       TIME:               out, type=dblarr
;                           The time associated with each averaging interval. TIME is in
;                               seconds and the first element of `DATA` corresponds to
;                               TIME=0
;                           
; :Returns:
;
;       AVG_DATA:           The smoothed data.
;
; :Uses:
;   Uses the following external programs::
;       nfft_intervals.pro
;       error_message.pro (Coyote Graphics)
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
;       Matthew Argall 2012
;
; :History::
;   Modification History::
;       04/10/2013  -   Written by Matthew Argall
;-
function shift_smooth, data, navg, nshift, $
CENTER_TIME = center_time, $
DIMENSION = dimension, $
DT = dt, $
TIME = time
    compile_opt idl2
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        ;Transpose the data back to how it was
        if n_elements(data) eq 0 then $
            data = transpose(temporary(data_temp), shift(dims, -thisDim))
            
        MrPrintF, 'LogErr'
    endif

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Create defaults.
    if n_elements(nshift) eq 0 then nshift = 1
    if n_elements(dimension) eq 0 then dimension = 1
    if arg_present(time) then keep_time = 1 else keep_time = 0
    if arg_present(avg_data) then keep_avg = 1 else keep_avg = 0
    if n_elements(dt) eq 0 then begin
        dt = 1.0
        navg_out = navg
    endif else navg_out = floor(navg * dt)
    
    ;Time stamp at the center of the packet or at the beginning?
    if keyword_set(center_time) then ct = 0.5 else ct = 0.0
    
    ;Make sure DIMENSION is between 1 and 7
    if dimension lt 1 or dimension gt 8 then $
        message, 'DIMENSION out of range: 1 <= DIMENSION <= 8.'
    
    ;Make sure NSHIFT is not 0.
    if nshift eq 0 then begin
        message, 'NSHIFT cannot equal 0. Switching to 1.', /INFORMATIONAL
        nshift = 1
    endif
     
;-----------------------------------------------------
;Juggle Dimensions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    data_temp = mrPut_Dimension_First(temporary(data), dimension)

;-----------------------------------------------------
;Calculate the Field-Aligned System \\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;How many intervals are to be taken?
    n_intervals = nfft_intervals(npts, navg, nshift)
   
    ;allocate the time array
    if keep_time then time = fltarr(n_intervals)
    
    ;allocate meme
    data_sz = size(data_temp, /STRUCTURE)
    if data_sz.n_dimensions le 1 $
        then avg_data = make_array(n_intervals, TYPE=data_sz.type) $
        else avg_data = make_array([n_intervals, (data_sz.dimensions)[1,*]], TYPE=data_sz.type)

    sindex = 0
    eindex = navg - 1
    ;for each fft interval...
    for i = 0, n_intervals-1 do begin
        
        ;Calculate the average field and normalize it.
        ;The desired dimension has been transposed to the leading dimension. IDL accepts
        ;a maximum of 8 dimensions. If some dimensions are empty, they will have no effect
        ;on the result.
        avg_data[i,*,*,*,*,*,*,*] = mean(data[sindex:eindex,*,*,*,*,*,*,*], DIMENSION=1)
        
        ;Keep the time?
        if keep_time then time[i] = (i + ct) * nshift * dt

        ;shift the fft by the desired number of points
        sindex += nshift
        eindex += nshift
    endfor

    ;Transpose the averaged dimension back to where it is suppose to be.
    if dimension ne 1 then begin
        data = mrPut_Dimension_First(temporary(data_temp), dimension)
        avg_data = mrPut_Dimension_First(avg_data, dimension)
    endif
    
    return, avg_data
end



;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Create a vector and smooth it by 2 points
vector = [0.415999, 0.091965, 0.756410, 0.529700, 0.930436, 0.383502, 0.653919, 0.066842, 0.722660, 0.671149]
svector = smooth(vector, 3)

;Create a vector array and smooth the second dimension
vecarr = transpose(rebin(vector, 10, 2))
svecarr = shift_smooth(vecarr, 3, 1, DIMENSION=2)

;Using the same vector array, smooth the second dimension, shift 2 points
ssvecarr = shift_smooth(vecarr, 3, 2, DIMENSION=2)


;Print the results
print, '-------------------------------------------------------'
print, 'The original vector and the result of a 2-point SMOOTH.'
print, FORMAT='(%"vector   = [%f, %f, %f, %f, %f, %f, %f, %f, %f, %f]")', vector
print, ''
print, FORMAT='(%"SMOOTHed = [%f, %f, %f, %f, %f, %f, %f, %f, %f, %f]")', svector
print, ''

print, '-------------------------------------------------------'
print, 'A vector array and the results of SHIFT_SMOOTH(NAVG=2, NSHIFT=1, DIMENSION=2)'
print, FORMAT='(%"vector           = [%f, %f, %f, %f, %f, %f, %f, %f, %f, %f]")', vecarr[0,*]
print, FORMAT='(%"                   [%f, %f, %f, %f, %f, %f, %f, %f, %f, %f]")', vecarr[1,*]
print, ''
print, FORMAT='(%"SHIFT_SMOOTHed   = [%f, %f, %f, %f, %f, %f, %f, %f, %f]")', svecarr[0,*]
print, FORMAT='(%"                   [%f, %f, %f, %f, %f, %f, %f, %f, %f]")', svecarr[1,*]
print, ''

print, '-------------------------------------------------------'
print, 'The same vector array and the results of SHIFT_SMOOTH(NAVG=2, NSHIFT=2, DIMENSION=2)'
print, FORMAT='(%"SHIFT_SMOOTHed   = [%f, %f, %f, %f, %f]")', ssvecarr[0,*]
print, FORMAT='(%"                   [%f, %f, %f, %f, %f]")', ssvecarr[1,*]

end