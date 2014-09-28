; docformat = 'rst'
;
; NAME:
;       REPLACE_FILLVAL
;
; PURPOSE:
;+
;       The purpose of this program is to replace the fill value within an array of data.
;
; :Categories:
;       Data Utilities
;
; :Params:
;       DATA:               in, optional, type=any
;                           An array of values containing any number of instances of
;                               `FILLVAL`.
;       FILLVAL:            in, required, type=size(`DATA`\, /TYPE)
;                           The value within `DATA` to be replaced.
;       X:                  in, optional, type=any
;                           The abscissa values for `DATA`.
;                               X must have the same number of elements as `DATA`, 
;                               and the values must be strictly ascending or descending.
;                               `DATA` will be searched for `FILLVALUE`. A copy of `X` will
;                               be made (XOUT), then `DATA` and `X` will have the indices
;                               at which each fill value was found removed. The INTERPOL
;                               procedure will then be used in interpolate `DATA` to its
;                               original points, XOUT.
;
; :Keywords:
;       REPLACE_VALUE:      in, optional, type=size(`DATA`\, /TYPE), default=!values.f_nan
;                           The value that should replace `FILLVAL`
;       NO_COPY:            in, optional, type=Boolean, default=0
;                           Do not create a copy of the input data. The affect of this is
;                               to leave `DATA` undefined upon termination of the program.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by IDL's INTERPOL procedure is also
;                               accepted for keyword inheritance.
;
; :Returns:
;       REPLACED_DATA:      Same as `DATA`, but with all instances of `FILLVAL` either
;                               replaced by `REPLACE_VALUE` or interpolated over, depending
;                               on if `X` was given or not. If all values of `DATA` are 
;                               equal to `FILLVAL`, then interpolation cannot be performed
;                               and `REPLACED_DATA` is an array of `REPLACE_VALUE`.
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
;       02/14/2013  -   Written by Matthew Argall
;       05/24/2013  -   Interpolation can be performed instead of merely replacing the
;                           fill value. Parameters X and XOUT were added, as well as the
;                           _EXTRA keyword. Ensure that FILLVAL is a scalar. - MRA
;       2014/09/10  -   FILLVAL can now be NAN, INF, -INF. - MRA
;-
function replace_fillval, data, fillval, x, $
REPLACE_VALUE = replace_value, $
NO_COPY = no_copy, $
_REF_EXTRA = extra
    compile_opt idl2
    on_error, 2

    ;Set the default replacement value
    if n_elements(replace_value) eq 0 then replace_value = !values.f_nan

;-----------------------------------------------------
;SEARCH FOR THE  FILL VALUE \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

   ;Find index locations of the fill value
    if finite(fillval) $
        then bad_pts = where(data eq fillval[0], nBad, COMPLEMENT=good_pts, NCOMPLEMENT=nGood) $
        else bad_pts = where(finite(data) eq 0,  nBad, COMPLEMENT=good_pts, NCOMPLEMENT=nGood)
    if nBad eq 0 then return, data
   
    ;Make the output array
    if keyword_set(no_copy) then replaced_data = temporary(data) $
                            else replaced_data = data

;-----------------------------------------------------
;REPLACE THE FILL VALUE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if n_elements(x) eq 0 then begin
        replaced_data[bad_pts] = replace_value

;-----------------------------------------------------
;INTERPOLATE OVER THE FILL VALUE \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin

        if nGood gt 0 $
            then replaced_data = interpol(replaced_data[good_pts], x[good_pts], x, _STRICT_EXTRA=extra) $
            else replaced_data[*] = replaced_value
    
    endelse
        
                    
    return, replaced_data
end