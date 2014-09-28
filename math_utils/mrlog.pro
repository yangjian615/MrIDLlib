; docformat = 'rst'
;
; NAME:
;       mrLog
;
; PURPOSE:
;+
;   The purpose of this program is to serve as a wrapper for IDL's ALOG10 function. It
;   checks for x < 0 and x = 0 and sets the result to NAN and INF, respectively, without
;   taking the log of those values. Prevents annoying math error outputs.
;
; :Catetories:
;
;       Math Utilities, Wrapper
;
; :Examples:
;
;       See the example program at the end of this file::
;
;           IDL> .r mrLog
;
; :Params:
;       X:                  in. required. type=numeric scalar or array
;                           The values for which the log is to be taken.
;
; :Keywords:
;       MISSING_VALUE:      in, optional, type=same as `X`
;                           Normally, the log of 0 is -infinity and the log of a
;                               negative number is NaN. Set this keyword to a
;                               finite numeric value to take the place of NaN and
;                               infinity.
;       DOUBLE:             in, optional, type=boolean, default=0
;                           Force computation to be performed in double precision.
;
; :Returns:
;       LOG10_X:            The log-base-10 of `X`.
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
;       03/01/2013  -   Written by Matthew Argall
;       2013/11/12  -   Added the MISSING_VALUE keyword. DOUBLE is applied properly. - MRA
;-
function mrLog, x, $
MISSING_VALUE=missing_value, $
DOUBLE = double
    compile_opt idl2
    on_error, 2
       
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    type = size(x, /TNAME)
    dims = size(x, /DIMENSIONS)
    
;---------------------------------------------------------------------
;Convert to Float or Double //////////////////////////////////////////
;---------------------------------------------------------------------
    case type of
        'BYTE':     if keyword_set(double) then xtemp = double(x) else xtemp = float(x)
        'INT':      if keyword_set(double) then xtemp = double(x) else xtemp = float(x)
        'LONG':     if keyword_set(double) then xtemp = double(x) else xtemp = float(x)
        'FLOAT':    if keyword_set(double) then xtemp = double(x) else xtemp = x
        'DOUBLE':   xtemp = x
        'COMPLEX':  if keyword_set(double) then xtemp = dcomplex(x) else xtemp = x
        'DCOMPLEX': xtemp = x
        'UINT':     if keyword_set(double) then xtemp = double(x) else xtemp = float(x)
        'ULONG':    if keyword_set(double) then xtemp = double(x) else xtemp = float(x)
        'LONG64':   if keyword_set(double) then xtemp = double(x) else xtemp = float(x)
        'ULONG64':  if keyword_set(double) then xtemp = double(x) else xtemp = float(x)
        else: message, 'X must be a numeric data type.'
    endcase        
    type = size(xtemp, /TNAME)
    
;---------------------------------------------------------------------
;Missing Value ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;They will get converted to double/dcomplex when they get put into
    ;the result.
    if n_elements(missing_value) gt 0 then begin
        nan_value     = missing_value
        neg_inf_value = missing_value
    endif else begin
        nan_value     =  !values.f_nan
        neg_inf_value = -!values.f_infinity
    endelse
            
    ;Allocate memory if X is an array.
    if n_elements(x) eq 1 $
        then log10_x = xtemp*0 $
        else log10_x = make_array(dims, TYPE=out_type)
       
;---------------------------------------------------------------------
;Log of Complex Numbers //////////////////////////////////////////////
;---------------------------------------------------------------------
    if (type eq 'COMPLEX') or (type eq 'DCOMPLEX') then begin
        ;To compute the log of a complex number:
        ;
        ;z = x + iy --> ln(z) = ln( r*exp(i*phi) ) = ln(r) + i*phi
        ;x = r*cos(phi), y = r*sin(phi)
        ;r = abs(z) = sqrt(x^2 + y^2)
        ;phi = atan(z, /PHASE) = atan(y, x)
        ;
        ;So, log10(z) = log10(r) + atan(z, /PHASE)/ln(10)
        ;The division by ln(10) is to convert from base e to base 10
        
        ;The only way ALOG10 will cause a math error is if abs(z) = 0.
        r = abs(xtemp)
        zeros = where(r eq 0, nzeros, COMPLEMENT=positives, NCOMPLEMENT=npos)
        
        ;Take the log of positive values and set 0's to -inf
        if npos   gt 0 then log10_x[positives] = alog10(xtemp[positives])
        if nzeros gt 0 then log10_x[zeros]     = neg_inf_value
       
;---------------------------------------------------------------------
;Log of Real Numbers /////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
    
        ;Find where x = 0 and where x <= 0. The former is a subset of the latter.
        zeros = where(xtemp eq 0, zero_count)
        negs  = where(xtemp le 0, neg_count, COMPLEMENT=positives, NCOMPLEMENT=npos)
        
        ;Take the log of all positive values. Set negative numbers equal to NaN and
        ;set 0's equal to infinity
        if npos       gt 0 then log10_x[positives] = alog10(xtemp[positives])
        if neg_count  gt 0 then log10_x[negs]      = nan_value
        if zero_count gt 0 then log10_x[zeros]     = neg_inf_value
    endelse
    
    return, log10_x
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Create variables of different types
x = [1.4, -26.432, 0, 24.186]

;Get their type codes
log_x = mrLog(x)
mlog_x = MrLog(x, MISSING_VALUE=-999)

;Print the results
print, format='(%"x             = [%f, %f, %f, %f]")', x
print, format='(%"MrLog(x)      = [%f, %f, %f, %f]")', log_x
print, format='(%"Missing Value = [%f, %f, %f, %f]")', mlog_x

end