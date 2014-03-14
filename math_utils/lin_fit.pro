pro lin_fit, x, y, wt, a, b, si
;+
; NAME:
;    LINFIT 
; PURPOSE:
;    To make a least squares fit to data with a straight line:   
;    Y = A + B*X  (IDL version of Bevington's LINFIT program)
;    
; CALLING SEQUENCE: 
;    LINFIT, X, Y, WT, A, B, SI
; INPUT PARAMETERS:
;
;    X -  vector containing the independent variable data
;    Y - vector containing the dependent variable data
;    WT - vector containing the weighting function data
;         as described by Bevington (p.104).   If uniform weighting is 
;         desired, set WT = 1. For instrumental uncertainties, use
;         WT=1/sigmay^2.
; OUTPUT PARAMETERS
;    A -  Required output scalar containing the y-intercept for the
;        least
;         squares fit.
;    B  - Output scalar containing the slope of the least squares
;         line.
; OPTIONAL OUTPUT PARAMETER:
;    SI - vector containing statistical information on the
;         fit. 
;         SI(0)= correlation coefficient (R) where 1. is a perfect fit
;                & 0. represents no correlation (see STATISTICAL
;                TREATMENT
;                OF EXPERIMENTAL DATA by Young p.130)
;         SI(1)= 1 sigma error of coefficient A (SIGMAA)
;         SI(2)= 1 sigma error of coefficient B (SIGMAB) 
;         SI(3)= mean value of Y array
;         if no weighting is specified the following are included:
;         SI(4)= scatter about mean (SQRT( TOTAL((Y-YAVG)^2)/N-1 ))
;         SI(5)= scatter about fit (SQRT( TOTAL((Y-(A+BX))^2)/N-2 ))
; EXAMPLES:
;
;    To fit measurements of equivalent width (EQW) to a linear
;    function
;    of time with uniform weighting of the individual points:
;
;    IDL> wt=0.*time+1.0 
;    IDL> linfit,time,eqw,wt,a,b,si    
;    IDL> yfit = b*time+a 
;    IDL> plot,time,eqw, psym = 4
;    IDL> oplot,time,yfit    
;
; NOTES:
;    Least squares (for further infomation see DATA REDUCTION
;    AND ERROR ANALYSIS FOR THE PHYSICAL SCIENCES by Bevington
;    pp. 92-118).
;    Modifications were made (9-25-84 RWT) which makes the procedure
;    differ slightly from Bevington's. Instead of fitting the
;    expression
;           Y = A + B*X,
;    LINFIT calculates
;          Y = a + b*(x-<X>)
;    where <X> = TOTAL(X*WT)/TOTAL(WT). LINFIT converts back to the
;    original expression using the equations:
;          A = a - b*<X>
;          B = b
;          SIGMAA = SQRT(SIGMAa^2 + SIGMAb^2*<X>^2)
;          SIGMAB = SIGMAb
;*MODIFICATION HISTORY: 
;    Adapted from the IUE RDAF          December, 1991
;-
;***********************************************************************
;
  On_error,2

  if N_params() LT 4 then begin
      print,'Calling sequence - linfit, x, y ,wt, a, b, si'
      return 
  endif

  nx = N_elements(x)
  ny = N_elements(y)
  if nx LT 2 then $
    message,'ERROR - X vector (first parameter) must have at least 2 elements'
  if nx NE ny then $
    message,'ERROR - X and Y vectors must have equal number of elements'
 ;
  si = fltarr(6)
  a  = 0.0 & b = 0.0
;
; if wt is scalar (i.e. no weighting) change to vector
;
  s = size(wt)
  if s(0) lt 1 then wt = fltarr(ny) + wt  
;
; calculate mean of x & subtract from x array
;
  wxav = total(wt*x) / total(wt)
  xs   = x - wxav
;
; calculate coefficients a & b
;
  sum = total(double(wt))
  sx  = total(double(xs*wt)) & sx2 = total(double(xs*xs*wt))
  sy  = total(double(y*wt))  & sy2 = total(double(y*y*wt)) 
  sxy = total(double(xs*y*wt))
  del = double(sum*sx2 - sx*sx)
;
  if del EQ 0.0d0 then $
    message,'ERROR - Zero denominator found'
;
  as = (sx2*sy - sx*sxy) / del
  bs = (sum*sxy - sx*sy) / del
  a  = as - bs*wxav
  b  = bs
;
; calculate standard deviations & correlation coeficients 
;
  si(0) = (sum*sxy - sx*sy) / sqrt(del*(sum*sy2 - sy*sy)) ; correl. coeff.
  si(3) = total(y) / ny         ; mean of y array
;
  if (s(0) gt 0) then var = 1. else begin
      var = (sy2 +as*as*sum +bs*bs*sx2 -2.0*(as*sy+bs*sxy-as*bs*sx)) / (ny-2) 
      si(4) = sqrt(total((y - si(3))*(y - si(3)))/(ny - 1)) ; scatter about mean
      if (var GE 0.0) then si(5) = sqrt(var) ; est. parent sigma
  endelse                       ; s(0) gt 0
;
  if var GE 0.0 then begin
      si(1) = sqrt(var*sx2 / del) ; sigmaa 
      si(2) = sqrt(var*sum / del) ; sigmab 
      si(1) = sqrt(si(1)*si(1) + wxav*wxav*si(2)*si(2)) ; sigmaa 
  endif else print,'Variance lt 0.0, LINFIT continuing'   
;
  return
end                             ; linfit
