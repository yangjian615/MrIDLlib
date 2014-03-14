; docformat = 'rst'
;
; NAME:
;       mrAutocorrelation
;
; PURPOSE:
;+
;       The purpose of this program is to compute the autocorrelation coefficient of a
;       set of data. Uses the formula
;
;       `\hat{\rho}_{k} = \frac{\sum_{i=1}^{N-k} (x_{t+k} - \overline{x}) (x_{t} - \overline{x})} {\sum_{i=1}^{N} (x_{t} - \overline{x})^{2}}`
;
; :Categories:
;       Math Utilities, Vector Math
;
; :Params:
;       X:              in, required, type=numeric vector
;                       The array for which the autocorrelation coefficient is to be
;                           calculated
;       LAG:            in, required, type=int
;                       The lag in the autocorrelation.
;
; :Keywords:
;
; :Returns:
;       ac_coeff:      Autocorrelation coefficient of `X`, lagged by `LAG`.
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
;
;       02/08/2012  -   Written by Matthew Argall
;-
function mrAutocorrelation, x, lag
    compile_opt idl2
    on_error, 2

    nx = n_elements(x)
    x_avg = mean(x)
    
    ;calculate the auto-correlation coefficient
    ac = total((x[0:nx-lag-1] - x_avg) * (x - x_avg)) / total((x - x_avg)^2)

end