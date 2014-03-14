;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 2000
;
; NAME:
;       logticks_exp
;
; PURPOSE:
;+
;       Function to print logarithmic axis tickmarks with exponential
;       output. The format of the tickmark will be, e.g., 10^3, where "^3" indicates that
;       the "3" will be a superscript. This format works well when the /XLOG or /YLOG
;       keyword is set.
;
; :Categories:
;       Graphics
;
; :Params:
;       AX:             in, required, type=string
;                       the axis on which the labels will be placed
;       INDEX:          in, required, type=int
;                       the index of the values passed into the function
;       VALUES:         in, required, type=fltarr
;                       a time array in seconds from midnight
;
; :Returns:
;       Function returns a string containing the tick mark labels.
;
; :History:
;   Modification History::
;       Written by:     Paul van Delst, CIMSS/SSEC, 08-Nov-2000
;                       paul.v...@ssec.wisc.edu
;       2013-07-23  -   Found on `David Fanning's website <http://www.idlcoyote.com/tips/exponents.html>`
;                           by following a link to the `IDL news group <https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/-01uvkmjsHM>`.
;                           Made more comments. Renamed from logTick_exp to logTickFormat. - Matthew Argall
;-
FUNCTION logTickFormat, axis, index, value
   ; Determine the base-10 exponent
   exponent   = LONG( ALOG10( value ) )
   ; Construct the tickmark string based on the exponent
   tickmark = '10!E' + string(exponent, FORMAT='(i0)') + '!N'
   ; Return the formatted tickmark string
   RETURN, tickmark
END