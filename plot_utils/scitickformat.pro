; docformat = 'rst'
;
; NAME:
;       SciNotation_labels
;
; PURPOSE
;+
;   The purpose of this program is to apply a scientific notation-like formatting to the
;   axis labels of a plot. Labels will appear as, e.g., 3x10^4, where the "^4" indicates
;   that the 4 is a superscript.
;
; :Categories:
;   Plot Utilities
;
; :Params:
;       AX:             in, required, type=string
;                       the axis on which the labels will be placed
;       INDEX:          in, required, type=int
;                       the index of the values passed into the function
;       VALUES:         in, required, type=fltarr
;                       a time array in seconds from midnight
;
; :Author:
;   Stein Vidar Hagfors Haugan
;
; :History:
;   Modification History::
;
;       2013-07-24  -   Copied from `David Fanning's website 
;                           <http://www.idlcoyote.com/tips/exponents.html>`. Renamed from
;                           Exponent to SciTickFormat. - Matthew Argall
;                           
;-
FUNCTION sciTickFormat, axis, index, number

  ; A special case.
  IF number EQ 0 THEN RETURN, '0'

  ; Assuming multiples of 10 with format.
  ex = String(number, Format='(e8.0)')
  pt = StrPos(ex, '.')

  first = StrMid(ex, 0, pt)
  sign = StrMid(ex, pt+2, 1)
  thisExponent = StrMid(ex, pt+3)

  ; Shave off leading zero in exponent
  WHILE StrMid(thisExponent, 0, 1) EQ '0' DO thisExponent = StrMid(thisExponent, 1)

  ; Fix for sign and missing zero problem.
  IF (Long(thisExponent) EQ 0) THEN BEGIN
     sign = ''
     thisExponent = '0'
  ENDIF

  ; Make the exponent a superscript.
  IF sign EQ '-' THEN BEGIN
     RETURN, first + 'x10!U' + sign + thisExponent + '!N'
  ENDIF ELSE BEGIN
     RETURN, first + 'x10!U' + thisExponent + '!N'
  ENDELSE

END