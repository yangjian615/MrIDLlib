; docformat = 'rst'
;
; NAME:
;
;       MrPut_Dimension_First
;
; PURPOSE:
;+
;   The purpose of this program is to transpose an array so that the specified dimesion
;   becomes the first dimension. If dimension N is to be put first, dimension N and
;   dimension 1 are interchanged.
;
;   To undo the operation, just call MrPut_Dimension_First again with the same argumetns.
;
; :Categories:
;       Array Utility
;
; :Params:
;
;       ARRAY:              in, required, type=any
;                           The array that is to be transformed.
;       DIMENSION:          in, required, type=int
;                           The dimension that will become the new leading dimension. Or,
;                               if `UNDO` is specified, where the leading dimension was
;                               originally. Dimensions range from 1-8.
;       
; :Returns:
;
;       ARRAY_OUT:          A copy of `ARRAY`, but with its dimensions shifted so that
;                           `DIMENSION` is first.
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
;       Matthew Argall 2013
;
; :History::
;   Modification History::
;       04/11/2013  -   Written by Matthew Argall
;-
function MrPut_Dimension_First, array, dimension
    compile_opt idl2
    on_error, 2
    
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Make sure dimension is between 1 and 8. IDL array have a maximum of 8 dimensions
    if dimension lt 1 or dimension gt 8 then $
        message, 'DIMENSION must be an integer between 1 and 8.'
    
    ;If DIMENSION is 1, then there is no need to transpose
    if dimension eq 1 then return, array
    
;-----------------------------------------------------
;Interchange dimensions 1 and DIMENSION \\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;get the size of the input array
    array_sz = size(array, /STRUCTURE)

    ;Create an array containing all of the possible dimensions
    if array_sz.n_dimensions eq 0 $
        then dims = 1 $
        else dims = indgen(array_sz.n_dimensions)      
    
    ;Interchange the first dimension with DIMENSION. TRANSPOSE takes dimensions from
    ;0-7, not 1-8.
    dims_temp = dims
    dims[0] = dims_temp[dimension-1]
    dims[dimension-1] = 0
        
;-----------------------------------------------------
;Transpose \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;The the dimensions enough to put the desired one first, then take the transpose
    ;so that it IS first.
    array_out = transpose(array, dims)
    
    return, array_out
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
array = findgen(13, 26, 18, 43, 6)
array_do = mrPut_Dimension_First(array, 3)
array_undo = mrPut_Dimension_First(array_do, 3, /UNDO)

print, 'The original array dimensions:'
help, array
print,  ''

print, 'After making the 3rd dimension first:'
help, array_do
print, ''

print, 'And undoing the transpose operation:
help, array_undo
print, ''

end