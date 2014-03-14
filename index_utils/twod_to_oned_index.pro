; docformat = 'rst'
;
; NAME:
;       twoD_to_oneD_index
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;       ***IMPORTANT***
;       This is not compatible with indices returned by IDL (e.g. max(array, imax)). IDL
;       Column Major. This program is Row Major. See "Convert ROW-MAJOR 1D -> 2D" example.
;       ***************
;
;       Convert a 2D subscript of an array to a corresponding 1D subscript.
;       Alternatively, converty a 1D subscript of an array to a corresponding
;       2D subscript.
;
;       Row increases before column. If DIMS = [3,3], then
;           [0,0] = 0       [1,0] = 3       [2,0] = 6
;           [0,1] = 1       [1,1] = 4       [2,1] = 7
;           [0,2] = 2       [1,2] = 5       [2,2] = 8
;
; :Categories:
;   Index Utility
;
; :Examples:
;   Example 1: Convert the 1D index of the maximum value of an array to a 2D index::
;       array = randomu(5,7,3)
;       dims = size(array, /dimensions)
;       array_max = max(array, imax)
;       2D_index = twoD_to_oneD_index(imax, dims, /oneD_to_twoD)
;
;   Example 2: Convert a 2D index value to a 1D index value::
;       array = randomu(5,7,7)
;       dims = size(array, /dimensions)
;       2D_index = [1,5]
;       1D_Index = twoD_to_oneD_index(2D_index, dims)
;
;   See also the main level program at the end of this file::
;       IDL> .r oneD_to_twoD_index
;
; :Params:
;       POS:            in, required, type=lonarr(2\,*)
;                       0 based subscript of an array [column, row]
;       DIMS:           in, required, type=lonarr(2)
;                       dimesensions of the array for which to convert POS
;                           e.g. dims = size(array, /dimensions)
;
; :Keywords:
;
;       ONED_TO_TWOD:   in, optional, type=Boolean, default=0
;                       Convert a 1D subscript of an array to a 2D subscript [col,row]
;                           This option will also work with multi dimensional subscripts.
;
; :Returns:
;
;       COLROW:         Returned if ONED_TO_TWOD is set. The corresponding 2D (or multi D)
;                           subscript of an array. Has same number of rows as POS
;       INDEX:          Returned if ONED_TO_TWOD is not set. The corresponding 1D 
;                           subscript of an array
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
;       11/03/2012  -   POS can now be an array, added COL_MAJOR keyword.
;       05/20/2013  -   Return a single row, not a single column, of 1D indices when 
;                           ONED_TO_TWOD is set. - MRA
;
function twoD_to_oneD_index, pos, dims, $
ONED_TO_TWOD = oneD_to_twoD, $
COL_MAJOR = col_major
    compile_opt idl2   
    
    ;convert an index number to a 2D number of the form [column, row]
    if keyword_set(oneD_to_twoD) then begin
        
        ;if the result is to be column-major...
        if keyword_set(col_major) then begin
            return, array_indices(dims, pos, /DIMENSIONS)
        
        ;otherwise, return a row-major result
        endif else begin
            col = floor(pos / dims[1])
            row = pos - col*dims[1]

            if n_elements(pos) eq 1 then colrow = [col, row] $
                                    else colrow = transpose([[col], [row]])
            return, colrow
        endelse
            
    ;convert [col, row] to a 1D index number
    endif else begin
        ;DIMS are 1-based, POS is 0-based
        if max(pos[0,*] gt dims[0]-1) or max(pos[0,*] lt 0) then message, 'POS is outside range of DIMS'
        if max(pos[1,*] gt dims[1]-1) or max(pos[1,*] lt 0) then message, 'POS is outside range of DIMS'

        ;calculate the index number. return to 0-based
        if keyword_set(col_major) then begin
            index = reform( (pos[1,*] * dims[0]) + pos[0,*] )
        endif else begin
            index = reform( (pos[0,*] * dims[1]) + pos[1,*] )
        endelse

        ;return a scalar if a single position was given.
        if n_elements(pos[0,*]) eq 1 then return, index[0] $
                                     else return, index

    endelse
end


;--------------------------------------------------
;Main Level Example Program (.r twoD_to_oneD_index)
;--------------------------------------------------
print, '------------------------------------------------'
print, 'Given the following array,'
array = randomu(2,5,7)
print, array
dims = size(array, /dimensions)

print, ''
;==============================================================
;Convert a 1-D Row-Major index to a 2-D Row-Major index ///////
;==============================================================
print, 'Convert ROW-MAJOR 1D -> 2D'

;find the column-major index of the maximum value of ARRAY
array_max = max(array, index_to_convert)

;Convert it to a row-major value....
;calculate the column-major index compatible with the IDL indexing convention,
;interchange the row and column, then convert it back to 1-D row-major index
twoD_col = twoD_to_oneD_index(index_to_convert, dims, /oneD_to_twoD, /COL_MAJOR)
twoD_temp = reverse(twoD_col)
index_to_convert = twoD_to_oneD_index(twoD_temp, dims)

;Calculate the row-major index (returned [col, row])
twoD_index = twoD_to_oneD_index(index_to_convert, dims, /oneD_to_twoD)

print, format='(%" Index to convert: ", i2)', index_to_convert
print, format='(%" Array Value: ", f8.6)', array[twoD_index[0], twoD_index[1]]
print, format='(%" Result [COL, ROW]: [", i2, ", ", i2, "]")', twoD_index
print, ''

;==============================================================
;Convert a 2-D Row-Major index to a 1-D Row-Major index ///////
;==============================================================
print, 'Convert ROW-MAJOR 2D -> 1D'

;pick a row-major index
twoD_index = [2,0]

;calculate the 1-D row-major index
oneD_index = twoD_to_oneD_index(twoD_index, dims)

print, format='(%" Index to convert [COL, ROW]: [", i2, ", ", i2, "]")', twoD_index
print, format='(%" Array Value: ", f8.6)', array[twoD_index[0], twoD_index[1]]
print, format='(%" Result: ", i3)', oneD_index
print, ''

;==============================================================
;Convert a 1-D Col-Major index to a 2-D Col-Major index ///////
;==============================================================
print, 'Convert COL-MAJOR 1D -> 2D'

;pick a 1D index to convert
index_to_convert = 2

;convert it to 2D
twoD_index = twoD_to_oneD_index(index_to_convert, dims, /oneD_to_twoD, /COL_MAJOR)

print, format='(%" Index to convert: ", i3)', index_to_convert
print, format='(%" Array Value: ", f8.6)', array[twoD_index[0], twoD_index[1]]
print, format='(%" Result [COL, ROW]: [", i2, ", ", i2, "]")', twoD_index
print, ''


;==============================================================
;Convert a 2-D Col-Major index to a 1-D Col-Major index ///////
;==============================================================
print, 'Convert COL-MAJOR 2D -> 1D'

;pick a row-major index
twoD_index = [2,0]

;calculate the 1-D row-major index
oneD_index = twoD_to_oneD_index(twoD_index, dims, /COL_MAJOR)

print, format='(%" Index to convert [COL, ROW]: [", i2, ", ", i2, "]")', twoD_index
print, format='(%" Array Value: ", f8.6)', array[twoD_index[0], twoD_index[1]]
print, format='(%" Result: ", i2)', oneD_index

print, '------------------------------------------------'

end