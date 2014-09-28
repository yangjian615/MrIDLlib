; docformat = 'rst'
;
; NAME:
;       MrArray_Bounds
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   Converts normal IDL indexing notation (represented as a string) into a
;   `lonarr(ndims, 3)` where the first row is start values, the second row is
;   the end values, and the last row is the stride value.
;
;       [start_index, stop_index, stride]
;
;   MrArray_Bounds is meant to be called in a manner similar to IDL's Array_Inidces()
;   function.
;
;   This can be used to determine the dataspace hyperslab in HDF5 files or, with the
;   OFFSET keyword, the hyperslab in CDF files.
;
; :Examples:
;   See the mail-level example program at the end of this document::
;       IDL> .r MrArray_Bounds
;
; :Categories:
;       Index Utilities
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
;       2014/08/22  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Convert a 1D array-indexing string to a [istart, istop, stride] notation.
;
; :Params:
;       BOUNDS:         in, required, type=string
;                       Notation for 1 dimension, e.g., '0', '3:9', '3:*:2'
;       DIM_SIZE:       in, required, type=long
;                       Size of the dimensions into which `BOUNDS` indexes.
;
; :Returns:
;       RESULT:         The [iStart, iStop, stride] values for accessing the hyperslab.
;-
function MrArray_Bounds_1D, bounds, dim_size
  compile_opt strictarr
  on_error, 2

    ;[iStart, iStop, stride] in IDL is represented as iStart:iStop:Stride. Separate
    ;the arguments by ":"
    args = strsplit(bounds, ':', /EXTRACT, COUNT=nargs)
    
    ;Assume we are getting every element.
    result = [0L, dim_size - 1L, 1L]

    case nargs of
        ;iStart
        1: begin
            ;The "*" case is assumed by default.
            ;   Turn negative indices positive.
            if (args[0] ne '*') then begin
                index = long(args)
                if (index lt 0L) then index += dim_size
                result[0:1] = index
            endif
        endcase
        
        ;iStart:iStop
        2: begin
            ;iStart:*
            ;   Ensure the start index is positive.
            ;   "*" is assumed by default.
            if (args[1] eq '*') then begin
                result[0] = long(args[0])
                result[0] = result[0] lt 0L ? (dim_size + result[0]) : result[0]
                
            ;iStart:iStop
            ;   Make indices positive
            endif else begin
                result[0:1] = long(args)
                if (result[0] lt 0L) then result[0] = dim_size + result[0]
                if (result[1] lt 0L) then result[1] = dim_size + result[1]
            endelse
        endcase
        
        ;iStart:iStop:Stride
        3: begin
            ;iStart:*:Stride
            if (args[1] eq '*') then begin
                result[0] = long(args[0])
                result[0] = result[0] lt 0L ? (dim_size + result[0]) : result[0]
                result[2] = long(args[2])
                
            ;iStart:iStop:Stride
            endif else begin
                result[0:2] = long(args)
                if (result[0] lt 0L) then result[0] = dim_size + result[0]
                if (result[1] lt 0L) then result[1] = dim_size + result[1]
            endelse
        endcase
        
        else: message, 'invalid indexing notation: ' + bounds
    endcase

    return, result
end


;+
;   Converts normal IDL indexing notation (represented as a string) into a
;   `lonarr(ndims, 3)` where the first row is start values, the second row is
;   the end values, and the last row is the stride value.
;
;       [start_index, stop_index, stride]
;
; :Private:
;
; :Params:
;       BOUNDS:         in, required, type=string
;                       Bounds specified as a string using IDL's normal indexing notation.
;
; :Keywords:
;       COUNT:          in, optional, type=boolean, default=0
;                       If set, `RESULT` will have the format [offset, count, interval],
;                           which is suitable for CDF files.
;       DIMENSIONS:     in, optioinal, type=boolean, default=0
;                       If set, then `ARRAY` represents the dimension sizes of the array
;                           for which `BOUNDS` are to be converted, as returned by IDL's
;                           Size(`ARRAY`, /DIMENSIONS) function.
;       SINGLE:         out, optional, type=boolean
;                       Set to a named variable to determine if the bounds expression was
;                           specified in single-index dimensioning
;
; :Returns:
;       RESULT:         The [iStart, iStop, stride] values for accessing the hyperslab.
;                           One column per dataspace dimension.
;-
function MrArray_Bounds, array, bounds, $
COUNT=count, $
DIMENSIONS=dimensions, $
SINGLE=single
    compile_opt strictarr
    on_error, 2

    ;Defaults
    count      = keyword_set(count)
    dimensions = keyword_set(dimensions)

    ;Were dimensions given?
    if dimensions $
        then arrDims = array $
        else arrDims = size(array, /DIMENSIONS) 
        

    ;Index ranges for each dimension are separated by a comma.
    dimIndices = strtrim(strsplit(bounds, '[],', /EXTRACT, COUNT=nDims), 2)
    result = lonarr(ndims, 3)

    ;Convert
    case ndims of
        ;Single dimension of indices for a dataset with possibly more than one dimension
        ;   - dimensions = [4, 2, 3]
        ;   - bounds     = '2:20'
        1: begin
            single = 1B
            result[0, *] = MrArray_Bounds_1D(dimIndices[0], product(arrDims))
        endcase
        
        ;One index range per dimension of data.
        ;   - dimensions = [4, 2, 3]
        ;   - bounds     = '[0:3:2, *, 1:*]'
        n_elements(arrDims): begin
            single = 0B
            for d = 0L, ndims - 1L do $
                result[d, *] = MrArray_Bounds_1D(dimIndices[d], arrDims[d])
        endcase
        
        else:  message, 'Invalid number of dimensions in array indexing notation.'
    endcase
    
    ;Convert to [offset, count, interval]?
    ;   - If [ (count mod interval) ne 0 ] then we need to add 1.
    ;   - Same as rounding up.
    if count then $
        result[*,1] = ceil( float((result[*,1] - result[*,0] + 1)) / result[*,2] )

    return, result
end



;-----------------------------------------------------
; Main-Level Example Program: IDL> .r MrArray_Bounds |
;-----------------------------------------------------

;EXAMPLE 1
theArr   = bytarr(32, 256, 16)
bounds   = '[*, 12:200:4, 4:*]'
results = MrArray_Bounds(theArr,  bounds, SINGLE=single)

;Print the results
print, '-----------------------------------'
print, 'Array:  bytarr(32, 256, 16)'
print, "Bounds: '" + bounds + "'"
print, 'Single: '  + string(single, FORMAT='(i1)')
print, 'Results:'
for i = 0, n_elements(results[*,0])-1 do print, FORMAT='(%"    [%3i, %3i, %3i]")', results[i,*]
print, ''


;EXAMPLE 2
;   - Same as EXAMPLE 1, except ...
;   - Use the COUNT keyword
theArr  = bytarr(32, 256, 16)
bounds   = '[*, 12:200:4, 4:*]'
results = MrArray_Bounds(theArr, bounds, SINGLE=single, /COUNT)

;Print the results
print, '-----------------------------------'
print, 'Array:  bytarr(32, 256, 16)'
print, "Bounds: '" + bounds + "'"
print, 'Single: '  + string(single, FORMAT='(i1)')
print, 'Results:'
for i = 0, n_elements(results[*,0])-1 do print, FORMAT='(%"    [%3i, %3i, %3i]")', results[i,*]
print, ''


;EXAMPLE 3
;   - Provide dimensions instead of an array
;   - Use single-dimensional subscripting
theArr  = bytarr(32, 256, 16)
theDims = size(theArr, /DIMENSIONS)
bounds  = '[10:130000:100]'
results = MrArray_Bounds(theDims, bounds, SINGLE=single, /DIMENSIONS)

;Print the results
print, '-----------------------------------'
print, 'Array:  bytarr(32, 256, 16)'
print, "Bounds: '" + bounds + "'"
print, 'Single: '  + string(single, FORMAT='(i1)')
print, 'Results:'
for i = 0, n_elements(results[*,0])-1 do print, FORMAT='(%"    [%6i, %6i, %6i]")', results[i,*]
print, ''

end