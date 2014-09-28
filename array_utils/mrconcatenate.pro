; docformat = 'rst'
;
;+
;   The purpose of this program is to concatenate two arrays along a given dimension.
;
; :Examples:
;   Try the main level program at the end of this document::
;       IDL> .r MrConcatenate
;
; :Categories:
;   Array Utility
;
; :Params:
;       ARRAY1:         in, required, type=list/array
;                       The array to be concatenated with `ARRAY2`. Both arrays must have
;                           the same number of dimensions and dimensions other than
;                           `DIMENSION` must be the same size. If ARRAY1 is a list, then
;                           all elements of the list must meet these requirements.
;       ARRAY2:         in, required, type=list/array
;                       The array to be concatenated with `ARRAY1`. Ignored if `ARRAY1` is
;                           a list.
;       DIMENSION:      in, optional, type=integer, default=1
;                       Dimension to be concatenated.
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
;       2013-12-23  -   Written by Matthew Argall
;       2013-12-25  -   Shallow dimensions are now preserved. - MRA
;       2013-12-29  -   Problem when lists had a single element. Fixed. - MRA
;       2014/03/03  -   1D arrays were causing problems. Fixed.
;       2014/03/05  -   Added quick case for DIMENSION=2. - MRA
;       2014/04/15  -   Made dimension juggling more readable. - MRA
;       2014/05/22  -   IDL allows 3 levels of bracket concatenation. Use 3rd level. - MRA
;-
function MrConcatenate, array1, array2, dimension
    compile_opt strictarr
    on_error, 2

;-----------------------------------------------------
;List \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------  
    if size(array1, /TNAME) eq 'OBJREF' then begin
        if obj_isa(array1, 'LIST') eq 0 then message, 'ARRAY1 must be a LIST object.'
        
        ;Use the DIMENSION keyword to List::ToArray if possible
        if MrCmpVersion('8.2.3') le 0 then $
            return, array1 -> ToArray(DIMENSION=dimension)
            
        ;Step through each element of the list.
        n = array1 -> Count()
        if n eq 1 then return, array1[0]
        
        for i = 1, n-1, 2 do begin
            ;Concatenate each element of the array.
            if i eq 1 $
                then data_out = MrConcatenate(array1[0], array1[i], dimension) $
                else data_out = MrConcatenate(data_out,  array1[i], dimension)
        endfor
        
        return, data_out
    endif
        
;-----------------------------------------------------
;Simple Cases \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    case dimension of
        1: return, [  array1  ,   array2  ]
        2: return, [ [array1] ,  [array2] ]
        3: return, [[[array1]], [[array2]]]
        else: ;Continue below
    endcase
        
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if n_elements(dimension) eq 0 $
        then dimension = [1] $
        else dimension = [dimension]
    
    ;Make editable copies of the arrays
    temparr1 = array1
    dims1    = size(array1, /DIMENSIONS)
    nDims1   = size(array1, /N_DIMENSIONS)
    
    temparr2 = array2
    dims2    = size(array2, /DIMENSION)
    nDims2   = size(array2, /N_DIMENSIONS)
    
    ;Make sure the number of dimensions agree.
    if nDims1 ne nDims2 then $
        message, 'ARRAY1 and ARRAY2 must have the same number of dimensions.'

    ;Dimensions not being concatenated.
    iDimKeep = where(histogram(dimension, MIN=1, MAX=nDims1) ne 1, nkeep) + 1

    ;Check if the other dimensions can be concatenated.
    if array_equal(dims1[iDimKeep-1], dims2[iDimKeep-1]) eq 0 $
        then message, 'Dimensions not being concatenated must be the same size.'
    
;-----------------------------------------------------
;Concatenate \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Allocate memory to the output array.
    nCat = dims1[dimension-1] + dims2[dimension-1]
    data_out = make_array([nCat, dims1[iDimKeep-1]], TYPE=size(array1, /TYPE))

    ;Transpose the arrays to put the chosen dimension first.
    if dimension ne 1 then begin
        temparr1 = transpose(temporary(temparr1), [dimension-1, iDimKeep-1])
        temparr2 = transpose(temporary(temparr2), [dimension-1, iDimKeep-1])
    endif

    ;Concatenate the data
    data_out = [temporary(temparr1), temporary(temparr2)]

    ;If a shallow dimension (of size 1) was transposed to the last dimension,
    ;IDL truncates it. We need to put it back with a REFORM. This happens only if
    ;   1. ARRAY1 is 1xN and DIMENSION=2 (already handled above)
    ;   2. DIMENSION = N_DIMENSIONS and the second to last dimension is shallow.
    if size(data_out, /N_DIMENSIONS) ne nDims1 then begin
        ;Find the shallow dimensions
        iShallow = where(dims1[iDimKeep-1] eq 1, nShallow, $
                         COMPLEMENT=iDeep, NCOMPLEMENT=nDeep)

        ;Remove all extra shallow dimensions
        if nShallow gt 1 then data_out = reform(data_out)
        
        ;Put shallow dimensions first: [shallow, catDim, deep]
        if nDeep gt 0 then begin
            shallowDims = dims1[iDimKeep[iShallow]-1]
            deepDims    = dims1[iDimKeep[iDeep]-1]
            unReform    = [shallowDims, nCat, deepDims]
            data_out    = reform(data_out, unReform, /OVERWRITE)
        endif else begin
            shallowDims = dims[iDimKeep[iShallow]-1]
            unReform    = [shallowDims, nCat]
            data_out    = reform(data_out, unReform, /OVERWRITE)
        endelse

        ;Order the dimensions properly. Store current index values at old index locations.
        ;   - Shallow dimensions first.
        ;   - Then the transposed dimension
        ;   - Then deep dimensions
        unTranspose = bytarr(nDims1)
        unTranspose[iDimKeep[iShallow]-1] = indgen(nShallow)
        unTranspose[dimension-1] = nShallow
        if nDeep gt 0 then unTranspose[iDimKeep[iDeep]-1] = indgen(nDeep) + nShallow + 1
        
        ;Un-transpose the data
        data_out = transpose(data_out, unTranspose)
        
    ;Last dimension is not shallow
    endif else begin
        unTranspose = bytarr(nDims1)
    
        ;Dimensions that were not concatenated.
        newTrail = where(histogram([1], MIN=1, MAX=nDims1) eq 0)
        unTranspose[iDimKeep-1] = newTrail
        unTranspose[dimension-1] = 0
        
        data_out = transpose(data_out, unTranspose)
    endelse

    return, data_out
end




;-----------------------------------------------------
;Main Level Example Program: IDL> .r MrConcatenate) \\
;-----------------------------------------------------

array1     = findgen(20)
array2     = findgen(10)
arrayCat12 = MrConcatenate(array1, array2, 1)
help, array1, array2, arrayCat12
print, ''

array3     = findgen(5,1,13)
array4     = findgen(5,1,7)
arrayCat34 = MrConcatenate(array3, array4, 3)
help, array3, array4, arrayCat34
print, ''

array5     = findgen(1,8)
array6     = findgen(1,7)
arrayCat56 = MrConcatenate(array5, array6, 2)
help, array5, array6, arrayCat56
print, ''

end