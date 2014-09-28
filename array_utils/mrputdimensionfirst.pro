; docformat = 'rst'
;
;+
;   The purpose of this program is to concatenate two arrays along a given dimension
;
; :Examples:
;   Try running the main level program at the end of this document::
;       IDL> .r MrPutDimensionFirst
;
; :Categories:
;   Array Utility
;
; :Params:
;       ARRAY:          in, required, type=array
;                       An array whose dimensions are to be transposed.
;       DIMENSION:      in, optional, type=int, default=1
;                       Dimension of `ARRAY` to be made the leading dimension.
;
; :Keywords:
;       UNREFORM:       out, optional, type=intarr
;                       Sometimes when `ARRAY` has a dimension of size 1, IDL truncates
;                           them. To undo the results of this program a Reform() is needed.
;                           This keyword is to be passed to the Reform function. See the
;                           examples for details.
;       UNTRANSPOSE:    out, optional, type=intarr
;                       A variable that will contain the dimension order required to put
;                           `DIMENSION` back in its original location. Note that the
;                           `UNREFORM` keyword might be required as well.
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
;       2014-04-14  -   Written by Matthew Argall
;       2014-04-15  -   Made dimension juggling easier to follow. - MRA
;-
function MrPutDimensionFirst, array, dimension, $
UNREFORM=unReform, $
UNTRANSPOSE=unTranspose
    compile_opt strictarr
    on_error, 2
    
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;DIMENSION must be an array for Histogram to work
    if n_elements(dimension) eq 0 $
        then dimension = [1] $
        else dimension = [dimension]
    
    ;Make editable copies of the arrays
    dims  = size(array, /DIMENSIONS)
    nDims = size(array, /N_DIMENSIONS)
    
    ;Simple case
    if dimension eq 1 then begin
        unTranspose = indgen(nDims)
        unReform    = dims
        return, array
    endif
    
    ;Trailing dimensions, starting with 1
    trailDims = where(histogram(dimension, MIN=1, MAX=nDims) ne 1, nTrail) + 1
    
;-----------------------------------------------------
;Transpose \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Transpose the arrays to put the chosen dimension first.
    result = transpose(array, [dimension-1, trailDims-1])

    ;
    ;Figure out how to undo
    ;
    ;If a shallow dimension (of size 1) was transposed to the last dimension,
    ;IDL truncates it. We need to put it back with a REFORM. This happens only if
    ;   * DIMENSION = N_DIMENSIONS and the second to last dimension is shallow.
    ;
    if size(result, /N_DIMENSIONS) ne nDims then begin
        ;Find the shallow dimensions
        iShallow = where(dims[trailDims-1] eq 1, nShallow, $
                         COMPLEMENT=iDeep, NCOMPLEMENT=nDeep)

        ;Put shallow dimensions first: [shallow, dimension, deep]
        if nDeep gt 0 then begin
            shallowDims = dims[trailDims[iShallow]-1]
            deepDims    = dims[trailDims[iDeep]-1]
            unReform    = [shallowDims, dims[dimension-1], deepDims]
        endif else begin
            shallowDims = dims[trailDims[iShallow]-1]
            unReform    = [shallowDims, dims[dimension-1]]
        endelse

        ;Store current index values at old index locations.
        unTranspose = bytarr(nDims)
        unTranspose[trailDims[iShallow]-1] = indgen(nShallow)
        unTranspose[dimension-1] = nShallow
        if nDeep gt 0 then unTranspose[trailDims[iDeep]-1] = indgen(nDeep) + nShallow + 1

    ;No shallow dimensions
    endif else begin
        unReform = size(result, /DIMENSIONS)
        unTranspose = bytarr(nDims)
    
        ;Dimensions that were not concatenated.
        newTrail = where(histogram([1], MIN=1, MAX=nDims) eq 0)
        unTranspose[trailDims-1] = newTrail
        unTranspose[dimension-1] = 0
    endelse

    return, result
end




;-----------------------------------------------------
;Main Level Example Program: IDL> .r MrConcatenate) \\
;-----------------------------------------------------

;EXAMPLE
;   A dimension of array gets truncated during the reform
array  = findgen(5,1,13)
result = MrPutDimensionFirst(array, 3, UNREFORM=unReform, UNTRANSPOSE=unTranspose)
undo   = transpose(reform(result, unReform), unTranspose)
print, '---------------------------------------------------'
print, 'PUT 3rd DIMENSION FIRST:'
help, array
help, result
help, undo
print, ''

;EXAMPLE
;   Several shallow dimensions are present, but none are truncated. No need to
;   reform.
array  = findgen(5,1,6,1,3,13)
result = MrPutDimensionFirst(array, 3, UNREFORM=unReform, UNTRANSPOSE=unTranspose)
undo   = transpose(result, unTranspose)
print, '---------------------------------------------------'
print, 'PUT 3rd DIMENSION FIRST:'
help, array
help, result
help, undo
print, ''

;EXAMPLE
;   Several shallow dimensions are present. Dimensions are truncated so a reform
;   is needed to put them back.
array  = findgen(1,1,1,1,13)
result = MrPutDimensionFirst(array, 5, UNREFORM=unReform, UNTRANSPOSE=unTranspose)
undo   = transpose(reform(result, unReform), unTranspose)
print, '---------------------------------------------------'
print, 'PUT 5th DIMENSION FIRST:'
help, array
help, result
help, undo
print, ''

;EXAMPLE
;   Several shallow dimensions are present. Dimensions are truncated so a reform
;   is needed to put them back.
array  = findgen(5,1,6,3,1,13)
result = MrPutDimensionFirst(array, 6, UNREFORM=unReform, UNTRANSPOSE=unTranspose)
undo   = transpose(reform(result, unReform), unTranspose)
print, '---------------------------------------------------'
print, 'PUT 6th DIMENSION FIRST:'
help, array
help, result
help, undo
print, ''

end