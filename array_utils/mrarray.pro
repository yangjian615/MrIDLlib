

;+
;   The initialization method.
;
; :Params:
;       ARRAY:              in, optional, type=any/integer
;                           If an array, then it is the array to be stored and all other
;                               parameters are ignored. If a scalar integer, the size
;                               if the first dimension in the resulting array.
;       D2:                 in, optional, type=integer
;                           Size of the second dimension. Ignored unless `ARRAY` is scalar.
;       D3:                 in, optional, type=integer
;                           Size of the third dimension. Ignored unless `ARRAY` is scalar.
;       D4:                 in, optional, type=integer
;                           Size of the fourth dimension. Ignored unless `ARRAY` is scalar.
;       D5:                 in, optional, type=integer
;                           Size of the fifth dimension. Ignored unless `ARRAY` is scalar.
;       D6:                 in, optional, type=integer
;                           Size of the sixth dimension. Ignored unless `ARRAY` is scalar.
;       D7:                 in, optional, type=integer
;                           Size of the seventh dimension. Ignored unless `ARRAY` is scalar.
;       D8:                 in, optional, type=integer
;                           Size of the eight dimension. Ignored unless `ARRAY` is scalar.
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, `ARRAY` will be copied directly into the object and
;                               will be left undefined.
;       TYPE:               in, optional, type=int/float, default='FLOAT'
;                           The type of array to be created. Ignored unless `ARRAY` is scalar
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's Make_Array procedure. Ignored
;                               unless `ARRAY` is scalar.
;
; :Returns:
;       ARRAY:              A MrArray object reference.
;-
function MrArray, array, D2, D3, D4, D5, D6, D7, D8, $
NO_COPY=no_copy, $
TYPE=type, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Create the array object
    objref = obj_new('MrArray', array, D2, D3, D4, D5, D6, D7, D8, $
                                NO_COPY=no_copy, TYPE=type, _EXTRA=extra)
    
    return, objref
end
