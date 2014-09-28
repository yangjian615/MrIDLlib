; docformat = 'rst'
;
; NAME:
;       MrInnerProduct
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
; PURPOSE
;+
;   A wrapper for the # operator or the Matrix_Multiply function, generalized to rank N
;   tensors. IDL's Matrix_Multiply truncate dimensions beyond rank 2.
;
;   For two IDL tensors, A and B, the last dimension of A will be contracted with the
;   first dimension of B.
;
; :Examples:
;   See the example program at the end of this document::
;       IDL> .r MrInnerProduct
;
; :Params:
;       A:          in, required, type=tensor rank n
;                   Rank N tensor whose last dimension will be contracted with the
;                       first dimension of `B`.
;       B:          in, required, type=tensor rank n
;                   Rank N tensor whose first dimension will be contracted with the
;                       last dimension of `A`.
;
; :Returns:
;       RESULT:     Result of the inner product.
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
;	Modification History::
;       2014/04/04  -   Written by Matthew Argall
;-
function MrInnerProduct, A, B
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Dimensionality of inputs
    nDimsA = size(A, /N_DIMENSIONS)
    nDimsB = size(B, /N_DIMENSIONS)
    dimsA = size(A, /DIMENSIONS)
    dimsB = size(B, /DIMENSIONS)
    
    ;Reform _A and B so that they have two dimensions
    _A = (nDimsA le 2) ? A : reform(A, product(dimsA[0:nDimsA-2]), dimsA[nDimsA-1])
    _B = (nDimsB le 2) ? B : reform(B, dimsB[0], product(dimsB[1:*]))

    ;Compute the inner product
    result = temporary(_A) # temporary(_B)

    ;Reform back to the original shape, minus the contracted dimension.
    if nDimsA gt 2 then dimsOut = dimsA[0:nDimsA-2]
    if nDimsB gt 2 then begin
        if n_elements(dimsOut) gt 0 $
            then dimsOut = [dimsOut, dimsB[1:*]] $
            else dimsOut = dimsB[1:*]
    endif
    if n_elements(dimsOut) gt 0 $
        then result = reform(result, dimsOut, /OVERWRITE)
        
    return, result
end


;---------------------------------------------------
; Main Level Example Program (.r MrInnerProduct) ///
;---------------------------------------------------

;EXAMPLE 1
;   For 2D matrices, MrInnerProduct is the same as the # operator
A = indgen(2,3)
B = indgen(3,2)
inner = MrInnerProduct(A, B)

print, '---------------------------------------------------------'
print, 'Inner product of two matrices:'
print, FORMAT='(%"   |%2i %2i|  |%2i %2i %2i|   |%2i %2i|")', A[*,0], B[*,0], inner[*,0]
print, FORMAT='(%"   |%2i %2i|  |%2i %2i %2i| = |%2i %2i|")', A[*,1], B[*,1], inner[*,1]
print, FORMAT='(%"   |%2i %2i|")', A[*,2]
print, ''


;EXAMPLE 2
;   Test higher rank tensors
A = indgen(5,3,7)
B = indgen(7,12,4,2)
inner = MrInnerProduct(A, B)

dimsA = size(A, /DIMENSIONS)
dimsB = size(B, /DIMENSIONS)
dimsI = size(inner, /DIMENSIONS)
print, '---------------------------------------------------------'
print, 'Inner Product between Rank 3 and Rank 4 Tensors.'
print, FORMAT='(%"   Dimensions of A:             [%i, %i, %i]")', dimsA
print, FORMAT='(%"   Dimensions of B:             [%i, %i, %i, %i]")', dimsB
print, FORMAT='(%"   Dimensions of Inner Product: [%i, %i, %i, %i, %i]")', dimsI
end