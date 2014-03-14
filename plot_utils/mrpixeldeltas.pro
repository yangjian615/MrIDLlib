; docformat = 'rst'
;
; NAME:
;       MrPixelDeltas
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
;   The purpose of this program is to calculate the location of the lower-left and upper-
;   right corners a set of pixels. Each pixel may have a unique size.
;
;   The input parameters can be used to define pixel sizes. Output parameters can be
;   supplied directly to POLYFILL::
;
;       for i = 0, image_dims[0]-1  do begin
;           for i = 0, image_dims[1] - 1 do begin
;               polyfill, [Xmin[i,j], Xmax[i,j], Xmax[i,j], Xmin[i,j]], $
;                         [Ymin[i,j], Ymin[i,j], Ymax[i,j], Ymax[i,j]], $
;                         COLOR=image[i,j]
;           endfor
;       endfor
;
; :Params:
;       IMAGE           in, required, type=NxM numeric
;                       An image whose dimensions will be used to correctly size the
;                           other input parameters.
;       X:              in, optional, type=M or NxM numeric array
;                       A vector the same size as the first dimension of `IMAGE` or a
;                           2D array the same size as `IMAGE` that marks the horizontal
;                           reference position from which `DX0` and `DX1` are measured.
;       Y:              in, optional, type=M or NxM numeric array
;                       A vector the same size as the second dimension of `IMAGE` or a
;                           2D array the same size as `IMAGE` that marks the vertical
;                           reference position from which `DX0` and `DX1` are measured.
;       DX0:            in, required, type=scalar\, N-element array\, NxM array
;                       The horizontal offset from `X` where the lower-left corner
;                           of each pixel is located.
;       DY0:            in, required, type=scalar\, M-element array\, NxM array
;                       The vertical offset from `Y` where the lower-left corner
;                           of each pixel is located.
;       DX1:            in, required, type=scalar\, N-element array\, NxM array
;                       The horizontal offset from `X` where the upper-right corner
;                           of each pixel is located.
;       DY1:            in, required, type=scalar\, N-element array\, NxM array
;                       The horizontal vertical offset from `Y` where the upper-right
;                           corner of each pixel is located.
;       XMIN:           out, type=NxM numeric
;                       The horizontal location of the lower-left corner of each pixel
;       YMIN:           out, type=NxM numeric
;                       The vertical location of the lower-left corner of each pixel
;       XMAX:           out, type=NxM numeric
;                       The horizontal location of the upper-right corner of each pixel
;       YMAX:           out, type=NxM numeric
;                       The vertical location of the upper-right corner of each pixel
;
; :Keywords:
;       DIMENSIONS:     in, optional, type=boolean, default=0
;                       If set, `IMAGE` represents the dimensions of and image, not
;                           an actual image.
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
;   2013/11/16  -   Written by Matthew Argall
;                       Adapted from: `spectrogram.pro <http://sprg.ssl.berkeley.edu/~davin/idl/socware_old1/external/CDAWlib/spectrogram.pro>`
;                       which is part of the CDAWlib library.
;-
pro MrPixelDeltas, image, x, y, dx0, dy0, dx1, dy1, Xmin, Ymin, Xmax, Ymax, $
DIMENSIONS=dimensions
    compile_opt strictarr
    on_error, 2
        
    if keyword_set(dimensions) $
        then dims = image $
        else dims = size(image, /DIMENSIONS)
    
    nx = n_elements(x)
    ny = n_elements(y)
    ndx0 = n_elements(dx0)
    ndx1 = n_elements(dx1)
    ndy0 = n_elements(dy0)
    ndy1 = n_elements(dy1)

    ;Make sure X has the same number of elements as IMAGE
    case nx of
        dims[0]: xx = rebin(reform(x), dims)
        dims[0]*dims[1]: begin
            xDims = size(x, /DIMENSIONS)
            case 1 of
                array_equal(xDims, dims): xx = x
                array_equal(xDims, dims[[1,0]]): xx = transpose(x)
                else: message, 'X: incorrect size.'
            endcase
        endcase
        else: message, 'X and IMAGE[i,*] must have the same number of elements.'
    endcase

    ;Make sure Y has the same number of elements as IMAGE
    case ny of
        dims[1]: yy = rebin(reform(y, 1, dims[1]), dims)
        dims[0]*dims[1]: begin
            yDims = size(y, /DIMENSIONS)
            case 1 of
                array_equal(yDims, dims): yy = y
                array_equal(yDims, dims[[1,0]]): yy = transpose(y)
                else: message, 'Y: incorrect size.'
            endcase
        endcase
        else: message, 'Y and IMAGE[*,i] must have the same number of elements.'
    endcase

;---------------------------------------------------------------------
;DX0 /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case ndx0 of
        1:       dX_minus = replicate(dx0, dims)
        dims[0]: dX_minus = rebin(reform(dx0), dims)
        
        ;Make sure DELTAX_MINUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            xDims = size(dx0, /DIMENSIONS)
            case 1 of
                array_equal(xDims, dims): dX_minus = dx0
                array_equal(xDims, dims[[1,0]]): dX_minus = transpose(dx0)
                else: message, 'DX0: incorrect size.'
            endcase
        endcase
        else: message, 'DX0: Incorrect number of elements.'
    endcase

;---------------------------------------------------------------------
;DX1 /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case ndx1 of
        1:       dX_plus = replicate(dx1, dims)
        dims[0]: dX_plus = rebin(reform(dx1), dims)
        
        ;Make sure DELTAX_PLUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            xDims = size(dx1, /DIMENSIONS)
            case 1 of
                array_equal(xDims, dims): dX_plus = dx1
                array_equal(xDims, dims[[1,0]]): dX_plus = transpose(dx1)
                else: message, 'DX1: incorrect size.'
            endcase
        endcase
        else: message, 'DX1: Incorrect number of elements.'
    endcase

;---------------------------------------------------------------------
;DY0 /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case ndy0 of
        1:       dY_minus = replicate(dy0, dims)
        dims[0]: dY_minus = rebin(reform(dy0), dims)

        ;Make sure DELTAY_MINUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            yDims = size(dy0, /DIMENSIONS)
            case 1 of
                array_equal(yDims, dims):        dY_minus = dy0
                array_equal(yDims, dims[[1,0]]): dY_minus = transpose(dy0)
                else: message, 'DY0: incorrect size.'
            endcase
        endcase
        else: message, 'DY0: Incorrect number of elements.'
    endcase

;---------------------------------------------------------------------
;DY1 /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case ndy1 of
        1:               dY_plus = replicate(dy1, dims)
        dims[0]:         dY_plus = rebin(reform(dy1), dims)

        ;Make sure DELTAY_MINUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            yDims = size(dy1, /DIMENSIONS)
            case 1 of
                array_equal(yDims, dims): dY_plus = dy1
                array_equal(yDims, dims[[1,0]]): dY_plus = transpose(dy1)
                else: message, 'DY1: incorrect size.'
            endcase
        endcase
        else: message, 'DY1: Incorrect number of elements.'
    endcase
    
;---------------------------------------------------------------------
;Outline Each Pixel //////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Create the min and max arrays that specify the lower-left and upper-right corner
    ;of each pixel.
    Xmin = xx - dX_minus
    Xmax = temporary(xx) + dX_plus
    Ymin = yy - dY_minus
    Ymax = temporary(yy) + dY_plus
end
