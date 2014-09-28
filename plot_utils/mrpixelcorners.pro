; docformat = 'rst'
;
; NAME:
;       MrPixelCorners
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
;   The purpose of this program is to calculate the location of the lower-left and
;   upper-right corners of a set of pixels. Each pixel may have a unique size.
;
;   The input parameters are made to be the same size as IMAGE and are returned. Output
;   parameters can be supplied directly to POLYFILL::
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
;       X0:             in, required, type=NxM numeric
;                       The horizontal location of the lower-left corner of each pixel
;       Y0:             in, required, type=NxM numeric
;                       The vertical location of the lower-left corner of each pixel
;       X1:             in, required, type=NxM numeric
;                       The horizontal location of the upper-right corner of each pixel
;       Y1:             in, required, type=NxM numeric
;                       The vertical location of the upper-right corner of each pixel
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
pro MrPixelCorners, image, x0, y0, x1, y1, Xmin, Ymin, Xmax, Ymax, $
DIMENSIONS=dimensions
    compile_opt strictarr
    on_error, 2
        
    if keyword_set(dimensions) $
        then dims = image $
        else dims = size(image, /DIMENSIONS)
    
    nXmin = n_elements(x0)
    nXmax = n_elements(x1)
    nYmin = n_elements(y0)
    nYmax = n_elements(y1)

;---------------------------------------------------------------------
;XMIN ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case nXmin of
        1:               Xmin = replicate(x0, dims)
        dims[0]:         Xmin = rebin(reform(x0), dims)
        
        ;Make sure DELTAX_MINUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            xDims = size(x0, /DIMENSIONS)
            case 1 of
                array_equal(xDims, dims): Xmin = x0
                array_equal(xDims, dims[[1,0]]): Xmin = transpose(x0)
                else: message, 'XMIN: incorrect size.'
            endcase
        endcase
        else: message, 'XMIN: Incorrect number of elements.'
    endcase

;---------------------------------------------------------------------
;XMAX ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case nXmin of
        1:               Xmax = replicate(x1, dims)
        dims[0]:         Xmax = rebin(reform(x1), dims)
        
        ;Make sure DELTAX_MINUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            xDims = size(x1, /DIMENSIONS)
            case 1 of
                array_equal(xDims, dims): Xmax = x1
                array_equal(xDims, dims[[1,0]]): Xmax = transpose(x1)
                else: message, 'XMAX: incorrect size.'
            endcase
        endcase
        else: message, 'XMAX: Incorrect number of elements.'
    endcase
    
;---------------------------------------------------------------------
;YMIN ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case nYmin of
        1:               Ymin = replicate(y0, dims)
        dims[1]:         Ymin = rebin(reform(y0, 1, dims[1]), dims)
        
        ;Make sure DELTAX_MINUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            yDims = size(y0, /DIMENSIONS)
            case 1 of
                array_equal(yDims, dims): Ymin = y0
                array_equal(yDims, dims[[1,0]]): Ymin = transpose(y0)
                else: message, 'YMIN: incorrect size.'
            endcase
        endcase
        else: message, 'YMIN: Incorrect number of elements.'
    endcase

;---------------------------------------------------------------------
;YMAX ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case nYmax of
        1:               Ymax = replicate(y1, dims)
        dims[1]:         Ymax = rebin(reform(y1, 1, dims[1]), dims)
        
        ;Make sure DELTAX_MINUS is not transposed and its dimensions are the right size
        dims[0]*dims[1]: begin
            yDims = size(y1, /DIMENSIONS)
            case 1 of
                array_equal(yDims, dims): Ymax = y1
                array_equal(yDims, dims[[1,0]]): Ymax = transpose(y1)
                else: message, 'YMAX: incorrect size.'
            endcase
        endcase
        else: message, 'YMAX: Incorrect number of elements.'
    endcase
end
