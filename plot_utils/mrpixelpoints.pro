; docformat = 'rst'
;
; NAME:
;       MrPixelPoints
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
;   A normal set of data coordinates can be used to logarithmically size image pixels for
;   display on a log-scale plot. Output parameters can be supplied directly to POLYFILL::
;
;       for i = 0, image_dims[0]-1  do begin
;           for i = 0, image_dims[1] - 1 do begin
;               polyfill, [Xmin[i,j], Xmax[i,j], Xmax[i,j], Xmin[i,j]], $
;                         [Ymin[i,j], Ymin[i,j], Ymax[i,j], Ymax[i,j]], $
;                         COLOR=image[i,j]
;           endfor
;       endfor
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
;                                and: `align_center.pro <http://spdf.gsfc.nasa.gov/pub/software/cdawlib/source/align_center.pro>`
;                       which is part of the CDAWlib library.
;
;-
;*****************************************************************************************
;+
;   Given the pixel centers, the purpose of this program is to calculate the location of the
;   borders of each pixel.
;
; :Params:
;       X0:             in, required, type=numeric array
;                       The x- or y- positions of the centers of a set of pixels.
;       XMIN1:          out, required, type=numeric array
;                       The location of the lower edge of the pixels.
;       XMAX1:          out, required, type=numeric array
;                       The location of the upper edge of the pixels.
;-
pro MrPixelPoints_CenterPixel, X0, Xmin1, Xmax1
    compile_opt strictarr
    on_error, 2
    
    ;Copy the input array to the output arrays
    Xmin1 = X0
    Xmax1 = X0
    
    ;Find finite/non-NaN values
    iFinite = where(finite(X0) eq 1, nFinite) ; find all real values
        
;---------------------------------------------------------------------
;Linear Spacing //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nFinite gt 0 then begin
        
        ;Select the finite values
        X = X0[iFinite]
        
        ;
        ; Original "align_center" code has 
        ;
        ;   nX = n_elements(X0)
        ;
        ; but I think this is a typo. This would imply that we are removing the grid cells
        ; with NaNs in them entirely, thus increacing the size and shifting the center
        ; of the remaining cells. By setting
        ;
        ;   nX = n_elements(X)
        ;
        ; we are interpolating over the NaNs, leaving the number of grid cells as well as
        ; their size and center alone.
        ;
        nX = n_elements(X0)
        
        ;Check the spacing between points
        dx1 = abs(X[1]-X[0])
        dx2 = abs(X[2]-X[1])
        
    ;---------------------------------------------------------------------
    ;Linear Spacing //////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if (abs(dx1-dx2) lt 1.e-6 * min([dx1, dx2])) then begin
            
            ;Create an array from 1 to nX
            w = lindgen(nX)+1
            
            ;Create an array from 0 to nX-1
            Xt = dindgen(nX)
            
            ;Spline interpolate the values of X onto a linearly spaced grid
            ;
            ;   Xt        are the independent variable values
            ;   X = F(Xt) are the   dependent variable values
            ;
            ;It does not matter what Xt is, so long as the spacing of Xt is linear, which
            ;it is. By adding "outside" points below, we are extrapolating the data to pad
            ;it. The SPL_INTERP function forces the slope to be zero at the end points
            ;(unless otherwise specified). The spline interpolation serves to fill values
            ;that were removed with the check for finite values.
            ;
            ;   SPL_INIT   - calculates the second derivative of Xt at points X (should
            ;                be zero for linearly spaced, monotonically increasing values)
            ;   SPL_INTERP - then uses the second derivative to ensure a smooth 1st
            ;                derivative and continuous 2nd derivative at points X
            sCoef = spl_init(Xt,X)                          ;setup spline coefficients
            Xs = spl_interp(Xt,X,sCoef,dindgen(nX+2)-1.)    ;add outside points
            
            ;Xs represents the center of each point. Xmin and Xmax are the distances to
            ;the edges of the box surrounding Xs.
            ;   This is essetially just a shifted difference. It works because we have
            ;   padded the results with one point on either side of the array.
            Xmin = (Xs[w-1] + Xs[w]) / 2.
            Xmax = (Xs[w] + Xs[w+1]) / 2.
        
    ;---------------------------------------------------------------------
    ;Log Spacing /////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
        
            ;Values that are <= 0 will be set explicitly to 0. Take the log of all
            ;other values. X is exponentially spaced, so ALOGX will be linearly spaced.
            alogX = MrLog(X, MISSING_VALUE=0)
            
            ;Create a linearly spaced array from 0 to nX-1
            Xt = dindgen(nX)
            
            ;Spline interpolate
            sCoef = spl_init(Xt,alogX)                           ; setup spline coefficients
            alogXs = spl_interp(Xt,alogX,sCoef,dindgen(nX+2)-1.) ; add outside points
            
            ;Create a linearly spaced array from 1 to nX
            w = lindgen(nX)+1
            
            ;Calculate the width of each cell.
            Xmin = 10^((alogXs[w-1] + alogXs[w]) / 2.)
            Xmax = 10^((alogXs[w] + alogXs[w+1]) / 2.)
        endelse
        
        Xmin1[iFinite] = Xmin
        Xmax1[iFinite] = Xmax
    endif 
end


;+
;   The purpose of this program is to calculate the location of the lower-left and
;   upper-right corners of a set of pixels. Each pixel may have a unique size.
;
;   A normal set of data coordinates can be used to logarithmically size image pixels for
;   display on a log-scale plot. Output parameters can be supplied directly to POLYFILL::
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
;                       The horizontal position on the display of the lower-left corner
;                           of each pixel.
;       Y:              in, optional, type=M or NxM numeric array
;                       The vertical position on the display of the lower-left corner
;                           of each pixel.
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
;       CENTER:         in, optional, type=boolean, default=0
;                       If set, `X` and `Y` are the data locations of the center of each
;                           pixel to be painted on the display.
;       DIMENSIONS:     in, optional, type=boolean, default=0
;                       If set, `IMAGE` represents the dimensions of and image, not
;                           an actual image.
;       XLOG:           in, optional, type=boolean, default=0
;                       If set, the X-axis will be log-scaled.
;       YLOG:           in, optional, type=boolean, default=0
;                       If set, the Y-axis will be log-scaled.
;-
pro MrPixelPoints, image, x, y, Xmin, Ymin, Xmax, Ymax, $
 CENTER = center, $
 DIMENSIONS = dimensions, $
 XLOG = xlog, $
 YLOG = ylog
    compile_opt strictarr
    on_error, 2
        
    if keyword_set(dimensions) $
        then dims = image $
        else dims = size(image, /DIMENSIONS)
    
    nx     = n_elements(x)
    ny     = n_elements(y)   
    center = keyword_set(center)
    xlog   = keyword_set(xlog)
    ylog   = keyword_set(ylog)

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
;Normal Image ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    Xmin = temporary(xx)
    Ymin = temporary(yy)

    ;X and Y mark the center of the pixels
    if center then begin
        ;Allocate memory
        Xmax = Xmin
        Ymax = Ymin

        ;Calculate the x-coordinate of the lower-left and upper-right corner of
        ;each pixel, centered on X.
        for i = 0L, dims[1]-1 do begin
            ;Log spacing is taken care of in _CenterPixel
            MrPixelPoints_CenterPixel, Xmin[*,i], temp_xmin, temp_xmax
            Xmin[*,i] = temporary(temp_xmin)
            Xmax[*,i] = temporary(temp_xmax)
        endfor
        
        ;Calculate the y-coordinate of the lower-left and upper-right corner of
        ;each pixel, centered on Y.
        for i = 0L, dims[0]-1 do begin
            ;Log spacing is taken care of in ALIGN_CENTER
            MrPixelPoints_CenterPixel, Ymin[i,*], temp_ymin, temp_ymax
            Ymin[i,*] = temporary(temp_ymin)
            Ymax[i,*] = temporary(temp_ymax)
        endfor
        
    ;X and Y mark the bottom left corner of the pixels
    endif else begin
        ;Shift everything to the left (down) by one column (row). The right (bottom) 
        ;boundary of the first pixel is equal to the left (top) boundary of the
        ;second pixel.
        Xmax = shift(Xmin, -1,  0)
        Ymax = shift(Ymin,  0, -1)
        
        ;Xmin is the difference between X[i] and X[i-1]. On a linear scale, this should
        ;be independent of the particular choice of i. Below, replace the wrapped value
        ;with, e.g.
        ;   Xn + delta(X) ...
        ;   Xmin[n-1,*] + (Xmin[n-1,*] - Xmin[n-2,*]) = 2*Xmin[n-1,*] - Xmin[n-2,*]
        if xlog $
            then Xmax[dims[0]-1,*] = 10^(2*alog10(Xmin[dims[0]-1,*]) - alog10(Xmin[dims[0]-2,*])) $
            else Xmax[dims[0]-1,*] = Xmin[dims[0]-1,*]*2 - Xmin[dims[0]-2,*]
        
        if ylog $
            then Ymax[*,dims[1]-1] = 10^(2*alog10(Ymin[*,dims[1]-1]) - alog10(Ymin[*,dims[1]-2])) $
            else Ymax[*,dims[1]-1] = Ymin[*,dims[1]-1]*2 - Ymin[*,dims[1]-2]
    endelse
end