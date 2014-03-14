; docformat = 'rst'
;
; NAME:
;       MRAIMAGE
;
;*****************************************************************************************
;   Copyright (c) 2011, Matthew Argall                                                   ;
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
;       A Wrapper for the TV procedure. Additions include:
;           o Draw an axes around the image
;           o Image can be placed in a subregion of a plot
;           o Image can be scaled before it is displayed
;           o Place a colorbar next to the image
;           o Set the range of the image to be displayed
;           o NaNs can be avoided
;           o Missing values can be avoided and/or given their own color
;           o Can paint the image pixel-by-pixel
;               - Allows pixels to be unequal sizes
;               - Allows for [XY]Log keywords.
;
; :Calling Sequence:
;   MraImage, image
;   MraImage, image, x, y
;   MraImage, image, Xmin, Ymin, Xmax, Ymax
;   MraImage, image, x, y, deltaX_minus, deltaY_minus, deltaX_plus, deltaY_plus
;
; :Examples:
;   All examples use the following data::
;       image = dist(256)
;       x = findgen(256)
;       y = findgen(256)
;
;   Plot image data::
;       image = dist(256)
;       MraImage, image
;
;   Image data, add axes and color table::
;       image = dist(256)
;       MraImage, image, /AXES, CTINDEX=11
;
;   Image data with x- and y-values, scaled with axes::
;       image = dist(256)
;       x = findgen(256)
;       y = findgen(256)
;       MraImage, image, x, y, /SCALE, /AXES, CTINDEX=11
;
;   Add NaNs and missing data::
;       bad_image = image
;       bad_image[0:10,0:10] = !values.f_nan
;       bad_image[245:255,245,155] = 300
;       MraImage, bad_image, x, y, /AXES, /SCALE, CTINDEX=11, MISSING_VALUE=300, MISSING_COLOR='Blue', /NAN
;
;   Add a colorbar::
;       MraImage, image, x, y, /AXES, /SCALE, CTINDEX=11, /ADDCOLORBAR
;
;   Place the image between 25 <= x <= 175
;       ymin = min(y)
;       ymax = max(y)
;       MraImage, image, x, y, /AXES, DATA_POS=[25,ymin,175,ymax]
;
; :Categories:
;       Data Visualization, Wrapper
;
; :Uses:
;   Uses the following external programs::
;       getdecomposedstate.pro (Coyote Graphics)
;       error_message.pro (Coyote Graphics)
;       cgCheckForSymbols (Coyote Graphics)
;       cgColor.pro (Coyote Graphics)
;       color_bar.pro
;       MrGetWindow.pro
;       MrLog.pro
;       MrPlotLayout.pro
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
;
;       Written by  -   Matthew Argall 27 October 2011
;       01/26/2012  -   added the DATA_POS keyword
;       04/24/2012  -   removed DEVICE, NORMAL, POSITION, NOERASE.
;                           renamed PLOT_EXTRA to _EXTRA
;                           done in an effort to leave all keyword inheritance
;                           to be managed by PLOT and to have IMAGE_PLOTS only
;                           add functionality.
;       05/25/2012  -   Added the AXES keywords to make the program
;                           act as a wrapper for the TV function (same default
;                           features as TV)
;                       Organized the keyword checking better
;                       Added DEVICE, NORMAL, and POSITION keywords again
;                           so that the image can be sized properly
;       11/14/2012  -   Added MINVALUE and MAXVALUE keywords. 
;                       Now check to see if the upper-right and lower-left corners
;                           supplied by DATA_POS are the same. If they are, do not use the
;                           data position.
;                       Added keyword COLORBAR
;       11/15/2012  -   If X and/or Y are given without setting AXES or DATA_POS, all PLOT
;                           actions avoided and the image goes straight to TV, which does
;                           not have an ERASE keyword. Thus, if the first plot in a set
;                           of plots, then these conditions would cause the display to not
;                           be erased.
;                       It turns out that if NOERASE is set, the background color of the
;                           plot will not be changed. This means an empty plot must be
;                           drawn before TV is called in order to set the background color.
;       11/18/2012  -   If IMAGE is NxM, then X and Y may now be N and M element arrays,
;                           respectively, that specify the data coordinates of IMAGE. This
;                           is useful for defining a coordinate system with /AXES.
;       11/19/2012  -   The color bar is now drawn before the image so that, upon exit,
;                           the !P, !X, and !Y system variables contain information about
;                           image, not about the color bar.
;       11/27/2012  -   If AXES is called, but POSITION is not defined, the !X and !Y
;                           system variables are used in determining the position of the
;                           image.
;       02/27/2013  -   Added NAN keyword. Treat infinities as NANs when creating the
;                           color bar axis if the NAN keyword is set. This prevents
;                           the infinite axis range error. MINVALUE and MAXVALUE now
;                           checked with other keywords, not in 3 places. - MRA
;       03/18/2013  -   After POSITION is changed to device coordinates, set /DEVICE
;                           in call to COLOR_BAR.PRO so that it is displayed properly.
;                           Added the COLOR_TABLE keyword. Removed the PS keyword. - MRA
;       04/12/2013  -   Renamed the COLOR_TABLE index to CTINDEX because it conflicted
;                           with the COLOR keyword. - MRA
;       04/13/2013  -   NOERASE=0 was causing the axes to erase the colorbar. Fixed. - MRA
;       04/14/2013  -   Added MISSING_VALUE, MISSING_COLOR, TOP, BOTTOM, and IMRANGE
;                           keywords. - MRA
;       04/17/2013  -   Default to using whatever color table is loaded. Do not load one
;                           unless CTINDEX is set to something. - MRA
;       05/03/2013  -   Added AXISCOLOR, renamed axiscolor to COLOR and background to
;                           BACKGROUND to be more consistent with IDL and to prepare for
;                           integration  with Coyote Graphics. Reset NOERASE to initial
;                           value upon exit. - MRA
;       05/11/2013  -   Check device name before plotting (e.g. PostScript). - MRA
;       08/03/2013  -   Added the PALETTE keyword. - MRA
;       08/12/2013  -   Added the LOG keyword. - MRA
;       09/07/2013  -   Removed the AXISCOLOR keyword in favor of the COLOR keyword. IMAGE
;                           is scaled if its range is not between 0 and 255. It is also
;                           scaled if MINVALUE, MAXVALUE, RANGE, or DPOSITION are set. If
;                           no position is defined (including !P.Position and !P.Multi),
;                           then one will be created. DPOSITION is handled after AXES so
;                           the coordinate system is already set-up. Add the colorbar
;                           after the image is displayed so that COLOR and BACKGROUND
;                           colors can be removed from the color table before draing.
;                           Separate MISSING_VALUE and NAN checks from byte-scaling. - MRA
;       09/08/2013  -   The DATA_POS keyword is not working, as far as I can tell. Now
;                           using cgColor instead of load_color.pro. Added examples. - MRA
;       09/26/2013  -   Use the Coyote Graphics default position of [0.125, 0.125, 0.925, 0.9]
;                           when no position is provided. Return it through the keyword. - MRA
;       10/09/2013  -   Draw axes after the image is displayed so that tick marks are on
;                           top of the image. If no window is open, create one. If NOERASE
;                           is not set, use cgErase to set the background color of the
;                           window.  - MRA
;       2013/10/25  -   Renamed DATA_POS to DPOSITION to avoid conflict with DATA. Needed
;                           to draw an invisible set of axes if DATA_POS is provided. - MRA
;       2013/10/28  -   Added the TITLE, XTITLE, and YTITLE keywords so that symbols can
;                           be used in the titles via cgCheckForSymbols. - MRA
;       2013/11/12  -   Renamed from IMAGE_PLOTS to MrImage.pro. Added the XLOG, YLOG,
;                           DELTAX_MINUS, DELTAX_PLUS, DELTAY_MINUS, and DELTAY_PLUS
;                           keywords. Now use cgLoadCT when CTINDEX is provided. Added
;                           the MrImage_Paint and MrImage_AlignCenter functions. - MRA.
;       2013/11/27  -   Renamed from MrImage.pro to MraImage.pro to resolve conflict with
;                           the MrImage function. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this program is to display an image.
;
; :Params:
;       IMAGE:          in, required, type=NxM numeric array
;                       Image to be displayed
;       X:              in, optional, type=scalar/Nx1 numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the first dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       Y:              in, optional, type=scalar/Mx1 numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the second dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       X0:             in, optional, type=1D vector/2D array
;                       The x-coordinate, in data coordinates, of the upper-right corner
;                           if each pixel in `IMAGE`. If supplied, then `X` markes the
;                           x-coordinate of the lower-left corner of each pixel in `IMAGE`.
;                           Must be used with `Y0`. If given, `PAINT` will be set to 1.
;       Y0:             in, optional, type=1D vector/2D array
;                       The y-coordinate, in data coordinates, of the upper-right corner
;                           if each pixel in `IMAGE`. If supplied, then `Y` markes the
;                           y-coordinate of the lower-left corner of each pixel in `IMAGE`.
;                           Must be used with `X0`. If given, `PAINT` will be set to 1.
;       X1:             in, optional, type=scalar/1D vector/2D array
;                       If provided, then `X` and `Y` are the "center" locations of each
;                           pixel of `IMAGE`. `X0` and X1 mark the displacement to the
;                           left and right edges of each pixel, respectively. X1 and `Y1`
;                           must be supplied together. If provided, `PAINT` will be set.
;       Y1:             in, optional, type=scalar/1D vector/2D array
;                       If provided, then `X` and `Y` are the "center" locations of each
;                           pixel of `IMAGE`. `Y0` and Y1 mark the displacement to the
;                           bottom and top edges of each pixel, respectively. `X1` and Y1
;                           must be supplied together. If provided, `PAINT` will be set.
;                       
;
; :Keywords:
;       AXES:           in, optional, type=Boolean, default=0
;                       Draw a coordinate axes around the image.
;       BACKGROUND:     in, optional, type=string/color index/color triple, default='white'
;                       Overrides the BACKGROUND keyword in PLOT. Determines the color of
;                           the background on top of which the image will be displayed.
;       ADDCOLORBAR:    in, optional, type=Boolean, default=0,
;                       Add a colorbar to the image.
;       BOTTOM:         in, optional, type=byte, default=0
;                       If `SCALE` is set, this is the minimum value of the scaled image.
;       CENTER:         in, optional, type=boolean, default=0
;                       If set, `X` adn `Y` are the data locations of the center of each
;                           pixel to be painted on the display. Ignored if `PAINT`=0.
;       COLOR:          in, optional, type=string/color index/color triple, default='Black'
;                       Overrides the COLOR keyword in PLOT. Determines the color of
;                           the foreground used in drawing axes and annotations.
;       CTINDEX:        in, optional, type=int, default=13
;                       Color table to be loaded prior to displaying the image.
;       DPOSITION:      in, optional, type=fltarr(4), default=fltarr(4)
;                       The position, in data coordinates, of the lower left and upper 
;                           right corners of the image to be displayed.
;                           If the image to be plotted is not suppose to fill the data
;			                axes, then these are the data coordinates that specify the
;			                subregion of the plot in which to put the image. DATA_POS
;                           is converted to device coordinates using the CONVERT_COORD. If
;                           `XRANGE` or `YRANGE` are not provided, then the scaling factors
;                           of the previously drawn plot will be used (i.e. ![P,X,Y]).
;                           If the lower right and upper left coordinates are the same,
;                           this keyword is ignored. Setting this keyword automatically
;                           sets `SCALE`=1.
;       DEVICE:         in, optional, type=Boolean, default=0
;                       Indicate that POSITION is supplied in device coordinates. The
;                           default is NORMAL
;       LOG:            in, optional, type=Boolean, default=0
;                       Take the log of the image before plotting it.
;       MAXVALUE:       in, optional, type=float, default=MAX(IMAGE)
;                       The value considered to be the maximum when byte scaling the image.
;                           If not supplied, or if it equals `MINVALUE`, the image will be
;                           searched for its maximum value. Setting MAXVALUE automatically
;                           sets the `SCALE` keyword. If Max(`IMAGE`) > 255, then MAXVALUE
;                           is set and `SCALE`=1.
;       MINVALUE:       in, optional, type=float, default=MIN(IMAGE)
;                       The value considered to be the minimum when byte scaling the image.
;                           If not supplied, or if it equals `MAXVALUE`, the image will be
;                           searched for its minimum value. Setting MINVALUE automatically
;                           sets the `SCALE` keyword. If Min(`IMAGE`) < 0, then MINVALUE
;                           is set and `SCALE`=1.
;       MISSING_VALUE:  in, optional, type=numeric
;                       The missing value of the image.
;       MISSING_COLOR:  in, optional, type=string, default=`background`
;                       The color name to color the `MISSING_VALUE`s
;       NAN:            in, optional, type=Boolean, default=0
;                       When `SCALE` is set, look for NAN's and treat them as missing data.
;                           NaN's will be treated as missing values and displayed with
;                           the color of `MISSING_COLOR`.
;       NORMAL:         in, optional, type=Boolean, default=1
;                       Indicate that POSITION is supplied in normal coordinates. This is
;                           the default.
;       PAINT:          in, optional, type=boolean, default=0
;                       If set, the image will be painted pixel-by-pixel. In this case,
;                           `X` and `Y` can be NxM arrays the same size as `IMAGE` and
;                           would specify the lower-left corner of each pixel (the center
;                           if `CENTER`=1). Setting `XLOG`, `YLOG`, `X0`, `Y0`, `X1`, or
;                           `Y1` all automatically set PAINT=1. PAINT cannot be used
;                           with `DPOSITION`.
;       PALETTE:        out, optional, type=bytarr(3\,256)/bytarr(256\,3)
;                       An [r,g,b] color table to be loaded before the image is displayed.
;                           PALETTE takes precedence over `CTINDEX`.
;       POSITION:       in, optional, type=fltarr(4)
;                       [x0, y0, x1, y1] where [x0, y0] is the lower left and [x1, y1] is
;                           the upper right coordinate where the image is to be displayed.
;       RANGE:          in, optional, type=fltarr(2), default=[min(`IMAGE`)\, max(`IMAGE`)]
;                       A two element vector specifying the minimum and maximum values
;                           of the image. This keyword takes precedence over `MINVALUE`
;                           and `MAXVALUE`.
;       SCALE:          in, optional, type=Boolean. default=0
;                       Byte scale the image.
;       TOP:            in, optional, type=byte, default=!D.Table_Size-1
;                       If `SCALE` is set, this is the maximum value of the scaled image.
;       XLOG:           in, optional, type=boolean, default=0
;                       If set, the X-axis will be log-scaled and sets `PAINT`=1.
;       XRANGE:         in, optional, type=fltarr(4).
;                       If AXES is set, this determines the data range of the x axis
;       YLOG:           in, optional, type=boolean, default=0
;                       If set, the Y-axis will be log-scaled and sets `PAINT`=1.
;       YRANGE:         in, optional, type=fltarr(4).
;                       If AXES is set, this determines the data range of the y axis
;       _REF_EXTRA:     in, optional, type=any
;                       Any additional keywords accepted by IDL's PLOT procedure is also
;                           accepted for keyword inheritance.
;-
pro MraImage, image, x, y, x0, y0, x1, y1, $
 ADDCOLORBAR = addcolorbar, $
 AXES = AXES, $
 BACKGROUND = background, $
 BOTTOM = bottom, $
 CENTER = center, $
 COLOR = color, $
 CTINDEX = ctindex, $
 DPOSITION = data_pos, $
 DEVICE = device, $
 LOG = log, $
 MAXVALUE = maxvalue, $
 MINVALUE = minvalue, $
 MISSING_VALUE = missing_value, $
 MISSING_COLOR = missing_color, $
 NAN = nan, $
 NORMAL = normal, $
 NOERASE = noerase, $        ;Used to erase the current window
 PAINT = paint, $
 PALETTE = palette, $
 POSITION = position, $
 RANGE = range, $
 SCALE = scale, $
 TOP = top, $
 XFILLVAL = xFillVal, $
 XLOG = xlog, $
 XTITLE = xtitle, $          ;Use cgCheckForSymbols()
 YFILLVAL = yFillVal, $
 YLOG = ylog, $
 YTITLE = ytitle, $          ;Use cgCheckForSymbols()
 TITLE = title, $            ;Use cgCheckForSymbols()
 XSTYLE = xstyle, $          ;Needed for DATA_POS and AXES
 YSTYLE = ystyle, $          ;Needed for DATA_POS and AXES
 XRANGE = xrange, $          ;Needed for DATA_POS and AXES
 YRANGE = yrange, $          ;Needed for DATA_POS and AXES
_REF_EXTRA = extra
    compile_opt strictarr

    ;Catch any errors that might occur.
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        if n_elements(r) ne 0 then tvlct, r, g, b
        if n_elements(init_decomp) ne 0 then device, DECOMPOSED=init_decomp
        if n_elements(p_current) ne 0 then !P = p_current
        if n_elements(x_current) ne 0 then !X = x_current
        if n_elements(y_current) ne 0 then !Y = y_current
        if n_elements(newWin) ne 0 then wdelete, newWin
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;INITIAL STATE ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Make sure the correct number of parameters were given
    if isMember([1,3,5,7], n_params()) eq 0 then $
        message, 'Incorrect number of parameters.'

    ;get the number of elements of x and y
    nx = n_elements(x)
    ny = n_elements(y)
    image_dims = size(image, /DIMENSIONS)

    ;Draw axes around the plot? If so, we need to scale the image
    axes = keyword_set(axes)
    if (axes eq 1) then scale = 1
    
	
	;Get the initial decomposed state and color table, then set DECOMPOSED=0
	init_decomp = cgGetColorState()
	tvlct, r, g, b, /GET
	device, decomposed=0

    ;Store the current system variables	
    p_current = !P
    x_current = !X
    y_current = !Y

;---------------------------------------------------------------------
;PAINT OR PASTE THE IMAGE? ///////////////////////////////////////////
;---------------------------------------------------------------------

    paint = keyword_set(paint)
    xlog = keyword_set(xlog)
    ylog = keyword_set(ylog)
    
    ;Paint the image pixel-by-pixel?
    if xlog + ylog gt 0 or n_params() gt 3 then paint = 1B
    
    ;Remove conflicts
    if paint then begin
        if n_elements(data_pos) gt 0 then begin
            message, 'DPOSITION cannot be used when painting pixels. Ignoring.', /INFORMATIONAL
            void = temporary(data_pos)
        endif
        
        center = keyword_set(center)
        if n_elements(xFillVal) eq 0 then xFillVal = !values.f_nan
        if n_elements(yFillVal) eq 0 then yFillVal = !values.f_nan
    endif
    
;---------------------------------------------------------------------
;CHECK COLORS ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Default to normal coordinates
	if ~keyword_set(normal) and ~keyword_set(device) then normal = 1
	if keyword_set(normal) and keyword_set(device) then $
	    message, 'DEVICE and NORMAL are mutually exclusive.'

	;Foreground, Background, and Missing Color
	if n_elements(color) eq 0 then color = 'black'
    if n_elements(background) eq 0 then background = 'white'
    if n_elements(missing_color) eq 0 then missing_color = background

	;Set the color range to use. Leave room at the top for the missing color value.
	if n_elements(bottom) eq 0 then bottom = 0B
	if n_elements(top) eq 0 then begin
	    if n_elements(missing_value) eq 0 and keyword_set(nan) eq 0 $
	        then top = !d.table_size-1 $
	        else top = !d.table_size-2
	endif else begin
	    if top eq !d.table_size-1 and (n_elements(missing_value) ne 0 or keyword_set(NaN)) $
	        then top = !d.table_size-2
	endelse

	;The number of color table indices (from 1-256) to fill when displaying the image.
	;MISSING_COLOR is being loaded at the top of the color table, so we need to leave
	;one index open.
	ncolors = top - bottom + 1
	
    if n_elements( title) eq 0 then  title = ''
    if n_elements(xtitle) eq 0 then xtitle = ''
    if n_elements(ytitle) eq 0 then ytitle = ''
;---------------------------------------------------------------------
;WINDOW //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Check that a window is open so we can convert to device coordinates.
    if ((!d.flags and 256) ne 0) && (!d.window lt 0) then winID = MrGetWindow(/FREE)
    if ~keyword_set(noerase) then cgErase, background

;---------------------------------------------------------------------
;POSITION ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Create an editable copy of the position, if it exists
	if n_elements(position) ne 0 then begin
	    position_out = position
	    
	    ;convert to device coordinates if need be
	    if keyword_set(normal) then begin
            position_out[[0,2]] = floor(position_out[[0,2]] * !d.x_vsize)
            position_out[[1,3]] = floor(position_out[[1,3]] * !d.y_vsize)
        endif
    endif else begin
        ;Use the default Coyote Graphics position. Make sure it is returned in the POSITION
        ;keyword, as coyote graphics are.
        ; If you get here with no position defined, and no layout, and no !P.Multi and no nothing,
        ; then for God's sake, define a reasonable position in the window!
        if (n_elements(position) eq 0) && (total(!p.position) eq 0) && (total(!p.multi) le 0) then begin
            position = [0.125, 0.125, 0.925, 0.9]
            position_out = fltarr(4)
            position_out[[0,2]] = floor(position[[0,2]] * !d.x_vsize)
            position_out[[1,3]] = floor(position[[1,3]] * !d.y_vsize)
        endif
    endelse

;---------------------------------------------------------------------
;RANGES //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Scale the image if its range is not between 0 and 255
    if n_elements(missing_value) ne 0 then begin
        iNotMissing = where(image ne missing_value, nNotMissing)
        if nNotMissing gt 0 then temp_min = min(image[iNotMissing], NAN=nan, max=temp_max) $
                            else temp_min = min(image, NAN=nan, max=temp_max)
    endif else begin
        temp_min = min(image, max=temp_max, NAN=nan)
    endelse
    
    ;Set SCALE if Max(Image) > 255 or Min(Image) < 0 so that it fits within the
    ;colortable.
    if temp_min lt 0 || temp_max gt 255 then scale = 1
    
    ;Check RANGE
    if n_elements(range) ne 0 then begin
        minvalue = range[0]
        maxvalue = range[1]
        scale = 1
    endif

    ;Check the min and max values.
    if n_elements(minvalue) eq 0 || minvalue eq maxvalue then minvalue = temp_min
    if n_elements(maxvalue) eq 0 || maxvalue eq minvalue then maxvalue = temp_max
    range = [minvalue, maxvalue]
    
    ;X- and Y-axis ranges must be set if AXES or PAINT or DPOSITION is set.
    if axes or paint or n_elements(data_pos) gt 0 then begin
        ;Make sure the [xy]-axis range is exact.
        if n_elements(xstyle) eq 0 then xstyle = 1 else xstyle += ~(xstyle and 1)
        if n_elements(ystyle) eq 0 then ystyle = 1 else ystyle += ~(ystyle and 1)
        
        ;Set the [xy]-range
        if n_elements(xrange) eq 0 then xrange = [min(x, max=xMax, NAN=nan), xMax]
        if n_elements(yrange) eq 0 then yrange = [min(y, max=yMax, NAN=nan), yMax]
    endif
	
;---------------------------------------------------------------------
;DATA POSITION ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;If DATA_POS is set, then it supplies the data coordinates of the image.
	;They must be converted to DEVICE coordinates.
	if n_elements(data_pos) gt 0 && array_equal(data_pos[[0,1]], data_pos[[2,3]]) eq 0 then begin
		if n_elements(data_pos) ne 4 then message, 'DATA_POS must be a 4 element vector' + $
												   'specifying the position in data coordinates.'

		;If DATA_POS does not fit within [XY]RANGE then send and error message.
		if (data_pos[0] lt xrange[0]) or (data_pos[2] gt xrange[1]) $
		    then message, 'DATA_POS does not lie within XRANGE.'
		if (data_pos[1] lt yrange[0]) or (data_pos[3] gt yrange[1]) $
		    then message, 'DATA_POS does not lie within YRANGE.'

        ;Automatically scale.
	    scale = 1
                
        ;A window must be open for CONVERT_COORD to work. If no window is open,
        ;create an invisible pixmap window if the device supports them.
        if (!d.window eq -1) and ((!d.flags and 256) gt 0) $
            then newWin = MrGetWindow(/Free, /Pixmap)
            		
        ;If an x- and y-range were given, then we need to set the !P, !X, and !Y system
        ;variables in order for CONVERT_COORD to be able to convert from DATA to DEVICE
        ;coordinates. The only way to do that is to create an invisible plot.

        ;Create an invisible plot to set the system variables.
        plot, [0,0], /NODATA, /NOERASE, XLOG=xlog, YLOG=ylog, $
                     XRANGE=xrange, YRANGE=yrange, XSTYLE=5, YSTYLE=5, $
                     POSITION=position_out, /DEVICE
        
        ;Convert from data to device coordinates.
        data_pos_out = convert_coord(data_pos[[0,2]], data_pos[[1,3]], /DATA, /TO_DEVICE)
        data_pos_out = reform(data_pos_out[0:1,0:1], 4, 1)
        
        ;reset the system variables.
        !P = p_current
        !X = x_current
        !Y = y_current
            
        ;Delete the pixmap window if it was made
        if n_elements(newWin) gt 0 then wdelete, newWin
	endif

;---------------------------------------------------------------------
;LOG SCALING /////////////////////////////////////////////////////////
;---------------------------------------------------------------------	
    if keyword_set(log) then begin
        image_out = MrLog(image)
        range = [min(image_out, /NAN, max=imMax), imMax]
    endif else begin
        image_out = image
    endelse

;---------------------------------------------------------------------
;MISSING VALUES & NANS ///////////////////////////////////////////////
;---------------------------------------------------------------------	

    if n_elements(missing_value) gt 0 || keyword_set(nan) then begin
        ;Create a mask. 1=display, 0=hide (mask)
        mask = bytarr(size(image_out, /DIMENSIONS)) + 1B
        
        ;look for NaN's and missing values
        if keyword_set(nan) $
            then iNaN = where(finite(image_out, /NAN) eq 1, nNaN) $
            else nNaN = 0
        if n_elements(missing_value) ne 0 $
            then iMissing = where(image_out eq missing_value, nMissing) $
            else nMissing = 0
        
        ;Mask non-data values
        if nNaN gt 0 then mask[iNaN] = 0B
        if nMissing gt 0 then mask[iMissing] = 0B

        ;The missing color index is TOP+1. Set missing values equal to the missing
        ;color index.
        ikeep = where(mask eq 1, nkeep, COMPLEMENT=iMask, NCOMPLEMENT=nmask)
        if nmask gt 0 then image_out[iMask] = top + 1
    endif

;---------------------------------------------------------------------
;SCALE THE IMAGE /////////////////////////////////////////////////////
;---------------------------------------------------------------------	

	;Scale the image?
	if keyword_set(scale) then begin
	
        ;Scale non-missing values
        iScale = where(image_out ne top+1, nScale)
        if nScale gt 0 then image_out[iScale] = bytscl(image_out[iScale], MIN=range[0], MAX=range[1], TOP=top-bottom)
        
        ;BYTSCL scales between 0 and TOP. Bump everything up by BOTTOM.
        if bottom ne 0 then image_out += bottom
	endif

;---------------------------------------------------------------------
;SETUP THE COLORS ////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Load the palette
    if n_elements(palette) ne 0 then begin
        dims = size(palette, /DIMENSIONS)
        if dims[0] eq 3 then palette = transpose(palette)
        tvlct, palette
    
    ;Load the color table.
    endif else if n_elements(ctindex) ne 0 then begin
        cgLoadCT, ctindex, BOTTOM=bottom, NCOLORS=ncolors
    
    ;Load the original colortable, before COLOR and BACKGROUND were added.
    endif else begin
        tvlct, r, g, b
    endelse
        
    ;Add the missing color to the color table
    if n_elements(missing_value) ne 0 or keyword_set(nan) $
        then imissing = cgColor(missing_color, top+1)

;---------------------------------------------------------------------
;PAINT THE IMAGE PIXEL-BY-PIXEL? /////////////////////////////////////
;---------------------------------------------------------------------
	if paint then begin
	    ;Caluculate the corners of the pixels
	    case n_params() of
            3: MrPixelPoints,  image_dims[0:1], x, y, Xmin, Ymin, Xmax, Ymax, $
                                CENTER=center, XLOG=xlog, YLOG=ylog, /DIMENSIONS
            5: MrPixelCorners, image_dims[0:1], x, y, x0, y0, Xmin, Ymin, Xmax, Ymax, /DIMENSIONS
            7: MrPixelDeltas,  image_dims[0:1], x, y, x0, y0, x1, y1, Xmin, Ymin, Xmax, Ymax, /DIMENSIONS
        endcase

	    ;A data coordinate system must be established, so draw an invisible set of axes.
	    ;Do NOT set XLOG or YLOG
        plot, [0,0], /NODATA, /NOERASE, XLOG=xlog, YLOG=ylog, $
                     XRANGE=xrange, YRANGE=yrange, XSTYLE=5, YSTYLE=5, $
                     POSITION=position_out, /DEVICE

        ;Draw and fill each pixel
        for i = 0L, image_dims[0] - 1 do begin
            for j = 0L, image_dims[1] - 1 do begin
                if total(finite([Xmin[i,j], Xmax[i,j], Ymin[i,j], Ymax[i,j]])) eq 4 $
                    then polyfill, [Xmin[i,j], Xmax[i,j], Xmax[i,j], Xmin[i,j]], $
                                   [Ymin[i,j], Ymin[i,j], Ymax[i,j], Ymax[i,j]], $
                                   COLOR=image_out[i,j]
            endfor
        endfor

;---------------------------------------------------------------------
;USE TV TO DISPLAY? //////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if nx eq 1 then begin
	    if ny eq 0 then tv, image_out, x $
	               else tv, image_out, x, y

;---------------------------------------------------------------------
;SETUP THE COLORS ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
	    ;If a data position was provided, switch to that now.
	    if n_elements(data_pos_out) gt 0 $
	        then image_position = data_pos_out $
	        else image_position = position_out

        ;adjust the image position a little to fit inside the axes, not on top of them
        xsize = (image_position[2] - image_position[0])  - 1
        ysize = (image_position[3] - image_position[1])  - 1
        xstart = image_position[0] + 1
        ystart = image_position[1] + 1

        ;size the image differently, depending out the output window
	    if !D.Name eq 'PS' then tv, image_out, xstart, ystart, xsize=xsize, ysize=ysize $
					       else tv, congrid(image_out, xsize, ysize), xstart, ystart
	endelse
    
;---------------------------------------------------------------------
;PUT AXES AROUND THE IMAGE ///////////////////////////////////////////
;---------------------------------------------------------------------

	;create axes for the image, if desired
	if keyword_set(axes) then begin

	    ;if x and y were given, use them in setting up the coordinate system
	    if nx eq image_dims[0] or ny eq image_dims[1] then begin
            plot, x, y, BACKGROUND=cgColor(background), COLOR=cgColor(color), $
                  /NODATA, /DEVICE, /NOERASE, TITLE=cgCheckForSymbols(title), $
                  POSITION=position_out, XRANGE=xrange, YRANGE=yrange, XLOG=xlog, YLOG=ylog, $
                  XTITLE=cgCheckForSymbols(xtitle), YTITLE=cgCheckForSymbols(ytitle), $
                  XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra
        
        ;otherwise just use the keywords
	    endif else begin
            plot, [0], BACKGROUND=cgColor(background), COLOR=cgColor(color), $
                  /NODATA, /DEVICE, /NOERASE, TITLE=cgCheckForSymbols(title), $
                  POSITION=position_out, XRANGE=xrange, YRANGE=yrange, XLOG=xlog, YLOG=ylog, $
                  XTITLE=cgCheckForSymbols(xtitle), YTITLE=cgCheckForSymbols(ytitle), $
                  XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra
        endelse
    endif

;---------------------------------------------------------------------
;RESET COLOR TABLE AND DEVICE ////////////////////////////////////////
;---------------------------------------------------------------------

	tvlct, r, g, b
	device, decomposed=init_decomp
end
