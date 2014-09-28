; docformat = 'rst'
;
; NAME:
;   MrLayout
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
;   The purpose of this program is to calculate and return the positions of the array of
;   plots specified in layout.
;
; :Categories:
;   Plot Utilities
;
; :Examples:
;   Try the main level program at the end of this document::
;       IDL> .r MrLayout
;
;   Single plot, output in normal coordinates (Default)::
;       position = MrLayout([1,1])
;       plot, x, y, position=position
;
;   2 columns containing 4 plots each, output in device coordinates, with no gaps between plots::
;       positions = MrLayout([2,4], XGAP=0, YGAP=0, /DEVICE)
;
;   Accessing the position of the plot in the 2nd column and 3rd row::
;       positions = MrLayout([2,4])
;       plot, x, y, position=position[*,5]
;
;   Returning the position of a specific plot::
;       position = MrLayout([2,4], [1,2])
;
;
; :Params:
;       LAYOUT:         in, required, type=intarr
;                       If a 2-element vector, then the number of column and rows in the
;                           plot layout: [ncols, nrows]. If 3-elements: [ncols, nrows, index],
;                           where "index" is the overall index into the 2D plotting grid
;                           corresponding to the `POSITION` to be returned. "index" starts
;                           at 1 in the upper-left corner, and increases first down then
;                           across. If LAYOUT is 4 elements, then it takes the form
;                           [ncols, nrows, col, row], where "col" and "row" are the
;                           column and row of the plot `POSITION` to be returned.
;       LOCATION:       in, optional, type=int/intarr(2)
;                       Either a scalar integer indicating the overall grid location, or a
;                           two element vector specifying the [col, row] location of where
;                           a plot is to be placed. Plot indices start with 1 ([1,1]) in
;                           the upper-left corner, and increase first down each column then
;                           across each row. This keyword is only used if `LAYOUT` has
;                           exaclty two elements.
;
; :Keywords:
;       ASPECT:         in, optional, type=float/fltarr
;                       The aspect ratio (plot height/plot width) of each plot. For square
;                           plots, ASPECT=1.0, for plots that are twice as wide as the are
;                           long, ASPECT=0.5. If an array is given, it must have the same
;                           number of elements as there are locations in the layout.
;       CHARSIZE:       in, optional, type=float, default=1
;                       Scaling factor for the font size.
;       COL_WIDTH:      in, optional, type=fltarr
;                       An array of floats specifying the percentage of the width of
;                           `P_REGION` that each column should be. The sum of the elements
;                           must add to unity. The default is to create equal-width columns.
;       DEVICE:         in, optional, type=Boolean, default=0
;                       Indicate that the output positions are to be given in device coordinates
;       OXMARGIN:       in, optional, type=intarr(2), default="[0,0]"
;                       The size of the left and right outer margins, in multiples of
;                           !D.X_CH_SIZE. This the area surrounding the plotting region --
;                           it is like the matte of a picture.
;       OYMARGIN:       in, optional, type=intarr(2), default="[0,0]"
;                       The size of the bottom and top outer margins, in multiples of
;                           !D.Y_CH_SIZE. This the area surrounding the plotting region --
;                           it is like the matte of a picture.
;       IXMARGIN:       in, optional, type=fltarr(2), default="[10, 3]"
;                       The inner x-margins in character widths. [left margin, right margin]
;                           The margins specify the distance between the edge of the plot 
;                           axes and `P_AREA`.
;       IYMARGIN:       in, optional, type=fltarr(2), default="[4, 2]"
;                       The inner y-margins in character widths. [bottom margin, top margin]
;                           The margins specify the distance between the edge of the plot 
;                           axes and `P_AREA`.
;       NORMAL:         in, optional, type=Boolean, default=1
;                       Indicate that the output positions are to be given in normal
;                           coordinates.
;       P_REGION:       out, optional, type=fltarr(4)
;                       A vector in the form [x0, y0, x1, y1] where (x0,y0) is the lower
;                           left corner and (x1,y1) is the upper right corder of the
;                           plotting region -- the area containing the plot axes, titles,
;                           tick marks, ticklables, etc. (everything but the margins).
;       P_AREAS:        out, optional, type=4xN float
;                       Vectors specifying the plotting areas. `P_REGION` is broken into
;                           rows and column, then `[XY]GAPS` are interposed between them.
;                           The remaining spaces are the plot areas.
;       ROW_HEIGHT:     in, optional, type=fltarr
;                       An array specifying the percent of the height of `P_REGION` that
;                           each row should be. The default is to make equal-height rows.
;       SQUARE:         in, optional, type=boolean, default=0
;                       If set, `ASPECT` will be set to 1.0 and all positions will be
;                           square.
;       WDIMS:          in, optional, type=lonarr(2)
;                       The dimensions of the device window, in pixels. If not provided,
;                           the currently open window will be queried. If no window is
;                           open, an attempt will be made to create one and if windows are
;                           not supported, an error will be thrown.
;       XGAP:           in, optional, type=float/fltarr, default=14
;                       The horizontal gap between plot areas (see `P_AREAS`), in
;                           character units. The gap between each column can be specified
;                           by supplying an array with one less element than the total
;                           number of columns.
;       YGAP:           in, optional, type=float/fltarr, default=7
;                       The vertical gap between plot areas (see `P_AREAS`), in
;                           character units. The gap between each row can be specified
;                           by supplying an array with one less element than the total
;                           number of rows.
;
; :Returns:
;       POSITIONS:      An 4xN dimensional vector, where N is the "index" of the plot,
;                           starting with 0 in the upper-left corner and increasing first
;                           across then down. Positions are `P_AREAS` less `I[XY]MARGIN`.
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
;       09/25/2011 - Written by Matthew Argall
;       10/01/2011 - Added [XY]Range, CHARSIZE, and xyouts to create axis and plot titles,
;                       made the plots from top to bottom instead of bottom to top - MRA
;       10/03/2011 - Added [xy]ticks, [xy]tickv, and [xy]tickformat.
;                       Converted from function to procedure.
;                       Added MULTIPLEAXES keyword. - MRA
;       10/04/2011 - Added INITIALIZE keyword - MRA
;       10/20/2011 -    Added keywords DEVICE, NORMAL, MSG, SQUARE, OVERPLOT
;                       converted to working with device coordinates for SQUARE plots,
;                       removed plotting keywords in favor of _extra,
;                       made plot_positions its own function, removing the plotting aspect,
;                       altered how gaps were implemented applying gaps to inner edges only - MRA
;       02/18/2012 - everything is changed to device coordinates right away,
;                       fixed how gaps were being implemented - MRA
;       02/28/2012 - Added [XY]MARGIN, specified in character widths. 
;                       Added XGAP and YGAP, specified in character widths.
;                       Removed MARGINS, [XY]TITLE_OFFSETS, and MSG
;                       Simplified the check of user inputs
;                       Changed everything to mimic how the IDL calculates plot positions - MRA
;       11/14/2012 - GAP-related keywords now properly in units of character size. P_WINDOW
;                       and P_REGION now returned in the proper units. - MRA
;       11/29/2012 - SQUARE keyword now works properly. - MRA
;       04/23/2013 - Changed name from plot_positions.pro to MrPlotLayout, added ASPECT,
;                       removed SQUARE, added LOCATION parameter. Changed the dimension
;                       order of the output positions to [4,N,M] so that reform() is not
;                       needed to retreive, e.g., positions[*,2,1]. This is possible
;                       because IDL automatically contracts trailing dimensions of size 1.
;                       Create a window if no window has been opened. Removed some keyword
;                       checks because they take care of themselves later in the code
;                       (Some inspiration from David Fanning's cgLayout.pro). - MRA
;       04/26/2013 - Removed GAPS keyword. - MRA
;       08/09/2013 - thisPos can be an integer or a 2-element vector. - MRA
;       08/16/2013 - LAYOUT can now be a 3- or 4-element vector. - MRA
;       08/19/2013 - THISPOS increases across rows then down columns, the IDL way. - MRA
;       08/21/2013 - Use MrGetWindow to set up !D if no window is currently open. Check
;                       if windows supported was failing when it should not. Fixed. - MRA
;       08/23/2013 - Array_Indices uses 0-based indices, not 1-based. Fixed. - MRA.
;       2013/11/22 - Added the OXMARGIN and OYMARGIN keywords. - MRA
;       2013/11/24 - Renamed from MrPlotLayout to MrLayout. - MRA
;       2013/11/28 - "index" values were returning positions in reverse order. Fixed.
;                       `POSITIONS` is now a 4xN array, where plot positions are retreived
;                       by "index" value, not [col,row]. - MRA
;       2014/02/10 - Added the P_AREAS, COL_HEIGHT, ROW_WIDTH, and WDIMS keywords.
;                       Removed the P_WINDOW keyword. Renamed [XY]Margin to O[XY]Margin
;                       and made their behavior match the IDL behavior. Gaps between
;                       columns and rows can vary. Column and row widths can also vary.
;                       Aspect ratio can vary between plots. - MRA
;
;-
function MrLayout, layout, location, $
;KEYWORDS
ASPECT = aspect, $
COL_WIDTH = col_width, $
CHARSIZE = charsize, $
DEVICE = device, $
IXMARGIN = ixmargin, $
IYMARGIN = iymargin, $
NORMAL = normal, $
OXMARGIN = oxmargin, $
OYMARGIN = oymargin, $
P_REGION = p_region, $
P_AREAS = p_areas, $
ROW_HEIGHT = row_height, $
SQUARE = square, $
WDIMS = wDims, $
XGAP = xgap, $
YGAP = ygap
    compile_opt strictarr
    on_error, 2

    normal = keyword_set(normal)

    ;We must have the dimensions of the window
    if n_elements(wDims) eq 0 then begin
        if (!d.window eq -1) then begin
            if ((!d.flags and 256) gt 0) then begin
                iWin = MrGetWindow(/FREE, /PIXMAP)
                wDims = [!d.x_vsize, !d.y_vsize]
                wDelete, iWin
            endif else begin
                message, 'Windows not supported and window not available. Provide WDIMS.'
            endelse
        endif else begin
            wDims = [!d.x_vsize, !d.y_vsize]
        endelse
    endif
    
    xsize = double(wDims[0])
    ysize = double(wDims[1])

;------------------------------------------------------------------------------------
;CHECK KEYWORDS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;------------------------------------------------------------------------------------
    ;LAYOUT must be a 2-, 3-, or 4-element vector
    case n_elements(layout) of
        2: ;do nothing
        3: location = layout[2]
        4: location = layout[2:3]
        else: message, 'LAYOUT must be a 2-, 3-, or 4-element vector.'
    endcase

    ;Convert LOCATION to its 1D index, if necessary. LOCATION increases first across rows
    ;then down columns.
    case n_elements(location) of
        0: ;do nothing
        1: iLoc = location - 1
        2: iLoc = layout[0]*(location[1]-1) + location[0] - 1
        else: message, 'LOCATION must be a scalar or 2-element vector.'
    endcase

	;number of plots in the vertical and horizontal direction
    nrows = layout[1]
    ncols = layout[0]
    
    ;Coordinates
    normal = keyword_set(normal)
    device = keyword_set(device)
    if normal + device eq 0 then normal = 1
    if normal + device ne 1 then message, 'NORMAL and DEVICE are mutually exclusive.'

	;Default margins, gaps, and character size
    if keyword_set(square)         then aspect     = 1.0
	if n_elements(ixmargin)   eq 0 then ixmargin   = [ 0.0, 0.0]
	if n_elements(iymargin)   eq 0 then iymargin   = [ 0.0, 0.0]
	if n_elements(oymargin)   eq 0 then oymargin   = [ 4.0, 2.0]
	if n_elements(oxmargin)   eq 0 then oxmargin   = [10.0, 3.0]
	if n_elements(xgap)       eq 0 then xgap       = 14.0
	if n_elements(ygap)       eq 0 then ygap       = 6.0
	if n_elements(charsize)   eq 0 then charsize   = 1.0
	
	;Aspect Ratio
	case n_elements(aspect) of
	    0: ;Do nothing
	    1: aspect = replicate(aspect, nCols, nRows)
	    nCols*nRows: aspect = reform(aspect, nCols, nRows)
	    else: message, 'ASPECT: Incorrect number of elements.'
	endcase
	
	;Row Height
	case n_elements(row_height) of
	    0:     row_height = replicate(1.0 / nRows, nRows)
	    nRows: if total(row_height) ne 1.0 then message, 'Total(ROW_HEIGHT) must equal 1.0.'
	    else: message, 'ROW_HEIGHT: Incorrect number of elements.'
	endcase
	
	;Column Width
	case n_elements(col_width) of
	    0:     col_width = replicate(1.0 / nCols, nCols)
	    nCols: if total(col_width) ne 1.0 then message, 'Total(COL_WIDTH) must equal 1.0.'
	    else: message, 'COL_WIDTH: Incorrect number of elements.'
	endcase

    ;X-Gaps
	if nCols gt 1 then begin
        xspace = fltarr(nCols-1)
        case n_elements(xgap) of
            1:       xspace = replicate(xgap, nCols - 1)
            nCols-1: xspace = xgap
            else: message, 'XGap: Incorrect number of elements.'
        endcase
        xspace = [0.0, xspace]
    endif else xspace = 0.0
	
	;Y-Gaps
	if nRows gt 1 then begin
        yspace = fltarr(nRows-1)
        case n_elements(ygap) of
            1:       yspace = replicate(ygap, nRows - 1)
            nRows-1: yspace = ygap
            else: message, 'YGap: Incorrect number of elements.'
        endcase
        yspace = [0.0, yspace]
    endif else yspace = 0.0

;-----------------------------------------------------------------------------------------
;Calculate Margins and Areas \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	;Convert from character units to pixels.
	xcharsize = ceil(double(!d.x_ch_size)*charsize)
	ycharsize = ceil(double(!d.y_ch_size)*charsize)
	ximargin = ixmargin * xcharsize
    yimargin = iymargin * ycharsize
	xomargin = oxmargin * xcharsize
	yomargin = oymargin * ycharsize
	xspace   = xspace   * xcharsize
	yspace   = yspace   * ycharsize

	;Calculate the area of the region in which plots will be drawn.
	p_region = [xomargin[0], yomargin[0], xsize - xomargin[1], ysize - yomargin[1]]

    ;Calculate the plot dimensions
    plot_width  = (p_region[2] - p_region[0] - total(xspace)) * col_width 
    plot_height = (p_region[3] - p_region[1] - total(yspace)) * row_height

    ;Offset between upper left corner of p_region and lower right corner of
    ;the plot area of each plot.
    xoffset = total(plot_width  + xspace, /CUMULATIVE)
    yoffset = total(plot_height + yspace, /CUMULATIVE)

	;Calculate the areas in which the plots will be created.
	p_areas = fltarr(4, nCols, nRows)
	for ii = 0, nCols-1 do begin
		for jj = 0, nRows-1 do begin
		    p_areas[2,ii,jj] = p_region[0] + xoffset[ii]
		    p_areas[1,ii,jj] = p_region[3] - yoffset[jj]
		    p_areas[0,ii,jj] = p_areas[2,ii,jj] - plot_width[ii]
		    p_areas[3,ii,jj] = p_areas[1,ii,jj] + plot_height[jj]
		endfor
	endfor 

;-----------------------------------------------------------------------------------------
;Calculate Positions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	;Subtract the inner margin to create the plot position
	positions = fltarr(4, nCols, nRows)
	positions[0,*,*] = p_areas[0,*,*] + ximargin[0]
	positions[2,*,*] = p_areas[2,*,*] - ximargin[1]
	positions[1,*,*] = p_areas[1,*,*] + yimargin[1]
	positions[3,*,*] = p_areas[3,*,*] - yimargin[1]
	
	;Reform into a 4xnCols*nRows array
	positions  = reform(positions, 4, nCols*nRows)
	p_areas = reform(p_areas, 4, nCols*nRows)

;-----------------------------------------------------------------------------------------
;Set the Aspect Ratio \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	if n_elements(aspect) gt 0 then begin	    
	    ;Loop through all of the plots
	    for i = 0, nCols*nRows-1 do begin
	        if aspect[i] eq 0 then continue
	        
	        pWidth  = positions[2,i] - positions[0,i]
	        pHeight = positions[3,i] - positions[1,i]
	        
	        ;Make sure the scaled dimension becomes smaller
	        newPWidth  = pWidth
	        newPHeight = newPWidth * aspect[i]
	        if newPHeight gt pHeight then begin
	            newPHeight = pHeight
	            newPWidth = newPHeight / aspect[i]
	        endif
	        
	        ;Center the new position within its old position
	        positions[0,i] = positions[0,i] + (pWidth  - newPWidth)  / 2
	        positions[1,i] = positions[1,i] + (pHeight - newPHeight) / 2
	        positions[2,i] = positions[0,i] + newPWidth
	        positions[3,i] = positions[1,i] + newPHeight
	    endfor
	endif

;-----------------------------------------------------------------------------------------
;Normal or Device Coordinates? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	if keyword_set(normal) then begin
		positions[[0,2],*] = positions[[0,2],*] / xsize
		positions[[1,3],*] = positions[[1,3],*] / ysize
		
		p_region[[0,2]] = p_region[[0,2]] / xsize
		p_region[[1,3]] = p_region[[1,3]] / ysize
		
		p_areas[[0,2],*] = p_areas[[0,2],*] / xsize
		p_areas[[1,3],*] = p_areas[[1,3],*] / ysize

	;Create integer values for device coordinates.
	endif else begin
	    positions[[0,1],*] = fix(floor(positions[[0,1],*]))
	    positions[[2,3],*] = fix(ceil(positions[[2,3],*]))
	    
	    p_region[[0,1]] = fix(floor(p_region[[0,1]]))
	    p_region[[2,3]] = fix(ceil(p_region[[2,3]]))
		
		p_areas[[0,1],*] = fix(floor(p_areas[[0,1]]))
		p_areas[[2,3],*] = fix(ceil(p_areas[[2,3]]))
	endelse

;-----------------------------------------------------------------------------------------
;Specific Location? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	if n_elements(iLoc) gt 0 then positions = positions[*,iLoc]

	return, positions
end




;-----------------------------------------------------
;Main Level Example Program (.r MrGridLayout) \\\\\\\\
;-----------------------------------------------------
;EXAMPLE
;   Display the different parts of the plot window. 
window, /FREE, xsize=1000, ysize=600
ncols      = 3
nrows      = 3
oxmargin   = [14,6]
oymargin   = [2,4]
ixmargin   = [2,2]
iymargin   = [2,2]
xgap       = [14,6]
ygap       = [6,12]
col_width  = [0.4, 0.25, 0.35]
row_height = [0.5, 0.25, 0.25]
charsize   = 1.0
pos = MrLayout([ncols, nrows], OXMARGIN=oxmargin, OYMARGIN=oymargin, $
                               IXMARGIN=ixmargin, IYMARGIN=iymargin, $
                               XGAP=xgap, YGAP=ygap, $
                               COL_WIDTH=col_width, ROW_HEIGHT=row_height, $
                               P_REGION=p_region, P_AREAS=p_areas)

;[XY]MARGIN -- Bounded in blue by P_REGION. The space between the edge of the window
;              and the blue box
plots, [p_region[0], p_region[2], p_region[2], p_region[0], p_region[0]], $
       [p_region[1], p_region[1], p_region[3], p_region[3], p_region[1]], $
       /NORMAL, COLOR=cgColor('blue')

;wait here to show plot window and region
wait, 1

;I[XY]MARGIN -- Bounded in red by P_AREAS. The space between the red box and the axes.
;[XY]GAPS    -- Space between P_AREAS (red boxes) demonstrates the gaps.
for ii = 0, nCols*nRows-1 do begin
    plots, [p_areas[0,ii], p_areas[2,ii], p_areas[2,ii], p_areas[0,ii], p_areas[0,ii]], $
           [p_areas[1,ii], p_areas[1,ii], p_areas[3,ii], p_areas[3,ii], p_areas[1,ii]], $
           /NORMAL, COLOR=cgColor('red')
endfor

;Wait here to show the plot areas.
wait, 1

;Draw the plot axes.
for ii=0,nCols*nRows-1 do $
    plot, !x.range, !y.range, position=pos[*,ii], /NOERASE, /NODATA, /NORMAL, $
          TITLE='title', XTITLE='xtitle', YTITLE='ytitle', CHARSIZE=charsize


;EXAMPLE
;   Create a 2x2 grid of plots
ncols = 2
nrows = 2
xgap = 0.5
ygap = 0.5
charsize = 2.0
pos = MrLayout([nCols, nRows], XGAP=xgap, YGAP=ygap, CHARSIZE=charsize)

;Open a window
window, /FREE

for i = 0, ncols*nrows - 1 do $
    plot, [0,1], [0,1], POSITION=pos[*,i], /NOERASE, /NODATA, /NORMAL, $
          TITLE=title, XTITLE='X Title', YTITLE='Y Title', CHARSIZE=charsize


end