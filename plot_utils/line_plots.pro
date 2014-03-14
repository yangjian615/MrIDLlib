; docformat = 'rst'
;
; NAME:
;       LINE_PLOTS
;
; PURPOSE:
;+
;       A wrapper for the PLOT routine that plots each column of data as separate line-plot
;       within the same axes.
;
;       Additional options include::
;           1) plotting with the y-axis on the right side of the plot
;           2) drawing a dashed line at y = 0
;           3) plotting the different columns in different colors
;           4) putting a legend on the graph
;
; :Categories:
;
;       Wrapper, Plot, Data Visualization
;
; :Params:
;
;       X:              in, optional, type=real numeric
;                       A vector representing the abscissa values to be plotted. 
;                           If X is not specified, Y is plotted as a function of point number 
;                           (starting at zero). If both arguments are provided, Y is plotted
;                           as a function of X.
;       Y:              in, required, type=real numeric
;                       The ordinate data to be plotted. Y should be a column vector. If
;                           it is a 1D row-vector, the transpose will be taken. If it is
;                           2D, then each column of data will be plotted independently
;                       
; :Keywords:
;
;   AXISCOLOR:          in, optional, type=string/color index, default='black'
;                       the forground color of the plot (no longer COLOR)
;   BACKGROUND:         in, optional, type=string/color index, default='white'
;                       the background color of the plot
;   COLOR:              in, optional, type=string/color index, default='black'
;                       the numeric value of the color of each component. Can have
;                           any number of elements, but only the first N will be used,
;                           where N is the number of components of y and, separately,
;                           the number of elements in legend. The default is the
;                           foreground color of the plot.
;   POSITION:           in, optional, type=fltarr(4)
;                       positon of the plot [x0, y0, x1, y1] where (x0, y0) specifies the
;                           lower left and (x1, y1) specifies the upper right corner of the plot
;   XRANGE:             in, optional, type=fltarr(2)
;                       [min, max] values of X
;   YRANGE:             in, optional, type=fltarr(2)
;                       [min, max] values of Y
;   YTITLE:             in, optional, type=string, default=''
;                       title for the y axis
;   LEGEND:             in, optional, type=strarr, default=['']
;                       the labels of each data point. Each element of legend is
;                           plotted the same color as its corresponding element in
;                           COLORS. The default is the foreground color.
;   YAX_RIGHT:          in, optional, type=Boolean, default=0
;                       draw the y axis on the right side of the plot
;   ZERO_LINE:          in, optional, type=Boolean, default=0
;                       draw a line at y = 0
;   _EXTRA:             in, optional, type=structure
;                       a structure containing any plotting keywords to be inherited
;                           by PLOT
;
; :Restrictions:
;
;       Uses the following programs from my library::
;           `load_color.pro`
;           `error_message.pro` (Coyote Graphics)
;
;       If colors are provided as strings, they begin loading at color index 1. If a
;       mix of strings and color indices are provided, take caution.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2011
;
; :History:
;   Modification History::
;
;       Written by:   Matthew Argall 11 November 2011
;       11/30/2011  -   added COLORS keyword, LEGEND can have any number of
;                          elements and all will be put on the plot.
;       01/20/2012  -   addes ZERO_LINE
;       02/26/2012  -   added keyword YAX_RIGHT
;       04/01/2012  -   removed MIN_VALUE, FILL_VAL, POSITION, NORMAL, DEVICE
;                          and leave them to the inheritance structure _EXTRA
;                       Now defaults to having no legend
;                       Changed the legend character size to 2*!d.[xy]_ch_size
;                       Removed default _extra keywords for PLOT
;                       No longer attempt to cover the previous plot's tickmarks
;                          with an extra call to AXIS
;       04/07/2012  -   default to drawing a line at y=0 if ymin < 0 < ymax
;       05/24/2012  -   Simplified the keyword checking process.
;                       Included a select number of PLOT keywords in the keyword list
;                          so that they are extracted from the EXTRA struture if present.
;                          Those keywords need to be controled explicitly if other,
;                          wrapper keywords are used.
;                       Made X optional and transpose 1D row vector so that it
;                          can be plotted properly
;       12/11/2012  -   Removed TITLE from the keywords list. Added YSTYLE keyword to fix bug
;                          with YAX_RIGHT. Added check for [XY]RANGE=[0,0] to get ZERO_LINE
;                          to be drawn properly.
;       17/02/2012  -   Simplified color checking/loading scheme a little - MRA
;       04/19/2013  -   Added AXISCOLOR, renamed FGCOLOR to COLOR and BGCOLOR to
;                           BACKGROUND to be more consisten with IDL and to prepare for
;                           integration  with Coyote Graphics. - MRA
;-
pro line_plots, x, y, $
;PLOT KEYWORDS -- will be extracted from _EXTRA
AXISCOLOR = axiscolor, $
BACKGROUND = background, $
POSITION = position, $
XRANGE = xrange, $                  ;needed for /YAX_RIGHT and /ZERO_LINE
YRANGE = yrange, $                  ;needed for /YAX_RIGHT and /ZERO_LINE
YSTYLE = ystyle, $                  ;needed for /YAX_RIGHT
YTITLE = ytitle, $                  ;needed for /YAX_RIGHT
;WRAPPER KEYWORDS
LEGEND = legend, $
COLOR = color, $
YAX_RIGHT = yax_right, $
ZERO_LINE = zero_line, $
_EXTRA = extra
    compile_opt idl2

    ;Catch any errors that might occur.
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;CHECK POSITIONAL PARAMETERS /////////////////////////////////////////
;---------------------------------------------------------------------
    ;if 1 parameter was given, then that parameter is y... x -> y
    if n_params() eq 1 then y_out = x else y_out = y

    ;make sure that Y is 2D (i.e. a column vector, not a row vector)
    nydims = size(y_out, /n_dimensions)
    ydims = size(y_out, /dimensions)
    if nydims eq 1 then y_out = transpose(y_out)
    
    ;if only 1 parameter was given, make X mark the index number of the points in 
    ;each component of Y
    if n_params() eq 1 $
        then x_out = lindgen(1, n_elements(y_out[0,*])) $
        else x_out = x

	;find out how many components are to be plotted
	sz = size(y_out)
	ndims = sz[0]
	ncomps = sz[1]
    if ndims ne 2 then message, '[XY] must be a(n array of) column vector(s)'

;---------------------------------------------------------------------
;CHECK KEYWORDS //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;get the current color table	
    ;start loading colors at color index 1 (1 is added later)
    tvlct, r, g, b, /get
    itop = 0

    ;default to white background
	if n_elements(background) eq 0 then background = 'white'
	if size(background, /type) eq 7 then ibackground = load_color(background, bottom=itop+1, itop=itop) $
                                 else ibackground = background
	
	;default to black foreground
	if n_elements(axiscolor) eq 0 then axiscolor = 'black'
    if size(axiscolor, /type) eq 7 then iaxiscolor = load_color(axiscolor, bottom=itop+1, itop=itop) $
                                 else iaxiscolor = axiscolor

	;if no colors were supplied, make the colors the forground color
	ncolors = n_elements(color)
	if ncolors eq 0 then color = replicate(iaxiscolor, ncomps)
	if size(color, /type) eq 7 then icolor = load_color(color, bottom=itop+1, itop=itop) $
                               else icolor = color

    ;if ZERO_LINE is set, then we also need XRANGE and YRANGE
    if keyword_set(zero_line) then begin
        if n_elements(xrange) eq 0 || array_equal(xrange, [0,0]) $
            then xrange = [min(x_out, max=xmax), xmax]
            
        if n_elements(yrange) eq 0 || array_equal(yrange, [0,0]) $
            then yrange = [min(y_out, max=ymax), ymax]
    endif
    
    ;if YAX_RIGHT is set, then make the PLOT command that follows omit TITLE and not
    ;draw a y-axis over the one that is already in place (YSTYLE=5)
    if keyword_set(yax_right) then begin
        if n_elements(ystyle) ne 0 then ystyle_right = ystyle
        ystyle = 5
        ytitle_right = ytitle
        ytitle = ''
    endif

;---------------------------------------------------------------------
;Create an Empty Plot, then Overplot the Data in its Respective Color
;---------------------------------------------------------------------
	;create an empty plot
	;use x_out and y_out (instead of, e.g., [0]) so that if XRANGE or YRANGE are not given,
	;they will be calculated properly.
	plot, x_out, y_out, /NODATA, COLOR=iaxiscolor, BACKGROUND=ibackground, $;, TITLE=title, $
	      XRANGE=xrange, YRANGE=yrange, YTITLE=ytitle, YSTYLE=ystyle, POSITION=position, $
	      _EXTRA=extra

	;overplot each component onto the empty plot and annotate the axes
	for i = 0, ncomps - 1 do begin
		if i lt ncolors then color_out = icolor[i] else color_out = iaxiscolor

		;make the line plots within the empty axis that was just created
		oplot, x_out, y_out[i,*], COLOR=color_out, _EXTRA=extra
	endfor

;---------------------------------------------------------------------
;Put the Axis on the Right Side of the Plot //////////////////////////
;---------------------------------------------------------------------
	
	;draw an extra axis, if requested
	if keyword_set(yax_right) then begin
		if n_elements(position) eq 0 then begin
		    ax_loc_x = !x.crange[1]
		    ax_loc_y = !y.crange[1]
		endif else begin
		    ax_loc_x = position[2]
		    ax_loc_y = position[1]
		endelse

		axis, ax_loc_x, ax_loc_y, COLOR=iaxiscolor, /YAXIS, $
			  YTITLE=ytitle_right, YSTYLE=ystyle_right, TICKLEN=-!p.ticklen, $
			  _EXTRA=extra, /NORMAL
	endif


;---------------------------------------------------------------------
;Draw a Dashed Line at Y=0 if Requested //////////////////////////////
;---------------------------------------------------------------------
	
	;plot a line at y = 0 if requested, but only if ymin < 0 < ymax
	if keyword_set(zero_line) then begin
	    if yrange[0] lt 0 and yrange[1] gt 0 then $
            plots, [xrange[0], xrange[1]], [0,0], /DATA, COLOR=iaxiscolor, LINESTYLE=2
    endif

;---------------------------------------------------------------------
;Place the Legend on the Graph in Same Color as Data /////////////////
;---------------------------------------------------------------------
	
	;if a legend was provided, put it on the plot
	if n_elements(legend) ne 0 then begin
	
        ;make sure LEGEND is of type string
        if size(legend, /type) ne 7 then message, 'Legend must be a string [array]'
        
        ;start by making the legend bigger
	    if n_elements(charsize) eq 0 then charsize = 2.0
	
        ;set the offset position of the legend
        xcharsize = float(!d.x_ch_size) / float(!d.x_size) * charsize
        ycharsize = float(!d.y_ch_size) / float(!d.y_size) * charsize
        outs_xoffset = 2.0 * xcharsize
        outs_yoffset = ycharsize
    
        ;add the legend elements to the graph
        for i = 0, n_elements(legend) - 1 do begin
            if i lt ncolors then color_out = icolor[i] else color_out = iaxiscolor
            
            ;label each line in the upper right corner of the graph
            xyouts, !x.window[0] + outs_xoffset, $
                    !y.window[1] - ycharsize - i*outs_yoffset, legend[i], $
                    color=color_out, charsize=charsize, /normal
        endfor
    endif
end
