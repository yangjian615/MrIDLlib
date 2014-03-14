; docformat = 'rst'
;
; NAME:
;       COLOR_BAR
;
; PURPOSE:
;+
;       The purpose of this program is to create a color bar that indicates the color
;       scale of an image.
;
; :Categories:
;       Data Visualization
;
; :Params:
;       POSITION:       in, required, type=fltarr(4)
;                       The position of the bottom left and top right corners of the graph
;                           by which to place the color bar: [x0, y0, x1, y1]. For example,
;                           if the position of the plot was generated automatically, use
;                               color_bar, !p.position
;
; :Keywords:
;       AXCOLOR:        in, optional, type=8 bit color index/string, default='black'
;                       Color of the axes (i.e. 'white'): c.f. load_color()
;       BAR_OFFSET:     in, optional, type=float, default=1
;                       Distance between the plot and the colorbar in character widths
;       BAR_WIDTH:      in, optional, type=int, default=20
;                       Width of the color bar, given in the same units as POSITION
;       BGCOLOR:        in, optional, type=string/color index/color triple, default='white'
;                       Overrides the BACKGROUND keyword in PLOT. Determines the color of
;                           the background on top of which the image will be displayed.
;       DEVICE:         in, optional, type=Boolean, default=0
;                       The color bar position is supplied in device coordinates
;       FGCOLOR:        in, optional, type=string/color index/color triple, default='Black'
;                       Overrides the COLOR keyword in PLOT. Determines the color of
;                           the foreground used in drawing axes and annotations.
;       HORIZONTAL:     in, optional, type=Boolean, default=0
;                       Create a horizontal colorbar above the graph that spans the length
;                           of the graph. The default is to create a vertical colorbar on
;                           the right side of the graph.
;       CBBOTTOM:       in, optional, type=byte, default=0
;                       The minimum color table index that the color bar will display
;       CBRANGE:        in, optional, type=fltarr(2), default=Highest Color Table Index
;                       The data range of the color bar: [min, max]. If not set, the color
;                           scale indices will be used
;       CBTITLE:        in, optional, type=String, default=''
;                       A title to put on the colorbar.
;       CBTOP:          in, optional, type=byte, default=!d.table_size-1
;                       The maximum color table index value that the color bar will display.
;       CB_CTINDEX:     in, optional, type=int, default=13
;                       The color table to load when displaying the color bar.
;       TTL_OFFSET:     in, optional, type=Int, default=8
;                       Distance in normal coordinates between the color bar axis and the
;                       title (can be < 0). This leaves room for ticklabels on the axis.
;       CBLOG:          in, optional, type=Boolean, default=0
;                       Specify that the color bar axis is to be a log-scaled.
;       NOERASE:        in, optional, type=Boolean, default=1
;                       Do not erase the window when the colorbar is made.
;       NORMAL:         in, optional, type=Boolean, default=1
;                       Indicate that `position` is given in normal coordinates.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2011
;
; :History:
;   Modification History::
;
;       Written By  -   Matthew Argall 21 September 2011
;       10/01/2011  -   Changed floor to round in removal of partial pixels (nomal->device
;                          coords) - MRA
;       10/02/2011  -   Added call to PLOT before AXIS to position the bar properly. If the
;                          axis is drawn in the [xy] direction, then AXIS ignores positioning
;                          in the [xy] direction; it uses system variables instead - MRA
;                       Added BAR_OFFSET, BAR_WIDTH, and, TTL_OFFSET keywords - MRA
;       10/05/2011  -   Fixed tv positioning for postscript output - MRA
;       04/25/2012  -   Change everything to device coords before plotting
;                       Added _EXTRA inheritance keyword - MRA
;       11/14/2012  -   Prefixed "CB" to TITLE, XLOG, YLOG, and CHARSIZE -- keywords common to
;                          line and image plots. This way Color_Bar keywords can be inherited
;                          without causing 'duplicate keyword' errors. Other common keywords,
;                          like DEVICE, CHARSIZE, NORMAL, etc. have not been prefixed - with
;                          the thought that they will be specified explicitly by the calling
;                          program to create a uniformity in appearance.
;                       Removed CBXLOG and CBYLOG in favor of CBLOG
;       11/19/2012  -   Added FGCOLOR and BGCOLOR to dictate the foreground and background
;                          color of the display.
;                       Added NOERASE keyword. If NOERASE=1, then the background color does
;                          not change.
;       03/18/2013  -   Added COLOR_TABLE keyword. - MRA
;       03/18/2013  -   Added CBTICKFORMAT keyword. - MRA
;       04/13/2013  -   Renamed COLOR_TABLE to CB_CTINDEX because it conflicted with COLOR. - MRA
;       04/14/2013  -   Added  CBBOTTOM, CBTOP. - MRA
;       04/18/2013  -   Do not load a color table by default. Use whichever is loaded. - MRA
;       09/07/2013  -   If no color-table index was given, restore the table that was
;                           present at the beginning to erase AXCOLOR, and BGCOLOR. - MRA
;-
pro color_bar, position, $
;KEYWORDS
AXCOLOR = axcolor, $
BAR_OFFSET = bar_offset, $
BAR_WIDTH = bar_width, $
BGCOLOR = bgcolor, $
CBBOTTOM = cbbottom, $
CBTICKFORMAT = cbtickformat, $
CBLOG = cblog, $
CBRANGE = cbrange, $
CBTITLE = cbtitle, $
CBTOP = cbtop, $
CHARSIZE = charsize, $
CB_CTINDEX = cb_ctindex, $
DEVICE = device, $
FGCOLOR = fgcolor, $
HORIZONTAL = horizontal, $
NOERASE = noerase, $
NORMAL = normal, $
TTL_OFFSET = ttl_offset, $
_EXTRA = extra
    compile_opt idl2
    
    ;Catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        ;Return to the initial state
        if n_elements(r) ne 0 then tvlct, r, g, b
        if n_elements(init_decomp) ne 0 then device, DECOMPOSED=init_decomp
        if n_elements(init_device) ne 0 then set_plot, init_device
        
        void = cgErrorMsg()
    endif

    ;Save the current color table        
    tvlct, r, g, b, /GET

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;set some default values if the keywords are not set.
	itop = 1
	if n_elements(axcolor) eq 0 then axcolor='black'
	if size(axcolor, /type) eq 7 then axcolor_index = load_color(axcolor, bottom=itop, itop=itop) $
	                             else axcolor_index = axcolor
	if n_elements(fgcolor) eq 0 then fgcolor = 'black'
	if size(fgcolor, /type) eq 7 then fgcolor_index = load_color(fgcolor, bottom=itop+1, itop=itop) $
	                             else fgcolor_index = fgcolor
	if n_elements(bgcolor) eq 0 then bgcolor = 'white'
	if size(bgcolor, /type) eq 7 then bgcolor_index = load_color(bgcolor, bottom=itop+1, itop=itop) $
	                             else bgcolor_index = bgcolor
	if n_elements(cbcharsize) eq 0 then cbcharsize = 1
	if n_elements(cbtitle) eq 0 then cbtitle = ''
	if n_elements(cblog) eq 0 then cblog = 0
	if n_elements(cbrange) eq 0 then cbrange = [0, 255]
	if n_elements(position) ne 4 then message, 'Position needs to be an array [x0, y0, x1, y1]'
	if n_elements(normal) eq 0 and n_elements(device) eq 0 then normal = 1
	if keyword_set(horizontal) then yaxis=0 else yaxis=1
	if n_elements(cbbottom) eq 0 then cbbottom = 0B
	if n_elements(cbtop) eq 0 then cbtop = !d.table_size-1B
    
;---------------------------------------------------------------------
;Convert to Device Coordinates ///////////////////////////////////////
;---------------------------------------------------------------------

	;calculate the dimensions of the current font
    ;convert everything to device coordinates
    x_charsize = !d.x_ch_size
    y_charsize = !d.y_ch_size
    
    ;in character units -- independent of device and normal coords
    if n_elements(bar_offset) eq 0 then bar_offset = x_charsize $
                                   else bar_offset = bar_offset * x_charsize
    if n_elements(ttl_offset) eq 0 then ttl_offset = 8 * x_charsize $
                                   else ttl_offset = ttl_offset * x_charsize

    ;keep device coordinates -- convert normal to device
	if keyword_set(device) then begin
		if n_elements(bar_width) eq 0 then bar_width = 20
		
		pos_dev = position
	endif else begin    
		pos_dev = lonarr(4)
		pos_dev[[0,2]] = floor(position[[0,2]] * !d.x_vsize)
		pos_dev[[1,3]] = floor(position[[1,3]] * !d.y_vsize)
		
		if n_elements(bar_width) eq 0 then bar_width = 20 $
		                              else bar_width = bar_width * !d.x_vsize
	endelse
	normal = 0
	device = 1

;---------------------------------------------------------------------
;Create the Color Bar ////////////////////////////////////////////////
;---------------------------------------------------------------------

	;create the colorbar. TV only works with 8 bit indexed color scales, so I create an
	;8 bit byte array and replicate it along ten rows. I then take the transpose to make
	;it "vertical" -- 256 rows, 10 columns
	bar = transpose((bindgen(cbtop-cbbottom+1)+cbbottom) # replicate(1B, 100))

	;position the colorbar above the graph, with some space between the two
	;calculate the size of the bar in pixels
	;find the starting position of the bar in pixels
	;change the size of the colorbar to the specified dimensions and draw a box around it
	if keyword_set(horizontal) then begin
		;transpose to make horizontal
		bar = transpose(bar)
		bar_pos_dev = [pos_dev[0], pos_dev[3]+bar_space, pos_dev[2], pos_dev[3]+bar_space+bar_width]
	endif else begin
		bar_pos_dev = [pos_dev[2]+bar_offset, pos_dev[1], pos_dev[2]+bar_offset+bar_width, pos_dev[3]]
	endelse

	xbar_size = (bar_pos_dev[2] - bar_pos_dev[0]) - 1
	ybar_size = (bar_pos_dev[3] - bar_pos_dev[1]) - 1
	xbar_start = bar_pos_dev[0] + 1
	ybar_start = bar_pos_dev[1] + 1
    
;---------------------------------------------------------------------
;Outline the Color Bar and Draw an Axis //////////////////////////////
;---------------------------------------------------------------------

	plot, [0], BACKGROUND=bgcolor_index, /NODATA, NOERASE=noerase, NORMAL=normal, DEVICE=device, $
		  POSITION=bar_pos_dev, COLOR=fgcolor_index, $
		  XTICKS=1, YTICKS=1, $
		  XTICKFORMAT='(a1)', YTICKFORMAT='(a1)'

	axis, YAXIS=yaxis, NORMAL=normal, DEVICE=device, COLOR=axcolor_index, $
	      XRANGE=cbrange, XLOG=cblog, XTICKLEN=-0.02, XSTYLE=1, XTICKFORMAT=cbtickformat, $
	      YRANGE=cbrange, YLOG=cblog, YTICKLEN=-0.02, YSTYLE=1, YTICKFORMAT=cbtickformat, $
		  CHARSIZE=charsize

	;put a title on the colorbar
	if keyword_set(horizontal) then begin
		xyouts, bar_pos_dev[0]+(bar_pos_dev[2]-bar_pos_dev[0])/2, $
				bar_pos_dev[1]+bar_space, $
				DEVICE=device, NORMAL=normal, $
				cbtitle, ALIGNMENT=0.5, CHARSIZE=charsize, $
				COLOR=axcolor_index
	endif else begin
		xyouts, bar_pos_dev[2]+ttl_offset, $
				bar_pos_dev[1]+(bar_pos_dev[3]-bar_pos_dev[1])/2, $
				DEVICE=device, NORMAL=normal, $
				cbtitle, ALIGNMENT=0.5, ORIENTATION=270, CHARSIZE=charsize, $
				COLOR=axcolor_index
	endelse
    
;---------------------------------------------------------------------
;Display the Color Bar ///////////////////////////////////////////////
;---------------------------------------------------------------------

	init_decomp = getdecomposedstate()
	init_device = !d.name
	device, decomposed=0

	;If a color table index was given, load the color table. If not, reset the color table
	;to what it was before COLOR and BACKGROUND were given.
	if n_elements(cb_ctindex) gt 0 $
	    then loadct, cb_ctindex, /SILENT $
	    else tvlct, r, g, b
	    
	;	tvscl, congrid(bar, xbar_size, ybar_size), xbar_start, ybar_start
	if !d.name eq 'PS' then begin
		tv, bar, xbar_start, ybar_start, xsize=xbar_size, ysize=ybar_size, /device;, /inches
	endif else begin
		tv, congrid(bar, xbar_size, ybar_size), xbar_start, ybar_start, /device
	endelse
	
	tvlct, r, g, b
	device, decomposed=init_decomp
	set_plot, init_device

end