; docformat = 'rst'
;
; NAME:
;       LINE_PLOTS
;
; PURPOSE:
;+
;       A wrapper for the CONTOUR routine that sets the default colors. The foreground
;       and contours are white while the background is black.
;
; :Categories:
;
;       Wrapper, Contour, Data Visualization
;
; :Params:
;
;       X:              in, optional, type=real numeric
;                       A vector representing the abscissa values to be plotted. 
;                           If X is not specified, Y is plotted as a function of point number 
;                           (starting at zero). If both arguments are provided, Y is plotted
;                           as a function of X.
;       Y:              in, optional, type=real numeric
;                       The ordinate data to be plotted. Y should be a column vector. If
;                           it is a 1D row-vector, the transpose will be taken. If it is
;                           2D, then each column of data will be plotted independently
;                       
; :Keywords:
;
;   BGCOLOR:            in, optional, type=string/color index, default='white'
;                       the background color of the plot (no longer BACKGROUND)
;   FGCOLOR:            in, optional, type=string/color index, default='black'
;                       the forground color of the plot (no longer COLOR)
;   POSITION:           in, optional, type=fltarr(4)
;                       positon of the plot [x0, y0, x1, y1] where (x0, y0) specifies the
;                           lower left and (x1, y1) specifies the upper right corner of the plot
;   COLOR:              in, optional, type=string/color index/24-bit color, default='black'
;                       the numeric value of the color of each component. Can have
;                           any number of elements, but only the first N will be used,
;                           where N is the number of components of y and, separately,
;                           the number of elements in legend. The default is the
;                           foreground color of the plot.
;   C_COLOR:            in, optional, type=string/color index/RGB, default='black'
;                       The color of each contour line drawn.
;   C_SPACING           in, optional, type=float, default=!Null
;                       The spacing in centimeters between lines used to fill contours
;                           when `FILL` or `CELL_FILL` are set.
;   CELL_FILL           in, optional, type=Boolean, default=0
;                       Produce a filled contour plot using a “cell filling” algorithm. 
;                           Use this keyword instead of FILL when you are drawing filled
;                           contours over a map, when you have missing data, or when 
;                           contours that extend off the edges of the contour plot
;   FILL                in, optional, type=Boolean, default=0
;                       Set this keyword to produce a filled contour plot. The contours
;                           are filled with solid or line-filled polygons.
;   _EXTRA:             in, optional, type=structure
;                       a structure containing any plotting keywords to be inherited
;                           by CONTOUR
;
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
;       03/09/2013  -   Written by Matthew Argall
;       03/10/2013  -   Added CELL_FILL, FILL, C_SPACING, and C_ORIENTATION keywords. The
;                           documentation says that C_SPACING and C_ORIENTATION are only
;                           used if CELL_FILL and FILL are set, but this is not the case. 
;                           C_SPACING and C_ORIENTATION are now explicitly set to !Null 
;                           if neither of the other keywords are set. - MRA
;-
pro contour_plots, z, x, y, $
;PLOT KEYWORDS -- will be extracted from _EXTRA
BGCOLOR = bgcolor, $
FGCOLOR = fgcolor, $
POSITION = position, $
;WRAPPER KEYWORDS
COLOR = color, $
C_COLORS = c_colors, $
CELL_FILL = cell_fill, $
FILL = fill, $
C_SPACING = c_spacing, $
C_ORIENTATION = c_orientation, $
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
;CHECK INPUTS ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    nx = n_elements(x)
    ny = n_elements(y)

    ;C_SPACING over-rides the CELL_FILL and FILL keywords, even though it should not.
    ;Must make C_SPACING undefined if neither of the other two keywords are set.
    if n_elements(cell_fill) eq 0 then cell_fill = 0
    if n_elements(fill) eq 0 then fill = 0
    if cell_fill eq 0 and fill eq 0 then begin
        c_spacing = !Null
        c_orientation = !Null
    endif

;---------------------------------------------------------------------
;CHECK COLORS ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;get the current color table	
    ;start loading colors at color index 1 (1 is added later)
    tvlct, r, g, b, /get
    itop = 0

    ;default to white background
	if n_elements(bgcolor) eq 0 then bgcolor = 'white'
	if size(bgcolor, /type) eq 7 $
	    then ibgcolor = load_color(bgcolor, bottom=itop+1, itop=itop) $
        else ibgcolor = bgcolor
	
	;default to black foreground
	if n_elements(fgcolor) eq 0 then fgcolor = 'black'
    if size(fgcolor, /type) eq 7 $
        then ifgcolor = load_color(fgcolor, bottom=itop+1, itop=itop) $
        else ifgcolor = fgcolor

	;if no colors were supplied, make the colors the forground color
	ncolors = n_elements(color)
	if ncolors eq 0 then color = ifgcolor
	if size(color, /type) eq 7 $
	    then icolor = load_color(color, bottom=itop+1, itop=itop) $
        else icolor = color
    
    ;if no c_colors were supplied, make the colors the forground color.
    nc_colors = n_elements(c_color)
    if nc_colors eq 0 then c_colors = ifgcolor
    if size(c_colors, /type) eq 7 $
        then ic_colors = load_color(c_colors, bottom=itop+1, itop=itop) $
        else ic_colors = c_colors
            
;---------------------------------------------------------------------
;Make the Contour Plot ///////////////////////////////////////////////
;---------------------------------------------------------------------
	if nx eq 0 and ny eq 0 then begin
	    contour, z, BACKGROUND=ibgcolor, COLOR=ifgcolor, C_COLORS=ic_colors, $
                    C_SPACING=c_spacing, CELL_FILL=cell_fill, FILL=fill, $
                    C_ORIENTATION=c_orientation, POSITION=position, _EXTRA=extra
	endif else begin 
	    contour, z, x, y, BACKGROUND=ibgcolor, COLOR=ifgcolor, C_COLORS=ic_colors, $
                          C_SPACING=c_spacing, CELL_FILL=cell_fill, FILL=fill, $
                          C_ORIENTATION=c_orientation, POSITION=position, _EXTRA=extra
	endelse
    
    ;reset the color table             
	tvlct, r, g, b

end