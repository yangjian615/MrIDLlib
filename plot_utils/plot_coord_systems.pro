; docformat = 'rst'
;
; NAME:
;       PLOT_COORD_SYSTEMS
;
; PURPOSE:
;+
;       Plot a second orthonormal basis within the standard (x, y, z) cartesian coordinate
;       system.
;
; :Categories:
;
;       Graphics Utility, Coordinate Systems
;
; :Examples:
;   See the main-level program at the end of this file::
;
;       IDL> .r plot_coord_systems
;
; :Params:
;
;       FRAME:          in, required, type="float(3,3)"
;                       A matrix that transforms a vector from normal (x, y, z) coordinate
;                           system into the new (x', y', z') frame. 
;                           If::
;                               x' = A * x
;
;                           then Frame = A
;       AXES_LABELS:    in, optional, type=strarr(3)
;                       Labels to be placed on the FRAME coordinate system axes.
;
; :Keywords:
;       AX:             in, optional, type=float, default=30
;                       This keyword specifies the angle of rotation, about the X axis,
;                           in degrees towards the viewer. This changes the orientation
;                           of the 3D view.
;       AZ:             in, optional, type=float, default=-30
;                       This keyword specifies the counterclockwise angle of rotation
;                           about the Z axis. This changes the orientation of the 3D view.
;       COLORS:         in, optional, type=lonarr(3), default="cgColor['blue', 'green', 'red'])"
;                       The color of each coodinate axes of FRAME. If decomposed color is
;                           being used, the color indices cannot be loaded into indices
;                           1 or 2. Those are used for white and black, respectively.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;	Modification History::
;
;       10/29/2012  -   Written by Matthew Argall
;       11/01/2012  -   Swapped the foreground and background colors: now black on white
;       07/09/2013  -   Added the AZ and AX keywords. - MRA
;       2014/01/30  -   Replaced Load_Color with cgColor. - MRA
;-
pro plot_coord_systems, frame, axes_labels, $
AX = ax, $
AZ = az, $
COLORS = colors
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    if n_elements(ax) eq 0 then ax = 30
    if n_elements(az) eq 0 then az = -30
    if n_params() eq 1 then axes_labels = ["X'", "Y'", "Z'"]
    ncolors = n_elements(colors)
    
    ;To plot black on white in 24-bit color mode, we must get a hexidecimaml number.
    ;If the decomposed state is off (8-bit mode) then load white and black into indices
    ;1 and 2 of the current color table
    device, get_decomposed=decomposed_state
    if decomposed_state eq 1 then begin
        if ncolors eq 0 then colors = cgColor(['blue', 'dark_green', 'red'])
        white = cgColor('white')
        black = cgColor('black')
    endif else begin
        loadct, r, g, b, /get
        if ncolors eq 0 then colors = cgColor(['blue', 'dark_green', 'red'], ITOP=itop)
    
        bw = cgColor(['white', 'black'], BOTTOM=itop)
        white = bw[0]
        black = bw[1]
    endelse
    
    ;the reference coordinate system is the identity matrix
    ref_labels = ['X', 'Y', 'Z']
    
    ;create a circle of radius r=1
    x = findgen(101) / 50 - 1
    y = sqrt(1 - x^2)
    
    ;define the plot types.
    xy = [[0, 2], $     ;z vs. x
          [0, 1], $     ;y vs. x
          [1, 2]]       ;z vs. y
    
;---------------------------------------------------------------------
;2D Plots /////.//////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;get the positions of the plots
    window, 1
    positions = MrLayout([3,1], XGAP=9, ASPECT=1.0)

    for i = 0, 2 do begin
        x0 = replicate(0,3)
        x1 = transpose(frame[*, xy[0,i]])
        y0 = replicate(0,3)
        y1 = transpose(frame[*, xy[1,i]])
        
        if i gt 0 then noerase=1
        
        ;Draw a circle of r=1 inside the plotting region to give an idea of the length of
        ;each unit vector in FRAME.
        plot, x, y, $
              COLOR=black, BACKGROUND=white, $
              XTITLE=ref_labels[xy[0,i]], XRANGE=[-1,1], $
              YTITLE=ref_labels[xy[1,i]], YRANGE=[-1,1], $
              position=reform(positions[*,i,0]), NOERASE=noerase
        oplot, x, -y, COLOR=black
        
        ;draw each unit vector in FRAME a different colored arrow
        arrow, x0[0], y0[0], x1[0], y1[0], /DATA, COLOR=colors[0]
        arrow, x0[1], y0[1], x1[1], y1[1], /DATA, COLOR=colors[1]
        arrow, x0[2], y0[2], x1[2], y1[2], /DATA, COLOR=colors[2]
        
        ;plot FRAME's labels on each arrow
        xyouts, x1, y1, axes_labels, /DATA, CHARSIZE=1.5, COLOR=colors
    endfor
    
;---------------------------------------------------------------------
;3D Plot /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    window, 2
    ;make a 3D plot
    surface, fltarr(2,2), /NODATA, $
             COLOR=black, BACKGROUND=white, $
             XRANGE=[-1,1], YRANGE=[-1,1], ZRANGE=[-1,1], /SAVE, $
             XSTYLE=5, YSTYLE=5, ZSTYLE=5, AZ=az, AX=ax
    
    ;draw the axes so that they intersect at the origin, not so that they are bordering
    ;the plotting region
    axis, 0, /XAXIS, COLOR=black, /T3D
    axis, 0, /YAXIS, COLOR=black, /T3D
    axis, 0, /ZAXIS, COLOR=black, /T3D
    
    ;label each axis
    xyouts, 1, 0, Z=0, 'X', COLOR=black, /DATA, /T3D, CHARSIZE=3, TEXT_AXES=2
    xyouts, 0, 1, Z=0, 'Y', COLOR=black, /DATA, /T3D, CHARSIZE=3, TEXT_AXES=2
    xyouts, 0, 0, Z=1, 'Z', COLOR=black, /DATA, /T3D, CHARSIZE=3, TEXT_AXES=2
    
    ;draw the new frame
    tri_arrow, [replicate(0,1,3), transpose(frame[*,0])], $
               [replicate(0,1,3), transpose(frame[*,1])], $
               [replicate(0,1,3), transpose(frame[*,2])], $
               COLOR=colors, LABEL=axes_labels, /DATA
    
    ;draw lines connecting the new frame to the old frame to better visualize the
    ;relationship
    for i = 0, 2 do begin
        ;draw a line over to the z-axis
        plots, [frame[i,0], 0], [frame[i,1], 0], [frame[i,2], frame[i,2]], $
               COLOR=colors[i], LINESTYLE=2, /DATA, /T3D
        
        ;draw a line down to the xy-plane
        plots, [frame[i,0], frame[i,0]], [frame[i,1], frame[i,1]], [frame[i,2], 0], $
               COLOR=colors[i], LINESTYLE=2, /DATA, /T3D
        
        ;draw a line from the xy-plane to the x-axis
        plots, [frame[i,0], frame[i,0]], [frame[i,1], 0], [0, 0], $
               COLOR=colors[i], LINESTYLE=2, /DATA, /T3D
        
        ;draw a line from the xy-plane to the y-axis
        plots, [frame[i,0], 0], [frame[i,1], frame[i,1]], [0, 0], $
               COLOR=colors[i], LINESTYLE=2, /DATA, /T3D
    endfor
    
    ;reset the color table if need be
    if decomposed_state eq 0 then loadct, r, g, b
end


;---------------------------------------------------
; Main Level Example Program (.r plot_coord_systems)
;---------------------------------------------------

;Say you have a coordinate system transformation x' = A ## x where
A = [[ 0.8451,  0.2598,  0.4673], $
     [-0.3757,  0.9104,  0.1732], $
     [-0.3804, -0.3219,  0.8670]]
     
;Then to see how the x' frame looks within the frame of x,
plot_coord_systems, A
end