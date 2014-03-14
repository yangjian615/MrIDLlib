; docformat = 'rst'
;
; NAME:
;       TRI_ARROW
;
; PURPOSE:
;+
;       Draw arrows on 3D coordinate axes.
;
; :Categories:
;
;       3D Graphics, Plot Utilities, Annotation
;
; :Params:
;
;       X:              in, required, type="fltarr(2,*)"
;                       [[x0, x1], ...]: the x coordintes of the arrow tail (x0) and arrow
;                           head (x1).
;       Y:              in, required, type="fltarr(2,*)"
;                       [[y0, y1], ...]: the y coordintes of the arrow tail (y0) and arrow
;                           head (y1).
;       Z:              in, required, type="fltarr(2,*)"
;                       [[z0, z1], ...]: the z coordintes of the arrow tail (z0) and arrow
;                           head (z1). X, Y, and Z must all have the same number of elements.
;
; :Keywords:
;
;	    DATA:           in, optional, type=Boolean, default=0
;                       if set, implies that coordinates are in data coords.
;	    NORMALIZED:     in, optional, type=Boolean, default=0
;                       if set, coordinates are specified in normalized coords.
;	    HSIZE:          in, optional, type=float, default=1/64th the width of the device\, (!D.X_SIZE / 64.).
;		                If the size is positive, it is assumed to be in device
;		                    coordinate units.  If it is NEGATIVE, then the head length
;		                    is set to the vector length * abs(hsize), giving heads
;		                    proportional in size to the bodies.  The size is defined as
;		                    the length of each of the lines (separated by 60 degrees)
;		                    that make the head.
;	    COLOR:          in, optional, type=lonarr(3). default=highest color index
;                       drawing color.
;	    HTHICK:         in, optional, type=float, default=1.0
;                       thickness of heads.
;	    SOLID:          in, optional, type=Boolean, default=0
;                       if set, make a solid arrow, using polygon fills, looks better
;		                for thick arrows.
;	    THICK:          in, optional, type=Boolean, default=1.0
;                       thickness of body.
;       LABEL:          in, optional, type=strarr(3), default="['', '', '']"
;                       Axes label for the (x', y', z') axes.
;       COLOR:          in, optional, type=lonarr(3). default: NONE
;                       The color of each coodinate axes of FRAME
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
;       10/29/2012      -   Modified from IDL's ARROW.PRO by Matthew Argall
;-
PRO tri_arrow, x, y, z, $
HSIZE = hsize, $
COLOR = color, $
LABEL = label, $
HTHICK = hthick, $
THICK = thick, $
DATA = data, $
NORMALIZED = norm, $
SOLID = solid

COMPILE_OPT idl2

;ON_ERROR, 2
    ;  Set up keyword params
    
    if n_elements(thick) eq 0 then thick = 1.
    if n_elements(hthick) eq 0 then hthick = thick
    
    ;Head size in device units
    if n_elements(hsize) eq 0 then arrowsize = !d.x_size/64. * (hthick/2. > 1) $
                              else arrowsize = float(hsize)
    if n_elements(label) eq 0 then label = replicate('', n_elements(x[0,*]))
    if n_elements(color) eq 1 and n_elements(color) ne n_elements(x[0,*]) $
        then color = replicate(color, n_elements(x[0,*]))
    if n_elements(color) eq 0 then color = !P.color
    
    ;We use 30 degrees for head angle
    cosA = cos(30d * !pi/180d)
    sinA = sin(30d * !pi/180d)
    
    for i = 0L, n_elements(x[0,*])-1 do begin		;Each vector
    
        ;Convert to device coordinates
        if keyword_set(data) $		;Convert?
            then p = convert_coord([x[0,i],x[1,i]], [y[0,i],y[1,i]], [z[0,i],z[1,i]], /data, /to_dev, /t3d) $
        else if keyword_set(normal) $
            then p = convert_coord([x[0,i],x[1,i]], [y[0,i],y[1,i]], [z[0,i],z[1,i]], /norm, /to_dev, /t3d) $
            else p = [  [x[0,i],x[1,i]], [y[0,i],y[1,i]], [z[0,i],z[1,i]]  ]
    
        ;the x, y, and z device coordinates of the arrows
        xp = [p[0,0], p[0,1]]
        yp = [p[1,0], p[1,1]]
        zp = [p[2,0], p[2,1]]
    
        ;the length of each arrow
        dx = xp[1] - xp[0]
        dy = yp[1] - yp[0]
        dz = zp[1] - zp[0]
        r = sqrt(dx^2d + dy^2d + dz^2d)	;Length
    
        ;Get the angles between the vector and each axis. Theta is the polar angle,
        ;increasing positively from x to y. Phi is the azimuthal angle increasing
        ;positively from +z down to the xy-plane (as in spherical coordinates).
        if r gt 0 then begin
            Rxy = sqrt(dx^2 + dy^2)
            cTheta = dx/Rxy	            ;cos(theta)
            sTheta = dy/Rxy             ;sin(theta)
            cPhi = dz/r                 ;cos(phi)
            sPhi = Rxy/r                ;sin(phi)
        endif else begin
            cTheta = 1
            sTheta = 0
            cPhi = 0
            sPhi = 1
            r = 1.
        endelse
        
        if arrowsize gt 0 then a = arrowsize $  ;a = length of head
                          else a = -r * arrowsize
    
        ;Now we are going to make a pyramid with the tip of the pyramid at the head of the
        ;arrow's shaft. The pyramid will have a square base with corners c1, c2, c3 and c4,
        ;located at (xi, yi, zi) where i=1,2,3,4. The base of the pyramid is perpendicular to
        ;the arrow shaft.
        ;
        ;Break the rising edges of the pyramid into parallel and perpendicular components.
        a_par_x = a * cosA * sPhi * cTheta
        a_par_y = a * cosA * sPhi * sTheta
        a_par_z = a * cosA * cPhi
        
        ;two parts of the arrow head will always be in the xy-plane (Phi = 90)
        a12_perp_x = a * sinA * sPhi * sTheta
        a12_perp_y = a * sinA * sPhi * cTheta
        a12_perp_z = a * sinA * cPhi
        
        ;the other two will always be in the xz-plane (Theta = 0)
        a34_perp_x = a * sinA * cPhi * cTheta
        a34_perp_y = a * sinA * cPhi * sTheta
        a34_perp_z = a * sinA * sPhi
        
        ;Then add the parallel and perpendicular components together to get the locations of
        ;the corners of the pyramid.
        x1 = xp[1] - a_par_x + a12_perp_x
        y1 = yp[1] - a_par_y - a12_perp_y
        z1 = zp[1] - a_par_z; - a12_perp_z
        
        x2 = xp[1] - a_par_x - a12_perp_x
        y2 = yp[1] - a_par_y + a12_perp_y
        z2 = zp[1] - a_par_z; + a12_perp_z
        
        x3 = xp[1] - a_par_x + a34_perp_x
        y3 = yp[1] - a_par_y; - a34_perp_y
        z3 = zp[1] - a_par_z - a34_perp_z
        
        x4 = xp[1] - a_par_x - a34_perp_x
        y4 = yp[1] - a_par_y; + a34_perp_y
        z4 = zp[1] - a_par_z + a34_perp_z
    
        if keyword_set(solid) then begin	;Use polyfill?
            b = a * (-cosA) * .9d	;End of arrow shaft (Fudge to force join)
            
            ;draw the arrow shaft
            plots, [xp[0], xp[1]+b*cTheta], [yp[0], yp[1]+b*sTheta], [zp[0], zp[1]+b*cPhi], /DEVICE, $
                   COLOR=color[i], THICK=thick, /T3D
            
            ;if you draw an x in the base of the pyramid then extend the lines up to the
            ;top so that you have two crossing triangles, then those are the two triangles
            ;that will be filled in, not the entire pyramid
            polyfill, [x1, xp[1], x2, x1], [y1, yp[1], y2, y1], [z1, zp[1], z2, z1], $
                      /DEVICE, COLOR=color[i], /T3D
            polyfill, [x3, xp[1], x4, x3], [y3, yp[1], y4, y3], [z3, zp[1], z4, z3], $
                      /DEVICE, COLOR=color[i], /T3D
                      
        endif else begin
            ;draw the arrow's shaft and put a label on it
            plots, [xp[0], xp[1]], [yp[0], yp[1]], [zp[0], zp[1]], /DEVICE, $
                   COLOR = color[i], THICK = thick, /T3D
            xyouts, x[1,i], y[1,i], Z=z[1,i], label[i], $
                    COLOR=color[i], /DATA, /T3D, ORIENTATION=0, TEXT_AXES=1, CHARSIZE=3
            
            ;draw half of the arrow head
            plots, [x1, xp[1], x2], [y1, yp[1], y2], [z1, zp[1], z2], /DEVICE, COLOR=color[i], $
                   THICK=hthick, /T3D
            
            ;draw the other half of the arrow head
            plots, [x3, xp[1], x4], [y3, yp[1], y4], [z3, zp[1], z4], /DEVICE, COLOR=color[i], $
                   THICK=hthick, /T3D
        endelse
    ENDFOR
end
