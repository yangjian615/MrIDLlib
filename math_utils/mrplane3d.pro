; docformat = 'rst'
;
; NAME:
;    MrLine3D
;
; PURPOSE:
;+
;   Create a series of point along a line in 3D space, given to points that define the line.
;
; :Categories:
;    Bill Daughton, Simulation
;
; :Params:
;       R0:             in, required, type=fltarr(3)
;                       A point in cartesian space through which the plane should pass.
;       R1:             in, required, type=fltarr(3)
;                       A point in cartesian space through which the plane should pass.
;       R2:             in, required, type=fltarr(3)
;                       A point in cartesian space through which the plane should pass.
;
; :Keywords:
;       NX:             in, optional, type=integer, default=100
;                       In the frame of the plane, the number of points along the
;                           horizontal dimension.
;       NY:             in, optional, type=integer, default=100
;                       In the frame of the plane, the number of points along the
;                           vetical dimension.
;
; :Returns:
;       PLANE:          out, required, type=flaot(3\,`NX`*`NY`)
;                       Points on the plane containing `R0`, `R1`, and `R2`.
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 113
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@wildcats.unh.edu
;
; :History:
;    Modification History::
;       2014/10/28  -   Written by Matthew Argall
;-
function MrPlane3D, r0, r1, r2, $
NX=nx, $
NY=ny
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    if n_elements(nx) eq 0 then nx = 100
    if n_elements(ny) eq 0 then ny = 100
    
    ;Make sure difference vectors have more than 5 degrees of separation.
    dr01  = r1 - r0
    dr02  = r2 - r0
    angle = acos( total(dr01*dr02) / ( sqrt(total(dr01^2)) * sqrt(total(dr02^2)) ) )
    if (angle * !radeg) lt 5.0 then message, '(r1 - r0) and (r2 - r0) must have > 5 degrees of separation.'

    ;Form the normal vector
    ;   - This is the z-direction in the frame of the plane
    dr01xdr02 = crossp(dr01, dr02)
    n         = dr01xdr02 / sqrt( total(dr01xdr02^2) )
    
    ;Coordinate system in the frame of the plane
    ;   - pz is along the plane normal
    ;   - px is along dr01
    ;   - py = pz x px
    pz = temporary(n)
    px = dr01 / sqrt(total(dr01^2))
    py = crossp(pz, px)
    
    ;Coordinates of points within the plane
    plane      = fltarr(3, nx*ny)
    plane[0,*] = reform(rebin(findgen(1, nx), ny, nx), nx*ny)
    plane[1,*] = reform(rebin(findgen(ny), ny, nx), nx*ny)
    
    ;Create a transformation matrix to move from the frame of the plane to
    ;the original frame of reference.
    ref2plane = [[px], [py], [pz]]
    plane2ref = transpose(ref2plane)
    
    ;Transform into the original frame of reference
    ref = plane
    ref[0,*] = plane2ref[0,0]*plane[0,*] + plane2ref[1,0]*plane[1,*] + plane2ref[2,0]*plane[2,*]
    ref[1,*] = plane2ref[0,1]*plane[0,*] + plane2ref[1,1]*plane[1,*] + plane2ref[2,1]*plane[2,*]
    ref[2,*] = plane2ref[0,2]*plane[0,*] + plane2ref[1,2]*plane[1,*] + plane2ref[2,2]*plane[2,*]
    
    return, ref
end




;-------------------------------------------------------
; Main Level Program (.r MrLine3D) /////////////////////
;-------------------------------------------------------
;Define three points
r0 = [10.0, 0.0, 10.0]
r1 = [ 3.0, 0.0,  6.0]
r2 = [ 1.0, 0.0, 10.0]

;Create a line with 11 points
plane = MrPlane3D(r0, r1, r2, NX=11, NY=11)
print, plane
end