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
;                       A point in cartesian space through which the line should pass.
;       R1:             in, required, type=fltarr(3)
;                       A point in cartesian space through which the line should pass.
;
; :Keywords:
;       NPOINTS:        in, optional, type=integer, default=100
;                       Number of points to pass between `X0` and `X1`.
;
; :Returns:
;       LINE:           out, required, type=flaot(3\,`NPOINTS`)
;                       Points along the line connecting `X0` and `X1`.
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
;       2014/10/15  -   Written by Matthew Argall
;-
function MrLine3D, r0, r1, $
NPOINTS=npoints
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    if n_elements(nPoints) eq 0 then nPoints = 100
    
    ;
    ;Knowing two vectors that point from the origin to two points in space
    ;   r0 = (x0, y0, z0)
    ;   r1 = (x1, y1, z1)
    ;
    ;A vector, v, parallel to the vector a, which connects r0 to r1, can be formed
    ;   v = r1 - r0
    ;     = (x1 - x0, y1 - y0, z1 - z0)
    ;     = (a, b, c)
    ;
    ;A vector from the origin to any other point on the line  can be formed by adding some
    ;multiplicative factor, t, of v to r0
    ;   r = r0 + vt
    ;
    ;It follows that
    ;   x = x0 + at
    ;   y = y0 + bt
    ;   z = z0 + ct
    ;
    ;Or
    ;   t = (x - x0) / a
    ;     = (y - y0) / b
    ;     = (z - z0) / c
    ;
    ;Given two points, then vectors r0 and v are known. Any other point can be found
    ;by choosing x and computing y and z:
    ;   x = known
    ;   v = (a, b, c) = known
    ;   t = (x - x0) / a
    ;   y = y0 + b*t
    ;   z = z0 + c*t
    ;
    
    ;Create v
    v = r1 - r0

    ;One of v=(a,b,c) must not be zero.
    case 1 of
        ;dx > 0
        v[0] ne 0: begin
            ;Compute x and t
            x = r0[0] + (r1[0] - r0[0]) * findgen(nPoints)/(nPoints-1)
            t = (x - r0[0]) / v[0]
    
            ;Compute y and z
            y = r0[1] + v[1]*t
            z = r0[2] + v[2]*t
        endcase
        
        ;dy > 0
        v[1] ne 0: begin
            ;Compute y and t
            y = r0[1] + (r1[1] - r0[1]) * findgen(nPoints)/(nPoints-1)
            t = (x - r0[1]) / v[1]
    
            ;Compute x and z
            x = r0[0] + v[0]*t
            z = r0[2] + v[2]*t
        endcase
        
        ;dz > 0
        v[2] ne 0: begin
            ;Compute z and t
            z = r0[2] + (r1[2] - r0[2]) * findgen(nPoints)/(nPoints-1)
            t = (z - r0[2]) / v[2]
    
            ;Compute y and z
            x = r0[0] + v[0]*t
            y = r0[1] + v[1]*t
        endcase
        
        ;Line of zero length
        else: begin
            nPoints = 1
            x = r0[0]
            y = r0[1]
            z = r0[2]
        endcase
    endcase
    
    ;Form the line
    line = fltarr(3, nPoints)
    line[0,*] = temporary(x)
    line[1,*] = temporary(y)
    line[2,*] = temporary(z)
    
    return, line
end




;-------------------------------------------------------
; Main Level Program (.r MrLine3D) /////////////////////
;-------------------------------------------------------
;Define two points
r0 = [0,0,0]
r1 = [10,10,10]

;Create a line with 11 points
line = MrLine3D(r0, r1, NPOINTS=11)
print, line
end