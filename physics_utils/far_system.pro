; docformat = 'rst'
;
; NAME:
;
;       FAR_SYSTEM
;
; PURPOSE:
;+
;       The purpose of this program is to create a coordinate system with the z-axis
;       directed along the background magnetic field. The y-axis is found by crossing
;       the radial vector from the center of the earth to the spacecraft's location into
;       the mean field direction and negating it. In this manner, y points roughly
;       duskward on account of z pointing southward. The third component is found by
;       taking the cross product of the first two.
;
; :Categories:
;
;       Physics Utility, Coordinate Systems
;
; :Params:
;       B_FIELD:            in, required, type=3xN numeric
;                           The three components of the magnetic field measured at each
;                               `POSITION`.
;       POSITION:           in, required, type=3xN numeric
;                           The three components of the position vector, measured from
;                               the center of the earth to the spacecraft, at each 
;                               measurement of `B_FIELD`.
;       NAVG:               in, optional, type=int/float
;                           If `B_FIELD` is not the average magnetic field (as indicated
;                               by the `B_AVG` keyword), then the background field will
;                               be calculated using a boxcar average of NAVG points.
;
; :Keywords:
;       ISMEAN:             in, optional, type=boolean, default=0
;                           If set, then `B_FIELD` is actually the mean, background
;                               magnetic field of each component.
;       B_AVG:              out, optional, type=3xN numeric
;                           If `ISMEAN`=0, then this is a named variable into which the
;                               mean, background magnetic field of each component will be
;                               calculated and returned.
;                               
;                           
; :Returns:
;       FAR_SYS:            The field-aligned coordinate system, the rows of which are
;                           radial, azimuthal, polar... or r, theta, phi.
;
; :Uses:
;   Uses the following external programs::
;       divide_vec.pro
;       normalize.pro
;       cross_product.pro
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
;       Matthew Argall 2012
;
; :History::
;   Modification History::
;       04/25/2013  -   Written by Matthew Argall
;       06/13/2013  -   Renamed to FAR_SYSTEM.PRO. - MRA
;-
function far_system, b_field, position, navg, $
B_AVG = b_avg, $
ISMEAN = isMean
    compile_opt idl2

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    bdims = size(b_field, /DIMENSIONS)
        
    r_hat = fltarr(bdims)
    x_hat = fltarr(bdims)
    y_hat = fltarr(bdims)
    z_hat = fltarr(bdims)
    far_sys = fltarr([3, bdims])

;-----------------------------------------------------
;Z-HAT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Calculate the average magnetic field.
    if n_elements(isMean) eq 0 then begin
        b_avg = fltarr(bdims)
    
        b_avg[0,*] = smooth(b_field[0,*], navg, /EDGE_TRUNCATE)
        b_avg[1,*] = smooth(b_field[1,*], navg, /EDGE_TRUNCATE)
        b_avg[2,*] = smooth(b_field[2,*], navg, /EDGE_TRUNCATE)
        
    endif else b_avg = b_field

    ;The z-axis will be along the average field direction
    z_hat = divide_vec(b_avg, magnitude_vec(b_avg))

;-----------------------------------------------------
;R-HAT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ; r = (x, y, z) / sqrt(x^2 + y^2 + z^2)
    r_hat = normalize(position)
 
;-----------------------------------------------------
;Y-HAT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Calculate the (quasi-) azimuthal/duskward component
    ;   y = -(z x r)
    y_hat = normalize(cross_product(z_hat, r_hat))
 
;-----------------------------------------------------
;X-HAT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Calculate the (quasi-) radial/sunward component
    ;   x = y x z
    x_hat = normalize(cross_product(y_hat, z_hat))
    
;-----------------------------------------------------
;Create the Coordinate Matrix \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Fill the first row with x_hat, the second row with y_hat, and the third row
    ;with z_hat.
    far_sys[*,0,*] = temporary(x_hat)
    far_sys[*,1,*] = temporary(y_hat)
    far_sys[*,2,*] = temporary(z_hat)
    
    return, far_sys
end