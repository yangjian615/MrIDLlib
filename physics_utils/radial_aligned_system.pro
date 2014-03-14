; docformat = 'rst'
;
; NAME:
;
;       RADIAL_ALIGNED_SYSTEM
;
; PURPOSE:
;+
;       The purpose of this program is to create a coordinate system with one axis along
;       the radial component pointing from the center of the earth to the spacecraft,
;       the azimuthal component being (theta = z x r), where "z" is [0,0,1] in the system
;       of the spacecraft, and the polar component completes the system, phi = r x theta.
;
;       The 
;
; :Categories:
;
;       Physics Utility, Coordinate Systems
;
; :Params:
;
;       POSITION:           in, required, type=3xN numeric
;                           The position of the spacecraft or other body for which to 
;                               form the coordinate system.
;                           
; :Returns:
;
;       RA_SYSTEM:          The field-aligned coordinate system, the rows of which are
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
;       04/11/2013  -   Written by Matthew Argall
;-
function radial_aligned_system, position
    compile_opt idl2
 
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    pos_sz = size(position, /STRUCTURE)
    if pos_sz.dimensions[0] ne 3 then $
        message, 'POSITION must be a 3xN vector array.'
 
;-----------------------------------------------------
;Calculate the Othogonal Axes \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;calculate the radial component
    ;   r = (x, y, z) / sqrt(x^2 + y^2 + z^2)
    r_hat = normalize(position)
    
    ;Calculate the azimuthal component
    ;   theta = z x r
    theta_hat = normalize(cross_product([0,0,1], r_hat))

    ;Calculate the polar component
    ;   phi = r x theta
    phi_hat = normalize(cross_product(r_hat, theta_hat))
    
;-----------------------------------------------------
;Create the Coordinate Matrix \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;allocate memory to the output array
    ra_system = make_array([3, pos_sz.dimensions], TYPE=pos_sz.type)
    
    ;Fill the first row with r_hat, the second row with theta_hat, and the third row
    ;with phi_hat.
    ra_system[*,0,*] = temporary(r_hat)
    ra_system[*,1,*] = temporary(theta_hat)
    ra_system[*,2,*] = temporary(phi_hat)
    
    return, ra_system
end