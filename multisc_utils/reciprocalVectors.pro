;+
; NAME:
;       RECIPROCALVECTORS
;
; PURPOSE:
;
;       Calculate the reciprocal vectors for a 4-spacecraft tetrahedron configuration.
;       See references for details. Positions are assumed to be given in kilometers.
;
; AUTHOR:
;
;       Matthew Argall
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 Collge Rd.
;		Durham, NH, 03824
;       mry27@wildcats.unh.edu
;
; CATEGORY:
;
;       Data Analysis
;
; CALLING SEQUENCE:
;
;       recipVecT = reciprocalVectors(positions, fields)
;
; INPUT POSITIONAL PARAMETERS
;
;       POSITIONS   -   In. Required. Type=ptrarr(4) <- fltarr(3,*).
;                       3-component position (km) vectors for each of the 4-spacecraft
;                           that make up the tetrahedron.
;
; RETURN VALUE:
;
;       RECIPVECT   -   Reciprocal vectors (meters^-1)
;
; REFERENCES
;
;   Paschmann, G, Daly, P.W., Analysis Methods for Multi-Spacecraft Data, ISSI Scientific
;       Report, 1998 (Chp 14.2)
;       http://geo.phys.spbu.ru/~runov/Cluster/analysis_methods_1_1.pdf
;
function reciprocalVectors, positions
    compile_opt idl2
    
    ;conversion factor from kilometers to meters
    km_to_m = 1e3
    
    ;count the number of spacecraft and the number of data points per component
    n_sc = n_elements(positions)
    n_pts = n_elements((*positions[0])[0,*])
    
    ;return the reciprocal vectors as a pointer array
    recipVect = ptrarr(n_sc)
    
    ;initialize some indices
    i = 1
    j = 2
    k = 3
    for m = 0, n_sc - 1 do begin
        ;calculate the separation between spacecraft
        ;r_ij = r_j - r_i   --  the position vector pointing from spacecraft i to
        ;                       spacecraft j
        r_ij = (*positions[j] - *positions[i]) * km_to_m
        r_ik = (*positions[k] - *positions[i]) * km_to_m
        r_im = (*positions[m] - *positions[i]) * km_to_m
    
        ;the reciprocal vector for vertex m of the tetrahedron points normal to the area
        ;of the face of the tetrahedron opposite to vertex m. The normal vector to this
        ;surface can be found by taking the cross product between to vectors that lie in
        ;the plane of the surface.
        area = cross_product(r_ij, r_ik)
        
        ;calculate the volume of the tetrahedron
        volume = dot_product(r_im, cross_product(r_ij, r_ik))
        
        ;the reciprical vector is the area's normal vector normalized to the tetrahedron
        ;volume
        recipVect[m] = ptr_new([area[0,*] / volume, area[1,*] / volume, area[2,*] / volume])
        
        ;increase the indices cyclically
        i += 1
        j += 1
        k += 1
        
        ;cyclically means that they start back at 0 after reaching the last one.
        if i eq n_sc then i = 0
        if j eq n_sc then j = 0
        if k eq n_sc then k = 0
    endfor
    
    return, recipVect
end