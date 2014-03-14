; docformat = 'rst'
;
;+
;   The purpose of this program is to calculate the third component of the electric
;   field assuming \vec{E} \cdot \vec{B} = 0::
;       E_{z} = -\frac{( E_{x} B_{x} + E_{y} B_{y})} {B_{z}}
;
;   NOTE:
;       Do not assign two output parameters to variables with the same name. This will
;       cause unexpected results.
;
; :Categories:
;   RBSP, Magnetometer, Spectrogram
;
; :Params:
;       E:                  in, required, type=2xN float
;                           The two-component electric field
;       B:                  in, required, type=3xN float
;                           The three-component magnetic field
;       T_E:                in, optional, type=fltarr
;                           Time stamps coorresponding to the data in `E`. If provided
;                               with `T_B`, the magnetic and electric fields will be
;                               interpolated to the time stamps of the data product with
;                               the slower sampling rate.
;       T_B:                in, optional, type=fltarr
;                           Time stamps coorresponding to the data in `B`. If provided
;                               with `T_E`, the magnetic and electric fields will be
;                               interpolated to the time stamps of the data product with
;                               the slower sampling rate.
;       T_OUT:              out, optional, type=fltarr
;                           Time stamps of the interpolated fields. Only calculated if
;                               `T_E` and `T_B` are given.
;       E_OUT:              out, optional, type=3xN float
;                           The interpolated electric field. Only calculated if `T_E` and
;                               `T_B` are given. If `COMBINE` is set, E_OUT will not be
;                               returned.
;       B_OUT:              out, optional, type=3xN float
;                           The interpolated magnetic field. Only calculated if `T_E` and
;                               `T_B` are given.
;       DT:                 out, optional, type=float
;                           Sampling period of the interpolated fields.
;                           
;
; :Keywords:
;       B_MIN:              in, optional, type=float
;                           If this is specified, any value of Bz < min_Bz will cause
;                               !values.f_nan to be used as the result.
;       COMBINE:            in, optional, type=boolean, default=0
;                           If set, the missing component of the electric field will be
;                               concatenated into `E` before being returned.
;       TMATRIX:            in, optional, type=3x3 float
;                           A matrix that transforms B into the same frame as E. Typically,
;                               this is from an inertial system into the spacecraft frame.
;                               If provided and `COMBINE` is set, the 3D electric field
;                               will be transformed into the `B`'s original frame.
;       X:                  in, optional, type=boolean, default=0
;                           If set, the X-component of `E` will be computed. In this case
;                               E[0,*] = Ey, E[1,*] = Ez.
;       Y:                  in, optional, type=boolean, default=0
;                           If set, the Y-component of `E` will be computed. In this case
;                               E[0,*] = Ex, E[1,*] = Ez.
;       Z:                  in, optional, type=boolean, default=0
;                           If set, the Z-component of `E` will be computed. In this case
;                               E[0,*] = Ex, E[1,*] = Ey. If none of `X`, `Y`, or `Z` are
;                               set, then we will assume you want the z-component.
;
; :Returns:
;       E:                  The missing component of the electric field. If `COMBINE` is
;                               set then all three components will be returned in a 3xN
;                               array.
;
; :Uses:
;   Uses the following external programs::
;       dot_product.pro
;       divide_vec.pro
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
; :History:
;   Modification History::
;       03/18/2013  -   Written by Matthew Argall
;       09/20/2013  -   Added the X, Y, and Z keywords. - MRA
;       10/04/2013  -   Added T_E, T_B, E_OUT, B_OUT, and DT parameters. Added TMATRIX
;                           and COMBINE keywords. - MRA
;       2013/10/28  -   E_OUT is no longer returned if COMBINE is set. - MRA
;-
function EdotB_zero, E, B, t_E, t_B, E_out, B_out, t_out, dt, $
B_MIN = B_min, $
COMBINE = combine, $
TMATRIX=TMatrix, $
X=x, $
Y=y, $
Z=z
    compile_opt strictarr
    on_error, 2

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    E_type = size(E, /TYPE)
    E_i = make_array(1, n_elements(E[0,*]), TYPE=E_type)
    
    if (E_type eq 5) or (E_type eq 9) $
        then fill = !values.d_nan $
        else fill = !values.f_nan
    
    combine = keyword_set(combine)
    x = keyword_set(x)
    y = keyword_set(y)
    z = keyword_set(z)
    if (x + y + z eq 0) then z = 1
    if (x + y + z ne 1) then message, 'On and only one of X, Y, Z must be set.'

;-----------------------------------------------------
;Interpolate the Fields? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if n_elements(t_E) gt 0 and n_elements(t_B) gt 0 then begin
        ;Rotate the magnetic field into the SCS frame
        if n_elements(TMatrix) gt 0 $
            then B_temp = rotate_vector(TMatrix, B) $
            else B_temp = B

        ;Interpolate B and E
        MrInterp_TS, temporary(B_temp), E, t_B, t_E, B_out, E_out, t_out, DT_OUT=dt
        
    endif else begin
        B_out = B
        E_out = E
    endelse

;-----------------------------------------------------
;Compute the Third Component \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;If there are no restrictions on B, then compute E_i
    if n_elements(B_min) eq 0 then begin
        ;make E_z a column vector before dividing
        case 1 of 
            x: E_i = -transpose(dot_product(E_out, B_out[ 1:2, *])) / B_out[0,*]
            y: E_i = -transpose(dot_product(E_out, B_out[[0,2],*])) / B_out[1,*]
            z: E_i = -transpose(dot_product(E_out, B_out[ 0:1, *])) / B_out[2,*]
        endcase
    
    ;Otherwise, 
    endif else begin
        ;Find where Bz is too small
        iReplace = where(abs(B[2,*]) lt B_min, nreplace, N_COMPLEMENT=nkeep, COMPLEMENT=ikeep)
        
        ;Make those values NaNs and keep the rest
        if nreplace ne 0 then E_i[0, iReplace] = fill
        if nkeep ne 0 then begin
            case 1 of
                x: begin
                    iReplace = where(abs(B_out[0,*]) lt B_min, nreplace, $
                                     N_COMPLEMENT=nkeep, COMPLEMENT=iKeep)
                    if nKeep   gt 0 then E_i[iKeep] = -transpose(dot_product(E_out, B_out[ 1:2, iKeep])) / B_out[0,iKeep]
                    if nRemove gt 0 then E_i[iRemove] = fill
                endcase
                
                y: begin
                    iReplace = where(abs(B_out[1,*]) lt B_min, nreplace, $
                                     N_COMPLEMENT=nkeep, COMPLEMENT=iKeep)
                    if nKeep   gt 0 then E_i[iKeep] = -transpose(dot_product(E_out, B_out[[0,2],*])) / B_out[1,*]
                    if nRemove gt 0 then E_i[iRemove] = fill
                endcase
                
                z: begin
                    iReplace = where(abs(B_out[2,*]) lt B_min, nreplace, $
                                     N_COMPLEMENT=nkeep, COMPLEMENT=iKeep)
                    if nKeep   gt 0 then E_i[iKeep] = -transpose(dot_product(E_out, B_out[ 0:1, *])) / B_out[2,*]
                    if nRemove gt 0 then E_i[iRemove] = fill
                endcase
            endcase
        endif
    endelse

;-----------------------------------------------------
;Combind the components \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if (combine eq 1) then begin
        case 1 of
            x: E_i = [E_i, E_out]
            y: E_i = [E_out[0,*], E_i, E_out[1,*]]
            z: E_i = [E_out, E_i]
        endcase
        E_out = 0
        void = temporary(E_out)
        
        ;Transform the 3D field into the refrence frame of B
        if n_elements(TMatrix) ne 0 then E_i = rotate_vector(transpose(TMatrix), E_i)
    endif
    
    return, E_i
end