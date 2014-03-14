; docformat = 'rst'
;
; NAME:
;   RBSP_MGSE_TO_GSE
;
;+
;   The purpose of this program is to detrend a vector field and transform it to a new
;   coordinate system. Basically, it is a combination of steps that are typically performed
;   on magnetic field data before plotting its power spectra.
;
; :Categories:
;   Van Allen Probes, Rotations
;
; :Params:
;       FIELD:              in, required, type=3xN or Nx3 numeric
;                           The 3-component field to be detrended and/or rotated.
;       NDETREND:           in, optional, type=integer, default=0
;                           When detrending, the background field is subtracted from
;                               `FIELD`. Set this keyword equal to a positive integer to
;                               indicate the number of points that should be averaged when
;                               computing the mean, background field.
;       NSYSTEM:            in, optional, type=integer
;                           If set to a positive integer, `FIELD` will be rotated into a
;                               field-aligned coordinate system (FAS) in which the z-axis
;                               is is along mean background field. `NSYSTEM` is the number
;                               of points used to determine the mean background field. If
;                               `NDETREND` is also set, NSYSTEM will be set equal to
;                               `NDETREND`. The x-axis of the FAS is formed by crossing
;                               z_FAS with [0,1,0], the y-axis of the original system.
;                               y_FAS completes the right-handed system.
;
; :Keywords:
;       DIMENSION:          in, optional, type=int, default=2
;                           The dimension along which to detrend the data.
;       MEAN_FIELD:         out, optional, type=fltarr
;                           The mean field subtracted from `FIELD` when `NDETREND`>0.
;       POSITION:           in, optional, type=3xN numeric
;                           The 3-component position of the spacecraft position at each
;                               point in `FIELD`. If provided, `NSYSTEM` will determine
;                               how many points to average when rotating into a field-
;                               aligned radial (FAR) coordinate system. In this system,
;                               z-hat is again along the background field, the azimuthal
;                               direction is formed by crossing the position vector with
;                               z-hat, and the radial component completes the system.
;       RMATRIX:            in, out, optional, type=3x3 numeric
;                           If provided, `FIELD` will be transformed into a new coordinate
;                               system using this rotation matrix. If not provided, and
;                               either `FAR` or `FAS` are non-zero, then the transformation
;                               matrix to that system will be returned.
;
; :Returns:
;       FIELD_OUT:          The result of dentrending and rotating `FIELD`.
;
; :Uses:
;   Uses the following external programs::
;       detrend_data.pro
;       fa_system.pro
;       far_system.pro
;       rotate_vector.pro
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
;       09/19/2013  -   Written by Matthew Argall
;       09/23/2013  -   Input and output RMATRIX were not separated, so data was being
;                           rotated twice. Fixed. - MRA
;       10/04/2013  -   Added the MEAN_FIELD keyword.
;-
function MrDetrendRotate, field, nDetrend, nSystem, $
DIMENSION = dimension, $
MEAN_FIELD = mean_field, $
POSITION = position, $
RMATRIX = rMatrix
    compile_opt idl2
    on_error, 2
    
    ;Detrend and Rotate?
    if n_elements(nDetrend)  eq 0 then nDetrend  = 0
    if n_elements(dimension) eq 0 then dimension = 2
    if n_elements(nSystem)   eq 0 then nSystem   = 0
    if (nSystem ne 0) and (nDetrend ne 0) then nSystem = nDetrend
    
    ;If NSYSTEM was given
    fas = 0
    far = 0
    if (nSystem gt 0) then begin
        ;If POSITION was given, transform to the FAR system. Otherwise, the FAS system.
        if n_elements(position) eq 0 $
            then fas = 1 $
            else far = 1
    endif
    
    ;Make sure only one system is provided.
    if (nSystem gt 0) and n_elements(rMatrix) ne 0 then $
        message, 'NSYSTEM and RMATRIX are mutually exclusive.'
    
    ;Create an editable copy of the field
    field_out = field

;-----------------------------------------------------
;Detrend the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if nDetrend ne 0 then $
        field_out = detrend_data(field_out, nDetrend, $
                                 DIMENSION=dimension, $
                                 BACKGROUND=mean_field)

;-----------------------------------------------------
;Field-Aligned System? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Rotate to field-aligned system?
    if nSystem gt 0 and fas eq 1 then begin

        ;Has the data been detrended already?
        if nDetrend eq 0 $
            then rMatrix_out = fa_system(field_out, nFAS) $
            else rMatrix_out = fa_system(mean_field, /ISMEAN)

        ;Rotate into the FAR frame
        field_out = rotate_vector(rMatrix_out, field_out)
    endif

;-----------------------------------------------------
;Field-Aligned, Radial System? \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Rotate to field-aligned system?
    if nSystem gt 0 and far eq 1 then begin

        ;Has the data been detrended already?
        if nDetrend eq 0 $
            then rMatrix_out = far_system(field_out, position, nFAR) $
            else rMatrix_out = far_system(mean_field, position, /ISMEAN)
            
        ;Rotate into the FAR frame
        field_out = rotate_vector(rMatrix, field_out)
    endif

;-----------------------------------------------------
;Other Coordinate System? \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Rotate into a different coordinate system? Return the rotation matrix?
    if n_elements(rMatrix) ne 0 $
        then field_out = rotate_vector(rMatrix, field_out) $
        else if arg_present(rMatrix) and n_elements(rMatrix_out) gt 0 then rMatrix = rMatrix_out

    return, field_out
end