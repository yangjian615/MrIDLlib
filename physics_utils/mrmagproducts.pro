; docformat = 'rst'
;
; NAME:
;       PlotMag
;
;*****************************************************************************************
;   Copyright (c) 2013, University of New Hampshire                                      ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   The purpose of this program is to plots of various magnetic field quantities. Various
;   operations can be performed on this data before 
;
;   Quantities::
;       - Vector combined (Bx, By, Bz)
;       - Magnitude: |B|
;       - Position combined (X, Y, Z)
;       - Magnetic field components: Bx, By, Bz
;       - Clock Angle: atan(By/Bz) * !radeg
;       - Power Spectral Density: Px, Py, Pz
;       - Polarization parameters: polarization, ellipticity, k-dot-B angle, coherency, intensity
;       - Poynting spectra: Sx, Sy, Sz
;
;   Operations::
;       - Rotated into a new reference frame
;       - Calculate E dot B = 0
;       - Subtract mean field (detrend)
;       - Take an n-th order difference
;       - Rotate into a field-aligned frame
;       
;   Order of Operations::
;       1. Compute third component of E, if necessary.
;       2. Rotate using TMatrix (sticky)
;       2. Plot "natural" data products
;               - |B|
;               - B_vec
;               - Position
;       3. More times series products
;               - Clock Angle
;               - Bx
;               - By
;               - Bz
;       4. Poynting Spectra
;               * Interpolate B and E
;               * Rotate B from GSE to MGSE
;               * E dot B = 0 --> Obtain Ex
;               * Detrend and/or Rotate into a field-aligned system.
;       5. Detrend and/or Rotate into a field-aligned system
;       6. Order N Difference
;       7. Detrended and/or field-aligned products
;               - dB_vec
;               - dBx
;               - dBy
;               - dBz
;       8. Power spectral density
;               - pwr_x
;               - pwr_y
;               - pwr_z
;       9. Polarization Parameters
;               - Polarization
;               - Ellipticity
;               - Intensity
;               - k-dot-B Angle
;               - Coherency
;
; :Categories:
;       Physics Utilties
;
; :Params:
;       B:                  in, optional, type=3xN or Nx3 numeric
;                           Three components of the magnetic field (Bx, By, Bz).
;       T_SSM:              in, required, type=fltarr
;                           Time stamps in seconds since midnight (ssm) of `B`, `B_MAG`
;                               and `POS`.
;       BMAG:               in, optional, type=numeric array
;                           Mangitude of the magnetic field.
;       POS:                in, optional, type=3xN or Nx3 numeric
;                           Position of the spacecraft. This is a required parameter if
;                               `NFAR` > 0.
;       E:                  in, optional, type=2xN\, 3xN\, Nx2\, or Nx3 numeric array.
;                           Components of the electric field (Ex, Ey, or Ez). If two
;                               components are provided, it will be assumed that Z follows
;                               Y follows Z. It will also be assumed that the field is in
;                               the satellite's inertial frame (SCS). In this case, `B`
;                               must be rotated into SCS and `T_INTERT_TO_SCS` must be
;                               provided. `E` is only used if `POYNTING_TS`, `POYNTING_X`
;                               `POYNTING_Y` or , `POYNTING_Z` is set, in which case it is
;                               a required parameter.
;       TE_SSM:             in, optional, type=fltarr
;                           Time stamps in seconds since midnight (ssm) of `E`.
;
; :Keywords:
;       COORD_SYSTEM:       in, optional, type=strarr(3), default=['x', 'y', 'z']
;                           A 3-element array specifying the names of the coordinates of
;                               the system in which the magnetic field and spacecraft
;                               position reside. Handy if `TMATRIX` is given.
;       DIRECTORY:          in, optional, type=string, default=pwd()
;                           The directory in which to search for FGM data.
;       DIMENSION:          in, optional, type=boolean, default=largest dimension of `B`
;                           The dimension over which plots will be made. If `B` is a
;                           3xN array and DIMENSION=2, then B[0,*], B[1,*] and
;                               B[2,*] will be the bases for plots. The same rule
;                               applies to `POS` and `E`. This keyword also
;                               affects `NDETREND`, `NFAR`, `NFAS`, `NDIFF`.
;       TMATRIX:            in, optional, type=fltarr(3\,3)
;                           A coordinate transformation matrix for rotating into a new
;                               inertial frame. If provided, data will be rotated into
;                               this frame first. All other results will be based in
;                               the new coordinate system.
;       T_INERT_TO_SCS:     in, optional, type=boolean
;                           A coordinate transformation matrix from the inertial frame
;                               in which `B` resides to the inertial spacecraft system
;                               (SCS), in which the 2-component `E` resides. This must
;                               be provided if `E` has only two components and the
;                               poynting spectra are being plotted.
;       OREF:               in, optional, type=boolean, default=0
;                           If set, then all data product keywords must be named variables
;                               into which their plot's object reference will be returned.
;                               In this case, the plots will not be put into a MrWindow
;                               widget and `MrWin` will be an invalid object reference.
;                               If a particular data product is a vector quantity, a 2-
;                               element object array will be returned with the plot's
;                               object reference and a legend describing it. The same is
;                               true for image data products, but a colorbar instead of
;                               a legend.
;
;       COMPUTE_EX:         in, optional, type=boolean, default=0
;                           If set, then the X-Component of `E` will be computed. This
;                               is only necessary if `E` has only two-components. The
;                               third component of the electric field is computed assuming
;                               (E dot B = 0). `COMPUTE_EZ` is assumed.
;       COMPUTE_EY:         in, optional, type=boolean, default=0
;                           If set, then the Y-Component of `E` will be computed. This
;                               is only necessary if `E` has only two-components. The
;                               third component of the electric field is computed assuming
;                               (E dot B = 0). `COMPUTE_EZ` is assumed.
;       COMPUTE_EZ:         in, optional, type=boolean, default=0
;                           If set, then the Z-Component of `E` will be computed. This
;                               is only necessary if `E` has only two-components. The
;                               third component of the electric field is computed assuming
;                               (E dot B = 0). `COMPUTE_EZ` is assumed.
;       NDETREND:           in, optional, type=integer, default=0
;                           Number of points by which `B` (and `E`) will be
;                               detrended. A sliding box-car average NDETREND points wide
;                               will be slid over each component, as determined by the
;                               `DIMENSION` keyword. This average will then be subtracted
;                               from the component.
;       NDIFF:              in, optional, type=integer, default=0
;                           A positive or negative integer specifying the order of an
;                               N-th order difference to be performed on each component
;                               of `B` (and `E`), as determined by `DIMENSION`.
;       NFAS:               in, optional, type=in, default=0
;                           If non-zero, data will be rotated into a field-aligned
;                               coordinate system (FAS). NFAS number of points will be
;                               used to determine the mean, background field. If `NDETREND`
;                               is also set, then NFAS will be set equal to NDETREND. The
;                               Z'-axis then points along the background field, X is the
;                               cross product of Z' and [0,1,0]. Y' completes the system.
;                               [0,1,0] is the y-axis in the coordinate system of `B`.
;       NFAR:               in, optional, type=in, default=0
;                           If non-zero, data will be rotated into a field-aligned, radial
;                               coordinate system (FAR). NFAR number of points will be
;                               used to determine the mean, background field. If `NDETREND`
;                               is also set, then NFAR will be set equal to NDETREND. The
;                               Z'-axis then points along the background field, Y' (aximuth)
;                               is the cross product instantaneous spacecraft `POS`
;                               with Z', and X' (radial) completes the system. Note that
;                               because the magnetic field varies in direction, X' and Y'
;                               are only approximately in the radial and azimuthal
;                               directions, respectively.
;
;       NFFT:               in, optional, type=int
;                           The number of points to use per FFT. Used with the power
;                               spectral density and poynting spectra plots.
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed. Used with the power spectral density and
;                               poynting spectra plots.
;       NSHIFT:             in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT. Used with
;                               the power spectral density and poynting spectra plots.
;       FFTKWDS:            in, optional, type=structure
;                           A structure of keyword-value pairs for any keyeord accepted
;                               by MrFFT.pro.
;
;       B_MAG:              in, optional, type=boolean, default=0
;                           If set, the magnitude of the magnetic field will be plotted.
;       B_VEC:              in, optional, type=boolea, default=0
;                           If set, the 3-component magnetic field will be plotted. All
;                               components will be contained within the same plot axes.
;       BX:                 in, optional, type=boolean, default=0
;                           If set, the X-component of `B` will be plotted.
;       BY:                 in, optional, type=boolean, default=0
;                           If set, the Y-component of `B` will be plotted.
;       BZ:                 in, optional, type=boolean, default=0
;                           If set, the Z-component of `B` will be plotted.
;       DB_VEC:             in, optional, type=boolea, default=0
;                           If set, the 3-component magnetic field will be plotted. All
;                               components will be contained within the same plot axes.
;                               This plot is made after any of the "operations" have been
;                               performed on the data.
;       DBX:                in, optional, type=boolean, default=0
;                           If set, the X-component of `DB_VEC` will be plotted.
;       DBY:                in, optional, type=boolean, default=0
;                           If set, the Y-component of `DB_VEC` will be plotted.
;       DBZ:                in, optional, type=boolean, default=0
;                           If set, the Z-component of `DB_VEC` will be plotted.
;       POS:                in, optional, type=boolean, default=0
;                           If set, the 3-components of the spacecraft location in GSE
;                               coordinates will be plotted. All components will be
;                               displayed within the same set of axes.
;       CLOCK_ANGLE         in, optional, type=boolean, default=0
;                           If set, a plot of the magnetic field clock angle will be made.
;                               The clock angle is calculated as C.A. = atan(By/Bz)*!radeg
;       PWR_X:              in, optional, type=boolean, default=0
;                           If set, the power spectral density of `BX` will be plotted.
;       PWR_Y:              in, optional, type=boolean, default=0
;                           If set, the power spectral density of `BY` will be plotted.
;       PWR_Z:              in, optional, type=boolean, default=0
;                           If set, the power spectral density of `BZ` will be plotted.
;       COHERENCY:          in, optional, type=boolean, default=0
;                           If set, the coherency of the polarized waves will be plotted.
;       ELLIPTICITY:        in, optional, type=boolean, default=0
;                           If set, the ellipticity of the polarized waves will be plotted.
;                               Positive values are right-handed, negative values are
;                               left-handed.
;       INTENSITY:          in, optional, type=boolean, default=0
;                           If set, the intensity of the polarized waves will be plotted.
;       KDOTB_ANGLE:        in, optional, type=boolean, default=0
;                           If set, the angle between the wave's propagation direction and
;                               `B` will be plotted.
;       POLARIZATION:       in, optional, type=boolean, default=0
;                           If set, the percent polarization of the polarized waves will
;                               be plotted. 0 represents a completely unpolarized wave,
;                               while 1 represents a purely polarized wave.
;       POYNTING_TS:        in, optional, type=boolean, default=0
;                           If set, a time series of the poyting vector will be plotted.
;                               In this case, `E` must be provided.
;       POYNTING_X:         in, optional, type=boolean, default=0
;                           If set, the X-component of the poynting flux spectal density
;                               will be plotted. In this case, `E` must be provided.
;       POYNTING_Y:         in, optional, type=boolean, default=0
;                           If set, the Y-component of the poynting flux spectal density
;                               will be plotted. In this case, `E` must be provided.
;       POYNTING_Z:         in, optional, type=boolean, default=0
;                           If set, the Z-component of the poynting flux spectal density
;                               will be plotted. In this case, `E` must be provided.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrPlot__Define or MrImage__Define,
;                               depending on the type of plot being made. Note that these
;                               keywords will be applied to each plot being created.
;
; :Returns:
;       FGM_WIN:            out, required, type=object
;                           Object reference to the MrWindow object containing each of the
;                               requested plots. If `OREF` is set, then this is an invalid
;                               object and the individual plot references are returned
;                               through their respective keywords.
;
; :Uses:
;   Uses the following external programs::
;       setDefaultValue (Coyote_Graphics)
;       error_message (Coyote_Graphics)
;       rotate_vector
;       mrwindow
;       cdf_read
;       cluster_find_file
;       dissectdatetime
;       pwd
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       10/02/2013  -   Written by Matthew Argall
;       10/04/2013  -   Added the COORD_SYSTEM keyword. - MRA.
;-
function MrMagProducts, t_ssm, B, Bmag, pos, E, tE_ssm, $
COORD_SYSTEM=coord_system, $
DIRECTORY = directory, $
DIMENSION = dimension, $
TMATRIX = TMatrix, $
T_INERT_TO_SCS = T_INERT_to_SCS, $
OREF = oRef, $
;Operations
COMPUTE_EX = compute_Ex, $
COMPUTE_EY = compute_Ey, $
COMPUTE_EZ = compute_Ez, $
NDETREND = nDetrend, $
NDIFF = nDiff, $
NFAS = nFAS, $
NFAR = nFAR, $
;FFT
NFFT = nfft, $
NSHIFT = nshift, $
DT = dt, $
FFTKWDS = fftkwds, $
;Data Products
B_MAG = B_Mag, $
B_VEC = B_vec, $
BX = Bx, $
BY = By, $
BZ = Bz, $
DB_VEC = dB_vec, $
DBX = dBx, $
DBY = dBy, $
DBZ = dBz, $
CLOCK_ANGLE=clock_angle, $
POSITION = position, $
;Power Spectral Density
PWR_X = pwr_x, $
PWR_Y = pwr_y, $
PWR_Z = pwr_z, $
;Polarization Parameters
COHERENCY = coherency, $
ELLIPTICITY = ellipticity, $
INTENSITY = intensity, $
KDOTB_ANGLE = kdotb_angle, $
POLARIZATION = polarization, $
;Poynting Flux
POYNTING_TS = poynting_ts, $    ;Requires electric field
POYNTING_X = poynting_x, $      ;Requires electric field
POYNTING_Y = poynting_y, $      ;Requires electric field
POYNTING_Z = poynting_z, $      ;Requires electric field
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        
        ;Reset the color table
        if n_elements(rr) ne 0   then tvlct, rr, gg, bb

        ;Vectors (also have legends)
        if max(obj_valid(bvec_plot))  eq 1 then obj_destroy, bvec_plot
        if max(obj_valid(pos_plot))   eq 1 then obj_destroy, pos_plot
        if max(obj_valid(dBvec_plot)) eq 1 then obj_destroy, dBvec_plot
        
        ;Scalars
        if obj_valid(bmag_plot) then obj_destroy, bmag_plot
        if obj_valid(ca_plot)   then obj_destroy, ca_plot
        if obj_valid(Bx_plot)   then obj_destroy, Bx_plot
        if obj_valid(By_plot)   then obj_destroy, By_plot
        if obj_valid(Bz_plot)   then obj_destroy, Bz_plot
        if obj_valid(dBx_plot)  then obj_destroy, dBx_plot
        if obj_valid(dBy_plot)  then obj_destroy, dBy_plot
        if obj_valid(dBz_plot)  then obj_destroy, dBz_plot
        
        ;Images (also have colorbars)
        if max(obj_valid(coh_image))   eq 1 then obj_destroy, coh_image
        if max(obj_valid(ellip_image)) eq 1 then obj_destroy, ellip_image
        if max(obj_valid(inten_image)) eq 1 then obj_destroy, inten_image
        if max(obj_valid(kdb_image))   eq 1 then obj_destroy, kdb_image
        if max(obj_valid(Px_image))    eq 1 then obj_destroy, Px_image
        if max(obj_valid(Py_image))    eq 1 then obj_destroy, Py_image
        if max(obj_valid(Pz_image))    eq 1 then obj_destroy, Pz_image
        if max(obj_valid(Pol_image))   eq 1 then obj_destroy, pzation_image
        if max(obj_valid(Sx_image))    eq 1 then obj_destroy, Sx_image
        if max(obj_valid(Sy_image))    eq 1 then obj_destroy, Sy_image
        if max(obj_valid(Sz_image))    eq 1 then obj_destroy, Sz_image
        
        ;Plotting Window
        if obj_valid(MyWin) then obj_destroy, MyWin
        
        return, obj_new()
    endif
        
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    n_B  = n_elements(B)
    n_p  = n_elements(pos)
    n_tE = n_elements(tE_ssm)
    n_E  = n_elements(E)

    ;Get the current color table
    tvlct, rr, gg, bb, /GET
    
    ;Defaults
    setDefaultvalue, directory, pwd()
    setDefaultvalue, oRef, 0, /BOOLEAN
    add = ~oRef
    
    ;Detrend, Rotate, Difference?
    if n_elements(nDetrend) eq 0 then nDetrend = 0
    if n_elements(nDiff)    eq 0 then nDiff = 0
    if n_elements(nFAS)     eq 0 then nFAS = 0
    if n_elements(nFAR)     eq 0 then nFAR = 0
    if nFAS gt 0 and nFAR gt 0 then message, 'Only one of NFAS and NFAR may be non-zero.'
    if nDetrend gt 0 and nFAS gt 0 then nFAS = nDetrend
    if nDetrend gt 0 and nFAR gt 0 then nFAR = nDetrend
    
    if n_elements(coord_system) eq 0 then coords = ['X', 'Y', 'Z'] else coords = coord_system
    Compute_Ex = keyword_set(Compute_Ex)
    Compute_Ey = keyword_set(Compute_Ey)
    Compute_Ez = keyword_set(Compute_Ez)
    
    ;Dimension
    if n_elements(dimension) eq 0 then begin
        if (n_B gt 0) then begin
            dims = size(B, /DIMENSIONS)
            dimension = where(dims eq min(dims)) + 1
        endif else if (n_p) gt 0 then begin
            dims = size(pos, /DIMENSIONS)
            dimension = where(dims eq min(dims)) + 1
        endif
    endif

    ;Dimension not being plotted
    case dimension of
        0: xdim = 0
        1: xdim = 1
        2: xdim = 0
    endcase
        
;---------------------------------------------------------------------
;Which Data Products? ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Which data product is to be plotted?
    if keyword_set(oRef) then begin
        B_mag        = arg_present(B_mag)
        B_vec        = arg_present(B_vec)
        Bx           = arg_present(Bx)
        By           = arg_present(By)
        Bz           = arg_present(Bz)
        dB_vec       = arg_present(dB_vec)
        dBx          = arg_present(dBx)
        dBy          = arg_present(dBy)
        dBz          = arg_present(dBz)
        position     = arg_present(position)
        clock_angle  = arg_present(clock_angle)
        coherency    = arg_present(coherency)
        ellipticity  = arg_present(ellipticity)
        intensity    = arg_present(intensity)
        kdotb_angle  = arg_present(kdotb_angle)
        polarization = arg_present(polarization)
        pwr_x        = arg_present(pwr_x)
        pwr_y        = arg_present(pwr_y)
        pwr_z        = arg_present(pwr_z)
        poynting_ts  = arg_present(poynting_ts)
        poynting_x   = arg_present(poynting_x)
        poynting_y   = arg_present(poynting_y)
        poynting_z   = arg_present(poynting_z)
    endif else begin
        B_mag        = keyword_set(B_mag)
        B_vec        = keyword_set(B_vec)
        Bx           = keyword_set(Bx)
        By           = keyword_set(By)
        Bz           = keyword_set(Bz)
        dB_vec       = keyword_set(dB_vec)
        dBx          = keyword_set(dBx)
        dBy          = keyword_set(dBy)
        dBz          = keyword_set(dBz)
        position     = keyword_set(position)
        clock_angle  = keyword_set(clock_angle)
        coherency    = keyword_set(coherency)
        ellipticity  = keyword_set(ellipticity)
        intensity    = keyword_set(intensity)
        kdotb_angle  = keyword_set(kdotb_angle)
        polarization = keyword_set(polarization)
        pwr_x        = keyword_set(pwr_x)
        pwr_y        = keyword_set(pwr_y)
        pwr_z        = keyword_set(pwr_z)
        poynting_ts  = keyword_set(poynting_ts)
        poynting_x   = keyword_set(poynting_x)
        poynting_y   = keyword_set(poynting_y)
        poynting_z   = keyword_set(poynting_z)
    endelse
    
    ;Default to plotting B_vec
    if (B_mag + B_vec + position + clock_angle + coherency + ellipticity + intensity + $
        pwr_x + pwr_y + pwr_z + polarization + poynting_x + poynting_y + poynting_z + $
        Bx + By + Bz + dB_vec + dBx + dBy + dBz + kdotb_angle eq 0) $
    then B_vec = 1
        
;---------------------------------------------------------------------
;Check Data //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;T_SSM must always be provided
    if n_elements(t_ssm) eq 0 then message, 'T_SSM must be provided.'
    
    ;|B|
    if (B_mag eq 1) then if n_elements(Bmag) ne 0 then message, 'BMAG must be provided.'
    
    ;B
    if (B_vec + Bx + By + Bz + dB_vec + dBx + dBy + dBz + clock_angle + coherency + $
        ellipticity + intensity + pwr_x + pwr_y + pwr_z + polarization + poynting_x + $
        poynting_y + poynting_z + kdotb_angle) gt 0 $
    then if n_B eq 0 then message, 'B must be provided.'
    
    ;E and TE_SSM
    if (poynting_x + poynting_y + poynting_z) gt 0 then begin
        if n_E  eq 0 then message, 'E must be provided for POYNTING analysis.'
        if n_tE eq 0 then message, 'TE_SSM must be provided for POYNTING analysis.'
    endif
    
    ;If nFAR > 0, then we need the position
    if nFAR gt 0 then if n_p eq 0 then message, 'POS must be provided if NFAR > 0.'
    
    
    ;Create the window
    MyWin = MrWindow(XSIZE=500, YSIZE=700, DISPLAY=~oRef, REALIZE=0)
        
;---------------------------------------------------------------------
;Interpolate E and B /////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;If we are doing a poynting spectra analysis, make sure E has three components/
    if (poynting_x + poynting_y + poynting_x) then begin
        Edims = size(E, /DIMENSIONS)
        
    ;---------------------------------------------------------------------
    ;E has 2 Components: E dot B = 0 /////////////////////////////////////
    ;---------------------------------------------------------------------
        if Edims[xdim] eq 2 then begin
            ;Make sure a rotation matrix was given.
            if n_elements(T_INERT_to_SCS) eq 0 then $
                message, 'E has only 2-Components. Supply T_INERT_to_SCS to calculate third.'
            
            ;Make sure a component was chosen
            if Compute_Ex + Compute_Ey + Compute_Ez eq 0 then begin
                Compute_Ez = 1
                message, 'E has two components. Computing Ez.', /INFORMATIONAL
            endif
            
            ;Make sure only one component is being calculated.
            if Compute_Ex + Compute_Ey + Compute_Ez ne 1 then $
                message, 'One and only one of Compute_Ex, Compute_Ey, Compute_Ez should be set.'
                
            ;Rotate the magnetic field into the SCS frame
            B_scs = rotate_vector(T_INERT_to_SCS, B)
            E_temp = E
    
            ;Interpolate B and E
            MrInterp_TS, B_scs, E, t_ssm, tE_ssm, B_poynt, E_temp, t_poynt_ssm, DT_OUT=dt_poynt
            
            ;Compute the third component assuming E dot B = 0
            Ew = EdotB_zero(E, B_poynt, X=Compute_Ex, Y=Compute_Ey, Z=Compute_Ez)
            
            ;Put the third component in the correct place
            if xdim eq 1 then begin
                case 1 of
                    Compute_Ex: E_poynt = [temporary(Ew), E_temp]
                    Compute_Ey: E_poynt = [E_temp[0,*], temporary(Ew), E_temp[1,*]]
                    Compute_Ez: E_poynt = [E_temp, temporary(Ew)]
                endcase
            endif else begin
                case 2 of
                    Compute_Ex: E_poynt = [[temporary(Ew)], [E_temp]]
                    Compute_Ey: E_poynt = [[E_temp[*,0]], [temporary(Ew)], [E_temp[*,1]]]
                    Compute_Ez: E_poynt = [[E_temp], [temporary(Ew)]]
                endcase
            endelse
            
            ;Free E_temp
            undefine, E_temp
        
    ;---------------------------------------------------------------------
    ;E has 3 Components //////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
    
            ;Interpolate B and E
            MrInterp_TS, B, E, t_ssm, tE_ssm, B_poynt, E_poynt, t_poynt_ssm, DT_OUT=dt_poynt
            
        endelse
        
    ;---------------------------------------------------------------------
    ;Position ////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        
        ;Interpolate the position as well, if nFAR ne 0
        if nFAR gt 0 then MrInterp_TS, pos, E, t_ssm, tE_ssm, pos_poynt
    endif

;---------------------------------------------------------------------
;Rotate Data? ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Rotate from one inertial system to another?
    if n_elements(TMatrix) gt 0 then begin
        if n_B gt 0 then B_temp   = rotate_vector(TMatrix, B)
        if n_p gt 0 then pos_temp = rotate_vector(TMatrix, pos)
        
    endif else begin
        if n_B gt 0 then B_temp   = B
        if n_p gt 0 then pos_temp = pos
    endelse

;---------------------------------------------------------------------
;B_MAG ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if (B_mag eq 1) then begin        
        ;Create the plot.
        Bmag_plot = MyWin -> Plot(t_ssm, Bmag, $
                                  TITLE='Magnitude of the Magnetic Field: |B|', $
                                  XTITLE='UT (HH:MM:SS)', $
                                  XTICKFORMAT='time_labels', $
                                  YTITLE='|B|!C(nT)', $
                                  DRAW=0, $
                                  ADD=add)
        
        ;Set additional keywords
        if n_elements(extra) ne 0 then Bmag_plot -> SetProperty, _EXTRA=extra

        ;Return the object reference?
        if (add eq 0) then B_mag = Bmag_plot
    endif
        
;---------------------------------------------------------------------
;B_VEC ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if (B_vec eq 1) then begin
        Bvec_plot = MyWin -> Plot(t_ssm, B_temp, $
                                  DIMENSION=dimension, $
                                  TITLE = 'Magnetic Field', $
                                  XTITLE='UT (HH:MM:SS)', $
                                  XTICKFORMAT='time_labels', $
                                  YTITLE='B!C(nT)', $
                                  DRAW=0, $
                                  ADD=add)
        
        Bvec_legend = MrWin -> Legend(TITLE='B$\down' + coords + '$', $
                                      GRAPHIC=Bvec_plot, $
                                      LOCATION=8, $
                                      DRAW=0, $
                                      ADD=add)
    
        ;Set additional keywords
        Bvec_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then B_vec = [Bvec_plot, Bvec_legend]
    endif
        
;---------------------------------------------------------------------
;POSITION ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if (position eq 1) then begin
        pos_plot = FGM_oPlot -> Plot(t_ssm, pos_temp, $
                                     DIMENSION=dimension, $
                                     TITLE = 'Spacecraft Position', $
                                     XTITLE='UT (HH:MM:SS)', $
                                     XTICKFORMAT='time_labels', $
                                     YTITLE='Position!C(km)', $
                                     DRAW=0, $
                                     ADD=add)
        
        pos_legend = MyWin -> Legend(TITLE=coords, $
                                     GRAPHIC=pos_plot, $
                                     LOCATION=8, $
                                     DRAW=0, $
                                     ADD=add)
    
        ;Set additional keywords
        pos_plot -> SetProperty, TITLE='Coordinates', _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then position = [pos_plot, pos_legend]
    endif    
    
;---------------------------------------------------------------------
;Bx //////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (Bx eq 1) then begin
        Bx_plot = MyWin -> Plot(t_ssm, B_temp[0,*], $
                                TITLE='Magneic Field ' + coords[0] + '-Component', $
                                XTITLE='UT (HH:MM:SS)', $
                                XTICKFORMAT='time_labels', $
                                YTITLE='B$\down' + coords[0] + '$!C(nT)', $
                                DRAW=0, $
                                ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then Bx_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then Bx = Bx_plot
    endif
    
;---------------------------------------------------------------------
;By //////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (By eq 1) then begin
        By_plot = MyWin -> Plot(t_ssm, B_temp[1,*], $
                                TITLE='Magneic Field ' + coords[1] + '-Component', $
                                XTITLE='UT (HH:MM:SS)', $
                                XTICKFORMAT='time_labels', $
                                YTITLE='B$\down' + coords[1] + '$!C(nT)', $
                                DRAW=0, $
                                ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then By_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then By = By_plot
    endif
    
;---------------------------------------------------------------------
;Bz //////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if (Bz eq 1) then begin
        Bz_plot = MyWin -> Plot(t_ssm, B_temp[2,*], $
                                TITLE='Magneic Field ' + coords[0] + '-Component', $
                                XTITLE='UT (HH:MM:SS)', $
                                XTICKFORMAT='time_labels', $
                                YTITLE='B$\down' + coords[2] + '$!C(nT)', $
                                DRAW=0, $
                                ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then Bz_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then Bz = Bz_plot
    endif
    
;---------------------------------------------------------------------
;CLOCK ANGLE /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ;Plot the clock angle before the power, poynting flux, and
    ;polarization parameters. This is so that no detrending or rotating
    ;has been performed.
    ;
      
    if (clock_angle eq 1) then begin
        ;Calculate the clock angle: atan(By/Bz)
        B_hat = normalize(B_temp)
        ca_data = atan(B_hat[1,*] / B_hat[2,*]) * !radeg    ;degrees
        
        ;Create the plot
        ca_plot = MyWin -> Plot(t_ssm, temporary(ca_data), $
                                TITLE='Magnetic Field Clock Angle', $
                                XTITLE='UT (HH:MM:SS)', $
                                XTICKFORMAT='time_labels', $
                                YTITLE='Clock Angle!C(deg)', $
                                DRAW=0, $
                                ADD=add)
        
        ;Set additional keywords
        if n_elements(extra) gt 0 then ca_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then clock_angle = ca_plot
    endif
    
;---------------------------------------------------------------------
;POYNTING SPECTRA ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ;The poynting spectra must be plotted before the power spectra
    ;and the polarization parameters so that the magnetic field can
    ;be rotated into the MGSE frame before detrending and rotating
    ;into a field-aligned coordinate system.
    ;
    if (poynting_x + poynting_y + poynting_z gt 0) then begin
        
        ;Detrend and Rotate the data?
        if (nDetrend gt 0) or (nFAS gt 0) or (nFAR gt 0) then begin
            nSystem = nFAR > 0 ? nFAR : nFAS
            
            ;The rotation matrix to the field aligned system is calculated and
            ;returned here.
            B_poynt = MrDetrendRotate(B_poynt, nDetrend, nSystem, $
                                      POSITION=pos_poynt, $
                                      RMATRIX=rMatrix, $
                                      DIMENSION=2)
            
            ;rMatrix is input here to rotate the electric field
            E_poynt = MrDetrendRotate(E_poynt, nDetrend, $
                                      RMATRIX=rMatrix, $
                                      DIMENSION=2)
        endif

        ;Calculate the Poynting Flux
        S = MrPoynting_Spectra(temporary(E_poynt), temporary(B_poynt), nfft, dt_poynt, nshift, $
                               DIMENSION = dimension, $
                               FREQUENCIES = frequencies, $
                               TIME = t_poynt_spec, $
                              _REF_EXTRA = FFTkwds)
        t_poynt_spec += t_poynt_ssm[0]
        
        ;X
        if poynting_x gt 0 then begin
            Sx_image = MyWin -> Image(S[*,*,0], t_poynt_ssm, frequencies, $
                                      /AXES, $
                                      PALETTE=ctBWR, $
                                      TITLE='Poynting Flux: Sx', $
                                      XTICKFORMAT='time_labels', $
                                      XTITLE='UT (HH:MM:SS)', $
                                      YTITLE='Frequency!C(Hz)', $
                                      DRAW=0, $
                                      ADD=add)
            Sx_cb = MyWin -> Colorbar(GRAPHIC=Sx_image, $
                                      TITLE='Sx!C(' + cgSymbol('mu') + 'W/m^2)', $
                                      DRAW=0, $
                                      ADD=add)
            
            ;Bind the colorbar and image? Return the object reference?
            if add $
                then MyWin -> Bind, Sx_image, Sx_cb, /CAXIS $
                else poynting_x = [Sx_image, Sx_cb]
        endif
        
        ;Y
        if poynting_y gt 0 then begin
            Sy_image = MyWin -> Image(S[*,*,1], t_ssm_poynting, frequencies, $
                                      /AXES, $
                                      PALETTE=ctBWR, $
                                      TITLE='Poynting Flux: Sy', $
                                      XTICKFORMAT='time_labels', $
                                      XTITLE='UT (HH:MM:SS)', $
                                      YTITLE='Frequency!C(Hz)', $
                                      DRAW=0, $
                                      ADD=add)
            Sy_cb = MyWin -> Colorbar(GRAPHIC=Sy_image, $
                                      TITLE='Sy!C(' + cgSymbol('mu') + 'W/m^2)', $
                                      DRAW=0, $
                                      ADD=add)
            
            ;Bind the colorbar and image? Return the object reference?
            if add $
                then MyWin -> Bind, Sy_image, Sy_cb, /CAXIS $
                else poynting_y = [Sy_image, Sy_cb]
        endif
        
        ;Z
        if poynting_z gt 0 then begin
            Sz_image = MyWin -> Image(S[*,*,2], t_ssm_poynting, frequencies, $
                                      /AXES, $
                                      PALETTE=ctBWR, $
                                      TITLE='Poynting Flux: Sz', $
                                      XTICKFORMAT='time_labels', $
                                      XTITLE='UT (HH:MM:SS)', $
                                      YTITLE='Frequency!C(Hz)', $
                                      DRAW=0, $
                                      ADD=add)
            Sz_cb = MyWin -> Colorbar(GRAPHIC=Sz_image, $
                                      TITLE='Sz!C(' + cgSymbol('mu') + 'W/m^2)', $
                                      DRAW=0, $
                                      ADD=add)
            
            ;Bind the colorbar and image? Return the object reference?
            if add $
                then MyWin -> Bind, Sz_image, Sz_cb, /CAXIS $
                else poynting_z = [Sz_image, Sz_cb]
        endif
        
        ;Free data
        undefine, S, t_poynt_spec, t_poynt_ssm
    endif
    
;---------------------------------------------------------------------
;DETREND AND FIELD-ALIGNED SYSTEM ////////////////////////////////////
;---------------------------------------------------------------------
        
    ;Detrend and Rotate the data?
    if ((nDetrend gt 0) or (nFAS gt 0) or (nFAR gt 0)) and $
       (pwr_x + pwr_y + pwr_z +  polarization + ellipticity + intensity + $
        kdotb_angle + coherency gt 0) $
    then begin
        nSystem = nFAR > 0 ? nFAR : nFAS
        B_temp = MrDetrendRotate(B_temp, nDetrend, nSystem, $
                                 POSITION=pos, $
                                 DIMENSION=dimension)
    endif
    
;---------------------------------------------------------------------
;ORDER-N DIFFERENCE //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if (nDiff ne 0) then B_temp = MrTS_Diff(B_temp, nDiff, DIMENSION=dimension, RECURSIVE=0)
    
;---------------------------------------------------------------------
;dB_vec //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (dB_vec eq 1) then begin
        dBvec_plot = MyWin -> Plot(t_ssm, B_temp, $
                                   DIMENSION=2, $
                                   TITLE='Magneic Field: $\delta$B', $
                                   XTITLE='UT (HH:MM:SS)', $
                                   XTICKFORMAT='time_labels', $
                                   YTITLE='$\delta$B!C(nT)', $
                                   DRAW=0, $
                                   ADD=add)
        
        dBvec_legend = MyWin -> Legend(TITLE='$\delta$B$\down' + coords + '$', $
                                       GRAPHIC=dBvec_plot, $
                                       LOCATION=8, $
                                       DRAW=0, $
                                       ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then dBvec_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then dB_vec = [dBvec_plot, dBvec_legend]
    endif
    
;---------------------------------------------------------------------
;dBx /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (dBx eq 1) then begin
        dBx_plot = MyWin -> Plot(t_ssm, B_temp[0,*], $
                                 DIMENSION=2, $
                                 TITLE='Magneic Field: $\delta$B$\down' + coords[0] + '$', $
                                 XTITLE='UT (HH:MM:SS)', $
                                 XTICKFORMAT='time_labels', $
                                 YTITLE='$\delta$B$\down' + coords[0] + '$!C(nT)', $
                                 DRAW=0, $
                                 ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then dBx_plot -> SetProperty, _EXTRA=extra
        
        ;Add the plot to the window? Return the object reference?
        if (add eq 0) then dBx = dBx_plot
    endif
    
;---------------------------------------------------------------------
;dBy /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (dBy eq 1) then begin
        dBy_plot = MyWin -> Plot(t_ssm, B_temp[1,*], $
                                 DIMENSION=2, $
                                 TITLE='Magneic Field: $\delta$B$\down' + coords[1] + '$', $
                                 XTITLE='UT (HH:MM:SS)', $
                                 XTICKFORMAT='time_labels', $
                                 YTITLE='$\delta$B$\down' + coords[1] + '$!C(nT)', $
                                 DRAW=0, $
                                 ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then dBy_plot -> SetProperty, _EXTRA=extra
        
        ;Add the plot to the window? Return the object reference?
        if (add eq 0) then dBy = dBy_plot
    endif
    
;---------------------------------------------------------------------
;dBz /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (dBz eq 1) then begin
        dBz_plot = MyWin -> Plot(t_ssm, B_temp[2,*], $
                                 DIMENSION=2, $
                                 TITLE='Magneic Field: $\delta$B$\down' + coords[2] + '$', $
                                 XTITLE='UT (HH:MM:SS)', $
                                 XTICKFORMAT='time_labels', $
                                 YTITLE='$\delta$B$\down' + coords[2] + '$!C(nT)', $
                                 DRAW=0, $
                                 ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then dBz_plot -> SetProperty, _EXTRA=extra
        
        ;Add the plot to the window? Return the object reference?
        if (add eq 0) then dBz = dBz_plot
    endif
    
;---------------------------------------------------------------------
;POWER SPECTRA ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
      
    if (pwr_x + pwr_y + pwr_z gt 0) then begin
        pwrXYZ_image = MrSpectrogram(B_temp, nfft, dt, nshift, /OREF, $
                                     DIMENSION=dimension, $
                                     T0=t_ssm[0], $
                                     CBARR=pwrXYZ_cb, $
                                     TITLE='Power Spectral Density: B$\down' + coords + '$', $
                                     CBTITLE='Log B$\down' + coords + '$ Power!C(nT^2 * Hz)', $
                                    _EXTRA=FFTKwds)
        
        ;Add and Bind the images?
        if (add eq 1) then begin
            if (pwr_x eq 1) then begin
                MyWin -> Add, [pwrXYZ_image[0], pwrXYZ_cb[0]]
                MyWin -> Bind, pwrXYZ_image[0], pwrXYZ_cb[0], /CAXIS
            endif else obj_destroy, [pwrXZY_image[0], pwr_XYZ_cb[0]]
            
            if (pwr_y eq 1) then begin
                MyWin -> Add, [pwrXYZ_image[1], pwrXYZ_cb[1]]
                MyWin -> Bind, pwrXYZ_image[1], pwrXYZ_cb[1], /CAXIS
            endif else obj_destroy, [pwrXZY_image[1], pwr_XYZ_cb[1]]
            
            if (pwr_z eq 1) then begin
                MyWin -> Add, [pwrXYZ_image[2], pwrXYZ_cb[2]]
                MyWin -> Bind, pwrXYZ_image[2], pwrXYZ_cb[2], /CAXIS
            endif else obj_destroy, [pwrXZY_image[2], pwr_XYZ_cb[2]]
        
        ;Return the object references?
        endif else begin
            if (pwr_x eq 1) then pwr_x = [pwrXYZ_image[0], pwrXYZ_cb[0]] $
                            else obj_destroy, [pwrXYZ_image[0], pwrXYZ_cb[0]]
            if (pwr_y eq 1) then pwr_y = [pwrXYZ_image[1], pwrXYZ_cb[1]] $
                            else obj_destroy, [pwrXYZ_image[1], pwrXYZ_cb[1]]
            if (pwr_z eq 1) then pwr_z = [pwrXYZ_image[2], pwrXYZ_cb[2]] $
                            else obj_destroy, [pwrXYZ_image[2], pwrXYZ_cb[2]]
        endelse
    endif
    
;---------------------------------------------------------------------
;POLARIZATION ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if (polarization + ellipticity + intensity + kdotb_angle + coherency gt 0) then begin
        void = MrPolPlot(B_temp, nfft, dt, nshift, /OREF, $
                         DIMENSION = dimension, $
                         POLARIZATION = polarization_image, $
                         COHERENCY = coherency_image, $
                         ELLIPTICITY = ellipticity_image, $
                         INTENSITY = intensity_image, $
                         KDOTB_ANGLE = kdotb_angle_image, $
                         T0 = t_ssm[0])
        
        ;Add and Bind the images?
        if (add eq 1) then begin
            if (intensity eq 1) then begin
                MyWin -> Add, intensity_image
                MyWin -> Bind, intensity_image, /CAXIS
            endif else obj_destroy, intensity_image
            
            if (polarization eq 1) then begin
                MyWin -> Add, polarization_image
                MyWin -> Bind, polarization_image, /CAXIS
            endif else obj_destroy, polarization_image
            
            if (ellipticity eq 1) then begin
                MyWin -> Add, ellipticity_image
                MyWin -> Bind, ellipticity_image, /CAXIS
            endif else obj_destroy, ellipticity_image
            
            if (kdotb_angle eq 1) then begin
                MyWin -> Add, kdotb_angle_image
                MyWin -> Bind, kdotb_angle_image, /CAXIS
            endif else obj_destroy, kdotb_angle_image
            
            if (coherency eq 1) then begin
                MyWin -> Add, coherency_image
                MyWin -> Bind, coherency_image, /CAXIS
            endif else obj_destroy, coherency_image
            
        ;Return the object references?
        endif else begin
            if (polarization eq 1) then polarization = polarization_image else obj_destroy, polarization_image
            if (ellipticity  eq 1) then ellipticity  = ellipticity_image  else obj_destroy, ellipticity_image
            if (intensity    eq 1) then intensity    = intensity_image    else obj_destroy, intensity_image
            if (kdotb_angle  eq 1) then kdotb_angle  = kdotb_angle_image  else obj_destroy, kdotb_angle_image
            if (coherency    eq 1) then coherency    = coherency_image    else obj_destroy, coherency_image
        endelse
    endif

;---------------------------------------------------------------------
;Create the Plot /////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Return a single plot?
    if keyword_set(oRef) eq 0 then begin
        ;Bind the X-axis of all images and plots together.
        allImPl = MyWin -> Get(/IMAGE, /PLOT, COUNT=nImPl)
        if nImPl gt 1 then MyWin -> BindEm, allImPl, /XAXIS
        
        allCB = MyWin -> Get(/COLORBAR, count=nCB)
        allLeg = MyWin -> Get(/LEGEND, count=nLeg)
        if nCB gt 0 $
            then MyWin -> SetProperty, XMARGIN=[10,15] $
            else if nLeg gt 0 then MyWin -> SetProperty, XMARGIN=[10,8]
        
        ;Draw and return.
        MyWin -> RealizeGUI
    
    ;Return all plots individually?
    endif else begin
        obj_destroy, MyWin
        MyWin = obj_new()
    endelse
    
    ;reset the color table
    tvlct, rr, gg, bb
    
    return, mywin
end