; docformat = 'rst'
;
; NAME:
;       MrEFWProducts
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
;   The purpose of this program is to create plots for different data products relating
;   to electric field data.
;
;   Order of Operations::
;       1. Read magnetic field and position data (if nFAR or nFAS ne 0)
;       2. Read electric field data
;       3. Compute Ex -- If true, then all products from here on out will be made with
;                        interpolated data.
;           - Rotate B to MGSE
;           - Interpolate B and E
;           - E dot B = 0
;       4. Plot CDF Data products
;           - E_vec
;           - Ex
;           - Ey
;           - Ez
;       5. Detrend and/or Rotate Data
;       6. Order N Difference
;       7. Plot time series products
;           - dEx
;           - dEy
;           - dEz
;       8. Plot PSD
;           - pwr_x
;           - pwr_y
;           - pwr_z
;       
;
; :Categories:
;
;       Van Allen Probes
;
; :Params:
;       SC:                 in, required, type=string/int
;                           The number of the Cluster spacecraft whose data is to be
;                               plotted. If an integer is given, it will be converted to
;                               a string.
;       SDATETIME:          in, required, type=string
;                           The date-time string indicating the start of the data interval
;                               to be read. Proper format is 'YYYY-MM-DDTHH:MM:SS.ddddddddd',
;                               where all delimeting characters are optional.
;       EDATETIME:          in, required, type=string
;                           The date-time string indicating the end of the data interval
;                               being read. Should be formated the same as `SDATETIME`.
;
; :Keywords:
;       DIRECTORY:          in, optional, type=string, default=pwd()
;                           The directory in which to search for EFW data.
;       LMN_FRAME:          in, optional, type=fltarr(3\,3), default=identity(3)
;                           A coordinate transformation matrix for rotating into the
;                               minimum variance frame.
;       OREF:               in, optional, type=boolean, default=0
;                           If set, then `B_MAG`, `B_VEC`, `POSITION`, and 'CLOCK_ANGLE'
;                               must be named variables into which their plot's object
;                               reference will be returned. In this case, the plots will
;                               not be put into a MrWindow widget and `EFW_WIN` will be
;                               an invalid object reference.
;
;       E_VEC:              in, optional, type=boolea, default=0
;                           If set, the 3-component electric field will be plotted. All
;                               components will be contained within the same plot axes.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrPlot__Define is also accepted
;                               for keyword inheritance. Note that these keywords will be
;                               applied to each plot being created.
;
; :Returns:
;       EFW_WIN:            out, required, type=object
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
;       MrWindow
;       cdf_read
;       cluster_find_file
;       dissectdatetime
;       pwd
;       rbsp_find_file
;       CDF_Read__define
;       epoch_to_ssm
;       rbsp_read_efw_asc
;       rbsp_mgse_to_gse
;       MrInterp_TS
;       EdotB_zero
;       MrDetrendRotate
;       MrSpectrogram
;       MrLog
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
;       10/06/2013  -   Written by Matthew Argall
;-
function MrEFWProducts, tE_ssm, E_data, components, tB_ssm, B_data, $
COORD_SYSTEM = coord_system, $
DIMENSION = dimension, $
DIRECTORY = directory, $
OREF = oRef, $
T_INERT_TO_SCS = T_INERT_to_SCS, $
TMATRIX = TMatrix, $
;Detrend and Rotate
NDETREND = nDetrend, $
NDIFF = nDiff, $
NFAS = nFAS, $
NFAR = nFAR, $
POSITION = position, $
;FFT
NFFT = nfft, $
NSHIFT = nshift, $
DT = dt, $
FFTKWDS = fftkwds, $
;Data Products
E_VEC = E_vec, $
EX = Ex, $
EY = Ey, $
EZ = Ez, $
DE_VEC = dE_vec, $
DEX = dEx, $
DEY = dEy, $
DEZ = dEz, $
COMPUTE_EX = compute_Ex, $
COMPUTE_EY = compute_Ey, $
COMPUTE_EZ = compute_Ez, $
;Other Products
PWR_X = pwr_x, $
PWR_Y = pwr_y, $
PWR_Z = pwr_z, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        
        ;Reset the color table
        tvlct, r, g, b
        
        ;Files
        if obj_valid(EFW_oRead) then obj_destroy, EFW_oRead
        if obj_valid(FGM_oRead) then obj_destroy, FGM_oRead

        ;Plots
        if obj_valid(Evec_plot)  then obj_destroy, Evec_plot
        if obj_valid(Ex_plot)    then obj_destroy, Ex_plot
        if obj_valid(Ey_plot)    then obj_destroy, Ey_plot
        if obj_valid(Ez_plot)    then obj_destroy, Ez_plot
        if obj_valid(dEvec_plot) then obj_destroy, dEvec_plot
        if obj_valid(dEx_plot)   then obj_destroy, dEx_plot
        if obj_valid(dEy_plot)   then obj_destroy, dEy_plot
        if obj_valid(dEz_plot)   then obj_destroy, dEz_plot
        
        ;Images
        if max(obj_valid(pwrXYZ_image) eq 1) then obj_destroy, pwrXYZ_image
        if max(obj_valid(pwrXYZ_cb)    eq 1) then obj_destroy, pwrXYZ_cb
        
        ;Legends
        if obj_valid(Evec_legend) then obj_destroy, Evec_legend
        
        ;Plotting Window
        if obj_valid(MyWin) then obj_destroy, MyWin
        
        return, obj_new()
    endif
        
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Get the current color table
    tvlct, r, g, b, /GET
    
    ;Defaults
    setDefaultValue, directory, pwd()
    setDefaultValue, oRef, 0, /BOOLEAN
    add = ~oRef
    
    ;Coordinate system and components. If the 2D electric field was given, we need to know
    ;which components are present.
    if n_elements(components)   eq 0 then components = ['X', 'Y', 'Z']
    if n_elements(coord_system) eq 0 then coords = ['X', 'Y', 'Z'] else coords = coord_system
    iX = where(components eq coords[0])
    iY = where(components eq coords[1])
    iZ = where(components eq coords[2])
    iCoords = [iX, iY, iZ]
    
    ;Which data product is to be plotted?
    if keyword_set(oRef) then begin
        E_vec  = arg_present(E_vec)
        Ex     = arg_present(Ex)
        Ey     = arg_present(Ey)
        Ez     = arg_present(Ez)
        dE_vec = arg_present(E_vec)
        dEx    = arg_present(dEx)
        dEy    = arg_present(dEy)
        dEz    = arg_present(dEz)
        pwr_x  = arg_present(pwr_x)
        pwr_y  = arg_present(pwr_y)
        pwr_z  = arg_present(pwr_z)
    endif else begin
        E_vec  = keyword_set(E_vec)
        Ex     = keyword_set(Ex)
        Ey     = keyword_set(Ey)
        Ez     = keyword_set(Ez)
        dE_vec = keyword_set(E_vec)
        dEx    = keyword_set(dEx)
        dEy    = keyword_set(dEy)
        dEz    = keyword_set(dEz)
        pwr_x  = keyword_set(pwr_x)
        pwr_y  = keyword_set(pwr_y)
        pwr_z  = keyword_set(pwr_z)
    endelse
    
    compute_Ex = keyword_set(compute_Ex)
    compute_Ey = keyword_set(compute_Ey)
    compute_Ez = keyword_set(compute_Ez)
    if (compute_Ex + compute_Ey + compute_Ez gt 1) then $
        message, 'COMPUTE_E[X,Y,Z] keywords are mutually exclusive.'
        
    ;Detrend and Rotate?
    if n_elements(nDetrend) eq 0 then nDetrend = 0
    if n_elements(nDiff)    eq 0 then nDiff    = 0
    if n_elements(nFAS)     eq 0 then nFAS     = 0
    if n_elements(nFAR)     eq 0 then nFAR     = 0
    
    ;FAS and FAR cannot be used together. They must match NDETREND
    if nFAS gt 0 and nFAR gt 0     then message, 'Only one of NFAS and NFAR may be non-zero.'
    if nDetrend gt 0 and nFAS gt 0 then nFAS = nDetrend
    if nDetrend gt 0 and nFAR gt 0 then nFAR = nDetrend
    if nFAR gt 0 and n_elements(position) eq 0 then message, 'POSITION must be provided if NFAR > 0.'
    
    ;If we are computing the X-component of E, then make sure we can tranform the
    ;magnetic field from GSE to MGSE. To do this, we need a transformation matrix
    ;from UVW to GSE.
    if (compute_Ex + compute_Ey + compute_Ez eq 1) then begin
        if n_elements(T_INERT_to_SCS) eq 0 then begin
            message, 'If COMPUTE_E[XYZ] is set, then T_INERT_to_SCS must be provided to transform ' + $
                     'magnetic field data into the spacecraft frame.', /INFORMATIONAL
            compute_Ex = 0
            Ex = 0
            pwr_x = 0
            dEx = 0
        endif 
    endif
    
    ;Default to plotting all of the standard data products
    if (E_vec + Ex + Ey + Ez + dE_vec + dEx + dEy + dEz + $
        pwr_x + pwr_y + pwr_z eq 0) $
    then begin
        E_vec = 1
    endif
    
    ;Create the window
    MyWin = MrWindow(XSIZE=500, YSIZE=700, BUILD=0)

;---------------------------------------------------------------------
;E dot B = 0? ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Compute the X-component by assuming (E dot B = 0)
    if (compute_Ex + compute_Ey + compute_Ez eq 1) then begin
        ;Transform B from an inertial frame into the spacecraft frame. Combine the
        ;third component into the other two.
        data_Evec = EdotB_zero(E_data, B_data, tE_ssm, tB_ssm, $
                               X=compute_Ex, Y=compuate_Ey, Z=compute_Ez, $
                               TMATRIX=T_INERT_to_SCS, /COMBINE)
        
        legend_title = ['Ex', 'Ey', 'Ez']
        legend_color = ['Blue', 'Forest Green', 'Red']
        color        = ['Forest Green', 'Red', 'Blue']
    endif else begin
        data_Evec = E_data
        legend_title = ['Ey', 'Ez']
        legend_color = ['Forest Green', 'Red']
        color        = legend_color
    endelse
        
;---------------------------------------------------------------------
;Rotate to a New Reference Frame? ////////////////////////////////////
;---------------------------------------------------------------------
      if n_elements(TMatrix) gt 0 then data_Evec = rotate_vector(TMatrix, data_Evec)
        
;---------------------------------------------------------------------
;E_Vec ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
        
    if (E_vec eq 1) then begin
        ;Create the plot
        Evec_plot = MyWin -> Plot(tE_ssm, data_Evec, $
                                  COLOR=color, $
                                  DIMENSION=2, $
                                  TITLE='Van Allen Probes EFW', $
                                  XTITLE='UT (HH:MM:SS)', $
                                  XTICKFORMAT='time_labels', $
                                  YTITLE='E!C(mV/m)', $
                                  DRAW=0, $
                                  ADD=add)
        
        ;Add a legend to the plot
        Evec_legend = MyWin -> Legend(TITLE=legend_title, $
                                      COLOR=legend_color, $
                                      LENGTH=0, $
                                      LOCATION=8, $
                                      DRAW=0, $
                                      ADD=add)

        ;Set additional keywords
        if n_elements(extra) gt 0 then Evec_plot -> SetProperty, _EXTRA=extra
        
        ;Add the plot to the window? Return the object reference?
        if (add eq 0) then E_vec = [Evec_plot, Evec_legend]
    endif
    
;---------------------------------------------------------------------
;Ex //////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if (Ex eq 1) then begin
        Ex_plot = MyWin -> Plot(tE_ssm, data_Evec[iCoords[0],*], $
                                DIMENSION=dimension, $
                                TITLE='Electric Field: E$\down' + coords[0] + '$', $
                                XTITLE='UT (HH:MM:SS)', $
                                XTICKFORMAT='time_labels', $
                                YTITLE='E$\down' + coords[0] + '$!C(mV/m)', $
                                DRAW=0, $
                                ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then Ex_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if add $
            then MyWin -> Add, Ex_plot $
            else Ex = Ex_plot
    endif
    
;---------------------------------------------------------------------
;Ey //////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (Ey eq 1) then begin
        Ey_plot = MyWin -> Plot(tE_ssm, data_Evec[iCoords[1],*], $
                                DIMENSION=dimension, $
                                TITLE='Electric Field: E$\down' + coords[1] + '$', $
                                XTITLE='UT (HH:MM:SS)', $
                                XTICKFORMAT='time_labels', $
                                YTITLE='E$\down' + coords[1] + '$!C(mV/m)', $
                                DRAW=0, $
                                ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then Ey_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then Ey = Ey_plot
    endif
    
;---------------------------------------------------------------------
;Ez //////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (Ez eq 1) then begin
        Ez_plot = MyWin -> Plot(tE_ssm, data_Evec[iCoords[2],*], $
                                DIMENSION=dimension, $
                                TITLE='Electric Field: E$\down' + coords[2] + '$', $
                                XTITLE='UT (HH:MM:SS)', $
                                XTICKFORMAT='time_labels', $
                                YTITLE='E$\down' + coords[2] + '$!C(mV/m)', $
                                DRAW=0, $
                                ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then Ez_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then Ez = Ez_plot
    endif

;---------------------------------------------------------------------
;DETREND AND FIELD-ALIGNED SYSTEM ////////////////////////////////////
;---------------------------------------------------------------------
        
    ;Field-aligned system? Create the transformation matrix with the magnetic field data.
    if (nFAS gt 0) or (nFAR gt 0) then begin
        nSystem = nFAR > 0 ? nFAR : nFAS
        data_Bvec = MrDetrendRotate(data_Bvec, nDetrend, nSystem, $
                                    POSITION=data_pos, $
                                    RMATRIX=rMatrix, $
                                    DIMENSION=2)
        data_Evec = MrDetrendRotate(data_Evec, nDetrend, nSystem, $
                                    RMATRIX=rMatrix, $
                                    DIMENSION=2)

    ;Simply detrend the data?
    endif else if nDetrend gt 0 then begin
        data_Evec = MrDetrendRotate(data_Evec, nDetrend, nSystem, $
                                    POSITION=data_pos, $
                                    RMATRIX=rMatrix, $
                                    DIMENSION=2)
    endif

;---------------------------------------------------------------------
;ORDER-N DIFFERENCE //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if nDiff gt 0 then data_Evec = MrTS_Diff(data_Evec, nDiff, RECURSIVE=0, DIMENSION=2)
    
;---------------------------------------------------------------------
;dEx /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;By design -- If dEx = 1, then COMPUTE_EX = 1
    if (dEx eq 1) then begin
        dEx_plot = MyWin -> Plot(tE_ssm, data_Evec[iCoords[0],*], $
                                 TITLE='$\delta$E$\down' + coords[0] + '$', $
                                 XTITLE='UT (HH:MM:SS)', $
                                 XTICKFORMAT='time_labels', $
                                 YTITLE='$\delta$E$\down' + coords[0] + '$!C(mV/m)', $
                                 DRAW=0, $
                                 ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then dEx_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then dEx = dEx_plot
    endif
    
;---------------------------------------------------------------------
;dEy /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (dEy eq 1) then begin
        dEy_plot = MyWin -> Plot(tE_ssm, data_Evec[iCoords[1],*], $
                                 TITLE='$\delta$E$\down' + coords[1] + '$', $
                                 XTITLE='UT (HH:MM:SS)', $
                                 XTICKFORMAT='time_labels', $
                                 YTITLE='$\delta$E$\down' + coords[1] + '$!C(mV/m)', $
                                 DRAW=0, $
                                 ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then dEy_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then dEy = dEy_plot
    endif
    
;---------------------------------------------------------------------
;dEx /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (dEz eq 1) then begin
        dEz_plot = MyWin -> Plot(tE_ssm, data_Evec[iCoords[2],*], $
                                 TITLE='$\delta$E$\down' + coords[2] + '$', $
                                 XTITLE='UT (HH:MM:SS)', $
                                 XTICKFORMAT='time_labels', $
                                 YTITLE='$\delta$E$\down' + coords[2] + '$!C(mV/m)', $
                                 DRAW=0, $
                                 ADD=add)
                                
        ;Set additional keywords
        if n_elements(extra) gt 0 then dEz_plot -> SetProperty, _EXTRA=extra
        
        ;Return the object reference?
        if (add eq 0) then dEz = dEz_plot
    endif






;---------------------------------------------------------------------
;POWER SPECTRA ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
      
    if (pwr_x + pwr_y + pwr_z gt 0) then begin
        pwrXYZ_image = MrSpectragram(data_Evec, nfft, dt, nshift, /OREF, $
                                     DIMENSION=2, $
                                     T0=t_ssm[0], $
                                     CBARR=pwrXYZ_cb, $
                                     TITLE='Power Spectral Density: E$\down' + coords + '$', $
                                     CBTITLE='Log E$\down' + coords + '$ Power!C(nT^2 * Hz)', $
                                    _EXTRA=FFTKwds)
        
        ;Add and Bind the images?
        if (add eq 1) then begin
            if (pwr_x eq 1) then begin
                MyWin -> Add, [pwrXYZ_image[iCoords[0]], pwrXYZ_cb[iCoords[0]]]
                MyWin -> Bind, pwrXYZ_image[iCoords[0]], pwrXYZ_cb[iCoords[0]], /CAXIS
            endif else obj_destroy, [pwrXZY_image[iCoords[0]], pwr_YZ_cb[iCoords[0]]]
            
            if (pwr_y eq 1) then begin
                MyWin -> Add, [pwrXYZ_image[iCoords[1]], pwrXYZ_cb[iCoords[1]]]
                MyWin -> Bind, pwrXYZ_image[iCoords[1]], pwrXYZ_cb[iCoords[1]], /CAXIS
            endif else obj_destroy, [pwrXZY_image[iCoords[1]], pwrXYZ_cb[iCoords[1]]]
            
            if (pwr_z eq 1) then begin
                MyWin -> Add, [pwrXYZ_image[iCoords[2]], pwrXYZ_cb[iCoords[2]]]
                MyWin -> Bind, pwrXYZ_image[iCoords[2]], pwrXYZ_cb[iCoords[2]], /CAXIS
            endif else obj_destroy, [pwrXZY_image[iCoords[2]], pwrXYZ_cb[iCoords[2]]]
        
        ;Return the object references?
        endif else begin
            if (pwr_x eq 1) then pwr_x = [pwrXYZ_image[iCoords[0]], pwrXYZ_cb[iCoords[0]]] $
                            else obj_destroy, [pwrXYZ_image[iCoords[0]], pwrXYZ_cb[iCoords[0]]]
            if (pwr_y eq 1) then pwr_y = [pwrXYZ_image[iCoords[1]], pwrXYZ_cb[iCoords[1]]] $
                            else obj_destroy, [pwrXYZ_image[iCoords[1]], pwrXYZ_cb[iCoords[1]]]
            if (pwr_z eq 1) then pwr_z = [pwrXYZ_image[iCoords[2]], pwrXYZ_cb[iCoords[2]]] $
                            else obj_destroy, [pwrXYZ_image[iCoords[2]], pwrXYZ_cb[iCoords[2]]]
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

        ;Resize the window
        if nImPl gt 0 and nImPl lt 5 then MyWin -> SetProperty, YSIZE=nImPl*250
        
        ;If a colorbar or legend was made, increase the right margin so it fits.
        allCB = MyWin -> Get(/COLORBAR, count=nCB)
        allLeg = MyWin -> Get(/LEGEND, count=nLeg)
        if nCB gt 0 then xmargin = [10,15] else if nLeg gt 0 then xmargin = [10,8]
        MyWin -> SetProperty, XMARGIN=xmargin
        
        ;Draw and return.
        MyWin -> RealizeGUI
    
    ;Return all plots individually?
    endif else begin
        obj_destroy, MyWin
        MyWin = obj_new()
    endelse
    
    ;reset the color table
    tvlct, r, g, b
    
    return, mywin
end