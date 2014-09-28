; docformat = 'rst'
;
; NAME:
;       c_PEA_DistFn
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
;   Create plots of pitch angle distribution functions using data from the PEACE
;   instrument on board the Cluster spacecraft.
;
; :Categories:
;       Cluster
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       cgLoadCT.pro
;       cgPlot.pro
;       MrFile__Define.pro
;       MrLayout.pro
;       MrLog.pro
;       Undefine.pro (Coyote Graphics)
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
;       2014/06/02  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Draw the distribution function on a linear scale.
;
; :Private:
;
; :Params:
;       HEEA_DEFlux:        in, required, type=AxExPxT fltarr
;                           HEEA differential energy flux data. A, E, and P represent
;                               the number of azimuthal, energy, and pitch-angle bins,
;                               respectively. T represents the number of records.
;       HEEA_E:             in, required, type=fltarr
;                           Energy for instrument bin centers.
;       HEEA_E_DPlus:       in, required, type=fltarr
;                           Upper bound for the instrument energy bins
;       HEEA_E_DMinus:      in, required, type=fltarr
;                           Lower bound for the instrument energy bins
;       HEEA_PA:            in, required, type=fltarr
;                           Pitch angle for instrument bin centers.
;       HEEA_PA_DPlus:      in, required, type=fltarr
;                           Upper bound of the instrument pitch angle bins.
;       HEEA_PA_DMinus:     in, required, type=fltarr
;                           Lower bound of the instrument pitch angle bins.
;       LEEA_DEFlux:        in, required, type=AxExPxT fltarr
;                           LEEA differential energy flux data. A, E, and P represent
;                               the number of azimuthal, energy, and pitch-angle bins,
;                               respectively. T represents the number of records.
;       LEEA_E:             in, required, type=fltarr
;                           Energy for instrument bin centers.
;       LEEA_E_DPlus:       in, required, type=fltarr
;                           Upper bound for the instrument energy bins
;       LEEA_E_DMinus:      in, required, type=fltarr
;                           Lower bound for the instrument energy bins
;       LEEA_PA:            in, required, type=fltarr
;                           Pitch angle for instrument bin centers.
;       LEEA_PA_DPlus:      in, required, type=fltarr
;                           Upper bound of the instrument pitch angle bins.
;       LEEA_PA_DMinus:     in, required, type=fltarr
;                           Lower bound of the instrument pitch angle bins.
;-
pro c_PEA_DistFn_linear, HEEA_DEFlux, HEEA_E,  HEEA_E_DPlus,  HEEA_E_DMinus, $
                                              HEEA_PA, HEEA_PA_DPlus, HEEA_PA_DMinus, $
                                 LEEA_DEFlux, LEEA_E,  LEEA_E_DPlus,  LEEA_E_DMinus, $
                                              LEEA_PA, LEEA_PA_DPlus, LEEA_PA_DMinus
    compile_opt strictarr
    
    catch, theError
    if theError ne 0 then begin
        catch, /cancel
        if n_elements(this_decomposed) gt 0 then device, DECOMPOSED=this_decomposed
        if n_elements(r) gt 0 then tvlct, r, g, b
        void = cgErrorMsg()
        return
    endif

    
;---------------------------------------------------------------------
; Prepare Log Output /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ; What should be done:
    ;   Draw a bin centered on E with corners:
    ;       1 = (E - dE) * cos(PA - dPA)
    ;       2 = (E + dE) * cos(PA - dPA)
    ;       3 = (E - dE) * sin(PA + dPA)
    ;       4 = (E + dE) * sin(PA + dPA)
    ;
    
    ;Number of elements in each dimension
    nTime   = n_elements(time)
    nEnergy = n_elements(LEEA_E[*,0])
    nPA     = n_elements(LEEA_PA[*,0])

    if velocity then begin
        eMass  = 9.1E-31
        charge = 1.6e-19
        coef   = (2*charge) / (eMass * 1.0e14)     ;1.0e7 to make units 10^4
        
        ;Convert to velocity space
        LEEA_E        = sqrt(coef * LEEA_E)
        LEEA_E_DPlus  = sqrt(coef * LEEA_E_DPlus)
        LEEA_E_DMinus = sqrt(coef * LEEA_E_DMinus)
    endif else begin
        ;Convert to log energy
        LEEA_E        = MrLog(LEEA_E)
        LEEA_E_DPlus  = MrLog(LEEA_E_DPlus)
        LEEA_E_DMinus = MrLog(LEEA_E_DMinus)
    endelse
    
    ;Convert to radians
    LEEA_PA        = LEEA_PA        * !DtoR
    LEEA_PA_DPlus  = LEEA_PA_DPlus  * !DtoR
    LEEA_PA_DMinus = LEEA_PA_DMinus * !DtoR
    
    ;Reform to have correct dimensions
    LEEA_E         = rebin(reform(LEEA_E,        nEnergy, 1, nTime), nEnergy, nPA, nTime)
    LEEA_E_DPlus   = rebin(reform(LEEA_E_DPlus,  nEnergy, 1, nTime), nEnergy, nPA, nTime)
    LEEA_E_DMinus  = rebin(reform(LEEA_E_DMinus, nEnergy, 1, nTime), nEnergy, nPA, nTime)
    LEEA_PA        = rebin(reform(LEEA_PA,        1, nPA, nTime), nEnergy, nPA, nTime)
    LEEA_PA_DPlus  = rebin(reform(LEEA_PA_DPlus,  1, nPA, nTime), nEnergy, nPA, nTime)
    LEEA_PA_DMinus = rebin(reform(LEEA_PA_DMinus, 1, nPA, nTime), nEnergy, nPA, nTime)
    
    ;X
    x    = LEEA_E * cos(LEEA_PA)
    x_bl = (LEEA_E - LEEA_E_DMinus) * cos(LEEA_PA - LEEA_PA_DMinus)
    x_br = (LEEA_E + LEEA_E_DPlus)  * cos(LEEA_PA - LEEA_PA_DMinus)
    x_tl = (LEEA_E - LEEA_E_DMinus) * cos(LEEA_PA + LEEA_PA_DPlus)
    x_tr = (LEEA_E + LEEA_E_DPlus)  * cos(LEEA_PA + LEEA_PA_DPlus)
    
    ;Y
    y    = LEEA_E * sin(LEEA_PA)
    y_bl = (LEEA_E - LEEA_E_DMinus) * sin(LEEA_PA - LEEA_PA_DMinus)
    y_br = (LEEA_E + LEEA_E_DPlus)  * sin(LEEA_PA - LEEA_PA_DMinus)
    y_tl = (LEEA_E - LEEA_E_DMinus) * sin(LEEA_PA + LEEA_PA_DPlus)
    y_tr = (LEEA_E + LEEA_E_DPlus)  * sin(LEEA_PA + LEEA_PA_DPlus)
    
    ;Prep the image
    image_LEEA = MrLog(mean(LEEA_DEFlux, DIMENSION=3))
    
    ;Free some data
    undefine, LEEA_E, LEEA_E_DMinus, LEEA_E_DPlus
    undefine, LEEA_PA, LEEA_PA_DPlus, LEEA_PA_DMinus
    
;---------------------------------------------------------------------
; Prepare HEEA ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ; HEEA is going to be plotted from 180-360 degrees: 0,180] -> [180,360]
    HEEA_PA = 360.0 - HEEA_PA

    ;Convert to velocity space
    if velocity then begin
        HEEA_E        = sqrt(coef * HEEA_E)
        HEEA_E_DPlus  = sqrt(coef * HEEA_E_DPlus)
        HEEA_E_DMinus = sqrt(coef * HEEA_E_DMinus)
    ;Convert to log energy
    endif else begin
        HEEA_E        = MrLog(HEEA_E)
        HEEA_E_DPlus  = MrLog(HEEA_E_DPlus)
        HEEA_E_DMinus = MrLog(HEEA_E_DMinus)
    endelse
    
    ;Convert to radians
    HEEA_PA        = HEEA_PA        * !DtoR
    HEEA_PA_DPlus  = HEEA_PA_DPlus  * !DtoR
    HEEA_PA_DMinus = HEEA_PA_DMinus * !DtoR
    
    ;Reform to have correct dimensions
    HEEA_E         = rebin(HEEA_E,        nEnergy, nPA, nTime)
    HEEA_E_DPlus   = rebin(HEEA_E_DPlus,  nEnergy, nPA, nTime)
    HEEA_E_DMinus  = rebin(HEEA_E_DMinus, nEnergy, nPA, nTime)
    HEEA_PA        = rebin(reform(HEEA_PA,        1, nPA, nTime), nEnergy, nPA, nTime)
    HEEA_PA_DPlus  = rebin(reform(HEEA_PA_DPlus,  1, nPA, nTime), nEnergy, nPA, nTime)
    HEEA_PA_DMinus = rebin(reform(HEEA_PA_DMinus, 1, nPA, nTime), nEnergy, nPA, nTime)
    
    ;U
    ;   Since pitch angles range from [180, 360], we must add DMinus to get to 0.
    u    = HEEA_E * cos(HEEA_PA)
    u_bl = (HEEA_E - HEEA_E_DMinus) * cos(HEEA_PA + HEEA_PA_DMinus)
    u_br = (HEEA_E + HEEA_E_DPlus)  * cos(HEEA_PA + HEEA_PA_DMinus)
    u_tl = (HEEA_E - HEEA_E_DMinus) * cos(HEEA_PA - HEEA_PA_DPlus)
    u_tr = (HEEA_E + HEEA_E_DPlus)  * cos(HEEA_PA - HEEA_PA_DPlus)
    
    ;V
    v    = HEEA_E * sin(HEEA_PA)
    v_bl = (HEEA_E - HEEA_E_DMinus) * sin(HEEA_PA + HEEA_PA_DMinus)
    v_br = (HEEA_E + HEEA_E_DPlus)  * sin(HEEA_PA + HEEA_PA_DMinus)
    v_tl = (HEEA_E - HEEA_E_DMinus) * sin(HEEA_PA - HEEA_PA_DPlus)
    v_tr = (HEEA_E + HEEA_E_DPlus)  * sin(HEEA_PA - HEEA_PA_DPlus)

    ;Prep the image
    image_HEEA = MrLog(mean(HEEA_DEFlux, DIMENSION=3))
    
    ;Free some data
    undefine, HEEA_E, HEEA_E_DMinus, HEEA_E_DPlus
    undefine, HEEA_PA, HEEA_PA_DPlus, HEEA_PA_DMinus
    
;---------------------------------------------------------------------
; Select Time to Display /////////////////////////////////////////////
;---------------------------------------------------------------------
    it = 50

    ;LEEA
    x_bl = x_bl[*,*,it]
    x_br = x_br[*,*,it]
    x_tl = x_tl[*,*,it]
    x_tr = x_tr[*,*,it]
    y_bl = y_bl[*,*,it]
    y_br = y_br[*,*,it]
    y_tl = y_tl[*,*,it]
    y_tr = y_tr[*,*,it]
    image_LEEA = image_LEEA[*,*,it]
    
    ;HEEA
    u_bl = u_bl[*,*,it]
    u_br = u_br[*,*,it]
    u_tl = u_tl[*,*,it]
    u_tr = u_tr[*,*,it]
    v_bl = v_bl[*,*,it]
    v_br = v_br[*,*,it]
    v_tl = v_tl[*,*,it]
    v_tr = v_tr[*,*,it]
    image_HEEA = image_HEEA[*,*,it]
    
;---------------------------------------------------------------------
; Display the distribution ///////////////////////////////////////////
;---------------------------------------------------------------------
    ;Get a position and create the plot
    position = MrLayout([1,1], /SQUARE)
    cgPlot, [0,0], POSITION=positon, /NODATA, $
            XRANGE=[min(u, /NAN), max(u, /NAN)], XSTYLE=1, $
            YRANGE=[min(v, /NAN),  max(y_tr, /NAN)], YSTYLE=1
    
    zrange = [min([[[image_HEEA]], [[image_LEEA]]], MAX=zmax, /NAN), zmax]
    image_LEEA = bytscl(image_LEEA, MIN=zrange[0], MAX=zrange[1])
    image_HEEA = bytscl(image_HEEA, MIN=zrange[0], MAX=zrange[1])
    
    ;Get the current configuration
    device, GET_DECOMPOSED=this_decomposed
    tvlct, r, g, b, /GET
    
    ;Set the device and color table
    device, DECOMPOSED=0
    cgLoadCT, 13
    
    ;Paint the distribution function
    for i = 0, nEnergy - 2 do begin
        for j = 0, nPA - 1 do begin
            ;LEEA Bins
            xpoly = [x_bl[i,j], x_br[i,j], x_tr[i,j], x_tl[i,j], x_bl[i,j]]
            ypoly = [y_bl[i,j], y_br[i,j], y_tr[i,j], y_tl[i,j], y_bl[i,j]]
            
            ;HEEA Bins
            upoly = [u_bl[i,j], u_br[i,j], u_tr[i,j], u_tl[i,j], u_bl[i,j]]
            vpoly = [v_bl[i,j], v_br[i,j], v_tr[i,j], v_tl[i,j], v_bl[i,j]]
 
            ;LEEA distribution
            if total(finite(xpoly)) eq 5 && total(finite(ypoly)) eq 5 $
                then polyfill, xpoly, ypoly, /DATA, COLOR=image_LEEA[i,j]
                
            ;HEEA distribution
            if total(finite(upoly)) eq 5 && total(finite(vpoly)) eq 5 $
                then polyfill, upoly, vpoly, /DATA, COLOR=image_HEEA[i,j]
        endfor
    endfor
    
    ;Return to original settings
    tvlct, r, g, b
    device, DECOMPOSED=this_decomposed
end


;+
;   Draw the distribution function on a log scale.
;
;   Note:
;       - This depends on HEEA and LEEA having the same pitch angle bins.
;       - Bins do not make use of DELTA_PLUS and DELTA_MINUS
;       - Bin for an energy E has E at the bottom right corner. Adjacent energies make
;           up the other corners (see in-program notes).
;
; :Private:
;
; :Params:
;       HEEA_DEFlux:        in, required, type=fltarr
;                           HEEA differential energy flux data. A, E, and P represent
;                               the number of azimuthal, energy, and pitch-angle bins,
;                               respectively. T represents the number of records.
;       HEEA_E:             in, required, type=fltarr
;                           Energy for instrument bin centers.
;       HEEA_PA:            in, required, type=fltarr
;                           Pitch angle for instrument bin centers.
;       HEEA_PA_DMinus:     in, required, type=fltarr
;                           Lower bound of the instrument pitch angle bin.
;       LEEA_DEFlux:        in, required, type=fltarr
;                           LEEA differential energy flux data. A, E, and P represent
;                               the number of azimuthal, energy, and pitch-angle bins,
;                               respectively. T represents the number of records.
;       LEEA_E:             in, required, type=fltarr
;                           Energy for instrument bin centers.
;       LEEA_PA:            in, required, type=fltarr
;                           Pitch angle for instrument bin centers.
;       LEEA_PA_DMinus:     in, required, type=fltarr
;                           Lower bound of the instrument pitch angle bin.
;-
pro c_PEA_DistFn_log, HEEA_DEFlux, HEEA_E, HEEA_PA, HEEA_PA_DMinus, $
                              LEEA_DEFlux, LEEA_E, LEEA_PA, LEEA_PA_DMinus
    compile_opt strictarr
    
    catch, theError
    if theError ne 0 then begin
        catch, /cancel
        if n_elements(this_decomposed) gt 0 then device, DECOMPOSED=this_decomposed
        if n_elements(r) gt 0 then tvlct, r, g, b
        void = cgErrorMsg()
        return
    endif
    
;---------------------------------------------------------------------
; Prepare Log Output /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ; What should be done:
    ;   Draw a bin centered on E with corners:
    ;       1 = (E - dE) * cos(PA - dPA)
    ;       2 = (E + dE) * cos(PA - dPA)
    ;       3 = (E - dE) * sin(PA + dPA)
    ;       4 = (E + dE) * sin(PA + dPA)
    ;
    ; However, since energy is exponentially space, we have to take its log. Thusly,
    ; addition and subtraction of dE is problematic. As such, the energies themselves
    ; are used as the corners of the polygon grid. These are determined by
    ;       x = E * cos(PA - dPA)
    ;       y = E * sin(PA - dPA)
    ;
    ; where the polygon corners are now defined as
    ;       1: [x[i],   y[i]]
    ;       2: [x[i+1], y[i]]
    ;       3: [x[i+1], y[i+1]]
    ;       4: [x[i],   y[i+1]]
    ;
    ; To close the semi-circle, we must add an extra bin:
    ;       xf = E * cos(PA + dPA)
    ;       yf = E * sin(PA + dPA)
    ;       x = [x, xf]
    ;       y = [y, yf]
    ;
    nTime   = n_elements(LEEA_E[0,*])
    nEnergy = n_elements(LEEA_E[*,0])
    nPA     = n_elements(LEEA_PA[*,0])
    
    ;Add the extra pitch-angle bin and convert to radians
    LEEA_PA = [LEEA_PA - LEEA_PA_DMinus] * !DtoR
    HEEA_PA = [360 - HEEA_PA - HEEA_PA_DMinus] * !DtoR
    
    ;Create a loop in PA
    ;   - Join LEEA to HEEA at 180 degrees
    ;   - Join HEEA to LEEA at 380 degrees
    LEEA_PA = [LEEA_PA,      HEEA_PA[-1,*]]
    HEEA_PA = [LEEA_PA[0,*], HEEA_PA]
    nPA    += 1
    
    ;Create a loop in energy
    ;   - Repeat the last energy bin for HEEA and LEEA
    LEEA_E   = MrLog([LEEA_E,  LEEA_E[-1,*]])
    HEEA_E   = MrLog([HEEA_E,  HEEA_E[-1,*]])
    nEnergy += 1

    ;Give them the same dimensions
    LEEA_E  = rebin(reform(LEEA_E, nEnergy, 1, nTime), nEnergy, nPA, nTime)
    HEEA_E  = rebin(reform(HEEA_E, nEnergy, 1, nTime), nEnergy, nPA, nTime)
    LEEA_PA = rebin(reform(LEEA_PA, 1, nPA, nTime), nEnergy, nPA, nTime)
    HEEA_PA = rebin(reform(HEEA_PA, 1, nPA, nTime), nEnergy, nPA, nTime)
    
    ;Create the bin boundaries
    x = LEEA_E * cos(LEEA_PA)
    y = LEEA_E * sin(LEEA_PA)
    u = HEEA_E * cos(HEEA_PA)
    v = HEEA_E * sin(HEEA_PA)
    
    ;Prep the image
    ;   - Average over azimuth
    image_LEEA = MrLog(mean(LEEA_DEFlux, DIMENSION=3))
    image_HEEA = MrLog(mean(HEEA_DEFlux, DIMENSION=3))
    
    it = 0
    x = x[*,*,it]
    y = y[*,*,it]
    u = u[*,*,it]
    v = v[*,*,it]
    image_LEEA = image_LEEA[*,*,it]
    image_HEEA = image_HEEA[*,*,it]
    
;---------------------------------------------------------------------
; Display the distribution ///////////////////////////////////////////
;---------------------------------------------------------------------
    ;Get a position and create the plot
    position = MrLayout([1,1], /SQUARE)
    cgPlot, [0,0], POSITION=positon, /NODATA, $
            XRANGE=[min(u, /NAN), max(u, /NAN)], XSTYLE=1, $
            YRANGE=[min(v, /NAN), max(y, /NAN)], YSTYLE=1
    
    zrange = [min([[[image_HEEA]], [[image_LEEA]]], MAX=zmax, /NAN), zmax]
    image_LEEA = bytscl(image_LEEA, MIN=zrange[0], MAX=zrange[1])
    image_HEEA = bytscl(image_HEEA, MIN=zrange[0], MAX=zrange[1])
    
    ;Get the current configuration
    device, GET_DECOMPOSED=this_decomposed
    tvlct, r, g, b, /GET
    
    ;Set the device and color table
    device, DECOMPOSED=0
    cgLoadCT, 13
    
    ;Paint the distribution function
    for i = 0, nEnergy - 2 do begin
        for j = 0, nPA - 2 do begin
            ;LEEA Bins
            xpoly = [x[i,j], x[i+1,j], x[i+1,j+1], x[i,j+1], x[i,j]]
            ypoly = [y[i,j], y[i+1,j], y[i+1,j+1], y[i,j+1], y[i,j]]
            
            ;HEEA Bins
            upoly = [u[i,j], u[i+1,j], u[i+1,j+1], u[i,j+1], u[i,j]]
            vpoly = [v[i,j], v[i+1,j], v[i+1,j+1], v[i,j+1], v[i,j]]
 
            ;LEEA distribution
            if total(finite(xpoly)) eq 5 && total(finite(ypoly)) eq 5 $
                then polyfill, xpoly, ypoly, /DATA, COLOR=image_LEEA[i,j]
                
            ;HEEA distribution
            if total(finite(upoly)) eq 5 && total(finite(vpoly)) eq 5 $
                then polyfill, upoly, vpoly, /DATA, COLOR=image_HEEA[i,j]
        endfor
    endfor
    
    ;Return to original settings
    tvlct, r, g, b
    device, DECOMPOSED=this_decomposed
end


;+
;   Create a distribution function out of PEACE differential energy flux data.
;-
pro c_PEA_DistFn, it
    compile_opt strictarr
    
    catch, theError
    if theError ne 0 then begin
        catch, /cancel
        if obj_valid(oFile) then obj_destroy, oFile
        void = cgErrorMsg()
        return
    endif

    ;Define inputs
    directory = '/Users/argall/Desktop/data_dump/'
    sc        = 1
    sDateTime = '2005-01-25T14:47:00Z'
    eDateTime = '2005-01-25T14:52:00Z'
    velocity  = 0
    
    ;Convert to string
    _sc = string(sc, FORMAT='(i1)')

    ;Create the file name
    filename = filepath('C' + _sc + '_CP_PEA_PITCH_FULL_DEFlux__20050125_144700_20050125_145200_V111128.cdf', ROOT_DIR=directory)

;---------------------------------------------------------------------
; Read Data //////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Create variable names
    LEEA_vname = 'Data_LEEA__C' + _sc + '_CP_PEA_PITCH_FULL_DEFlux'
    HEEA_vname = 'Data_HEEA__C' + _sc + '_CP_PEA_PITCH_FULL_DEFlux'
    
    ;Read data and its support data
    oFile = MrFile(filename, STIME=sDateTime, ETIME=eDateTime, /OPEN)
    oFile -> Read, [LEEA_vname, HEEA_vname], /SUPPORT_DATA
    
    ;Get the data
    LEEA_DEFlux = oFile -> GetVarData(LEEA_vname, DEPEND_0=time, DEPEND_1=LEEA_AZ, $
                                      DEPEND_2=LEEA_PA, DEPEND_3=LEEA_E)
    HEEA_DEFlux = oFile -> GetVarData(HEEA_vname, DEPEND_1=HEEA_AZ, $
                                      DEPEND_2=HEEA_PA, DEPEND_3=HEEA_E)
    
    ;LEEA -- Get delta plus/minus
    dep2_vname     = oFile -> GetVarAttrValue(LEEA_vname, 'DEPEND_2')
    dep3_vname     = oFile -> GetVarAttrValue(LEEA_vname, 'DEPEND_3')
    LEEA_PA_DPlus  = oFile -> GetVarAttrValue(dep2_vname, 'DELTA_PLUS',  /FOLLOW_PTRS)
    LEEA_PA_DMinus = oFile -> GetVarAttrValue(dep2_vname, 'DELTA_MINUS', /FOLLOW_PTRS)
    LEEA_E_DPlus   = oFile -> GetVarAttrValue(dep3_vname, 'DELTA_PLUS',  /FOLLOW_PTRS)
    LEEA_E_DMinus  = oFile -> GetVarAttrValue(dep3_vname, 'DELTA_MINUS', /FOLLOW_PTRS)
    
    ;HEEA -- Get delta plus/minus
    dep2_vname     = oFile -> GetVarAttrValue(HEEA_vname, 'DEPEND_2')
    dep3_vname     = oFile -> GetVarAttrValue(HEEA_vname, 'DEPEND_3')
    HEEA_PA_DPlus  = oFile -> GetVarAttrValue(dep2_vname, 'DELTA_PLUS',  /FOLLOW_PTRS)
    HEEA_PA_DMinus = oFile -> GetVarAttrValue(dep2_vname, 'DELTA_MINUS', /FOLLOW_PTRS)
    HEEA_E_DPlus   = oFile -> GetVarAttrValue(dep3_vname, 'DELTA_PLUS',  /FOLLOW_PTRS)
    HEEA_E_DMinus  = oFile -> GetVarAttrValue(dep3_vname, 'DELTA_MINUS', /FOLLOW_PTRS)
    
    ;Replace fill values
    for i = 0, oFile.nVars do oFile[0] -> ReplaceFillVal
    obj_destroy, oFile

    if velocity then begin
        eMass  = 9.1E-31
        charge = 1.6e-19
        coef   = (2*charge) / (eMass * 1.0e14)     ;1.0e7 to make units 10^4
        
        ;Convert to velocity space
        LEEA_E        = sqrt(coef * LEEA_E)
        LEEA_E_DPlus  = sqrt(coef * LEEA_E_DPlus)
        LEEA_E_DMinus = sqrt(coef * LEEA_E_DMinus)
    endif else begin
        ;Convert to log energy
        LEEA_E        = MrLog(LEEA_E)
        LEEA_E_DPlus  = MrLog(LEEA_E_DPlus)
        LEEA_E_DMinus = MrLog(LEEA_E_DMinus)
    endelse
    
    log_output = 1
    if log_output then begin
        test_pa_distribution_log, HEEA_DEFlux, HEEA_E, HEEA_PA, HEEA_PA_DMinus, $
                                  LEEA_DEFlux, LEEA_E, LEEA_PA, LEEA_PA_DMinus
    endif else begin
        test_pa_distribution_linear, HEEA_DEFlux, HEEA_E,  HEEA_E_DPlus,  HEEA_E_DMinus, $
                                                  HEEA_PA, HEEA_PA_DPlus, HEEA_PA_DMinus, $
                                     LEEA_DEFlux, LEEA_E,  LEEA_E_DPlus,  LEEA_E_DMinus, $
                                                  LEEA_PA, LEEA_PA_DPlus, LEEA_PA_DMinus
    endelse
end