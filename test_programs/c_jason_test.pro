; docformat = 'rst'
;
; NAME:
;       C_eBulkHeating
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
;   The purpose of this program make an overview plot for visualizing flux tranfer events
;   using data from the Cluster mission.
;
;   Plots made, and their names for retrieval purposes are::
;       Display Window          -   'PEA Flux'
;       HEEA Energy Flux        -   'E HEEA'
;       HEEA Pitch Angle Flux   -   'PA HEEA'
;       LEEA Energy Flux        -   'E LEEA'
;       LEEA Pitch Angle Flux   -   'PA LEEA'
;       SPIN Energy Flux        -   'E SPIN'
;       SPIN Pitch Angle Flux   -   'PA SPIN'
;       Associated Colorbars    -   'CB: ' + name
;
; :Categories:
;       Cluster
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
;       DIRECTORY:          in, optional, type=string, default=PWD()
;                           Directory in which to look for data.
;       FILENAME:           in, optional, type=string, default=''
;                           Name of the file in which to save the graphics. If provided,
;                               the widget will not be realized.
;       LMN_FRAME:          in, optional, type=fltarr(3\,3), default=identity(3)
;                           A coordinate transformation matrix for rotating into the
;                               minimum variance frame.
;
; :Returns:
;       MYWIN:              out, required, type=object
;                           Object reference to the plot containing the magnitude of the
;                               magnetic field, ion density, spacecraft potential, and
;                               ion temperature.
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
;       09/07/2013  -   Written by Matthew Argall
;       09/17/2013  -   Added the _REF_EXTRA keyword
;-
function c_jason, sc, sDateTime, eDateTime, $
DIRECTORY = directory, $
FILENAME = filename, $
LMN_FRAME = lmn_frame
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMSG()

        ;Plotting window
        if obj_valid(eBulk_win) then obj_destroy, eBulk_win
        if obj_valid(trash_win) then obj_destroy, trash_win
        
        return, obj_new()
    endif
    
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Defaults
    if n_elements(directory) eq 0 then void = cgRootName(DIRECTORY=directory)
    if n_elements(filename) eq 0 then filename = ''
    if filename eq '' $
        then buffer = 0 $
        else buffer = 1
    
    ;Make sure SC is a string
    if size(sc, /TYPE) ne 7 then sc = string(sc, FORMAT='(i1)')
    dissectdatetime, sDateTime, sDate, sTime, /SEPARATE
    dissectdatetime, eDateTime, eDate, eTime, /SEPARATE
    
    ;Minimum variance coordinates?
    if n_elements(lmn_frame) eq 0 then begin
        doLMN = 1
        components = ['X', 'Y', 'Z']
        system = '(GSE)'
    endif else begin
        doLMN = 0
        components = ['N', 'M', 'L']
        system = '(LMN)'
    endelse
    
    ;Set the x-axis range based on the input times.
    xrange = hms_to_ssm([sTime, eTime])
    if n_params() eq 6 then xrange = time_to_distance(xrange, vMP, di, ti)

    ;Create the window. Build it, but do not realize it yet.
    MyWin = MrWindow(XSIZE=500, YSIZE=700, OXMARGIN=[12,15], YGAP=0, REFRESH=0, $
                     BUFFER=buffer, NAME='e- Bulk Heating')

;---------------------------------------------------------------------
;Create all of the Plots /////////////////////////////////////////////
;---------------------------------------------------------------------

    ;-----------------
    ; Mangetic Field |
    ;-----------------
    void = c_FGM_Full(sc, sDateTime, eDateTime, /B_VEC, /CURRENT, $
                      DIRECTORY=directory, LMN_FRAME=lmn_frame)

    ;If successful
    Bvec_plot = MyWin['Bxyz']
    if obj_valid(Bvec_plot) then begin
        ;Create a legend
        Bvec_legend = MrLegend(LOCATION=8, $
                               TARGET=Bvec_plot, $
                               TITLE=components, $
                               COLOR=color, $
                               LENGTH=0)
    endif

    ;-----------------------
    ; Spacecraft Potential |
    ;-----------------------
;    !Null = c_efw(sc, sDateTime, eDateTime, /CURRENT, /SCP, DIRECTORY=directory, $
;                  LMN_FRAME=lmn_frame)


    ;-------------------------------------
    ; Ion Density, Velocity, Temperature |
    ;-------------------------------------
    !Null = c_ion_moments(sc, sDateTime, eDateTime, /CURRENT, /CODIF_H1, $
                          /VELOCITY, DIRECTORY=directory, $
                          LMN_FRAME=lmn_frame)

    ; -----------------------------------------
    ; Electron Temperature, perp and parallel |
    ; -----------------------------------------
;    !Null = c_pea_moments(sc, sDateTime, eDateTime, /CURRENT, /T_PAR, /T_PERP,$
;                          DIRECTORY=directory,LMN_FRAME=lmn_frame)

    ;------------
    ; EFW E DSI |
    ;------------
;    !Null = c_efw(sc, sDateTime, eDateTime, /CURRENT, /E_DSI, DIRECTORY=directory, $
;                  LMN_FRAME=lmn_frame)
        
    ;---------------------------
    ; PEA Electron Spectrogram |
    ;---------------------------
;    void = c_pea_flux(sc, sDateTime, eDateTime, /CURRENT, $
;                      /E_SPIN, DIRECTORY=directory)
;    
;    Ee_image = MyWin['E SPIN']
;    if obj_valid(Ee_image) then begin
;        ;Find NaNs in the image.
;        Ee_image -> GetData, image
;        iFinite = where(finite(image) eq 0, nFinite)
;                
;        ;Sum over all pitch angles. Replace non-counts with 0 counts
;        if nFinite gt 0 then image[iFinite] = 0.0
;        image = total(image, 3)
;        range = [min(image, max=imMax, /NAN), imMax]
;        
;        ;Replace the data and set some properties
;        Ee_image -> SetData, image
;        Ee_image.range = range
;    endif
        
;    ;-----------------------------
;    ; RAPID Electron Energy Flux |
;    ;-----------------------------
;    void = c_rapid(sc, sDateTime, eDateTime, /CURRENT, /EFLUX, DIRECTORY=directory)
        
;    ;---------------------------
;    ; CODIF Proton Energy Flux |
;    ;---------------------------
;    void = c_ion_flux(sc, sDateTime, eDateTime, /CURRENT, /CODIF_H1, /ENERGY, DIRECTORY=directory)

;    ;---------------------------
;    ; CODIF Oxygen Energy Flux |
;    ;---------------------------
;    void = c_ion_flux(sc, sDateTime, eDateTime, /CURRENT, /CODIF_O1, /ENERGY, DIRECTORY=directory)

;---------------------------------------------------------------------
;Prepare for Display /////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Get the object references of all of the plots
    allPlots = MyWin -> Get(/ALL, ISA=['MRIMAGE', 'MRPLOT'], COUNT=nPlots)
    for i = 0, nPlots-1 do begin
        allPlots[i] -> SetProperty, XRANGE=xrange, XTICKFORMAT='(a1)', XTITLE='', TITLE=''
    endfor

; Adds titles back to plots (JRS)
;    MyWin['Ion E Flux'] -> SetProperty, TITLE='', XRANGE=xrange, $
;                                        XTITLE='UT (HH:MM:SS)', XTICKFORMAT='time_labels'
;    MyWin['Bxyz'] -> SetProperty, TITLE='Event 1 - 2001 Mar 31 - C4 - 11:35UT to 11:55UT';, XRANGE=xrange, $
                                        ;XTITLE='UT (HH:MM:SS)', XTICKFORMAT='time_labels'
 ;   MyWin['ni'].ylog = 1
    ;MyWin['Ion E Flux'].ylog = 0
    ;MyWin['EFW Spacecraft Potential'].ylog = 1
;---------------------------------------------------------------------
;Return //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Bind axes and create the GUI
    allPlots = MyWin -> Get(/ALL, ISA=['MrPlot', 'MrImage'], COUNT=nObj)
    if nObj gt 1 then MyWin -> Bind, allPlots, /XAXIS
    MyWin -> Refresh

    return, MyWin
end

