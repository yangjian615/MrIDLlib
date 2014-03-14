; docformat = 'rst'
;
; NAME:
;       MrPolPlot
;
; PURPOSE:
;+
;       Calculate polarization parameters for a set of data and make plots of the results. 
;
; :Categories:
;   Plot Utility
;
; :Params:
;       DATA:           in, required, type=2D numeric
;                       The data for which a spectrogram plot is to be made.
;       NFFT:           in, optional, type=int, default=1024
;                       The number of points to use per FFT
;       DT:             in, optional, type=float. default=1.0
;                       The time between data samples. If not present, unit spacing
;                           is assumed.
;       NSHIFT:         in, optional, type=int. default=`NFFT`/2
;                       The number of points to shift ahead after each FFT.
;
; :Keywords:
;       BACKGROUND:     in, optional, type=float
;                       A value for `INTENSITY` below which everything is to be considered
;                           background noise. These pixels associated with these values
;                           will be "erased" from the `POLARIZATION`, `COHERENCY`,
;                           `KDOTB_ANGLE` and `ELLIPTICITY` images.
;       DIMENSION:      in, optional, type=int, default=2
;                       The dimension over which to take the spectra. If 0, then the FFT
;                           is taken over all dimensions (this is the default). As an
;                           example, say DATA is an N1xN2 array. If DIMENSION=2, then
;                           the N1 FFTs will be taken along each DATA[i,*]
;       OUTPUT_FNAME:   in, optional, type=string, default=''
;                       The name of a file in which the spectragram will be saved. If
;                           given, a null object will be returned.
;       _REF_EXTRA:     in, optional, type=structure
;                       Any keyword accepted by MrPolarization is also accepted via
;                           keyword inheritance.
;
; :Returns:
;       IMARR:          out, required, type=object/objarr
;                       MrImage objects of each spectra being made.
;
; :Uses:
;   Uses the following external programs::
;       error_message.pro (Coyote Graphics)
;       undefine (Coyote Graphics)
;       MrWindow
;       MrPolarization
;       cgLoadCT (Coyote Graphics)
;       MrCreateCT
;       weColorbar.pro
;       time_labels.pro
;                           
; :Author:
;    Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Copyright 2013 by Matthew Argall
;
; :History:
;   Modification History::
;       10/04/2013  -   Written by Matthew Argall
;       10/09/2013  -   Added the BACKGROUND and OUTPUT_FNAME keywords. Removed keywords
;                           CBARR and CBKWDS. - MRA
;       2013-28-10  -   Remove BACKGROUND from polarization as well. - MRA
;-
function MrPolPlot, data, nfft, dt, nshift, $
BACKGROUND = background, $
DIMENSION = dimension, $
DRAW = draw, $
OREF = oRef, $
OUTPUT_FNAME = output_fname, $
RANGE = range, $
POLARIZATION = polarization, $
INTENSITY = intensity, $
COHERENCY = coherency, $
KDOTB_ANGLE = kdotb_angle, $
ELLIPTICITY = ellipticity, $
YLOG = ylog, $
_REF_EXTRA = extra
    compile_opt idl2

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        ;Plotting Window
        if obj_valid(polWin) then obj_destroy, polWin
        
        ;Images
        if (max(obj_valid(imArr)) eq 1) then obj_destroy, imArr
        
        ;Colorbars
        if (max(obj_valid(cbArr)) eq 1) then obj_destroy, cbArr
            
        void = cgErrorMsg()
        return, MrNull('ObjRef')
    endif
    
    dims = size(data, /DIMENSION)

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Defaults -- NFFT, NSHIFT, DT, and DIMENSION will be handled by MrPSD.
    if n_elements(t0) eq 0 then t0 = 0.0
    if n_elements(output_fname) eq 0 then output_fname = ''
    oRef = keyword_set(oRef)
    if n_elements(draw) eq 0 then draw = 1 else draw = keyword_set(draw)
    
    ;Save to file?
    if output_fname ne '' then begin
        display = 0
        oRef = 0
        add = 1
    
    ;If not...
    endif else begin
        if oRef eq 0 then display = draw else display = 0
        if oRef eq 0 then add = 1 else add = 0
    endelse
    
    ;Which data products will be kept?
    if (oRef eq 0) then begin
        coherency    = keyword_set(coherency)
        ellipticity  = keyword_set(ellipticity)
        intensity    = keyword_set(intensity)
        kdotb_angle  = keyword_set(kdotb_angle)
        polarization = keyword_set(polarization)
    endif else begin
        coherency    = arg_present(coherency)
        ellipticity  = arg_present(ellipticity)
        intensity    = arg_present(intensity)
        kdotb_angle  = arg_present(kdotb_angle)
        polarization = arg_present(polarization)
    endelse
    
    ;How many images will we make?
    nImages = coherency + ellipticity + intensity + kdotb_angle + polarization
    
    ;If nothing was selected, plot them all
    if (nImages eq 0) then begin
        coherency = 1
        ellipticity = 1
        intensity = 1
        kdotb_angle = 1
        polarization = 1
        nImages = 5
    endif
    
    ;Create a window. Leave room for a colorbar.
    polWin = MrWindow(BUILD=0, XMARGIN=[10,15], YSIZE=600, DISPLAY=display)
    
;-----------------------------------------------------
;Calculate PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Compute the spectrogram
    pzation_data = MrPolarization(data, nfft, dt, nshift, $
                                  DIMENSION = dimension, $
                                  FREQUENCIES = frequencies, $
                                  TIME = time, $
                                  ELLIPTICITY = ellipticity_data, $
                                  INTENSITY = intensity_data, $
                                  KDOTB_ANGLE = kdotb_angle_data, $
                                  COHERENCY = coherency_data, $
                                 _EXTRA = extra)
    
    ;Log Intensity
    intensity_data = MrLog(intensity_data)
    
    ;Remove background signal?
    if n_elements(background) gt 0 $
        then iMissing = where(intensity_data le background, nMissing) $
        else nMissing = 0

;-----------------------------------------------------
;INTENSITY \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Check for the intensity
    if (intensity eq 1) then begin
    
        ;Create the image
        IntImage = polWin -> Image(intensity_data, time, frequencies, $
                                   /AXES, $
                                   /NAN, $
                                   CTINDEX=13, $
                                   RANGE=range, $
                                   TITLE='Intensity', $
                                   XTICKFORMAT='time_labels', $
                                   XTITLE='Time (UT)', $
                                   YTITLE='Frequency (Hz)', $
                                   YLOG=ylog, $
                                   DRAW=0, $
                                   ADD=add)
        
        IntCB = polWin -> Colorbar(TITLE='Log Intensity!C(Units^2 * Hz)', $
                                   TARGET=IntImage, $
                                   DRAW=0, $
                                   ADD=add)
        
        ;Return the object references?
        if (add eq 0) then intensity = [IntImage, IntCB]
    endif else undefine, intensity_data

;-----------------------------------------------------
;POLARIZATION \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if (polarization eq 1) then begin
        ;Remove the background
        if nMissing gt 0 then pzation_data[iMissing] = !values.f_nan
        
        ;Reverse the Black->White colortable
        cgLoadCT, 0, RGB_TABLE=ctBW, /REVERSE
        
        PolIm = polWin -> Image(pzation_data, time, frequencies, $
                                /AXES, $
                                /NAN, $
                                MISSING_COLOR='Antique White', $
                                PALETTE=ctBW, $
                                TITLE='Percent Polarization', $
                                XTICKFORMAT='time_labels', $
                                XTITLE='Time (UT)', $
                                YTITLE='Frequency (Hz)', $
                                YLOG=ylog, $
                                DRAW=0, $
                                ADD=add)

        PolCB = polWin -> Colorbar(TITLE='% Polarization', $
                                   TARGET=PolIm, $
                                   DRAW=0, $
                                   ADD=add, $
                                  _EXTRA=cbkwds)
        
        ;Return the object references?
        if (add eq 0) then polarization = [PolIm, PolCB]
    endif else undefine, pzation_data

;-----------------------------------------------------
;ELLIPTICITY \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if (ellipticity eq 1) then begin
        ;Remove the background
        if nMissing gt 0 then ellipticity_data[iMissing] = !values.f_nan
        
        ;Create a color table that transitions linearly from blue to white to red.
        palette = MrCreateCT(/RWB, /REVERSE)

        ;Create the image
        EllIm = polWin -> Image(ellipticity_data, time, frequencies, $
                                /AXES, $
                                /NAN, $
                                MISSING_COLOR='Antique White', $
                                PALETTE=palette, $
                                TITLE='Ellipticity', $
                                XTICKFORMAT='time_labels', $
                                XTITLE='Time (UT)', $
                                YTITLE='Frequency (Hz)', $
                                YLOG=ylog, $
                                DRAW=0, $
                                ADD=add)

        EllCB = polWin -> Colorbar(TITLE='Ellipticity', $
                                   TARGET=EllIm, $
                                   DRAW=0, $
                                   ADD=add, $
                                  _EXTRA=cbkwds)
        
        ;Return the object references?
        if (add eq 0) then ellipticity = [EllIm, EllCB]
    endif else undefine, ellipticity_data

;-----------------------------------------------------
;K-DOT-B ANGLE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if (kdotb_angle eq 1) then begin
        ;Remove the background?
        if nMissing gt 0 then kdotb_angle_data[iMissing] = !values.f_nan
        
        ;Reverse the Black->White colortable
        cgLoadCT, 0, RGB_TABLE=ctBW, /REVERSE
        
        ;Create the image
        kdbIm= polWin -> Image(kdotb_angle_data*!radeg, time, frequencies, $
                               /AXES, $
                               /NAN, $
                               MISSING_COLOR='Antique White', $
                               PALETTE=ctBW, $
                               TITLE='Angle Between k and B', $
                               XTICKFORMAT='time_labels', $
                               XTITLE='Time (UT)', $
                               YTITLE='Frequency (Hz)', $
                               YLOG=ylog, $
                               DRAW=0, $
                               ADD=add)
        
        kdbCB = polWin -> Colorbar(TITLE='k dot B!C(Deg)', $
                                   TARGET=kdbIm, $
                                   DRAW=0, $
                                   ADD=add, $
                                  _EXTRA=cbkwds)
        
        ;Return the object references?
        if (add eq 0) then kdotb_angle = [kdbIm, kdbCB]
    endif else undefine, kdotb_angle_data

;-----------------------------------------------------
;COHERENCY \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Check for the coherency
    if (coherency eq 1) then begin
        ;Remove the background?
        if nMissing gt 0 then coherency_data[iMissing] = !values.f_nan
        
        ;Reverse the Black->White colortable
        cgLoadCT, 0, RGB_TABLE=ctBW, /REVERSE
        
        ;Create the image
        CohIm = polWin -> Image(coherency_data, time, frequencies, $
                                /AXES, $
                                /NAN, $
                                MISSING_COLOR='Antique White', $
                                PALETTE=ctBW, $
                                TITLE='Coherency', $
                                XTICKFORMAT='time_labels', $
                                XTITLE='Time (UT)', $
                                YTITLE='Frequency (Hz)', $
                                YLOG=ylog, $
                                DRAW=0, $
                                ADD=add)
        
        CohCB = polWin -> Colorbar(TITLE='Coherency', $
                                   TARGET=CohIm, $
                                   DRAW=0, $
                                   ADD=add, $
                                  _EXTRA=cbkwds)
        
        ;Return the object references?
        if (add eq 0) then coherency = [CohIm, CohCB]
    endif else undefine, coherency_data
    
;-----------------------------------------------------
;Return Window or Objects? \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Return the object references?
    if (oRef eq 1) then begin

        ;Destroy the window we created
        obj_destroy, polWin
        return, obj_new()
    
    ;Return a window
    endif else if (oRef eq 0) and (output_fname eq '') then begin
        polWin -> SetProperty, YSIZE=nImages*120
        if keyword_set(draw) then polWin -> RealizeGUI
        return, polWin
        
    ;Output the plot to a file?
    endif else if (output_fname ne '') then begin
        ;The plots need to be drawn first so that the TVRD can grab the image.
        polWin -> Draw
        polWin -> Output, output_fname
        obj_destroy, polWin
        return, obj_new()
    
    endif else message, 'Invalid keyword combination.'
end