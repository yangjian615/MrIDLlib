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
;       2014-08-17  -   Added the CURRENT keyword. Updated to work with the current
;                           version of MrWindow. - MRA
;       2014-10-05  -   Set range of images to full range of quantity. Scale images. - MRA
;-
function MrPolPlot, data, nfft, dt, nshift, $
BACKGROUND = background, $
CURRENT = current, $
DIMENSION = dimension, $
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
            
        void = cgErrorMsg()
        return, MrNull('ObjRef')
    endif
    
    dims = size(data, /DIMENSION)

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Defaults -- NFFT, NSHIFT, DT, and DIMENSION will be handled by MrPSD.
    current      = keyword_set(current)
    coherency    = keyword_set(coherency)
    ellipticity  = keyword_set(ellipticity)
    intensity    = keyword_set(intensity)
    kdotb_angle  = keyword_set(kdotb_angle)
    polarization = keyword_set(polarization)
    if n_elements(t0)           eq 0 then t0 = 0.0
    if n_elements(output_fname) eq 0 then output_fname = ''
    if n_elements(draw)         eq 0 then draw = 1 else draw = keyword_set(draw)    
    
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
    
    ;Create a window.
    refresh_in = 1
    if current then begin
        polWin     = GetMrWindows(/CURRENT)
        refresh_in = polWin.REFRESH
        polWin    -> Refresh, /DISABLE
    endif else begin
        BUFFER = output_fname ne ''
        polWin = MrWindow(OXMARGIN=[10,15], YSIZE=600, BUFFER=buffer, REFRESH=0)
    endelse
    
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
        IntImage = MrImage(intensity_data, time, frequencies, $
                           /AXES, $
                           /CURRENT, $
                           /LOG, $
                           /NAN, $
                           /SCALE, $
                           CTINDEX       = 13, $
                           NAME          = 'Intensity', $
                           MISSING_COLOR = 'Antique White', $
                           RANGE         = range, $
                           TITLE         = 'Intensity', $
                           XTICKFORMAT   = 'time_labels', $
                           XTITLE        = 'Time (UT)', $
                           YTITLE        = 'Frequency (Hz)', $
                           YLOG          = ylog)

        IntCB = MrColorbar(/CURRENT, $
                           NAME      = 'CB: Intensity', $
                           TITLE     = 'Intensity!C(Units$\up2$/Hz)', $
                           TARGET    = IntImage, $
                           WIDTH     = 1.5)
    endif
    intensity_data = !Null

;-----------------------------------------------------
;POLARIZATION \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if (polarization eq 1) then begin
        ;Remove the background
        if nMissing gt 0 then pzation_data[iMissing] = !values.f_nan
        
        ;Reverse the Black->White colortable
        cgLoadCT, 0, RGB_TABLE=ctBW, /REVERSE
        PolIm = MrImage(pzation_data, time, frequencies, $
                        /AXES, $
                        /CURRENT, $
                        /NAN, $
                        /SCALE, $
                        NAME          = 'Polarization', $
                        MISSING_COLOR = 'Antique White', $
                        PALETTE       = ctBW, $
                        RANGE         = [0, 1], $
                        TITLE         = 'Percent Polarization', $
                        XTICKFORMAT   = 'time_labels', $
                        XTITLE        = 'Time (UT)', $
                        YTITLE        = 'Frequency (Hz)', $
                        YLOG          = ylog)

        PolCB = MrColorbar(/CURRENT, $
                           NAME          = 'CB: Polarization', $
                           TITLE         = '% Polarization', $
                           TARGET        = PolIm, $
                           YMINOR        = 5, $
                           YTICKINTERVAL = 0.5, $
                           WIDTH         = 1.5, $
                          _EXTRA         = cbkwds)
    endif
    pzation_data = !Null

;-----------------------------------------------------
;ELLIPTICITY \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if (ellipticity eq 1) then begin
        ;Remove the background
        if nMissing gt 0 then ellipticity_data[iMissing] = !values.f_nan
        
        ;Create a color table that transitions linearly from blue to white to red.
        palette = MrCreateCT(/RWB, /REVERSE)

        ;Create the image
        EllIm = MrImage(ellipticity_data, time, frequencies, $
                        /AXES, $
                        /CURRENT, $
                        /NAN, $
                        /SCALE, $
                        NAME          = 'Ellipticity', $
                        MISSING_COLOR = 'Antique White', $
                        PALETTE       = palette, $
                        RANGE         = [-1,1], $
                        TITLE         = 'Ellipticity', $
                        XTICKFORMAT   = 'time_labels', $
                        XTITLE        = 'Time (UT)', $
                        YTITLE        = 'Frequency (Hz)', $
                        YLOG          = ylog)

        EllCB = MrColorbar(/CURRENT, $
                           NAME          = 'CB: Ellipticity', $
                           TITLE         = 'Ellipticity', $
                           TARGET        = EllIm, $
                           YMINOR        = 5, $
                           YTICKINTERVAL = 1, $
                           WIDTH         = 1.5, $
                          _EXTRA         = cbkwds)
    endif
    ellipticity_data = !Null

;-----------------------------------------------------
;K-DOT-B ANGLE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if (kdotb_angle eq 1) then begin
        ;Remove the background?
        if nMissing gt 0 then kdotb_angle_data[iMissing] = !values.f_nan
        
        ;Reverse the Black->White colortable
        cgLoadCT, 0, RGB_TABLE=ctBW, /REVERSE
        
        ;Create the image
        kdbIm= MrImage(kdotb_angle_data*!radeg, time, frequencies, $
                       /AXES, $
                       /CURRENT, $
                       /NAN, $
                       /SCALE, $
                       NAME          = 'k dot B', $
                       MISSING_COLOR = 'Antique White', $
                       PALETTE       = ctBW, $
                       RANGE         = [0,90], $
                       TITLE         = 'Angle Between k and B', $
                       XTICKFORMAT   = 'time_labels', $
                       XTITLE        = 'Time (UT)', $
                       YTITLE        = 'Frequency (Hz)', $
                       YLOG          = ylog)
        
        kdbCB = MrColorbar(/CURRENT, $
                           NAME          = 'CB: k dot B', $
                           TITLE         = 'k dot B!C(Deg)', $
                           TARGET        = kdbIm, $
                           YMINOR        = 2, $
                           YTICKINTERVAL = 30, $
                           WIDTH         = 1.5, $
                          _EXTRA         = cbkwds)
    endif
    kdotb_angle_data = !Null

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
        CohIm = MrImage(coherency_data, time, frequencies, $
                        /AXES, $
                        /CURRENT, $
                        /NAN, $
                        /SCALE, $
                        NAME          = 'Coherency', $
                        MISSING_COLOR = 'Antique White', $
                        PALETTE       = ctBW, $
                        RANGE         = [0,1], $
                        TITLE         = 'Coherency', $
                        XTICKFORMAT   = 'time_labels', $
                        XTITLE        = 'Time (UT)', $
                        YTITLE        = 'Frequency (Hz)', $
                        YLOG          = ylog)
        
        CohCB = MrColorbar(/CURRENT, $
                           NAME          = 'CB: Coherency', $
                           TITLE         = 'Coherency', $
                           TARGET        = CohIm, $
                           YMINOR        = 5, $
                           YTICKINTERVAL = 0.5, $
                           WIDTH         = 1.5, $
                          _EXTRA         = cbkwds)
    endif
    coherency_data = !Null
    
;-----------------------------------------------------
;Return Window or Objects? \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Return a window
    if (output_fname eq '') then begin

        polWin -> SetProperty, YSIZE=nImages*120
        polWin -> Refresh, DISABLE=~refresh_in
        return, polWin
        
    ;Output the plot to a file?
    endif else begin
        ;The plots need to be drawn first so that the TVRD can grab the image.
        polWin -> Refresh
        polWin -> Save, output_fname
        obj_destroy, polWin
        return, obj_new()
    endelse
end