; docformat = 'rst'
;
; NAME:
;       MrSpectrogram
;
; PURPOSE:
;+
;       Create a spectrogram plot from a data series. 
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
;       CBARR:          out, optional, type=Obj_Arr()
;                       A named variable into which an array of colorbar objects will be
;                           returned.
;       CBKWDS:         in, optional, type=structure
;                       A structure of keyword-value pairs accepted by weColorbar. Used
;                           only if `CBARR` is present.
;       CBTITLE:        in, optional, type=StrArr(), default='PSD!C(units^2 * Hz)'
;                       An easier way of specifying the titles of each colorbar. Used
;                           only if `CBARR` is present.
;       DIMENSION:      in, optional, type=int, default=2
;                       The dimension over which to take the spectra. If 0, then the FFT
;                           is taken over all dimensions (this is the default). As an
;                           example, say DATA is an N1xN2 array. If DIMENSION=2, then
;                           the N1 FFTs will be taken along each DATA[i,*]
;       OUTPUT_FNAME:   in, optional, type=string, default=''
;                       The name of a file in which the spectragram will be saved. If
;                           given, a null object will be returned.
;       T0:             in, optional, type=float, default=0.0
;                       The time offset at which `DATA` begins, in seconds.
;       TITLE:          in, optional, type=strarr(), default='Power Spectral Density'
;                       An easier way of specifying the title for each spectral plot.
;       _REF_EXTRA:     in, optional, type=structure
;                       Any keyword accepted by MrPSD.pro is alsomaccepted for keyword
;                           inheritance.
;
; :Returns:
;       IMARR:          out, required, type=object/objarr
;                       MrImage graphics objects of each spectra being made.
;
; :Uses:
;   Uses the following external programs::
;       error_message.pro (Coyote Graphics)
;       isMember.pro
;       MrPSD.pro
;       MrImage.pro
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
;       08/12/2013  -   Written by Matthew Argall
;       08/13/2013  -   Added the CBKWDS keyword. - MRA
;       10/01/2013  -   Added CBTITLE, TIME, TITLE, and FREQUENCIES keywords. - MRA
;       10/04/2013  -   Renamed from MrSpectralPlot to MrSpectragram. Removed the TIME
;                           and FREQUENCIES keywords, since the intention of this program
;                           is to create a plot. - MRA
;       10/09/2013  -   Added the OUTPUT_FNAME keyword. - MRA
;       2013/12/24  -   Use Mr-function graphics. - MRA
;-
function MrSpectrogram, data, nfft, dt, nshift, $
CBKWDS = cbKwds, $
CBTITLE = cbTitle, $
CURRENT = current, $
DIMENSION = dimension, $
OUTPUT_FNAME = output_fname, $
T0 = t0, $
TITLE = title, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if current eq 0 then if obj_valid(pwr_win) then obj_destroy, pwr_win
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    dims = size(data, /DIMENSION)

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Defaults -- NFFT, NSHIFT, DT, and DIMENSION will be handled by MrPSD.
    current = keyword_set(current)
    if n_elements(output_fname) eq 0 then output_fname = ''
    if n_elements(t0) eq 0 then t0 = 0.0
    
    ;Pick out the keywords specific to MrSpectrogram and MrFFT
    if n_elements(extra) gt 0 then begin
        spec_kwds = ['ALPHA', 'CENTER', 'DOUBLE', 'FMIN', 'FMAX', 'INVERSE', $
                     'LOGRANGE', 'LINRANGE', 'NWINDOW', 'OVERWRITE', 'NDIFF', $
                     'TCENTER', 'VERBOSE', 'VVERBOSE', 'WINDOW']    
        void = MrIsMember(spec_kwds, extra, iSpec, $
                          COUNT=nSpec, COMPLEMENT=iIm, NCOMPLEMENT=nIm)
        
        ;Separate the keywords from one another.
        if nSpec gt 0 then spec_kwds = extra[iSpec] else void = temporary(spec_kwds)
        if nIm   gt 0 then extra     = extra[iIm]   else void = temporary(extra)
    endif
    
    ;Create the window
    if current then begin
        pwr_win = GetMrWindows(/CURRENT)
        refresh_in = pwr_win.refresh
        pwr_win -> Refresh, /DISABLE
    endif else pwr_win = MrWindow(XSIZE=500, YSIZE=650, REFRESH=0, XMARGIN=[8,13])

;-----------------------------------------------------
;Calculate PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Compute the spectrogram
    psd = MrPSD(data, nfft, dt, nshift, $
                DIMENSION = dimension, $
                FREQUENCIES = frequencies, $
                T0 = t0, $
                TIME = time, $
               _EXTRA = spec_kwds)
    
    ;Take the log
    psd = MrLog(psd)

;-----------------------------------------------------
;Create Images and Colorbars \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;How many images are being displayed?
    nImages = n_elements(psd[0,0,*])
    imArr = objarr(nImages)
    cbArr = objarr(nImages)
    if n_elements(title) eq 0 then title = replicate('Power Spectral Density', nImages)
    if n_elements(cbtitle) eq 0 then cbtitle = replicate('PSD!C(Units^2 * Hz)', nImages)

    ;Create the spectrogram plots.
    for i = 0, nImages - 1 do begin
        strindex = string(i, FORMAT='(i0)')
        imArr[i] = MrImage(psd[*,*,i], time, frequencies, /CURRENT, $
                           NAME='PWR' + strindex, /AXES, $
                           /NAN, CTINDEX=13, TITLE=title[i], $
                           XTICKFORMAT='time_labels', XTITLE='Time (UT)', $
                           YTITLE='Frequency (Hz)')

        cbArr[i] = MrColorbar(TITLE=cbTitle[i], /CURRENT, NAME='CB: PWR' + strindex, $
                              TARGET=imArr[i], _EXTRA=cbkwds)
        
        ;Set the desired properties
        if nIm gt 0 and n_elements(extra) gt 0 then imArr[i] -> SetProperty, _EXTRA=extra
    endfor

    ;Bind the x- and y-axes together
    pwr_win -> Bind, [imArr, cbArr], /CAXIS
    pwr_win -> Bind, imArr, /XAXIS, /YAXIS

;-----------------------------------------------------
;Return a Window? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if (output_fname eq '') then begin
        
        ;Size window nicely
        if (nImages lt 5) then pwr_win -> SetProperty, YSIZE=200*nImages
        
        ;Put the objects in the window
        if current $
            then pwr_win -> Refresh, DISABLE=~refresh_in $
            else pwr_win -> Refresh
            
        return, pwr_win

;-----------------------------------------------------
;Output results to a file? \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if (output_fname ne '') then begin
        ;The plots need to be drawn first so that the TVRD can grab the image.
        pwr_win -> Draw
        pwr_win -> Output, output_fname
        obj_destroy, pwr_win
        return, obj_new()
    endif
end

