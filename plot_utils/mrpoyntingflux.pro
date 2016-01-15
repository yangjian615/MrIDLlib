; docformat = 'rst'
;
; NAME:
;       MrPoytingFlux
;
; PURPOSE:
;+
;       Calculate the Poynting vector spectral density in Fourier space.
;
; :Categories:
;       Plot Utility
;
; :Params:
;       E:              in, required, type=3xN float
;                       The electric field data for which the poytning flox image is to
;                           be made. Units are assumed to be milli-Volts per meter (mV/m)
;       B:              in, required, type=3xN float
;                       The magnetic field data for which the poytning flox image is to
;                           be made. Units are assumed to be nano-Tesla (nT).
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
;       T0:             in, optional, type=float, default=0.0
;                       The time offset at which `DATA` begins, in seconds.
;       TITLE:          in, optional, type=strarr(), default='Power Spectral Density'
;                       An easier way of specifying the title for each spectral plot.
;       _REF_EXTRA:     in, optional, type=structure
;                       Any keyword accepted by MrPoynting_Spectra.pro is also accepted
;                           for keyword inheritance.
;
; :Returns:
;       IMARR:          out, required, type=object/objarr
;                       MrImagePlot objects of each spectra being made.
;
; :Uses:
;   Uses the following external programs::
;       error_message.pro (Coyote Graphics)
;       isMember.pro
;       MrPoynting_Spectra.pro
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
;       10/06/2013  -   Written by Matthew Argall
;-
function MrPoyntingFlux, E, B, nfft, dt, nshift, $
CURRENT = current, $
CBKWDS = cbKwds, $
DIMENSION = dimension, $
T0 = t0, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if max(obj_valid(imArr)) then obj_destroy, imArr
        if n_elements(cbArr) gt 0 && max(obj_valid(cbArr)) eq 1 then obj_destroy, cbArr
        MrPrintF, 'LogErr'
        return, !null
    endif
    
    dims = size(data, /DIMENSION)
    components = ['X', 'Y', 'Z']

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Defaults -- NFFT, NSHIFT, DT, and DIMENSION will be handled by MrPSD.
    if arg_present(cbArr) then keep_cb = 1 else keep_cb = 0
    if n_elements(t0) eq 0 then t0 = 0.0
    current = keyword_set(current)
    
    ;Create the window
    refresh_in = 1
    if current then begin
        poyntWin = GetMrWindows(/CURRENT)
        refresh_in = poyntWin.refresh
        poyntWin -> Refresh, /DISABLE
    endif else poyntWin = MrWindow(XSIZE=500, YSIZE=650, REFRESH=0, XMARGIN=[8,13])
    
    
;-----------------------------------------------------
;Calculate PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Compute the spectrogram
    poynt = MrPoynting_Spectra(E, B, nfft, dt, nshift, /CURRENT, $
                               DIMENSION = dimension, $
                               FREQUENCIES = frequencies, $
                               T0 = t0, $
                               TIME = time, $
                              _EXTRA = extra)

;-----------------------------------------------------
;Create Images and Colorbars \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Create the spectrogram plots.
    for i = 0, 2 do begin
        name = 'S' + components[i]
    
        ;Create the images
        !Null = MrImage(poynt[*,*,i], time, frequencies, /CURRENT, $
                        /AXES, $
                        CTINDEX=13, $
                        MISSING_COLOR='Black', $
                        NAME=name, $
                        TITLE='Poyting-Flux: S$\down' + components[i] + '$', $
                        XTICKFORMAT='time_labels', $
                        XTITLE='Time (UT)', $
                        YTITLE='Frequency (Hz)')

        ;Create the colorbar        
        !Null = MrColorbar(TITLE='S$\down' + components[i] + '$!C($\mu$W/m^2 * Hz)', $
                            TARGET=poyntWin[name], NAME='CB: ' + name)
        
        ;Bind the colorbar to the image.
        poyntWin -> Bind, [poyntWin['CB: ' + name], poyntWin[name]], /CAXIS
    endfor
    
    ;Bind the X-axes together
    Ims = poyntWin -> Get(/ALL, ISA='MrImage')
    poyntWin -> Bind, Ims, /XAXIS

;-----------------------------------------------------
;Return Window or Objects? \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    poyntWin -> Refresh, DISABLE=~refresh_in
    return, poyntWin
end