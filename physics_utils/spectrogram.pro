; docformat = 'rst'
;
; NAME:
;
;       SPECTROGRAM
;
; PURPOSE:
;+
;       The purpose of this program is to create an image of the power spectral density of
;       a set of data.
;
;       NOTE: This program is now obsolete. It has been replaced by MrFFT.pro and
;             MrSpectrogram.pro
;
;   References::
;       Percival, D. and Walden, A., Spectral Analysis for Physical Applications: Multitaper
;           and Conventional Univariate Techniques, Cambridge University Press, 1993.
;
; :Categories:
;
;       Math Utility, Spectral Analysis
;
; :Examples:
;   See the main level program at the end of this file::
;
;       IDL> .r spectrogram
;
; :Params:
;
;       DATA:               in, required, type=fltarr
;                           The data for which a spectrogram is desired.
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       N_SHIFT:            in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;
;       ALPHA:              in, optional, type=float, default=0.5
;                           If `WINDOW` is set, then this indicates the width of the
;                               filtering window (0.5-1.0) used in creating the window.
;                               If ALPHA=0.5, the result is a "Hanning" window, if ALPHA=1.0,
;                               the result is a "Hamming" window.
;       CENTER_TIME:        in, optional, type=Boolean, default=1
;                           Indicate that the times returned in `TIME` are centered within
;                               their respective FFT bin. A value of 0 will cause the
;                               times to correspond with the beginning of the FFT packet.
;       FMIN:               in, optional, type=float, default=0.0
;                           The minimum frequency to be kept If LOGRANGE is set, this is
;                               the minimum power of 10 (not the DC component).
;       FMAX:               in, optional, type=float, default=FNYQUIST.
;                           The maximum frequency to be kept. If LOGRANGE is set, this is
;                               alog10(FNYQUIST)
;       FREQUENCIES:        out, type=fltarr(NFFT)
;                           The frequency bins of the FFT
;       LINRANGE:           in, optional, type=Int, default=0
;                           Indicate that the frequency range it to be reduced to
;                               LINRANGE number of linearly spaced frequencies between
;                               FMIN and FMAX.
;       LOGRANGE:           in, optional, type=Int, default=0
;                           Indicate that the frequency range it to be reduced to
;                               LOGRANGE number of logarithmically spaced frequencies
;                               between FMIN and FMAX.
;       NWINDOW:            in, optional, type=long, default=`NFFT`
;                           If `WINDOW` is set, this is the number of points to use in
;                               the filtering window.
;       TIME:               out, type=dblarr
;                           The time associated with each Power Spectral Density slice. It
;                               is taken at the beginning of each FFT.
;       VERBOSE:            in, optional, type=Boolean, default=0
;                           Print information about the FFT parameters to the command window.
;       WINDOW:             in. optional, type=Boolean, default=0
;                           Multiply the time series by a filtering window.
;                           
; :Returns:
;
;       SPECTGRAM:          The spectrogram.
;
; :Uses:
;   Uses the following external programs::
;       nfft_intervals.pro
;       fft_freqs.pro
;       logspace.pro
;       linspace.pro
;       image_plots.pro (for the example)
;       color_bar.pro (for the example)
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2012
;
; :History::
;   Modification History::
;       Written by:     Matthew Argall 13 November 2012
;       11/27/2012  -   Added FMIN and FMAX keywords. - MRA
;       11/29/2012  -   Added LOGRANGE keyword. - MRA
;       11/30/2012  -   Added LINRANGE keyword. - MRA
;       03/14/2013  -   Return the power (not the energy) of the spectra. Linearly
;                           interpolate the data if the LINRANGE or LOGRANGE keywords are
;                           set. Time stamps are returned as the center time of the FFT
;                           interval. Keyword CENTER_TIME added. - MRA
;       03/27/2013  -   Added VERBOSE keyword. - MRA
;       04/05/2013  -   Added WINDOW, NWINDOW, and ALPHA keywords. - MRA
;       04/12/2013  -   Since FFT already multiplies by 1/N, the result is multiplied
;                           by 1/dt, not 1/(dt*nfft). - MRA
;-
function spectrogram, data, nfft, dt, n_shift, $
ALPHA = alpha, $
CENTER_TIME = center_time, $
FREQUENCIES = frequencies, $
FMIN = fmin, $
FMAX = fmax, $
LOGRANGE = logrange, $
LINRANGE = linrange, $
NWINDOW = nwindow, $
TIME = time, $
VERBOSE = verbose, $
WINDOW = window, $
_EXTRA = extra
    compile_opt idl2
    on_error, 2

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Create defaults.
    if n_elements(nfft) eq 0 then nfft = floor(n_elements(data) / 4)
    if n_elements(dt) eq 0 then dt = 1.0
    if n_elements(n_shift) eq 0 then n_shift = floor(nfft/2)
    if n_elements(center_time) eq 0 then center_time = 1
    npts = n_elements(data)
    
    ;Time stamp at the center of the packet or at the beginning?
    if keyword_set(center_time) then ct = 0.5 else ct = 0.0
    
    ;Check how to window the data
    if keyword_set(window) then begin
        if n_elements(nwindow) eq 0 then nwindow = nfft
        if n_elements(alpha) eq 0 then alpha = 0.5
    endif

;-----------------------------------------------------
;FFT Parameters \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;get the sampling frequency at which the data was recorded, the frequency spacing,
    ;the number of FFT intervals and an array of the frequency bins.
    ;sampling_rate = 1.0 / dt
    df = 1 / (nfft * dt)
    n_intervals = nfft_intervals(npts, nfft, n_shift)
    frequencies = fft_freqs(nfft, dt, FNYQUIST=fnyquist, INYQUIST=inyquist)

    ;Set the default FMIN
    if n_elements(fmin) eq 0 then $
        if keyword_set(logrange) then fmin = alog10(frequencies[1]) $
                                 else fmin = frequencies[0]

    ;Set the default FMAX
    if n_elements(fmax) eq 0 then $
        if keyword_set(logrange) then fmax = alog10(frequencies[iNyquist]) $
                                 else fmax = frequencies[iNyquist]

;-----------------------------------------------------
;Print FFT Parameters \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if keyword_set(verbose) then begin
        print, format='(%"sample period = %f s")', dt
        print, format='(%"sample rate   = %f Hz")', 1.0 / dt
        print, format='(%"sample intrvl = %f s")', nfft*dt
        print, format='(%"df            = %f Hz")', df
        print, format='(%"f_min         = %f Hz")', fmin
        print, format='(%"f_max         = %f Hz")', fmax
        print, format='(%"NFFT          = %i")', nfft
        print, format='(%"NSHIFT        = %i")', n_shift
        print, format='(%"t_shift       = %i s")', n_shift*dt
    endif

;-----------------------------------------------------
;Frequency Range \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Will we need to interpolate to match the desired frequencies? This will be
    ;true if either of the LOGSPACE or LINSPACE or linspace keywords are set.
    tf_interp = 0

    ;LOGRANGE
    if keyword_set(logrange) then begin
        tf_interp = 1
        if logrange eq 1 then logrange = floor(n_elements(frequencies)/10)
        
        ;Create a logarithmically spaced array of frequencies
        interp_freqs = logspace(fmin, fmax, logrange)
        nfreqs = logrange

    ;LINRANGE
    endif else if keyword_set(linrange) then begin
        tf_interp = 1
        if linrange eq 1 then linrange = 50
        
        ;Create a linearly spaced array of frequencies.
        interp_freq = linspace(fmin, fmax, linrange)
        nfreqs = linrange

    ;NORMAL
    endif else begin
        ;Find the positive frequency range for which the power is to be calculated.
        theseFreqs = where(frequencies[0:iNyquist] ge fmin and $
                           frequencies[0:iNyquist] le fmax, nfreqs)
    
        ;If no frequencies fall within the desired range, pick everything
        if nfreqs eq 0 then begin
            theseFreqs = lindgen(iNyquist)
            nfreqs = iNyquist
        endif
    endelse
 
;-----------------------------------------------------
;Create the Spectrogram \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;allocate the power spectral density and time arrays
    spectgram = dblarr(n_intervals, nfreqs)
    time = fltarr(n_intervals)

    ;create the window
    if keyword_set(window) then theWindow = hanning(nwindow, ALPHA=alpha)
    
    sindex = 0
    eindex = nfft - 1
    ;for each fft interval...
    for i = 0, n_intervals-1 do begin
        if keyword_set(verbose) and i mod 500 eq 0 then $
            print, FORMAT='(%"%5.1f percent done. Interval %i of %i")', $
                   (float(i)+1)/float(n_intervals)*100, i+1, n_intervals
        
        ;calculate the fft
        if keyword_set(window) $
            then fft_data = fft(theWindow * data[sindex:eindex]) $
            else fft_data = fft(data[sindex:eindex])
        
        ;Store the power spectral density from each FFT into one column of the array.
        ;(The division by T converts from energy to power. C.f. Percival, pg. 59-60)
        if tf_interp then begin
            ;to interpolate, frequencies must be monotonic, so put negative frequencies
            ;first.
            fft_data = shift(fft_data, iNyquist-1)
            frequencies = shift(frequencies, iNyquist-1)
            
            ;Interpolate
            spectgram[i,*] = 1.0/dt * abs(interpol(fft_data, frequencies, interp_freqs))

        ;No interpolation
        endif else begin
            spectgram[i,*] = 1.0/dt * abs(fft_data[theseFreqs])
        endelse
        
        ;Create the time tag.
        time[i] = (i + ct) * n_shift * dt

        ;shift the fft by the desired number of points
        sindex += n_shift
        eindex += n_shift
    endfor

    ;Get only the desired frequencies
    if tf_interp $
        then frequencies = interp_freqs $
        else frequencies = frequencies[theseFreqs]
    
    return, spectgram
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Say I have 600 seconds of data sampling at 10Hz
;We are detecting a 1.5Hz wave
sample_frequency = 10.0
omega = 2 * !pi * 1.5 * sample_frequency
dt = 1 / sample_frequency
duration = 600
npts = duration * sample_frequency

;make the time and data arrays. plot them
t = findgen(npts)/(npts-1) * duration * dt
data = sin(omega * t)
plot, t, data, $
      TITLE='Input Signal (1.5Hz sampled at 10Hz).', $
      XTITLE='Time (seconds)', xrange=[0,1], $
      YTITLE='Amplitude'

;make the spectrogram
image = spectrogram(data, 100, dt, 25, FREQUENCIES=frequencies)

;select only the frequencies between 0 and 5Hz
fmin = 0
fmax = 5
ifreq = where(frequencies ge fmin and frequencies le fmax, count)
image = image[*,ifreq]

;plot the data
pos = reform(plot_positions([1,1], xmargin=[10,10]))
image_plots, alog10(image), /axes, /scale, position=pos, $
             xrange=[t[0], t[-1]], $
             yrange=[0, max(frequencies)]
color_bar, pos, cbrange=[min(alog10(image), max=imax), imax], /NOERASE
end