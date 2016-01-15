; docformat = 'rst'
;
; NAME:
;       MrFFT__Define
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
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
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
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
; PURPOSE
;+
;   The purpose of this class is to provide FFT, PSD, Spectrogram, and other frequency-
;   domain-related operations.
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg (Coyote Graphics)
;       MrFFT
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
;	Modification History::
;       2013/12/27  -   Written by Matthew Argall.
;-
;*****************************************************************************************
;+
;   The purpose of this program is to compute the frequency bin spacing, df, of a
;   Fast Fourier Transform.
;
; :Params:
;       NFFT                in, required, type=int
;                           The number of points to use per FFT
;       DT                  in, optional, type=float, default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;                           
; :Returns:
;       DF                  The difference in frequency between frequency bins.
;-
function MrFFT::df, nfft, dt
	compile_opt idl2
	
	if n_elements(dt) eq 0 then dt = 1.0
	
	;calculate the frequency bin size.
	df = 1.0 / (nfft * dt)
	
	return, df
end


;+
;   The purpose of this method is to calulate take the FFT of the data.
;
; :Params:
;       DATA:               in, required, type=any
;                           Data to which the FFT will be applied.
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=0
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;       ALPHA:              in, optional, type=float, default=0.5
;                           If `WINDOW` is set, then this indicates the width of the
;                               filtering window (0.5-1.0) used in creating the window.
;                               If ALPHA=0.5, the result is a "Hanning" window, if ALPHA=1.0,
;                               the result is a "Hamming" window.
;       CENTER:             in, optional, type=Boolean, default=0
;                           Perform a centered Fourier transform. In the forward direction,
;                               the zero frequency is shifted to the center of the array.
;                               In the reverse (inverse) direction, the input is assumed
;                               to be a centered FFT.
;       DIMENSION:          in, optional, type=int, default=longest dimension of `DATA`
;                           The dimension over which to take the FFT. As an example, say
;                               ARRAY is a [3,10] element array. MrFFT(ARRAY, DIMENSION=2),
;                               is the same as
;                               [MrFFT(Array[0,*]), MrFFT(Array[1,*]), MrFFT(Array[2,*])].
;       DOUBLE:             in, optional, type=boolean, default=0
;                           Force computation to be done in double precision.
;       FMIN:               in, optional, type=float, default=0.0
;                           The minimum positive frequency to be kept. If LOGRANGE is set,
;                               this is the minimum power of 10 (not the DC component).
;       FMAX:               in, optional, type=float, default=FNYQUIST.
;                           The maximum positive frequency to be kept. If LOGRANGE is set,
;                               this is alog10(FNYQUIST).
;       FREQUENCIES:        out, type=fltarr(NFFT)
;                           The frequency bins of the FFT
;       INVERSE:            in, optional, type=Boolean, default=1
;                           Perform an inverse (backward) Fourier transform.
;       LINRANGE:           in, optional, type=Int, default=0
;                           Indicate that the frequency range it to be reduced to
;                               LINRANGE number of linearly spaced frequencies between
;                               FMIN and FMAX. See "Limitations" in comment header.
;       LOGRANGE:           in, optional, type=Int, default=0
;                           Indicate that the frequency range it to be reduced to
;                               LOGRANGE number of logarithmically spaced frequencies
;                               between FMIN and FMAX. See "Limitations" in comment header.
;       NWIN1:              in, optional, type=long, default=`NFFT`
;                           If `WINDOW` is set, this is the number of points to use in
;                               the filtering window. If used with `NWIN2`, then the
;                               window will be 2D and NWIN1 indicates the number of
;                               columns in the 2D window.
;       NWIN2:              in, optional, type=long
;                           If `WINDOW` is set and `NWIN1` is specified, then this is the
;                               number of rows of a 2D filtering window.
;       T0:                 in, optional, type=float, default=0.0
;                           A time offset, in seconds, at which the FFT should begin.
;       TCENTER:            in, optional, type=Boolean, default=1
;                           Indicate that the times returned in `TIME` are centered within
;                               their respective FFT bin. A value of 0 will cause the
;                               times to correspond with the beginning of the FFT packet.
;       TIME:               out, type=dblarr
;                           The time associated with each Power Spectral Density slice.
;                               Where each time tag falls within the FFT interval is
;                               determined by the `TCENTER` keyword.
;       VERBOSE:            in, optional, type=Boolean, default=0
;                           Print information about the FFT parameters to the command window.
;       VVERBOSE:           in, optional, type=Boolean, default=0
;                           In addition to `VERBOSE`, print, percent completion.
;       WINDOW:             in. optional, type=Boolean, default=0
;                           Multiply the time series by a filtering window.
;
; :Returns;
;       RESULT:             The results of the FFT.
;-
function MrFFT::FFT, data, nfft, dt, nshift, $
ALPHA = alpha, $
CENTER = center, $
DIMENSION = dimension, $
DOUBLE = double, $
FREQUENCIES = frequencies, $
FMIN = fmin, $
FMAX = fmax, $
INVERSE = inverse, $
LOGRANGE = logrange, $
LINRANGE = linrange, $
NWINDOW = nwindow, $
T0 = t0, $
TCENTER = tcenter, $
TIME = time, $
VERBOSE = verbose, $
VVERBOSE = vverbose, $
WINDOW = window
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, !Null
    endif
    
    ;Calulate the FFT
    result = MrFFT(data, nfft, dt, nshift, $
                   ALPHA = alpha, $
                   CENTER = center, $
                   DIMENSION = dimension, $
                   DOUBLE = double, $
                   FREQUENCIES = frequencies, $
                   FMIN = fmin, $
                   FMAX = fmax, $
                   INVERSE = inverse, $
                   LOGRANGE = logrange, $
                   LINRANGE = linrange, $
                   NWINDOW = nwindow, $
                   T0 = t0, $
                   TIME = time, $
                   TCENTER = tcenter, $
                   VERBOSE = verbose, $
                   VVERBOSE = vverbose, $
                   WINDOW = window)
    
    return, result
end


;+
; :Params:
;       NFFT:               in, required, type=int
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float, default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;
; :Keywords:
;       FNYQUIST:           out, type=float
;                           The nyquist frequency
;       INYQUIST:           out, type=long
;                           The index value within FREQS at which to find the nyquist
;                               frequency
;                           
; :Returns:
;       FREQUENCIES:        The FFT frequencies
;-
function MrFFT::Freqs, nfft, dt, $
FNYQUIST = fnyquist, $
INYQUIST = inyquist
	compile_opt strictarr
	on_error, 2
    
    if n_elements(dt) eq 0 then dt = 1.0
    if nfft mod 2 ne 0 then message, 'NFFT must be even.'
    
    ;calculate the index of the nyquist frequency
    inyquist = nfft/2
    
    ;create an array of frequency indices
    ;the most negative frequency will be one index beyond the nyquist
    ;find the negative index value of the most negative frequency (inyquist+1 -> -inyquist+1)
    ifreqs = lindgen(nfft)
    imost_negf = inyquist + 1
    imost_neg = -inyquist + 1
    
    ;make an array of indices from the most negative to least negative index (-inyquist+1 to -1)
    ;to do so, create an array of imost_neg points ranging from 0 to imost_neg-1 (two away from inyquist)
    ;then subtract imost_neg so that it ranges from -imost_neg to -1
    neg_freqs = lindgen(-imost_neg) + imost_neg
    ifreqs[imost_negf:nfft-1] = neg_freqs

    ;divide by the number of points, nfft, and the sampling interval, dt, to get the 
    ;frequencies
    frequencies = float(ifreqs) / (nfft * dt)
    fnyquist = frequencies[inyquist]
    
    return, frequencies
end


;+
;   Get class properties.
;
; :Keywords:
;-
pro MrFFT::GetProperty
    ;No properties to get.
end


;+
;   The purpose of this program is to compute how many fft windows fit within a
;   given interval.
;
; :Params:
;       NPTS                in, required, type=long
;                           The number of points in the data interval to be analized.
;       NFFT                in, required, type=int
;                           The number of points to use per FFT
;       NSHIFT              in, optional, type=int, default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;                           
; :Returns:
;       N_INTERVALS         The number of FFT intervals of length NFFT that fit within
;                               NPTS (with a shift of NSHIFT).
;-
function MrFFT::NIntervals, npts, nfft, nshift
	compile_opt idl2
	
	;Default to shifting by 1 point
	if n_params() lt 2 then nshift = 1
	if nshift eq 0 then return, 1
	
	;Calculate the number of fft intervals that fit inside NPTS.
	n_intervals = floor(1 + (npts - nfft) / nshift)
	
	return, n_intervals
end


;+
;   The purpose of this method is to calculate the power spectral density.
;
; :Params:
;       DATA:               in, required, type=any
;                           Data to which the psd will be calculated.
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=0
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;       DIMENSION:          in, optional, type=int, default=longest dimension of `DATA`
;                           The dimension over which to take the PSD. As an example, say
;                               ARRAY is a [3,10] element array. PSD(ARRAY, DIMENSION=2),
;                               is the same as
;                               [PSD(Array[0,*]), PSD(Array[1,*]), PSD(Array[2,*])].
;       FREQUENCIES:        out, type=fltarr, type=fltarr(NFFT)
;                           The frequency bins of the spectrogram
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the FFT method is also accepted via
;                               keyword inheritance.
;
; :Returns:
;       THEPSD:             The power spectral density of `DATA`. 
;-
function MrFFT::PSD, data, nfft, dt, nshift, $
 DIMENSION = dimension, $
 FREQUENCIES = frequencies, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, !Null
    endif

;-----------------------------------------------------
;Calculate Spectrogram \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Take the FFT
    thePSD = self -> FFT(data, nfft, dt, nshift, $
                         DIMENSION=dimension, $
                         FREQUENCIES=frequencies, $
                        _STRICT_EXTRA=extra)
    if thePSD eq !Null then return, !Null

    ;Select only the positive frquencies
    posFreqs = where(frequencies gt 0)
    frequencies = frequencies[posFreqs]

    ;The FFT is returned with dimensions arranged as (time, frequency[, component]).
    ;IDL ignores the trailing "*" if there is no 3rd dimension.
    thePSD = abs(thePSD[*, posFreqs, *]) ;* 1.0/(nfft*dt) 
    
    return, thePSD
end


;+
;   The purpose of this method is to calculate the time between samples.
;
; :Params:
;       TIME:       in, required, type=any
;                   Time for which the sampling period is to be calculated.
;
; :Keywords:
;       MODE:       in, optional, type=boolean, default=0
;                   If set, the sample period will be calulated using the mode
;                       of the time between samples. Normally, the mean is used.
;
; :Returns:
;       DT:         Time between samples   
;-
function MrFFT::SamplePeriod, time, $
MODE=mode
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, !Null
    endif

    mode = keyword_set(mode)
    
    ;Use the mode.
    if mode then begin
        npts = n_elements(time)
        dt = MrMode(time[1:npts-1] - time[0:npts-2])
    
    ;Use the average time between samples.
    endif else begin

        ;Calculate the mean different between points.
        dt = moment(time[1:-1] - time[0:-2], MAXMOMENT=1, SDEV=sdev, _STRICT_EXTRA=extra)
        dt = dt[0]
    
        ;Make sure the standard deviation is not of the same order as dt.
        if sdev ge 0.1*dt then $
            message, 'Standard deviation is > 10% of the sample period.', /INFORMATIONAL
    endelse
    
    return, dt
end


;+
;   Set class properties.
;-
pro MrFFT::SetProperty
    ;No properties to set.
end


;+
;   Create a spectrogram plot from a data series.
;
;   Names of the window and graphics objects are::
;       Display Window          -   'PSD Window'
;       Power Spectral Dentiy   -   'PSD#'
;       Colorbars               -   'CB: PSD#'
;
;   Above, the '#' represents the element of `DIMENSION` for which the PSD was calculated.   
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
;                       Any keyword accepted by the MrPSD method is also accepted for 
;                           keyword inheritance.
;
; :Returns:
;       SpecWin:        out, required, type=object
;                       A MrWindow object containing spectrograms and their colorbars.
;-
function MrFFT::Spectrogram, data, nfft, dt, nshift, $
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
        MrPrintF, 'LogErr'
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
    endif else pwr_win = MrWindow(XSIZE=500, YSIZE=650, REFRESH=0, XMARGIN=[8,13], NAME='PSD Window')

;-----------------------------------------------------
;Calculate PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Compute the spectrogram
    psd = self -> PSD(data, nfft, dt, nshift, $
                      DIMENSION = dimension, $
                      FREQUENCIES = frequencies, $
                      T0 = t0, $
                      TIME = time, $
                     _EXTRA = spec_kwds)
    if psd eq !Null then return, !Null
    
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
                           YTITLE='Frequency (Hz)', _EXTRA=extra)

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


;+
;   Clean up after the object is destroyed
;-
pro MrFFT::cleanup
    ;Nothing do clean up yet
end


;+
;   The initialization method.
;-
function MrFFT::init
    return, 1
end


;+
;   The class definition statement.
;
; :Fields:
;       _MRFFT:         A dummy property.
;-
pro MrFFT__define
    compile_opt strictarr
    
    class = { MrFFT, $
              _MrFFT: 0B $
             }
end