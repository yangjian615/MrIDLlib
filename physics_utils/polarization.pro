; docformat = 'rst'
;
; NAME:
;
;       POLARIZATION
;
; PURPOSE:
;+
;       The purpose of this program is to determine the polarization and other polarization
;       related parameters of a wave.
;
;       IMPORTANT:
;           Matrices are stored in the mathematical sense, not in the IDL sense. As an
;           example, the power spectral matrix, J_prime, is::
;
;               J_prime = [[Hxx, Hxy, Hyz], $
;                          [Hyx, Hyy, Hyz], $
;                          [Hzx, Hzy, Hzz]]
;
;           Thus, to access the Hxy component, one must select matrix index J_prime[1,0]
;
;   Physical Notes::
;       1) Samson is ultimately the reference you want, followed by Means and Rankin.
;           Fowler is gives pretty much the same theoretical analysis as Rankin, but is
;           slightly more detailed when it comes to averaging over time and frequency.
;       2) Without smoothing over frequencies (i.e. NFAVG=0), the polarization at each
;           frequency will be 1. Consider a monochromatic wave whose k-vector makes an
;           angle of 40 degrees with respect to the mean magnetic field direction. 100%
;           of the monochromatic wave will be polarized at this angle. That is to say,
;           without mixing frequencies, the polarization is pure at all frequencies.
;       3) References to Percival and Barakat give the same analysis as Fowler, Rankin,
;           Means, and Samson, but use an enseble average as opposed to a time average.
;           The steps are qualitatively similar and Percival especially is enlightening,
;           but to generate an enseble, one would need to create a model and run it many
;           many times.
;       4) There are two places where NaNs are explicitly used to fill array elements::
;           a) If |B| is too small, the rotation matrix into the wave normal frame cannot
;               be properly determined.
;           b) If the trace of the covariance matrix is not sufficiently large, the wave-
;               normal direction cannot be determined properly (Applicable to the `MEANS`
;               method only).
;   References::
;
;       Barakat, R., Theory of Coherency Matrix for Light of Arbitrary Spectral Bandwidth,
;           Journal of the Optical Society of America, 53, 3, 1963.
;       Fowler, R. A., et. al, Polarization Analysis of Natural and Artificially Induced
;           Geomagnetic Micropulsations, Journal of Geophysical Research, 72, 11, 1967.
;       Means, J., Use of Three-Dimensional Covariance Matrix in Analyzing the
;           Polarization Properties of Plane Waves, Journal of Geophysical Research,
;           77, 28, 1972.
;       Percival, D. and Walden, A., Spectral Analysis for Physical Applications: Multitaper
;           and Conventional Univariate Techniques, Cambridge University Press, 1993.
;       Rankin, D., and Kurtz, R., Statistical Study of Micropulsation Polarization,
;           Journal of Geophysical Research, 75, 28, 1970.
;       Samson, J. C., and Olson, J. V., Some comments on the description of polarization
;           states of waves, Geophys J. R. Astr. Soc., 115-119, 61, 1980.
;
; :Categories:
;
;       Math Utility, Polarization
;
; :Examples:
;   See the main level program at the end of this file::
;
;       IDL> .r polarization
;
; :Uses:
;   Uses the following external programs::
;       nfft_intervals.pro
;       fft_freqs.pro
;       logspace.pro
;       linspace.pro
;       rotate_matrix.pro
;       image_plots.pro (for the example)
;       color_bar.pro (for the example)
;       mrsiggen.pro (for the example)
;       plot_positions.pro (for the example)
;       spectrogram.pro (for the example)
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
;       03/13/2013  -   Written by Matthew Argall
;       03/29/2013  -   Added the KDOTB_ANGLE keyword.
;       03/30/2013  -   Added the METHOD keyword and incorporated the method introduced
;                           by Fowler, which uses minimum variance to determine the
;                           wave-normal direction.
;       06/13/2013  -   Added the IS_DETRENDED keyword. - MRA
;       09/04/2013  -   Do not divide by T (period) when calculating J_prime. Assume that
;                           the data is provided in a field-aligned coordinate system with
;                           the mean field along the z-axis. Added the NDETREND and NFAS
;                           keywords. Removed the IS_DETRENDED keyword. Pick out only
;                           positive frequencies. - MRA
;-
;***************************************************************************************
;+
;   The purpose of this function is to calculate the wave-normal direction by using
;   the imaginary, anti-symmetric part of the spectral matrix, as done in Means.
;
;   :Private:
;
;   :Params:
;
;       J_PRIME:                in, required, type=3x3xN numeric
;                               Spectral matrix in the frame of the observer.
;       IPOL:                   out, required, type=long
;                               Indices of the frequencies that are polarized
;       ILIN:                   out, required, type=long
;                               Indices of the frequencies that are linear (unpolarized)
;
;   :Returns:
;
;       K_HAT:                  The wave normal direction at each frequency.
;-
function polarization_means, J_prime, ipol, ilin, npol, nlin
    compile_opt idl2, hidden
    on_error, 2

    ;allocate memory
    nfreqs = n_elements(J_prime[0,0,*])
    ab = fltarr(nfreqs)
    k_hat = fltarr(3, nfreqs)

    ;The wave normal direction can now be determined by examining the imaginary part of
    ;the spectral density matrix (Means pg. 5554, Bakarat eq 3.19).
    
    ;Using the imaginary components.
    ;a*b = Hxy^2 + Hzx^2 + Hyz^2
    ab = sqrt(imaginary(reform(J_prime[1,0,*]))^2 + $     ;Hxy^2
              imaginary(reform(J_prime[2,0,*]))^2 + $     ;Hxz^2
              imaginary(reform(J_prime[2,1,*]))^2)        ;Hyz^2

    ;The imaginary part of the power spectral matrix for linearly polarized waves
    ;is the 0-matrix. Thus, the analysis that follows cannot be undertaken. (Means
    ;pg. 5555-6). Treat them separately later.
    ilin  = where(ab eq 0, nlin, COMPLEMENT=ipol, NCOMPLEMENT=npol)
    
    ;Treat the linearly polarized cases first.
    if nlin gt 0 then begin
        ;The power of the linearly polarized wave
        ;Tr[J'] = a^2 = Jxx + Jyy + Jzz   --   (J is purely real)
        a2 = real_part(reform(J_prime[0,0,ilin] + J_prime[1,1,ilin] + J_prime[2,2,ilin]))
        
        ;If there is no power at the frequency
        ino_power = where(a2 lt 1e-4, nno_power, COMPLEMENT=ipower, NCOMPLEMENT=npower)
        if nno_power gt 0 then begin
            ;There is no direction of propagation.
            k_hat[*,ilin[ino_power]] = replicate(!values.f_nan, 3, nno_power)
        endif
            
        ;Otherwise
        if npower gt 0 then begin
            ;Calculate the direction of the linearized wave
            Lx = sqrt(real_part(reform(J_prime[0,0,ilin[ipower]])) / a2[ipower])    ;(Jxx / a^2)^(1/2)
            Ly = real_part(reform(J_prime[1,0,ilin[ipower]])) / (a2[ipower] * Lx)   ;Jxy / (a^2 * Lx)
            Lz = real_part(reform(J_prime[2,0,ilin[ipower]])) / (a2[ipower] * Lx)   ;Jxz / (a^2 * Lx)
            
            ;Store the direction of propagation
            k_hat[*,ilin[ipower]] = transpose([[Lx], [Ly], [Lz]])
        endif
    endif
    
    ;Now treat polarized waves.
    if npol gt 0 then begin
        ;Note that dividing by "ab" normalizes the components.
        kz =  imaginary(reform(J_prime[1,0,ipol])) / ab[ipol]   ;kz =  Hxy / (a*b)
        ky = -imaginary(reform(J_prime[2,0,ipol])) / ab[ipol]   ;ky = -Hxz / (a*b)
        kx =  imaginary(reform(J_prime[2,1,ipol])) / ab[ipol]   ;kx =  Hyz / (a*b)
    
        k_hat[*,ipol] = transpose([[kx], [ky], [kz]])
    endif

    ;This anaylsis has been done under the assumptions of a counter-clockwise,
    ;elliptically polarized plane wave. Thus, a clockwise polarized wave has k_hat < 0

    return, k_hat
end


;+
;   The purpose of this function is to calculate the wave-normal direction by using
;   the real, symmetric part of the covariance matrix (the cross-spectral matrix). This
;   involves diagonalizing the real part of the covariance matrix and using the
;   vector associated with the minimum eigenvalue as the wave normal directin (i.e. we
;   obtain the wave-normal vector via a minimum variance approach, as is often stated in
;   the literature).
;
;   :Private:
;
;   :Params:
;
;       J_PRIME:                in, required, type=3x3xN numeric
;                               The real, symmetric part of the Spectral matrix in the
;                                   frame of the observer.
;
;   :Returns:
;
;       K_HAT:                  The wave normal direction at each frequency.
;-
function polarization_fowler, J_prime
    compile_opt idl2, hidden
    on_error, 2

    ;The wave normal direction can now be determined by examining either the real
    ;part of the spectral density matrix (Fowler, pg. 2873-4, Means pg. 5554, 
    ;Bakarat eq 3.19).
    
    nfreqs = n_elements(J_prime[0,0,*])
    k_hat = fltarr(3, nfreqs)
    
    ;Diagonalizing the covariance matrix
    for i = 0, nfreqs-1 do begin
        ;Calculate the eigenvectors of the covariance matrix. The wave-normal direction
        ;points along the direction of minimum variance, i.e. the direction associated
        ;with the smallest eigenvalue
        eigvals = eigenql(J_prime[*,*,i], /ABSOLUTE, /ASCENDING, EIGENVECTORS=eigvecs)
        k_hat[*,i] = eigvecs[*,0]
    endfor

    return, k_hat
end


;+
;
;   The purpose of this program is to determine the polarization and other polarization
;   related parameters of a wave.
;
;   IMPORTANT:
;       Matrices are stored in the mathematical sense, not in the IDL sense. As an
;       example, the power spectral matrix, J_prime, is::
;
;           J_prime = [[Hxx, Hxy, Hyz], $
;                      [Hyx, Hyy, Hyz], $
;                      [Hzx, Hzy, Hzz]]
;
;       Thus, to access the Hxy component, one must select matrix index J_prime[1,0]
;
; :Params:
;
;       DATA:               in, required, type=string
;                           The filename of the CDF file to be opened
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;
;       CENTER_TIME:        in, optional, type=Boolean, default=1
;                           Indicate that the center time of each interval used to
;                               the polarization is to be used as the time stamp. If set
;                               equal to zero, the initial time of the interval will be
;                               used.
;       COHERENCY:          out, optional, type=NxM float
;                           Coherency of the wave in the wave-normal frame. 0 indicates
;                               that the waves are incoherent while a value of 1 indicates
;                               complete coherence.
;       ELLIPTICITY:        out, optional, type=NxM float
;                           Ellipticity of the wave in the wave-normal frame. Counter-
;                               clockwise if arg(Jxy) > 0.
;       FMIN:               in, optional, type=float, default=0.0
;                           The minimum frequency to be kept If `LOGRANGE` is set, this is
;                               the minimum power of 10.
;       FMAX:               in, optional, type=float, default=FNYQUIST.
;                           The maximum frequency to be kept. If `LOGRANGE` is set, this
;                               the maximum power of 10.
;       FREQUENCIES:        out, type=fltarr(NFFT)
;                           The frequency bins of the FFT
;       INTENSITY:          out, optional, type=NxM float
;                           Intensity of the wave in the wave-normal system.
;       IS_DETRENDED:       in, optional, type=boolean, default=0
;                           Set if `DATA` has already has already been detrended -- i.e.,
;                               the local mean field has already been subtracted.
;       K_HAT:              out, optional, type=3xM float
;                           A unit vector pointing in the wave-normal direction.
;       KDOTB_HAT:          out, optional, type=NxM float
;                           The angle between the wave normal direction and the mean
;                               magnetic field.
;       LINRANGE:           in, optional, type=Int, default=0
;                           Indicate that the frequency range it to be reduced to
;                               LINRANGE number of linearly spaced frequencies between
;                               FMIN and FMAX.
;       LOGRANGE:           in, optional, type=Int, default=0
;                           Indicate that the frequency range it to be reduced to
;                               LOGRANGE number of logarithmically spaced frequencies
;                               between FMIN and FMAX.
;       MEAN_FIELD:         out, optional, type=3xM float
;                           The mean magnetic field for each interval used in calculating
;                               the polarization and other parameters.
;       METHOD:             in, optional, type=int, default=1
;                           Two methods have been incorporated (with a third on the way).
;                               Method::
;                                   1 - Means, uses complex part of the covariance matrix
;                                        to determine the wave normal direction directly.
;                                   2 - Fowler diagonalizes the real part of the wave
;                                        normal direction and uses the minimum eigenvalue
;                                        as the wave-normal direction
;                                  [3]- Samson expands the covariance matrix in an ortho-
;                                        normal basis and computes the polarization
;                                        directly, without need of the wave-normal frame.
;       NDETREND:           in, optional, type=int, default=0
;                           If non-zero, `DATA` will be detrended. NDETREND number of
;                               points will be used to determine the average, background
;                               field that will be subracted from the data.
;       NFAS:               in, optional, type=in, default=0
;                           If non-zero, data will be rotated into a field-aligned
;                               coordinate system. NFAS number of points will be used to
;                               determine the mean, background field. If `NDETREND` is
;                               also set, then NFA will be set equal to NDETREND. The Z-
;                               axis then points along the background field, X is the
;                               cross product of Z and [0,1,0]. Y completes the system.
;                               [0,1,0] is the y-axis in the coordinate system of `DATA`.
;       NFAVG:              in, optional, type=int, default=7
;                           Number of frequency bins to average. Must be an odd integer
;                               <= 3.
;       POLARIZATION_ANGLE: out, optional, type=NxM float
;                           Angle of polarization, i.e. the angle between the wave-normal
;                               frame's x-axis and the major axis of the polarization
;                               ellipse.
;       SPECTRAL_MATRIX:    out, optional, type=3x3xNxM float
;                           The power spectral matrix of the waveform in the reference
;                               frame of the wave.
;       TIME:               out, type=fltarr(M)
;                           The time associated with each interval used in calculating
;                               the polarization and other parameters. This time is
;                               taken to be the center time of the interval unless
;                               `CENTER_TIME` is set to 0, in which case the initial
;                               time of the interval is taken.
;       VERBOSE:            in, optional, type=Boolean, default=0
;                           Print FFT parameters and progress to command window.
;       WAVE_NORMAL_MATRIX: out, optional, type=2x2xNxM float
;                           The power spectral matrix in the wave-normal frame.
;                           
; :Returns:
;
;       PZATION:            The plarization state of the wave.
;
; :Uses:
;   Uses the following external programs::
;       nfft_intervals.pro
;       fft_freqs.pro
;       logspace.pro
;       linspace.pro
;       rotate_matrix.pro
;       image_plots.pro (for the example)
;       color_bar.pro (for the example)
;       mrsiggen.pro (for the example)
;       plot_positions.pro (for the example)
;       spectrogram.pro (for the example)
;-
function polarization, data, nfft, dt, nshift, $
CENTER_TIME = center_time, $
COHERENCY = coherency, $
DIMENSION = dimension, $
ELLIPTICITY = ellipticity, $
FREQUENCIES = frequencies, $
FMIN = fmin, $
FMAX = fmax, $
INTENSITY = intensity, $
K_HAT = k_hat, $
KDOTB_ANGLE = kdotb_angle, $
LOGRANGE = logrange, $
LINRANGE = linrange, $
MEAN_FIELD = mean_field, $
METHOD = method, $
NDETREND = nDetrend, $
NFAS = nFAS, $
NFAVG = nfavg, $
POLARIZATION_ANGLE = polarization_angle, $
SPECTRAL_MATRIX = spectral_matrix, $
TIME = time, $
VERBOSE = verbose, $
WAVE_NORMAL_MATRIX = wave_normal_matrix, $
_EXTRA = extra
    compile_opt idl2
;    on_error, 2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, !Null
    endif

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Create defaults.
    if n_elements(nfft) eq 0 then nfft = floor(n_elements(data) / 4)
    if n_elements(dt) eq 0 then dt = 1
    if n_elements(n_shift) eq 0 then n_shift = floor(nfft/2)
    if n_elements(center_time) eq 0 then center_time = 1
    if n_elements(nDetrend) eq 0 then nDetrend = 0
    if n_elements(nFAS) eq 0 then nFAS = 0
    if n_elements(nfavg) eq 0 then nfavg = 7
    if n_elements(verbose) eq 0 then verbose = 0
    if n_elements(method) eq 0 then method = 1
    npts = n_elements(data[0,*])
    
    ;Must be method 1 or 2
    if method lt 1 or method gt 2 then message, 'Invalid method. METHOD = {1 | 2}.'
    
    ;Make sure NFAVG is odd and >= 3
    if (nfavg mod 2) eq 0 or nfavg lt 3 then $
        message, 'NFAVG must be an odd integer <= 3.'
    
    ;Make sure nFA and nDetrend are equal, if both are non-zero
    if nFAS ne 0 and nDetrend ne 0 then nFAS = nDetrend
    
    ;Time stamp at the center of the packet or at the beginning?
    if keyword_set(center_time) then ct = 0.5 else ct = 0.0
    
    ;Keep certain parameters?
    if arg_present(coherency) then keep_coherency = 1 else keep_coherency = 0
    if arg_present(intensity) then keep_intensity = 1 else keep_intensity = 0
    if arg_present(mean_field) then keep_mean_field = 1 else keep_mean_field = 0
    if arg_present(ellipticity) then keep_ellipticity = 1 else keep_ellipticity = 0
    if arg_present(time) then keep_time = 1 else keep_time = 0
    if arg_present(spectral_matrix) then keep_spectral_matrix = 1 else keep_spectral_matrix = 0
    if arg_present(wave_normal_matrix) then keep_wave_normal = 1 else keep_wave_normal = 0
    if arg_present(polarization_angle) then keep_angle = 1 else keep_angle = 0
    if arg_present(k_hat) then keep_khat = 1 else keep_khat = 0
    if arg_present(kdotb_angle) then keep_kdotb_angle = 1 else keep_kdotb_angle = 0

    dtype = size(data, /TYPE)
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
        print, format='(%"T             = %f s")', nfft*dt
        print, format='(%"df            = %f Hz")', df
        print, format='(%"f_min         = %f Hz")', fmin
        print, format='(%"f_max         = %f Hz")', fmax
        print, format='(%"NFFT          = %i")', nfft
        print, format='(%"NSHIFT        = %i")', nshift
        print, format='(%"t_shift       = %i s")', nshift*dt
        print, format='(%"N f avgd      = %i")', nfavg
        print, format='(%"f range avgd  = %f")', nfavg*df
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
        
        ;Create a logarithmically spaced array of frequencies, symmetric about 0
        interp_freqs = logspace(fmin, fmax, logrange)
        interp_freqs = [interp_freqs, -reverse(interp_freqs)]
        
        nfreqs = n_elements(interp_freqs)
        iMax_interp = logrange - 1

    ;LINRANGE
    endif else if keyword_set(linrange) then begin
        tf_interp = 1
        if linrange eq 1 then linrange = 50
        
        ;Create a linearly spaced array of frequencies, symmetric about 0
        interp_freq = linspace(fmin, fmax, linrange)
        if interp_freq[0] eq 0 $
            then interp_freqs = [interp_freqs, -reverse(interp_freqs[1:*])] $
            else interp_freqs = [interp_freqs, -reverse(interp_freqs)]
            
        nfreqs = n_elements(interp_freqs)
        iMax_interp = linrange - 1

    ;NORMAL
    endif else begin
        ;Pick out the desired frequency range
        posFreqs = where(frequencies ge fmin and $
                         frequencies le fmax, nfreqs)

        ;If no frequencies fall within the desired range, pick everything
        if nfreqs eq 0 then begin
            theseFreqs = lindgen(iNyquist)
            iMax = iNyquist - 1
        
        ;Otherwise, organize the frequency indices with positive first
        endif else begin
            ;Put the positive freuqn
            theseFreqs = posFreqs[sort(posFreqs)]
            iMax = nfreqs - 1
        endelse
    endelse
  
;-----------------------------------------------------
;Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;allocate the power spectral density and time arrays
    data_mean = fltarr(3)
    if keep_mean_field then mean_field = fltarr(3, n_intervals)
    if keep_coherency then coherency = fltarr(nfreqs, n_intervals)
    if keep_intensity then intensity = fltarr(nfreqs, n_intervals)
    if keep_ellipticity then ellipticity = fltarr(nfreqs, n_intervals)
    if keep_khat then k_hat = fltarr(3, nfreqs, n_intervals)
    if keep_time then time = fltarr(n_intervals)
    if keep_spectral_matrix then spectral_matrix = complexarr(3, 3, nfreqs, n_intervals)
    if keep_wave_normal then wave_normal_matrix = complexarr(2, 2, nfreqs, n_intervals)
    if keep_angle then polarization_angle = fltarr(nfreqs, n_intervals)
    if keep_kdotb_angle then kdotb_angle = fltarr(nfreqs, n_intervals)
    pzation = fltarr(nfreqs, n_intervals)
    
    ;Temporary Variables
    fft_final = complexarr(3, nfreqs)
    J_prime = complexarr(3, 3, nfreqs)
    J = complexarr(3, 3, nfreqs)
    Js = complexarr(3, 3, nfreqs)
    kx = fltarr(nfreqs)
    ky = fltarr(nfreqs)
    kz = fltarr(nfreqs)
    k_hat_temp = fltarr(3, nfreqs)
    R = fltarr(3, 3, nfreqs)
  
;-----------------------------------------------------
;Detrend the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    temp_data = data

    ;Detrend the data?
    if nDetrend ne 0 then $
        temp_data = detrend_data(temp_data, nDetrend, BACKGROUND=mean_field, DIMENSION=dimension)
    
    ;Rotate into a field-aligned system?
    if nFAS ne 0 then begin
        fas = fa_system(temp_data, nFAS, AVG_DATA=mean_field)
        temp_data = rotate_vector(fas, temp_data)
    endif
  
;-----------------------------------------------------
;Start Polarization Calculations \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    sindex = 0
    eindex = nfft - 1
    
    ;for each fft interval...
    for i = 0, n_intervals-1 do begin
        if keyword_set(verbose) and i mod 500 eq 0 then $
            print, FORMAT='(%"%5.1f percent done. Interval %i of %i")', $
                   (float(i)+1)/float(n_intervals)*100, i+1, n_intervals
    
    ;-----------------------------------------------------
    ;I. Take the FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        ;To start, we first calculate the fft
        fft_data = 1.0/dt * fft(temp_data[*,sindex:eindex], 2)
        if tf_interp then begin
            fft_data = shift(fft_data, 0, iNyquist-1)
            frequencies = shift(frequencies, iNyquist-1)
            
            fft_final[0,*] = interpol(fft_data[0,*], frequencies, interp_freqs)
            fft_final[1,*] = interpol(fft_data[1,*], frequencies, interp_freqs)
            fft_final[2,*] = interpol(fft_data[2,*], frequencies, interp_freqs)
        endif else begin
            fft_final = fft_data[*,theseFreqs]
        endelse

    ;-----------------------------------------------------
    ;II. Covariance Spectral Matrix \\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        
        ;Due to the Hermetian nature of the spectral denstiy matrix (eq 3.18 Bakarat), 
        ;Mead says,
        ;
        ;   The analytic representation of the real signal is easily obtained by 
        ;   multiplying the  Fourier transform of the real signal by the Heaviside step
        ;   function 
        ;
        ;       S(f) = 0        f <  0
        ;       S(f) = 1        f >= 0
        ;
        ;   and doubling the result.
        
        ;Using Means' notation, calculate the spectral density matrix. The "prime"
        ;indicates that the spectral density matrix is NOT in the principle axis system.
        ;Dividing by the length of the time interval converts from Energy to Power.
        J_prime[0,0,*] = 2.0 * fft_final[0,*] * conj(fft_final[0,*])      ;Hxx
        J_prime[1,0,*] = 2.0 * fft_final[0,*] * conj(fft_final[1,*])      ;Hxy
        J_prime[2,0,*] = 2.0 * fft_final[0,*] * conj(fft_final[2,*])      ;Hxz
        J_prime[0,1,*] = 2.0 * fft_final[1,*] * conj(fft_final[0,*])      ;Hyx
        J_prime[1,1,*] = 2.0 * fft_final[1,*] * conj(fft_final[1,*])      ;Hyy
        J_prime[2,1,*] = 2.0 * fft_final[1,*] * conj(fft_final[2,*])      ;Hyz
        J_prime[0,2,*] = 2.0 * fft_final[2,*] * conj(fft_final[0,*])      ;Hzx
        J_prime[1,2,*] = 2.0 * fft_final[2,*] * conj(fft_final[1,*])      ;Hzy
        J_prime[2,2,*] = 2.0 * fft_final[2,*] * conj(fft_final[2,*])      ;Hzz

        ;This is the spectral density matrix [covariance matrix in the frequency domain]
        ;(c.f. "Technique and Theory" and eq 1 in Means or eq 3.18 in Bakarat).

    ;-----------------------------------------------------
    ;III. Wave Normal Direction \\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
    
        if method eq 1 then begin
            k_hat_temp = polarization_means(J_prime, ipol, ilin, npol, nlin)
        
        endif else if method eq 2 then begin
            k_hat_temp = polarization_fowler(real_part(J_prime))
            ipol = lindgen(nfreqs)
            npol = nfreqs
            nlin = 0
            
        endif else if method eq 3 then begin
            ;Not implemented yet.
        endif
  
    ;-----------------------------------------------------
    ;IV. Wave-Normal System \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
    
        ;Polarization is always referenced to the magnetic field direction. A wave is
        ;right-hand polarized if it rotates in a counter-clockwise direction when looking
        ;down B_hat and left-handed if it rotates clockwise. 
        
        ;Now, the covariance spectral matrix must be rotated into a frame in which the
        ;wave normal direction is along one of the principal axes (the z-axis). Then,
        ;choosing x-direction to be in the k_hat dot B_hat plane, for elliptiicty
        ;measurement purposes, and the y-axis to complete the system. (Means, pg. 5555)
        
        ;Make sure k_hat is parallel to B, not anti-parallel
        k_dot_b = reform(k_hat_temp[2,*])

        iantipar = where(k_dot_b lt 0, count)
        if count gt 0 then begin
            k_hat_temp[*,iantipar] = -k_hat_temp[*,iantipar]
            k_dot_b = reform(k_hat_temp[2,*])
        endif
    
        ;Put the wave normal vector along the z-hat direction. Make "y" be in the plane
        ;perpendicular to k_hat and B_hat, then complete the system to make "x" such that
        ;"x" lies in the k-B plane.
        R[*,2,*] = k_hat_temp
        R[*,1,*] = cross_product(k_hat_temp, [0.0, 0.0, 1.0])
        R[*,0,*] = cross_product(reform(R[*,1,*]), reform(R[*,2,*]))
    
        ;The wave-normal matrix (R) just created is the transformation matrix required
        ;to transform the current spectral density matrix (J') into the wave-normal
        ;system (J) via the rotation J = R J' R^T.
    
        ;Calculating the real and imaginary parts separately,
        ;NOTE: It might be pertinent to set the diagonal components of J_prime equal to
        ;      zero explicily (c.f. Means pg. 5555).
        J = complex(rotate_matrix(R, real_part(J_prime)), $
                    rotate_matrix(R, imaginary(J_prime)))
  
    ;-----------------------------------------------------
    ;V. Average Over Frequency Bins \\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        ;Without smoothing over frequencies (i.e. NFAVG=0), the polarization at each
        ;frequency will be 1. Consider a monochromatic wave whose k-vector makes an
        ;angle of 40 degrees with respect to the mean magnetic field direction. 100%
        ;of the monochromatic wave will be polarized at this angle. That is to say,
        ;without mixing frequencies, the polarization is pure at all frequencies.
        
        ;The center of the averaging interval
        nfavg_half = (nfavg-1)/2
        
        ;step through all of the averaging intervals.
        for k = 0, nfreqs - nfavg do begin
            ;Calculate index for the center, start, and end frequencies
            icenter_bin = k + nfavg_half
            istart = icenter_bin - nfavg_half
            iend = icenter_bin + nfavg_half

            ;Average of the frequency inteval (Js => J smoothed over frequency)
            Js[*,*,icenter_bin] = mean(J[*,*,istart:iend], DIMENSION=3)
            
            ;Keep the angle between k and b?
            if keep_kdotb_angle then $
                kdotb_angle[icenter_bin,i] = acos(mean(k_dot_b[istart:iend]))
        endfor
        
        ;We only want non-zero indices to prevent floating point underflow and divide-by-
        ;zero errors.
        ipol = ipol[where(ipol gt nfavg_half-1)]
        ipol = ipol[where(ipol lt nfreqs-nfavg_half)]

    ;-----------------------------------------------------
    ;VI. Polarization Parameters \\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        ;Now we switch to the paper by Rankin and Kurtz (1970) to get the polarization
        ;parameters.
        
        ;Start with the "General Case" and note that "J" in Rankin and Kurts
        ;corresponds to the 2x2 x-y submatrix in our J.
        
        ;Recall that linearly polarized waves do not work for this analysis. Their
        ;spectral power lies only in the Jzz component, leaving Jxx, Jxy, Jyx, Jyy = 0.
        ;This causes floating point errors.
        
        ;A) Degree of Polarization
        ;   |J| = (Jxx * Jyy) - (Jxy * Jyx)
        ; => plrz = { 1 - [ (4*|J|) / (Jxx + Jyy)^2 ] }^(1/2)
        det_Js = real_part(reform(Js[0,0,*]*Js[1,1,*] - Js[1,0,*]*Js[0,1,*]))
stop
        ;Save the polarization
        if npol gt 0 then pzation[ipol,i] = sqrt( 1 - ((4*det_Js[ipol]) / real_part(reform(Js[0,0,ipol] + Js[1,1,ipol]))^2) )
        if nlin gt 0 then pzation[ilin,i] = !values.f_nan

        ;B) Coherency
        if keep_coherency then begin
            ;C = |Jxy| / sqrt( Jxx*Jyy )
            ;  = sqrt( Jxy*Jxy / (Jxx*Jyy) )
            if npol gt 0 then coherency[ipol,i] = reform( sqrt( real_part(Js[0,1,ipol]*Js[1,0,ipol]) / $
                                                                real_part(Js[0,0,ipol]*Js[1,1,ipol]) ) )
            if nlin gt 0 then coherency[ilin,i] = !values.f_nan
        endif
        
        ;C) Angle of Polarization
        ;       This is the angle between the x-axis of frame R and the major axis of the
        ;       polarization ellips. It is NOT the angle between k and B.
        if keep_angle then begin
            ;tan(2*Theta') = 2*Re[Jxx] / (Jxx - Jyy)
            ; => Theta' = 0.5 * atan( 2*Re[Jxx] / (Jxx - Jyy) )
            if npol gt 0 then begin
                tan2Theta = 2*reform( real_part(Js[0,0,ipol]) / $
                                      real_part(Js[0,0,ipol] - Js[1,1,ipol]) )
                polarization_angle[ipol,i] = 0.5 * atan(tan2Theta)
            endif
            if nlin gt 0 then polarization_angle[ilin,i] = !values.f_nan
        endif
        
        ;D) Ellipticity
        if keep_ellipticity then begin
            ;e = tan(B)
            ;sin(2B) = 2*Im[Jxy] / sqrt( (Jxx + Jyy)^2 - 4*|J| )
            ;  =>   e = tan(0.5 * asin[sin(2B)])
            ;Note that sin(2B) < 0 for clockwise rotation of the vector.
            if npol gt 0 then begin
                sin2B = 2*imaginary(reform(Js[1,0,ipol])) / $
                        sqrt( real_part(reform(Js[0,0,ipol] + Js[1,1,ipol]))^2 - 4*det_Js[ipol] )
                ellipticity[ipol,i] = tan(0.5 * asin(sin2B))
            endif
            if nlin gt 0 then ellipticity[ilin,i] = !values.f_nan
        endif
                    
    ;-----------------------------------------------------
    ;VII. Other Useful Parameters \\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        
        ;Mean Field
        if keep_mean_field then mean_field[*,i] = data_mean
        
        ;Wave Normal Vector
        if keep_khat then k_hat[*,*,i] = k_hat_temp
        
        ;Intensity in the Wave Normal Frame
        if keep_intensity then begin
            ;I = Jxx + Jyy
            intensity[*,i] = Js[0,0,*] + Js[1,1,*]
        endif
        
        ;Power Spectral Matrix
        if keep_spectral_matrix then spectral_matrix[*,*,*,i] = J_prime
        
        ;Power Spectral Matrix in the Wave Normal Frame.
        if keep_wave_normal then begin
            wave_normal_matrix[0,0,*,i] = Js[0,0,*]
            wave_normal_matrix[0,1,*,i] = Js[0,1,*]
            wave_normal_matrix[1,0,*,i] = Js[1,0,*]
            wave_normal_matrix[1,1,*,i] = Js[1,1,*]
        endif
        
        ;Angle between k and B
        ;   (calculated above)
        
        ;Time Tags
        if keep_time then time[i] = (i + ct) * n_shift * dt
  
    ;-----------------------------------------------------
    ;Set-Up the Next Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        ;Shift the fft by the desired number of points
        sindex += n_shift
        eindex += n_shift
    endfor
    
    ;Use only the desired frequencies
    if tf_interp $
        then frequencies = interp_freqs $
        else frequencies = frequencies[theseFreqs]
    
    return, pzation
end


;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
nfft = 128
nshift = 64
sample_rate = 64.0
dt = 1.0 / sample_rate
freqs = fft_freqs(nfft, dt, INYQUIST=inyquist, FNYQUIST=fnyquist)
nfreqs = n_elements(freqs) / 2

;-----------------------------------------------------
;Monochromatic Signal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;First test a monochromatic plane wave with a single frequency and a phase difference
;between the x- and y- components of !pi/5
thisFreq = 3
f = freqs[thisFreq]
Bx = mrsiggen(f, 1, 1, DC=0.76, SAMPLE_RATE=sample_rate, N_PERIODS=100, TIME=t_full)
By = mrsiggen(f, 1, 1, DC=0.76, PHASE_A=!pi/5.0, PHASE_B=!pi/5.0, SAMPLE_RATE=sample_rate, N_PERIODS=100)
Bz = mrsiggen(f, 1, 1, DC=0.76, PHASE_A=0.0, PHASE_B=0.0, SAMPLE_RATE=sample_rate, N_PERIODS=100)
B = fltarr(3, n_elements(Bx))
B = transpose([[temporary(Bx)], [temporary(By)], [temporary(Bz)]])

;Calculate the polarization. Only take the value at f = 1.2
pzation = polarization(B, nfft, dt, nshift, FREQUENCIES=frequencies, $
                       POLARIZATION_ANGLE=pa, SPECTRAL_MATRIX=spectral_matrix, $
                       TIME=t)

;Reduce everything to a line plot
pzation = transpose(pzation[thisFreq,*])
pa = transpose(pa[thisFreq,*])
spectral_matrix = reform(spectral_matrix[*,*,thisFreq,*])

;Make a new window
win1 = free_window(XSIZE=500, YSIZE=650)

;Plot the signal
pos = plot_positions([1,5], xmargin=[10,10], ygap=3)
line_plots, t_full, B, $
            TITLE='Input Signal', $
            XTICKFORMAT='(a1)', XRANGE=[t[0], t[-1]], XSTYLE=1, $
            YTITLE='Amplitude', $
            COLOR=['blue', 'green', 'red'], LEGEND=['X', 'Y', 'Z'], $
            POSITION=reform(pos[0,0,*])

;POLARIZATION
line_plots, t, transpose(pzation), /NOERASE, $
            TITLE='Polarization at f = 1.2Hz', $
            XTICKFORMAT='(a1)', XRANGE=[t[0], t[-1]], XSTYLE=1, $
            YTITLE='Polarization', $
            POSITION=reform(pos[0,1,*])

;[Bxx, Byy, Bzz] POWER
spec_mat = reform(real_part([[spectral_matrix[0,0,*,*]], $
                             [spectral_matrix[1,1,*,*]], $
                             [spectral_matrix[2,2,*,*]]]))
spec_mat_range = mrlog([min(spec_mat, max=smax), smax])
line_plots, t, mrlog(spec_mat), $
            TITLE='Bxx, Byy, Bzz Spectral Power at f=1.2Hz', /NOERASE, $
            XTICKFORMAT='(a1)', XRANGE=[t[0], t[-1]], XSTYLE=1, $
            YTITLE='log Power!Clog(nT^2 * Hz)', $
            COLOR=['blue', 'green', 'red'], LEGEND=['X', 'Y', 'Z'], $
            POSITION=reform(pos[0,2,*])

;Plot the signal
line_plots, t, transpose(pa) * 180.0/!pi, /NOERASE, $
            TITLE='Angle of Polarization for Phase Shift of -36 Degrees', $
            XTITLE='Time (seconds)', XRANGE=[t[0], t[-1]], XSTYLE=1, $
            YTITLE='Angle (deg)', YRANGE=[-45, 0], YSTYLE=1, $
            POSITION=reform(pos[0,3,*])

;-----------------------------------------------------
;A More Complex Signal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Create magnetic field component at each spectral frequency.
Bx = real_part(mrsiggen(freqs[1:inyquist], 1, 3, PHASE_A=replicate(!pi/16, inyquist), DC=1, SAMPLE_RATE=sample_rate, TIME=t))
By = real_part(mrsiggen(freqs[1:inyquist], 1, 3, DC=1, SAMPLE_RATE=sample_rate))
Bz = real_part(mrsiggen(freqs[1:inyquist], 1, 3, DC=1, SAMPLE_RATE=sample_rate))

B = fltarr(3, n_elements(Bx))
B = transpose([[temporary(Bx)], [temporary(By)], [temporary(Bz)]])
dt = t[1] - t[0]

;make the spectrogram
pzation = polarization(B, nfft, dt, nshift, FREQUENCIES=frequencies, $
                       POLARIZATION_ANGLE=pa, SPECTRAL_MATRIX=spectral_matrix)

win2 = free_window(XSIZE=500, YSIZE=650)

;make the time and data arrays. plot them
pos = plot_positions([1,4], XMARGIN=[10,15], YGAP=3)
line_plots, t, B, $
            TITLE='Input Signal', $
            XTICKFORMAT='(a1)', xrange=[t[0], t[-1]], $
            YTITLE='Amplitude', $
            COLOR=['blue', 'green', 'red'], LEGEND=['X', 'Y', 'Z'], $
            POSITION=reform(pos[0,0,*])
            
;POLARIZATION
image_plots, transpose(pzation), t, frequencies, $
             /AXES, /SCALE, POSITION=reform(pos[0,1,*]), $
             TITLE='Polarization', $
             XTICKFORMAT='(a1)', XRANGE=[t[0], t[-1]], XSTYLE=1, $
             YTITLE='Frequency (Hz)', YRANGE=[min(frequencies, max=frmax), frmax], YSTYLE=1, $
             /ADDCOLORBAR, /NOERASE, /NAN, $
             CBTITLE='Polarization'
            
;Bxx POWER
image_plots, mrlog(transpose(reform(spectral_matrix[0,0,*,*]))), t, frequencies, $
             /AXES, /SCALE, POSITION=reform(pos[0,2,*]), $
             TITLE='Bxx Power', $
             XTICKFORMAT='(a1)', XRANGE=[t[0], t[-1]], XSTYLE=1, $
             YTITLE='Frequency (Hz)', YRANGE=[min(frequencies, max=frmax), frmax], YSTYLE=1, $
             /ADDCOLORBAR, /NOERASE, /NAN, $
             CBTITLE='Power'

;ANGLE OF POLARIZATION
image_plots, transpose(pa)*180.0/!pi, t, frequencies, $
             /AXES, /SCALE, POSITION=reform(pos[0,3,*]), $
             TITLE='Angle of Polarization', $
             XTITLE='Time (s)', XRANGE=[t[0], t[-1]], XSTYLE=1, $
             YTITLE='Frequency (Hz)', YRANGE=[min(frequencies, max=frmax), frmax], YSTYLE=1, $
             /ADDCOLORBAR, /NOERASE, /NAN, $
             CBTITLE='Angle (Deg)'

wdelete, win1
wdelete, win2
end