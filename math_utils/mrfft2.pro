; docformat = 'rst'
;
; NAME:
;
;       MrFFT
;
; PURPOSE:
;+
;   The purpose of this program is to serve as a wrapper for IDL's FFT routine::
;       `DATA` is >= 3D::
;           IDL's FFT routine is called immediately and the result is returned.
;       `DATA` is 2D and `DIMENSION`=0::
;           A 2D tapering filter can be applied to an image.
;           - WINDOW
;           - NWIN1
;           - NWIN2
;           - ALPHA
;       Otherwise::
;           Wrapper algorithms are used.
;
;   The last case is intended to apply to time series vectors or arrays of time series
;   vectors. For example, the velocity of a particle as a 1D vector or a 3D magnetic field
;   represented as an array of 3 vectors: [Bx, By, Bz]. 
;
;   In the last case, the following capabilities are available::
;       - FFT is taken along the 1st dimension by default (TRANSPOSE if DIMENSION=2)
;       - Take the FFT of an array in sections
;       - Specify the number of points per FFT
;       - Shift the FFT window by a given amount
;       - Apply a tapering window
;       - Return time stamps of each FFT bin
;
;   Notes:
;       IDL ignores empty dimensions when the "*" operator is used, so indexing ARRAY[i,*,*]
;       is the same for 2D and 3D arrays. Furthermore, trailing shallow dimensions are
;       automatically reformed, so ARRAY[*,*,i] is the same as REFORM(ARRAY[*,*,i]). For this
;       reason, the output will be kept as [time, frequency, component]
;
;   References::
;       Percival, D. and Walden, A., Spectral Analysis for Physical Applications: Multitaper
;           and Conventional Univariate Techniques, Cambridge University Press, 1993.
;
; :Categories:
;       Math Utility
;
; :Params:
;
;       DATA:               in, required, type=fltarr
;                           The data for which a spectrogram is desired.
;       DT:                 in, optional, type=float. default=1
;                           If a scalar, the time between data samples, assumed to be
;                               uniform. If an array, the time stamp of each sample in
;                               data along `DIMENSION`.
;
; :Keywords:
;       AMPLITUDE:          out, optional, type=float
;                           A named variable to receive the signal amplitude.
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
;       DT_OUT:             out, optional, type=float
;                           A named variable to receive the sampling interval between
;                              time stamps in `TIME`. If the DT_OUT is uniform, then a
;                              scalar is returned. Otherwise, an array is returned. DT_OUT
;                              can be an array, for example, if `DT` was given as a time
;                              array in which the sampling interval changed.
;       FILLVAL:            in, optional, type=same as `DATA`
;                           A value that represents bad points within `DATA`. They will
;                               be converted to NaNs before the FFT is performed, unless
;                               the `INTERP_PCT` keyword is used.
;       FMIN:               in, optional, type=float, default=0.0
;                           The minimum positive frequency to be kept. If LOGRANGE is set,
;                               this is the minimum power of 10 (not the DC component).
;       FMAX:               in, optional, type=float, default=FNYQUIST.
;                           The maximum positive frequency to be kept. If LOGRANGE is set,
;                               this is alog10(FNYQUIST).
;       FREQUENCIES:        out, type=fltarr(NFFT)
;                           The frequency bins of the FFT
;       INTERP_PCT:         in, optional, type=float, default=0.0
;                           The maximum percentage of `FILLVAL` allowed in an FFT of size
;                               `NPTS`. Below this percent, instances of the fill value
;                               will be interpolated over. Above this percent, fill values
;                               are converted to NaNs.
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       NSHIFT:             in, optional, type=int. default=0
;                           The number of points to shift ahead after each FFT.
;       PHASE:              out, optional, type=fltarr
;                           A named variable to recieve the signal's phase.
;       T0:                 in, optional, type=float, default=0.0
;                           Time offset in seconds at which `TIME` begins. Used only if
;                               `DT` is scalar.
;       TIME:               out, type=dblarr
;                           The time associated with each Power Spectral Density slice.
;                               Where each time tag falls within the FFT interval is
;                               determined by the `TCENTER` keyword.
;       TCENTER:            in, optional, type=Boolean, default=1
;                           Indicate that the times returned in `TIME` are centered within
;                               their respective FFT bin. A value of 0 will cause the
;                               times to correspond with the beginning of the FFT packet.
;       VERBOSE:            in, optional, type=Boolean, default=0
;                           Print information about the FFT parameters to the command window.
;       WINDOW:             in. optional, type=Boolean, default=0
;                           Multiply the time series by a filtering window. This keyword
;                               is only available if `DATA` has <= 2 dimensions or if
;                               `DIMENSION` NE 0 (because I do not know how to make a
;                               window that is > 2D).
;
; :Returns:
;
;       D_FFT:              The FFT of `DATA` with dimensions organized as 
;                               [time, frequency, component]
;
; :See Also:
;   MrPSD2, MrFFT, MrPSD
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Matthew Argall 2015
;
; :History::
;   Modification History::
;       2015-01-19  -   Written by Matthew Argall
;-
function MrFFT2, data, dt, $
AMPLITUDE = amplitude, $
ALPHA = alpha, $
CENTER = center, $
DIMENSION = dimension, $
DOUBLE = double, $
DT_OUT = dt_out, $
FILLVAL = fillval, $
FMIN = fmin, $
FMAX = fmax, $
FREQUENCIES = freqs, $
INTERP_PCT = interp_pct, $
NFFT = nfft, $
NSHIFT = nshift, $
PHASE = phase, $
T0 = t0, $
TIME = time, $
TCENTER = tcenter, $
VERBOSE = verbose, $
WINDOW = window
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Accept only 1D or 2D data.
	;   - Find the dimension with maximum length
	dims  = size(data, /DIMENSIONS)
	nDims = size(data, /N_DIMENSIONS)
	type  = size(data, /TNAME)
	void  = max(dims, iMaxDim, SUBSCRIPT_MIN=iMinDim)
	if nDims gt 2 then message, 'Only 1D or 2D time series data is allowed.'

	;FFT over longest dimension
	if n_elements(dimension) eq 0 then dimension = iMaxDim + 1
	n1 = dims[dimension-1]
	if nDims eq 2 $
		then n2 = dimension le 1 ? dims[0] : dims[1] $
		else n2 = 1

	;Create defaults
	tf_center   = keyword_set(center)
	tf_fillval  = n_elements(fillval) gt 0
	tf_tcenter  = keyword_set(tcenter)
	tf_verbose  = keyword_set(verbose)
	tf_window   = keyword_set(window)
	tf_double   = n_elements(double) eq 0 ? (type eq 'DOUBLE') : keyword_set(double)
	tf_frange   = n_elements(fmin) gt 0 || n_elements(fmax) gt 0
	if n_elements(interp_pct) eq 0 then interp_pct = 0.0
	if n_elements(nfft)       eq 0 then nfft       = n1
	if n_elements(nshift)     eq 0 then nshift     = floor(nfft/2)

	;Make sure at least one FFT can be done.
	if nfft gt n1 then begin
		message, string(FORMAT='(%"NFFT (%i) is longer than the data interval (%i). ' + $
		                'Setting NFFT=%i.")', nfft, n1, n1), /INFORMATIONAL
		nfft = n1
	endif

	;Check how to window the data. Default to a "Hanning" window.
	if tf_window then begin
		if n_elements(alpha) eq 0 then alpha   = 0.5
	endif
	
	;Number of intervals
	n_int = nfft_intervals(n1, nfft, nshift)
	
;-----------------------------------------------------
; Compute Time? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	tf_calc_dt = 0
	tf_time    = arg_present(time)
	if n_elements(t0) eq 0 then t0 = 0.0

	;Sampling interval (SI)
	nt = n_elements(dt)
	case nt of
		0:    SI = 1.0
		1:    SI = dt
		n1:   tf_calc_dt = 1
		else: message, 'Invalid number of elements in DT.'
	endcase
	
;-----------------------------------------------------
; Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; The sampling interval, dt, is permitted to vary. As it does,
	; the frequency resolution, df, and the nyquist frequency, fN,
	; also change. Thus, we have to keep track of the frequency
	; bins as a function of time. We will also keep track of dt as
	; a function of time.
	;


	;Result of FFT
	;   - [frequency, time, component]
	;   - TF_CALC_DT => A time array was given and we have to compute DT
	;   - TF_TIME    => User wants us to return time stamps for each FFT interval
	d_fft  = make_array(nfft, n_int, n2, DCOMPLEX=tf_double, COMPLEX=~tf_double)
	freqs  = make_array(nfft, n_int, /FLOAT, VALUE=!values.f_nan)
	if tf_calc_dt then dt_out = fltarr(n_int)
	if tf_time then time = fltarr(n_int)
	
	nf_tot = 0
	t_0    = t0
	
;-----------------------------------------------------
; Step Through Intervals \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	istart = 0
	iend   = nfft-1
	for i = 0, n_int do begin
		;Extract subinterval
		;   - Put FFT dimension first to simplify array checking below
		dtemp = dimension le 1 ? data[istart:iend,*] : transpose(data[*,istart:iend])
	
	;-----------------------------------------------------
	; Compute Sample Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_calc_dt then begin
			;Time and sampling interval
			ttemp   = dt[istart:iend]
			delta_t = ttemp[1:*] - ttemp
		
			;Is the sampling interval consistent throughout?
			;   - A "bad" sampling interval occurs if it is 10% different
			;     from the first sampling interval
			SI   = median(delta_t)
			ibad = total( abs(temporary(delta_t) - SI) gt 0.1*SI )
			if ibad ne 0 then begin
				message, 'Sampling interval changes during FFT. Skipping.', /INFORMATIONAL
				continue
			endif
		endif
	
	;-----------------------------------------------------
	; Compute Frequencies \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Determine frequencies
		if tf_calc_dt || i eq 0 then begin
			ftemp = fft_freqs(nfft, SI, FNYQUIST=fN, INYQUIST=ifN)

			;Default frequency range
			f_min = n_elements(fmin) eq 0 ? ftemp[0]   : fmin
			f_max = n_elements(fmax) eq 0 ? ftemp[ifN] : fmax
	
			;Indices of the frequencies to keep
			if_pos = where(ftemp[0:ifN]   ge  f_min and ftemp[0:ifN]   le  f_max, npos)
			if_neg = where(ftemp[ifN+1:*] le -f_min and ftemp[ifN+1:*] ge -f_max, nneg)
			if_out = [temporary(if_pos), temporary(if_neg)+ifN+1]
			nf_out = npos + nneg
		endif

	;-----------------------------------------------------
	; Fill Value \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Look for fill values
		;   - Assume fill values are consistent across DIMENSION
		if tf_fillval then begin
			;Percentage of fill values within interval
			if finite(fillval) $
				then iFill = where(dtemp[*,0] eq fillval, nFill, COMPLEMENT=igood, NCOMPLEMENT=ngood) $
				else iFill = where(finite(dtemp[*,0]) eq 0, nFill, COMPLEMENT=igood, NCOMPLEMENT=ngood)
	
			;Interpolate/Replace fill values
			if nFill gt 0 then begin
				;Interpolate if we can.
				if pct_fill le interp_pct then begin
					if tf_verbose then print, FORMAT='(%"Interval %i is %0.1f\% fill values. Interpolating.")', i+1, pct_fill
					for j = 0, dims[1] - 1 do $
						dtemp[0,j] = interpol(dtemp[igood,j], igood, lindgen(nfft))
	
				;Replace if we must.
				endif else begin
					if tf_verbose then print, FORMAT='(%"Interval %i is %0.1f\% fill values (> %0.1f\%)")', i+1, pct_fill, interp_pct
					dtemp[iFill,*] = tf_double ? !values.d_nan : !values.f_nan
				endelse
			endif
		endif

	;-----------------------------------------------------
	; Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------

		;Create the window
		if tf_window then begin
			;Create the window function
			theWindow = hanning(nfft, ALPHA=alpha)
			if ndims eq 2 then theWindow = rebin(theWindow, [nwindow, dims[1]])
		
			;If the second dimension is empty, IDL will ignore it.
			dtemp *= theWindow
		endif

	;-----------------------------------------------------
	; FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
	
		;Compute the FFT
		fft_temp = fft(temporary(dtemp), DIMENSION=1, DOUBLE=tf_double)
	
		;TODO: Center FFT
		;   IF Keyword_Set(center) THEN ...
	
		;Eliminate frequencies
		if nf_out ne nfft then begin
			fft_temp  = fft_temp[if_out, *]
			ftemp     = ftemp[if_out]
			nf_tot   >= nf_out
		endif

	;-----------------------------------------------------
	; Outputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		
		;Save data
		;   The trailing "*" is ignored when irrelevant
		d_fft[0:nf_out-1, i, *] = temporary(fft_temp)

		if tf_calc_dt then begin
			freqs[0:nf_out-1, *] = temporary(ftemp)
			dt_out[i] = SI
		endif
		
		;Time stamp
		if tf_time then begin
			time[i] = i eq 0 ? t_0 : t_0 + SI
			t_0     = time[i]
		endif

	;-----------------------------------------------------
	; Next Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Advance forward by NSHIFT
		if i lt n_int-1 then begin
			istart += nshift
			iend   += nshift
			
		;Extend backward from the end of the array
		;   TODO: Fix time stamp for this interval
		endif else begin
			if iend eq n1-1 then begin
				i += 1
			endif else begin
				iend   = n1 - 1
				istart = n1 - nfft
			endelse
		endelse
	endfor

;-----------------------------------------------------
; Finishing Touches \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Reduce dimensions if sampling interval never changed
	;   - fN = 1 / T = 1 / (nfft*dt)
	;   - NFFT is constant, but DT is permitted to vary
	;   - If DT changes, then we must keep track of FN as a function of time
	;   - If not, then FN and F do not vary with time, and we can reduce to one copy
	if tf_calc_dt then begin
		if total( (dt_out - dt_out[0]) lt 0.1*dt_out[0] ) eq 0 then begin
			freqs  = freqs[*,0]
			dt_out = dt_out[0]
		endif
	endif else begin
		freqs  = temporary(ftemp)
		dt_out = SI
	endelse

	;Remove extra frequencies
	if nf_tot gt 0 then begin
		d_fft = d_fft[0:nf_tot-1, *, *]
		freqs = freqs[0:nf_tot-1, *]
	endif
	
	;Center times
	;   TODO: Adjust last time step
	if tf_tcenter then time[0,n_int-1] += (time[1:*] - time) / 2.0
	
	;Amplitude and phase
	if arg_present(amplitude) then amplitude = abs(d_fft)
	if arg_present(phase)     then phase     = atan(d_fft)
	
	return, d_fft
end
