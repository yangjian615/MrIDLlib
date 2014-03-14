; docformat = 'rst'
;
; NAME:
;       COMPLEX_SINE
;
; PURPOSE:
;+
;   The idea behind this program is to illustrate why, when a real time-series signal is
;   fourier transformed into the frequency domain, the positive and negative frequencies
;   have the same amplitudes.
;
;=========================================================================================
;
;   The most general periodic wave is the superposition of sines and cosines at various
;   frequencies.
;
;       s = a0/2 + \sum_{n=1}^{\infty} \left( a_{n} \cos{2 \pi n t / T} +
;                                             b_{n} \sin{2 \pi n t / T} \right)
;
;   This can be written in complex form as (where f = n/T)
;
;       s = \sum_{-\infty}^{+\infty} G_{n} \exp^{i 2 \pi f n t}
;
;   and G_{n} is
;
;                   (a_{n} - ib_{n}) / 2             for n \ge 1
;       G_{n} =     a0/2                             for n = 0
;                   (a_{|n|} + ib_{|n|}) / 2         for n \le -1
;
;   Note the the exponential can be written as
;
;       exp^{+i \theta} = \cos{\theta} + i\sin{\theta}
;       exp^{-i \theta} = \cos{\theta} - i\sin{\theta}
;
;   since cosine is even and sine is odd... \sin{-\theta} = -\sin{\theta}.
;
;=========================================================================================
;
;   It can be seen that the mathematically imposed complex nature of the waves enters
;   into the coefficients.
;
;       \cos{i \theta} = \frac{\exp{i \theta} + \exp{-i \theta}} {2}
;       \sin{i \theta} = \frac{\exp{i \theta} - \exp{-i \theta}} {2i}
;
;   Thus, the positive and negative frequencies are the same (both are \theta). If we
;   factor in the coefficients, G_{n}, we see that the same coefficient multiplies the
;   positive and negative frequency component. Thus, the positive and negative frequencies
;   have equal amplitudes for a real signal.
;
;=========================================================================================
;
;   In "movie" that follows, cosine and sine components of the wave will be plotted on the
;   real and complex axes, respecively, over time. It will be shown that the sum of the
;   real and imaginary components always lies along the real-axis. The implications of
;   this are that the amplitudes of the positive and negative components are equal,
;   otherwise, the sum would leave the real axis.
;
; :Categories:
;       Data Visualization
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
;   Modification History::
;       10/16/2013  -   Written by Matthew Argall
;-
pro complex_sine
    compile_opt strictarr
    on_error, 2
    
    device, DECOMPOSED=0, RETAIN=2
    loadct, 14
    tvlct, red, green, blue, /GET

    dt = 0.1
    npts = 100

    t = findgen(npts) * dt
      cos_theta =  cos(t)
     isin_theta =  sin(t)
    _isin_theta = -sin(t)
    
;---------------------------------------------------------------------
;View of the Real and Imaginary Components vs. Time //////////////////
;---------------------------------------------------------------------

    ;Create an empty set of 3D axes
    shade_surf, dist(2), /NODATA, XRANGE=[0,10], YRANGE=[-1,1],$
               ZRANGE=[-1,1], XTITLE='time', CHARSIZE=2, /SAVE,$
               YTITLE='Real', ZTITLE='Complex'

    ;Step through each point in the time series
    for i=0, 99 do begin

        ;Plot the positive freuqency component
        plots, [t[i]], [cos_theta[i]], [isin_theta[i]], /T3D, COLOR=10, PSYM=2
        
        ;Plot the negative frequency component
        plots, [t[i]], [cos_theta[i]], [_isin_theta[i]], /T3D, COLOR=50, PSYM=2
        
        ;Plot the average of the positive and negative frequencies
        plots, [t[i]], [(cos_theta[i] + cos_theta[i])/2.0], [(isin_theta[i] + _isin_theta[i])/2.0], $
               /T3D, COLOR=20, PSYM=2

        wait, 1.0/10

    ;write_gif,'complex.gif',tvrd(),red,green,blue,/multiple,repeat_count=0,delay_time=1

    endfor
    
;---------------------------------------------------------------------
;View of the Real signal vs. Time ////////////////////////////////////
;---------------------------------------------------------------------

    ;Create an empty set of 3D axes
    shade_surf, dist(2), /NODATA, XRANGE=[0,10], YRANGE=[-1,1],$
               ZRANGE=[-1,1], XTITLE='time', CHARSIZE=2, /SAVE,$
               YTITLE='Real', ZTITLE='Complex'

    ;Step through each point
    for i=0, 99 do begin

        ;
        plots, [t(i)], [x1(i)], [y1(i)], /T3D, COLOR=10, PSYM=2
        plots, [t(i)], [x2(i)], [y2(i)], /T3D, COLOR=50, PSYM=2
        plots, [t(i)], [x1(i)], [0],     /T3D, COLOR=20, PSYM=2

        wait, 1.0/10

    ;write_gif,'complex.gif',tvrd(),red,green,blue,/multiple,repeat_count=0,delay_time=1

    endfor

    ;write_gif,'complex.gif',/close

end
