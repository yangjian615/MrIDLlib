; docformat = 'rst'
;
; NAME:
;       COMPLEX_SINE
; PURPOSE:
;+
;   The idea behind this program is to visualize the polarization of left- and right-
;   handed waves.
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
pro wave_polarization
    compile_opt strictarr
    on_error, 2
    
    device, DECOMPOSED=0, RETAIN=2
    loadct, 14
    tvlct, red, green, blue, /GET

    dt = 0.1
    npts = 100

    t = findgen(npts) * dt
    
    ;Right-hand polarized waves have E = (e,  ie, 0)
    ;Left -hand polarized waves have E = (e, -ie, 0)
    Ex = cos(t)
    Ey = sin(t)
    
;---------------------------------------------------------------------
;Right-Hand Polarized Waves //////////////////////////////////////////
;---------------------------------------------------------------------

    window, XSIZE=600, YSIZE=600
    position = MrPlotLayout([1,1], [1,1], ASPECT=1.0, XMARGIN=[15,4], YMARGIN=[6,4])

    ;Create an empty set of 3D axes
    plot, [0], /NODATA, XRANGE=[-1,1], POSITION=position, $
               YRANGE=[-1,1], XTITLE='Ex', CHARSIZE=2, $
               YTITLE='Ey', TITLE='Right-Hand Polarized Waves'

    ;Step through each point in the time series
    for i=0, 99 do begin

        ;Plot the right-hand polarized waves
        plots, [Ex[i]], [Ey[i]], COLOR=10, PSYM=2

        wait, 1.0/10

    ;write_gif,'complex.gif',tvrd(),red,green,blue,/multiple,repeat_count=0,delay_time=1

    endfor
    
;---------------------------------------------------------------------
;Right-Hand Polarized Waves //////////////////////////////////////////
;---------------------------------------------------------------------

    ;Create an empty set of 3D axes
    plot, [0], /NODATA, XRANGE=[-1,1], POSITION=position, $
               YRANGE=[-1,1], XTITLE='Ex', CHARSIZE=2, $
               YTITLE='Ey', TITLE='Left-Hand Polarized Waves'

    ;Step through each point in the time series
    for i=0, 99 do begin
        
        ;Plot the left-hand polarized waves
        plots, [Ex[i]], [-Ey[i]], COLOR=50, PSYM=2

        wait, 1.0/10

    ;write_gif,'complex.gif',tvrd(),red,green,blue,/multiple,repeat_count=0,delay_time=1

    endfor

end
