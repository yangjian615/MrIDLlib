; docformat = 'rst'
;
; NAME:
;       L_WHISTLER
;
; PURPOSE:
;+
;   The idea behind this program is to plot the left-hand whistler mode waves in a cold,
;   uniform, magnetized plasma. 
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
pro l_whistler
    compile_opt strictarr
    on_error, 2
    
    
;---------------------------------------------------------------------
;View of the Real and Imaginary Components vs. Time //////////////////
;---------------------------------------------------------------------

    w_pH  = w_plasma(1.5, 'H+')
    w_pHe = w_plasma(1.5, 'He+')
    w_pe  = w_plasma(1.5, 'e-')
    w_ce  = w_cyclotron(160, 'e-')
    w_cH  = w_cyclotron(160, 'H+')
    w_cHe = w_cyclotron(160, 'He+')
    w1 = linspace(0, 1.5*w_cH, 1000)
    w2 = linspace(1.5*w_cH, 1.5*w_ce, 1000)
    w = [w1,w2]

    ;Index of refraction when n > 0
    R_e  = w_pe^2  / (w * (w - w_ce))
    R_H  = w_pH^2  / (w * (w + w_cH))
    R_He = w_pHe^2 / (w * (w + w_cHe))
    
    ;Index of refraction when n > 0
    L_e  = w_pe^2  / (w * (w + w_ce))
    L_H  = w_pH^2  / (w * (w - w_cH))
    L_He = w_pHe^2 / (w * (w - w_cHe))

    R = 1.0 - (R_e + R_H); + R_He)
    L = 1.0 - (L_e + L_H + L_He)

    R_position = MrPlotLayout([2,1], [1,1])
    L_position = MrPlotLayout([2,1], [2,1])
    plot, w, R, XRANGE=[0.1,100*w_ce], $
          XTITLE='Frequency', YTITLE='Index of Refraction', XLOG=1, XSTYLE=1, $
          POSITION=R_position
    
    ;Put a horizontal line at n = 0
    plots, 10^!X.CRange, [0, 0], LINESTYLE=1

    ;Put a vertical line at w_ce, w_cH and w_cHe
    plots, [w_ce,  w_ce],  !Y.CRange, LINESTYLE=1
    plots, [w_cH,  w_cH],  !Y.CRange, LINESTYLE=1
    plots, [w_cHe, w_cHe], !Y.CRange, LINESTYLE=1
          
    plot, w, L, XRANGE=[0.1,100*w_ce], /NOERASE, $
          XTITLE='Frequency', YTITLE='Index of Refraction', XLOG=1, XSTYLE=1, $
          POSITION=L_position
    
    ;Put a horizontal line at n = 0
    plots, 10^!X.CRange, [0, 0], LINESTYLE=1

    ;Put a vertical line at w_ce, w_cH and w_cHe
    plots, [w_ce,  w_ce],  !Y.CRange, LINESTYLE=1
    plots, [w_cH,  w_cH],  !Y.CRange, LINESTYLE=1
    plots, [w_cHe, w_cHe], !Y.CRange, LINESTYLE=1
end
