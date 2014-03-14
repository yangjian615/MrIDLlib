; docformat = 'rst'
;
; NAME:
;       PLOT_HODOGRAMS
;
; PURPOSE:
;+
;       The purpose of this program is to create a hodogram by plotting the first
;       first component of an array of vectors against the other two. This is typically
;       used with magnetic field data, especially after having performed minimum
;       variance analysis.
;
; :Categories:
;       Graphics Utility, Multi-Spacecraft Methods
;
; :Examples:
;
; :Params:
;       X:              in, required, type="fltarr(3,*)"
;                       The array of vectors for which a hodogram is to be made
;
; :Keywords:
;       DEVICE:         in, optional, type=Boolean, default=0
;                       Indicate that the positions provided are in device coordinates
;       EXTRA:          in, optional, type=structure
;                       A structure of keywords to be passed to PLOT
;       NOERASE:        in, optional, type=Boolean, default=0
;                       Do not erase the contents of the current window.
;       NORMAL:         in, optional, type=Boolean, default=1
;                       Indicate that the positions provided are in normal coordinates.
;       POSITIONS:      in, optional, type="fltarr(4,2)", default="plot_positions([2,1])"
;                       The positions of the two plots to be made.
;       TITLES:         in, optional, type=strarr(2), default=strarr(2)
;                       The titles of both hodograms to be made.
;       XTITLES:        in, optional, type=strarr(2), default=strarr(2)
;                       The x-titles of both hodograms to be made
;       YTITLE:         in, optional, type=string, default=''
;                       The y-title of the hodogram.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       08/25/2012  -   Created by Matthew Argall
;-
pro plot_hodograms, x, $
;Keywords:
DEVICE = device, $
extra = extra, $
NOERASE = noerase, $
NORMAL = normal, $
POSITIONS = positions, $
TITLES = titles, $
XTITLES = xtitles, $
YTITLE = ytitle
    compile_opt idl2

;------------------------------
;CHECK Keywords////////////////
;------------------------------

  ;if the titles are not given, make them up.
  ;One title for each graph. If only one is provided, then
  ;duplicate it on the other graph
  if n_elements(titles) eq 0 then begin
      titles = ['Hodogram of N vs. M', 'Hodogram of N vs. L']
  endif else if n_elements(titles) eq  1 then begin
      titles = [titles[0], titles[0]]
  endif else if n_elements(titles) gt 2 then begin
      message, 'titles must be a 2 element array'
  endif

  ;the x component is N or M in either graph, so two
  ;titles must be provided. If only one is provided, duplicate it
  if n_elements(xtitles) eq 0 then begin
      xtitles = ['Minimum Varying', 'Intermediately Varying']
  endif else if n_elements(xtitles) eq 1 then begin
      xtitles = [xtitles[0], xtitles[0]]
  endif else if n_elements(xtitles) gt 2 then begin
      message, 'xtitles must contain 2 titles'
  endif 

  ;the y component is the same for each graph, so only
  ;1 title is needed.
  if n_elements(ytitle) eq 0 then begin
      ytitle = 'Maximum Varying'
  endif else if n_elements(ytitle) gt 1 then begin
      message, 'ytitle must be 1 element'
  endif
  
  ;determine whether the position is given in device or normal
  ;coordinates. Default to normal coordinates.
  if n_elements(normal) eq 0 and n_elements(device) eq 0 then begin
      normal = 1
      device = 0
  endif
  if keyword_set(normal) then device = 0
  if keyword_set(device) then normal = 0

  ;set the plot positions. since position was not defined, normal
  ;coordinates can be chosen regardless of any keyword.
  sz = size(positions)
  if n_elements(positions) eq 0 then begin
      pos = plot_positions([2,1], gap=[0.0, 0.06], /normal)
      positions = [[reform(pos[0,0,*])], [reform(pos[1,0,*])]]
      normal = 1
      device = 0
  endif else if sz[0] ne 2 && sz[1] ne 4 and sz[2] ne 2 then begin
      message, 'position must be array of 2 4-element row vectors'
  endif

  if n_elements(noerase) eq 0 then begin
      noerase = [0, 1]
  endif else if n_elements(noerase) ne 2 then begin
      message, 'noerase must have 2 elements'
  endif

;------------------------------
;START PLOTTING////////////////
;------------------------------

  ;find the max and min of each component
  max_n = max(x[0,*], min=min_n)
  max_m = max(x[1,*], min=min_m)
  max_l = max(x[2,*], min=min_l)

  ;calculate the average value of each component
  x_avg = [mean(x[0,*]), mean(x[1,*]), mean(x[2,*])]

  ;make the hodogram of L vs. N. Draw a vertical line at the
  ;average value of N
  plot, x[0,*], x[2,*], noerase=noerase[0], $
        position=positions[*,0], normal=normal, device=device, $
        title=titles[0], $
        xtitle=xtitles[0], xrange=[min_n ,max_n], $
        ytitle=ytitle, ystyle=1
  plots, [x_avg[0],x_avg[0]], [min_l, max_l], linestyle=2, color=red

  ;make the hodogram of L vs. M. Draw a vertical line at the
  ;average value of M
  plot, x[1,*], x[2,*], noerase=noerase[1], $
        position=positions[*,1], normal=normal, device=device, $
        title=titles[1], $
        xtitle=xtitles[1], xrange=[min_m, max_m], $
        ytitle=ytitle, ystyle=1
  plots, [x_avg[1],x_avg[1]], [min_l, max_l], linestyle=2, color=red

end
