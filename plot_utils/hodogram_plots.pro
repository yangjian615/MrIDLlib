; docformat = 'rst'
;
; NAME:
;       HODOGRAM_PLOTS
;
; PURPOSE:
;+
;       The purpose of this program is to perform a minimum or maximum variance analysis
;       on an array of vectors then create a hodogram by plotting the minimally varying
;       component against the maximally and intermediately varying components, separately.
;
; :Categories:
;       Graphics Utility, Multi-Spacecraft Methods
;
; :Examples:
;
; :Params:
;       X_IN:           in, required, type="fltarr(3,*)"
;                       The array of vectors for which a hodogram is to be made
;
; :Keywords:
;       ANNOTATE:       in, optional, type=Boolean, default=0
;                       Print the eigenvalues, eigenvectors, and the average of the data
;                           below the hodogram.
;       DEVICE:         in, optional, type=Boolean, default=0
;                       Indicate that the positions provided are in device coordinates
;       EIGENVALUES:    out, optional, type=fltarr(3)
;                       The eigenvalues returned by MVA
;       EIGENVECTORS:   out, optional, type="fltarr(3,3)"
;                       The eigenvectors returned by MVA. EIGENVECTORS(*,0) correspond to
;                           `EIGENVALUE(0)`.
;       EXTRA:          in, optional, type=structure
;                       A structure of keywords to be passed to PLOT
;       MAXIMUM:        in, optional, type=Boolean, default=0
;                       Indicate that maximum variance analysis is to be performed.
;       MINIMUM:        in, optional, type=Boolean, default=1
;                       Indicate that a minimum variance analysis is to be performed.
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
pro hodogram_plots, x_in, $
;KEYWORDS
ANNOTATE = annotate, $
DEVICE = device, $
EIGENVALUES = eigenvalues, $
EIGENVECTORS = eigenvectors, $
EXTRA = extra, $
MAXIMUM = maximum, $
MINIMUM = minimum, $
NOERASE = noerase, $
NORMAL = normal, $
POSITIONS = positions, $
TITLES = titles, $
XTITLES = xtitles, $
YTITLE = ytitle

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
      create an offset below the hodograms to annotate the graph
      if keyword_set(annotate) then begin
          offset = float(!d.y_ch_size * 1.5)/float(!d.y_size)
          bottom = 5 * offset
      endif else begin
          bottom = 0.05
      endelse

      ;get the plot positions
      pos = plot_positions([2,1], gap=[0.06, 0.0], $
                           margins=[0.05, bottom, 0.05, 0.05], /normal)

      ;rearrange the plot positions to be a [4,2] array
      ;specify normal coordinates
      positions = [[reform(pos[0,0,*])], [reform(pos[1,0,*])]]
      normal = 1
      device = 0
  ;if positions is supplied, but is not a [4,2] array, then throw an
  ;error message
  endif else if sz[0] ne 2 && sz[1] ne 4 and sz[2] ne 2 then begin
      message, 'position must be array of 2 4-element row vectors'
  ;Otherwise...
  endif else begin
      ;positions is supplied and everything is good. Now, just set
      ;an offset below the graph to place the annotations.
       if keyword_set(annotate) then begin
          offset = float(!d.y_ch_size * 1.5)/float(!d.y_size)
          bottom = positions[1,0] - 2.25 * offset

          ;check if there is enough space below the graph
          if bottom lt 4 * offset then $
            message, 'Increase margins to fit annotation on graph', /informational
      endif
  endelse

  ;check whether to erase the graphs
  if n_elements(noerase) eq 0 then begin
      noerase = [0, 1]
  endif else if n_elements(noerase) ne 2 then begin
      message, 'noerase must have 2 elements'
  endif

  ;if requested, perform a maximum or minimum variance analysis
  if keyword_set(maximum) then begin
      eigenvectors = min_var_cluster(x_in, /maximum, eigvals=eigenvalues)
      x_out = rotate_vector(x_in, eigenvectors) 
  endif else if keyword_set(minimum) then begin
      eigenvectors = min_var_cluster(x_in, eigvals=eigenvalues)
      x_out = rotate_vector(x_in, eigenvectors)
  endif else begin
      x_out = x_in
  endelse

  ;if annotate, then check to see if the eigenvectors and eigenvalues
  ;are properly given
  if keyword_set(annotate) then begin
      if n_elements(eigenvalues) ne 3 then begin
          message, 'eigenvalues must be a 3 element vector'
      endif

      sz = size(eigenvectors)
      if eigenvectors is not a 3x3 array, then send a message
      if sz[0] ne 2 || sz[1] ne 3 || sz[2] ne 3 then begin
          message, 'eigenvectors must be a 3x3 array of 3 row vectors'
      endif
  endif

;---------------------------------------------------------------------
;START PLOTTING///////////////////////////////////////////////////////
;---------------------------------------------------------------------

  ;find the max and min of each component
  max_n = max(x_out[0,*], min=min_n)
  max_m = max(x_out[1,*], min=min_m)
  max_l = max(x_out[2,*], min=min_l)

  ;calculate the average value of each component
  x_avg = [mean(x_out[0,*]), mean(x_out[1,*]), mean(x_out[2,*])]

  ;make the hodogram of L vs. N. Draw a vertical line at the
  ;average value of N
  plot, x_out[0,*], x_out[2,*], noerase=noerase[0], $
        position=positions[*,0], normal=normal, device=device, $
        title=titles[0], $
        xtitle=xtitles[0], xrange=[min_n ,max_n], $
        ytitle=ytitle, ystyle=1
  plots, [x_avg[0],x_avg[0]], [min_l, max_l], linestyle=2, color=red

  ;make the hodogram of L vs. M. Draw a vertical line at the
  ;average value of M
  plot, x_out[1,*], x_out[2,*], noerase=noerase[1], $
        position=positions[*,1], normal=normal, device=device, $
        title=titles[1], $
        xtitle=xtitles[1], xrange=[min_m, max_m], $
        ytitle=ytitle, ystyle=1
  plots, [x_avg[1],x_avg[1]], [min_l, max_l], linestyle=2, color=red

  ;annotate the plot with the eigenvalues, eigenvectors, and average values
  if keyword_set(annotate) then begin
      ;create the annotation strings
      lambda = greek('lambda')
      line1 = string(' ', format='(8x, a1)') + $
              string('x', 'y', 'z', 'B', format='(4(10x, a1))')
      line2 = string('i', eigenvalues[0], eigenvectors(*,0), x_avg[0], $
                     format='(a1, 3x, f9.4, 3x, 3(f7.4, 3x), f6.2)')
      
      line3 = string('j', eigenvalues[1], eigenvectors(*,1), x_avg[1], $
                     format='(a1, 3x, f9.4, 3x, 3(f7.4, 3x), f6.2)')
      
      line4 = string('k', eigenvalues[2], eigenvectors(*,2), x_avg[2], $
                     format='(a1, 3x, f9.4, 3x, 3(f7.4, 3x), f6.2)')
      
      ;write the strings onto the graph
      xyouts, 0.25, bottom-0.5*offset, line1, /normal
      xyouts, 0.25, bottom-0.5*offset - offset, line2, /normal
      xyouts, 0.25, bottom-0.5*offset - offset*2.0, line3, /normal
      xyouts, 0.25, bottom-0.5*offset - offset*3.0, line4, /normal
  endif


end
