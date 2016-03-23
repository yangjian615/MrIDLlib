pro test_time_bin
	compile_opt idl2
	on_error, 2

	;5-second time bins (e.g. EDI time tags)
	tmin = 0.0
	tmax = 3600.0
	dt   = 5.0
	nt   = (tmax - tmin) / dt
	t    = findgen(nt) * dt

	;EDP data
	;   - 3-minute period + 1.3-second period
	tmin_edp = 0.0
	tmax_edp = tmax
	dt_edp   = 1.0 / 32.0
	nt_edp   = (tmax_edp - tmin_edp) / dt_edp
	t_edp    = findgen(nt_edp) * dt_edp
	e_edp    = 5*cos(2*!pi*t_edp/180.0) + 1.5*sin(2*!pi*t_edp/1.3)
	e_edp    = rebin( reform(e_edp, 1, nt_edp), 3, nt_edp )
	
	;Locate EDP times in time bins, then put its data into those bins
	iedp  = value_locate(t, t_edp)
	chist = histogram(iedp, MIN=0, REVERSE_INDICES=ri)

	;Average data in each bin
	e_avg = fltarr(3, nt)
	for i = 0, n_elements(chist) - 1 do begin
		;Skip if there are no points in the bin
		if ri[i] eq ri[i+1] then continue
	
		;Indices of all data in current bin
		idx = iedp[ ri[ ri[i]:ri[i+1]-1 ] ]
	
		;Average the data
		e_avg[*,i] = mean( e_edp[*,idx], DIMENSION=2 )
	endfor
	
	;Plot original data
	p1 = Plot(t_edp, e_edp[0,*], $
	          COLOR  = 'Blue', $
	          LAYOUT = [1,3,1], $
	          NAME   = '$E_{x}$', $
	          TITLE  = 'E-Field - Full Time Cadence', $
	          XTITLE = 'Time (s)', $
	          YTITLE = '$E_{z}$')
	p2 = Plot(t_edp, e_edp[1,*], /CURRENT, LAYOUT=[1,3,2], YTITLE='$E_{y}$', NAME='$E_{y}$', COLOR='Green')
	p3 = Plot(t_edp, e_edp[2,*], /CURRENT, LAYOUT=[1,3,3], YTITLE='$E_{z}$', NAME='$E_{z}$', COLOR='Red')
	lg = Legend(/AUTO_TEXT_COLOR, $
	            POSITION             = [1.0, 1.0], $
	            HORIZONTAL_ALIGNMENT = 'RIGHT', $
	            VERTICAL_ALIGNMENT   = 'TOP', $
	            /RELATIVE, $
	            TARGET               = [p1, p2, p3])
	
	;Plot averaged data
	p4 = Plot(t, e_avg[0,*], $
	          COLOR  = 'Blue', $
	          LAYOUT = [1,3,1], $
	          NAME   = '$E_{x}$', $
	          TITLE  = 'E-Field - Full Time Cadence', $
	          XTITLE = 'Time (s)', $
	          YTITLE = '$E_{x}$!c(mV/m)')
	p5 = Plot(t, e_avg[1,*], /CURRENT, LAYOUT=[1,3,2], YTITLE='$E_{y}$', NAME='$E_{y}$', COLOR='Green')
	p6 = Plot(t, e_avg[2,*], /CURRENT, LAYOUT=[1,3,3], YTITLE='$E_{z}$', NAME='$E_{z}$', COLOR='Red')
	lg = Legend(/AUTO_TEXT_COLOR, $
	            POSITION             = [1.0, 1.0], $
	            HORIZONTAL_ALIGNMENT = 'RIGHT', $
	            VERTICAL_ALIGNMENT   = 'TOP', $
	            /RELATIVE, $
	            TARGET               = [p4, p5, p6])
end