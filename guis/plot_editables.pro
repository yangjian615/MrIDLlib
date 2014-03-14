; docformat = 'rst'
;
; NAME:
;       PLOT_EDITABLES
;
; PURPOSE:
;+
;       The purpose of this program is to generate a GUI for changing properties of
;       a plot. Changes are made through an {LP_EXTRA} named structure that contains
;       all of the keywords available to PLOT. See lp_extra__define.pro for more
;       information.
;
; :Categories:
;
;       GUI
;
; :Params:
;
;       LP_EXTRA:               in, optional, type=Named Struct: lp_extra__define
;                               A named structure containing all of the editable plot
;                                   properties.
;
;       GROUP_LEADER:           in, optional, type=int
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly created widget.
;
; :Returns:
;
;       PSTATE:                 A pointer to a named structure of type LP_EXTRA containing
;                                   altered plot property values.
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
;       Written by:     Matthew Argall 27 November 2012
;       12/06/2012  -   Append "selection" and "cancel" fields to the input structure. The
;                           former is used to make the Column and Row fields uneditable
;                           after a feature is changed. The latter indicates that the 
;                           cancel button was pushed. Both fields removed upon widget
;                           destruction.
;-
;*****************************************************************************************
;+
;   The purpose of this program is to serve as teh event handler for the 
;   "Titles" button. Changes::
;           TITLE
;           SUBTITLE
;           XTITLE
;           YTITLE
;           ZTITLE
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;
pro plot_editables_titles, event
    compile_opt idl2
    
    ;If a button was pressed (as opposed to released),
    if event.select eq 1 then begin
        
        ;get the state variable
        widget_control, event.top, GET_UVALUE=pstate
        
        ;Create a structure with all of the titles in it.
        titles = {title: (*pstate).title, $
                  subtitle: (*pstate).subtitle, $
                  xtitle: (*pstate).xtitle, $
                  ytitle: (*pstate).ytitle, $
                  ztitle: (*pstate).ztitle}
        
        ;Call the "Titles" widget
        pnew_titles = plottitles_gui(titles, event.top)
        
        ;if the GUI was cancelled, then return right away
        if ~ptr_valid(pnew_titles) then return
        ;Otherwise...
        
        ;update the state variable with the new titles.
        lp_extra_setProperty, *pstate, _EXTRA=*pnew_titles
        ptr_free, pnew_titles
    
        ;put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
end


;+
;   The purpose of this program is to serve as the event handler for the "Ranges" button. 
;   Changes::
;           XRANGE
;           YRANGE
;           ZRANGE
;           MIN_VALUE
;           MAX_VALUE
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;
pro plot_editables_ranges, event
    compile_opt idl2
    
    ;If a button was pressed (as opposed to released),
    if event.select eq 1 then begin
        
        ;get the state variable
        widget_control, event.top, GET_UVALUE=pstate
        
        ;Create a structure with all of the ranges in it.
        ranges = {xrange: (*pstate).xrange, $
                  yrange: (*pstate).yrange, $
                  zrange: (*pstate).zrange, $
                  max_value: (*pstate).max_value, $
                  min_value: (*pstate).min_value}
        
        ;Call the "Ranges" widget
        pnew_ranges = plotRanges_gui(ranges, event.top)
        
        ;if the GUI was cancelled, then retrun
        if ~ptr_valid(pnew_titles) then return
        ;Otherwise...
        
        ;update the state variable with the new ranges.
        lp_extra_setProperty, *pstate, _EXTRA=*pnew_ranges
        ptr_free, pnew_ranges
        
        ;put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
end


;+
;   The purpose of this program is to serve as the event handler for the
;   "Colors" button. Changes::
;           FGCOLOR
;           BGCOLOR
;           COLOR
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro plot_editables_colors, event
    compile_opt idl2
    
    ;If a button was pressed (as opposed to released),
    if event.select eq 1 then begin
        
        ;get the state variable
        widget_control, event.top, GET_UVALUE=pstate
        
        ;Join the legend values into a single string, separated by a comma
        color_string = strjoin((*pstate).legend, ', ')
        
        ;Create a structure with all of the ranges in it.
        old_colors = {fgcolor: (*pstate).fgcolor, $
                      bgcolor: (*pstate).bgcolor, $
                      color: color_string}
        
        ;Call the "Colors" widget
        pnew_colors = plotColors_gui(old_colors, event.top)
        
        ;if the colors GUI was canceled, then return
        if ~ptr_valid(pnew_colors) then return
        ;Otherwise...
        
        ;Break apart the COLORS string and turn it into an array
        (*pnew_colors).color = strsplit((*pnew_colors).color, ', ', /EXTRACT)
        
        ;update the state variable with the new titles.
        lp_extra_setProperty, *pstate, _EXTRA=*pnew_colors
        ptr_free, pnew_colors
        
        ;put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
end


;+
;   The purpose of this program is to serve as the event handler for the "Legend" button.
;   Changes::
;           LEGEND
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro plot_editables_legend, event
    compile_opt idl2
    
    ;If a button was pressed (as opposed to released),
    if event.select eq 1 then begin
        
        ;get the state variable
        widget_control, event.top, GET_UVALUE=pstate
        
        ;Call the "Legend" widget
        pnew_legend = plotLegend_gui((*pstate).legend, event.top)
        
        ;if the legend GUI was canceled, then return
        if ~ptr_valid(pnew_legend) then return
        ;otherwise...
        
        ;update the state variable with the new titles.
        lp_extra_setProperty, *pstate, LEGEND=*pnew_legend
        ptr_free, pnew_legend
        
        ;put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
end


;+
;   The purpose of this program is to serve as the event handler for the "Styles" button.
;   Changes::
;           XSTYLE
;           YSTYLE
;           ZSTYLE
;           GRIDSTYLE
;           LINESTYLE
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro plot_editables_styles, event
    compile_opt idl2
    
    ;If a button was pressed (as opposed to released),
    if event.select eq 1 then begin
        
        ;get the state variable
        widget_control, event.top, GET_UVALUE=pstate
        
        ;Create a structure with all of the ranges in it.
        styles = {xstyle: (*pstate).xstyle, $
                  ystyle: (*pstate).ystyle, $
                  zstyle: (*pstate).zstyle, $
                  linestyle: (*pstate).linestyle, $
                  xgridstyle: (*pstate).xgridstyle, $
                  ygridstyle: (*pstate).ygridstyle, $
                  zgridstyle: (*pstate).zgridstyle}
        
        ;Call the "Styles" widget
        pnew_styles = plotStyles_gui(styles, event.top)
        
        ;If the styles GUI was cancelled then return
        if ~ptr_valid(pnew_styles) then return
        ;otherwise...

        ;update the state variable with the new titles.
        lp_extra_setProperty, *pstate, _EXTRA=*pnew_styles
        ptr_free, pnew_styles
        
        ;put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
end


;+
;   The purpose of this program is to serve as the event handler for the "Symbols" button.
;   Changes::
;           PSYM
;           SYMSIZE
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro plot_editables_symbols, event
    compile_opt idl2
    
    ;If a button was pressed (as opposed to released),
    if event.select eq 1 then begin
        
        ;get the state variable
        widget_control, event.top, GET_UVALUE=pstate
                
        ;Create a structure with all of the ranges in it.
        symbols = {psym: (*pstate).psym, $
                   symsize: (*pstate).symsize}
        
        ;Call the "Styles" widget
        pnew_symbols = plotSymbols_gui(symbols, event.top)
        
        ;If the symbols GUI was cancelled, then return
        if ~ptr_valid(pnew_symbols) then return
        ;otherwise...
        
        ;update the state variable with the new titles.
        lp_extra_setProperty, *pstate, _STRICT_EXTRA=*pnew_symbols
        ptr_free, pnew_symbols
        
        ;put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
end


;+
;   The purpose of this program is to serve as the event handler for the "OK" button.
;   Destroy the widget.
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro plot_editables_ok, event
    compile_opt idl2
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;+
;   The purpose of this program is to serve as the event handler for the "Cancel" button.
;   The widget is destroyed and the state variable freed.
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro plot_editables_cancel, event
    compile_opt idl2
    
    ;Free the state variable pointer so that it is not valid
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /destroy
    
end


;+
;   The purpose of this program is to cleanup once the widget is destroyed.
;
; :Private:
;-
pro plot_editables_cleanup, event
    compile_opt idl2
    ;do nothing    
end


;+
;       The purpose of this program is to generate a GUI for changing properties of
;       a plot. Changes are made through an {LP_EXTRA} named structure that contains
;       all of the keywords available to PLOT. See lp_extra__define.pro for more
;       information.
;
; :Params:
;
;       LP_EXTRA:               in, optional, type=Named Struct: lp_extra__define
;                               A named structure containing all of the editable plot
;                                   properties.
;
;       GROUP_LEADER:           in, optional, type=int
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly created widget.
;
; :Returns:
;
;       PSTATE:                 A pointer to a named structure of type LP_EXTRA containing
;                                   altered plot property values. Returns a !NULL pointer
;                                   if the Cancel button is pressed.
;-
function plot_editables, lp_extra, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;1 parameter, not a structure => must be group leader. Create default structure
    if n_params() eq 1 and size(lp_extra, /TYPE) ne 8 $
        then group_leader = temporary(lp_extra)

    ;if LP_EXTRA was not given, fill a structure with IDL's default values.        
    if n_elements(lp_extra) eq 0 then lp_extra = lp_extra_defaults()

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. PLOT_EDITABLES is called by other
	;blocking widgets, so if a group_leader is given, then make PLOT_EDITABLES modal.
	if n_params() eq 2 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Editables', /COLUMN, $
	                      XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
	                      /MODAL)
	endif else begin
	    no_block = 0
	    tlb = widget_base(TITLE='Editables', /COLUMN, XOFFSET=200, YOFFSET=100, $
	                      UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse

;---------------------------------------------------------------------
;Create the Plot Editables Buttons ///////////////////////////////////
;---------------------------------------------------------------------
    
    ;Create base to hold each of the buttons. Make each button individually. CW_BGROUP() is
    ;not used because I want an event handler for each button.
    buttonBase = widget_base(tlb, COL=1, FRAME=5)	

    ;Editable features buttons
    titlesButton = widget_button(buttonBase, /ALIGN_CENTER, UNAME='titles', VALUE='Titles', $
                                 EVENT_PRO='plot_editables_titles')
    rangesButton = widget_button(buttonBase, /ALIGN_CENTER, UNAME='ranges', VALUE='Ranges', $
                                 EVENT_PRO='plot_editables_ranges')
    colorsButton = widget_button(buttonBase, /ALIGN_CENTER, UNAME='colors', VALUE='Colors', $
                                 EVENT_PRO='plot_editables_colors')
    legendButton = widget_button(buttonBase, /ALIGN_CENTER, UNAME='legend', VALUE='Legend', $
                                 EVENT_PRO='plot_editables_legend')
    stylesButton = widget_button(buttonBase, /ALIGN_CENTER, UNAME='styles', VALUE='Styles', $
                                 EVENT_PRO='plot_editables_styles')
    symbolButton = widget_button(buttonBase, /ALIGN_CENTER, UNAME='symbol', VALUE='Symbols', $
                                 EVENT_PRO='plot_editables_symbols')

    ;create a seperate base for the "ok" and "cancel" buttons
    okBase = widget_base(tlb, /COLUMN)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='plot_editables_ok')
    cancelButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                                 EVENT_PRO='plot_editables_cancel')

       
;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

    ;if OREF is a valid object reference, then store it as the state variable
    state = create_struct(lp_extra, 'selection', 0, 'cancel', 0)
    pstate = ptr_new(state, /no_copy)
	
	;set the state variable as the tob level base's user value for easy access
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plot_editables', tlb, cleanup='plot_editables_cleanup', $
	          event_handler='plot_editables_type', NO_BLOCK=no_block
        
    ;return the updated plotting keywords 
    return, pstate
end