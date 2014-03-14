;+
; NAME:
;       PLOTSTYLES_GUI
;
; PURPOSE:
;
;       A GUI for editing plot styles.
;
; AUTHOR:
;
;       Matthew Argall
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 Collge Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; CATEGORY:
;
;       GUI
;
; CALLING SEQUENCE:
;
;       titles = plotStyles_gui(titles [, group_leader])
;
; INPUT POSITIONAL PARAMETERS:
;
;       STYLES              -   In. Optional. Type=Struct
;                               A structure containing the current xstyle, ystyle, zstyle,
;                                   linestyle, xgridstyle, ygristyle, and zgridstyle of a 
;                                   plot. E.g.,
;                                       STYLES = {xstyle: 0, $
;                                                 ystyle: 0, $
;                                                 zstyle: 0, $
;                                                 linestyle: 0, $
;                                                 xgridstyle: 0, $
;                                                 ygridstyle: 0, $
;                                                 zgridstyle: 0}
;
;       GROUP_LEADER        -   In. Optional. Type=Int.
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly-created widget.
;
; RETURN VALUE:
;
;       PSTATE              -   A pointer to a copy of STYLES, but with the fields changed.
;                               PSTATE is an invalid pointer if 'Cancel' is pressed.
;       
; MODIFICATION HISTORY:
;
;       Written by:     Matthew Argall 27 November 2012
;       12/08/2012  -   STYLES is now optional
;-
;*****************************************************************************************

;
; NAME:
;       PLOTSTYLES_GUI_EVENTS
;
; PURPOSE:
;
;       Event handler for the [XYZ]STYLE buttons.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
function plotStyles_gui_axstyles, event
    compile_opt idl2

    ;If a button is pressed or released
    if event.select eq 0 || event.select eq 1 then begin
        ;get the state variable.
        widget_control, event.top, GET_UVALUE=pstate
        
        ;Retrieve the user-name and user-value of the widget that caused the event
        widget_control, event.id, GET_UVALUE=uvalue
        uname = widget_info(event.id, /UNAME)
    endif
    
    ;if a button is released
    if event.select eq 0 then begin
        ;subtract the [xyz]style bit from the current setting
        case uname of
            'xstyle': (*pstate).xstyle -= event.value
            'ystyle': (*pstate).ystyle -= event.value
            'zstyle': (*pstate).zstyle -= event.value
        endcase

        ;Put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    
    ;If a button was pushed
    endif else if event.select eq 1 then begin
        ;add the [xyz]style bit from the current setting
        case uname of
            'xstyle': (*pstate).xstyle += event.value
            'ystyle': (*pstate).ystyle += event.value
            'zstyle': (*pstate).zstyle += event.value
        endcase

        ;Put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
    
    
    return, 1
end


;
; NAME:
;       PLOTSTYLES_GUI_EVENTS
;
; PURPOSE:
;
;       Event handler for the LINESTYLE and [XYZ]GRIDSTYLE drop-lists.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
pro plotStyles_gui_lgstyles, event
    compile_opt idl2

    ;get the state variable.
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Retrieve the user-name and user-value of the widget that caused the event
    widget_control, event.id, GET_UVALUE=lgvalues
    uname = widget_info(event.id, /UNAME)
    
    ;subtract the [xyz]style bit from the current setting
    case uname of
        'linestyle': (*pstate).linestyle = lgvalues[event.index]
        'xgridstyle': (*pstate).xgridstyle = lgvalues[event.index]
        'ygridstyle': (*pstate).ygridstyle = lgvalues[event.index]
        'zgridstyle': (*pstate).zgridstyle = lgvalues[event.index]
    endcase

    ;Put the state variable back
    widget_control, event.top, SET_UVALUE=pstate
end


;
; NAME:
;       PLOTSTYLES_GUI_OK
;
; PURPOSE:
;
;       Event handler for the "OK" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
pro plotStyles_gui_ok, event
    compile_opt idl2
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;
; NAME:
;       PLOTSTYLES_GUI_CANCEL
;
; PURPOSE:
;
;       Event handler for the "Cancel" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
pro plotStyles_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable pointer
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /destroy
    
end


pro plotStyles_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


function plotStyles_gui, styles, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;1 Parameter, not structure => group leader.
    if n_params() eq 1 and size(styles, /TYPE) ne 8 then group_leader = temporary(styles)
        
    ;Create default SYTLES structure.
    if n_elements(styles) eq 0 $
        then styles = {xstyle: 0, $
                       ystyle: 0, $
                       zstyle: 0, $
                       linestyle: 0, $
                       xgridstyle: 0, $
                       ygridstyle: 0, $
                       zgridstyle: 0}

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. PLOTTITLE_GUI is called by other
	;blocking widgets, so if a group_leader is given, then make PLOTTITLE_GUI modal.
	if n_params() eq 2 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Plot Styles', /COLUMN, $
	                      XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
	                      /MODAL)
	endif else begin
	    no_block = 0
	    tlb = widget_base(TITLE='Plot Styles', /COLUMN, XOFFSET=200, YOFFSET=100, $
	                      UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse
       
;---------------------------------------------------------------------
;Convert Inputs to Strings ///////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Axis Styles
    axStyles = ['Force exact range.', $
                'Extend axis range.', $
                'Suppress entire axis.', $
                'Suppress box style axis.', $
                'Inhibit Y-minimum=0.']
    axValues = [1, 2, 4, 8, 16]
    
    ;Make a list of the buttons that are initially depressed.
    xVals = (axValues and styles.xstyle) gt 0
    yVals = (axValues and styles.ystyle) gt 0
    zVals = (axValues and styles.zstyle) gt 0
    
    ;Line and Grid Styles
    lineStyles = ['Solid', $
                  'Dotted', $
                  'Dashed', $
                  'Dash Dot', $
                  'Dash Dot Dot', $
                  'Long Dashes']
    lgValues = [0, 1, 2, 3, 4, 5]
    
;---------------------------------------------------------------------
;Create an *Ranges fields ////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Create base to hold each of the buttons. Make each button individually. CW_BGROUP() is
    ;not used because I want an event handler for each button.
    columnBase = widget_base(tlb, /COLUMN)
    
    ;Put a label across the top of the widget.
    stylesLabel = widget_label(tlb, VALUE='Styles', XSIZE=300, FRAME=4, /ALIGN_CENTER)
    
    ;Create columns for the axis styles
    axStyleBase = widget_base(tlb, COLUMN=3)
    xStyleBGroup = cw_bgroup(axStyleBase, axStyles, /NONEXCLUSIVE, LABEL_TOP='X Style', $
                            SET_VALUE=xVals, BUTTON_UVALUE=axValues, $
                            UNAME='xstyle', EVENT_FUNC='plotstyles_gui_axstyles')
    yStyleBGroup = cw_bgroup(axStyleBase, axStyles, /NONEXCLUSIVE, LABEL_TOP='Y Style', $
                            SET_VALUE=yVals, BUTTON_UVALUE=axValues, $
                            UNAME='ystyle', EVENT_FUNC='plotstyles_gui_axstyles')
    zStyleBGroup = cw_bgroup(axStyleBase, axStyles, /NONEXCLUSIVE, LABEL_TOP='Z Style', $
                            SET_VALUE=zVals, BUTTON_UVALUE=axValues, $
                            UNAME='zstyle', EVENT_FUNC='plotstyles_gui_axstyles')
                            
    ;Make a field for the line style
    lgStyleBase = widget_base(tlb, ROW=4)
    linStyleLabel = widget_label(lgStyleBase, VALUE='Line Style')
    linStyleDList = widget_droplist(lgStyleBase, VALUE=lineStyles, UNAME='linestyle', $
                                    UVALUE=lgvalues, EVENT_PRO='plotstyles_gui_lgstyles')
    
    xGridLabel = widget_label(lgStyleBase, VALUE='X Grid Style')
    xGridDList = widget_droplist(lgStyleBase, VALUE=lineStyles, UNAME='xgridstyle', $
                                 UVALUE=lgvalues, EVENT_PRO='plotstyles_gui_lgstyles')
    
    yGridLabel = widget_label(lgStyleBase, VALUE='Y Grid Style')
    yGridDList = widget_droplist(lgStyleBase, VALUE=lineStyles, UNAME='ygridstyle', $
                                 UVALUE=lgvalues, EVENT_PRO='plotstyles_gui_lgstyles')
                                 
    zGridLabel = widget_label(lgStyleBase, VALUE='Z Grid Style')
    zGridDList = widget_droplist(lgStyleBase, VALUE=lineStyles, UNAME='zgridstyle', $
                                 UVALUE=lgvalues, EVENT_PRO='plotstyles_gui_lgstyles')
    
    ;Make a field for the grid styles
    
    ;Create a row for the OK and Cancel buttons
    okBase = widget_base(tlb, /ROW)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='plotStyles_gui_ok')
    cancelButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                                 EVENT_PRO='plotStyles_gui_cancel')
    

       
;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

	;Set a pointer to the state structure as the user value of the top-level base
	pstate = ptr_new(styles)
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plotStyles_gui', tlb, cleanup='plotStyles_gui_cleanup', $
	          event_handler='plotStyles_gui_event', NO_BLOCK=no_block
    
    ;return the selected analysis type
    return, pstate
end