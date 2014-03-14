;+
; NAME:
;       PLOTRANGES_GUI
;
; PURPOSE:
;
;       A GUI for editing plot tiltles.
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
;       titles = plotRanges_gui(titles [, group_leader])
;
; INPUT POSITIONAL PARAMETERS:
;
;       RANGES              -   In. Optional. Type=Struct
;                               A structure containing the current xrange, yrange, zrange,
;                                   min_value, and max_value a plot. E.g.,
;                                       RANGES = {xrange: fltarr(2), $
;                                                 yrange: fltarr(2), $
;                                                 zrange: fltarr(2), $
;                                                 min_value: 0.0, $
;                                                 max_value: 0.0}
;
;       GROUP_LEADER        -   In. Optional. Type=Int.
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly-created widget.
;
; RETURN VALUE:
;
;       PSTATE              -   A pointer to a copy of RANGES, but with the fields changed.
;                               PSTATE is an invalid pointer if 'Cancel' is pressed.
;       
; MODIFICATION HISTORY:
;
;       Written by:     Matthew Argall 27 November 2012
;       12/08/2012  -   RANGES is now optional
;-
;*****************************************************************************************

;
; NAME:
;       PLOTRANGES_GUI_EVENTS
;
; PURPOSE:
;
;       Event handler for all of the ranges fields.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotRanges_gui_event, event
    compile_opt idl2

    ;if a single character is inserted or if text is deleted
    if event.type eq 0 or event.type eq 2 then begin
        ;get the state variable.
        widget_control, event.top, GET_UVALUE=pstate
        
        ;Retrieve the user-name of the widget that caused the event
        widget_control, event.id, GET_VALUE=value
        uname = widget_info(event.id, /UNAME)
        
        ;Update the title in the state variable
        case uname of
            'xmin': (*pstate).xrange[0] = float(value)
            'xmax': (*pstate).xrange[1] = float(value)
            'ymin': (*pstate).yrange[0] = float(value)
            'ymax': (*pstate).yrange[1] = float(value)
            'zmin': (*pstate).zrange[0] = float(value)
            'zmax': (*pstate).zrange[1] = float(value)
            'min': (*pstate).min_value = float(value)
            'max': (*pstate).max_value = float(value)
        endcase

        ;Put the state variable back
        widget_control, event.top, SET_UVALUE=pstate
    endif
end


;
; NAME:
;       PLOTRANGES_GUI_OK
;
; PURPOSE:
;
;       Event handler for the "OK" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotRanges_gui_ok, event
    compile_opt idl2
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;
; NAME:
;       PLOTRANGES_GUI_CANCEL
;
; PURPOSE:
;
;       Event handler for the "Cancel" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotRanges_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable pointer
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /destroy
    
end


;
; NAME:
;       PLOTRANGES_GUI_FOMRAT_CODE
;
; PURPOSE:
;
;       Convert an IDL type code to an IDL format code.
;
; ARGUMENTS
;
;       TYPE_CODE           -   In. Required. Type=Int.
;                               An IDL type code (see IDL's documentation for SIZE()).
;
; RETURN VALUES
;
;       FORMAT_CODE         -   A 1-character string indicating the format code that
;                                   corresponds to TYPE_CODE.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
function plotRanges_gui_format_code, type_code
    compile_opt idl2
    
    ;return a format code corresponding to the given type code.
    case type_code of
        2: format_code = 'i'
        3: format_code = 'i'
        4: format_code = 'f'
        5: format_code = 'f'
        7: format_code = 'a'
        12: format_code = 'i'
        13: format_code = 'i'
        14: format_code = 'i'
        15: format_code = 'i'
        else: message, 'Input variable type not recognized.'
    endcase
    
    return, format_code
end


pro plotRanges_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


function plotRanges_gui, ranges, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;1 Parameter, not structure => group leader.
    if n_params() eq 1 and size(ranges, /TYPE) ne 8 then group_leader = temporary(ranges)
        
    ;Create default RANGES structure.
    if n_elements(ranges) eq 0 $
        then ranges = {xrange: [0.0, 0.0], $
                       yrange: [0.0, 0.0], $
                       zrange: [0.0, 0.0], $
                       min_value: -!values.f_infinity, $
                       max_value: !values.f_infinity}

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. PLOTTITLE_GUI is called by other
	;blocking widgets, so if a group_leader is given, then make PLOTTITLE_GUI modal.
	if n_params() eq 2 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Plot Ranges', /COLUMN, $
	                      XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
	                      /MODAL)
	endif else begin
	    no_block = 0
	    tlb = widget_base(TITLE='Plot Ranges', /COLUMN, XOFFSET=200, YOFFSET=100, $
	                      UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse
       
;---------------------------------------------------------------------
;Convert Inputs to Strings ///////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get the type codes for each range
    x_type = size(ranges.xrange, /TYPE)
    y_type = size(ranges.yrange, /TYPE)
    z_type = size(ranges.zrange, /TYPE)
    min_type = size(ranges.min_value, /TYPE)
    max_type = size(ranges.max_value, /TYPE)
    
    ;Convert the type code to a format code
    x_format = plotRanges_gui_format_code(x_type)
    y_format = plotRanges_gui_format_code(y_type)
    z_format = plotRanges_gui_format_code(z_type)
    min_format = plotRanges_gui_format_code(min_type)
    max_format = plotRanges_gui_format_code(max_type)
    
    ;Convert the range to a string
    xmin = string(ranges.xrange[0], format='(' + x_format + '0)')
    xmax = string(ranges.xrange[1], format='(' + x_format + '0)')
    ymin = string(ranges.yrange[0], format='(' + y_format + '0)')
    ymax = string(ranges.yrange[1], format='(' + y_format + '0)')
    zmin = string(ranges.zrange[0], format='(' + z_format + '0)')
    zmax = string(ranges.zrange[1], format='(' + z_format + '0)')
    min_val = string(ranges.min_value, format='(' + min_format + '0)')
    max_val = string(ranges.max_value, format='(' + max_format + '0)')
       
;---------------------------------------------------------------------
;Create an *Ranges fields ////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Create base to hold each of the buttons. Make each button individually. CW_BGROUP() is
    ;not used because I want an event handler for each button.
    columnBase = widget_base(tlb, /COLUMN)
    
    ;Put a label across the top of the widget.
    rangeLabel = widget_label(tlb, VALUE='Ranges', XSIZE=300, FRAME=4, /ALIGN_CENTER)
    
    ;Create rows for the axis ranges
    axBase = widget_base(tlb, ROW=4)
    blankLabel = widget_label(axBase, VALUE='', XSIZE=40)
    minLabel = widget_label(axBase, VALUE='Min', XSIZE=130, /ALIGN_CENTER)
    maxLabel = widget_label(axBase, VALUE='Max', XSIZE=130, /ALIGN_CENTER)
    
    xrangeLabel = widget_label(axBase, VALUE='X Range', /ALIGN_LEFT)
    xminText = widget_text(axBase, VALUE=xmin, /EDITABLE, /ALL_EVENTS, $
                           UNAME='xmin', UVALUE=x_type)
    xmaxText = widget_text(axBase, VALUE=xmax, /EDITABLE, /ALL_EVENTS, $
                           UNAME='xmax', UVALUE=x_type)
    
    yrangeLabel = widget_label(axBase, VALUE='Y Range', /ALIGN_LEFT)
    yminText = widget_text(axBase, VALUE=ymin, /EDITABLE, /ALL_EVENTS, $
                           UNAME='ymin', UVALUE=y_type)
    ymaxText = widget_text(axBase, VALUE=ymax, /EDITABLE, /ALL_EVENTS, $
                           UNAME='ymax', UVALUE=y_type)
    
    ;Create a row for the z-range
    ;zBase = widget_base(tlb, /ROW)
    zrangeLabel = widget_label(axBase, VALUE='Z Range', /ALIGN_LEFT)
    zminText = widget_text(axBase, VALUE=zmin, /EDITABLE, /ALL_EVENTS, $
                           UNAME='zmin', UVALUE=z_type)
    zmaxText = widget_text(axBase, VALUE=zmax, /EDITABLE, /ALL_EVENTS, $
                           UNAME='zmax', UVALUE=z_type)
    
    ;Create a row for the min_value
    minBase = widget_base(tlb, /ROW)
    minLabel = widget_label(minBase, VALUE='Min Value', /ALIGN_CENTER)
    minText = widget_text(minBase, VALUE=min_val, /EDITABLE, /ALL_EVENTS, $
                          UNAME='min', UVALUE=min_type)
    
    ;Create a row for the max_value
    maxBase = widget_base(tlb, /ROW)
    maxLabel = widget_label(maxBase, VALUE='Max Value', /ALIGN_CENTER)
    maxText = widget_text(maxBase, VALUE=max_val, /EDITABLE, /ALL_EVENTS, $
                          UNAME='max', UVALUE=max_type)
    
    ;Create a row for the OK and Cancel buttons
    okBase = widget_base(tlb, /ROW)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='plotRanges_gui_ok')
    cancelButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                                 EVENT_PRO='plotRanges_gui_cancel')
    

       
;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

	;Set a pointer to the state structure as the user value of the top-level base
	pstate = ptr_new(ranges)
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plotRanges_gui', tlb, cleanup='plotRanges_gui_cleanup', $
	          event_handler='plotRanges_gui_event', NO_BLOCK=no_block
    
    ;return the selected analysis type
    return, pstate
end