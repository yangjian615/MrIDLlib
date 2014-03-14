;+
; NAME:
;       zoom_gui
;
; PURPOSE:
;
;       A GUI for choosing the source and destination locations on an NxM plotting grid.
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
;       fromTo = zoom_gui([group_leader])
;
; INPUT POSITIONAL PARAMETERS:
;
;       GROUP_LEADER        -   In. Optional. Type=Int.
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly-created widget.
;
; RETURN VALUE:
;
;       PSTATE              -   A pointer to a structure containing the source ("From")
;                                   and destination ("To") locations (1-Based).
;                               PSTATE is an invalid pointer if 'Cancel' is pressed.
;       
; MODIFICATION HISTORY:
;
;       Written by:     Matthew Argall 10 December 2012
;-
;*****************************************************************************************


;
; NAME:
;       ZOOM_GUI_ALL
;
; PURPOSE:
;
;       Event handler for the "Apply to all" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 10 December 2012
;
pro zoom_gui_all, event
    compile_opt idl2
    
    ;Get the state variable
    widget_control, event.top, GET_UVALUE=pstate
    
    ;set the state variable.
    (*pstate).all = event.select
end


;
; NAME:
;       ZOOM_GUI_LOCATION
;
; PURPOSE:
;
;       Event handler for the "Col" and "Row" fields.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 10 December 2012
;
function zoom_gui_location, event
    compile_opt idl2
    
    ;Get the state variable
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Get the user name  of the widget that caused the event
    uname = widget_info(event.id, /UNAME)
    
    ;Set the state variable
    case uname of
        'col': (*pstate).location[0] = event.value
        'row': (*pstate).location[1] = event.value
    endcase
    
    return, 1
end


;
; NAME:
;       ZOOM_GUI_OK
;
; PURPOSE:
;
;       Event handler for the "OK" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 10 December 2012
;
pro zoom_gui_ok, event
    compile_opt idl2
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;
; NAME:
;       ZOOM_GUI_CANCEL
;
; PURPOSE:
;
;       Event handler for the "Cancel" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 10 December 2012
;
pro zoom_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /DESTROY
    
end


pro zoom_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


function zoom_gui, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. zoom_gui is called by other
	;blocking widgets, so if a group_leader is given, then make zoom_gui modal.
	if n_params() eq 2 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, TITLE='', /COLUMN, $
	                      XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
	                      /MODAL)
	endif else begin
	    no_block = 0
	    tlb = widget_base(TITLE='', /COLUMN, XOFFSET=200, YOFFSET=100, $
	                      UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse

;---------------------------------------------------------------------
;Create the Editable Properties Buttons //////////////////////////////
;---------------------------------------------------------------------
    
    ;Select which plot is to be edited
	plotSelectBase = widget_base(tlb, /ROW, UNAME='plotSelect', FRAME=1, /ALIGN_CENTER)
	wBlankLabel = widget_label(plotSelectBase, VALUE='Zoom:', XSIZE=32)
	wColField = cw_field(plotSelectBase, VALUE=1, TITLE='Col', /INTEGER, /COLUMN, UNAME='col', $
	                     XSIZE=3, /ALL_EVENTS, EVENT_FUNC='zoom_gui_location')
	wRowField = cw_field(plotSelectBase, VALUE=1, TITLE='Row', /INTEGER, /COLUMN, UNAME='row', $
	                     XSIZE=3, /ALL_EVENTS, EVENT_FUNC='zoom_gui_location')
	
	;Create an exclusive button base to determine if all plots are to be affected.
	allBase = widget_base(tlb, /ROW, /NONEXCLUSIVE, /ALIGN_CENTER)
	wAllButton = widget_button(allBase, VALUE='Apply to all', UNAME='allButton', $
	                           EVENT_PRO='zoom_gui_all')

    ;create a seperate base for the "ok" button
    okBase = widget_base(tlb, /ROW)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='zoom_gui_ok')
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                             EVENT_PRO='zoom_gui_cancel')

;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

    ;Create the state variable
    pstate = ptr_new({location: [1,1], $
                      all: 0})

	;Realize the top-level base
	widget_control, tlb, /REALIZE
	
	;set the MrPlot object reference as the tob level base's user value for easy access
	widget_control, tlb, SET_UVALUE=pstate
	
	;Call XMANAGER
	xmanager, 'zoom_gui', tlb, cleanup='zoom_gui_cleanup', $
	          event_handler='zoom_gui_type', NO_BLOCK=no_block
	
	return, pstate
end