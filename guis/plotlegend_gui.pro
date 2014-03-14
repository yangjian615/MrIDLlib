;+
; NAME:
;       PLOTLEGEND_GUI
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
;       colors = plotLegend_gui(colors [, group_leader])
;
; INPUT POSITIONAL PARAMETERS:
;
;       LEGEND              -   In. Optional. Type=Strarr. Default=['']
;                               A string array containing the legend elements
;
;       GROUP_LEADER        -   In. Optional. Type=Int.
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly-created widget.
;
; RETURN VALUE:
;
;       PSTATE              -   A similar to LEGEND, containing the updated legend. Note 
;                                   that the legend elements must be separated by a 
;                                   comma-space: ", " when typing them into the gui field.
;                               PSTATE is an invalid pointer if 'Cancel" is pressed'
;       
; MODIFICATION HISTORY:
;
;       Written by:     Matthew Argall 27 November 2012
;       12/08/2012  -   LEGEND is now optional
;-
;*****************************************************************************************


;
; NAME:
;       PLOTLEGEND_GUI_OK
;
; PURPOSE:
;
;       Event handler for the "OK" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotLegend_gui_ok, event
    compile_opt idl2
    
    ;get the state variable
    widget_control, event.top, GET_UVALUE=pstate
    
    ;get the final legend string
    legendID = widget_info(event.top, FIND_BY_UNAME='legend')
    widget_control, legendID, GET_VALUE=legend_string
    
    ;break the legend parts into an array
    (*pstate) = strsplit(legend_string, ', ', /REGEX, /EXTRACT)
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;
; NAME:
;       PLOTLEGEND_GUI_CANCEL
;
; PURPOSE:
;
;       Event handler for the "Cancel" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;       12/08/2012  -   Free the state variable pointer.
;
pro plotLegend_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable pointer
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /destroy
    
end


pro plotLegend_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


function plotLegend_gui, legend, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;1 Parameter, not string => group leader.
    if n_params() eq 1 and size(legend, /TYPE) ne 7 then group_leader = temporary(legend)
        
    ;Create default LEGEND.
    if n_elements(legend) eq 0 then legend = ''
    
    ;Convert the legend elements into a single string. It will be made an array once again
    ;when the 'OK' button is pressed.
    legend_string = strjoin(legend, ', ')

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. PLOTTITLE_GUI is called by other
	;blocking widgets, so if a group_leader is given, then make PLOTTITLE_GUI modal.
	if n_params() eq 2 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Plot Legend', /COLUMN, $
	                      XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
	                      /MODAL)
	endif else begin
	    no_block = 0
	    tlb = widget_base(TITLE='Plot Legend', /COLUMN, XOFFSET=200, YOFFSET=100, $
	                      UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse
    
;---------------------------------------------------------------------
;Create the Widget ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Create a base to hold the drop down lists
    colorsLabel = widget_label(tlb, VALUE='Legend', FRAME=4, /ALIGN_CENTER, XSIZE=100)
    
    ;A base for the fore- and background color lists. Put a label on top of the lists.
    legendBase = widget_base(tlb, COLUMN=1)
    legendText = widget_text(tlb, /EDITABLE, VALUE=legend_string, UNAME='legend')
                           
    okBase = widget_base(tlb, /ROW)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='plotlegend_gui_ok')
    cancelButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                                 EVENT_PRO='plotlegend_gui_cancel')
           
;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

	;Set a pointer to the state structure as the user value of the top-level base
	pstate = ptr_new(legend)
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plotLegend_gui', tlb, cleanup='plotLegend_gui_cleanup', $
	          event_handler='plotLegend_gui_event', NO_BLOCK=no_block
    
    ;return the selected analysis type
    return, pstate
end