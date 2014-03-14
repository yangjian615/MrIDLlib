;+
; NAME:
;       PLOTTITLES_GUI
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
;       titles = plotTitles_gui(titles [, group_leader])
;
; INPUT POSITIONAL PARAMETERS:
;
;       TITLES              -   In. Optional. Type=Struct
;                               A structure containing the current title, xtitle, ytitle,
;                                   and ztitle of a plot. E.g.,
;                                       TITLES = {title: '', $
;                                                 subtitle: '', $
;                                                 xtitle: '', $
;                                                 ytitle: '', $
;                                                 ztitle: ''}
;
;       GROUP_LEADER        -   In. Optional. Type=Int.
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly-created widget.
;
; RETURN VALUE:
;
;       PSTATE              -   A similar to TITLES, containing the updated title, xtitle,
;                                   ytitle, and ztitle fields.
;                               PSTATE is an invalid pointer if 'Cancel' is pressed.
;       
; MODIFICATION HISTORY:
;
;       Written by:     Matthew Argall 27 November 2012
;       12/08/2012  -   TITES is now optional
;-
;*****************************************************************************************

;
; NAME:
;       PLOTTITLES_GUI_EVENTS
;
; PURPOSE:
;
;       Event handler for all of the titles fields.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotTitles_gui_event, event
    compile_opt idl2

    ;get the state variable.
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Retrieve the user-name of the widget that caused the event
    uname = widget_info(event.id, /UNAME)
    
    ;Update the title in the state variable
    case uname of
        'title': (*pstate).title = event.value
        'subtitle': (*pstate).subtitle = event.value
        'xtitle': (*pstate).xtitle = event.value
        'ytitle': (*pstate).ytitle = event.value
        'ztitle': (*pstate).ztitle = event.value
    endcase
 
    ;Put the state variable back
    widget_control, event.top, SET_UVALUE=pstate
end


;
; NAME:
;       PLOTTITLES_GUI_OK
;
; PURPOSE:
;
;       Event handler for the "OK" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotTitles_gui_ok, event
    compile_opt idl2
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;
; NAME:
;       PLOTTITLES_GUI_CANCEL
;
; PURPOSE:
;
;       Event handler for the "Cancel" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotTitles_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable pointer
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /destroy
    
end


pro plotTitles_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


function plotTitles_gui, titles, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;1 Parameter, not structure => group leader. Create default TITLES structure.
    if n_params() eq 1 and size(titles, /TYPE) ne 8 then group_leader = temporary(titles)

        
    if n_elements(titles) eq 0 $
        then titles = {title: '', $
                       subtitle: '', $
                       xtitle: '', $
                       ytitle: '', $
                       ztitle: ''}
;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. PLOTTITLE_GUI is called by other
	;blocking widgets, so if a group_leader is given, then make PLOTTITLE_GUI modal.
	if n_params() eq 2 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Plot Titles', /COLUMN, $
	                      XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
	                      /MODAL)
	endif else begin
	    no_block = 0
	    tlb = widget_base(TITLE='Plot Titles', /COLUMN, XOFFSET=200, YOFFSET=100, $
	                      UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse
       
;---------------------------------------------------------------------
;Create an title fields //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Create base to hold each of the buttons. Make each button individually. CW_BGROUP() is
    ;not used because I want an event handler for each button.
    titlesBase = widget_base(tlb, COL=1)
    titleLabel = widget_label(titlesBase, VALUE='Titles', FRAME=4, XSIZE=175, /ALIGN_CENTER)
    titleField = cw_field(titlesBase, /STRING, TITLE='   Title', UNAME='title', $
                          VALUE=titles.title, XSIZE=20, /ALL_EVENTS)
    subtitleField = cw_field(titlesBase, /STRING, TITLE='SubTitle', UNAME='subtitle', $
                             VALUE=titles.subtitle, XSIZE=20, /ALL_EVENTS)
    xtitleField = cw_field(titlesBase, /STRING, TITLE=' X Title', UNAME='xtitle', $
                           VALUE=titles.xtitle, XSIZE=20, /ALL_EVENTS)
    ytitleField = cw_field(titlesBase, /STRING, TITLE=' Y Title', UNAME='ytitle', $
                           VALUE=titles.ytitle, XSIZE=20, /ALL_EVENTS)
    ztitleField = cw_field(titlesBase, /STRING, TITLE=' Z Title', UNAME='ztitle', $
                           VALUE=titles.ztitle, XSIZE=20, /ALL_EVENTS)
                           
    okBase = widget_base(tlb, /ROW)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='plotTitles_gui_ok')
    cancelButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                                 EVENT_PRO='plotTitles_gui_cancel')
    

       
;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

	;Set a pointer to the state structure as the user value of the top-level base
	pstate = ptr_new(titles)
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plotTitles_gui', tlb, cleanup='plotTitles_gui_cleanup', $
	          event_handler='plotTitles_gui_event', NO_BLOCK=no_block
    
    ;return the selected analysis type
    return, pstate
end