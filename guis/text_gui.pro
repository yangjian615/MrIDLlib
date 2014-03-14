; docformat = 'rst'
;
; NAME:
;       text_gui
;
; PURPOSE:
;+
;       The purpose of this program is to create a GUI interface to collect inputs to
;       cgText and weText__define.
;
; :Categories:
;       GUI
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
;       05/16/2013  -   Written by Matthew Argall
;       08/26/2013  -   Now uses cgPickColorName to choose colors. - MRA
;-
;*****************************************************************************************
;+
;   General event handler. 
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Widget event structure returned by the XManager.
;-
pro text_gui_events, event
    ;do nothing
end


;+
;   Event handler for the "Color" dropbox.
;
; :Private:
;
; :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro text_gui_colorSelect, event
    compile_opt idl2
    
    ;Get the state variable
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Generate another GUI to show which colors are available
    theColor = cgPickColorName(GROUP_LEADER=event.top)
   
    ;Update the text box to show which color was selected.
    cID = widget_info(event.top, FIND_BY_UNAME='COLOR')
    widget_control, cID, SET_VALUE=tHeColor
   
    ;Save the chosen color
    (*pstate).color = theColor
end


;+
;   Event handler for the "OK" button. Read the "From:" and "To:" text fields.
;
; :Private:
;
; :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro text_gui_ok, event
    compile_opt idl2
    
    ;Get the state variable
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Read the text from the text field.
    textID = widget_info(event.top, FIND_BY_UNAME='TEXT')
    widget_control, textID, GET_VALUE=text

    ;Set the state variable
    (*pstate).text = text
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;+
;   The purpose of this program is to serve as the event handler for the 
;   "Cancel" button.
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro text_gui_cancel, event
    compile_opt idl2
    
    ;Set "Cancel" to 1
    widget_control, event.top, GET_UVALUE=pstate
    (*pstate).cancel = 1
    
    ;destroy the widget.
    widget_control, event.top, /DESTROY
    
end


;+
;   The purpose of this program is to cleanup once the widget is destroyed.
;
; :Private:
;-
pro text_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


;+
;   The purpose of this program is to create a GUI for enerting the text and color
;   to be given to the cgText or weText routines.
;
; :Params:
;
;       GROUP_LEADER:           in, optional, type=Int
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly created widget.
;
; :Returns:
;
;       PSTATE:                 A pointer to a structure containing the desired text
;                                   and color, as well as the cancel status.
;-
function text_gui, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. text_gui is called by other
	;blocking widgets, so if a group_leader is given, then make text_gui modal.
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
	textBase = widget_base(tlb, COLUMN=1, FRAME=1, /ALIGN_RIGHT)
	field = cw_field(tlb, /STRING, /ROW, TITLE='Annotation Text', UNAME='TEXT', VALUE='')

    colorBase = widget_base(tlb, ROW=1, /ALIGN_RIGHT)
    button = widget_button(colorBase, VALUE='Color', UNAME='PICK_COLOR', EVENT_PRO='text_gui_colorSelect')
    text   = widget_text(colorBase, VALUE='Black', UNAME='COLOR')

    ;create a seperate base for the "ok" button
    okBase = widget_base(tlb, /ROW)
    button = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                           EVENT_PRO='text_gui_ok')
    button = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                           EVENT_PRO='text_gui_cancel')

;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

    ;Create the state variable
    pstate = ptr_new({text: '', $
                      color: '', $
                      cancel: 0})

	;Realize the top-level base
	widget_control, tlb, /REALIZE
	
	;set the MrPlot object reference as the tob level base's user value for easy access
	widget_control, tlb, SET_UVALUE=pstate
	
	;Call XMANAGER
	xmanager, 'text_gui', tlb, cleanup='text_gui_cleanup', $
	          event_handler='text_gui_events', NO_BLOCK=no_block
	
	return, pstate
end