;+
; NAME:
;       PLOTSYMBOLS_GUI
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
;       titles = plotSymbols_gui(symbols [, group_leader])
;
; INPUT POSITIONAL PARAMETERS:
;
;       SYMBOLS             -   In. Optional. Type=Struct
;                               A structure containing the current psym and symsize of a 
;                                   plot. E.g.,
;                                       SYMBOLS = {psym: 0, $
;                                                  symsize: 1.0}
;
;       GROUP_LEADER        -   In. Optional. Type=Int.
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly-created widget.
;
; RETURN VALUE:
;
;       PSTATE              -   A pointer to a copy of SYMBOLS, but with the fields changed.
;                               PSTATE is an invalid pointer if 'Cancel' is pressed.
;       
; MODIFICATION HISTORY:
;
;       Written by:     Matthew Argall 27 November 2012
;       12/08/2012  -   SYMBOLS is now optional
;-
;*****************************************************************************************


;
; NAME:
;       PLOTSYMBOLS_GUI_SYMSIZE
;
; PURPOSE:
;
;       Event handler for the SYM SIZE field.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
function plotSymbols_gui_symsize, event
    compile_opt idl2

    ;get the state variable.
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Record which symbol was selected
    (*pstate).symsize = event.value

    ;Put the state variable back
    widget_control, event.top, SET_UVALUE=pstate
    
    return, 1
end


;
; NAME:
;       PLOTSYMBOLS_GUI_CONNECT
;
; PURPOSE:
;
;       Event handler for the "Connect-the-Dots" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
pro plotSymbols_gui_connect, event
    compile_opt idl2

    ;get the state variable.
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Retrieve the user-name and user-value of the widget that caused the event
    widget_control, event.id, GET_UVALUE=uvalue
    
    ;if the button is released (pressed), make psym positive (negative).
    if event.select eq 0 then (*pstate).psym = abs((*pstate).psym) $
                         else (*pstate).psym = -abs((*pstate).psym)

    ;Put the state variable back
    widget_control, event.top, SET_UVALUE=pstate
end


;
; NAME:
;       PLOTSYMBOLS_GUI_PSYMS
;
; PURPOSE:
;
;       Event handler for the PSYM drop-lists.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
pro plotSymbols_gui_psym, event
    compile_opt idl2

    ;get the state variable.
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Retrieve the user-name and user-value of the widget that caused the event
    widget_control, event.id, GET_UVALUE=psymValues
    
    ;Record which symbol was selected
    (*pstate).psym = psymValues[event.index]

    ;Put the state variable back
    widget_control, event.top, SET_UVALUE=pstate
end


;
; NAME:
;       PLOTSYMBOLS_GUI_OK
;
; PURPOSE:
;
;       Event handler for the "OK" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
pro plotSymbols_gui_ok, event
    compile_opt idl2
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;
; NAME:
;       PLOTSYMBOLS_GUI_CANCEL
;
; PURPOSE:
;
;       Event handler for the "Cancel" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 2 December 2012
;
pro plotSymbols_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable pointer
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /destroy
    
end


pro plotSymbols_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


function plotSymbols_gui, symbols, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;1 Parameter, not structure => group leader.
    if n_params() eq 1 and size(symbols, /TYPE) ne 8 then group_leader = temporary(symbols)
    
    ;Create default SYMBOLS structure.
    if n_elements(ranges) eq 0 $
        then symbols = {psym: 0, $
                        symsize: 1.0}

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
    plotSymbols = ['None', $
                   'Plus Sign (+)', $
                   'Asterisk (*)', $
                   'Diamond', $
                   'Triangle', $
                   'Square', $
                   'X', $
                   'User-Defined', $
                   'Undefined', $
                   'Histogram Mode']
    psymValues = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    
;---------------------------------------------------------------------
;Create an *Ranges fields ////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Create base to hold each of the buttons. Make each button individually. CW_BGROUP() is
    ;not used because I want an event handler for each button.
    columnBase = widget_base(tlb, /COLUMN)
    
    ;Put a label across the top of the widget.
    stylesLabel = widget_label(tlb, VALUE='Symbols', XSIZE=120, FRAME=4, /ALIGN_CENTER)
    
    ;Create a droplist with the symbol options in it.
    symSizeField = cw_field(tlb, /FLOATING, /ROW, TITLE='Sym Size', VALUE=symbols.symsize, $
                            EVENT_FUNC='plotsymbols_gui_symsize', UNAME='symsize', $
                            /ALL_EVENTS, XSIZE=5)
    psymDList = widget_droplist(tlb, VALUE=plotSymbols, UNAME='plotsymbol', UVALUE=psymValues, $
                                EVENT_PRO='plotsymbols_gui_psym')
    widget_control, psymDList, SET_DROPLIST_SELECT=symbols.psym
    
    ;A button for connecting the symbols with a line.
    connectBase = widget_base(tlb, /COLUMN, /NONEXCLUSIVE)
    psymButton = widget_button(connectBase, VALUE='Connect-the-Dots', $
                               EVENT_PRO='plotsymbols_gui_connect')
    
    ;Make a field for the grid styles
    
    ;Create a row for the OK and Cancel buttons
    okBase = widget_base(tlb, /ROW)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='plotSymbols_gui_ok')
    cancelButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                                 EVENT_PRO='plotSymbols_gui_cancel')

;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

	;Set a pointer to the state structure as the user value of the top-level base
	pstate = ptr_new(symbols)
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plotSymbols_gui', tlb, cleanup='plotSymbols_gui_cleanup', $
	          event_handler='plotSymbols_gui_event', NO_BLOCK=no_block
    
    ;return the selected analysis type
    return, pstate
end