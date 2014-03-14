;+
; NAME:
;       PLOTCOLORS_GUI
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
;       colors = plotColors_gui(colors [, group_leader])
;
; INPUT POSITIONAL PARAMETERS:
;
;       COLORS              -   In. Optional. Type=Struct.
;                               A structure containing the current foreground color,
;                                   background color, and line colors. E.g.,
;                                       COLORS = {fgcolor: 'white', $
;                                                 bgcolor: 'black', $
;                                                 color: ['blue, green, red']}
;
;       GROUP_LEADER        -   In. Optional. Type=Int.
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly-created widget.
;
; RETURN VALUE:
;
;       PSTATE              -   A pointer to a structure similar to COLORS, containing the
;                                   updated foreground color, background color, and line
;                                   colors.
;                               PSTATE is an invalid pointer if 'Cancel" is pressed'
;       
; MODIFICATION HISTORY:
;
;       Written by:     Matthew Argall 27 November 2012
;       12/07/2012  -   COLORS is now optional. COLORS.COLOR is now type STRARR().
;                           However, when inputting colors into the Color text field,
;                           individual colors must still be separated by a comma-space: ", ".
;-
;*****************************************************************************************

;
; NAME:
;       PLOTCOLORS_GUI_COLOR
;
; PURPOSE:
;
;       Event handler for the color fields.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
function plotColors_gui_color, event
    compile_opt idl2

    ;get the state variable and the list of colors
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Put the color string into the state variable.
    (*pstate).color = event.value
 
    ;Put the state variable back
    widget_control, event.top, SET_UVALUE=pstate
    
    return, 1
end

;
; NAME:
;       PLOTCOLORS_GUI_EVENTS
;
; PURPOSE:
;
;       Event handler for the foreground and background color lists.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotColors_gui_event, event
    compile_opt idl2

    ;get the state variable and the list of colors
    widget_control, event.top, GET_UVALUE=pstate
    widget_control, event.id, GET_UVALUE=pcolors
    
    ;Retrieve the user-name of the widget that caused the event
    uname = widget_info(event.id, /UNAME)
    
    ;Update the title in the state variable
    case uname of
        'fgcolor': (*pstate).fgcolor = (*pcolors)[event.index]
        'bgcolor': (*pstate).bgcolor = (*pcolors)[event.index]
    endcase
 
    ;Put the state variable back
    widget_control, event.top, SET_UVALUE=pstate
end


;
; NAME:
;       PLOTCOLORS_GUI_OK
;
; PURPOSE:
;
;       Event handler for the "OK" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotColors_gui_ok, event
    compile_opt idl2

;---------------------------------------------------------------------
;Check to See if the Colors Exist ////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get the state variable
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Separate the colors into an array
    line_colors = strsplit((*pstate).color, ', ', /REGEX, /EXTRACT)
    
    ;LOAD_COLORS() will throw an error if a color is not found.
    theseColors = load_color(line_colors)
    
    ;print information if a color is not found.
    if theseColors eq !NULL then begin
        print, 'COLOR NOT FOUND.'
        print, 'Be sure to separate the colors with a comma-space: ", ".'
        print, 'See the FGColor and BGColor list for available colors.'
        return
    endif
    
    ;Store the line colors as an array instead of a single string.
    *pstate = {fgcolor: (*pstate).fgcolor, $
               bgcolor: (*pstate).bgcolor, $
               color: line_colors}
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;
; NAME:
;       PLOTCOLORS_GUI_CANCEL
;
; PURPOSE:
;
;       Event handler for the "Cancel" button.
;
; MODIFICATION HISTORY
;
;       Written by:     Matthew Argall 17 November 2012
;
pro plotColors_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable pointer
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /destroy
    
end


pro plotColors_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


function plotColors_gui, colors, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;1 Parameter, not structure => group leader.
    if n_params() eq 1 and size(colors, /TYPE) ne 8 then group_leader = temporary(colors)
        
    ;Create default COLORS structure.
    if n_elements(colors) eq 0 $
        then colors = {bgcolor: 'Black', $
                       fgcolor: 'White', $
                       color: ['White']}
    
    ;make a single string out of the line colors with each color separated by ', ' (comma-
    ;space)
    color_string = strjoin(colors.color, ', ')
    
    ;Create the state variable using the color string. When 'Ok' is pressed, the string
    ;will be converted into an array again.
    state = {bgcolor: colors.bgcolor, $
             fgcolor: colors.fgcolor, $
             color: color_string}

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. PLOTTITLE_GUI is called by other
	;blocking widgets, so if a group_leader is given, then make PLOTTITLE_GUI modal.
	if n_params() eq 2 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Plot Colors', /COLUMN, $
	                      XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
	                      /MODAL)
	endif else begin
	    no_block = 0
	    tlb = widget_base(TITLE='Plot Colors', /COLUMN, XOFFSET=200, YOFFSET=100, $
	                      UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse
       
;---------------------------------------------------------------------
;Check the Given Colors //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get a list of available colors.
    color_list = tag_names(!color)
    
    ;Look for the given foreground color in the color list. Default to 'BLACK'
    ifgcolor = value_locate(color_list, strupcase(colors.fgcolor))
    if ~strcmp(color_list[ifgcolor], strupcase(colors.fgcolor)) $
        then ifgcolor = value_locate(color_list, 'BLACK')
    
    ;Look for the given background color in the color list. Default to 'WHITE'
    ibgcolor = value_locate(color_list, strupcase(colors.bgcolor))
    if ~strcmp(color_list[ibgcolor], strupcase(colors.bgcolor)) $
        then ibgcolor = value_locate(color_list, 'WHITE')
        
    ;make a pointer to the color list
    pcolors = ptr_new(color_list)
    
;---------------------------------------------------------------------
;Create the Widget ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Create a base to hold the drop down lists
    colorsLabel = widget_label(tlb, VALUE='Colors', FRAME=4, /ALIGN_CENTER, XSIZE=200)
    
    ;A base for the fore- and background color lists. Put a label on top of the lists.
    fbgBase = widget_base(tlb, COLUMN=2)
    fgLabel = widget_label(fbgBase, VALUE='FG Color')
    fgDrop = widget_list(fbgBase, VALUE=color_list, UNAME='fgcolor', YSIZE=7, UVALUE=pcolors)
    widget_control, fgDrop, SET_LIST_SELECT=ifgcolor
    
    bgLabel = widget_label(fbgBase, VALUE='BG Color')
    bgDrop = widget_list(fbgBase, VALUE=color_list, UNAME='bgcolor', YSIZE=7, UVALUE=pcolors)
    widget_control, bgDrop, SET_LIST_SELECT=ibgcolor
    
    ;Add a field for the line colors
    colorField = cw_field(tlb, /ALL_EVENTS, /STRING, /ROW, TITLE='Color', $
                          VALUE=colors.color, EVENT_FUNC='plotColors_gui_color')
                           
    okBase = widget_base(tlb, /ROW)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='plotColors_gui_ok')
    cancelButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                                 EVENT_PRO='plotColors_gui_cancel')
           
;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

	;Set a pointer to the state structure as the user value of the top-level base
	pstate = ptr_new(state, /NO_COPY)
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plotColors_gui', tlb, cleanup='plotColors_gui_cleanup', $
	          event_handler='plotColors_gui_event', NO_BLOCK=no_block
    
    ;return the selected analysis type
    return, pstate
end