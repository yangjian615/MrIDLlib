; docformat = 'rst'
;
; NAME:
;       PlotPositions_GUI
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
;+
;   The purpose of this method is to provide an interactive means of editing the layout
;   of a graphics window.
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
;	Modification History::
;       09/24/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Provide event handling for the text boxes.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;+
function plotPositions_gui_position, event
    compile_opt idl2

    ;only do something when text is inserted or deleted
    if event.type eq 0 or event.type eq 2 then begin
        ;get the state variable, the widget user value, and the value of the text field
        widget_control, event.top, get_uvalue=pstate
        widget_control, event.id,  get_uvalue=uval
        widget_control, event.id,  get_value=value
        
        ;make the value an integer
        value = fix(value[0], type=3)
        
        ;store the value in the proper state location
        case uval.tag of
            'LAYOUTCOL':    (*pstate).layout[0]  = value
            'LAYOUTROW':    (*pstate).layout[1]  = value
            'LEFTMARGIN':   (*pstate).xmargin[0] = value
            'RIGHTMARGIN':  (*pstate).xmargin[1] = value
            'BOTTOMMARGIN': (*pstate).ymargin[0] = value
            'TOPMARGIN':    (*pstate).ymargin[1] = value
            'XGAP':         (*pstate).xgap       = value
            'YGAP':         (*pstate).ygap       = value
            else: ;do nothing
        endcase
    endif

    return, 1
end


;+
;   Event handler for the OK button.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;+
pro plotPositions_gui_ok, event
    compile_opt idl2

    widget_control, event.top, /destroy
end


;+
;   Event handler for the Cancel button.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;+
pro plotPositions_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable pointer
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    widget_control, event.top, /destroy    
end


;+
;   General event handler. Does anything the other event handlers do not.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;+
pro plotPositions_gui_event, event
    ;do nothing   
end


;+
;   Cleanup after the GUI is destroyed.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;+
pro plotPositions_gui_cleanup, event
    ;do nothing    
end


;+
;   The purpose of this program is to create a GUI that allows the user to edit layout
;   properties of a graphics window.
;
; :Params:
;       GROUP_LEADER:       in, optional, type=structure
;                           The widget ID of a group leader.
;
; :Keywords:
;       LAYOUT:             in, optional, type=intarr(2)
;                           Specifies the number of columns and rows [ncols,nrows] within
;                               the layout.
;       XGAP:               in, optional, type=integer
;                           The horizontal space between plots, in multiples of the
;                               character width.
;       XMARGIN:            in, optional, type=intarr(2)
;                           The width of the margins in the horizontal direction. Margins
;                               are specified in multiples of the character width.
;       YGAP:               in, optional, type=integer
;                           The vertical space between plots, in multiples of the
;                               character height.
;       YMARGIN:            in, optional, type=intarr(2)
;                           The width of the margins in the vertical direction. Margins
;                               are specified in multiples of the character height.
;+
function plotPositions_gui, group_leader, $
LAYOUT = layout, $
XMARGIN = xmargin, $
YMARGIN = ymargin, $
XGAP = xgap, $
YGAP = ygap
    compile_opt idl2

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if n_elements(layout) eq 0 then layout = ['1','1'] else layout = strmid(string(layout), 2)
    if n_elements(xmargin) eq 0 then xmargin = ['10', '3'] else xmargin = strmid(string(xmargin), 2)
    if n_elements(ymargin) eq 0 then ymargin = ['4', '2'] else ymargin = strmid(string(ymargin), 2)
    if n_elements(xgap) eq 0 then xgap = '10' else xgap = strmid(string(xgap), 2)
    if n_elements(ygap) eq 0 then ygap = '5' else ygap = strmid(string(ygap), 2)

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
       
	;Make a top-level base with or without a groupleader
	if n_params() eq 1 then begin
	    no_block = 0
	    tlb = widget_base(GROUP_LEADER=group_leader, title='Plot Keywords', /column, $
	                      xoffset=100, yoffset=100, uname='tlb')
	endif else begin
	    no_block = 1
	    tlb = widget_base(title='Plot Keywords', /column, xoffset=100, yoffset=100, uname='tlb')
	endelse

;---------------------------------------------------------------------
;Make a Plot Position Form ///////////////////////////////////////////
;---------------------------------------------------------------------
	
	unames = ['layoutCol', 'layoutRow', 'leftMargin', 'rightMargin', $
	          'bottomMargin', 'topMargin', 'xgap', 'ygap']
	
	desc = ['1, BASE, , COLUMN', $
	        
	        '0, LABEL, Layout', $
	        '1, BASE, , ROW, FRAME=3', $
	        '0, INTEGER, ' + layout[0] + ', LABEL_TOP=Cols, WIDTH=3, TAG=' + unames[0] + ', EVENT=plotPositions_gui_position', $
	        '2, INTEGER, ' + layout[1] + ', LABEL_TOP=Rows, WIDTH=3, TAG=' + unames[1] + ', EVENT=plotPositions_gui_position', $
	        
	        '0, LABEL, Margins', $
	        '1, BASE, , COLUMN, FRAME=3', $
	        '1, BASE, , ROW', $
	        '0, INTEGER, ' + xmargin[0] + ', LABEL_TOP=Left, WIDTH=3, TAG=' + unames[2] + ', EVENT=plotPositions_gui_position', $
	        '2, INTEGER, ' + xmargin[1] + ', LABEL_TOP=Right, WIDTH=3, TAG=' + unames[3] + ', EVENT=plotPositions_gui_position', $
	        '1, BASE, , ROW', $
	        '0, INTEGER, ' + ymargin[0] + ', LABEL_TOP=Bottom, WIDTH=3, TAG=' + unames[4] + ', EVENT=plotPositions_gui_position', $
	        '2, INTEGER, ' + ymargin[1] + ', LABEL_TOP=Top, WIDTH=3, TAG=' + unames[5] + ', EVENT=plotPositions_gui_position', $
	        '2, LABEL, ,', $
	        
	        '0, LABEL, Gaps', $
	        '1, BASE, , ROW, FRAME=3', $
	        '0, INTEGER, ' + xgap + ', LABEL_TOP=X, WIDTH=3, TAG=' + unames[6] + ', EVENT=plotPositions_gui_position', $
	        '2, INTEGER, ' + ygap + ', LABEL_TOP=Y, WIDTH=3, TAG=' + unames[7] + ', EVENT=plotPositions_gui_position']

	posForm = cw_form(tlb, desc, ids=posIDs, uname='posForm')
       
;---------------------------------------------------------------------
;Create an OK and CANCEL button //////////////////////////////////////
;---------------------------------------------------------------------
       
    okBase = widget_base(tlb, /row)
    okButton = widget_button(okBase, value='OK', event_pro='plotPositions_gui_ok')
    cancelButton = widget_button(okBase, value='Cancel', event_pro='plotPositions_gui_cancel')
       
;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /realize

	;Create a state structure
	state = {layout:  fix(layout, type=3), $
             xmargin: fix(xMargin, type=3), $
             ymargin: fix(yMargin, type=3), $
             xgap:    fix(xgap, type=3), $
             ygap:    fix(ygap, type=3), $
             posIDs:  posIDs}

	;Set a pointer to the state structure as the user value of the top-level base
	pstate = ptr_new(state, /no_copy)
	widget_control, tlb, set_uvalue=pstate
	
	;Call XMANAGER
	xmanager, 'plotPositions_gui', tlb, cleanup='plotPositions_gui_cleanup', NO_BLOCK=no_block

    return, pstate
end