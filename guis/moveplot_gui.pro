; docformat = 'rst'
;
; NAME:
;       MOVEPLOT_GUI
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
; PURPOSE:
;+
;       The purpose of this program is to create a GUI for choosing the source and
;       destination locations on an NxM plotting grid. The idea is that a plot found
;       at "source" could be moved to "destination".
;
; :Categories:
;
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
;       Written by:     Matthew Argall 10 December 2012
;-
;*****************************************************************************************
;+
;       Event handler for the "OK" button. Read the "From:" and "To:" text fields.
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro movePlot_gui_ok, event
    compile_opt idl2
    
    ;Get the state variable
    widget_control, event.top, GET_UVALUE=pstate
    
    ;Get the IDs of the text widgets
    fromColID = widget_info(event.top, FIND_BY_UNAME='fromCol')
    fromRowID = widget_info(event.top, FIND_BY_UNAME='fromRow')
    toColID = widget_info(event.top, FIND_BY_UNAME='toCol')
    toRowID = widget_info(event.top, FIND_BY_UNAME='toRow')
    
    ;Get the values of the text fields
    widget_control, fromColID, GET_VALUE=fromCol
    widget_control, fromRowID, GET_VALUE=fromRow
    widget_control, toColID, GET_VALUE=toCol
    widget_control, toRowID, GET_VALUE=toRow
    
    ;Set the state variable
    (*pstate).from = [fromCol, fromRow]
    (*pstate).to = [toCol, toRow]
    
    ;destroy the widget.
    widget_control, event.top, /destroy
end


;+
;       The purpose of this program is to serve as the event handler for the 
;       "Cancel" button.
;
;   :Private:
;
;   :Params:
;       EVENT:              in, required, type=structure
;                           Button event structure.
;-
pro movePlot_gui_cancel, event
    compile_opt idl2
    
    ;free the state variable
    widget_control, event.top, GET_UVALUE=pstate
    ptr_free, pstate
    
    ;destroy the widget.
    widget_control, event.top, /DESTROY
    
end


;+
;   The purpose of this program is to cleanup once the widget is destroyed.
;
; :Private:
;-
pro movePlot_gui_cleanup, event
    compile_opt idl2
    ;do nothing    
end


;+
;       The purpose of this program is to create a GUI for choosing the source and
;       destination locations on an NxM plotting grid. The idea is that a plot found
;       at "source" will be moved to "destination".
;
; :Params:
;
;       GROUP_LEADER:           in, optional, type=Int
;                               The widget ID of an existing widget that serves as “group 
;                                   leader” for the newly created widget.
;
; :Returns:
;
;       PSTATE:                 A pointer to a structure containing the source ("From")
;                                   and destination ("To") locations (1 Based).
;                               PSTATE is an invalid pointer if 'Cancel' is pressed.
;-
function movePlot_gui, group_leader
    compile_opt idl2

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
    
	;Make a top-level base with or without a groupleader. MOVEPLOT_GUI is called by other
	;blocking widgets, so if a group_leader is given, then make MOVEPLOT_GUI modal.
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
	plotSelectBase = widget_base(tlb, ROW=3, UNAME='plotSelect', FRAME=1, /ALIGN_RIGHT)
	wBlankLabel = widget_label(plotSelectBase, VALUE='', XSIZE=32)
	wColLabel = widget_label(plotSelectBase, /ALIGN_CENTER, VALUE='Col', XSIZE=26)
	wRowLabel = widget_label(plotSelectBase, /ALIGN_CENTER, VALUE='Row', XSIZE=26)
	
	wRowLabel = widget_label(plotSelectBase, /ALIGN_CENTER, VALUE='From:')
	wColSelect = widget_text(plotSelectBase, value='1', XSIZE=2, /EDITABLE, uname='fromCol')
	wRowSelect = widget_text(plotSelectBase, value='1', XSIZE=2, /EDITABLE, uname='fromRow')
	
	wRowLabel = widget_label(plotSelectBase, /ALIGN_CENTER, VALUE='To:', XSIZE=30)
	wColSelect = widget_text(plotSelectBase, value='1', XSIZE=2, /EDITABLE, uname='toCol')
	wRowSelect = widget_text(plotSelectBase, value='2', XSIZE=2, /EDITABLE, uname='toRow')

    ;create a seperate base for the "ok" button
    okBase = widget_base(tlb, /COLUMN)
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                             EVENT_PRO='movePlot_gui_ok')
    okButton = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                             EVENT_PRO='movePlot_gui_cancel')

;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

    ;Create the state variable
    pstate = ptr_new({from: [1,1], $
                      to: [1,2]})

	;Realize the top-level base
	widget_control, tlb, /REALIZE
	
	;set the MrPlot object reference as the tob level base's user value for easy access
	widget_control, tlb, SET_UVALUE=pstate
	
	;Call XMANAGER
	xmanager, 'movePlot_gui', tlb, cleanup='movePlot_gui_cleanup', $
	          event_handler='movePlot_gui_type', NO_BLOCK=no_block
	
	return, pstate
end