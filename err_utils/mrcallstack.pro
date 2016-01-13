; docformat = 'rst'
;
; NAME:
;       MrCallstack
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may  be used to endorse or promote products derived from this     ;
;         software without specific prior written permission.                            ;
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
;   Parse the Scope_Traceback() report. The callstack is reported from the line
;   at which MrCallstack is called.
;
;   Output format is:
;       "In ROUTINE at (line ###)"
;   or
;       "In ROUTINE at (line ###) --> [filename]"
;
; :Params:
;       LEVEL:       in, optional, type=integer, default=1
;                    Level in the call stack at which to begin reporting traceback
;                        information. If LEVEL=1 (the default), the traceback will
;                        begin here, with MrCallstack.
;
; :Keywords:
;       CALLER:      out, optional, type=string
;                    Name of the calling program, identified by `LEVEL`.
;       LINE:        out, optional, type=string
;                    Line of the calling program, identified by `LEVEL`.
;       ADD_FILES:   in, optional, type=boolean, default=0
;                    If set, the names of the files in which the errors occurred
;                        will be included in the traceback report.
;
; :See Also:
;   MrTraceback
;
; :History:
;   Modification History:
;       2015/12/05  -   Written by Matthew Argall
;-
function MrCallstack, level, $
CALLER=caller, $
LINE=line, $
ADD_FILES=add_files
	compile_opt idl2
	on_error, 2
	
	;Defaults
	lvl = n_elements(level) eq 0 ? 1 : level
	
	; Get the call stack and the calling routine's name.
	stack  = Scope_Traceback(/STRUCTURE)
	nstack = n_elements(stack)
	caller = stack[nstack-2].routine

	;If there is only one element in the stack, then include MAIN
	if nstack eq 1 then begin
		lvl = 1
		
	;Otherwise, exclude it
	endif else if lvl gt 0 && lvl lt nstack then begin
		;Get the calling program
		caller = stack[nstack-lvl].routine
		
		;Ignore sister program MrPrintF
		if caller eq 'MRPRINTF' then lvl = lvl + 1
		
	;Level not allowed
	endif else begin
		message, 'LEVEL > stack depth.'
	endelse
	
	;Extract the stack elements
	;   - Eliminate $MAIN$ if possible
	;   - Reverse elements so caller is first.
	if lvl eq nstack $
		then stack = stack[0] $
		else stack = stack[nstack-lvl:1:-1]
	nstack = n_elements(stack)

	;Calling program
	caller = stack[0].routine
	line   = stack[0].line
	
	;Traceback report
	traceback = 'In ' + stack.routine + ' at (line ' + strtrim(stack.line, 2) + ')'
	if keyword_set(traceback) then traceback += ' --> ' + stack.filename

	return, traceback
END
