; docformat = 'rst'
;
; NAME:
;       MrTraceback
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
;   Parse the Help, /LAST_MESSAGE report. The traceback will begin at the
;   line at which the last error occurred, and propagate back through the
;   callstack.
;
;   Output format is:
;       "In ROUTINE at (line ###)"
;   or
;       "In ROUTINE at (line ###) --> [filename]"
;
; :Params:
;       CALLER:      out, optional, type=string
;                    Name of the calling program in which the error occurred.
;       LINE:        out, optional, type=string
;                    Line at which the error occurred in `CALLER`.
;
; :Keywords:
;       ADD_FILES:   in, optional, type=boolean, default=0
;                    If set, the names of the files in which the routines are saved
;                        will be included in the traceback report.
;
; :See Also:
;       MrCallstack
;
; :History:
;   Modification History:
;       2015/12/05  -   Written by Matthew Argall
;-
function MrTraceback, caller, line, $
ADD_FILES=add_files
	compile_opt idl2
	on_error, 2
	
	;
	;  Example (parse routine name and line number):
	;     % MMS_EDI_RMT: Only MAX_ADDR = {30 | 31 | 32} beams allowed.
	;     % Execution halted at:  MMS_EDI_RMT       304 /home/argall/IDL/MMS/costfn/mms_edi_rmt.pro
	;     %                       MMS_EDI_TEST_COSTFN 1194 /home/argall/IDL/MMS/diagnostics/mms_edi_test_costfn.pro
	;     %                       $MAIN$
	;
	
	;Get the traceback report from the last message
	;   - First element is the error message
	;   - Last element is $MAIN$
	Help, /LAST_MESSAGE, OUTPUT=traceback
	ntrace = n_elements(traceback)
	
	;Find the line that says, "Execution halted ... "
	;   - The error message segment can be multiple lines (elements) long
	;   - This will be the place at which to start parsing
	istart = where(strpos(traceback, 'Execution halted') ne -1, n)
	if n ne 1 then message, 'Unexpected traceback format.'
	istart = istart[0]
	iend   = ntrace-2

	;Error occurred from $MAIN$
	if istart gt iend then begin
		routine = '$MAIN$'
		lines   = 0

	;Error occurred in procedure or function
	endif else begin
		;Allocate memory
		routine = strarr(iend-istart+1)
		lines   = lonarr(iend-istart+1)

		;Parse each line of the traceback report
		for i = istart, iend do begin
			info              = stregex(traceback[i], ':?[ ]+([A-Z_0-9:]+)[ ]+([0-9]+)', /SUBEXP, /EXTRACT)
			routine[i-istart] = info[1]
			lines[i-istart]   = info[2]
		endfor
	endelse

	;Caller and line number
	caller = routine[0]
	line   = lines[0]
	
	;Traceback report
	traceback = 'In ' + routine + ' at (line ' + strtrim(lines, 2) + ')'

	return, traceback
END
