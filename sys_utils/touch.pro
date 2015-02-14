; docformat = 'rst'
;
; NAME:
;       Touch
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
;   Serve as an IDL implementation of Unix's "touch" command.
;
; :Params:
;       PATH:           in, required, type=string
;                       Path of the file or directory for which the access, status change,
;                           and/or modified times are to be updated.
;
; :Keywords:
;       ACCESS:         in, optional, type=boolean, default=0
;                       If set, only the access time will be changed. The default is to
;                           change all three [ACM]TIME.
;
; :Categories:
;   MMS, EDI
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
;       2015/02/12  -   Written by Matthew Argall
;-
pro touch, path, $
ACCESS=access
	on_error, 2

	;Information about the path provided.
	info = file_info(path)
	if info.exists eq 0 then message, 'PATH does not exist: "' + path + '".'
	
	;Defaults
	access = keyword_set(access)
	modify = ~access

	;Directory
	if info.directory then begin
		;Modified Time (also updates Access and Status Change times)
		if modify then begin
			if info.write eq 0 then message, 'PATH must have write privileges.'
		
			;Create a temporary file then delete it.
			temp_file = filepath('touch_temp_file.txt', ROOT_DIR=path)
			openw, lun, temp_file, /GET_LUN
			free_lun, lun
			file_delete, temp_file
		endif else begin
			message, 'The ACCESS option is not available for directories.'
		endelse
	
	;File
	endif else begin
		;Access Time
		if access then begin
			if info.read eq 0 then message, 'PATH must have read privileges.'
		
			;Read one byte of data
			var = 0B
			openr, lun, path, /GET_LUN
			readu, lun, var
			free_lun, lun
	
		;Modified Time (also updates Access and Status Change times)
		endif else begin
			if info.write eq 0 then message, 'PATH must have write privileges.'
		
			;Open to the end of the file and get the file position
			openw, lun, path, /GET_LUN, /APPEND
			point_lun, -lun, end_of_file
		
			;Write one byte of data
			writeu, lun, 0B
		
			;Truncate the contents just written & close the file.
			point_lun, lun, end_of_file
			truncate_lun, lun
			free_lun, lun
		endelse
	endelse
end