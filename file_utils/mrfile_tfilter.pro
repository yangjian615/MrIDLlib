
; docformat = 'rst'
;
; NAME:
;       MrFile_VFilter
;
;*****************************************************************************************
;   Copyright (c) 2016, University of New Hampshire                                      ;
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
;         contributors may be used to endorse or promote products derived from this      ;
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
;   Filter files by their time stamps
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       File name(s) to be filterd.
;       TSTART:         in, optional, type=string, default=''
;                       An ISO-8601 string specifying the start of an interval
;                           of interest. Any file containing this start time will
;                           be included in the results. If this parameter is
;                           provided, `FILE_PATH` must contain time tokens. If
;                           `FILE_PATH` does not contain both a start and end time
;                           outlining the data interval contained in the file,
;                           the search results may include files that do not
;                           contain `TSTART`.
;       TEND:           in, optional, type=string, default=''
;                       An ISO-8601 string specifying the start of an interval
;                           of interest. Any file containing this end time will
;                           be included in the results. If this parameter is
;                           provided, `FILE_PATH` must contain time tokens. If
;                           `FILE_PATH` does not contain both a start and end time
;                           outlining the data interval contained in the file,
;                           the search results may include files that do not
;                           contain `TEND`.
;
; :Keywords:
;       CLOSEST:        in, optional, type=boolean, default=0
;                       Find the nearest file with a start time <= `TSTART`.
;                           This option is ignored unless `TSTART` is specified.
;                           If `TEND` is also given, the file that starts <= `TEND`
;                           will serve as the upper limit of files times. All
;                           files within the range are returned.
;       COUNT:          out, optional, type=integer
;                       Number of files found.
;       FPATTERN:       in, optional, type=string, default='%Y%M%d%H%m%S'
;                       A pattern recognized by MrTokens that represents how time
;                           is incorporated into the file names.
;                       Filtering files by time with `TSTART` and `TEND` is
;                           done by converting dates to integers and performing
;                           math operations. As such, the times in FILENAME and
;                           in `TSTART` and `TEND` must be converted into a
;                           format where the slowest (fastest) changing time
;                           element is to the left (right). If `FILE_PATH`, `TSTART`
;                           and `TEND` are not already in such a format, set this
;                           parameter to a token pattern recognized by
;                           MrTimeParser so that they can be rearranged.
;       TPATTERN:       in, optional, type=string, default='%Y-%M-%dT%H:%m:%S'
;                       If `TSTART` and `TEND` are not ISO-8601 format, then
;                           use this parameter to specify their token pattern.
;                           Note that this pattern must be able to be broken down
;                           into `TIMEORDER`.
; 
;
; :Returns:
;       FILES:          Fully qualified path to the files or directories that match
;                           `PATTERN`.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016-02-24  -   Written by Matthew Argall
;-
function MrFile_VFilter, files, tstart, tend, $
CLOSEST=closest, $
COUNT=count, $
FPATTERN=fpattern, $
TPATTERN=tpattern, $
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_closest = keyword_set(closest)
	if n_elements(tstart)    eq 0 then tstart    = ''
	if n_elements(tend)      eq 0 then tend      = ''
	if n_elements(timeOrder) eq 0 then timeOrder = '%Y%M%d%H%m%S'
	if n_elements(tpattern)  eq 0 then tpattern  = '%Y-%M-%dT%H:%m:%S'
	
	;Restrictions
	if tstart eq '' || tend eq '' then message, 'At least one of TSTART or TEND must be given.'
	
	;Results
	file_filt = file_basename(files)
	dir_filt  = file_dirname(files)
	count     = n_elements(files)
	
	;How to filter
	tf_tstart = tstart ne ''
	tf_tend   = tend   ne ''
	
;-------------------------------------------
; Find Start and End Times in File Names ///
;-------------------------------------------
	;
	; Does the file name include TStart and TEnd?
	;   - Assume TEnd takes the same form as TStart.
	;   - Assume TStart does not repeat tokens.
	;   - Filenames include TStart and TEnd if the first token of TStart is repeated.
	;
	; Times are put into TimeOrder and converted to integers. This allows
	; for easy comparison. 
	;

	;Extract tokens
	tokens = MrTokens_Extract(inFile, COUNT=nTokens, POSITIONS=token_pos)
	
	;Is there an end time in the file name?
	;   - Look for a repeated token
	;   - Repeated token will be found at position TOKEN_POS[0]+2+IREPEAT
	iRepeat = strpos( strmid(inFile, token_pos[0]+2), '%'+tokens[0] )
	tf_fend = iRepeat ne -1

	;Convert the start time of each file to an integer
	;  - MrTimeParser will take the last match (i.e. FEND)
	;  - If the file name has an end time, parse up to the first repeated token.
	if tf_fend $
		then MrTimeParser, filesFound, strmid(inFile, 0, iRepeat+2+token_pos[0]), timeOrder, fstart $
		else MrTimeParser, filesFound, inFile, timeOrder, fstart
	ifstart = long64(fstart)

	;Convert the end time to an integer
	if tf_fend then begin
		MrTimeParser, filesFound, strmid(inFile, iRepeat+token_pos[0]+2), timeOrder, fend
		ifend = long64(fend)
	endif
		
	;Convert input times to integers
	if tf_tstart then begin
		MrTimeParser, tstart, tpattern, timeOrder, temp_start
		itstart = long64(temporary(temp_start))
	endif
	if tf_tend then begin
		MrTimeParser, tend, tpattern, timeOrder, temp_tend
		itend = long64(temporary(temp_tend))
	endif

;-------------------------------------------
; Filename Includes End Time ///////////////
;-------------------------------------------
	;
	; We decide which files to keep by first considering what happens when
	; we have all information: tstart, tend, fStart, and fEnd. In this
	; case, we want to choose any files that contain any portion of the
	; time interval [tstart, tend]. 
	;
	;                    |----Time Interval----|
	;   [--File Interval--]        ....       [--File Interval--]
	;
	; This means any interval such that
	;   ( (tstart >= fStart) & (tstart <  fEnd) )
	; OR
	;   ( (tend   >  fStart) & (tend   <= fEnd) )
	;
	; If we have less information, we simply remove the clause containing
	; the missing information.
	;
	if tf_fend then begin
		case 1 of
			(tf_tstart && tf_tend): ikeep = where( ( (itstart ge ifstart) and (itstart le ifend) ) or $
			                                       ( (itend   ge ifstart) and (itend   le ifend) ), count )
			tf_tstart:              ikeep = where( (itstart ge ifstart) and (itstart le ifend), count )
			tf_tend:                ikeep = where( (itend   ge ifstart) and (itend   le ifend), count )
		end

;-------------------------------------------
; Filename Does Not Include End Time ///////
;-------------------------------------------
	endif else begin
		case 1 of
			(tf_tstart && tf_tend): ikeep = where( (itstart ge ifstart) or (itend ge ifstart), count )
			tf_tstart:              ikeep = where(itstart ge ifStart, count)
			tf_tend:                ikeep = where(itend   ge ifStart, count)
		end
	endelse
	
	;Select the subset of files
	if count gt 0 then begin
		dirsFound  = dirsFound[ikeep]
		filesFound = filesFound[ikeep]
		fstart     = fstart[ikeep]
		if tf_fend then fend = fend[ikeep]
	endif else begin
		dirsFound  = ''
		filesFound = ''
	endelse

;-------------------------------------------
; Closest Time /////////////////////////////
;-------------------------------------------
	;
	; We want to find the closes time to 'TStart'
	;   - If the file has both a start and end time, there is
	;     sufficient information to select the appropriate files.
	;     We do not need to check anything.
	;   - If only a start time exists in the file name, then the
	;     selection process above may be too liberal. Find the
	;     file that starts at or just before 'TStart'.
	;   - If 'TEnd' was also given, find the file that starts
	;     just before 'TEnd', and select all files between
	;     'TStart' and 'TEnd'. Otherwise, just pick the file
	;     associated with 'TStart'.
	;
	if closest && ~tf_fend && count gt 0 then begin
		;
		; Find the file that starts closest in time to TSTART
		;

		;Find the largest start time <= TSTART
		istart = where(fstart le itstart, nstart)
		if nstart eq 0 then begin
			istart = 0
			nstart = 1
		endif
		void   = max(fstart[istart], imax)
		istart = istart[imax]
		
		;Find the smallest end time >= TEND
		if tf_tend then begin
			iend = where(fstart le itend, nend)
			void = max(fstart[iend], imax)
			iend = iend[imax]
		endif else begin
			iend = istart
			nend = 0
		endelse

		;Select the found files
		if nstart + nend gt 0 then begin
			if istart gt iend then message, 'TSTART must be before TEND.'
			dirsFound  = dirsFound[istart:iend]
			filesFound = filesFound[istart:iend]
			count      = iend - istart + 1
		endif else begin
			dirsFound  = ''
			filesFound = ''
			count      = 0
		endelse
	endif
end