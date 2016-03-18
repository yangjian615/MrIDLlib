
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
;   Filter files by their version number
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       File name(s) to be filterd.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files that passed the version filter.
;       MIN_VERSION:    in, optional, type=string, default=''
;                       The minimum version to be kept. Cannot be used with `VERSION`.
;       NEWEST:         in, optional, type=boolean
;                       Return the newest version of each file. If none of `VERSION`,
;                           `MIN_VERSION`, or NEWEST are set, then NEWEST is the default.
;                           Cannot be used with `VERSION`.
;       VERSION:        in, optional, type=string, default=''
;                       Specific version of files to return. Must match `VREGEX`. Cannot
;                           be used with `NEWEST` or `MIN_VERSION`.
;       VREGEX:         in, optional, type=string, default='([0-9]+)\.([0-9]+)\.([0-9]+)'
;                       Specify how the version can be dissected. See
;                           MrFile_VersionCompare.m for more details.
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
function MrFile_VFilter, files, $
COUNT=count, $
MIN_VERSION=min_version, $
NEWEST=newest, $
VERSION=version, $
VREGEX=vRegex
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_newest = keyword_set(newest)
	if n_elements(version)     eq 0 then version     = ''
	if n_elements(min_version) eq 0 then min_version = ''
	if n_elements(vRegex)      eq 0 then vRegex      = '([0-9]+)\.([0-9]+)\.([0-9]+)'
	
	;Restrictions
	;   - Cannot pick a minimum version and a specific version
	;   - Cannot pick the newest version and a specific version
	;   - CAN choose MIN_VERSION and NEWEST. MIN_VERSION will filter out
	;         old files so that they are not selected by NEWEST, even
	;         if the old version is the only version available.
	if version ne '' && min_version ne '' then message, 'VERSION and MIN_VERSION are mutually exclusive.'
	if tf_newest && version ne '' then message, 'NEWEST and VERSION are mutually exclusive.'
	
	;Default to choosing newest version
	if ~tf_newest && version eq '' && min_version eq '' then tf_newest = 1B

	;Results
	filtered = file_basename(files)
	filtdirs = file_dirname(files)
	count    = n_elements(filtered)

;-------------------------------------------
; Minimum Version //////////////////////////
;-------------------------------------------
	if min_version ne '' then begin
		;Search for files with versions GE MIN_VERSION
		vge = MrFile_VersionCompare(filtered, min_version, vRegex)
		iFiles = where(vge ge 0, count)
		
		;Select matching files
		if count gt 0 then begin
			filtered = filtered[iFiles]
			filtdirs = filtdirs[iFiles]
		endif
	endif

;-------------------------------------------
; Newest or Specific ///////////////////////
;-------------------------------------------
	if count gt 0 && (tf_newest || version ne '') then begin
		;Position in the file names where the version number begins
		vpos = stregex(filtered, vRegex, LENGTH=vlen)
	
		;Replace version number with the empty string
		;   - Concatenate the parts before and after the version number
		files_noversion = strmid(filtered, 0, transpose(vpos)) + $
		                  strmid(filtered, transpose(vpos) + transpose(vlen))

		;Find unique values
		iUniq = MrUniq(files_noversion, IARRAY=iArray, NUNIQ=count, /SORT)
		
		;Select only unique files
		dirsFound  = strarr(count)
		filesFound = strarr(count)

	;-------------------------------------------
	; Specific Version /////////////////////////
	;-------------------------------------------
		if version ne '' then begin
	
			;Search through all unique files
			n = 0
			for i = 0, count - 1 do begin
				;Pick out the copies
				iCopies = where(iArray eq iUniq[i], nCopies)
			
				;Search for matches
				tf_match = MrFile_VersionCompare(filtered[iCopies], version, vRegex) eq 0
				iMatch   = where(tf_match, nMatch)
			
				;Keep the first match
				if nMatch gt 0 then begin
					filesFound[n] = filtered[iCopies[iMatch[0]]]
					dirsFound[n]  = filtdirs[iCopies[iMatch[0]]]
					n++
				endif
			endfor
			count = n

			;Trim results
			if count gt 0 then begin
				filtered = filesFound[0:count-1]
				filtdirs = dirsFound[0:count-1]
			endif

	;-------------------------------------------
	; Newest Version ///////////////////////////
	;-------------------------------------------
		endif else if tf_newest then begin
			;Search through all unique files
			for i = 0, count - 1 do begin
				;Pick out the copies
				iCopies = where(iArray eq iUniq[i], nCopies)
				
				;Pick the first file as the newest
				newestFile = filtered[iCopies[0]]
				newestDir  = filtdirs[iCopies[0]]
				
				;Step through all copies
				for j = 1, nCopies - 1 do begin
					;Compare against remaining files
					if MrFile_VersionCompare(filtered[iCopies[j]], newestFile, vRegex) eq 1 then begin
						newestFile = filtered[iCopies[j]]
						newestDir  = filtdirs[iCopies[j]]
					endif
				endfor
				
				;Replace unique files with newest file and directory
				dirsFound[i]  = newestDir
				filesFound[i] = newestFile
			endfor
		
			;Copy results
			filtered = temporary(filesFound)
			filtdirs = temporary(dirsFound)
		endif
	endif

;-------------------------------------------
; Finish Up ////////////////////////////////
;-------------------------------------------
	
	;Combine file name with dir name
	;   - return scalar if COUNT=1
	if count eq 0 then begin
		filtered = ''
		filtdirs = ''
	endif else if count eq 1 then begin
		filtered = filepath(filtered[0], ROOT_DIR=filtdirs[0])
	endif else begin
		for i = 0, count - 1 do filtered[i] = filepath(filtered[i], ROOT_DIR=filtdirs[i])
	endelse
	
	return, filtered
end