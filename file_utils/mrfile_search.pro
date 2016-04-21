; docformat = 'rst'
;
; NAME:
;       MrFile_Search
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
;   Find files on the file system. Filter by time interval and version. Any token
;   recognized by MrTokens_ToRegex can be incorporated into `PATH_STR` to generalize
;   the search.
;
; :Params:
;       FILE_PATH:      in, required, type=string
;                       File path leading to the file to be found. The path can include
;                           any token recognized by MrTokens_ToRegex.
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
;       NEWEST:         in, optional, type=boolean
;                       Return the newest version of each file. If neither `VERSION` nor
;                           NEWEST are set, then NEWEST is the default.
;       DIRECTORY:      in, optional, type=string, default=''
;                       ???
;       TIMEORDER:      in, optional, type=string, default='%Y%M%d%H%m%S'
;                       Filtering files by time with `TSTART` and `TEND` is
;                           done by converting dates to integers and performing
;                           math operations. As such, the times in FILENAME and
;                           in `TSTART` and `TEND` must be converted into a
;                           format where the slowest (fastest) changing time
;                           element is to the left (right). If `FILE_PATH`, `TSTART`
;                           and `TEND` are not already in such a format, set this
;                           parameter to a token pattern recognized by
;                           MrTimeParser so that they can be rearranged.
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
;       TPATTERN:       in, optional, type=string, default='%Y-%M-%dT%H:%m:%S'
;                       If `TSTART` and `TEND` are not ISO-8601 format, then
;                           use this parameter to specify their token pattern.
;                           Note that this pattern must be able to be broken down
;                           into `TIMEORDER`.
;       VERSION:        in, optional, type=string, default=''
;                       Specific version of files to return. Must match `VREGEX`.
;       VREGEX:         in, optional, type=string, default='([0-9]+)\.([0-9]+)\.([0-9]+)'
;                       Specify how the version can be dissected. See
;                           MrFile_VersionCompare.m for more details.
; 
;
; :Returns:
;       FILES:          Fully qualified path to the files or directories that match
;                           `PATTERN`.
;
; :Examples:
;
;-------------------------------;
;   MMS data example:           ;
;-------------------------------;
;
;   Given the directory and file names
;    IDL> directory = '/Users/argall/Documents/Work/Data/MMS/EDI/'
;    IDL> fname     = 'mms4_edi_slow_l1a_efield_20150422_v0.1.0.cdf'
;
;  Create the directory and file name token patterns. The regex "[14]" will select
;  spacecraft 1 and 4. Enclosing it in "%(" and "%)" protects the regex from being
;  interpreted literally as part of the file name. "%Y", "%M", and "%d" represent
;  year, month, and day, respectively.
;    IDL> dpattern = directory
;    IDL> fpattern = 'mms%([14]%)_edi_slow_l1a_efield_%Y%M%d_v*.cdf'
;    IDL> pattern  = filepath(fpattern, ROOT_DIR=dpattern)
;
;  1. Find all files. Note the two versions of mms4 on 2015-04-22.
;    IDL> files = MrFile_Search(pattern, NEWEST=0)
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150421_v0.0.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150425_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150427_v0.2.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms4_edi_slow_l1a_efield_20150422_v0.0.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms4_edi_slow_l1a_efield_20150422_v0.1.0.cdf
;
;  2. Select only the most recent versions of the files. The mms4 version v0.0.0 files was removed.
;    IDL> files = MrFile_Search(pattern)
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150421_v0.0.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150425_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150427_v0.2.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms4_edi_slow_l1a_efield_20150422_v0.1.0.cdf
;
;  3. Select version v0.0.0
;    IDL> files = MrFile_Search(pattern, VERSION='v0.0.0')
;    IDL> vertcat(files{:})
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150421_v0.0.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms4_edi_slow_l1a_efield_20150422_v0.0.0.cdf
;
;  4. Filter by time interval with TStart. Because there is no end-time in the
;     file name, this will return all files that start at or before TSTART.
;     Also, the file name contains only year, month, and day, so TIMEORDER
;     must be changed.
;    IDL> files = MrFile_Search(pattern, $
;                               TSTART    = '2015-04-24T10:00:00Z', $
;                               TIMEORDER = '%Y%M%d')
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150421_v0.0.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms4_edi_slow_l1a_efield_20150422_v0.1.0.cdf
;
;  5. Pick the file that starts closest to TStart. This will select one file that starts
;     closest to TSTART. If two or more files start at the same time, the first of those
;     files is chosen.
;    IDL> files = MrFile_Search(pattern, /CLOSEST, $
;                               TSTART    = '2015-04-24T10:00:00Z', $
;                               TIMEORDER = '%Y%M%d')
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf
;
;  6. Filter by TSTART and TEND. Again, because the file name did not
;     include an end time, the filter returns all files for which
;     there is a possibility of containing data, i.e. those for which
;     the start time occurs before both TSTART and TEND (see comments in code).
;    IDL> files = MrFile_Search(pattern, $
;                               TSTART    = '2015-03-24T04:00:00Z', $
;                               TEND      = '2015-03-25T11:00:00Z', $
;                               TIMEORDER = '%Y%M%d');
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150421_v0.0.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150425_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms4_edi_slow_l1a_efield_20150422_v0.1.0.cdf
;
;  7. Specify /CLOSEST with TSTART and TEND. This time, files are
;     filtered by the closest file start <= TSTART and the closest
;     file start <= TEND. All files between the two are returned.
;     Note that if TEND = '2015-03-19T00:00:00Z' and you wanted
;     data only until 11:59:59.999 on 2015-03-18, the file for
;     2015-03-19 would still be returned. Time intervals are
;     inclusive on both ends to account for date rounding in file.
;     names.
;    IDL> files = MrFile_Search(pattern, /CLOSEST, $
;                               TSTART    = '2015-03-24T04:00:00Z', $
;                               TEND      = '2015-03-25T11:00:00Z', $
;                               TIMEORDER = '%Y%M%d');
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150425_v0.1.0.cdf
;
;  8. Filter by TSTART and TEND. Specify a time pattern. Note how TPATTERN
;     has all components necessary to form TIMEORDER. If this were not
;     the case, an error would occur.
;    IDL> files = MrFile_Search(pattern, /CLOSEST, $
;                              TSTART    = '2015-114T04:00:00Z', $
;                              TEND      = '2015-115T11:00:00Z', $
;                              TPATTERN  = '%Y-%DT%H:%m:%S', $
;                              TIMEORDER = '%Y%M%d')
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf
;      /Users/argall/Documents/Work/Data/MMS/EDI/mms1_edi_slow_l1a_efield_20150425_v0.1.0.cdf
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
;       2015-04-30  -   Written by Matthew Argall
;       2015-05-13  -   Error with /CLOSEST when all files started after TSTART. Fixed. - MRA
;       2015-06-29  -   Catch cases when no files found within time interval. - MRA
;       2015-08-21  -   Fixed bug when extracting end time from file name. - MRA
;-
function MrFile_Search, file_path, $
CLOSEST=closest, $
COUNT=count, $
MIN_VERSION=min_version, $
NEWEST=newest, $
DIRECTORY=directory, $
TIMEORDER=timeOrder, $
TSTART=tstart, $
TEND=tend, $
TPATTERN=tpattern, $
VERSION=version, $
VREGEX=vRegex
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if n_elements(pwd) gt 0 then CD, pwd
		MrPrintF, 'LogErr'
		return, ''
	endif

	;Input file and directory
	inDir  = file_dirname(file_path)
	inFile = file_basename(file_path)

	;Defaults
	closest   = keyword_set(closest)
	directory = ''
	if n_elements(min_version) eq 0 then min_version = ''
	if n_elements(timeOrder)   eq 0 then timeOrder   = '%Y%M%d%H%m%S'
	if n_elements(tstart)      eq 0 then tstart      = ''
	if n_elements(tend)        eq 0 then tend        = ''
	if n_elements(tpattern)    eq 0 then tpattern    = '%Y-%M-%dT%H:%m:%S'
	if n_elements(version)     eq 0 then version     = ''
	if n_elements(vRegex)      eq 0 then vRegex      = '([0-9]+)\.([0-9]+)\.([0-9]+)'
	
	;Pick the newest
	if n_elements(newest) eq 0 && version eq '' && min_version eq '' $
		then newest = 1B $
		else newest = 0B
	
	;Cannost specify VERSION and NEWEST
	if newest + (version ne '') + (min_version ne '') gt 0 $
		then message, 'NEWEST, VERSION, and MIN_VERSION are mutually exclusive.'

;-------------------------------------------
; Find Files ///////////////////////////////
;-------------------------------------------

	;Search for files
	files = MrFile_Finder(file_path, COUNT=count)

	;Copies are files differ only in their version number
	;   - Separate the file names from the directories
	allDirs  = file_dirname(files)
	allFiles = file_basename(files)

	;Files remaining after filters have been applied
	dirsFound  = allDirs
	filesFound = allFiles

;-------------------------------------------
; Filter by Version ////////////////////////
;-------------------------------------------
	if count gt 0 && ( newest || (version ne '') ) then begin
		;Position in the file names where the version number begins
		vpos = stregex(allFiles, vRegex, LENGTH=vlen)
	
		;Replace version number with the empty string
		;   - Concatenate the parts before and after the version number
		files_noversion = strmid(allFiles, 0, transpose(vpos)) + $
		                  strmid(allFiles, transpose(vpos) + transpose(vlen))

		;Find unique values
		iUniq = MrUniq(files_noversion, IARRAY=iArray, NUNIQ=count, /SORT)
		
		;Select only unique files
		dirsFound  = strarr(count)
		filesFound = strarr(count)

	;-------------------------------------------
	; Filter by Version -- Newest //////////////
	;-------------------------------------------
		if newest then begin
			;Search through all unique files
			for i = 0, count - 1 do begin
				;Pick out the copies
				iCopies = where(iArray eq iUniq[i], nCopies)
				
				;Pick the first file as the newest
				newestFile = allFiles[iCopies[0]]
				newestDir  = allDirs[iCopies[0]]
				
				;Step through all copies
				for j = 1, nCopies - 1 do begin
					;Compare against remaining files
					if MrFile_VersionCompare(allFiles[iCopies[j]], newestFile, vRegex) eq 1 then begin
						newestFile = allFiles[iCopies[j]]
						newestDir  = allDirs[iCopies[j]]
					endif
				endfor
				
				;Replace unique files with newest file and directory
				dirsFound[i]  = newestDir
				filesFound[i] = newestFile
			endfor

	;-------------------------------------------
	; Filter by Version -- Minimum /////////////
	;-------------------------------------------
		endif else if min_version ne '' then begin
			;Search for files with versions GE MIN_VERSION
			vgt = MrFile_VersionCompare(allFiles, min_version, vRegex)
			iFiles = where(vge gt 0, count)
			
			;Select matching files
			if count gt 0 then begin
				dirsFound  = allDirs[iFiles]
				filesFound = allFiles[iFiles]
			endif else begin
				dirsFound  = ''
				filesFound = ''
			endif

	;-------------------------------------------
	; Filter by Version -- Version /////////////
	;-------------------------------------------
		endif else if version ne '' then begin
			;Search through all unique files
			n = 0
			for i = 0, count - 1 do begin
				;Pick out the copies
				iCopies = where(iArray eq iUniq[i], nCopies)
				
				;Search for matches
				tf_match = MrFile_VersionCompare(allFiles[iCopies], version, vRegex) eq 0
				iMatch   = where(tf_match, nMatch)
				
				;Keep the first match
				if nMatch ne 0 then begin
					filesFound[n] = allFiles[iCopies[iMatch[0]]]
					dirsFound[n]  = allDirs[iCopies[iMatch[0]]]
					n++
				endif
			endfor
			count = n

			;Trim results
			if count gt 0 then begin
				filesFound = filesFound[0:count-1]
				dirsFound  = dirsFound[0:count-1]
			endif else begin
				filesFound = ''
				dirsFound  = ''
			endelse
		endif
	endif

;-------------------------------------------
; Filter by Time ///////////////////////////
;-------------------------------------------
	tf_tstart = tstart ne ''
	tf_tend   = tend   ne ''
	if count gt 0 && (tf_tstart || tf_tend) then begin
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
		
		;Is the first token repeated?
		tf_fend = strpos( strmid(inFile, token_pos[0]+2), '%'+tokens[0] ) ne -1
		
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
	endif

;-------------------------------------------
; Directory ////////////////////////////////
;-------------------------------------------

	if count gt 0 then $
		for i = 0, count - 1 do filesFound[i] = filepath(filesFound[i], ROOT_DIR=dirsFound[i])
	if count eq 1 then filesFound = filesFound[0]

	return, filesFound
end
