; docformat = 'rst'
;
; NAME:
;       MrFileFinder__Define
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
;   The purpose of this class is to provide a means of automatically finding data files
;   that match a given date and time pattern. There are two mechanisms in place for
;   file searching: the "FindFile" and "FileSearch" methods.
;
;   The FindFile method can be applied to any file system-like structure, such as HDF5
;   files and http protocol. Simply over-ride the GetPWD, LS, and SetPWD methods when
;   necessary.
;
;   See MrTokens.pro for a list of tokens that can be included in file names.
;   Or do the following::
;       IDL> print, MrTokens()
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
;       2015-09-13  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Change directories.
;
; :Params:
;       DESTINATION:    in, optional, type=string
;                       Directory to be made the present working directory.
;-
pro MrFileFinder::CD, destination
	compile_opt strictarr
	on_error, 2

	;Try to change directories
	cd, destination
	
	;Update the current directory
	;   - Use CD so that the path is fully qualified
	cd, CURRENT=pwd
	self.pwd = pwd
end


;+
;   Search the file system for files that match the file pattern.
;
; :Params:
;       PATH_STR:       in, optional, type=string
;                       File paths to match
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files found.
;
; :Returns:
;       FILES:          Fully qualified path to the files or directories that match
;                           `PATTERN`.
;-
function MrFileFinder::Find, pattern, $
COUNT=count
	compile_opt strictarr
	on_error, 2

	;Find files
	files = MrFile_Finder(pattern, COUNT=count)
	return, files
end


;+
;   Return the present working directory.
;-
function MrFileFinder::GetPWD
	return, self.pwd
end


;+
;   Get class properties.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of files found that match `FILENAME`.
;       ETIME:              out, optional, type=string
;                           Time string. Files that begin after this time will not be
;                               included in the search results.
;       FILENAME:           out, optional, type=string
;                           A complete file path of a whose date and time information
;                               have been replaced by the tokens defined above.
;       PATH_SEPARATOR:     out, optional, type=string
;                           String used to separate elements of a file path.
;       STIME:              out, optional, type=string
;                           Time string. Files that end before this time will not be
;                               included in the search results.
;       TPATTERN:           out, optional, type=string
;                           Token string denoting how `STIME` and `ETIME` are formatted.
;-
pro MrFileFinder::GetProperty, $
PWD=pwd, $
TSTART=tstart, $
TEND=tend, $
TPATTERN=tpatern
TIMEORDER=timeOrder, $
VERSION=version, $
VREGEX=vregex
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Get properties of the class.
	if arg_present(pwd)       then pwd       = self.pwd
	if arg_present(timeOrder) then timeOrder = self.time_order
	if arg_present(tstart)    then tstart    = self.tstart
	if arg_present(tend)      then tend      = self.tend
	if arg_present(tpattern)  then tpattern  = self.tpattern
	if arg_present(version)   then version   = self.version
	if arg_present(vregex)    then vregex    = self.vregex
end


;+
;   Print the contents of a directory.
;
; :Params:
;       SEARCHSTR:      in, optional, type=string, default='*'
;                       A search string used to filter directory listing results.
;                           If not provided, all items in current directory are
;                           returned. If a path specification is included, the contents
;                           of that path are returned.
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrLS. These include COUNT, DIRECTORY,
;                           OUTPUT, REGEX, SORT.
;-
pro MrFileFinder::LS, searchstr, $
_REF_EXTRA=ref_extra
	on_error, 2

	MrLS, searchstr, _STRICT_EXTRA=extra
end


;+
;   Print the present working directory.
;-
pro MrFileFinder::PWD
	print, self.pwd
end


;+
;   Search the file system for files that match the file pattern. Filter results
;   according to time range, version
;
; :Params:
;       PATH_STR:       in, optional, type=string
;                       File paths to match
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files found.
;
; :Returns:
;       FILES:          Fully qualified path to the files or directories that match
;                           `PATTERN`.
;-
function MrFileFinder::Search, file_path, $
CLOSEST=closest, $
COUNT=count, $
NEWEST=newest
	compile_opt strictarr
	on_error, 2

	;Search for files
	files = MrFile_Search( file_path, $
	                       CLOSEST   = closest, $
	                       COUNT     = count, $
	                       NEWEST    = newest, $
	                       DIRECTORY = self.pwd, $
	                       TIMEORDER = self.time_order, $
	                       TSTART    = self.tstart, $
	                       TEND      = self.tend, $
	                       TPATTERN  = self.tpattern, $
	                       VERSION   = self.version, $
	                       VREGEX    = self.vregex $
	                     )
	return, files
end


;+
;   Set class properties.
;
; :Keywords:
;       ETIME:              in, optional, type=string
;                           Time string. Files that begin after this time will not be
;                               included in the search results.
;       FILENAME:           in, optional, type=string
;                           A complete file path of a whose date and time information
;                               have been replaced by the tokens defined above.
;       PATH_SEPARATOR:     in, optional, type=string
;                           String used to separate elements of a file path.
;       STIME:              in, optional, type=string
;                           Time string. Files that end before this time will not be
;                               included in the search results.
;       TPATTERN:           in, optional, type=string
;                           Token string denoting how `STIME` and `ETIME` are formatted.
;-
pro MrFileFinder::SetTime, tstart, tend, $
TIMEORDER=timeorder
	compile_opt strictarr
	on_error, 2
	
	if n_elements(tstart)    eq 0 then tstart    = ''
	if n_elements(tend)      eq 0 then tend      = ''
	if n_elements(timeOrder) eq 0 then timeOrder = self.time_order
	
	;Make sure the time matches its string
	if tstart ne '' && ~MrTokens_IsMatch(tstart, timeOrder) $
		then message, 'TSTART must match TIMEORDER (' + timeOrder + ').'
	if tend ne '' && ~MrTokens_IsMatch(tend, timeOrder) $
		then message, 'TEND must match TIMEORDER (' + timeOrder + ').'

	;Set time range
	self.tstart     = tstart
	self.tend       = tend
	self.time_order = timeOrder
end


;+
;   Set class properties.
;
; :Keywords:
;       ETIME:              in, optional, type=string
;                           Time string. Files that begin after this time will not be
;                               included in the search results.
;       FILENAME:           in, optional, type=string
;                           A complete file path of a whose date and time information
;                               have been replaced by the tokens defined above.
;       PATH_SEPARATOR:     in, optional, type=string
;                           String used to separate elements of a file path.
;       STIME:              in, optional, type=string
;                           Time string. Files that end before this time will not be
;                               included in the search results.
;       TPATTERN:           in, optional, type=string
;                           Token string denoting how `STIME` and `ETIME` are formatted.
;-
pro MrFileFinder::SetProperty, $
TPATTERN  = tpattern, $
VERSION   = version, $
VREGEX    = vregex
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	if n_elements(tPattern)  gt 0 then self.tPattern  = tPattern
	if n_elements(version)   gt 0 then self.version   = version
	if n_elements(vregex)    gt 0 then self.vregex    = vregex
end


;+
;   Change the present working directory. An alternative to the CD method.
;
; :Params:
;       DESTINATION:    in, optional, type=string
;                       Directory to be made the present working directory.
;-
pro MrFileFinder::SetPWD, destination
	on_error, 2

	;Set the current directory
	cd, destination

	;Update
	;   - Use CD to fully qualify the path
	cd, CURRENT=pwd
	self.pwd = pwd
end


;+
;   Clean up after the object is destroy
;-
pro MrFileFinder::cleanup
	;Nothing to do yet
end


;+
;   The initialization method. Here, a directory and filename can be given with a date
;   and time code embedded.
;
; :Params:
;       FILENAME:       in, required, type=string
;                       File path of the file to be found. It can contain any token
;                           recognized by MrTimesTokenToRegex.pro.
;
; :Keywords:
;       ETIME:          in, optional, type=string
;                       A string representing the end time of the interval intended
;                           to be read.
;       PATH_SEPARATOR: in, optional, type=string, default=Path_Sep()
;                       Character that delimites directories on the file system.
;       STIME:          in, optional, type=string
;                       A string representing the start time of the interval intended
;                           to be read.
;       TPATTERN:       in, optional, type=string, default="%Y-%M-%dT%H:%m:%S"
;                       A string dictating, by use of tokens, how `STIME` and `ETIME`
;                           are formatted.
;-
function MrFileFinder::init, tstart, tend, $
DIRECTORY = pwd, $
TIMEORDER = timeOrder, $
TPATTERN  = tpattern, $
VERSION   = version, $
VREGEX    = vregex
	compile_opt strictarr
	
	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return, 0
	endif
	
	;Defaults
	if n_elements(directory) eq 0 then directory = ''
	if n_elements(timeOrder) eq 0 then timeOrder = '%Y%M%d'
	if n_elements(tPattern)  eq 0 then tPattern  = '%Y-%M-%dT%H:%m:%S'
	if n_elements(version)   eq 0 then version   = ''
	if n_elements(vregex)    eq 0 then vregex    = '([0-9]+)\.([0-9]+)\.([0-9])'
	
	;Make sure the time matches its string
	if ~MrTokens_IsMatch(tstart, timeOrder) $
		then message, 'TSTART must match TIMEORDER (' + timeOrder + ').'
	if ~MrTokens_IsMatch(tend, timeOrder) $
		then message, 'TEND must match TIMEORDER (' + timeOrder + ').'
	
	;Change directories
	if directory ne '' then self -> SetPWD, directory
	
	;Set times
	self.tstart     = tstart
	self.tend       = tend
	self.time_order = timeOrder
	
	;Set other properties
	self -> SetProperty, TPATTERN  = tpattern, $
	                     VERSION   = version, $
	                     VREGEX    = vregex
	
	return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:              out, optional, type=structure
;                           Class definition structure.
;
; :Fields:
;       DIRECTORY:          Directory in which to search for files.
;       ETIME:              Time at which to stop reading data.
;       FILE_NAMES:         Files that have been found.
;       FILEBASE:           File base name of the file to be found.
;       NFILES:             Number of files found that match the filename given.
;       PATH_SEP:           System path separator.
;       STIME:              Time at which to begin reading data.
;       TIME_PATTERN:       Static string of how time is represented internally.
;       TPATTERN:           Format string for parsing `STIME` and `ETIME`.
;-
pro MrFileFinder__define, class
	compile_opt strictarr
	
	class = { MrFileFinder, $
	          inherits IDL_Object, $
	          pwd:        '', $
	          time_order: '', $
	          tstart:     '', $
	          tend:       '', $
	          tpattern:   '', $
	          version:    '', $
	          vregex:     '' $
	        }
end