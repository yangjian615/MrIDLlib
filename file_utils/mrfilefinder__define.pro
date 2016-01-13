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
;       2015-12-03  -   Error handled by MrPrintF, 'LogErr'. - MRA
;-
;*****************************************************************************************
;+
;   Change directories.
;
; :Params:
;       DESTINATION:    in, optional, type=string
;                       Directory to be made the present working directory.
;
; :Examples:
;   Change directories
;       IDL> oFF -> PWD
;          /Users/argall/
;       IDL> oFF -> CD, 'Documents'
;       IDL> oFF -> PWD
;          /Users/argall/Documents
;-
pro MrFileFinder::CD, destination
	compile_opt strictarr
	on_error, 2

	;Forward to SetPath
	self -> SetPWD, destination
end


;+
;   Search for files
;
; :Private:
;-
function MrFileFinder::FindFile, path, recur, $
COUNT=count, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Search for files
	if n_elements(recur) eq 0 $
		then files = file_search(path,        COUNT=count, /TEST_REGULAR, /FULLY_QUALIFY_PATH, _STRICT_EXTRA=extra) $
		else files = file_search(path, recur, COUNT=count, /TEST_REGULAR, /FULLY_QUALIFY_PATH, _STRICT_EXTRA=extra)

	;Return
	return, files
end


;+
;   Search for files
;
; :Private:
;
; :Params:
;       PATTERN:        in, required, type=string
;                       A search pattern that can include any token recognized by
;                           MrTokens.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       A named variable to receive the number of files found.
;       PATHSEP:        in, optional, type=string, default=Path_Sep()
;                       Character(s) that separate path segments.
;-
function MrFileFinder::FindPattern, pattern, $
COUNT=count, $
PATHSEP=pathsep
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		cd, old_dir
		MrPrintF, 'LogErr'
		return, ''
	endif

;---------------------------------------------------------------------
; Get Current Directory //////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Get the current directory for save keeping
	cd, CURRENT=pwd
	if n_elements(pathsep) eq 0 then pathsep = path_sep()

	;Look for the system root specifier.
	;   - Searching for MrTokens requires me to split FILEPATH at each SEP
	;   - Unix root is '/', so will be removed. Windows is C:, so will not be removed
	case !version.os_family of
		'unix':    sysroot = '/'
		'windows': sysroot = ''
		else: begin
			message, 'Unexpected file system. Errors may occur.', /INFORMATIONAL
			sysroot = ''
		endcase
	endcase

;---------------------------------------------------------------------
; First Token ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Break the directory pattern into parts
	parts = strsplit(pattern, pathsep, COUNT=nParts, /EXTRACT)

	;Find the directory elements with a "%" token identifier
	allTokens = strjoin(MrTokens())
	iToken    = where(stregex(parts, '%', /BOOLEAN), nTokens)

	;Parse the pattern into a part without tokens and a part with tokens
	;   root       - Leading path segments without tokens
	;   subpattern - Trailing path segments with tokens
	case iToken[0] of
		;Zero tokens.
		-1: begin
			root       = file_dirname(pattern)
			subpattern = file_basename(pattern)
		endcase
		
		;Token is in the first directory of the directory tree
		0: begin
			root       = sysroot
			subpattern = parts[0]
		endcase
		
		;Token is in the middle of the directory tree
		else: begin
			root       = filepath('', ROOT_DIR=sysroot, SUBDIR=parts[0:iToken[0]-1])
			subpattern = parts[iToken[0]]
			subpattern = MrTokens_ToRegex(subpattern)
		endcase
	endcase
	
	;Look for whole words
	subpattern = '^' + subpattern + '$'

;---------------------------------------------------------------------
; Parse This Piece ///////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Change to the directory
	cd, root
	
	;Get all of the files in the current directory
	MrLS, subpattern, COUNT=count, OUTPUT=pathOut, /REGEX
	if count eq 0 then begin
		cd, pwd
		return, ''
	endif

	;Combine the matches with the root
	; - Nothing left to parse if there are no more tokens or if we are at the last token.
	; - Append the remaining parts together.
	if nTokens eq 0 || iToken[0] eq nParts-1 $
		then remainder = '' $
		else remainder = filepath('', ROOT_DIR=pathsep, SUBDIR=parts[iToken[0]+1:nParts-1])

;---------------------------------------------------------------------
; Parse Next Piece ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	if remainder ne '' then begin
		;Step through each directory found
		for i = 0, count - 1 do begin
			;Append the directory to the root
			; - root / dirOut[i] / remainder
			next = filepath(remainder, ROOT_DIR=root, SUBDIR=pathOut[i])
			
			;Search again
			tempTree = MrFile_Finder(next, COUNT=tempCount)
		
			;Create the tree
			if tempCount gt 0 then begin
				if n_elements(tree) eq 0 $
					then tree = temporary(tempTree) $
					else tree = [tree, temporary(tempTree)]
			endif
		endfor

;---------------------------------------------------------------------
; Parse Last Piece ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		;Form the complete file path
		tree = filepath(pathOut, ROOT_DIR=root)
	endelse
	
	;Switch back to the original directory
	cd, pwd

	;Count the results
	count = n_elements(tree)
	if count eq 0 then tree = ''

	return, tree
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
;
; :Examples:
;   Find files via a standard File_Search()
;      IDL> directory = '/Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/'
;      IDL> path      = filepath('mms1_hpca_srvy_l1b_ion*.cdf', ROOT_DIR=directory)
;      IDL> files     = oFF -> Find(path)
;      IDL> print, files
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/mms1_hpca_srvy_l1b_ion_20150622021541_v0.0.0.cdf
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/mms1_hpca_srvy_l1b_ion_20150622080034_v0.0.0.cdf
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/mms1_hpca_srvy_l1b_ion_20150622094331_v0.0.0.cdf
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/mms1_hpca_srvy_l1b_ion_20150622140037_v0.0.0.cdf
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/mms1_hpca_srvy_l1b_ion_20150622160906_v0.0.0.cdf
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/mms1_hpca_srvy_l1b_ion_20150622200041_v0.0.0.cdf
;
;   Recursively search for files via File_Search() (Only a subset of results are shown).
;      IDL> path  = '/Users/argall/data/mms/'
;      IDL> recur = 'mms*.cdf'
;      IDL> files = oFF -> Find(path, recur)
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/ion/2015/mms1_hpca_srvy_l1b_ion_20150622021541_v0.0.0.cdf
;          /Users/argall/data/mms/mms1/l1b/srvy/hpca/moments/2015/mms1_hpca_srvy_l1b_moments_20150622021549_v0.0.0.cdf
;          /Users/argall/data/mms/mms1/ql/srvy/afg/2015/mms1_afg_srvy_ql_20150505_v0.0.0.cdf
;          /Users/argall/data/mms/mms2/sitl/fast/fpi/2015/mms2_fpi_fast_sitl_20150622000000_v0.0.0.cdf
;          /Users/argall/data/mms/mms3/l1b/srvy/feeps/electron/2015/mms3_feeps_srvy_l1b_electron_20150622000000_v1.1.1.cdf
;          /Users/argall/data/mms/mms3/ql/srvy/afg/2015/mms3_afg_srvy_ql_20150505_v0.0.0.cdf
;          /Users/argall/data/mms/mms3/sitl/fast/fpi/2015/mms3_fpi_fast_sitl_20150507000000_v0.0.0.cdf
;          /Users/argall/data/mms/mms4/ql/srvy/afg/2015/mms4_afg_srvy_ql_20150505_v0.0.0.cdf
;
;   Find files using MrTokens
;      IDL> dir   = '/Users/argall/autoplot_data/fscache/http/mmsdata.sr.unh.edu/mms1/dfg/srvy/ql/%Y/%M/'
;      IDL> path  = filepath('mms1_dfg_srvy_ql_%Y%M%d_v0.0.3.cdf', ROOT_DIR=dir)
;      IDL> files = oFF -> Find(path)
;      IDL> print, files
;          /Users/argall/autoplot_data/fscache/http/mmsdata.sr.unh.edu/mms1/dfg/srvy/ql/2015/08/mms1_dfg_srvy_ql_20150815_v0.0.3.cdf
;          /Users/argall/autoplot_data/fscache/http/mmsdata.sr.unh.edu/mms1/dfg/srvy/ql/2015/08/mms1_dfg_srvy_ql_20150816_v0.0.3.cdf
;          /Users/argall/autoplot_data/fscache/http/mmsdata.sr.unh.edu/mms1/dfg/srvy/ql/2015/08/mms1_dfg_srvy_ql_20150820_v0.0.3.cdf
;-
function MrFileFinder::Find, path, recur, $
COUNT=count
	compile_opt strictarr
	
	catch, the_error
	if the_error gt 0 then begin
		catch, /CANCEL
		cd, old_dir
		MrPrintF, 'LogErr'
		return, ''
	endif

	;Switch to directory of interest
	cd, self.pwd, CURRENT=old_dir

	;Find files
	;   - Search using MrTokens if there are "%"-signs present
	;   - Otherwise use File_Search() 
	if n_elements(recur) gt 0 || strpos(path, '%') eq -1 $
		then files = self -> FindFile(path, recur, COUNT=count) $
		else files = self -> FindPattern(path, COUNT=count)

	;Return to the working directory
	cd, old_dir
	
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
TPATTERN=tpatern, $
TIMEORDER=timeOrder, $
VERSION=version, $
VREGEX=vregex
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
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
;
; :Examples:
;   List the contents of the current directory::
;       IDL> oFF -> LS
;         Applications
;         Desktop
;         Documents
;         Downloads
;         Library
;         Movies
;         Music
;         Pictures
;
;   List contents that begin with "Do"
;       IDL> oFF -> LS, 'Do*'
;         Documents
;         Downloads
;
;   List contents of the "./Documents" directory
;       IDL> oFF -> LS, 'Documents'
;         gitWiki
;         IDL
;         img2pdf.workflow
;         Letter to a Prospective Student.odt
;         MATLAB
;         OpenOfficeFormula.pdf
;         Papers
;         pdf2tiff.workflow
;         unix_commands.txt
;         Work
;
;   List contents of the "../" directory
;       IDL> oFF -> LS, '..'
;         argall
;         Shared
;
;   List contents two directories up: "../../"
;       IDL> oFF -> LS, '../../'
;         Applications
;         bin
;         dev
;         etc
;         home
;         opt
;         Users
;         usr
;
;   List contents of a parallel directory: '../Shared'
;       IDL> oFF -> LS, '../Shared'
;         adi
;         Library
;         SC Info
;-
pro MrFileFinder::LS, searchstr, $
_REF_EXTRA=ref_extra
	on_error, 2

	;Change to the current directory
	cd, self.pwd, CURRENT=old_dir

	;Get the directory listings
	MrLS, searchstr, _STRICT_EXTRA=extra
	
	;Change back
	cd, old_dir
end


;+
;   Print the present working directory.
;
; :Examples:
;    Get the present working directory.
;       IDL> oFF -> PWD
;          /home/username
;-
pro MrFileFinder::PWD
	print, '  ' + self.pwd
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
		MrPrintF, 'logerr'
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

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		cd, old_path
		MrPrintF, 'logerr'
		return
	endif

	;To make sure the object and IDL do not get out of
	;sync, we are going to keep track of the path independently
	;from wherever IDL is currently at.
	cd, self.pwd, CURRENT=old_path
	cd, destination

	;Update
	;   - Use CD to fully qualify the path
	cd, CURRENT=pwd
	self.pwd = pwd
	
	;Return to the old path
	cd, old_path
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
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Defaults
	if n_elements(directory) eq 0 then cd, CURRENT=directory
	if n_elements(timeOrder) eq 0 then timeOrder = '%Y%M%d'
	if n_elements(tPattern)  eq 0 then tPattern  = '%Y-%M-%dT%H:%m:%S'
	if n_elements(version)   eq 0 then version   = ''
	if n_elements(vregex)    eq 0 then vregex    = '([0-9]+)\.([0-9]+)\.([0-9])'
	
	;Change directories
	if directory ne '' then self -> SetPWD, directory
	
	;Set times
	self -> SetTime, tstart, tend, TIMEORDER=timeOrder
	
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