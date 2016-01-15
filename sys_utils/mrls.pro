; docformat = 'rst'
;
; NAME:
;       MrLS
;
; PURPOSE:
;+
;    Print the list of files in the current directory.
;
; :Categories:
;   System tools
;
; :Examples:
;   Print the contents of the current directory::
;       IDL> MrLS
;
; :Params:
;       SEARCHSTR:      in, optional, type=string, default=''
;                       A pattern to match against files in the current directory.
;                           Files and directoires matching this search string will
;                           be returned. If the search string is preceeded by a
;                           directory path, the search will take place in that
;                           directory instead of the current directory.
;
; :Keywords:
;       COUNT:          in, optional, type=integer
;                       Number of files found.
;       DIRECTORY:      in, optional, type=boolean, default=0
;                       Return only directories.
;       OUTPUT:         out, optional, type=string/strarr
;                       A named variable into which the directory contents are returned.
;                           If present, the contents are not printed to the display.
;       REGEX:          in, optional, type=boolean, default=0
;                       If set, StRegExp will be used with `PATTERN` instead of StrMatch.
;
; :Examples:
;   List the contents of the current directory::
;       IDL> MrLS
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
;       IDL> MrLS, 'Do*'
;         Documents
;         Downloads
;
;   List contents of the "./Documents" directory
;       IDL> MrLS, 'Documents'
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
;       IDL> MrLS, '..'
;         argall
;         Shared
;
;   List contents two directories up: "../../"
;       IDL> MrLS, '../../'
;         Applications
;         bin
;         dev
;         etc
;         home
;         opt
;         Users
;         usr
;
;   List contents of a parallel directory: '../shared'
;       IDL> MrLS, '../../'
;         adi
;         Library
;         SC Info
;      
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
;       2015-03-30  -   Written by Matthew Argall
;       2015-04-28  -   Marking directories causes regex filter to fail. Fixed. - MRA
;       2015-09-02  -   Search in any directory. DIRECTORIES keyword renamed
;                           to DIRECTORY and does not have redundant check. - MRA
;       2015-09-29  -   Special directories and wildcards work. Added examples. - MRA
;-
pro MrLS, searchstr, $
COUNT=count, $
DIRECTORY=directory, $
OUTPUT=output, $
REGEX=regex, $
SORT=tf_sort
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		cd, pwd
		void = cgErrorMSG(/QUIET)
		return
	endif

	;Defaults
	tf_regex = keyword_set(regex)
	tf_sort  = keyword_set(tf_sort)
	output   = ''

	;Get the current directory
	cd, CURRENT=pwd

	;Extract the directory from the search pattern.
	if n_elements(searchstr) gt 0 then begin
		dir  = file_dirname(searchstr)
		srch = file_basename(searchstr)
	endif else begin
		dir  = '.'
		srch = ''
	endelse

	;Looking in special directories?
	;   - Special directories:
	;         '~'   =  home directory
	;         '.'   = current directory
	;         '..'  = Up one directory
	;   - The strings "~/", "./", and "../" will return "." as dirname and
	;     "~", ".", and ".." as basename.
	;   - Switch the special directory over to DIR
	if stregex(srch, '^((~|\.|\.\.)' + path_sep() + '?)+', /BOOLEAN) then begin
		dir  = filepath(srch, ROOT_DIR=dir)
		srch = ''
	
	;Look in a specific directory?
	;   - In a directory chain, File_BaseName() returns the final directory while
	;     File_DirName() returns the penultimate and all others.
	;   - Re-combine both to test if it is a directory.
	;   - Do not expand wildcard characters.
	endif else if file_test(filepath(srch, ROOT_DIR=dir), /DIRECTORY, /NOEXPAND_PATH) then begin
		dir  = filepath(srch, ROOT_DIR=dir)
		srch = ''
	endif

	;Change directories if we are not looking in the current directory
	if ~stregex(dir, '^\.' + path_sep() + '?$', /BOOLEAN) then begin
		if ~file_test(dir) then message, 'Cannot look in "' + dir + '".'
		cd, dir
	endif

	;Regular search.
	;   - The empty string does not warrant a special search
	if srch eq '' then tf_regex = 0
	if ~tf_regex then begin
		files = file_search(srch, COUNT=count, NOSORT=~tf_sort, TEST_DIRECTORY=directory)
	
	;Search with regular expression.
	endif else begin
		files = file_search(COUNT=count, NOSORT=~tf_sort, TEST_DIRECTORY=directory)

		;File pattern
		if count gt 0 && srch ne '' then begin
			;Apply the pattern
			tf_match = stregex(files, srch, /BOOLEAN)

			;Weed out matches
			iMatch = where(tf_match, count)
			if count eq 0 $
				then files = '' $
				else files = files[iMatch]
		endif
	endelse
	
	;Change back to the original directory
	cd, pwd

	;Output or print?
	if arg_present(output) then begin
		output = temporary(files)
	endif else begin
		;Count does not like scalars
		if count le 1 then files = [files]
		print, '  ' + transpose(files)
	endelse
end
