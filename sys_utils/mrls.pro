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
;       PATTERN:        in, optional, type=string, default=''
;                       A pattern used to match files. Any pattern recognized by IDL's
;                           StrMatch function will do.
;
; :Keywords:
;       COUNT:          in, optional, type=integer
;                       Number of files found.
;       DIRECTORIES:    in, optional, type=boolean, default=0
;                       Return only directories.
;       OUTPUT:         out, optional, type=string/strarr
;                       A named variable into which the directory contents are returned.
;                           If present, the contents are not printed to the display.
;       REGEX:          in, optional, type=boolean, default=0
;                       If set, StRegExp will be used with `PATTERN` instead of StrMatch.
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
;-
pro MrLS, pattern, $
COUNT=count, $
DIRECTORIES=directories, $
OUTPUT=output, $
REGEX=regex, $
SORT=tf_sort
	on_error, 2

	;Defaults
	directories = keyword_set(directories)
	regex       = keyword_set(regex)
	tf_sort     = keyword_set(tf_sort)
	if n_elements(pattern) eq 0 then pattern = ''

	;Search for all things in the current directory
	files = file_search(COUNT=count, NOSORT=~tf_sort, TEST_DIRECTORY=directories)
	
	;Select directories
	if count gt 0 && directories then begin
		;Search for directories
		iDirs = where(file_test(files, /DIRECTORY), count)
		if count eq 0 then return
		
		;Weed out files
		files = files[iDirs]
	endif
	
	;File pattern
	if count gt 0 && pattern ne '' then begin
		;Apply the pattern
		if regex $
			then tf_match = stregex(files, pattern, /BOOLEAN) $
			else tf_match = strmatch(files, pattern)

		;Weed out matches
		iMatch = where(tf_match, count)
		if count eq 0 $
			then files = '' $
			else files = files[iMatch]
	endif

	;Output or print?
	if arg_present(output) then begin
		output = temporary(files)
	endif else begin
		;Count does not like scalars
		if count le 1 then files = [files]
		print, transpose(files)
	endelse
end
