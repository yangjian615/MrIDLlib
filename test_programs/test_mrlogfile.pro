; docformat = 'rst'
;
; NAME:
;       Test_MrLogFile
;
; PURPOSE:
;+
;       Test the MrLogFile error logger.
;               
;
; :Categories:
;       Test Program
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
;       2015/10/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Log an error at a second level of the call stack.
;-
pro test_mrlogfile_level2, olog
	compile_opt idl2
	
	;Log an error
	olog -> AddError, 'User error at level 2.'
end


;+
;   Create a MrLogFile, log an error, and decend another level in the callstack.
;-
pro test_mrlogfile
	compile_opt idl2
	
	;Create an error logging object
	olog = MrLogFile('~/test.txt')
	
	;Write to the log file
	;   - Causes the object to open the log file and assing a LUN
	olog -> AddError, 'User error.'
	
	;Get the lun
	lun  = olog.lun

	;Assign the same file to standard output and error.
	lun  = MrStdOut(lun)
	lun  = MrStdErr(lun)
	olog = MrStdLog(olog)
	
	;Log an error
	MrPrintF, 'stdout', 'Standard output text.'
	MrPrintF, 'stderr', 'Standard error text.'
	MrPrintF, 'stdlog', 'Standard log text.'
	
	;Descend another level
	test_mrlogfile_level2, olog
	
	;Destroy the object
	obj_destroy, olog
end