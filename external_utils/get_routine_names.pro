; docformat = 'rst'
;
; NAME:
;       GET_ROUTINE_NAMES
;
; PURPOSE:
;+
; The purpose of this program is to make a text file containing all the IDL-defined
; procedures and function names, for incorporation into the text module for TextWrangler
; Note: these are inbuilt routines, not those written in the IDL language
;
; :Params:
;       outputFile:         in, optional, type=string
;                           Name of the output file. If ommitted, a dialog box will appear
;                               asking the user to pick a file.
;
; :Keywords:
;       LACASE:             in, optional, type=boolean, default=0
;                           If set, routines names will be written in lowercase instead
;                               of uppercase.
;       LIBRARY:            in, optional, type=boolean, default=0
;                           If set, library functions will be incorporated into the output.
;                               Library functions are taken from the `LIBDIR`.
;       LIBDIR:             in, optional, type=strarr, default='/Applications/exelis/idl82/lib/'
;                           Directories in which to search for '*.pro' files. Those files
;                               will be included in the output. Providing this keyword
;                               automatically sets `LIBRARY`=1.
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2006/06/21  -   initial write - WJP
;       2014/01/19  -   Commented, Added the LIBRARY and LIBDIR keywords. - Matthew R Argall
;-
PRO GET_ROUTINE_NAMES, outputFile, $
LCASE=lcase, $
LIBDIR=libdir, $
LIBRARY=library

;---------------------------------------------------------------------
;Check outputFile ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Open a dialog box to allow the user to pick an output file
    IF (N_ELEMENTS(outputFile) EQ 0) THEN outputFile = DIALOG_PICKFILE(/write)

    ;Make sure an output file was selected
    IF (N_ELEMENTS(outputFile) EQ 0) THEN BEGIN
        MESSAGE, 'No output file specified, exiting', /INFORMATIONAL
        RETURN
    ENDIF

    ;Make sure we can write to the directory
    IF ~FILE_TEST(FILE_DIRNAME(outputFile), /DIRECTORY) THEN BEGIN
        MESSAGE, 'Unable to access directory: ' + file_dirname(outputFile), /INFORMATIONAL
        RETURN
    ENDIF
    
    lcase = KEYWORD_SET(lcase)
    library = KEYWORD_SET(library)
    if N_ELEMENTS(LIBDIR) GT 0 THEN library = 1

;---------------------------------------------------------------------
;Get Routine Names ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Get all of the system procedures and functions
    procedures = ROUTINE_INFO(/SYSTEM)
    functions  = ROUTINE_INFO(/SYSTEM, /FUNCTIONS)

    ;Get library procedures and functions
    IF library THEN BEGIN
        ;Default library directory
        IF N_ELEMENTS(libdir) EQ 0 THEN libdir = '/Applications/exelis/idl82/lib/'
        
        ;Get the programs
        libfiles = FILE_SEARCH(libdir, '*.pro')
    ENDIF
    
    ;Lowercase?
    IF lcase THEN BEGIN
        procedures = STRLOWCASE(TEMPORARY(procedures))
        functions  = STRLOWCASE(TEMPORARY(functions))
        IF library THEN libfiles = STRLOWCASE(TEMPORARY(libfiles))
    ENDIF
     
;---------------------------------------------------------------------
;Create the Output File //////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Open the file for writing
    OPENW, lun, outputFile, /GET_LUN

    ;Create a header, then print all of the procedure names, encased in XML <string> tags
    PRINTF, lun, '<!-- System Procedures -->'
    FOR ii = 0, N_ELEMENTS(procedures) - 1 DO $
        PRINTF, lun, '<string>' + procedures[ii] + '</string>'

    ;Do the same for functions.
    PRINTF, lun, ''
    PRINTF, lun, '<!-- System Functions -->'
    FOR ii = 0, N_ELEMENTS(functions) - 1 DO $
        PRINTF, lun, '<string>' + functions[ii] + '</string>'

    ;And library programs.
    IF library THEN BEGIN
        PRINTF, lun, '<!-- Library Programs -->'
        FOR ii = 0, N_ELEMENTS(libfiles) - 1 DO $
            PRINTF, lun, '<string>' + cgRootName(libfiles[ii]) + '</string>'
    ENDIF
        
    ;Close the file and free its LUN
    FREE_LUN, lun
    
    ;Let the user know where the file was written to.
    MESSAGE, 'System procedures and functions written to ' + outputFile, /INFORMATIONAL
END
