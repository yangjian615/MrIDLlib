; docformat = 'rst'
;
; NAME:
;       FILENAME_GENERATOR
;
; PURPOSE:
;+
;   The purpose of this program is to take a file name, find a date within the name
;   somewhere, and create a sequence if file names spanning from `SDATE` to `EDATE`.
;
; :Categories:
;   Van Allen Probes, Ephemeris
;
; :Params:
;
;       FILENAME:               in, required, type=string
;                               File name, which contains a date, from which more names
;                                   are to be generated.
;       SDATE:                  in, required, type=string
;                               Start date of the first file. Must be in the format
;                                   'YYYY-MM-DD', where the delimiters are optional.
;       EDATE:                  in, required, type=string
;                               End of the last file. Must be in the format
;                                   'YYYY-MM-DD', where the delimiters are optional.
;
; :Returns:
;       FILES:                  out, required, type=strarr
;                               An array of file names with dates ranging from SDATE to
;                                   EDATE.
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
;       05/21/2013  -   Written by Matthew Argall
;-
function filename_generator, filename, sdate, edate
    compile_opt idl2
    on_error, 2
    
    ;Break FILENAME into supexpressions
    pos = stregex(filename, '(.*)([0-9]{4})([^0-9]?)([0-9]{2})([^0-9]?)([0-9]{2})(.*)', $
                  /SUBEXP, LEN=length)
    
    ;Extract the various parts of the file name
    head = strmid(filename, pos[1], length[1])
    year = strmid(filename, pos[2], length[2])
    sep1 = strmid(filename, pos[3], length[3])
    month = strmid(filename, pos[4], length[4])
    sep2 = strmid(filename, pos[5], length[5])
    day = strmid(filename, pos[6], length[6])
    tail = strmid(filename, pos[7], length[7])
    
    ;Create a series of dates between SDATE and EDATE
    dates = dategen(sdate, edate)
    dissectDate, dates, yr, mo, dy
    
    ;Create an array of files exactly like the one given, but with the date changed
    files = head + yr + sep1 + mo + sep2 + dy + tail

    return, files

end