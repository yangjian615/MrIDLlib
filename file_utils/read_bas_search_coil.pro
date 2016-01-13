; docformat = 'rst'
;
; NAME:
;       READ_RBSP_MAG_ASC
;
; PURPOSE:
;+
;       Read data from a British Antarctic Survey search coil magnetometer file. 
;
;       Data can be found `here
;           <http://psddb.nerc-bas.ac.uk/data/access/coverage.php?menu=1&class=101&bc=1>`
;       By clicking on the successive time intervals, you will eventually get to a plot.
;       Clicking on the plot will lead you to a download site.
;
;       Alternatively, there is the MACCs download site run by `Augsburg College
;           <http://space.augsburg.edu/maccs/>`
;
; :Categories:
;   Data Reader, BAS
;
; :Params:
;       FILENAME:           in, required, type=string
;                           The filename of the data to be read.
;       TIME:               out, optional, type=fltarr(N)
;                           The time tag of each point. If `KEEP_TIME_AS_IS` is set,
;                               then this is a 4xN array with each column defined as
;                               [hour, minute, second, microseconds].
;                               Otherwise, this is the time in seconds since midnight.
;
; :Returns:
;       DATA:               out, optional, type=lonarr(3\,N)
;                           Magnetic field compoents: [[Bx], [By], [Bz]].
;
; :Uses:
;   Uses the following external programs::
;       error_message.pro (Coyote Graphics)
;                           
; :Author:
;    Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Copyright 2013 by Matthew Argall
;
; :History:
;   Modification History::
;
;       02/21/2013  -   Written by Matthew Argall
;-
function read_bas_search_coil, filename, time
    compile_opt idl2
    
    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        ;close the file and free its logical unit number
        if n_elements(lun) ne 0 then close, lun
        
        MrPrintF, 'LogErr'
        return, !null
    endif
    
    ;information about the file
    nlines = file_lines(data_file)
    header = 2
    npts = nlines-header
    
    ;allocate memory for the time and field arrays
    time = fltarr(npts)
    mag_data = lonarr(3, npts)
    void = ''
    
    ;Open the document and read the header
    openr, lun, data_file, /GET_LUN
    for i = 0, header - 1 do readf, lun, format='(a0)', void
    
    ;Step through all of the data
    for i = 0, npts-1 do begin
        readf, lun, format='(4x, f8.1, 3(5x, 7i))', t, bx, by, bz
        
        time[i] = t
        mag_data[*,i] = [bx, by, bz]
    endfor
    
    ;close the file
    close, lun
    
    return, mag_data
end