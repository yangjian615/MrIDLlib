; docformat = 'rst'
;
; NAME:
;       DATETIMEGEN
;
; PURPOSE:
;
;+
;   The purpose of this program is to read an orbit file created by
;   rbsp_group_orbit_files.pro. Finds orbits between the desired dates or, alternatively,
;   between the desired orbit numbers.
;
; :Categories:
;       Van Allen Probes, File Reader, Orbit Files
;
; :Params:
;       SDATE:                  in, optional, type=string
;                               The start date and time, with format 
;                                   'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp' (delimiters optional),
;                                   of the first date-time to be generated.
;       EDATE:                  in, optional, type=string/integer
;                               The start date and time, with format 
;                                   'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp' (delimiters optional),
;                                   of the first date-time to be generated. If EDATE is
;                                   an integer, then it is the number of days to be
;                                   generated. In this case, the end time will be
;                                   '23:59:59.999999999' on the last date.
;
; :Keywords:
;       DATE_DELIMITER:         in, optional, type=string, default='-'
;                               The delimiter between the year, month, and date
;       TIME_DELIMITER:         in, optional, type=string, default=':'
;                               The delimiter between the hour, minutes, and seconds
;       SEPARATOR:              in, optional, type=string, default='T'
;                               The separator between date and time.
;
; :Returns:
;       DATETIMEARR:            out, required, type=strarr(2\,N)
;                               The start and end date-times of every day between
;                                   `SDATE` and `EDATE`.
;
; :Uses:
;   Uses the following external programs::
;       dissectDateTime.pro
;       dissectTime.pro
;       days_between.pro
;       dategen.pro
;                               
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
;       05/21/2012  -   Written by Matthew Argall
;-
function dateTimeGen, sdate, edate, $
DATE_DELIMITER = date_delimiter, $
TIME_DELIMITER = time_delimiter
    compile_opt idl2
    on_error, 2
    
    if n_elements(date_delimiter) eq 0 then date_delimiter = '-'
    if n_elements(time_delimiter) eq 0 then time_delimiter = ':'
    if n_elements(separator) eq 0 then separator = 'T'

    ;Was an end date or the number of days given?
    date_type = size(edate, /TYPE)

    ;If a date was given, then dissect it and form a
    if date_type eq 7 then begin
        ;Dissect the start and end dates
        dissectDateTime, sdate, syear, smonth, sday, shour, sminute, ssecond, smilli, smicro, snano
        dissectDateTime, edate, eyear, emonth, eday, ehour, eminute, esecond, emilli, emicro, enano
    
        ;How many days are between the start date and end date?
        sCalDate = syear + smonth + sday
        eCalDate = eyear + emonth + eday

        ndays = days_between(sCalDate, eCalDate)

    ;Otherwise, EDATE is actually the number of days to generate.
    endif else begin
        ndays = edate
        
        ;End at the end of day
        temp_time = '23:59:59.999999999'
        dissectTime, temp_time, eday, ehour, eminute, esecond, emilli, emicro, enano
    endelse

    ;Get all of the intervals.
    if ndays gt 0 then begin
        dates = dategen(sCalDate, ndays+1, DELIMITER='-')
        
        ;Make an array of start times. The first time is given by SDATE. Every day after
        ;that starts at midnight.
        start_times = strarr(ndays+1)
        start_times[0] = shour + time_delimiter + sminute + time_delimiter + ssecond + $
                         '.' + smilli + smicro + snano
        start_times[1:*] = '00' + time_delimiter + '00' + time_delimiter + '00.000'
        
        ;Make an array of end times. The last time is given by EDATE. Every day before
        ;that ends at '23:59:59.999999999'
        end_times = strarr(ndays+1)
        end_times[ndays] = ehour + time_delimiter + eminute + time_delimiter + esecond + $
                           '.' + emilli + emicro + enano
        end_times[0:ndays-1] = '23' + time_delimiter + '59' + time_delimiter + '59.999999999'
    
    ;If the start and end dates are the same, then just stitch everything back together again
    endif else begin
        dates = sCalDate
        start_times = shour + sminute + ssecond + '.' + smilli + smicro + snano
        end_times = ehour + eminute + esecond + '.' + emilli + emicro + enano
    
    endelse
    
    ;Combine the dates and times.
    dateTimeArr = transpose( [[dates + separator + start_times], $
                              [dates + separator + end_times]] )
                   
    return, dateTimeArr
end