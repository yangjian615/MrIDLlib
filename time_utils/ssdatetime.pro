; docformat = 'rst'
;
; NAME:
;       SSDATETIME
;
; PURPOSE:
;+
;       The purpose of this program is to calculate the number of seconds between two
;       different date-time strings. The format for each date-time string must be of
;       the form 'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp'. The decimal places and the
;       delimiters are optional; however, both input times must be formatted in the same
;       manner.
;
; :Examples:
;       See the main level program at the end of this file::
;           IDL> .r ssDateTime
;
; :Params:
;       SDATETIME:              in, required, type=strarr
;                               Start date of the first file. Must be in the format
;                                   'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp', where the
;                                   delimiters and decimal places are optional.
;       EDATETIME:              in, required, type=strarr
;                               End of the last file. Must be in the format
;                                   'YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp', where the
;                                   delimiters and decimal places are optional.
;
; :Returns:
;       SECONDS_BETWEEN:        out, type=dblarr
;                               The number of seconds between `SDATETIME` and `EDATETIME`.
;
; :Uses:
;   Uses the following external programs::
;       days_between.pro
;       dissectDateTime.pro
;       hms_to_ssm.pro
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
;       05/22/2013  -   Written by Matthew Argall
;       05/23/2013  -   SDATETIME and EDATETIME can now be arrays. - MRA
;-
function ssDateTime, sDateTime, eDateTime
	compile_opt idl2
    on_error, 2

    ;Allow sDateTime to be a scalar.
    nStart = n_elements(sDateTime)
    nEnd = n_elements(eDateTime)
    if nStart ne nEnd and nStart ne 1 then $
        message, 'sDateTime must be a scalar or have the same number of elements as eDateTime.'
    
    ;SDATETIME must be a scalar if it has one element
    _sDateTime = (nStart eq 1) ? sDateTime[0] : sDateTime
    
    ;allocate memory
    seconds_between = dblarr(nEnd)

    ;Split the date and time
    dissectDateTime, _sDateTime, sDate, sTime, /SEPARATE
    dissectDateTime, eDateTime, eDate, eTime, /SEPARATE

    ;Find matching and disparate dates
    iSame = where(eDate eq sDate, nSame, COMPLEMENT=iDiff, NCOMPLEMENT=nDiff)

;-----------------------------------------------------------------------------------------
;Same Dates //////////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    
    ;For the same date...
    if nSame gt 0 then begin
        ;Start-time in seconds since midnight
        if nStart eq 1 $
            then start_seconds = hms_to_ssm(sTime) $
            else start_seconds = hms_to_ssm(sTime[iSame])
        
        ;End-time in seconds since midnight.
        end_seconds = hms_to_ssm(eTime[iSame])

        ;Subtract to get the total number of seconds elapsed
        seconds_between[iSame] = end_seconds - start_seconds
    endif

;-----------------------------------------------------------------------------------------
;Different Dates /////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    
    ;For different dates...
    if nDiff gt 0 then begin
    
        ;If sDateTime and eDateTime are not the same or adjacent days, then we need to
        ;count the number of whole days between them.
        if nStart eq 1 $
            then nWholeDays = days_between(replicate(_sDateTime, nDiff), eDateTime[iDiff]) $
            else nWholeDays = days_between(_sDateTime[iDiff], eDateTime[iDiff])
        nWholeDays -= nWholeDays ne 0
        nDaySec = nWholeDays * 86400D
        
        ;Number of seconds to reach the next day
        if nStart eq 1 $
            then start_seconds = 86400D - hms_to_ssm(sTime) $
            else start_seconds = 86400D - hms_to_ssm(sTime[iDiff])

        ;Seconds since midnight
        end_seconds = hms_to_ssm(eTime[iDiff])

        ;Subtract to get the total number of seconds elapsed
        seconds_between[iDiff] = start_seconds + nDaySec + end_seconds
    endif
        
    return, seconds_between
end


;-----------------------------------------------------------------------------------------
;Main Level Program (.r ssDateTime) //////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------

;Four seconds apart on the same day
sDateTime = '2013-01-01T01:01:00.000'
eDateTime = '2013-01-01T01:01:04.000'
sBetween = ssDateTime(sDateTime, eDateTime)

print, '---------------------------------------'
print, FORMAT='(%"Start Time: %s")', sDateTime
print, FORMAT='(%"End Time:   %s")', eDateTime
print, FORMAT='(%"Seconds:    %f")', sBetween
print, ''

;Eight seconds apart, but crossing midnight.
sDateTime = '2013-01-01T23:59:58.000'
eDateTime = '2013-01-02T00:00:06.000'
sBetween = ssDateTime(sDateTime, eDateTime)

print, '---------------------------------------'
print, FORMAT='(%"Start Time: %s")', sDateTime
print, FORMAT='(%"End Time:   %s")', eDateTime
print, FORMAT='(%"Seconds:    %f")', sBetween
print, ''

;One day and eight seconds apart, crossing two midnights.
sDateTime = '2013-01-01T23:59:58.000'
eDateTime = '2013-01-03T00:00:06.000'
sBetween = ssDateTime(sDateTime, eDateTime)

print, '---------------------------------------'
print, FORMAT='(%"Start Time: %s")', sDateTime
print, FORMAT='(%"End Time:   %s")', eDateTime
print, FORMAT='(%"Seconds:    %f")', sBetween
print, ''


;Two dates: 1 set on different days, 1 set on the same day.
sDateTime = ['2013-01-01T23:59:58.000', '2013-01-15T00:00:06.000']
eDateTime = ['2013-01-03T00:00:06.000', '2013-01-15T00:00:23.000']
sBetween = ssDateTime(sDateTime, eDateTime)

print, '---------------------------------------'
print, FORMAT='(%"Start Time: [%s, %s]")', sDateTime
print, FORMAT='(%"End Time:   [%s, %s]")', eDateTime
print, FORMAT='(%"Seconds:    [%f, %f]")', sBetween
print, ''

;Two dates: 1 set on different days, 1 set on the same day.
sDateTime = '2013-01-01T23:59:58.000'
eDateTime = ['2013-01-03T00:00:06.000', '2013-01-15T00:00:23.000']
stop
sBetween = ssDateTime(sDateTime, eDateTime)

print, '---------------------------------------'
print, FORMAT='(%"Start Time:  %s")', sDateTime
print, FORMAT='(%"End Time:   [%s, %s]")', eDateTime
print, FORMAT='(%"Seconds:    [%f, %f]")', sBetween

end