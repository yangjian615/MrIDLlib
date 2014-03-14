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
;   The purpose of this class is to provide a means of automatically finding dataa files
;   that match a given date and time pattern. The file system will be searched in the
;   following order::
;       1. If a directory was provided, it and its subdirectories will be searched first.
;       2. The current directory and all of its subdirectories.
;       3. All directories and subdirectories in the IDL !Path
;
;   LIST OF TOKENS::
;       %Y      -   Four-digit year: 2012, 2013, etc.
;       %y      -   Two-digit year: 60-59
;       %M      -   Two-digit month: 01-12
;       %C      -   Calendar month: January, Feburary, etc.
;       %c      -   Abbreviated calendar month: Jan, Feb, etc.
;       %d      -   Day of month: 01-31
;       %D      -   Day of year: 000-366
;       %W      -   Week day: Monday, Tuesday, etc.
;       %w      -   Abbreviated week day: Mon, Tue, etc.
;       %H      -   Hour on a 24-hour clock: 00-24
;       %h      -   Hour on a 12-hour clock: 01-12
;       %m      -   Minute: 00-59
;       %S      -   Seconds: 00-59
;       %A      -   A.M. on a 12-hour clock
;       %P      -   P.M. on a 12-hour clock
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
;	Modification History::
;       2013-10-21  -   Written by Matthew Argall.
;       2014-02-19  -   Added the FileSearch, ExtractTokens, Breakdown method. - MRA
;       2014-02-23  -   Renamed FileNames ot FileGen and added the TEST keyword. - MRA
;-
;*****************************************************************************************
;+
;   Extract date and time information from a file or directory name using the
;   token pattern.
;
;   Keywords return information about the [start, end] of an interval, as present
;   in the file or directory name. If a token is not found for the start or end of the
;   interval, the corresponding elements are the empty strings.
;
;   It is assumed that `NAMES` at most will contain date and time information regarding
;   when the start and end of the interval of the data contained within `NAME`. It is
;   further assumed that the date and time information will be ordered in descending order,
;   i.e. from date to month to year to hour, etc. If this is not the case, then anticipate
;   problems when deducing when the end of the data interval begins.
;
; :Params:
;       NAMES:              in, required, type=string/strarr
;                           A string (without tokens) that matches `PATTERN`.
;       PATTERN:            in, required, type=string
;                           Pattern containing tokens that identify different date and
;                               time elements within `NAMES`.
;
; :Keywords:
;       YEAR:               out, optional, type=strarr(2)
;                           4-digit year that matches a %Y token for the [start, end] of
;                               the file interval.
;       YR:                 out, optional, type=strarr(2)
;                           2-digit year that matches a %y token for the [start, end] of
;                               the file interval.
;       DOY:                out, optional, type=strarr(2)
;                           3-digit day-of-year that matches a %D token for the [start, end]
;                               of the file interval.
;       MONTH:              out, optional, type=strarr(2)
;                           2-digit month that matches a %M token for the [start, end] of
;                               the file interval.
;       CMONTH:             out, optional, type=strarr(2)
;                           Calendar month name (e.g. January, February, etc.) that
;                               matches a %C token for the [start, end] of the file
;                               interval.
;       CALMO:              out, optional, type=strarr(2)
;                           3-character abbreviated calendar month name (e.g. Jan, Feb, ...)
;                               that matches a %c token for the [start, end] of the file
;                               interval.
;       WEEKDAY:            out, optional, type=strarr(2)
;                           Weekday (e.g. Monday, Tuesday, etc.) that matches a %W token
;                               for the [start, end] of the file interval.
;       WKDAY:              out, optional, type=strarr(2)
;                           3-character abbreviated week day (e.g. Mon, Tue, etc.) that
;                               matches a %M token for the [start, end] of the file interval.
;       DAY:                out, optional, type=strarr(2)
;                           2-digit day that matches a %d token for the [start, end] of
;                               the file interval.
;       HOUR:               out, optional, type=strarr(2)
;                           2-digit hour on a 24-hour clock that matches a %H token for
;                               the [start, end] of the file interval.
;       HR:                 out, optional, type=strarr(2)
;                           2-digit hour on a 12-hour clock that matches a %h token for
;                               the [start, end] of the file interval.
;       MINUTE:              out, optional, type=strarr(2)
;                           2-digit minute that matches a %m token for the [start, end]
;                               of the file interval.
;       SECOND:             out, optional, type=strarr(2)
;                           2-digit second that matches a %S token for the [start, end] of
;                               the file interval.
;       AM_PM:              out, optional, type=strarr(2)
;                           "AM" or "PM" string that matches a %A token for the
;                               [start, end] of the file interval.
;-
pro MrFileFinder::Breakdown, names, pattern, $
YEAR=year, $
YR=yr, $
DOY=doy, $
MONTH=month, $
CMONTH=cMonth, $
CALMO=calmo, $
WEEKDAY=weekday, $
DAY=day, $
HOUR=hour, $
HR=hr, $
MINUTE=minute, $
SECOND=second, $
AM_PM=am_pm
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    nNames = n_elements(names)
;-----------------------------------------------------
;Tokens and File Parts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Break the filename into pieces. Throw away the overall match.
    regex_str = self -> Token2Regex(pattern)
    parts = stregex(names, regex_str, /SUBEXP, /EXTRACT, /FOLD_CASE)
    parts = parts[1:*,*]

    ;Get all of the tokens
    tokens = self -> ExtractTokens(pattern)

;-----------------------------------------------------
;Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    year    = strarr(2,nNames)
    yr      = strarr(2,nNames)
    doy     = strarr(2,nNames)
    month   = strarr(2,nNames)
    cmonth  = strarr(2,nNames)
    calmo   = strarr(2,nNames)
    weekday = strarr(2,nNames)
    wkday   = strarr(2,nNames)
    day     = strarr(2,nNames)
    hour    = strarr(2,nNames)
    hr      = strarr(2,nNames)
    minute  = strarr(2,nNames)
    second  = strarr(2,nNames)
    am_pm   = strarr(2,nNames)
    
;-----------------------------------------------------
;Match Tokens to File Parts \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    order = 0
    int = 0
    for i = 0, n_elements(tokens) - 1 do begin
        ;If the order gets smaller, then we jumped to the end of the time interval
        prev_order = order
        order = self -> TokenOrder(tokens[i])
        if order ne -1 && order lt prev_order then int += 1
        
        ;Extract the file name values
        case tokens[i] of
            'Y': year[int,*]    = parts[i,*]
            'y': yr[int,*]      = parts[i,*]
            'D': doy[int,*]     = parts[i,*]
            'M': month[int,*]   = parts[i,*]
            'C': cmonth[int,*]  = parts[i,*]
            'c': calMo[int,*]   = parts[i,*]
            'd': day[int,*]     = parts[i,*]
            'W': weekday[int,*] = parts[i,*]
            'w': wkday[int,*]   = parts[i,*]
            'H': hour[int,*]    = parts[i,*]
            'h': hr[int,*]      = parts[i,*]
            'm': minute[int,*]  = parts[i,*]
            'S': second[int,*]  = parts[i,*]
            'A': am_pm[int,*]   = parts[i,*]
            else: message, 'Token "' + tokens[i] + '" not recognized.', /INFORMATIONAL
        endcase
    endfor
end


;+
;   The purpose of this method is to extract all of the tokens from the file pattern.
;
; :Params:
;       PATTERN:        in, required, type=string
;                       Pattern containing tokens that identify different date and
;                           time elements within the string it represents.
;
; :Returns:
;       TOKENS:         A string array that contains all of the tokens used in the file
;                       name.
;-
function MrFileFinder::ExtractTokens, pattern
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    ;Count the number of tokens
    void = strsplit(pattern, '%', COUNT=nTokens)
    if strmid(pattern, 0, 1) ne '%' then nTokens -= 1
    
    ;Get all of the tokens
    tokens = stregex(pattern, strjoin(replicate('%([YyMCcdDWwHhmSA]).*', nTokens)), $
                     /SUBEXP, /EXTRACT)
    
    ;Do not return the overall match, only the tokens.
    return, tokens[1:*]
end


;+
;   Search the file system for files that match the file pattern.
;
;   Notes::
;       - `NAMES` must have a beginning year in it, otherwise an error will be thrown.
;       - If year, month, etc. information is not provided in the file name, then the
;         entire duration of the largest missing element will be assumed. As an example,
;         if `NAME` only has the year in it, `INTERVAL` will range from Jan 1 to Dec 31.
;         If the year and month are given, then the interval will range from the first
;         to the last of the month.
;
; :Params:
;       NAMES:          in, required, type=string/strarr
;                       A string (without tokens) that matches `PATTERN`.
;       PATTERN:        in, required, type=string
;                       Pattern containing tokens that identify different date and
;                           time elements within `NAMES`.
;
; :Returns:
;       INTERVAL:          Iso-1806 [start, end] times of the interval imbedded in `NAMES`.
;-
function MrFileFinder::ExtractInterval, names, pattern
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif

    nNames  = n_elements(names)
    sYear   = strarr(nNames)
    sMonth  = strarr(nNames)
    sDay    = strarr(nNames)
    sHour   = strarr(nNames)
    sMinute = strarr(nNames)
    sSecond = strarr(nNames)
    eYear   = strarr(nNames)
    eMonth  = strarr(nNames)
    eDay    = strarr(nNames)
    eHour   = strarr(nNames)
    eMinute = strarr(nNames)
    eSecond = strarr(nNames)
    noTime_flag = bytarr(6)     ;[year, month, day, hour, minute, second]

    ;Breakdown the file names
    self -> Breakdown, names, pattern, YEAR=year, YR=yr, DOY=doy, MONTH=month, $
                                       CMONTH=cMonth, CALMO=calMo, WEEKDAY=weekday, $
                                       DAY=day, HOUR=hour, HR=hr, MINUTE=minute, $
                                       SECOND=second, AM_PM=am_pm

;-----------------------------------------------------
;Start of the Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;4-Digit year?
    if year[0,0] ne '' then begin
        sYear = reform(year[0,*])
    ;2-Digit year?
    endif else if yr[0,0] ne '' then begin
        iTwenty = where(fix(year[0,*]) gt 59, nTwenty, COMPLEMENT=iNineteen, N_COMPLEMENT=nNineteen)
        if nTwenty   gt 0 then sYear[iTwenty]   = '20' + yr[0,*]
        if nNineteen gt 0 then sYear[iNineteen] = '19' + yr[0,*]
    endif else begin
        message, 'Start year not present. Uncertain how to proceed.'
        noTime_flag[0] = 1B
    endelse
    
    ;2-Digit month?
    if month[0,0] ne '' then begin
        sMonth = reform(month[0,*])
    ;Day-of-year?
    endif else if doy[0,0] ne '' then begin
        moday = year_day(doy[0,*], YEAR=sYear, /TO_MODAY)
        sMonth = strmid(moday, 0, 2)
        sDay = strmid(moday, 2, 2)
    ;Calendar month name?
    endif else if cMonth[0,0] ne '' then begin
        sMonth = MonthNameToNumber(cMonth[0,*], /STRING)
    ;Abbreviated calendar month name?
    endif else if calMo[0,0] ne '' then begin
        sMonth = MonthNameToNumber(calMo[0,*], /ABBR, /STRING)
    endif else begin
        sMonth[*] = '01'
        noTime_flag[1] = 1B
    endelse
    
    ;Day
    if day[0,0] ne '' then begin
        sDay[*] = reform(day[0,*])
    ;Year-Day -- Could have been extracted above.
    endif else if sDay[0] eq '' && doy[0,0] ne '' then begin
        moday = year_day(doy[0,*], YEAR=sYear, /TO_MODAY)
        sDay = strmid(moday, 2, 2)
    endif else begin
        sDay[*] = '01'
        noTime_flag[2] = 1B
    endelse
    
    ;24-hour clock
    if hour[0,0] ne '' then begin
        sHour = reform(hour[0,*])
    ;12-hour clock
    endif else if hr[0,0] ne '' then begin
        sHour = reform(hr[0,*])
        if am_pm[0,0] ne '' then begin
            iPM = where(strupcase(strmid(am_pm, 0, 1)) eq 'P', nPM)
            if nPM gt 0 then sHour[iPM] = string(12 + fix(sHour[iPM]), FORMAT='(i02)')
        endif
    endif else begin
        sHour[*] = '00'
        noTime_flag[3] = 1B
    endelse
    
    ;Minutes
    if minute[0,0] ne '' then begin
        sMinute[*] = reform(minute[0,*])
    endif else begin
        sMinute[*] = '00'
        noTime_flag[4] = 1B
    endelse
    
    ;Seconds
    if second[0,0] ne '' then begin
        sSecond[*] = reform(second[0,*])
    endif else begin
        sSecond[*] = '00.000'
        noTime_flag[5] = 1B
    endelse

;-----------------------------------------------------
;End of the Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;4-Digit year?
    if year[1,0] ne '' then begin
        eYear = reform(year[1,*])
    ;2-Digit year?
    endif else if yr[1,0] ne '' then begin
        iTwenty = where(fix(year[1,*]) gt 59, nTwenty, COMPLEMENT=iNineteen, N_COMPLEMENT=nNineteen)
        if nTwenty   gt 0 then eYear[iTwenty]   = '20' + yr[1,*]
        if nNineteen gt 0 then eYear[iNineteen] = '19' + yr[1,*]
    endif else begin
        eYear = sYear
    endelse
    
    ;2-Digit month?
    if month[1,0] ne '' then begin
        eMonth = reform(month[1,*])
    ;Day-of-year?
    endif else if doy[1,0] ne '' then begin
        moday = year_day(doy[1,*], YEAR=eYear, /TO_MODAY)
        eMonth = strmid(moday, 0, 2)
        eDay = strmid(moday, 2, 2)
    ;Calendar month name?
    endif else if cMonth[1,0] ne '' then begin
        eMonth = MonthNameToNumber(cMonth[1,*], /STRING)
    ;Abbreviated calendar month name?
    endif else if calMo[1,0] ne '' then begin
        eMonth = MonthNameToNumber(calMo[1,*], /ABBR, /STRING)
    endif else begin
        if noTime_flag[1] eq 0B $
            then eMonth = sMonth $
            else eMonth = '12'
    endelse
    
    ;Day
    if day[1,0] ne '' then begin
        eDay[*] = reform(day[1,*])
    ;Year-Day -- Could have been extracted above.
    endif else if eDay[0] eq '' && doy[1,0] ne '' then begin
        moday = year_day(doy[1,*], YEAR=eYear, /TO_MODAY)
        eDay = strmid(moday, 2, 2)
    endif else begin
        if noTime_flag[2] eq 0B then begin
            eDay = sDay
        endif else begin
            days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            eDay = days[fix(eMonth)-1]
        endelse
    endelse
    
    ;24-hour clock
    if hour[1,0] ne '' then begin
        eHour = reform(hour[1,*])
    ;12-hour clock
    endif else if hr[1,0] ne '' then begin
        eHour = reform(hr[1,*])
        if am_pm[1,0] ne '' then begin
            iPM = where(strupcase(strmid(am_pm, 0, 1)) eq 'P', nPM)
            if nPM gt 0 then sHour[iPM] = string(12 + fix(eHour[iPM]), FORMAT='(i02)')
        endif
    endif else begin
        if noTime_flag[3] eq 0B $
            then eHour = sHour $
            else eHour[*] = '23'
    endelse
    
    ;Minutes
    if minute[1,0] ne '' then begin
        eMinute[*] = reform(minute[1,*])
    endif else begin
        if noTime_flag[4] eq 0B $
            then eMinute = sMinute $
            else eMinute[*] = '59'
    endelse
    
    ;Seconds
    if second[1,0] ne '' then begin
        eSecond[*] = reform(second[1,*])
    endif else begin
        eSecond[*] = '59.999'
    endelse

;-----------------------------------------------------
;Form the Iso Time Interval \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    interval = [[sYear + '-' + sMonth + '-' + sDay + 'T' + sHour + ':' + sMinute + ':' + sSecond], $
                [eYear + '-' + eMonth + '-' + eDay + 'T' + eHour + ':' + eMinute + ':' + eSecond]]
    
    return, interval
end


;+
;   Search the file system for files that match the file pattern.
;
; :Returns:
;       FILES:          Fully qualified path to the files that match the file pattern.
;-
function MrFileFinder::FileSearch, dir_pattern, file_pattern, $
COUNT=count
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Replace all of the tokens with the wild card "*".
    file_str = strjoin(strsplit(file_pattern, '(%[YyMCcdDWwHhmSA])', /REGEX, /EXTRACT), '*')
    dir_str  = strjoin(strsplit(dir_pattern,  '(%[YyMCcdDWwHhmSA])', /REGEX, /EXTRACT), '*')

    ;Search first in the specified directory
    if dir_str ne '' $
        then files = file_search(dir_str, file_str, /FULLY_QUALIFY_PATH, COUNT=count)

    ;Next, search in the IDL path
    if count eq 0 then begin
        dir_spec = strsplit(!path, path_sep(/SEARCH_PATH), /EXTRACT)
        files = file_search(!path, file_str, /FULLY_QUALIFY_PATH, COUNT=count)
    endif
        
    ;If no files were found, return
    if count eq 0 then return, ''
    
    ;Use the regex pattern to look for matches
    regex_str = self -> Token2Regex()
    if regex_str eq '' then return, ''
    parts = stregex(files, regex_str, /EXTRACT, /SUBEXP)

    ;Weed out non-matches.
    iMatch = where(parts[0,*] ne '', count)
    if count eq 0 then begin
        message, 'No files found.', /INFORMATIONAL
        return, ''
    endif
    
    ;Return matches.
    return, files[iMatch]
end


;+
;   The purpose of this method is to generate an array of file names that contain date
;   and time information spanning the input data interval.
;
; :Keywords:
;       NFILES:             out, optional, type=integer
;                           Number of file names created.
;       EXCLUDE:            in, optional, type=boolean, default=0
;                           If set, then files that do not exist on the file system will
;                               be excluded from the results. Setting this keywords auto-
;                               matically sets `TEST`=1.
;       TEST:               in, optional, type=boolean, default=0
;                           If set, each file generated will be tested to see if it exists
;                               in the file system.
;
; :Returns:
;       FILENAMES:          The file names containing the data within the data interval.
;-
function MrFileFinder::FileGen, $
EXCLUDE=exclude, $
NFILES = nFiles, $
TEST=test
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    ;Defaults
    test = keyword_set(test)
    exclude = keyword_set(exclude)
    if exclude then test = 1
    
    ;Generate a range of dates between the start and end of the data interval.
    dateRange = dateTimeGen(self.iso_start, self.iso_end)
    nFiles = n_elements(dateRange[0,*])
    filenames = strarr(nFiles)

    ;Loop through each start-end date.
    count = 0
    iExist = intarr(nFiles)
    for i = 0, nFiles - 1 do begin
        ;Replace the tokens in the directory and the file pattern
        dir = self  -> Token2Time(self.dir_pattern, dateRange[0,i], dateRange[1,i])
        file = self -> Token2Time(self.file_pattern, dateRange[0,i], dateRange[1,i])

        ;Store the file name
        filenames[i] = dir + file
        
        ;Test the files?
        if test then begin
            if file_test(dir+file) eq 0 then begin
                filenames[i] = file_search(dir + file)
                iExist[count] = i
                count += 1
            endif else begin
                message, 'File not found: "' + dir + file + '".', /INFORMATIONAL
            endelse
        endif
    endfor
    
    ;Exclude files that do not exist.
    if exclude eq 1 then begin
        if count gt 0 $
            then filenames = filenames[iExist[0:count-1]] $
            else filenames = ''
    endif

    return, filenames
end


;+
;   The purpose of this method is to return the names of the files that match the
;   file pattern provided. If a time interval was also privided, only those files
;   whose beginning intervals lie within the indicated time interval will be returned.
;
; :Keywords:
;       NFILES:         out, optional, type=int
;                       Number of files found.
;       UNIQUE:         in, optional, type=boolean, default=0
;                       If set, only files with unique basenames will be returned.
;
; :Returns:
;       FILES:          The filenames that lie within the specified time interval. If
;                           no interval is indicated, all files names are returned. If
;                           the search did not find any files, !Null is returned.
;-
function MrFileFinder::GetFiles, $
NFILES=nFiles, $
UNIQUE=unique
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        nFiles = 0
        return, !Null
    endif
    
    unique = keyword_set(unique)
    
    ;Make sure files have been found
    if self.nFiles eq 0 then begin
        nFiles = 0
        message, 'No files found: "' + self.dir_pattern + self.file_pattern + '"', /INFORMATIONAL
        return, ''
    endif
    
    ;Return all files?
    if self.iso_start eq '' then begin
        files = *self.file_names
    endif else begin
        ;Get the time intervals over which the files span
        intervals = self -> ExtractInterval(*self.file_names, self.file_pattern)
        if intervals eq !Null then begin
            nFiles = 0
            return, !Null
        endif

        ;Find the files that start within the time interval provided
        iFiles = where( (intervals[*,0] gt self.iso_start  and $
                         intervals[*,0] lt self.iso_end)   or  $
                        (intervals[*,1] lt self.iso_end    and $
                         intervals[*,1] gt self.iso_start) or  $
                        (intervals[*,0] lt self.iso_start  and $
                         intervals[*,1] gt self.iso_end), nFiles)

        ;No files in the given interval?
        if nFiles eq 0 then begin
            message, 'No files found within the interval. Returning all files.', /INFORMATIONAL
            files = *self.file_names
            nFiles = n_elements(files)
        endif else begin
            files = (*self.file_names)[iFiles]
            nFiles = n_elements(files)
        endelse
    endelse
    
    ;Return only unique file names?
    if unique eq 1 && nFiles gt 1 then begin
        filebase = file_basename(files)
        files = files[MrUniq(filebase, /SORT, NUNIQ=nFiles)]
    endif
    
    return, files
end


;+
;   Get class properties.
;
; :Keywords:
;       PATTERN:            out, optional, type=string
;                           A complete file path of a whose date and time information
;                               have been replaced by the tokens defined above.
;       ISO_START:          out, optional, type=string
;                           A date-time string containing information that will replace
;                               tokens found in `PATTERN`.
;       ISO_END:            out, optional, type=string
;                           A date-time string containing information that will replace
;                               tokens found in `PATTERN`.
;-
pro MrFileFinder::GetProperty, $
PATTERN=pattern, $
ISO_START=iso_start, $
ISO_END=iso_end
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get properties of the class.
    if arg_present(pattern)   then pattern   = self.dir_pattern + self.file_pattern
    if arg_present(iso_start) then iso_start = self.iso_start
    if arg_present(iso_end)   then iso_end   = self.iso_end
end


;+
;   Set class properties.
;
; :Keywords:
;       PATTERN:            in, optional, type=string
;                           A complete file path of a whose date and time information
;                               have been replaced by the tokens defined above.
;       ISO_START:          in, optional, type=string
;                           A date-time string containing information that will replace
;                               tokens found in `FILE_PATTERN`.
;       ISO_END:            in, optional, type=string
;                           A date-time string containing information that will replace
;                               tokens found in `FILE_PATTERN`.
;-
pro MrFileFinder::SetProperty, $
PATTERN=pattern, $
ISO_START=iso_start, $
ISO_END=iso_end
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Set properties of the class.
    if n_elements(pattern) gt 0 then begin
        file_pattern = cgRootName(pattern, DIRECTORY=dir_pattern, EXTENSION=ext)
        self.file_pattern = file_pattern + '.' + ext
        self.dir_pattern = dir_pattern

        ;Search for matches.
        *self.file_names = self -> FileSearch(dir_pattern, self.file_pattern, COUNT=nFiles)
        self.nFiles = nFiles
    endif
    
    if n_elements(iso_start) gt 0 then self.iso_start = iso_start
    if n_elements(iso_end) gt 0 then self.iso_end = iso_end
end


;+
;   The purpose of this method is to replace the tokens found in the input file pattern
;   with date and times defined in the data interval.
;
; :Params:
;       PATTERN:            in, required, type=string
;                           Pattern containing tokens that identify different date and
;                               time elements within the string it represents.
;       ISO_START:          in, required, type=string
;                           A iso-time string containing information that will replace
;                               tokens found in `PATTERN`.
;       ISO_END:            in, required, type=string
;                           A iso-time string containing information that will replace
;                               tokens found in `PATTERN`.
;
; :Returns:
;       REPLACESTRING:      Results of replacing the tokens with date and time elements.
;-
function MrFileFinder::Token2Time, pattern, iso_start, iso_end
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Breakdown the time interval
    dissectDateTime, [iso_start, iso_end], sYear, sMonth, sDay, sHour, sMinute, sSecond

    ;Create a copy of the file pattern
    replaceString = pattern

    ;Find all of the tokens in the file pattern
    tf_more_tokens = 1
    while tf_more_tokens eq 1 do begin
        
        ;Get the current length of the string
        rsLen = strlen(replaceString)
    
        ;Get the position and length of the token
        token = stregex(replaceString, '(%)([YyMCcdDWwHhmSAP])', /EXTRACT, /SUBEXP)
        pos = stregex(replaceString, '%[YyMCcdDWwHhmSAP]', LEN=len, /SUBEXP)
        if token[0] eq '' then tf_more_tokens = 0
        
        ;Get the parts of the string surrounding the token
        before_token = strmid(replaceString, 0, pos)
        after_token = strmid(replaceString, pos+len, rsLen-pos-len)

        ;Replace the token with the date and time information
        case token[2] of
            'Y': replaceString = before_token + sYear + after_token
            'y': if fix(text) gt 60 $
                    then replaceString = before_token + '19' + sYear + after_token $
                    else replaceString = before_token + '20' + sYear + after_token
            'M': replaceString = before_token + sMonth + after_token
            'C': 
            'c': 
            'd': replaceString = before_token + sDay + after_token
            'D': 
            'W': 
            'w': 
            'H': replaceString = before_token + sHour + after_token
            'h': 
            'm': replaceString = before_token + sMinute + after_token
            'S': replaceString = before_token + sSecond + after_token
            'A': 
            '':  ;No token was found.
            else: message, 'Delimiter "' + delimiter + '" not recognized.'
        endcase
    endwhile
    
    return, replaceString
end


;+
;   Replace tokens with regex expressions that can be used to pull information out of
;   the file names.
;
; :Params:
;       PATTERN:        in, required, type=string
;                       Pattern containing tokens that identify different date and
;                           time elements within the string it represents.
;
; :Returns:
;       REGEX_STR:      A string that can be passed to the STREGEX function to extract
;                           time elements from the string that `PATTERN` represents.
;                           It may be necessary to set the /FOLD_CASE keyword in STREGEX.
;-
function MrFileFinder::Token2Regex, pattern
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    regex_str = n_elements(pattern) eq 0 ? self.file_pattern : pattern

    ;Replace "." with "\."
    ;Replace "*" with ".*"
    regex_str = strjoin(strsplit(regex_str, '.', /EXTRACT), '\.')
    regex_str = strjoin(strsplit(regex_str, '*', /EXTRACT), '.*')

    ;Find all of the tokens in the file pattern
    tf_more_tokens = 1B
    while tf_more_tokens eq 1B do begin
        ;Get the current length of the string
        rsLen = strlen(regex_str)
    
        ;Get the position and length of the token
        token = stregex(regex_str, '%([YyMCcdDWwHhmSAP])', /EXTRACT, /SUBEXP)
        pos = stregex(regex_str, '%[YyMCcdDWwHhmSAP]', LEN=len, /SUBEXP)
        if token[0] eq '' then tf_more_tokens = 0B

        ;Get the parts of the string surrounding the token
        before_token = strmid(regex_str, 0, pos)
        after_token = strmid(regex_str, pos+len, rsLen-pos-len)

        ;Replace the token with the date and time information
        case token[1] of
            'Y': regex_str = before_token + '([0-9]{4})' + after_token
            'y': regex_str = before_token + '([0-9]{2})' + after_token
            'M': regex_str = before_token + '([0-9]{2})' + after_token
            'C': regex_str = before_token + '(January|February|March|April|May|June|' + $
                                            'July|August|September|October|November|December)' + after_token
            'c': regex_str = before_token + '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)' + after_token
            'd': regex_str = before_token + '([0-9]{2})' + after_token
            'D': regex_str = before_token + '([0-9]{3})' + after_token
            'W': regex_str = before_token + '(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)' + after_token
            'w': regex_str = before_token + '(Sun|Mon|Tue|Wed|Thu|Fri|Sat)' + after_token
            'H': regex_str = before_token + '([0-9]{2})' + after_token
            'h': regex_str = before_token + '([0-9]{2})' + after_token
            'm': regex_str = before_token + '([0-9]{2})' + after_token
            'S': regex_str = before_token + '([0-9]{2})' + after_token
            'A': regex_str = before_token + '(AM|PM)'    + after_token
            else: if tf_more_tokens eq 1B then message, 'Token "' + token[1] + '" not recognized.'
        endcase
    endwhile
        
    return, regex_str
end


;+
;   The purpose of this method is to retreive the order of the embedded date and time
;   codes.
;
; :Private
;
; :Params:
;       TOKEN:          in, required, type=string
;                       A character referencing a portion of a date or time.
;
; :Returns:
;       ORDER:          The order of the date/time code.
;-
function MrFileFinder::TokenOrder, token
    
    case token of
        'y': order = 0
        'Y': order = 0
        'M': order = 1
        'C': order = 1
        'c': order = 1
        'D': order = 1
        'd': order = 2
        'W': order = 2
        'w': order = 2
        'H': order = 3
        'h': order = 3
        'm': order = 4
        'S': order = 5
        'A': order = 6
        else: order = -1
    endcase
    
    return, order
end


;+
;   Clean up after the object is destroy
;-
pro MrFileFinder::cleanup
    ptr_free, self.file_names
end


;+
;   The initialization method. Here, a directory and filename can be given with a date
;   and time code embedded.
;
;   TOKEN LIST::
;       %Y      -   Four-digit year: 2012, 2013, etc.
;       %y      -   Two-digit year: 60-59
;       %M      -   Two-digit month: 01-12
;       %C      -   Calendar month: January, Feburary, etc.
;       %c      -   Abbreviated calendar month: Jan, Feb, etc.
;       %d      -   Day of month: 01-31
;       %D      -   Day of year: 000-366
;       %W      -   Week day: Monday, Tuesday, etc.
;       %w      -   Abbreviated week day: Mon, Tue, etc.
;       %H      -   Hour on a 24-hour clock: 00-24
;       %h      -   Hour on a 12-hour clock: 01-12
;       %m      -   Minute: 00-59
;       %S      -   Seconds: 00-59
;       %A      -   A.M. on a 12-hour clock
;       %P      -   P.M. on a 12-hour clock
;
; :Params:
;       FILE_PATTERN:       in, required, type=string
;                           A complete file path of a file whose date and time substrings
;                               have been replaced by the tokens defined above.
;       ISO_START:          in, required, type=string
;                           A iso-time string containing information that will replace
;                               tokens found in `FILE_PATTERN`.
;       ISO_END:            in, required, type=string
;                           A iso-time string containing information that will replace
;                               tokens found in `FILE_PATTERN`.
;
;-
function MrFileFinder::init, pattern, $
ISO_START = iso_start, $
ISO_END = iso_end
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Allocate heap
    self.file_names = ptr_new(/ALLOCATE_HEAP)
    
    ;Set the filename for known filetypes
    self -> SetProperty, PATTERN=pattern, $
                         ISO_START=iso_start, $
                         ISO_END=iso_end
    
    return, 1
end


;+
;   The class definition statement.
;
; :Fields:
;       DIR_PATTERN:        The pattern associated with the data directory.
;       ISO_START:          ISO-1086 time at which to begin reading data.
;       ISO_END:            ISO-1086 time at which to stop reading data.
;       FILE_PATTERN:       Pattern associated with data file names.
;       ITERATE:            Indicates that the filename can be iterated. If not, searched.
;-
pro MrFileFinder__define
    compile_opt strictarr
    
    class = { MrFileFinder, $
              inherits IDL_Object, $
              dir_pattern:  '', $
              file_pattern: '', $
              file_names:   ptr_new(), $
              iso_start:    '', $
              iso_end:      '', $
              nFiles:       0, $
              iterate:      0B  $
            }
end