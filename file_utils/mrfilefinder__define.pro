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
;       2014-03-21  -   Added the CD, PWD, SetPWD, GetPWD, UpOneDir, AppendPath,
;                           ResolvePath, and FollowDirTree methods. - MRA
;       2014-05-08  -   Examine only the files' basenames when extracting intervals. - MRA
;       2014-05-09  -   String with no tokens caused error. Fixed. - MRA
;       2014-05-12  -   When filter failed, returned all files. Now, return only the files
;                           that pass the filtering process. - MRA
;       2014-06-10  -   Added the KnownTokens method. Check for only those tokens when
;                           determining whether a file name contains time information. - MRA
;       2014-07-01  -   Removed the ExtractTokens, KnownTokens, Token2Regex, Token2Time,
;                           FileSearch and FileGen methods. Make use of MrTokens_* routines. - MRA
;-
;*****************************************************************************************
;+
;   Append a path to a root directory.
;
; :Params:
;       PATH:       in, required, type=string/strarr
;                   Paths to be appended to `ROOT`.
;       ROOT:       in, optional, type=string, default=pwd
;                   Root directory to which PATH is relative.
;
; :Returns:
;       RESULT:     `ROOT` + `PATH_SEPARATOR` + `PATH`. Care is taken to ensure only
;                       one path separator exists between `ROOT` and `PATH`.
;-
function MrFileFinder::AppendPath, path, root
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        return, ''
    endif
    
    ;Output
    pathOut = path
    
    ;Default to the directory.
    if n_elements(root) eq 0 then root = self -> GetPWD()
    sep_path = self.path_sep
    
    ;Ensure the root has a single trailing "/"
    ; - (.*[^/])[/]*$ = any string of characters whose penultimate character is not
    ;                   "/", but whose last character is "/".
    tailSlash = stregex(root, '(.*[^' + sep_path + '])[' + sep_path + ']*$', /SUBEXP, /EXTRACT)
    rootOut = tailSlash[1] + sep_path
    
    ;Remove leading "/" from path
    ; - ^[/]*(.*) = Look for "/" characters at the beginning of the string and extract
    ;               everything after that. 
    leadSlash = stregex(path, '^[' + sep_path + ']*(.*)', /SUBEXP, /EXTRACT)
    pathOut = reform(leadSlash[1,*])
    
    ;Remove trailing "/" from path (same as for ROOT)
    tailSlash = stregex(pathOut, '(.*[^' + sep_path + '])[' + sep_path + ']*$', /SUBEXP, /EXTRACT)
    pathOut = reform(tailSlash[1,*])

    result = rootOut + pathOut
    if n_elements(result) eq 1 then result = result[0]
    return, result
end


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
    regex_str = MrTokens_ToRegex(pattern, /IGNORE_PARENS, COUNT=nTokens)
    parts = stregex(names, regex_str, /SUBEXP, /EXTRACT, /FOLD_CASE)
    parts = parts[1:*,*]

    ;Get all of the tokens
    tokens = MrTokens_Extract(pattern, /IGNORE_PARENS)

;-----------------------------------------------------
;Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    count   = 0
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
    ;More tokens are available than those accepted here (see MrTokens.pro).
    ;   Tokens that do not contain part of a date will fall through without a match.
    ;   The number of tokens is not necessarily equal to the number of parts extracted
    order = 0
    int = 0
    for i = 0, nTokens - 1 do begin
        ;Create an array of counters, one counter per token
        counter = intarr(14)

        ;If the order gets smaller, then we jumped from the start time to the end time.
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
            else: ;Do nothing
        endcase
    endfor
end


;+
;   Change directories. Leading ".." and "./" characters are honored. If
;   no leading path separator is present, `DESTINATION` will be taken with respect
;   to the current directory. The "~" character is not honored.
;
; :Params:
;       DESTINATION:    in, optional, type=string
;                       Directory to be made the present working directory.
;-
pro MrFileFinder::CD, destination, $
PATH_SEPARATOR=path_separator
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        ;Return to the initial directory
        self -> SetPWD, pwd
        return
    endif

    ;Get the current directory
    pwd = self -> GetPWD()
    sep_path = n_elements(path_separator) eq 0 ? self.path_sep : path_separator

    ;Check for ".."
    if stregex(destination, '^\.\.[' + sep_path + ']?.*', /BOOLEAN) then begin
        self -> UpOneDir
        destOut = stregex(destination, '^\.\.[' + sep_path + ']?(.*)', /SUBEXP, /EXTRACT)
        destOut = destOut[1]
        self -> CD, destOut
        return
    
    ;Check for './'
    endif else if stregex(destination, '^.[' + sep_path + ']', /BOOLEAN) then begin
        destOut = self -> AppendPath(destination)
        
    ;Check if no leading "/" is present
    endif else if stregex(destination, '^[^' + sep_path + ']', /BOOLEAN) then begin
        destOut = self -> AppendPath(destination)
        
    ;If a leading "/" is present, change directories
    endif else if stregex(destination, '^[' + sep_path + ']', /BOOLEAN) then begin
        destOut = destination
    endif else begin
        destOut = destination
    endelse

    ;Change directories    
    if destOut ne '' then self -> SetPWD, destOut
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
        sSecond[*] = reform(second[0,*]) + 'Z'
    endif else begin
        sSecond[*] = '00Z'
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
            else eHour[*] = '24'
    endelse
    
    ;Minutes
    if minute[1,0] ne '' then begin
        eMinute[*] = reform(minute[1,*])
    endif else begin
        if noTime_flag[4] eq 0B $
            then eMinute = sMinute $
            else eMinute[*] = '00'
    endelse
    
    ;Seconds
    if second[1,0] ne '' $
        then eSecond[*] = reform(second[1,*]) + 'Z' $
        else eSecond[*] = '00Z'

;-----------------------------------------------------
;Form the Iso Time Interval \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    interval = [[sYear + '-' + sMonth + '-' + sDay + 'T' + sHour + ':' + sMinute + ':' + sSecond], $
                [eYear + '-' + eMonth + '-' + eDay + 'T' + eHour + ':' + eMinute + ':' + eSecond]]
    
    return, interval
end


;+
;   Search the file system for files that match the file pattern. This method is
;   faster than the FileSearch method. However, it does not search beyond the scope
;   of the pattern given.
;
;   First, parse the directory pattern, following only those directories that match the
;   pattern. At the end of the directory chain, it parses the file names and selects
;   files that match the file pattern.
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
;-
function MrFileFinder::FindFile, path_str, $
COUNT=count
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(pwd) gt 0 then self -> CD, pwd
        void = cgErrorMsg()
        return, ''
    endif

    if n_elements(path_str) eq 0 $
        then path_str = self -> AppendPath(self.filebase, self.directory)

    ;Current directory
    pwd = self -> GetPWD()

;---------------------------------------------------------------------
; Find First Token ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Split the directroy
    fullPath = self -> ResolvePath(path_str)
    path_parts = strsplit(fullPath, self.path_sep, COUNT=nParts, LENGTH=len, /EXTRACT)
    
    ;Find the largest path not containing a token
    iNoTokens = max(where(strpos(path_parts, '%') eq -1))
    root = strmid(fullPath, 0, total(len[0:iNoTokens])+iNoTokens+2)

    ;Get the first piece with tokens. Convert it to a regular expression.
    if iNoTokens ge nParts-1 then begin
        root = file_dirname(fullPath)
        subPattern = file_basename(fullPath)
    endif else begin
        subPattern = path_parts[iNoTokens+1]
        subPattern = MrTokens_ToRegex(subPattern)
    endelse

;---------------------------------------------------------------------
; Search for Match ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Change directories to ROOT and search for PATTERN
    self -> CD, root
    self -> LS, subPattern, /REGEX, COUNT=count, OUTPUT=dirOut
    if count eq 0 then begin
        self -> CD, pwd
        return, ''
    endif

    ;Remainder of the directory to be parsed
    if iNoTokens+2 le nParts-1 $
        then remainder = strjoin(path_parts[iNoTokens+2:nParts-1], self.path_sep) $
        else remainder = ''

;---------------------------------------------------------------------
; Parse Next Piece ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if remainder ne '' then begin
        ;Step through each match
        tf_tree = 0B
        for i = 0, count - 1 do begin
            ;Substitute the match for its regular expression and search again
            ;   root + regex + remainder ---> root + match + remainder
            nextDir = self -> AppendPath(dirOut[i], root)
            nextDir = self -> AppendPath(remainder, nextDir)
            temp_tree = self -> FindFile(nextDir, COUNT=nFound)
            
            ;Gather all of the complete file paths.
            if nFound gt 0 then begin
                if tf_tree eq 0 then begin
                    tree = temp_tree
                    tf_tree = 1B
                endif else tree = [tree, temp_tree]
            endif
        endfor

;---------------------------------------------------------------------
; Last Element ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;Form the complete file path
        ;   root + regex ----> root + match
        tree = self -> AppendPath(dirOut, root)
    endelse
    
    ;Return
    self -> SetPWD, pwd
    count = n_elements(tree)
    if count eq 0 then tree = ''
    return, tree
end


;+
;   Return the present working directory WITH a path separator.
;-
function MrFileFinder::GetPWD
    on_error, 2
    
    ;Get the directory
    cd, CURRENT=pwd
    return, pwd + self.path_sep
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
    nFiles = self.nFiles
    
    ;Make sure files have been found
    if nFiles eq 0 then return, ''

    ;Return all files?
    ;   If no time was give     (i.e. cannot filter results)
    ;   If no tokens were given (i.e. cannot find time intervals)
    allTokens = self -> GetTokens()
    regex = '%[' + strjoin(allTokens) + ']'
    if self.sTime eq '' || stregex(self.filebase, regex, /BOOLEAN) eq 0 then begin
        nFiles = self.nFiles
        files = *self.file_names
        if nFiles eq 1 then files = files[0]
        
    endif else begin
        ;Get the time intervals over which the files span
        intervals = self -> ExtractInterval(file_basename(*self.file_names), self.filebase)
        if intervals eq !Null then begin
            nFiles = 0
            return, !Null
        endif

        ;Find the files that start within the time interval provided
        iFiles = where( (intervals[*,0] ge self.sTime  and $    ; iStart <= fStart && fStart <= iEnd
                         intervals[*,0] le self.eTime) or  $    ;   - Start time lies within interval
                        (intervals[*,1] le self.eTime  and $    ;   iEnd <= fEnd   &&   iEnd >= iStart
                         intervals[*,1] ge self.sTime) or  $    ;   - End time lies within interval
                        (intervals[*,0] le self.sTime  and $    ; iStart >= fStart &&   iEnd <= fEnd
                         intervals[*,1] ge self.eTime), nFiles) ;   - Interval lies within start and end time

        ;No files in the given interval?
        if nFiles eq 0 then return, ''

        ;Select the files.
        files = (*self.file_names)[iFiles]
        nFiles = n_elements(files)
    endelse
    
    ;Return only unique file names?
    if unique eq 1 && nFiles gt 1 then begin
        filebase = file_basename(files)
        files = files[MrUniq(filebase, /SORT, NUNIQ=nFiles)]
    endif
    
    if nFiles eq 1 then files = files[0]
    return, files
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
COUNT=count, $
ETIME=eTime, $
FILENAME=filename, $
PATH_SEPARATOR=path_separator, $
STIME=sTime, $
TPATTERN=tPattern
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get properties of the class.
    if arg_present(count)          then count          = self.nFiles
    if arg_present(filename)       then filename       = filepath(self.filebase, ROOT_DIR=self.directory)
    if arg_present(sTime)          then sTime          = self.sTime
    if arg_present(tPattern)       then tPattern       = self.tPattern
    if arg_present(eTime)          then eTime          = self.eTime
    if arg_present(path_separator) then path_separator = self.path_sep
    
    ;Convert back to the input format
    if arg_present(sTime) then begin
        if self.tPattern ne self.time_pattern $
            then MrTimeParser, self.sTime, self.time_pattern, self.tPattern, sTime $
            else sTime = self.sTime
    endif
    
    ;Convert back to the input format
    if arg_present(eTime) then begin
        if self.tPattern ne self.time_pattern $
            then MrTimeParser, self.eTime, self.time_pattern, self.tPattern, eTime $
            else eTime = self.eTime
    endif
end


;+
;   Return an array of tokens recognized by the MrFileFinder (a subset of those
;   recognized by MrTimeParser.pro).
;
; :Returns:
;       TOKENS:         Array of recognized tokens.
;-
function MrFileFinder::GetTokens
    return, ['Y', 'y', 'D', 'M', 'C', 'c', 'd', 'W', 'w', 'H', 'h', 'm', 'S', 'A']
end


;+
;   Print the contents of a directory.
;
; :Params:
;       PATTERN:        in, optional, type=string, default='*'
;                       Pattern to match file names against. `PATTERN` is only checked
;                           against files in the current directory.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files found.
;       OUTPUT:         out, optional, type=string/strarr
;                       Variable into which the names of the files found will be returned.
;                           If present, printing is suppressed.
;       STREGEX:        in, optional, type=boolean, default=0
;                       If set, `PATTERN` is a regular expression pattern. IDL's
;                           StRegEx() function will be used with /BOOLEAN set.
;-
pro MrFileFinder::LS, pattern, directory, $
COUNT=nFiles, $
REGEX=regex, $
OUTPUT=output
    on_error, 2

    ;Default
    regex = keyword_set(regex)

    ;Get all of the files.
    files = file_search('*', COUNT=nFiles)
    
    ;Was a pattern given?
    if n_elements(pattern) gt 0 then begin
        if regex $
            then iMatch = where(stregex(files, pattern, /BOOLEAN), nFiles) $
            else iMatch = where(strmatch(files, pattern), nFiles)
    
        ;No matches?
        if nFiles eq 0 then begin
            if arg_present(output) then output = '' else print, ''
            return
        endif
        
        files = files[iMatch]
    endif
    
    ;Output or print?
    if arg_present(output) $
        then output = files $
        else print, transpose(files)
end


;+
;   Print the present working directory.
;-
pro MrFileFinder::PWD
    print, self -> GetPWD()
end


;+
;   Fully qualify paths. If a path begins with the path separator, it remains unchanged.
;   Otherwise, the present working directory is appended to the beginning of the path.
;
; :Params:
;       PATH:       in, required, type=string/strarr
;                   Paths to be resolved.
;
; :Returns:
;       PATHOUT:    Absolute paths of `PATH`.
;-
function MrFileFinder::ResolvePath, path
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        return, ''
    endif
    
    pathOut = path
    
    ;Search for absence of leading "/"
    jResovle = where(stregex(path, '^[^' + self.path_sep + ']', /BOOLEAN), nResolve)
    
    ;Resolve relative paths.
    if nResolve gt 0 then pathOut[jResolve] = self -> AppendPath(path[jResolve])
    
    return, pathOut
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
ETIME=eTime, $
FILENAME=filename, $
PATH_SEPARATOR=path_separator, $
STIME=sTime, $
TPATTERN=tPattern
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    if n_elements(tPattern)       gt 0 then self.tPattern = tPattern
    if n_elements(path_separator) gt 0 then self.path_sep  = path_separator
    
    ;Convert the start time to the fixed format
    if n_elements(sTime) gt 0 then begin
        if self.tPattern ne self.time_pattern $
            then MrTimeParser, sTime, self.tPattern, self.time_pattern, _sTime $
            else _sTime = sTime
        self.sTime = _sTime
    endif
    
    ;Convert the end time to the fixed format
    if n_elements(eTime) gt 0 then begin
        if self.tPattern ne self.time_pattern $
            then MrTimeParser, eTime, self.tPattern, self.time_pattern, _eTime $
            else _eTime = eTime
        self.eTime = _eTime
    endif
    
    ;If the filename changed, search for matching files automatically.
    if n_elements(filename) gt 0 then begin
        self.filebase  = file_basename(filename)
        self.directory = file_dirname(filename, /MARK_DIRECTORY)

        ;Search for matches.
        *self.file_names = self -> FindFile(filename, COUNT=nFiles)
        self.nFiles = nFiles
    endif
end


;+
;   A utility method for setting the present working directory. To change directories,
;   use the CD method.
;
; :Params:
;       DESTINATION:    in, optional, type=string
;                       Directory to be made the present working directory.
;-
pro MrFileFinder::SetPWD, destination
    on_error, 2
    
    ;Set the current directory
    cd, destination
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
;   Set the pesent working directory.
;
; :Params:
;       DESTINATION:    in, optional, type=string
;                       Directory to be made the present working directory.
;-
pro MrFileFinder::UpOneDir
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        ;Return to the initial directory
        self -> SetPWD, pwd
        return
    endif

    ;Get the current directory tree
    pwd = self -> GetPWD()
    pwd_parts = strsplit(pwd, self.path_sep, /EXTRACT, COUNT=nParts)
    
    ;If we are not at the root already, then move up one directory.
    if nParts gt 1 then begin
        dirOut = strjoin(pwd[0:nParts-1], self.path_sep)
        self -> SetPWD, dirOut
    endif
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
function MrFileFinder::init, filename, $
ETIME=eTime, $
PATH_SEPARATOR=path_separator, $
STIME=sTime, $
TPATTERN=tPattern
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Defaults
    if n_elements(path_separator) eq 0 then path_separator = path_sep()
    if n_elements(tPattern) eq 0 then tPattern = '%Y-%M-%dT%H:%m:%S%z'
    
    ;Allocate heap
    self.file_names = ptr_new(/ALLOCATE_HEAP)
    
    self.time_pattern = '%Y-%M-%dT%H:%m:%S%z'
    
    ;Set the filename for known filetypes
    self -> SetProperty, ETIME=eTime, $
                         FILENAME=filename, $
                         PATH_SEPARATOR=path_separator, $
                         STIME=sTime, $
                         TPATTERN=tPattern
    
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
              directory:    '', $
              filebase:     '', $
              file_names:   ptr_new(), $
              path_sep:     '', $
              sTime:        '', $
              eTime:        '', $
              nFiles:       0, $
              tPattern:     '', $
              time_pattern: '' $
            }
end