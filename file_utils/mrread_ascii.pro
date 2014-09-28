; docformat = 'rst'
;
;+
;   The purpose of this program is to combine the functionality of READ_ASCII and
;   ASCII_TEMPLATE.
;
; :Examples:
;    Try the main-level example program at the end of this file::
;       IDL> .run MrRead_Ascii
;
; :Params:
;    FILENAME:          in, required, type=string
;                       Filename of file to read. If no file is given or the empty string,
;                           a dialog box will appear asking to pick a file. Furthermore,
;                           if `TEMPLATE`, `COLUMN_NAMES`, `COLUMN_TYPES` and `GROUP` are
;                           all undefined, Ascii_Template() will be called to assist in
;                           reading the file.
;    GROUP0:            out, optional, type=any
;                       A named variable that will contain data from the 1st `GROUP`.
;    GROUP1:            out, optional, type=any
;                       A named variable that will contain data from the 2nd `GROUP`.
;    GROUP2:            out, optional, type=any
;                       A named variable that will contain data from the 3rd `GROUP`.
;    GROUP3:            out, optional, type=any
;                       A named variable that will contain data from the 4th `GROUP`.
;    GROUP4:            out, optional, type=any
;                       A named variable that will contain data from the 5th `GROUP`.
;    GROUP5:            out, optional, type=any
;                       A named variable that will contain data from the 6th `GROUP`.
;    GROUP6:            out, optional, type=any
;                       A named variable that will contain data from the 7th `GROUP`.
;    GROUP7:            out, optional, type=any
;                       A named variable that will contain data from the 8th `GROUP`.
;    GROUP8:            out, optional, type=any
;                       A named variable that will contain data from the 9th `GROUP`.
;    GROUP9:            out, optional, type=any
;                       A named variable that will contain data from the 10th `GROUP`.
;    GROUP10:           out, optional, type=any
;                       A named variable that will contain data from the 11th `GROUP`.
;    GROUP11:           out, optional, type=any
;                       A named variable that will contain data from the 12th `GROUP`.
;    GROUP12:           out, optional, type=any
;                       A named variable that will contain data from the 13th `GROUP`.
;    GROUP13:           out, optional, type=any
;                       A named variable that will contain data from the 14th `GROUP`.
;    GROUP14:           out, optional, type=any
;                       A named variable that will contain data from the 15th `GROUP`.
;    GROUP15:           out, optional, type=any
;                       A named variable that will contain data from the 16th `GROUP`.
;    GROUP16:           out, optional, type=any
;                       A named variable that will contain data from the 17th `GROUP`.
;
; :Keywords:
;    COLUMN_NAMES:      in, optional, type=strarr
;                       Names for the columns in the data; if there are groups specified,
;                           the column names should be repeated for each column in the
;                           group, so that the number of column names is always equal to
;                           the number of columns. The default is to name the columns
;                           "Field#", where "#" represents the `GROUP` it belongs to.
;    COLUMN_TYPES:      in, optional, type=strarr/lonarr, default=4 (float)
;                       SIZE type codes for the columns in the data; if there are groups
;                           specified, the column types should be repeated for each column
;                           in the group, so that the number of column types is always
;                           equal to the number of columns
;    COMMENT_SYMBOL:    in, optional, type=string, default=''
;                       Specifies a comment character for the lines in the file
;    COUNT:             out, optional, type=long
;                       Set to a named variable to get the number of records read
;    VARIABLE:          in, optional, type=string/integer
;                       Either the name in `COLUMN_NAMES` or the group in `GROUPS` that
;                           represents the column(s) of data that should be returned in
;                           `DATA`. If present, GROUP# parameters are ignored unless the
;                           DEPEND_# keywords are present.
;    DATA_START:        in, optional, TYPE=long, DEFAULT=0L
;                       Number of lines to skip at the beginning of the file
;    DELIMITER:         in, optional, type=string, default=' '
;                       Delimiter between columns of data.
;    DEPEND_0:          in, optional, type=string/integer
;                       Either the name in `COLUMN_NAMES` or the group in `GROUPS` that
;                           represents the data on which `VARIABLE` depends. Typically,
;                           DEPEND_0 represents time in a time-series. If present, the
;                           data will be returned into the `GROUP0` parameter. Ignored
;                           if `VARIABLE` is not present.
;    DEPEND_1:          in, optional, type=string/integer
;                       Either the name in `COLUMN_NAMES` or the group in `GROUPS` that
;                           represents the data on which `VARIABLE` depends. If `VARIABLE`
;                           is multi-dimensional, then DEPEND_1 characterizes one of its
;                           dimensions. If present, the data will be returned into the
;                           `GROUP1` parameter. Ignored if `VARIABLE` and `DEPEND_0` are
;                           not present.
;    DEPEND_2:          in, optional, type=string/integer
;                       Either the name in `COLUMN_NAMES` or the group in `GROUPS` that
;                           represents the data on which `VARIABLE` depends. If `VARIABLE`
;                           is multi-dimensional, then DEPEND_2 characterizes one of its
;                           dimensions. If present, the data will be returned into the
;                           `GROUP2` parameter. Ignored if `VARIABLE`, `DEPEND_0`,
;                           and `DEPEND_1` are not present.
;    DEPEND_3:          in, optional, type=string/integer
;                       Either the name in `COLUMN_NAMES` or the group in `GROUPS` that
;                           represents the data on which `VARIABLE` depends. If `VARIABLE`
;                           is multi-dimensional, then DEPEND_3 characterizes one of its
;                           dimensions. If present, the data will be returned into the
;                           `GROUP3` parameter. Ignored if `VARIABLE`, `DEPEND_0`,
;                           `DEPEND_1`, and `DEPEND_2` are not present.
;                       The group name or number o
;    DIALOG_PARENT:     in, optional, type=widget_id
;                       The widget ID of that will serve as the parent to the dailog
;                           windows that may appears.
;    GROUPS:            in, optional, type=lonarr
;                       Indices of groups for each column, i.e.::
;
;                               [0, 0, 0, 0, 0, 0, 0]
;
;                           indicates all seven columns are in a single group, where::
;
;                               [0, 1, 2, 3, 4, 5, 6]
;
;                           would put each column in a new group. If columns are grouped
;                           together, they will be put into a 2D array, having the same
;                           number of columns as there are members in the group. The
;                           default is to put each column in its own group.
;    HASH:              in, optional, type=boolean, default=0
;                       If set, `DATA` will be returned as a hash instead of a structure.
;                           The hash keys will be the same as `COLUMN_NAMES`.
;    HEADER:            out, optional, type=strarr
;                       Set to a named variable to get the header information skipped by
;                           `DATA_START`
;    MISSING_VALUE:     in, optional, type=scalar, default=!values.f_nan
;                       Value to use for missing items in the data
;    NUM_RECORDS:       in, optional, type=long
;                       Number of records to read; default is to read all available records
;    RECORD_START:      in, optional, type=long, DEFAULT=0
;                       Set to index of first record to read (after `DATA_START` is taken
;                           into account)
;    TEMPLATE:          in, out, opitonal, type=structure
;                       An template returned by Ascii_Template(). If provided, all of the
;                           keywords related to Ascii_Template will be ignored. Upon exit,
;                           the template used to read the ascii file will be returned via
;                           this keyword.
;    VARIABLE:          in, optional, type=string/integer
;                       Either the name in `COLUMN_NAMES` or the group in `GROUPS` that
;                           represents the data to be returned. If provided, `DATA` will
;                           be an array containing data only from the column(s) indicated.
;    VERBOSE:           in, optional, type=boolean
;                       Set to print runtime messages
;
; :Returns:
;    DATA:              A data structure (or hash if `HASH` is set) with each field (key)
;                           containing a `GROUP` of data. If dialog boxes were cancelled,
;                           -1 will be returned. Furthermore, if any of the Group#
;                           parameters are present, -1 will be returned.
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
;       2014/02/11  -   Written by Matthew Argall. Adapted from Michael Galloy's
;                           `mg_read_ascii.pro <https://github.com/mgalloy/`
;       2014/02/19  -   Added the VARIABLE and DEPEND_[0123] keywords. - MRA
;       2014/02/20  -   Made the program more human readable. - MRA
;       2014/05/19  -   COLUMN_TYPES can be a string array of datatype names. - MRA
;       2014/05/25  -   If a filename is given, but no way to distinguish between columns,
;                           open the ascii template dialog with the file already chosen. - MRA
;-
function MrRead_Ascii, filename, Group0, Group1, Group2, Group3, Group4, Group5, $
                                 Group6, Group7, Group8, Group9, Group10, Group11, $
                                 Group12, Group13, Group14, Group15, Group16, $
DEPEND_0=depend_0, $
DEPEND_1=depend_1, $
DEPEND_2=depend_2, $
DEPEND_3=depend_3, $
DIALOG_PARENT=dialog_parent, $
HASH=to_hash, $
VARIABLE=variable, $
;Ascii_Template
COLUMN_NAMES=columnNames, $
COLUMN_TYPES=columnTypes, $
COMMENT_SYMBOL=commentSymbol, $
DATA_START=dataStart, $
DELIMITER=delimiter, $
GROUPS=groups, $
MISSING_VALUE=missingValue, $
TEMPLATE=template, $
;Read_Ascii
COUNT=count, $
HEADER=header, $
NUM_RECORDS=numRecords, $
RECORD_START=recordStart, $
VERBOSE=verbose
  compile_opt strictarr
  on_error, 2

    _filename      = n_elements(filename)      eq 0  ? ''  : filename
    _commentSymbol = n_elements(commentSymbol) eq 0  ? ''  : commentSymbol
    _dataStart     = n_elements(dataStart)     eq 0L ? 0L  : dataStart
    _delimiter     = n_elements(delimiter)     eq 0  ? ' ' : delimiter

;---------------------------------------------------------------------
;Ask for a file? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if _filename eq '' then begin
        _filename = dialog_pickfile(/READ, /MUST_EXIST, DIALOG_PARENT=dialog_parent)
        if _filename eq '' then return, -1
    endif

;---------------------------------------------------------------------
;Template Given? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(template) gt 0 then begin
        t = template

;---------------------------------------------------------------------
;Build the Template //////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;ColumnNames, ColumnTypes, and Groups must have the same number of elements
        ;or be undefined, but one of them must be defined
        nColumns = n_elements(columnTypes) eq 0L $
                    ? (n_elements(columnNames) eq 0L $
                        ? (n_elements(groups) eq 0L $
                            ? 0L $
                            : n_elements(groups)) $
                        : n_elements(columnNames)) $
                   : n_elements(columnTypes)

    ;---------------------------------------------------------------------
    ;Call Ascii_Template? ////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if (nColumns eq 0L) then begin
            if _filename eq '' $
                then t = ascii_template(CANCEL=cancel, GROUP_ID=dialog_parent) $
                else t = ascii_template(_filename, CANCEL=cancel, GROUP_ID=dialog_parent)
            if cancel eq 1B then return, -1
            
    ;---------------------------------------------------------------------
    ;Create a Custom Template ////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin

            ;Pad column names with the correct number of zeros. ALog10 will return
            ;the number of digits in NCOLUMNS
            colNameFormat = '(I0' + strtrim(ceil(alog10(nColumns)), 2) + ')'

            ;Default to "Column#", where # indicates the column number
            _columnNames = n_elements(columnNames) eq 0L $
                           ? 'Field' + string(sindgen(nColumns), FORMAT=colNameFormat) $
                           : columnNames
                   
            ;Default to floats
            if n_elements(columnTypes) gt 0 then begin
                if MrIsA(columnTypes, 'STRING') $
                    then _columnTypes = typeName2Code(columnTypes) $
                    else _columnTYpes = columnTypes
            endif else begin
                _columnTypes = lonarr(nColumns) + 4L
            endelse

            ;Defualt to putting everything in its own group.
            _groups = n_elements(groups) eq 0L ? lindgen(nColumns) : groups

            ;Default to !Values.F_NaN
            _missingValue = n_elements(missingValue) eq 0L $
                            ? !Values.F_NaN $
                            : missingValue

            ;Create the template
            t = { version:        1.0, $
                  datastart:      _dataStart, $
                  delimiter:      byte(_delimiter), $
                  missingvalue:   _missingValue, $
                  commentsymbol:  _commentSymbol, $
                  fieldcount:     nColumns, $
                  fieldtypes:     _columnTypes, $
                  fieldnames:     _columnNames, $
                  fieldlocations: lindgen(nColumns), $
                  fieldgroups:    _groups $
                }
        endelse
    endelse
            
;---------------------------------------------------------------------
;Read the Data ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    data = read_ascii(filename, COUNT=count, HEADER=header, TEMPLATE=t, $
                      NUM_RECORDS=numRecords, RECORD_START=recordStart, $
                      VERBOSE=verbose)
    
    ;Return the template
    template = temporary(t)
            
;---------------------------------------------------------------------
;Were VARIABLE and DEPEND_[0-3] Given? ///////////////////////////////
;---------------------------------------------------------------------
    ;
    ; If so, get the depend_# data first so that the large data structure
    ; can be over-written when VARIABLE is extracted.
    ;
    
    if n_elements(variable) gt 0 then begin
        nDep0    = n_elements(depend_0)
        nDep1    = n_elements(depend_1)
        nDep2    = n_elements(depend_2)
        nDep3    = n_elements(depend_3)
        colNames = tag_names(data)
        
    ;---------------------------------------------------------------------
    ;DEPEND_0 ////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if (nDep0 gt 0) then begin
            ;Column Name or Group?
            if MrIsA(depend_0, 'STRING', /SCALAR) $
                then iCol = where(colNames eq strupcase(depend_0), nCol) $
                else iCol = where(groups eq depend_0, nCol)
        
            ;Was the column found?
            if nCol eq 0 then begin
                message, 'Depend_0 "' + strtrim(depend_0, 2) + '" is not a Column_Name or Group. ' + $
                         'Ignoring.', /INFORMATIONAL
            endif else group0 = data.(iCol[0])
        endif
    
    ;---------------------------------------------------------------------
    ;DEPEND_1 ////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if (nDep0 gt 0) && (nDep1 gt 0) then begin
            ;Column Name or Group?
            if MrIsA(depend_1, 'STRING', /SCALAR) $
                then iCol = where(colNames eq strupcase(depend_1), nCol) $
                else iCol = where(groups eq depend_1, nCol)
        
            ;Was the column found?
            if nCol eq 0 then begin
                message, 'Depend_1 "' + strtrim(depend_1, 2) + '" is not a Column_Name or Group. ' + $
                         'Ignoring.', /INFORMATIONAL
            endif else group1 = data.(iCol[0])
        endif
    
    ;---------------------------------------------------------------------
    ;DEPEND_2 ////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if (nDep0 gt 0) && (nDep1 gt 0) && (nDep2 gt 0) then begin
            ;Column Name or Group?
            if MrIsA(depend_2, 'STRING', /SCALAR) $
                then iCol = where(colNames eq strupcase(depend_2), nCol) $
                else iCol = where(groups eq depend_2, nCol)
        
            ;Was the column found?
            if nCol eq 0 then begin
                message, 'Depend_2 "' + strtrim(depend_2, 2) + '" is not a Column_Name or Group. ' + $
                         'Ignoring.', /INFORMATIONAL
            endif else group2 = data.(iCol[0])
        endif
    
    ;---------------------------------------------------------------------
    ;DEPEND_3 ////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if (nDep0 gt 0) && (nDep1 gt 0) && (nDep2 gt 0) && (nDep3 gt 0) then begin
            ;Column Name or Group?
            if MrIsA(depend_3, 'STRING', /SCALAR) $
                then iCol = where(colNames eq strupcase(depend_3), nCol) $
                else iCol = where(groups eq depend_3, nCol)
        
            ;Was the column found?
            if nCol eq 0 then begin
                message, 'Depend_3 "' + strtrim(depend_3, 2) + '" is not a Column_Name or Group. ' + $
                         'Ignoring.', /INFORMATIONAL
            endif else group3 = data.(iCol[0])
        endif
    
    ;---------------------------------------------------------------------
    ;VARIABLE ////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if MrIsA(variable, 'STRING', /SCALAR) $
            then iCol = where(colNames eq strupcase(variable), nCol) $
            else iCol = where(groups eq variable, nCol)
    
        if nCol eq 0 then begin
            message, 'Depend_1 "' + strtrim(variable, 2) + '" is not a Column_Name or Group. ' + $
                     'Ignoring.', /INFORMATIONAL
            return, data
        endif else begin
            return, data.(iCol[0])
        endelse

;---------------------------------------------------------------------
;Return a Hash or Structure? /////////////////////////////////////////
;---------------------------------------------------------------------
    endif else if n_params() le 1 then begin
        if keyword_set(to_hash) $
            then return, hash(data, /EXTRACT) $
            else return, data
            
;---------------------------------------------------------------------
;Return Structure Elements? //////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        switch n_params() of
            18: Group16 = data.(16)
            17: Group15 = data.(15)
            16: Group14 = data.(14)
            15: Group13 = data.(13)
            14: Group12 = data.(12)
            13: Group11 = data.(11)
            12: Group10 = data.(10)
            11: Group9  = data.(9)
            10: Group8  = data.(8)
             9: Group7  = data.(7)
             8: Group6  = data.(6)
             7: Group5  = data.(5)
             6: Group4  = data.(4)
             5: Group3  = data.(3)
             4: Group2  = data.(2)
             3: Group1  = data.(1)
             2: Group0  = data.(0)
             1: return, -1
        endswitch
    endelse
end


            
;---------------------------------------------------------------------
;Main level program: IDL> .r MrAscii_Template ////////////////////////
;---------------------------------------------------------------------
;An example using column names and types, skipping a header, and putting
;everthing into its own group.
col_names = ['lon', 'lat', 'elev', 'temp', 'dewpt', 'wind_speed', 'wind_dir']
wdata = MrRead_Ascii(file_which('ascii.txt'), DATA_START=5, $
                     COLUMN_TYPES=[4, 4, 3, 3, 3, 3, 3], $
                     COLUMN_NAMES=col_names, $
                     GROUPS=lindgen(7), COUNT=nrows)
help, wdata

;This time, put each column into the same group and look for missing values.
adata = MrRead_Ascii(file_which('ascii.dat'), DATA_START=3, $
                     DELIMITER=string(9B), COMMENT_SYMBOL='%', $
                     GROUPS=lonarr(4), MISSING_VALUE=-999.)
print, adata.field0

end

