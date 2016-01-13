; docformat = 'rst'
;
; NAME:
;       MrRead__Define
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   The purpose of this class is to provide general methods for reading data from a
;   variety of file formats. Formats currently implemented include::
;       * ASCII
;       * Common Data Format
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG
;       cgRootName
;       MrConcatenate
;       MrFindFile__Define
;       MrRead_Ascii
;       CDF_Read__Define
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
;       2014/02/23  -   Written by Matthew Argall.
;       2014/02/28  -   Added option to convert CDF Epoch times to seconds since midnight. - MRA
;       2014/03/06  -   Inherit MrRead_Container. Added File_Finder as a property. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to concatenate data together.
;
; :Params:
;       DATA:               in, required, type=hash/structure
;                           A structure or hash whose fields are to be concatenated.
;                               Each tag/key should conatain one data array.
;       ICAT:               in, required, type=integer
;                           The dimension, starting with 1 (one), that should be
;                               concatenated. All other dimensions should have equal size.
;
; :Returns:
;       CAT_ARRAY:          The concatenated data array.
;-
function MrRead::Concatenate, data, iCat
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, !Null
    endif
    
    type = size(data, /TNAME)
    
    ;Structure
    if type eq 'STRUCT' then begin
        nTags = n_tags(data)
        for i = 1, nTags-1 do begin
            if i eq 1 $
                then cat_array = MrConcatenate(data.(0), data.(i), iCat) $
                else cat_array = MrConcatenate(temporary(cat_array), data.(i), iCat)
        endfor
    
    ;Hash
    endif else if type eq 'OBJREF' then begin
        keys  = data -> Keys()
        nKeys = data -> Count()
        
        ;Step through the keys
        for i = 1, nTags-1 do begin
            if i eq 1 $
                then cat_array = MrConcatenate(data[keys[0]], data[keys[i]], iCat) $
                else cat_array = MrConcatenate(temporary(cat_array), data[keys[i]], iCat)
        endfor
    endif
        
    return, cat_array
end



;+
;   Get class properties.
;
; :Keywords:
;       _REF_EXTRA:             in, optional, type=any
;                               Any keyword accepted by the MrFileFinder::SetProperty
;                                   method is also accepted via keyword inheritance.
;-
function MrRead::GetFiles, $
NFILES=nFiles
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(oFile) then obj_destroy, oFile
        MrPrintF, 'LogErr'
        return, ''
    endif
    
    ;Update the properties.
    self.file_finder -> SetProperty, TPATTERN=self.pattern, STIME=self.sTime, ETIME=self.eTime
    
    ;Find the files
    files = self.file_finder -> GetFiles(NFILES=nFiles, /UNIQUE)
    
    ;Return files found in this manner.
    return, files
end


;+
;   Get class properties.
;
; :Keywords:
;       _REF_EXTRA:             in, optional, type=any
;                               Any keyword accepted by the MrFileFinder::SetProperty
;                                   method is also accepted via keyword inheritance.
;-
pro MrRead::GetProperty, $
PATTERN=pattern, $
STIME=sTime, $
ETIME=eTime
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Get object properties
    if arg_present(pattern) then pattern = self.pattern
    if arg_present(sTime)   then sTime   = self.sTime
    if arg_present(eTime)   then eTime   = self.eTime
end


;+
;   The purpose of this method is to replace the tokens found in the input file pattern
;   with date and times defined in the data interval.
;
; :Params:
;
; :Returns:
;-
function MrRead::Read, param1, param2, param3, param4, param5, $
STRUCTURE=structure, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, !Null
    endif

    ;Which file type are we reading?
    if self.sTime eq '' or self.eTime eq '' then $
        message, 'Use the SetProperty method to set STIME and ETIME.'
        
    ;Get the file names
    void = cgRootName(self.file_pattern, EXTENSION=ext)
    
    case strupcase(ext) of
        'CDF': extension = 'CDF'
        'DAT': extension = 'ASC'
        'TXT': extension = 'ASC'
        'ASC': extension = 'ASC'
        'H5':  extension = 'H5'
        else: message, 'Extension unknown: "' + ext + '".'
    endcase
    
    ;Call the proper method.
    case extension of
        'CDF': begin
            case n_params() of
                0: data = self -> Read_CDF(STRUCTURE=structure)
                1: data = self -> Read_CDF(param1, STRUCTURE=structure)
                2: data = self -> Read_CDF(param1, param2, STRUCTURE=structure)
                3: data = self -> Read_CDF(param1, param2, param3, STRUCTURE=structure)
                4: data = self -> Read_CDF(param1, param2, param3, param4, STRUCTURE=structure)
                5: data = self -> Read_CDF(param1, param2, param3, param4, param5, STRUCTURE=structure)
                else: message, 'Incorrect number of parameters.'
            endcase
        endcase
        
        'ASC': begin
            case n_elements(param1) of
                0: data = self -> Read_ASCII(STRUCTURE=structure, _EXTRA=extra)
                1: data = self -> Read_ASCII(param1, STRUCTURE=structure, _EXTRA=extra)
                2: data = self -> Read_ASCII(param1, param2, STRUCTURE=structure, _EXTRA=extra)
                3: data = self -> Read_ASCII(param1, param2, param3, STRUCTURE=structure, _EXTRA=extra)
                4: data = self -> Read_ASCII(param1, param2, param3, param4, STRUCTURE=structure, _EXTRA=extra)
                5: data = self -> Read_ASCII(param1, param2, param3, param4, param5, STRUCTURE=structure, _EXTRA=extra)
                else: message, 'Incorrect number of elements: PARAM1.'
            endcase
        endcase
            
        'H5': message, 'HDF5 files have not been incorporated yet. Try the "MrHDF5" object.'
    endcase
    
    return, data
end


;+
;   Read an ASCII file
;
; :Params:
;       VARIABLES:      in, required, type=strarr/intarr
;                       Either the name in COLUMN_NAMES or the group in GROUP
;                           corresponding to the data column containing the data and
;                           its dependencies. VARIABLES can have up to 5 elements, one
;                           for `DATA`, `DEPEND_0`, `DEPEND_1`, `DEPEND_2` and `DEPEND_3`.
;       DEPEND_0:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_0" variable attribute under `VARIABLES`
;       DEPEND_1:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_1" variable attribute under `VARIABLES`
;       DEPEND_2:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_2" variable attribute under `VARIABLES`
;       DEPEND_3:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_3" variable attribute under `VARIABLES`
;
; :Keywords:
;       STRUCTURE:      in, optional, type=boolean, default=0
;                       If set and the data requested spans multiple data files, then
;                           `DATA`, `DEPEND_0`, `DEPEND_1`, `DEPEND_2` and `DEPEND_3` will
;                           be an anonymous into a structure whose fields are "FILE1",
;                           "FILE2", etc.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the MrRead_Ascii procedure is also
;                           accepted via keyword inheritance.
;
; :Returns:
;       DATA:           For single files, the data associated with `VARIABLES`. For
;                           multiple files, a hash with one key-value
;-
function MrRead::Read_ASCII, variables, depend_0, depend_1, depend_2, depend_3, $
VARIABLE=variable, $    ;Ignored -- Replaced by VARIABLES parameter
DEPEND_0=dep_0, $       ;Ignored -- Replaced by VARIABLES parameter
DEPEND_1=dep_1, $       ;Ignored -- Replaced by VARIABLES parameter
DEPEND_2=dep_2, $       ;Ignored -- Replaced by VARIABLES parameter
DEPEND_3=dep_3, $       ;Ignored -- Replaced by VARIABLES parameter
STRUCTURE=structure, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(oCDF) then obj_destroy, oCDF
        MrPrintF, 'LogErr'
        return, !Null
    endif

;---------------------------------------------------------------------
;Inputs //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    structure = keyword_set(structure)
    filenames = self -> GetFiles(NFILES=nFiles)

    nIn = n_elements(variables)
    key_fmt = string(ceil(alog10(nFiles)), FORMAT='(i0)')

;---------------------------------------------------------------------
;Create Hash? ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if structure eq 0 && nFiles gt 1 then begin
        switch nIn of
            5: depend_3 = hash()
            4: depend_2 = hash()
            3: depend_1 = hash()
            2: depend_0 = hash()
            1: data     = hash()
        endswitch
    endif

;---------------------------------------------------------------------
;Read Data ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    for i = 0, nFiles - 1 do begin
        ;Read data from the file.
        case nIn of
            0: temp_data = MrRead_Ascii(filenames[i], VARIABLE=variables[0], REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            1: temp_data = MrRead_Ascii(vname, $
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            2: temp_data = MrRead_Ascii(vname, temp_0, $
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            3: temp_data = MrRead_Ascii(vname, temp_0, temp_1, $
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            4: temp_data = MrRead_Ascii(vname, temp_0, temp_1, temp_2,$
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            5: temp_data = MrRead_Ascii(vname, temp_0, temp_1, temp_2, temp_3, $
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            else: message, 'Incorrect number of parameters.'
        endcase
        
        ;If only one file, then break out of the loop without creating a hash or
        ;a structure.
        if nFiles eq 1 then begin
            switch nIn of
                5: depend_3 = temporary(temp_3)
                4: depend_2 = temporary(temp_2)
                3: depend_1 = temporary(temp_1)
                2: depend_0 = temporary(temp_0)
                1: data     = temporary(temp_data)
            endswitch
            
            BREAK
        endif
        
        ;Create the key/tag
        field_name = 'FILE' + string(i, FORMAT='(i0' + key_fmt  + ')')
    ;---------------------------------------------------------------------
    ;Hash ////////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if structure eq 0 then begin
            switch nIn of
                5: depend_3[field_name] = temporary(temp_3)
                4: depend_2[field_name] = temporary(temp_2)
                3: depend_1[field_name] = temporary(temp_1)
                2: depend_0[field_name] = temporary(temp_0)
                1: data[field_name]     = temporary(temp_data)
            endswitch

    ;---------------------------------------------------------------------
    ;Structure ///////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            switch nIn of
                5: if i eq 0 $
                        then depend_3 = create_struct(field_name, temporary(temp_3)) $
                        else depend_3 = create_struct(field_name, temporary(temp_3), depend_3)
                4: if i eq 0 $
                        then depend_2 = create_struct(field_name, temporary(temp_2)) $
                        else depend_2 = create_struct(field_name, temporary(temp_2), depend_2)
                3: if i eq 0 $
                        then depend_1 = create_struct(field_name, temporary(temp_1)) $
                        else depend_1 = create_struct(field_name, temporary(temp_1), depend_1)
                2: if i eq 0 $
                        then depend_0 = create_struct(field_name, temporary(temp_0)) $
                        else depend_0 = create_struct(field_name, temporary(temp_0), depend_0)
                1: if i eq 0 $
                        then data = create_struct(field_name, temporary(temp_data)) $
                        else data = create_struct(field_name, temporary(temp_data), data)
            endswitch
        endelse
    endfor
    
    return, data
end


;+
;   Read a CDF file
;
; :Params:
;       VNAME:          in, required, type=string
;                       The variable name within the CDF file whose data is to be read.
;       DEPEND_0:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_0" variable attribute under `VNAME`
;       DEPEND_1:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_1" variable attribute under `VNAME`
;       DEPEND_2:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_2" variable attribute under `VNAME`
;       DEPEND_3:       out, optional, type=array/structure/hash
;                       Data listed as a "depend_3" variable attribute under `VNAME`
;
; :Keywords:
;       ICAT:           in, optional, type=integer, default=0
;                       The dimension, starting with 1 (one), along which to concatenate
;                           data from various files. If zero, no concatenation is implied.
;       SECONDS:        in, optional, type=boolean, default=0
;                       If set, CDF EPOCH times in `DEPEND_0` will be converted to seconds
;                           since midnight.
;       STRUCTURE:      in, optional, type=boolean, default=0
;                       If set and the data requested spans multiple data files, then
;                           `DATA`, `DEPEND_0`, `DEPEND_1`, `DEPEND_2` and `DEPEND_3` will
;                           be an anonymous into a structure whose fields are "FILE1",
;                           "FILE2", etc.
;
; :Returns:
;       DATA:           For single files, the data associated with `VNAME`. For multiple
;                           files, a hash with one key-valeu
;-
function MrRead::Read_CDF, vname, depend_0, depend_1, depend_2, depend_3, $
ICAT=iCat, $
SECONDS=seconds, $
STRUCTURE=structure
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(oCDF) then obj_destroy, oCDF
        MrPrintF, 'LogErr'
        return, !Null
    endif

;---------------------------------------------------------------------
;Inputs //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    nIn = n_params()
    if n_elements(iCat) eq 0 then iCat = 0
    seconds   = keyword_set(seconds)
    structure = keyword_set(structure)
    
    ;Get the names of the files to read.
    filenames = self -> GetFiles(NFILES=nFiles)
    if nFiles eq 0 then return, !Null
    if nFiles eq 1 then iCat = 0

    ;Number of digits in the structure/hash keys.
    key_fmt = string(ceil(alog10(nFiles)), FORMAT='(i0)')

;---------------------------------------------------------------------
;Create Hash? ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if structure eq 0 && nFiles gt 1 then begin
        switch nIn of
            5: depend_3 = hash()
            4: depend_2 = hash()
            3: depend_1 = hash()
            2: depend_0 = hash()
            1: data     = hash()
        endswitch
    endif

;---------------------------------------------------------------------
;Read Data ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    for i = 0, nFiles - 1 do begin
        oCDF = obj_new('cdf_read', filenames[i])

        ;Read data from the file.
        case nIn of
            0: temp_data = oCDF -> Read(REC_START=self.sTime, REC_END=self.eTime, /REC_TIMES, /TO_EPOCH)
            1: temp_data = oCDF -> Read(vname, $
                                        REC_START=self.sTime, REC_END=self.eTime, /REC_TIMES, /TO_EPOCH)
            2: temp_data = oCDF -> Read(vname, temp_0, $
                                        REC_START=self.sTime, REC_END=self.eTime, /REC_TIMES, /TO_EPOCH)
            3: temp_data = oCDF -> Read(vname, temp_0, temp_1, $
                                        REC_START=self.sTime, REC_END=self.eTime, /REC_TIMES, /TO_EPOCH)
            4: temp_data = oCDF -> Read(vname, temp_0, temp_1, temp_2,$
                                        REC_START=self.sTime, REC_END=self.eTime, /REC_TIMES, /TO_EPOCH)
            5: temp_data = oCDF -> Read(vname, temp_0, temp_1, temp_2, temp_3, $
                                        REC_START=self.sTime, REC_END=self.eTime, /REC_TIMES, /TO_EPOCH)
            else: message, 'Incorrect number of parameters.'
        endcase
        obj_destroy, oCDF
        
        ;If only one file, then break out of the loop without creating a hash or
        ;a structure.
        if nFiles eq 1 then begin
            switch nIn of
                5: depend_3 = temporary(temp_3)
                4: depend_2 = temporary(temp_2)
                3: depend_1 = temporary(temp_1)
                2: if seconds eq 1 $
                        then depend_0 = epoch_to_ssm(temporary(temp_0)) $
                        else depend_0 = temporary(temp_0)
                1: data     = temporary(temp_data)
            endswitch

            BREAK
        endif
        
        ;Create the key/tag
        field_name = 'FILE' + string(i, FORMAT='(i0' + key_fmt  + ')')
    ;---------------------------------------------------------------------
    ;Hash ////////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if structure eq 0 then begin
            switch nIn of
                5: depend_3[field_name] = temporary(temp_3)
                4: depend_2[field_name] = temporary(temp_2)
                3: depend_1[field_name] = temporary(temp_1)
                2: depend_0[field_name] = temporary(temp_0)
                1: data[field_name]     = temporary(temp_data)
            endswitch

    ;---------------------------------------------------------------------
    ;Structure ///////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            switch nIn of
                5: if i eq 0 $
                        then depend_3 = create_struct(field_name, temporary(temp_3)) $
                        else depend_3 = create_struct(field_name, temporary(temp_3), depend_3)
                4: if i eq 0 $
                        then depend_2 = create_struct(field_name, temporary(temp_2)) $
                        else depend_2 = create_struct(field_name, temporary(temp_2), depend_2)
                3: if i eq 0 $
                        then depend_1 = create_struct(field_name, temporary(temp_1)) $
                        else depend_1 = create_struct(field_name, temporary(temp_1), depend_1)
                2: if i eq 0 $
                        then depend_0 = create_struct(field_name, temporary(temp_0)) $
                        else depend_0 = create_struct(field_name, temporary(temp_0), depend_0)
                1: if i eq 0 $
                        then data = create_struct(field_name, temporary(temp_data)) $
                        else data = create_struct(field_name, temporary(temp_data), data)
            endswitch
        endelse
    endfor

    ;Concatenate    
    if iCat ne 0 then data = self -> Concatenate(temporary(data), iCat)
    
    return, data
end


;+
;   Set class properties.
;
; :Keywords:
;       _REF_EXTRA:             in, optional, type=any
;                               Any keyword accepted by the MrFileFinder::SetProperty
;                                   method is also accepted via keyword inheritance.
;-
pro MrRead::SetProperty, $
PATTERN=pattern, $
STIME=sTime, $
ETIME=eTime
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    if n_elements(sTime)   gt 0 then self.sTime   = sTime
    if n_elements(eTime)   gt 0 then self.eTime   = eTime
    if n_elements(pattern) gt 0 then self.pattern = pattern
end


;+
;   Clean up after the object is destroy
;-
pro MrRead::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg(/QUIET)
        return
    endif
    
    ;Destroy objects
    obj_destroy, self.file_finder
end


;+
;   The initialization method.
;
; :Params:
;-
function MrRead::init, pattern, sTime, eTime
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, 0
    endif
    
    ;File finding object
    self.file_finder = obj_new('MrFileFinder')
    
    ;Set object properties
    self -> SetProperty, PATTERN=pattern, $
                         STIME=sTime, $
                         ETIME=eTime
    
    return, 1
end


;+
;   The class definition statement.
;
; :Fields:
;-
pro MrRead__define
    compile_opt strictarr
    
    class = { MrRead, $
              file_finder: obj_new(), $
              pattern:     '', $
              sTime:       '', $
              eTime:       '' $
            }
end