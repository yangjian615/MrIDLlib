; docformat = 'rst'
;
; NAME:
;       MrRead2__Define
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
;       MrRead2_Ascii
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
;       2014/03/06  -   Inherit MrRead2_Container. Added File_Finder as a property. - MRA
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
function MrRead2::Concatenate, data, iCat
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
function MrRead2::GetFiles, $
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
pro MrRead2::GetProperty, $
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
;                       Any keyword accepted by the MrRead2_Ascii procedure is also
;                           accepted via keyword inheritance.
;
; :Returns:
;       DATA:           For single files, the data associated with `VARIABLES`. For
;                           multiple files, a hash with one key-value
;-
function MrRead2::Read_ASCII, variables, depend_0, depend_1, depend_2, depend_3, $
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
            0: temp_data = MrRead2_Ascii(filenames[i], VARIABLE=variables[0], REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            1: temp_data = MrRead2_Ascii(vname, $
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            2: temp_data = MrRead2_Ascii(vname, temp_0, $
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            3: temp_data = MrRead2_Ascii(vname, temp_0, temp_1, $
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            4: temp_data = MrRead2_Ascii(vname, temp_0, temp_1, temp_2,$
                                        REC_START=self.iso_start, REC_END=self.iso_end, /REC_TIMES, /TO_EPOCH)
            5: temp_data = MrRead2_Ascii(vname, temp_0, temp_1, temp_2, temp_3, $
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
;       SECONDS:        in, optional, type=boolean, default=0
;                       If set, CDF EPOCH times in `DEPEND_0` will be converted to seconds
;                           since midnight.
;       STRUCTURE:      in, optional, type=boolean, default=0
;                       If set and the data requested spans multiple data files, then
;                           `DATA`, `DEPEND_0`, `DEPEND_1`, `DEPEND_2` and `DEPEND_3` will
;                           be an anonymous into a structure whose fields are "FILE1",
;                           "FILE2", etc.
;       TO_ARRAY:       in, optional, type=boolean
;                       If set, then whem multiple files are found, data will be
;                           concatenated into a single array. This is the assumed action.
;       TO_MRARRAY:     in, optional, type=boolean, default=0
;                       If set, then when multiple files are found, data will be
;                           gathered into MrArray objects.
;       TO_HASH:        in, optional, type=boolean, default=0
;                       If set, then when multiple files are found, data from each file
;                           will be stored in a has, with keys being "FILE##", where ##
;                           corresponds to the file index.
;       TO_STRUCTURE:   in, optional, type=boolean, default=0
;                       If set, then when multiple files are found, data from each file
;                           will be stored in a has, with field being "FILE##", where ##
;                           corresponds to the file index.
;
; :Returns:
;       DATA:           For single files, the data associated with `VNAME`. For multiple
;                           files, a hash with one key-value pair per file.
;-
function MrRead2::Read_CDF, varName, depend_0, depend_1, depend_2, depend_3, $
SECONDS=seconds, $
TO_ARRAY=to_array, $
TO_MRARRAY=to_MrArray, $
TO_HASH=to_hash, $
TO_STRUCTURE=to_structure
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
    seconds   = keyword_set(seconds)
    
    ;Get the names of the files to read.
    filenames = self -> GetFiles(NFILES=nFiles)
    if nFiles eq 0 then return, !Null
    if nFiles eq 1 then iCat = 0

    ;Number of digits in the structure/hash keys.
    key_fmt = string(ceil(alog10(nFiles)), FORMAT='(i0)')

;---------------------------------------------------------------------
; Single File ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nFiles eq 0 then begin
        ;Open the file
        oCDF = obj_new('CDF_File', filenames)
        
        ;Read the data
        data = oCDF -> Read(varName, depend_0, depend_1, depend_2, depend_3, $
                            REC_START=self.sTime, REC_END=self.eTime, /ISO)
        obj_destroy, oCDF
                            
        ;Return the data
        return, data
    endif

;---------------------------------------------------------------------
; Multiple Files /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    to_mrarray   = keyword_set(to_mrarray)
    to_array     = keyword_set(to_array)
    to_hash      = keyword_set(to_hash)
    to_structure = keyword_set(to_structure)
    if (to_mrarray + to_array + to_hash + to_struct) eq 0 then to_array = 1
    if (to_mrarray + to_array + to_hash + to_struct) ne 1 then $
        message, 'One and only one of TO_ARRAY, TO_HASH, TO_MRARRAY, and TO_STRUCTURE may be chosen.'
    
    ;Store data in a list before concatenating?
    if to_array eq 1 and MrCmpVersion('8.0') le 0 $
        then to_list = 1 $
        else to_list = 0
    
    ;Create a hash?
    if to_hash then begin
        switch nIn of
            5: depend_3 = hash()
            4: depend_2 = hash()
            3: depend_1 = hash()
            2: depend_0 = hash()
            1: data     = hash()
        endswitch
    
    ;Create a MrArray?
    endif else if to_MrArray then begin
        switch nIn of
            5: depend_3 = MrArray()
            4: depend_2 = MrArray()
            3: depend_1 = MrArray()
            2: depend_0 = MrArray()
            1: data     = MrArray()
        endswitch
        
    ;Create a list?
    endelse if to_list then begin
        switch nIn of
            5: depend_3 = List()
            4: depend_2 = List()
            3: depend_1 = List()
            2: depend_0 = List()
            1: data     = List()
        endswitch
    endelse
    
    ;Step through each file.
    for i = 0, nFiles - 1 do begin
        ;Open the file
        oCDF = obj_new('CDF_File', filenames[i])
        
        ;Read the data
        tempData = oCDF -> Read(varName, temp_0, temp_1, temp_2, temp_3, $
                                REC_START=self.sTime, REC_END=self.eTime, /ISO)
        obj_destroy, oCDF
        
        ;Create the key/tag
        field_name = 'FILE' + string(i, FORMAT='(i0' + key_fmt  + ')')
        
    ;---------------------------------------------------------------------
    ; Hash ///////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if to_hash then begin
            switch nIn of
                5: depend_3[field_name] = temporary(temp_3)
                4: depend_2[field_name] = temporary(temp_2)
                3: depend_1[field_name] = temporary(temp_1)
                2: depend_0[field_name] = temporary(temp_0)
                1: data[field_name]     = temporary(temp_data)
            endswitch

    ;---------------------------------------------------------------------
    ; List ///////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if to_list then begin
            switch nIn of
                5: depend_3 -> Add, temporary(temp_3)
                4: depend_2 -> Add, temporary(temp_2)
                3: depend_1 -> Add, temporary(temp_1)
                2: depend_0 -> Add, temporary(temp_0)
                1: data     -> Add, temporary(temp_data)
            endswitch
            
    ;---------------------------------------------------------------------
    ; Structure //////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else if to_structure then begin
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
            
    ;---------------------------------------------------------------------
    ; MrArrays ///////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endelse to_MrArray then begin
            switch nIn of
                5: depend_3 -> Append(temp_3,   0, /NO_COPY)
                4: depend_2 -> Append(temp_2,   0, /NO_COPY)
                3: depend_1 -> Append(temp_1,   0, /NO_COPY)
                2: depend_0 -> Append(temp_0,   0, /NO_COPY)
                1: data     -> Append(tempData, 0, /NO_COPY)
            endswitch
        
        endif else begin
            
    ;---------------------------------------------------------------------
    ; Regular Arrays /////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            switch nIn of
                5: if i eq 0 $
                        then depend_3 = temporary(temp_3) $
                        else depend_3 = MrConcatentate(depend_3, temporary(temp_3), 0)
                4: if i eq 0 $
                        then depend_2 = temporary(temp_2) $
                        else depend_2 = MrConcatentate(depend_2, temporary(temp_2), 0)
                3: if i eq 0 $
                        then depend_1 = temporary(temp_1) $
                        else depend_1 = MrConcatentate(depend_1, temporary(temp_1), 0)
                2: if i eq 0 $
                        then depend_0 = temporary(temp_0) $
                        else depend_0 = MrConcatentate(depend_0, temporary(temp_0), 0)
                1: if i eq 0 $
                        then data = temporary(tempData) $
                        else data = MrConcatentate(data, temporary(tempData), 0)
            endswitch
        endelse
    endfor
    
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
pro MrRead2::SetProperty, $
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
    
    ;Update the file finder object
    self.File_Finder -> SetProperty, ISO_START=self.sTime, ISO_START=self.eTime, $
                                     PATTERN=self.pattern
end


;+
;   Clean up after the object is destroy
;-
pro MrRead2::cleanup
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
function MrRead2::init, pattern, sTime, eTime
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
pro MrRead2__define
    compile_opt strictarr
    
    class = { MrRead2, $
              file_finder: obj_new(), $
              pattern:     '', $
              sTime:       '', $
              eTime:       '' $
            }
end