; docformat = 'rst'
;
; NAME:
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
; PURPOSE
;+
;   The purpose of this class is to provide a set of commonly used data analysis methods.
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG
;       detrend_data
;       fa_system
;       MrFFT__Define
;       MrInterpol
;       replace_fillval
;       rotate_matrix
;       rotate_vector
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
;       2013/11/01  -   Written by Matthew Argall.
;       2013/12/27  -   Added the DetrendRotate method. Moved FFT, PSD, and SamplePeriod
;                           methods into their own class. - MRA
;       2013/12/28  -   Changed data properties from DATA and TIME to Y and X,
;                           respectively. Renamed GetResults to GetData, converted data
;                           keywords to parameters, and removed the PTR keyword. - MRA
;       2013/12/29  -   Added the SetData method. Interpol_TS now accepts a MrDataSet
;                           object as input. Added the Save and Restore methods. Independent
;                           variable now set in Interpol. Added the Plot and Image methods.
;                           Added Z as a data property. - MRA.
;       2014/01/27  -   Changed data properties to DATA, DEPEND_[0-3]. Access data and
;                           save results exclusively through the [GS]etData methods. - MRA
;       2014/01/28  -   Added the _OverloadBracketsRightSide, _OverloadBracketsLeftSide,
;                           _OverloadPrint, _Convert_Bounds, and PSD methods. - MRA
;       2014/04/29  -   Added the Read_CDF and SetTimeRange methods. Removed the
;                           _Convert_Bounds method.
;       2014/05/02  -   File and variable information can be provided to the INIT method.
;                           Data will not be read of the variable or time range have not
;                           been defined yet. _OverloadBRS returns the correct data. - MRA
;-
;*****************************************************************************************
;+
;   Allow square-bracket array indexing from the right side of an operator.
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT1:         in, required, type=string,integer/intarr(3)
;                           Index subscripts into object property. Alternatively, a scalar
;                               string indicating which data array to access. Choices are::
;                                   "DATA"
;                                   "DEPEND_0"
;                                   "DEPEND_1"
;                                   "DEPEND_2"
;                                   "DEPEND_3"
;                               If not a string, "DATA" is assumed.
;       SUBSCRIPT2:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT3:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT4:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT5:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT6:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT7:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT8:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;
; :Keywords:
;       DATASET:            in, optional, private, type=MrArray object
;                           Used internally on a recursive call if `SUBSCRIPT1` is a string.
;
; :Returns:
;       RESULT:             in, required, type=numeric array
;                           The subarray accessed by the input parameters.
;-
function MrDataSet::_OverloadBracketsRightSide, isRange, subscript1, subscript2, $
                                                subscript3, subscript4, subscript5, $
DATASET=dataSet
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif
    
    ;Pick the proper data set to alter
    if IsA(subscript1, 'STRING', /SCALAR) then begin
        case strupcase(subscript1) of
            'DATA':     dataSet = self.data
            'DEPEND_0': dataSet = self.depend_0
            'DEPEND_1': dataSet = self.depend_1
            'DEPEND_2': dataSet = self.depend_2
            'DEPEND_3': dataSet = self.depend_3
            else: message, 'Subscript1 must be "DEPEND_" + 0-3.'
        endcase
        
        ;Call the method on the desired dataset
        if n_elements(isRange) eq 1 then begin
            result = dataset -> GetArray()
        endif else begin
            result = dataset -> _OverloadBracketsRightSide(isRange[1:*], subscript2, $
                                                 subscript3, subscript4, subscript5, $
                                                 subscript6, subscript7, subscript8)
        endelse
        
        return, result
        
    endif else begin
        if n_elements(dataSet) eq 0 || obj_valid(dataSet) eq 0 then dataSet = self.data
    endelse
    
    ;Call the MrArray overload brackets method
    result = self.data -> _OverloadBracketsRightSide(isRange, subscript1, subscript2, $
                                                  subscript3, subscript4, subscript5, $
                                                  subscript6, subscript7, subscript8)
    
    return, result
end


;+
;   Allow square-bracket array indexing from the left side of an operator.
;
; :Params:
;       OBJREF:             in, required, type=ObjRef
;                           The object reference variable that is being indexed (i.e. "self")
;       VALUE:              in, required, type=numeric array
;                           The value specified on the right-hand side of the equal sign.
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT1:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property. Alternatively, a scalar
;                               string indicating which data array to access. Choices are::
;                                   "DATA"
;                                   "DEPEND_0"
;                                   "DEPEND_1"
;                                   "DEPEND_2"
;                                   "DEPEND_3"
;                               If not a string, "DATA" is assumed.
;       SUBSCRIPT2:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT3:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT4:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT5:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT6:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT7:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;       SUBSCRIPT8:         in, required, type=integer/intarr(3)
;                           Index subscripts into object property.
;
; :Keywords:
;       DATASET:            in, optional, private, type=MrArray object
;                           Used internally on a recursive call if `SUBSCRIPT1` is a string.
;-
pro MrDataSet::_OverloadBracketsLeftSide, objRef, value, isRange, subscript1, subscript2, $
                                          subscript3, subscript4, subscript5, subscript6, $
                                          subscript7, subscript8, $
DATASET=dataSet
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Deal with SUBSCRIPT1. If it is a string, pick the appropriate data array to be
    ;accessed and remove the empty 0 element, shifting everything down one entry.
    if IsA(subscript1, 'STRING', /SCALAR) then begin
        case strupcase(subscript1) of
            'DATA':     dataSet = self.data
            'DEPEND_0': dataSet = self.depend_0
            'DEPEND_1': dataSet = self.depend_1
            'DEPEND_2': dataSet = self.depend_2
            'DEPEND_3': dataSet = self.depend_3
            else: message, 'Subscript1 must be "DEPEND_" + 0-3.'
        endcase
        
        ;Call the method again, without the string subscript, and with the proper
        ;array passed in.
        self -> _OverloadBracketsLeftSide, dataSet, value, isRange, $
                                           subscript2, subscript3, subscript4, subscript5, $
                                           subscript6, subscript7, subscript8, DATASET=dataSet
        return
        
    ;Select the data and store the subscripts
    endif else begin
        if n_elements(objRef) eq 0 || obj_valid(objRef) eq 0 then objRef = self.data
    endelse
        
    ;Overload the proper dataSet
    dataSet -> _OverloadBracketsLeftSide, dataSet, value, isRange, subscript1, $
                                          subscript2, subscript3, subscript4, subscript5, $
                                          subscript6, subscript7, subscript8
end


;+
;   The purpose of this method is to provide information when the PRINT procedure
;   is called.
;
; :Params:
;       VARNAME:        in, required, type=string
;                       Name of the variable in which the object reference is stored.
;-
function MrDataSet::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Info about the object
    heapnum = obj_valid(self, /GET_HEAP_IDENTIFIER)
    type    = size(self, /TNAME)
    class   = obj_class(self)
    
    ;Normal help string
    str = string(type, '<ObjHeapVar', heapnum, '(', class, ')>', $
                 FORMAT='(a-12, a11, i0, a1, a0, a2)')

    ;Help about the dat    
    h_data     = '[' + strjoin(strtrim(self.data.dimensions, 2), ', ') + ']'
    h_depend_0 = '[' + strjoin(strtrim(self.depend_0.dimensions, 2), ', ') + ']'
    h_depend_1 = '[' + strjoin(strtrim(self.depend_1.dimensions, 2), ', ') + ']'
    h_depend_2 = '[' + strjoin(strtrim(self.depend_2.dimensions, 2), ', ') + ']'
    h_depend_3 = '[' + strjoin(strtrim(self.depend_3.dimensions, 2), ', ') + ']'

    ;Concatenate all of the help together
    str = [[str], $
           ['   DATA       ' + h_data], $
           ['   DEPEND_0   ' + h_depend_0], $
           ['   DEPEND_1   ' + h_depend_1], $
           ['   DEPEND_2   ' + h_depend_2], $
           ['   DEPEND_3   ' + h_depend_3]]
    
    return, str
end


;+
;   Clear data from the internal arrays.
;-
pro MrDataSet::Clear
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Save the data
    self.data     -> Clear
    self.depend_0 -> Clear
    self.depend_1 -> Clear
    self.depend_2 -> Clear
    self.depend_3 -> Clear
end


;+
;   Remove a boxcar sliding average from the data. This is done using IDL's SMOOTH
;   function with the EDGE_TRUNCATE keyword set by default. In the call to ::Detrend, 
;   set EDGE_TRUNCATE=0 explicitly to turn this off.
;
; :Params:
;       NAVG:               in, required, type=int
;                           The number of points to average when calculating the
;                               background value.
;
; :Keywords:
;       BACKGROUND:         out, optional, type=Size(`DATA`\, /TYPE)
;                           The background values subtracted from `DATA`.
;       EDGE_TRUNCATE:      in, optional, type=boolean, default=1
;                           If set, data will be edge-truncated. See IDL's SMOOTH function
;                               for details.
;       _REF_EXTRA:         in, optional, type=any
;                           Any additional keyword accepted by IDL's Smooth function
;-
pro MrDataSet::Detrend, navg, $
 BACKGROUND = background, $
 EDGE_TRUNCATE = edge_truncate, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get the data
    self -> GetData, data

    ;Return the background values?
    if arg_present(background) $
        then result = detrend_data(temporary(data), navg, BACKGROUND=background, $
                                   DIMENSION=self.dimension, $
                                   EDGE_TRUNCATE=edge_truncate, _EXTRA=extra) $
        else result = detrend_data(temporary(data), navg, DIMENSION=self.dimension, $
                                   EDGE_TRUNCATE=edge_truncate, _EXTRA=extra)
    
    ;Set the result
    self -> SetData, result, /NO_COPY
end


;+
;   The purpose of this program is to detrend a vector field and transform it to a new
;   coordinate system. It is a shortcut for the Detrend and RotateVector methods, which
;   are typically used in conjunction when analyzing, e.g., magnetic field data.
;
; :Params:
;       NDETREND:           in, optional, type=integer, default=0
;                           When detrending, the background field is subtracted from
;                               `FIELD`. Set this keyword equal to a positive integer to
;                               indicate the number of points that should be averaged when
;                               computing the mean, background field.
;       NSYSTEM:            in, optional, type=integer
;                           If set to a positive integer, `FIELD` will be rotated into a
;                               field-aligned coordinate system (FAS) in which the z-axis
;                               is is along mean background field. `NSYSTEM` is the number
;                               of points used to determine the mean background field. If
;                               `NDETREND` is also set, NSYSTEM will be set equal to
;                               `NDETREND`. The x-axis of the FAS is formed by crossing
;                               z_FAS with [0,1,0], the y-axis of the original system.
;                               y_FAS completes the right-handed system.
;       POSITION:           in, optional, type=3xN numeric
;                           The 3-component position of the spacecraft position at each
;                               point in `FIELD`. If provided, `NSYSTEM` will determine
;                               how many points to average when rotating into a field-
;                               aligned radial (FAR) coordinate system. In this system,
;                               z-hat is again along the background field, the azimuthal
;                               direction is formed by crossing the position vector with
;                               z-hat, and the radial component completes the system.
;
; :Keywords:
;       MEAN_FIELD:         out, optional, type=fltarr
;                           The mean field subtracted from `FIELD` when `NDETREND`>0.
;       RMATRIX:            in, out, optional, type=3x3 numeric
;                           If provided, `FIELD` will be transformed into a new coordinate
;                               system using this rotation matrix. If not provided, and
;                               either `FAR` or `FAS` are non-zero, then the transformation
;                               matrix to that system will be returned.
;       _REF_EXTRA:         in, optional, type=any
;                           Any additional keyword accepted by IDL's Smooth function
;
; :Returns:
;       FIELD_OUT:          The result of dentrending and rotating `FIELD`.
;-
pro MrDataSet::DetrendRotate, nDetrend, nSystem, position, $
MEAN_FIELD = mean_field, $
RMATRIX = rMatrix, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Detrend and Rotate?
    if n_elements(nDetrend)  eq 0 then nDetrend  = 0
    if n_elements(nSystem)   eq 0 then nSystem   = 0
    if (nSystem ne 0) and (nDetrend ne 0) then nSystem = nDetrend
    
    ;If NSYSTEM was given
    fas = 0
    far = 0
    if (nSystem gt 0) then begin
        ;If POSITION was given, transform to the FAR system. Otherwise, the FAS system.
        if n_elements(position) eq 0 then fas = 1 else far = 1
    endif
    
    ;Make sure only one system is provided.
    if (nSystem gt 0) and n_elements(rMatrix) ne 0 then $
        message, 'NSYSTEM and RMATRIX are mutually exclusive.'

;-----------------------------------------------------
;Detrend the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    if nDetrend gt 0 then $
        self -> Detrend, nDetrend, DIMENSION=self.dimension, BACKGROUND=mean_field, _EXTRA=extra

;-----------------------------------------------------
;Field-Aligned System? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Rotate to field-aligned system?
    if nSystem gt 0 and fas eq 1 then begin

        ;Has the data been detrended already?
        if nDetrend eq 0 $
            then rMatrix_out = self -> fa_system(nFAS) $
            else rMatrix_out = self -> fa_system(nFAS, MEAN_FIELD=mean_field)

        ;Rotate into the FAR frame
        self -> RotateVector, rMatrix_out
    endif

;-----------------------------------------------------
;Field-Aligned, Radial System? \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Rotate to field-aligned system?
    if nSystem gt 0 and far eq 1 then begin

        ;Has the data been detrended already?
        if nDetrend eq 0 $
            then rMatrix_out = self -> far_system(position, nFAR) $
            else rMatrix_out = self -> far_system(position, nFAR, MEAN_FIELD=mean_field)

        ;Rotate into the FAR frame
        self -> RotateVector, rMatrix_out
    endif

;-----------------------------------------------------
;Other Coordinate System? \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Rotate into a different coordinate system? Return the rotation matrix?
    if n_elements(rMatrix) gt 0 $
        then self -> RotateVector, rMatrix $
        else if arg_present(rMatrix) and n_elements(rMatrix_out) gt 0 then rMatrix = rMatrix_out
end


;+
;   The purpose of this method is to caluclate the average field over an
;   interval and form a coordinate system that has its z-axis pointing along the
;   average field. Data must be 3xN or Nx3.
;
; :Params:
;       NAVG:               in, optional, type=int/float
;                           The number of points to average when finding the background
;                               field strength. Required if `AVG_DATA` is not provided.
;                               The data must be a 3xN array.
;
; :Keywords:
;       MEAN_FIELD:         in, out, optional, type=same as `DATA`
;                           Either a vector specifying the mean field at each point, or a
;                               named variable into which the mean field will be returned.
;                               In the former case, ISMEAN is set to 1 automatically.
;       ISMEAN:             in, optional, type=boolean
;                           If set, then object's data field is the average, background
;                               field. In this case, no averaging will be performed.
;       EDGE_TRUNCATE:      in, optional, type=boolean, default=1
;                           If set, data will be edge-truncated. See IDL's SMOOTH function
;                               for details.
;       _REF_EXTRA:         in, optional, type=any
;                           Any additional keyword accepted by IDL's Smooth function
;-
function MrDataSet::FA_System, nFAS, $
MEAN_FIELD=mean_field, $
EDGE_TRUNCATE=edge_truncate, $
ISMEAN=isMean, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif
    
    ;Get the data
    if n_elements(mean_field) gt 0 then begin
        data = mean_field
        isMean = 1
    endif else begin
        self -> GetData, data
        isMean = keyword_set(isMean)
    endelse
    
    ;Create the rotation matrix.
    if ~isMean and arg_present(mean_field) $
        then faSys = fa_system(*data, nFAS, AVG_DATA=mean_field, EDGE_TRUNCATE=edge_truncate, $
                               ISMEAN=isMean, _EXTRA=extra) $
        else faSys = fa_system(*data, nFAS, EDGE_TRUNCATE=edge_truncate, ISMEAN=isMean, $
                               _EXTRA=extra)
    
    return, faSys
end


;+
;       The purpose of this method is to create a coordinate system with the z-axis
;       directed along the background magnetic field. The y-axis is found by crossing
;       the radial vector from the center of the earth to the spacecraft's location into
;       the mean field direction and negating it. In this manner, y points roughly
;       duskward on account of z pointing southward. The third component is found by
;       taking the cross product of the first two.
;
; :Params:
;       POSITION:           in, required, type=3xN numeric
;                           The three components of the position vector, measured from
;                               the center of the earth to the spacecraft, at each 
;                               measurement of `B_FIELD`.
;       NAVG:               in, optional, type=int/float
;                           If `B_FIELD` is not the average magnetic field (as indicated
;                               by the `B_AVG` keyword), then the background field will
;                               be calculated using a boxcar average of NAVG points.
;
; :Keywords:
;       ISMEAN:             in, optional, type=boolean
;                           If set, then object's data field is the average, background
;                               field. In this case, no averaging will be performed.
;       MEAN_FIELD:         in, out, optional, type=same as `DATA`
;                           Either a vector specifying the mean field at each point, or a
;                               named variable into which the mean field will be returned.
;                               In the former case, ISMEAN is set to 1 automatically.
;-
function MrDataSet::FAR_System, position, navg, $
MEAN_FIELD = mean_field, $
ISMEAN = isMean
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif
    
    ;Get the data
    if n_elements(mean_field) gt 0 then begin
        data = mean_field
        isMean = 1
    endif else begin
        self -> GetData, data
        isMean = keyword_set(isMean)
    endelse
    
    ;Interpolate over the fill values or just replace them?
    if ~isMean and arg_present(mean_field) $
        then farSys = far_system(data, position, navg, B_AVG=mean_field) $
        else farSys = far_system(data, position, navg, ISMEAN=isMean)
    
    return, farSys
end


;+
;   Get class properties.
;
; :Keywords:
;-
pro MrDataSet::GetProperty, $
DIMENSION=dimension, $
FILENAME=filename, $
FIRST_READ=first_read, $
LAST_READ=last_read, $
STIME=sTime, $
ETIME=eTime
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    if arg_present(dimension)  then dimension  = self.dimension
    if arg_present(first_read) then first_read = self.first_read
    if arg_present(last_read)  then last_read  = self.last_read
    if arg_present(filename)   then self.FileFinder -> GetProperty, PATTERN=filename
    if arg_present(sTime)      then self.FileFinder -> GetProperty, ISO_START=sTime
    if arg_present(eTime)      then self.FileFinder -> GetProperty, ISO_END=eTime
end


;+
;   Get the results of data analysis. If there are no results, the untouched input
;   data will be returned.
;
; :Keywords:
;       X:                  out, optional, type=string
;                           Independent variable data to be analyzed.
;       Y:                  out, optional, type=string
;                           Dependent variable data (associated with `X`) to be analyzed.
;-
pro MrDataSet::GetData, arg0, arg1, $
DEPEND_1=depend_1, $
DEPEND_2=depend_2, $
DEPEND_3=depend_3
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get data
    case n_params() of
        1: arg0 = self.data[*]
        2: begin
            arg0 = self.depend_0[*]
            arg1 = self.data[*]
        endcase
        else: ;Do nothing
    endcase
    
    ;Get Dependent Data
    if arg_present(depend_1) then depend_1 = self.depend_1[*]
    if arg_present(depend_2) then depend_2 = self.depend_2[*]
    if arg_present(depend_3) then depend_3 = self.depend_3[*]
end


;+
;   Make an image of the data.
;
;   Names of the display window and images are::
;       Display window          -   'DA Window'
;       Images                  -   'DA Image#'
;       Colorbars               -   'CB: DA Image#'
;
;   In the above, "#" stands for the index of DIMENSION corresponding to the image.
;
; :Keywords:
;       CURRENT:        in, optional, type=boolean, default=0
;                       If set, put the plots into the current MrWindow graphics window.
;                           The default is to create a new window.
;       SEPARATE:       in, optional, type=boolean, default=1
;                       Set this flag to 0 if the Y data property is a 3D data image. The
;                           third dimension will be treated as a flipbook. The default is
;                           to display each element of the third dimension separately.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrImage is also accepted for keyword
;                           inheritance.
;
;   :Returns:
;       DAWIN:          An object refrence to the MrWindow gaphics windows containing
;                           the plots.
;-
function MrDataSet::Image, $
CURRENT=current, $
SEPARATE=separate, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if current eq 0 then if obj_valid(daWin) then obj_destroy, daWin
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    ;Set Defaults
    current = keyword_set(current)
    if n_elements(separate) eq 0 then separate = 1 else separate = keyword_set(separate)
    
    ;Get the data
    self -> GetData, depend_0, data, DEPEND_1=depend_1
    
    ;Create a window to put the plots in.
    refresh_in = 0
    if current then begin
        daWin = GetMrWindows(/CURRENT)
        refresh_in = daWin.refresh_in
        daWin -> Refresh, /DISABLE
    endif else daWin = Mrwindow(XSIZE=500, YSIZE=500, REFRESH=0, NAME='DA Window')
    
    ;Is there data for the vertical axiz?
    nDep1 = n_elements(depend_1)
    
    ;Create images for each page?
    if separate then begin        
        ;Plot each page.
        for i = 0, nPlots-1 do begin
            index = string(i, FORMAT='(i0)')
            
            ;Create the image
            if nDep1 gt 0 $
                then imTmp = MrImage(y[*,*,i], depend_0, depend_1, /CURRENT, NAME='DA Image' + index, _EXTRA=extra) $
                else imTmp = MrImage(y[*,*,i], depend_0, /CURRENT, NAME='DA Image' + index, _EXTRA=extra)
                
            ;Create a colorbar
            !Null = MrColorbar(/CURRENT, TARGET=tempImage)
        endfor
    
    ;Display a flipbook?
    endif else begin
        tempImage = MrImage(data, depend_0, depend_1, /CURRENT, NAME='DA Image', _EXTRA=extra)
        !Null = MrColorbar(/CURRENT, TARGET=tempImage)
    endelse
    
    ;Return the window
    daWin -> Refresh, DISABLE=~refresh_in
    return, daWin
end


;+
;   The purpose of this method is to interpolate two sets of data without having to
;   determine beforehand which is sampled less frequency. The data with the smaller gap
;   between elements of its independent variable will interpolated. Data with the larger
;   gap between elements will remain untouched.
;
; :Params:
;       DA_OBJECT:      in, required, type=object
;                       A 'DataAnalysis' object containing the data will be considered
;                           for interpolation.
;
; :Keywords:
;       DX_OUT:         out, optional, type=any
;                       Spacing between elements of the independent variable, after
;                           interpolation.
;       SUCCESS:        out, optional, type=byte
;                       A flag for indicating if interpolation was successful::
;                           0 - Not successful
;                           1 - Successful and the calling object's data was interpolated.
;                           2 - Successful and DA_OBJECT's data was interpolated.
;       X_OUT:          out, optional, type=any
;                       Independent variable data with the largest spacing between points.
;-
pro MrDataSet::Interp_TS, da_object, $
DX_OUT=dx_out, $
SUCCESS=success, $
X_OUT=x_out
    compile_opt strictarr
        
;---------------------------------------------------------------------
;Error Handling //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    success = 0B
        
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    self      -> GetData, x1, !Null
    da_object -> GetData, x2, !Null
            
;---------------------------------------------------------------------
;Sampling Rates //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Take the average time between samples
    dx1 = self -> SamplePeriod(x1)
    dx2 = self -> SamplePeriod(x2)
    
;---------------------------------------------------------------------
;Interpolate /////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Interpolate to the lowest sample period.
    if dx2 gt dx1 then begin
        self -> Interpol, x2
        if arg_present(x_out) then x_out = x2
        if arg_present(dx_out) then dx_out = dt2
        success = 1B
    
    ;Interpolate Y2 to the time stamps of A
    endif else begin
        da_object -> Interpol, x1
        if arg_present(x_out) then x_out = x1
        if arg_present(dx_out) then dx_out = dx1
        success = 2B
    endelse
end


;+
;   The purpose of this method is to generalize the INTERPOL procedure to 2D arrays.
;   Each column of data will be interpolated indepentently at points `Xout`::
;           A[1xM] and X[M] then use `interpol`
;           A[3xM] and X[M] then use `interpol` 3 times
;           A[Mx3] and X[M] then use `interpol` 3 times
;           A[NxM] and X[M] then loop over each A[i,*], interpolating each column
;           A[MxN] and X[M] then loop over each A[*,i], interpolating each row
;
; :Params:
;       Xout:           in, required, type=Numeric array
;                       The new abcissa values.
;-
pro MrDataSet::Interpol, Xout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get the data
    self -> GetData, depend_0, data
    
    ;Interpolate
    Yout = MrInterpol(data, depend_0, Xout)
    
    ;Save results.
    self -> SetData, Xout, temporary(Yout)
end


;+
;   Make a plot of the data.
;
; :Keywords:
;       CURRENT:        in, optional, type=boolean, default=0
;                       If set, put the plots into the current MrWindow graphics window.
;                           The default is to create a new window.
;       SEPARATE:       in, optional, type=boolean, default=0
;                       If set, each element in the dimension specified by the DIMENSION
;                           property will be plotted in its own set of axes. The default
;                           is to overplot them in the same set of axes.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrPlot is also accepted for keyword
;                           inheritance.
;
;   :Returns:
;       DAWIN:          An object refrence to the MrWindow gaphics windows containing
;                           the plots.
;-
function MrDataSet::Plot, $
CURRENT=current, $
SEPARATE=separate, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if current eq 0 then if obj_valid(daWin) then obj_destroy, daWin
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    ;Set Defaults
    current = keyword_set(current)
    separate = keyword_set(separate)
    
    ;Get the data
    self -> GetData, depend_0, data
    
    ;Create a window to put the plots in.
    refresh_in = 0
    if current then begin
        daWin = GetMrWindows(/CURRENT)
        refresh_in = daWin.refresh
        daWin -> Refresh, /DISABLE
    endif else daWin = Mrwindow(XSIZE=500, YSIZE=500, REFRESH=0, NAME='DA Window')
    
    ;Plot each element of DIMENSION separately?
    if separate then begin        
        dims = size(data, /DIMENSIONS)
        nPlots = dims[self.dimension-1]
        
        ;Plot each component.
        for i = 0, nPlots-1 do !Null = MrPlot(depend_0, data, /CURRENT, NAME='DA Plot' + strtrim(i,2), _EXTRA=extra)
    
    ;Plot them in the same axes?
    endif else begin
        !Null = MrPlot(depend_0, data, /CURRENT, DIMENSION=self.dimension, NAME='DA Plot', _EXTRA=extra)
    endelse
    
    ;Return the window
    daWin -> Refresh, DISABLE=~refresh_in
    return, daWin
end


;+
;   The purpose of this method is to calculate the power spectral density.
;
; :Params:
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       NSHIFT:             in, optional, type=int. default=0
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;       DIMENSION:          in, optional, type=int, default=longest dimension of `DATA`
;                           The dimension over which to take the PSD. As an example, say
;                               ARRAY is a [3,10] element array. PSD(ARRAY, DIMENSION=2),
;                               is the same as
;                               [PSD(Array[0,*]), PSD(Array[1,*]), PSD(Array[2,*])].
;       FREQUENCIES:        out, type=fltarr, type=fltarr(NFFT)
;                           The frequency bins of the spectrogram
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the MrFFT::FFT method is also accepted
;                               via keyword inheritance.
;-
pro MrDataSet::PSD, nfft, nshift, $
 DIMENSION = dimension, $
 TIME = time, $
 FREQUENCIES = frequencies, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(oFFT) then obj_destroy, oFFT
        void = cgErrorMsg()
        return
    endif
    
    ;Get the data and sampling period
    self -> GetData, depend_0, data
    dt = self -> SamplePeriod()

    ;Create a MrFFT object
    oFFT = obj_new('MrFFT')

    ;Take the FFT
    thePSD = oFFT -> PSD(temporary(data), nfft, dt, nshift, $
                         DIMENSION=dimension, $
                         FREQUENCIES=frequencies, $
                         TIME=time, $
                         T0=depend_0[0], $
                        _STRICT_EXTRA=extra)
    obj_destroy, oFFT
    if thePSD eq !Null then return
    
    ;Set the data
    self -> SetData, time, thePSD, DEPEND_1=frequencies, /NO_COPY
end


;+
;   Read a CDF file.
;
;   This method should not be called directly. Instead, use the SetVariable and
;   SetTimeRange methods.
;
; :Private:
;
; :Params:
;       VNAME:          in, required, type=string
;                       The variable name within the CDF file whose data is to be read.
;-
pro MrDataSet::Read_CDF, $
BEFORE=before
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(oCDF) then obj_destroy, oCDF
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;Inputs //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    data     = keyword_set(data)
    depend_0 = keyword_set(depend_0)
    depend_1 = keyword_set(depend_1)
    depend_2 = keyword_Set(depend_2)
    depend_3 = keyword_set(depend_3)
    if data + depend_0 + depend_1 + depend_2 + depend_3 eq 0 then depend_3 = 1
    
    ;Get the names of the files to read.
    filenames = self.FileFinder -> GetFiles(NFILES=nFiles)
    if nFiles eq 0 then return
    
    ;Variable to read
    if self.variable eq '' then return
    
    ;Time interval to read
    self.FileFinder -> GetProperty, ISO_START=sTime, ISO_END=eTime
    if sTime eq '' || eTime eq '' then return
    
    ;Setup to append.
    switch 1 of
        depend_3: self.depend_3 -> Append, 1, /START, BEFORE=before
        depend_2: self.depend_2 -> Append, 1, /START, BEFORE=before
        depend_1: self.depend_1 -> Append, 1, /START, BEFORE=before
        depend_0: self.depend_0 -> Append, 1, /START, BEFORE=before
        data:     self.data     -> Append, 1, /START, BEFORE=before
    endswitch

;---------------------------------------------------------------------
;Read Data ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    for i = 0, nFiles - 1 do begin
        oCDF = obj_new('CDF_File', filenames[i])

        ;Read data from the file.
        switch 1 of
            depend_3: temp_data = oCDF -> Read(self.variable, temp_0, temp_1, temp_2, temp_3, $
                                               REC_START=sTime, REC_END=eTime, $
                                               PATTERN=self.time_pattern)
            depend_2: temp_data = oCDF -> Read(self.variable, temp_0, temp_1, temp_2, $
                                               REC_START=sTime, REC_END=eTime, $
                                               PATTERN=self.time_pattern)
            depend_1: temp_data = oCDF -> Read(self.variable, temp_0, temp_1, $
                                               REC_START=sTime, REC_END=eTime, $
                                               PATTERN=self.time_pattern)
            depend_0: temp_data = oCDF -> Read(self.variable, temp_0, $
                                               REC_START=sTime, REC_END=eTime, $
                                               PATTERN=self.time_pattern)
            data:     temp_data = oCDF -> Read(self.variable, $
                                               REC_START=sTime, REC_END=eTime, $
                                               PATTERN=self.time_pattern)
        endswitch
        obj_destroy, oCDF
        
        ;Append the data.
        if n_elements(temp_3) gt 0 then self.depend_3 -> Append, temp_3, /NO_COPY
        if n_elements(temp_2) gt 0 then self.depend_2 -> Append, temp_2, /NO_COPY
        if n_elements(temp_1) gt 0 then self.depend_1 -> Append, temp_1, /NO_COPY
        if n_elements(temp_0) gt 0 then self.depend_0 -> Append, temp_0, /NO_COPY
        self.data -> Append, temp_data, /NO_COPY
    endfor
    
    ;Finalize apend.
    switch 1 of
        depend_3: self.depend_3 -> Append, /FINISH
        depend_2: self.depend_2 -> Append, /FINISH
        depend_1: self.depend_1 -> Append, /FINISH
        depend_0: self.depend_0 -> Append, /FINISH
        data:     self.data     -> Append, /FINISH
    endswitch

    ;Store first and last times read.
    self.first_read = MrCDF_Epoch_Encode(self['DEPEND_0',  0], PATTERN=self.time_pattern)
    self.last_read  = MrCDF_Epoch_Encode(self['DEPEND_0', -1], PATTERN=self.time_pattern)
end


;+
; :Params:
;       FILLVAL:            in, required, type=same as data
;                           The value within the data to be replaced.
;
; :Keywords:
;       INTERP:             in, optional, type=boolean, default=0
;                           Interpolate over the fill values, not just replace them.
;       REPLACE_VALUE:      in, optional, type=same as data, default=!values.f_nan
;                           The value that should replace `FILLVAL`
;       _REF_EXTRA:         in, optional, type=any
;                           If `INTERP` is set, then any keyword accepted by IDL's INTERPOL
;                               procedure is also accepted for keyword inheritance.
;-
pro MrDataSet::ReplaceFillVal, fill_value, $
 INTERP = interp, $
 REPLACE_VALUE = replace_value, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Interpolate?
    interp = keyword_set(interp)
    
    ;Get the data
    self -> GetData, depend_0, data
    
    ;Interpolate over the fill values or just replace them?
    if interp $
        then result = replace_fillval(data, fill_value, depend_0, _EXTRA=extra) $
        else result = replace_fillval(data, fill_value, REPLACE_VALUE=replace_value)
    
    ;Set the results
    self -> SetData, result, /NO_COPY
end


;+
;   The purpose of this method is to reset the output data so that analysis
;   starts afreshs.
;-
pro MrDataSet::Reset
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Transfer the initial data to the results.
    self.data     = self.data_init[*]
    self.depend_0 = self.depend_0_init[*]
    self.depend_1 = self.depend_1_init[*]
    self.depend_2 = self.depend_2_init[*]
    self.depend_3 = self.depend_3_init[*]
end


;+
;   Restore data from a previous call to the Save method. Current data will be lost.
;
; :Keywords:
;       CLEAR:              in, optional, type=boolean, default=0
;                           If set, saved data will be cleared from memory after it is
;                               restored.
;-
pro MrDataSet::Restore, $
CLEAR=clear
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Restore the saved data.
    self.data     -> SetArray, self.data_save
    self.depend_0 -> SetArray, self.depend_0_save
    self.depend_1 -> SetArray, self.depend_1_save
    self.depend_2 -> SetArray, self.depend_2_save
    self.depend_3 -> SetArray, self.depend_3_save
    
    ;Clear the check-point
    if keyword_set(clear) then begin
        data_save     -> Clear
        depend_0_save -> Clear
        depend_1_save -> Clear
        depend_2_save -> Clear
        depend_3_save -> Clear
    endif
end


;+
;   The purpose of this method is to rotate time-series matrices. Data must be a 3x3xN
;   or an Nx3x3 array.
;
; :Params:
;       RMATRIX:        in, required, type=Nx3x3/3x3xN numeric
;                       A set of matrices by which to rotate the data. Data must also
;                           be a set of matrices.
;-
pro MrDataSet::RotateMatrix, RMatrix
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Get the data
    self -> GetData, data

    ;Rotate the data
    result = rotate_matrix(RMatrix, data)
    
    ;Save the result
    self -> SetData, result, /NO_COPY
end


;+
;   The purpose of this method is to rotate time-series vectors. The data must be a 3xN
;   or Nx3 array.
;
; :Params:
;       RMATRIX:        in, required, type=scalar/Nx3x3/3x3xN numeric
;                       If a scalar number, then it must be used in conjunction with
;                           `ALPHA`, `BETA`, or `GAMMA` and represents the angle about
;                           the x-, y-, or z- axis, respectively, by which to rotate the
;                           data. If it is a 3x3 numeric array, then it is the rotation
;                           matrix used to transform `X` into a new coordinate system.
;                           If 3x3xN or Nx3x3, then there must be a single rotation matrix
;                           for each point in the data.
;
; :Keywords:
;       ALPHA:          in, optional, type=Boolean, default=0
;                       Indicate that `RMATRIX` represents the counter-clockwise angle of
;                           rotation about the x-axis by which to rotate the data.
;       BETA:           in, optional, type=Boolean, default=0
;                       Indicate that `RMATRIX` represents the counter-clockwise angle of
;                           rotation about the x-axis by which to rotate the data.
;       GAMMA:          in, optional, type=Boolean, default=0
;                       Indicate that `RMATRIX` represents the counter-clockwise angle of
;                           rotation about the x-axis by which to rotate the data.
;       EULER:          in, optional, type=Boolean, default=0
;                       If set then `ALPHA`, `BETA` and `GAMMA` represent the traditional
;                           Euler angles and that the rotation is though of as rotating
;                           the coordinate system, not the vectors.
;-
pro MrDataSet::RotateVector, RMatrix, $
ALPHA = alpha, $
BETA = beta, $
GAMMA = gamma, $
EULER = euler
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get data.
    self -> GetData, data
    
    ;Rotate the data
    result = rotate_vector(RMatrix, data, ALPHA=alpha, BETA=beta, GAMMA=gamma, EULER=euler)
    
    ;Save the result
    self -> SetData, result, /NO_COPY
end


;+
;   The purpose of this method is to calculate the time between samples.
;
; :Params:
;       TIME:       in, required, type=any
;                   Time for which the sampling period is to be calculated.
;
; :Keywords:
;       MODE:       in, optional, type=boolean, default=0
;                   If set, the sample period will be calulated using the mode
;                       of the time between samples. Normally, the mean is used.
;
; :Returns:
;       DT:         Time between samples   
;-
function MrDataSet::SamplePeriod, time, $
MODE=mode
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif

    mode = keyword_set(mode)
    if n_elements(time) eq 0 then self -> GetData, time, !Null
    
    ;Use the mode.
    if mode then begin
        npts = n_elements(time)
        dt = MrMode(time[1:npts-1] - time[0:npts-2])
    
    ;Use the average time between samples.
    endif else begin

        ;Calculate the mean different between points.
        dt = moment(time[1:-1] - time[0:-2], MAXMOMENT=1, SDEV=sdev, _STRICT_EXTRA=extra)
        dt = dt[0]
    
        ;Make sure the standard deviation is not of the same order as dt.
        if sdev ge 0.1*dt then $
            message, 'Standard deviation is > 10% of the sample period.', /INFORMATIONAL
    endelse
    
    return, dt
end


;+
;   The purpose of this method is to create a check-point for the data. Once saved, it
;   can be restored from this checkpoint with the Restore method. A second save will
;   over-write any existing saved data. 
;-
pro MrDataSet::Save
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Save the data
    self.data_save     -> SetArray, self.data
    self.depend_0_save -> SetArray, self.depend_0
    self.depend_1_save -> SetArray, self.depend_1
    self.depend_2_save -> SetArray, self.depend_2
    self.depend_3_save -> SetArray, self.depend_3
end


;+
;   Set the time range of the data interval. If the time range increases, more data will
;   be read. If the time range decreases, data will be truncated. If the time range does
;   not overlap with the current time interval, data will be replaced.
;
; :Params:
;       STIME:          in, optional, type=string, default=''
;                       Time at which the data interval should begin. If the empty string
;                           is given, the current interval's start time is used. The
;                           string is assumbed to be formatted as YYYY-MM-DDTHH:mm:SS.d,
;                           where "d" can be any number of decimal places.
;       ETIME:          in, optional, type=string, default=''
;                       Time at which the data interval should end. If the empty string
;                           is given, the current interval's end time is used. The
;                           string is assumbed to be formatted as YYYY-MM-DDTHH:mm:SS.d,
;                           where "d" can be any number of decimal places.
;
; :Keywords:
;       PATTERN:        in, optional, type=string, default='%Y-%M-%dT%H:%m:$S%f'
;                       If `STIME` and `ETIME` do not match the anticipated format, then
;                           a formatting pattern can be supplied. See MrTimeTokensToRegex.pro
;                           for details.
;-
pro MrDataSet::SetTimeRange, sTime, eTime, $
PATTERN=pattern
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
;---------------------------------------------------------------------
; Check Inputs ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Default to an ISO-1806 time with an unknown number of decimal points
    _sTime = n_elements(sTime) eq 0 ? '' : sTime
    _eTime = n_elements(eTime) eq 0 ? '' : eTime
    if n_elements(pattern) eq 0 then pattern = '%Y-%M-%dT%H:%m:%S%f'
    
    ;Use the first and last times read?
    if _sTime eq '' then _sTime = self.first_read
    if _eTime eq '' then _eTime = self.last_read
    
    ;Convert inputs to the desired format
    if pattern ne self.time_pattern then begin
        MrTimeParser, _sTime, pattern, self.time_pattern, _sTime
        MrTimeParser, _eTime, pattern, self.time_pattern, _eTime
    endif
    
    ;Has a variable been defined?
    ;   If not, simply set the time range and return.
    if self.variable eq '' then begin
        self.FileFinder -> SetProperty, ISO_START=_sTime, ISO_END=_eTime
        return
    endif
    
;---------------------------------------------------------------------
; Intervals to Read //////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Figure out the epoch type of the current data
    epoch_type = MrCDF_Epoch_Type(self['DEPEND_0', 0])

    ;Convert to desired times to epochs for comparison
    new_start = MrCDF_Epoch_Parse(_sTime, PATTERN=self.time_pattern, EPOCH_TYPE=epoch_type)
    new_stop  = MrCDF_Epoch_Parse(_eTime, PATTERN=self.time_pattern, EPOCH_TYPE=epoch_type)
    old_start = MrCDF_Epoch_Parse(self.first_read, PATTERN=self.time_pattern, EPOCH_TYPE=epoch_type)
    old_stop  = MrCDF_Epoch_Parse(self.last_read,  PATTERN=self.time_pattern, EPOCH_TYPE=epoch_type)
    
    ;Compare the beginning and end of the new interval with those of the old interval.
    before = MrCDF_Epoch_Compare(new_start, old_start)
    after  = MrCDF_Epoch_Compare(new_stop,  old_stop)
    
;---------------------------------------------------------------------
; Read New Interval //////////////////////////////////////////////////
;---------------------------------------------------------------------
    if ((before eq -1) && (MrCDF_Epoch_Compare(new_stop,  old_start) eq -1)) || $
       ((after  eq  1) && (MrCDF_Epoch_Compare(new_start, old_stop)  eq  1)) $
    then begin
        ;Clear the data
        self -> Clear
        
        ;Set the time interval
        self.FileFinder -> SetProperty, ISO_START=_sTime, ISO_STOP=_eTime
    
        ;Read the data
        self -> Read_CDF
    
;---------------------------------------------------------------------
; Extend/Contract Current Interval ///////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
    
        ;Read or trim before?
        case before of
            -1: read_before = [_sTime, self.first_read]
             0: ;Do nothing
             1: trim_before = [self.first_read, _sTime]
        endcase
    
        ;Read or trim after?
        case after of
            -1: read_after = [self.last_read, _eTime]
             0: ;Do nothing
             1: trim_after = [_eTime, self.last_read]
        endcase
    
        ;Read before
        if n_elements(read_before) gt 0 then begin
            ;Set the time range and read the data
            self.FileFinder -> SetProperty, ISO_START=read_before[0], ISO_END=read_before[1]
            self -> Read_CDF, /BEFORE
        endif
    
        ;Read after
        if n_elements(read_after) gt 0 then begin
            ;Set the time range and read the data
            self.FileFinder -> SetProperty, ISO_START=read_after[0], ISO_END=read_after[1]
            self -> Read_CDF
        endif
    
        ;Trim before
        if n_elements(trim_before) then begin
            ;Get the time data and compare it to the trimmed interval
            t_epoch = self.depend_0 -> GetArray(/NO_COPY)
            epoch_cmp = MrCDF_Epoch_Compare(t_epoch, trim_before[0], trim_before[1])
            
            ;Pick data outside of the trim range
            iKeep = where(epoch_cmp eq 0, nKeep)
            if nKeep eq 0 then message, 'No data to trim. Re-specify new data interval.'
            
            ;Set the data
            self.depend_0 -> SetData, t_epoch[iKeep]
            self.data     -> SetData, self.data[*,iKeep]
        endif
    
        ;Trim after
        if n_elements(trim_after) then begin
            ;Get the time data and compare it to the trimmed interval
            t_epoch = self.depend_0 -> GetArray(/NO_COPY)
            epoch_cmp = MrCDF_Epoch_Compare(t_epoch, trim_after[0], trim_after[1])
            
            ;Pick data outside of the trim range
            iKeep = where(epoch_cmp eq 0, nKeep)
            if nKeep eq 0 then message, 'No data to trim. Re-specify new data interval.'
            
            ;Set the data
            self.depend_0 -> SetData, t_epoch[iKeep]
            self.data     -> SetData, self.data[*,iKeep]
        endif
    endelse
end


;+
;   Set the name of the variable whose name is to be read. Changing variable names clears
;   the current data and reads data associated with the new variable.
;
; :Params:
;       VNAME:          in, required, type=string
;                       Name of the variable whose data is to be read.
;-
pro MrDataSet::SetVariable, vname
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Same variable given
    if vname eq self.variable then return
    
    ;Clear the data
    self -> Clear
    
    ;Read data for the new variable
    self.variable = vname

    ;Read the data        
    self -> Read_CDF
end


;+
;   Get the results of data analysis. If there are no results, the untouched input
;   data will be returned.
;
; :Keywords:
;       X:          out, optional, type=string
;                   Independent variable data to be analyzed.
;       Y:          out, optional, type=string
;                   Dependent variable data (associated with `X`) to be analyzed.
;       Z:          out, optional, type=string
;                   Placeholder for tertiary data. As an example of when to use Z, consider
;                       `X` and `Y` initially as a time-series waveform. After calculating
;                       the power spectra, `Y` becomes 2D, dependent on `X` (time), and a
;                       second variable (frequency). In this example, Z could be used to
;                       hold the frequency data. Calling the Image method, then, would
;                       produce accurate axes around the image.
;-
pro MrDataSet::SetData, arg0, arg1, $
DEPEND_1=depend_1, $
DEPEND_2=depend_2, $
DEPEND_3=depend_3, $
NO_COPY=no_copy
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    case n_params() of
        1: self.data -> SetProperty, ARRAY=arg0, NO_COPY=no_copy
        
        2: begin
            self.data     -> SetProperty, ARRAY=arg1, NO_COPY=no_copy
            self.depend_0 -> SetProperty, ARRAY=arg0, NO_COPY=no_copy
        endcase
    endcase
    
    ;DEPEND_3
    if n_elements(depend_3) gt 0 $
        then self.depend_3 -> SetProperty, ARRAY=depend_3, NO_COPY=no_copy
    
    ;DEPEND_2
    if n_elements(depend_2) gt 0 $
        then self.depend_2 -> SetProperty, ARRAY=depend_2, NO_COPY=no_copy

    ;DEPEND_1
    if n_elements(depend_1) gt 0 $
        then self.depend_1 -> SetProperty, ARRAY=depend_1, NO_COPY=no_copy
end


;+
;   Set class properties.
;
; :Keywords:
;       DIMENSION:          in, optional, type=int
;                           The dimension of `DATA` over which analysis operations are to act.
;       FILENAME:           in, optional, type=string
;                           Name of the file to be read. Instead of a file name, it can
;                               be a file pattern containing any token known to
;                               MrTimeTokensToRegex.pro. Directories and file names will
;                               be checked against the file pattern for matches. If give,
;                               the variable and time interval will be reset.
;-
pro MrDataSet::SetProperty, $
DIMENSION = dimension, $
FILENAME = filename
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    if n_elements(dimension) gt 0 then self.dimension = dimension
    
    ;If the file pattern is set, reset everything
    if n_elements(filename) gt 0 then begin
        self.FileFinder -> SetProperty, ISO_START='', ISO_END='', PATTERN=filename
        self.variable = ''
    endif
end


;+
;   Clean up after the object is destroyed
;-
pro MrDataSet::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Destroy objects
    obj_destroy, self.data
    obj_destroy, self.data_init
    obj_destroy, self.data_save
    obj_destroy, self.depend_0
    obj_destroy, self.depend_0_init
    obj_destroy, self.depend_0_save
    obj_destroy, self.depend_1
    obj_destroy, self.depend_1_init
    obj_destroy, self.depend_1_save
    obj_destroy, self.depend_2
    obj_destroy, self.depend_2_init
    obj_destroy, self.depend_2_save
    obj_destroy, self.depend_3
    obj_destroy, self.depend_3_init
    obj_destroy, self.depend_3_save
    
    obj_destroy, self.FileFinder
end


;+
;   The initialization method.
;
; :Params:
;       DATA:               in, required, type=string
;                           The data to be analyzed.
;       TIME:               in, required, type=string
;                           Time associated with `DATA`.
;
; :Keywords:
;       DIMENSION:          in, optional, type=int, default=biggest dimension
;                           The dimension of `DATA` over which analysis operations are to act.
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, `DATA` and `TIME` will be copied directly into the
;                               object and will become undefined.
;-
function MrDataSet::init, arg0, arg1, $
DEPEND_1=depend_1, $
DEPEND_2=depend_2, $
DEPEND_3=depend_3, $
DIMENSION=dimension, $
ETIME=eTime, $
FILENAME=filename, $
NO_COPY=no_copy, $
NO_REVERT=no_revert, $
STIME=sTime, $
TIME_PATTERN=time_pattern, $
VARIABLE=variable
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Create MrArray objects
    self.data          = MrArray()
    self.data_init     = MrArray()
    self.data_save     = MrArray()
    self.depend_0      = MrArray()
    self.depend_0_init = MrArray()
    self.depend_0_save = MrArray()
    self.depend_1      = MrArray()
    self.depend_1_init = MrArray()
    self.depend_1_save = MrArray()
    self.depend_2      = MrArray()
    self.depend_2_init = MrArray()
    self.depend_2_save = MrArray()
    self.depend_3      = MrArray()
    self.depend_3_init = MrArray()
    self.depend_3_save = MrArray()
    self.filefinder    = MrFileFinder()

    ;Set Properties
    self.time_pattern = '%Y-%M-%dT%H:%m:%S.%1%2%3%4'
    self -> SetProperty, FILENAME=filename, DIMENSION=dimension

    ;Set the data
    case n_params() of
        0: begin
            if n_elements(sTime) gt 0 && n_elements(eTime) gt 0 $
                then self -> SetTimeRange, sTime, eTime, PATTERN=time_pattern
            if n_elements(variable) gt 0 then self -> SetVariable, variable
        endcase
        
        1: self -> SetData, arg0, DEPEND_1=depend_1, DEPEND_2=depend_2, DEPEND_3=depend_3, $
                            NO_COPY=no_copy, NO_REVERT=no_revert
        2: self -> SetData, arg0, arg1, DEPEND_1=depend_1, DEPEND_2=depend_2, DEPEND_3=depend_3, $
                            NO_COPY=no_copy, NO_REVERT=no_revert
        else: message, 'Incorrect number of parameters.'
    endcase
    
    return, 1
end


;+
;   The class definition statement.
;
; :Fields:
;       DATA:               Data to be analyzed
;       DATA_INIT:          Original input data for `DATA`
;       DATA_SAVE:          `DATA` at a particular saved state (see the Save method)
;       DEPEND_0:           Data on which the first dimension of `DATA` depends. Usually time.
;       DEPEND_0_INIT:      Original input data for `DEPEND_0`
;       DEPEND_0_SAVE:      `DEPEND_0` at a particular saved state.
;       DEPEND_1:           Data on which the second dimension of `DATA` depends.
;       DEPEND_1_INIT:      Original input data for `DEPEND_0`
;       DEPEND_1_SAVE:      `DEPEND_1` at a particular saved state.
;       DEPEND_2:           Data on which the third dimension of `DATA` depends.
;       DEPEND_2_INIT:      Original input data for `DEPEND_0`
;       DEPEND_2_SAVE:      `DEPEND_2` at a particular saved state.
;       DEPEND_3:           Data on which the fourth dimension of `DATA` depends.
;       DEPEND_3_INIT:      Original input data for `DEPEND_0`
;       DEPEND_3_SAVE:      `DEPEND_3` at a particular saved state.
;       DIMENSION:          Dimension of `DATA` along which to apply analysis methods, when applicable.
;       FILEFINDER:         A MrFileFinder object for locating files on the file system.
;       FIRST_READ:         Time stamp of the first data point.
;       LAST_READ:          Time stamp of the last data point.
;       TIME_PATTERN:       String of tokens representing the time format.
;       VARIABLE:           Variable name for which `DATA` is read.
;-
pro MrDataSet__define
    compile_opt strictarr
    
    class = { MrDataSet, $
              inherits IDL_Object, $
              
              ;Data Storage
              data:          obj_new(), $
              data_init:     obj_new(), $
              data_save:     obj_new(), $
              depend_0:      obj_new(), $
              depend_0_init: obj_new(), $
              depend_0_save: obj_new(), $
              depend_1:      obj_new(), $
              depend_1_init: obj_new(), $
              depend_1_save: obj_new(), $
              depend_2:      obj_new(), $
              depend_2_init: obj_new(), $
              depend_2_save: obj_new(), $
              depend_3:      obj_new(), $
              depend_3_init: obj_new(), $
              depend_3_save: obj_new(), $
              
              dimension:     0, $
              fileFinder:    obj_new(), $
              first_read:    '', $
              last_read:     '', $
              time_pattern:  '', $
              variable:      ''  $
            }
end