; docformat = 'rst'
;
; NAME:
;       TEST_H5G_GET_NUM_ATTRS
;
; PURPOSE:
;+
;   H5G_Get_Num_Attrs has been taking an unusually long time. This program is to
;   try and reproduce the problem
;
;   Result:
;       Does not reproduce the slowness...
;
; :Categories:
;       Test Program
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
;       2014/05/29  -   Written by Rob Klooster
;-
pro test_h5a_get_num_attrs
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        h5_close
        void = cgErrorMSG()
        return
    endif
    
    ;Data file to read
    file = '/Users/argall/Documents/Work/Data/RBSP/Ephemeris/A/rbspa_def_MagEphem_T89Q_20130130_v1.0.0.h5'

    ;Open the file and get the number of object
    h5_id   = h5f_open(file)
    root_id = h5g_open(h5_id, '/')

    ;Get the number of objects and their names
    nObjs     = h5g_get_num_objs(root_id)
    obj_names = strarr(nObjs)
    obj_types = strarr(nObjs)
    nDatasets = 0
    for i = 0, nObjs - 1 do begin
        ;Get the object name
        obj_names[i] = h5g_get_obj_name_by_idx(root_id, i)

        ;Figure out its type
        info = h5g_get_objinfo(root_id, obj_names[i])
        obj_types[i] = info.type
    endfor

    ;Get all of the datasets
    iDatasets = where(obj_types eq 'DATASET', nDatasets)
    data_ids  = lonarr(nDatasets)

    ;Step through all datasets in the file
    for i = 0, nDatasets - 1 do begin
        ;Open the dataset
        data_name   = obj_names[iDatasets[i]]
        data_ids[i] = h5d_open(root_id, data_name)

        ;Count how many attributes they have
        nAttrs = h5a_get_num_attrs(data_ids[i])
    endfor


    ;Close in reverse order
    for i = 0, nDatasets - 1 do h5d_close, data_ids[i]
    h5g_close, root_id
    h5f_close, h5_id
end

;Main level program
profiler
profiler, /SYSTEM
test_h5a_get_num_attrs
profiler, /REPORT
profiler, /RESET
profiler, /CLEAR