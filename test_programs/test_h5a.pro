; docformat = 'rst'
;
; NAME:
;       TEST_H5A
;
; PURPOSE:
;+
;   IDL's H5A and other HDF5 attribute-related routines take an extraordinary amount of
;   time to execute. This is an attempt to reproduce the problem I am having with my own
;   HDF5 file object.
;
;   Result:
;       If files are parsed and closed before the next files is parsed, then the time
;       H5A_OPEN_INDEX, H5A_OPEN_NAME, and H5A_GET_NUM_ATTRS take to execute increases
;       linearly with the number of files. If, however, all files are parsed before any
;       of them are closed, the H5A routines take an increasing amount of time per file.
;
;       Opened forum post on EXELIS VIS:
;           http://www.exelisvis.com/Support/Forums/tabid/184/forumid/7/postid/15679/scope/posts/Default.aspx#15679
;
;       
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
;       2014/07/02  -   Written by Matthew Argall
;-
pro test_h5a
    compile_opt idl2
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if max(obj_valid(oFiles)) then obj_destroy, oFiles
        void = cgErrorMSG(/QUIET)
        return
    endif

    ;Array of file names
    fnames = [['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120901_v2.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120902_v2.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120903_v2.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120904_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120905_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120906_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120907_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120908_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120909_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120910_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120911_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120912_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120913_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120914_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspa_def_MagEphem_TS04D_20120915_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120901_v2.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120902_v2.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120903_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120904_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120905_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120906_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120907_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120908_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120909_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120910_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120911_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120912_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120913_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120914_v1.0.0.h5'], $
              ['/Users/argall/Documents/Work/Data/RBSP/Ephemeris/rbspb_def_MagEphem_TS04D_20120915_v1.0.0.h5']]

    ;Number of files
    nFiles = n_elements(fnames)

    ;Step through each file
    for i = 0, nFiles - 1 do begin
        ;Print status message
        print, FORMAT='(%"Opening file %i of %i.")', i+1, nFiles
        
        ;Open the file.
        oFiles = MrHDF5_File(fnames[i])
        
        ;Close the file
        obj_destroy, oFiles
    endfor
end

    
;---------------------------------------------------------------------
; %Main-level program: IDL> .r test_h5a //////////////////////////////
;---------------------------------------------------------------------
;Profile the system routines (which include H5A_*, etc.)
profiler, /SYSTEM
test_h5a
profiler, /REPORT
profiler, /RESET
profiler, /CLEAR
end