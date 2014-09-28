pro Test_MrCDF_File
    compile_opt strictarr
    on_error, 2
    ;
    ; CDF_Open takes an extra long time (~4s) to execute. This procedure intends to
    ; isolate the behavior.
    ;
    
    ;File to test
    magdir  = '/Users/argall/Documents/Work/Data/RBSP/Emfisis/A/2013_gse/01/'
    magfile = FilePath('rbsp-a_magnetometer_hires-gse_emfisis-L3_20130130_v1.3.2.cdf', ROOT_DIR=magdir)

    ;Try several times for good measure.
    nAttempts = 4
    CDFObjs   = objarr(nAttempts)

    ;Loop through each attempt
    for i = 0, nAttempts - 1 do begin
        ;Open the CDF file
        CDFObjs[i] = MrCDF_File(magfile)
        
        ;Read some data
        data = CDFObjs[i] -> Read('Mag')
    endfor
    
    ;Destroy the objects, thereby closing the files
    obj_destroy, CDFObjs
end

;-------------------------------------------------------
;Main-Level example program: IDL> .run Test_MrCDF_File /
;-------------------------------------------------------
;Profile all user and system routines
profiler
profiler, /SYSTEM

;Run the test program
TEST_MrCDF_File

;Report
profiler, /REPORT

;Reset the profiler then stop profiling
profiler, /RESET
profiler, /CLEAR

end