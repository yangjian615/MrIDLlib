pro test_cdf_open
  compile_opt strictarr
  on_error, 2
  ;
  ; CDF_Open takes an extra long time (~4s) to execute. This procedure intends to
  ; isolate the behavior.
  ;
  
  ;Which file
  ; 1 = Cluster
  ; 0 = RBSP
  cluster_file = 1
  
  ;You or me?
  if !version.os_family eq 'unix' then begin
      data_dir = '/Users/argall/Google Drive/Coworkers-Shared/Matt & Rich'
  endif else begin
    CD, 'D:\IDL\examples'
    data_dir  = 'D:\IDL\examples\'
  endif

  ;Cluster file
  if cluster_file eq 1 then begin
    data_file = filepath('C3_CP_EFW_L2_E__20010608_053000_20010608_054000_V110503.cdf', ROOT_DIR=data_dir)
    varname   = 'E_Vec_xy_ISR2__C3_CP_EFW_L2_E'

  ;RBSP file
  endif else begin
    data_file = filepath('rbsp-a_magnetometer_hires-gse_emfisis-L3_20131109_v1.3.3.cdf', ROOT_DIR=data_dir)
    varname   = 'Mag'
  endelse

  ;Try several times for good measure.
  nAttempts = 100

  ;Loop through each attempt
  for i = 0, nAttempts - 1 do begin
    ;Open the CDF file
    cdfID = cdf_open (data_file)
    
    ; Read some data
    cdf_varget, cdfID, varname, data

    ; Close each file
    cdf_close, cdfID
  endfor
end

;-------------------------------------------------------
;Main-Level example program: IDL> .run test_cdf_open ///
;-------------------------------------------------------
;Profile all currently compiled user and system routines
profiler
profiler, /SYSTEM

;Run the test program
IDL_cdf_open_speed_test_0101

;Report
profiler, /REPORT

;Reset the profiler then stop profiling
profiler, /RESET
profiler, /CLEAR

end




; % Compiled module: CDF_OPEN_SPEED_TEST_0101.
; C3_CP_EFW_L2_E__20010608_053000_20010608_054000_V110503.cdf
; % Loaded DLM: CDF.
; Module          Type  Count     Only(s)   Avg.(s)     Time(s)   Avg.(s)
; CD              (S)       1    0.000033  0.000033    0.000033  0.000033
; CDF_CLOSE       (S)     100    0.002388  0.000024    0.002388  0.000024
; CDF_OPEN        (S)     100    0.180192  0.001802    0.180192  0.001802
; CDF_OPEN_SPEED_TEST_0101
;                 (U)       1    0.000742  0.000742    0.183391  0.183391
; LONARR          (S)       1    0.000006  0.000006    0.000006  0.000006
; ON_ERROR        (S)       1    0.000000  0.000000    0.000000  0.000000
; PRINT           (S)       1    0.000030  0.000030    0.000030  0.000030

; % Compiled module: CDF_OPEN_SPEED_TEST_0101.
; C3_CP_EFW_L2_E__20010608_053000_20010608_054000_V110503.cdf
; Module          Type  Count     Only(s)   Avg.(s)     Time(s)   Avg.(s)
; CD              (S)       1    0.000041  0.000041    0.000041  0.000041
; CDF_CLOSE       (S)     100    0.001753  0.000018    0.001753  0.000018
; CDF_OPEN        (S)     100    0.076946  0.000769    0.076946  0.000769
; CDF_OPEN_SPEED_TEST_0101
;                 (U)       1    0.000213  0.000213    0.078989  0.078989
; LONARR          (S)       1    0.000005  0.000005    0.000005  0.000005
; ON_ERROR        (S)       1    0.000001  0.000001    0.000001  0.000001
; PRINT           (S)       1    0.000030  0.000030    0.000030  0.000030

; % Compiled module: CDF_OPEN_SPEED_TEST_0101.
; C3_CP_EFW_L2_E__20010608_053000_20010608_054000_V110503.cdf
; Module          Type  Count     Only(s)   Avg.(s)     Time(s)   Avg.(s)
; CD              (S)       2    0.000081  0.000040    0.000081  0.000040
; CDF_CLOSE       (S)     100    0.001829  0.000018    0.001829  0.000018
; CDF_OPEN        (S)     100    0.084522  0.000845    0.084522  0.000845
; CDF_OPEN_SPEED_TEST_0101
;                 (U)       1    0.000222  0.000222    0.086645  0.086645
; LONARR          (S)       1    0.000005  0.000005    0.000005  0.000005
; ON_ERROR        (S)       1    0.000001  0.000001    0.000001  0.000001
; PRINT           (S)       1    0.000029  0.000029    0.000029  0.000029
; PROFILER        (S)       3    0.000075  0.000025    0.000075  0.000025

; % Compiled module: CDF_OPEN_SPEED_TEST_0101.
; C3_CP_EFW_L2_E__20010608_053000_20010608_054000_V110503.cdf
; % Loaded DLM: CDF.
; Module          Type  Count     Only(s)   Avg.(s)     Time(s)   Avg.(s)
; CD              (S)       1    0.000044  0.000044    0.000044  0.000044
; CDF_CLOSE       (S)     100    0.002540  0.000025    0.002540  0.000025
; CDF_OPEN        (S)     100    0.112309  0.001123    0.112309  0.001123
; CDF_OPEN_SPEED_TEST_0101
;                 (U)       1    0.000845  0.000845    0.115787  0.115787
; LONARR          (S)       1    0.000007  0.000007    0.000007  0.000007
; ON_ERROR        (S)       1    0.000001  0.000001    0.000001  0.000001
; PRINT           (S)       1    0.000042  0.000042    0.000042  0.000042

; % Compiled module: CDF_OPEN_SPEED_TEST_0101.
; C3_CP_EFW_L2_E__20010608_053000_20010608_054000_V110503.cdf
; Module          Type  Count     Only(s)   Avg.(s)     Time(s)   Avg.(s)
; CD              (S)       2    0.000070  0.000035    0.000070  0.000035
; CDF_CLOSE       (S)     100    0.001920  0.000019    0.001920  0.000019
; CDF_OPEN        (S)     100    0.087834  0.000878    0.087834  0.000878
; CDF_OPEN_SPEED_TEST_0101
;                 (U)       1    0.000241  0.000241    0.090057  0.090057
; LONARR          (S)       1    0.000004  0.000004    0.000004  0.000004
; ON_ERROR        (S)       1    0.000001  0.000001    0.000001  0.000001
; PRINT           (S)       1    0.000023  0.000023    0.000023  0.000023
; PROFILER        (S)       3    0.000071  0.000024    0.000071  0.000024
