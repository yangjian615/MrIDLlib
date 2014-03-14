;data_in is a 3xN array with [Bx, By, Bz]
;et_in is a 1xN array of the ephemeris time stamps -- the time stamps for data_in


;-----------------------------------------------------
;\\\\\\\\\\\\\\\\\\\ INPUTS \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;set n_sec = {0 | >0} for {slow | fast} transformation.
n_sec = 0
kernel = '/path/to/kernel/kernel.blah'
sc = 'A'     ;or 'B'
inertial_frame = 'GSE'

;-----------------------------------------------------
;Read In Your Data Here \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Read in your data here. You need:
;   1. data_in
;   2. epoch_time (in cdf_epoch_tt2000 -- the regular CDF times)

;-----------------------------------------------------
;TRANSFORM TO INERTIAL SYSTEM? \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;convert epoch times to ephemeris times then rotate mag data from the spacecraft frame
;to an inertial frame.
et_in = epoch_to_et(epoch_time, KERNEL=kernel)
mag_inert = rbsp_scs_to_inert(data_in, et_in, sc_in, scs_to_inert_out, scs_inds_out, $
                              KERNEL=kernel, N_SEC=n_sec, INERTIAL_FRAME=inertial_frame)

end