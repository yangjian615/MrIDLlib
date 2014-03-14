; docformat = 'rst'
;
;+
;   The purpose of this program is to test the CDF_DATA object, it being new, as well
;   as the RBSP_SCS_TO_INERT transformation program, which uses SPICE to transform from
;   the spacecraft frame into an inertial reference frame.
;
;   SPICE has been shown to have discontinuities when it comes to creating transformation
;   matrices. The interpolation of the angle between one frame and the next is not smooth
;   and there is a jump discontinuity with a frequency of about 1Hz.
;
;   The goal here is to show that discontinuity and its effects within magnetometer
;   spectra, if possible. RBSP_SCS_TO_INERT has two methods, one transforms all points
;   individually using SPICE while the other despins a chunk of data manually and does
;   a batch transformation. Both will be tested.
;
; :Categories:
;   Test Program, RBSP
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;-
;-----------------------------------------------------
;\\\\\\\\\\\\\\\\\\\ INPUTS \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
restore_sav = 0
create_sav = 0
tf_read = 1

tstart = '170000'
tend = '220000'

;Name of the restore file.
root = '/Users/argall/Documents/Work/Programs/MyLibraryIDL/test_programs'
save_file = root + '/sav/rbsp_ascii_0502013_' + tstart + '_' + tend + '.sav'
data_file = root + '/data/rbspb-emfisis_mag-292_QL-1sec-gsm_2013-02-15_v1.1.3.2.asc'

;Date of data
date_pos = stregex(data_file, '([0-9]{4}[^0-9]?[0-9]{2}[^0-9]?[0-9]{2})', len=len, /subexp)
date = strmid(data_file, date_pos[0], len[0])


;Cannot restore and save at the same time.
if restore_sav and create_sav then $
    message, 'CREATE_SAV and RESTORE_SAV are mutually exclusive.'

;-----------------------------------------------------
;RESTORE SAVE FILE? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
if restore_sav then restore, save_file, /VERBOSE
    
;-----------------------------------------------------
;GET DATA? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
if tf_read then begin
    ;read data from the ascii file
    mag_data = read_rbsp_mag_asc(data_file, time, $
                                 TSTART=hms_to_ssm(tstart), TEND=hms_to_ssm(tend))
endif

;-----------------------------------------------------
;CREATE IDL SAVE FILE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
if create_sav then begin
    save, time, mag_data, /COMPRESS, $
          FILENAME = save_file
          DESCRIPTION='RBSP-A data from ' + date + ' ' + tstart + ' to ' + tend
endif

;-----------------------------------------------------
;CREATE SPECTRA OF THE DATA \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
nfft = 512*8
dt = 1.0/1.0
nshift = 64
mag_spectra_x = spectrogram(mag_data[0,*], nfft, dt, nshift, $
                            FREQUENCIES=f, FMIN=0, FMAX=0.08, TIME=bin_time)
                              
mag_spectra_y = spectrogram(mag_data[1,*], nfft, dt, nshift, $
                            FREQUENCIES=f, FMIN=0, FMAX=0.08)
                              
mag_spectra_z = spectrogram(mag_data[2,*], nfft, dt, nshift, $
                            FREQUENCIES=f, FMIN=0, FMAX=0.08)

;-----------------------------------------------------
;PLOT THE RESULTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;t_ssm = epoch_to_ssm(epoch_time)
bin_time = bin_time + time[0]

;B UVW
myplot = MrPlot(time, mag_data, TITLE='RBSP-B Mag GSM ' + date + ' ' + tstart + ' - ' + tend, $
                XTITLE = 'Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                YTITLE = 'B (nT)', YRANGE=[min(mag_data, max=maxB), maxB], $
                COLOR=['blue', 'green', 'red'], LEGEND=['X', 'Y', 'Z'])

;Bx Spectra
freq_range = [min(f, max=maxf), maxf]
myplot -> addplot, bin_time, f, IMAGE=alog10(mag_spectra_x), /AXES, /SCALE, /ADDCOLORBAR, $
                   TITLE='Mag Spectra X-Component GSM', CBTITLE='log(Power)!Clog[(nT)^2 * Hz]', $
                   XTITLE='Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                   YTITLE='Frequency (Hz)', YRANGE=freq_range, YSTYLE=1, $
                   DATA_POS=[bin_time[0], freq_range[0], bin_time[-1], freq_range[1]]

;Bx Spectra
myplot -> addplot, bin_time, f, IMAGE=alog10(mag_spectra_y), /AXES, /SCALE, /ADDCOLORBAR, $
                   TITLE='Mag Spectra Y-Component GSM', CBTITLE='log(Power)!Clog[(nT)^2 * Hz]', $
                   XTITLE='Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                   YTITLE='Frequency (Hz)', YRANGE=freq_range, YSTYLE=1, $
                   DATA_POS=[bin_time[0], freq_range[0], bin_time[-1], freq_range[1]]

;Bx Spectra
myplot -> addplot, bin_time, f, IMAGE=alog10(mag_spectra_z), /AXES, /SCALE, /ADDCOLORBAR, $
                   TITLE='Mag Spectra Z-Component GSM', CBTITLE='log(Power)!Clog[(nT)^2 * Hz]', $
                   XTITLE='Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                   YTITLE='Frequency (Hz)', YRANGE=freq_range, YSTYLE=1, $
                   DATA_POS=[bin_time[0], freq_range[0], bin_time[-1], freq_range[1]]
end