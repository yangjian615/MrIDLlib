; docformat = 'rst'
;
;+
;   The purpose of this program is to test spectrogram.pro using data from the `British
;   Antarctic Survey Halley Bay magnetometer station http://psddb.nerc-bas.ac.uk/data/access/plots.php?menu=1&class=101&type=SCM&site=Halley&bc=1,4,7,9&graph=1&year=2013&day=050`
;
;   Clicking on the plot will bring you to the data download page.
;
; RESULTS
;   Spectrogram.pro is able to reproduce the signatures seen in the ground magnetometer
;   data, but the scale of the colorbar is different. This could be due to a
;   misunderstanding of how /SCALE and /CBLOG interact.
;
; :Categories:
;   Test Program, spectrogram
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
restore_sav = 1
create_sav = 0

;Name of the restore file.
root = '/Users/argall/Documents/Work/Programs/MyLibraryIDL/test_programs'
save_file = root + '/test_spectrogram.sav'
data_file = root + '/0502013.TXT'

;Cannot restore and save at the same time.
if restore_sav and create_sav then $
    message, 'CREATE_SAV and RESTORE_SAV are mutually exclusive.'

;-----------------------------------------------------
;RESTORE SAVE FILE? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
if restore_sav then begin
    restore, save_file, /VERBOSE

;-----------------------------------------------------
;GET DATA AND TRANSFORM TO GSE? \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
endif else if create_sav then begin
    
    ;-----------------------------------------------------
    ;GET DATA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
    mag_data = read_bas_search_coil(data_file, time)
    
    ;-----------------------------------------------------
    ;CREATE IDL SAVE FILE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
    save, time, mag_data, /COMPRESS, FILENAME = save_file, $
          DESCRIPTION='Halley Bay data: 050 2013'
endif

;-----------------------------------------------------
;CREATE SPECTRA OF THE DATA \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
nfft = 512
dt = 0.1
nshift = nfft/2
mag_spectra_x = spectrogram(mag_data[0,*], nfft, dt, nshift, $
                            FREQUENCIES=f, FMAX=1.0, TIME=bin_times)
                              
mag_spectra_y = spectrogram(mag_data[1,*], nfft, dt, nshift, FMAX=1.0)
                              
mag_spectra_z = spectrogram(mag_data[2,*], nfft, dt, nshift, FMAX=1.0)

;-----------------------------------------------------
;PLOT THE RESULTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
bin_times = bin_times + time[0]

;B UVW
myplot = MrPlot(time, mag_data, TITLE='BAS Halley Bay 050 2013', $
                XTITLE = 'Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                YTITLE = 'B (nT)', YRANGE=[min(mag_data, max=maxB), maxB], $
                COLOR=['blue', 'green', 'red'], LEGEND=['X', 'Y', 'Z'])

;Bu Spectra
myplot -> addplot, bin_times, f, IMAGE=mag_spectra_x, /AXES, /SCALE, /ADDCOLORBAR, $
                   TITLE='Mag Spectra X', CBTITLE='(nT)^2 * Hz', $
                   XTITLE='Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                   YTITLE='Frequency (Hz)', YRANGE=[min(f, max=maxf), maxf], YSTYLE=1

;Bx Spectra
myplot -> addplot, bin_times, f, IMAGE=mag_spectra_y, /AXES, /SCALE, /ADDCOLORBAR, $
                   TITLE='Mag Spectra Y', CBTITLE='(nT)^2 * Hz', $
                   XTITLE='Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                   YTITLE='Frequency (Hz)', YRANGE=[min(f, max=maxf), maxf], YSTYLE=1

;Bx Spectra
myplot -> addplot, bin_times, f, IMAGE=mag_spectra_z, /AXES, /SCALE, /ADDCOLORBAR, $
                   TITLE='Mag Spectra Z', CBTITLE='(nT)^2 * Hz', $
                   XTITLE='Time (UTC)', XTICKFORMAT='time_labels', XSTYLE=1, $
                   YTITLE='Frequency (Hz)', YRANGE=[min(f, max=maxf), maxf], YSTYLE=1
end