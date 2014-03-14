root = '/Users/argall/Documents/Work/Programs/MyLibraryIDL/test_programs'
filenames = ['/data/rbspb-emfisis_mag-292_QL-1sec-gsm_2013-02-15_v1.1.3.1.asc', $
             '/data/rbspb-emfisis_mag-292_QL-1sec-gsm_2013-02-15_v1.1.3.2.asc']
filenames = root + filenames

tstart = '10:48:00'
tend = '10:50:00'

for i = 0, n_elements(filenames) - 1 do begin
    if i eq 0 $
        then , filenames[i], tstart, tend, myplot, /BX, /BY, /BZ, /BMAG $
        else rbsp_plot_mag_ts, filenames[i], tstart, tend, myplot, /BX, /BY, /BZ, /BMAG
endfor

end
    