@c_jason_test

PRO plotevent

    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif

    ; define parameters
    stime = '2004-04-06T03:00:00'
    etime = '2004-04-06T06:00:00'
;    stime = '2001-08-17T16:30:00'
;    etime = '2001-08-17T16:40:00'
    sc = 4
    title = 'C4 - 2001-08-17'
    directory = '/Users/argall/Documents/Work/Data/20040406_030000_060000/'
;    directory = '/home/shuster/MrPlot/GRLexhaust/2001_08_17/data/'
;    device,retain=2
    device,set_font = 'Helvetica Bold', /TT_FONT


    ; ------------------------------------------------------------------------
    ; MVA FRAME ROTATION INFORMATION (automatic calculation for magnetopause).
    ; ------------------------------------------------------------------------
    ;IDL Format for Copy + Paste
    ; eigvecs = [[Nx, Ny, Nz], $
    ;            [Mx, My, Mz], $
    ;            [Lx, Ly, Lz]]
    ;tstart = '16:20:00.040000'
    ;tend =   '16:49:59.990000'
    ;eigvals = [   51.4623,   247.4609,   683.9588]
    eigvecs = [[ 0.0642,  0.5509, -0.8321], $
               [-0.0062,  0.8340,  0.5517], $
               [ 0.9979, -0.0303,  0.0569]]


    ; --------------------------------------------------
    ; MVA FRAME in magnetotail (swap L and N from above)
    ; --------------------------------------------------
    ; eigvecs = [[Lx, Ly, Lz], $
    ;            [Mx, My, Mz], $
    ;            [Nx, Ny, Nz]]
    ;tstart = '16:20:00.040000'
    ;tend =   '16:49:59.990000'
    ;eigvals = [   51.4623,   247.4609,   683.9588]
    eigvecs = [[ 0.9979, -0.0303,  0.0569], $
               [-0.0062,  0.8340,  0.5517], $
               [ 0.0642,  0.5509, -0.8321]]

stop
    ; --------------------------
    ; Call Matt's MRPLOT script.
    ; -------------------------
    j = c_jason(sc,stime,etime,directory=directory,lmn_frame=eigvecs)


    ;Do not plot every time something changes.
    j -> Refresh, /DISABLE

    print, j ; display plot info
    wait, 0.5

    ; Adds titles back to plots (JRS)
    j['Bxyz'].TITLE = title;, XRANGE=xrange, $
                           ;XTITLE='UT (HH:MM:SS)', XTICKFORMAT='time_labels'

    ; make density plot y axis log scale
;    j['ni'].ylog = 1

    ; All plots will be adjusted
    j -> SetProperty, OXMARGIN=[9,6]
    j -> SetGlobal, XTICKS=10, XMINOR=6, XTICKLEN=.05, FONT=1, CHARTHICK=2.0, CHARSIZE=3.0, $
                    XTHICK=2, YTHICK=2

    ; Add horizontal lines to B and V plots
    j['Bxyz'] -> SetProperty, YTICKLEN=1,YGRIDSTYLE=1;,XTICKLEN=1,XGRIDSTYLE=1
    j['Vi'] -> SetProperty, YTICKLEN=1,YGRIDSTYLE=1;,XTICKLEN=1,XGRIDSTYLE=1

    j['Vi'] -> SetProperty, TITLE='', XRANGE=xrange, $
                            XTITLE='UT (HH:MM:SS)', XTICKFORMAT='time_labels',$
                            YTITLE='H+ Velocity!C(km s^-1)'
                                      
                                      
    ;Now turn refresh back on.
    j -> Refresh

; ------------
; OLD COMMANDS
; ------------
    ;j['Ion E Flux'] -> SetProperty, YRANGE=[1,3e4],/YLOG
;    j['Ion E Flux'].ylog = 0
;    j['Ion E Flux'].yrange = [1,2.5e4]
;    j['Ion E Flux'].ytitle = 'H+ (eV)!CCODIF'
;    j['RAPID e- Flux'].ytitle = 'e- Flux!CRAPID'
;    j['E SPIN'].ytitle = 'e- (eV)!CPEACE'
    ;j['Ion E Flux'].range = [4,5] ; to change colorbar range
    ;j['E SPIN'].ylog = 1
;    j[8].yrange = [1000,10000]
;print,j[8].range
;    j[8].range = [1,100]
;    j[9].range = [1,100]
;    j -> SetProperty,11,xticks=4,xminor=5,xticklen=.05
    ;j[9].range = [4,5] ; to change colorbar range

    ;j -> Rotate, eigvecs, [1,1] ; rotate B to LMN

    ;To change the color scale (i.e. the axis), type
    ;mywin[4].range = [min, max]
    ;or
    ;j['CB: Ion Flux'].range = [3, 10] ; change the colorbar range

    ; if you need to set a bunch of properties at the same time, you can use the "SetProperty" method like
    ; mywin[1].SetProperty, YRANGE=[min, max], /YLOG, TITLE='Title', etc.

    ;Also, you can rename your plots with the "NAME" keyword
    ;mywin[5].name = 'New Name'
    ;and then change its properties like
    ;mywin['New Name'].yrange = [min,max]

    ; data reading file directory:
    ;/home/argall/Work/MyLibraryIDL/cluster/c_data

END
