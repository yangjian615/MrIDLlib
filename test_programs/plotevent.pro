@c_jason_test

PRO plotevent

    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMSG()
        return
    endif

    ; define parameters
    stime = '2004-04-06T03:00:00'
    etime = '2004-04-06T06:00:00'
    sc = 4
    title = '2004-04-06'
    directory = '/Users/argall/Documents/Work/Data/20040406_030000_060000/'
;    device,retain=2
    device,set_font = 'Helvetica Bold', /TT_FONT
;    eigvecs = [[ 0.8546, -0.1878,  0.4841], $
;           [ 0.1642,  0.9822,  0.0911], $
;           [-0.4926,  0.0017,  0.8702]]


    ; call Matt's MRPLOT script
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
    j -> SetProperty, XMARGIN=[9,6]
    j -> SetGlobal, XTICKS=4, XMINOR=5, XTICKLEN=0.1, FONT=1, CHARTHICK=2.0, CHARSIZE=3.0

    ; Add horizontal lines to B and V plots
    j['Bxyz'] -> SetProperty, YTICKLEN=1,YGRIDSTYLE=1, XTICKLEN=1, XGRIDSTYLE=1
    j['Vi'] -> SetProperty, YTICKLEN=1,YGRIDSTYLE=1, XTICKLEN=1, XGRIDSTYLE=1

    ; add tick lables to E 2D DSI plot:
    j['Vi'] -> SetProperty, TITLE='', XRANGE=xrange, $
                                      XTITLE='UT (HH:MM:SS)', XTICKFORMAT='time_labels'
                                      
                                      
    ;Now turn refresh back on.
    j -> Refersh

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
