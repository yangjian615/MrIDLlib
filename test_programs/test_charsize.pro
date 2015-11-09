; docformat = 'rst'
;
; NAME:
;       TEST_REF_EXTRA
;
; PURPOSE:
;+
;       The purpose of this program is to test font sizes compatability between the
;       'PS' and 'X' device.
;               
;
; :Categories:
;
;       Test Program
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;       
; :History:
;   Modification History::
;       2015/11/08  -   Written by Matthew Argall
;-
;*****************************************************************************************
function test_charsize, $
SAVE=tf_save
	compile_opt idl2

	tf_save = keyword_set(tf_save)

;-----------------------------------------------------
; MrWindow \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Window
	win = MrWindow(XSIZE=640, YSIZE=512, REFRESH=0)
	
	;Create a dummy graphic
	p1 = MrPlot([0], [1], /CURRENT, /NODATA, XSTYLE=5, YSTYLE=5, POSITION=[0.0, 0.0, 1.0, 1.0])

	;Draw text with different sizes
	tA = MrText(0.005, 0.90, 'A', /NORMAL, NAME='A', TARGET=p1, CHARSIZE=0.5)
	tB = MrText(0.020, 0.90, 'B', /NORMAL, NAME='B', TARGET=p1, CHARSIZE=1.0)
	tC = MrText(0.040, 0.90, 'C', /NORMAL, NAME='C', TARGET=p1, CHARSIZE=1.5)
	tD = MrText(0.062, 0.90, 'D', /NORMAL, NAME='D', TARGET=p1, CHARSIZE=2.0)
	tE = MrText(0.090, 0.90, 'E', /NORMAL, NAME='E', TARGET=p1, CHARSIZE=2.5)
	tF = MrText(0.120, 0.90, 'F', /NORMAL, NAME='F', TARGET=p1, CHARSIZE=3.0)
	tG = MrText(0.155, 0.90, 'G', /NORMAL, NAME='G', TARGET=p1, CHARSIZE=3.5)
	tH = MrText(0.195, 0.90, 'H', /NORMAL, NAME='H', TARGET=p1, CHARSIZE=4.0)
	tI = MrText(0.240, 0.90, 'I', /NORMAL, NAME='I', TARGET=p1, CHARSIZE=4.5)
	tJ = MrText(0.265, 0.90, 'J', /NORMAL, NAME='J', TARGET=p1, CHARSIZE=5.0)
	tK = MrText(0.310, 0.90, 'K', /NORMAL, NAME='K', TARGET=p1, CHARSIZE=5.5)
	tL = MrText(0.370, 0.90, 'L', /NORMAL, NAME='L', TARGET=p1, CHARSIZE=6.0)
	tM = MrText(0.425, 0.90, 'M', /NORMAL, NAME='M', TARGET=p1, CHARSIZE=6.5)

	;Refresh the window
	win -> Refresh
	
	;Save as PS and PNG
	if tf_save then begin
		win -> Save, './MrText.ps'
		win -> Save, './MrText.png'
	endif

;-----------------------------------------------------
; cgWindow \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create the window
	cgWindow, WXSIZE=640, WYSIZE=512, WOBJECT=cgwin
	
	;Draw text
	cgText, 0.005, 0.90, 'A', /ADDCMD, /NORMAL, CHARSIZE=0.5
	cgText, 0.020, 0.90, 'B', /ADDCMD, /NORMAL, CHARSIZE=1.0
	cgText, 0.040, 0.90, 'C', /ADDCMD, /NORMAL, CHARSIZE=1.5
	cgText, 0.062, 0.90, 'D', /ADDCMD, /NORMAL, CHARSIZE=2.0
	cgText, 0.090, 0.90, 'E', /ADDCMD, /NORMAL, CHARSIZE=2.5
	cgText, 0.120, 0.90, 'F', /ADDCMD, /NORMAL, CHARSIZE=3.0
	cgText, 0.155, 0.90, 'G', /ADDCMD, /NORMAL, CHARSIZE=3.5
	cgText, 0.195, 0.90, 'H', /ADDCMD, /NORMAL, CHARSIZE=4.0
	cgText, 0.240, 0.90, 'I', /ADDCMD, /NORMAL, CHARSIZE=4.5
	cgText, 0.265, 0.90, 'J', /ADDCMD, /NORMAL, CHARSIZE=5.0
	cgText, 0.310, 0.90, 'K', /ADDCMD, /NORMAL, CHARSIZE=5.5
	cgText, 0.370, 0.90, 'L', /ADDCMD, /NORMAL, CHARSIZE=6.0
	cgText, 0.425, 0.90, 'M', /ADDCMD, /NORMAL, CHARSIZE=6.5

	;Save as PS and PNG
	if tf_save then begin
		cgWin -> SetProperty, IM_RASTER=0
		cgWin -> Output, './cgText.ps'
		cgWin -> Output, './cgText.png'
	endif

	return, win
end