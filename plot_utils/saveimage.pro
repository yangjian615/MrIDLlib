; docformat = 'rst'
;
; NAME:
;       SAVEIMAGE
;
; PURPOSE:
;+
;       The purpose of this program is to provide a means of saving the contents of a
;       graphics window to a file. Output file-type options include::
;
;           1. JPEG
;           2. TIFF
;           3. PNG
;           4. GIF
;
; :Params:
;
;       WIN_ID:         in, optional, type=int
;                       The ID of the graphics window to be read.
;
;
; :Keywords:
;
;       DIRECTORY   -   in, optional, type=String, default=current directory
;                       The directory to which the save dialog should be opened to or a
;                           named variable into which the chosen directory will be returned.
;                           If provided and the directory is changed via the dialog, this
;                           will also be changed.
;       JPEG        -   In, optional, type=Boolean, default=1
;                       Indicate that a ".jpeg" file is to be written.
;       TIFF        -   In, optional, type=Boolean, default=0
;                       Indicate that a ".tiff" file is to be written.
;       PNG         -   In, optional, type=Boolean, default=0
;                       Indicate that a ".png" file is to be written.
;       GIF         -   In, optional, type=Boolean, default=0
;                       Indicate that a ".gif" file is to be written.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2012
;
; :History:
;	Modification History::
;   11/13/2012  -   Written by Matthew Argall
;
pro saveImage, win_id, $
DIRECTORY=directory, $
JPEG=jpeg, $
TIFF=tiff, $
PNG=png, $
GIF=gif
	compile_opt idl2
	
	;Make sure only one image type was selected. Default to JPEG.
	nkwds = 0
	nkwds  += keyword_set(jpeg) + keyword_set(tiff) + keyword_set(png) + keyword_set(gif)
	if nkwds eq 0 then jpeg = 1
	if nkwds gt 1 then begin
	    print, 'Image type options are mutually exclusive."
	    print, 'Choose: /jpeg OR /tiff OR /png OR /gif.'
	    return
	endif
	
	;check where to save the image
	if n_elements(directory) eq 0 then cd, CURRENT=directory
	
	;set the file extension
	if keyword_set(jpeg) then extension = 'jpeg'
	if keyword_set(tiff) then extension = 'tiff'
	if keyword_set(png) then extension = 'png'
	if keyword_set(gif) then extension = 'gif'
	
	;set the window
	if n_elements(win_id) ne 0 then wset, win_id
    
    ;select a directory and choose a filename for the image
    filename = dialog_pickfile(FILE='myImage', PATH=directory, GET_PATH=directory, $
                               DEFAULT_EXTENSION=extension, /WRITE, $
                               TITLE='Save Image As:')
    
    ;save the selected directory  
    if strcmp(filename, '') then return 
		
    ;get the current color table and read the screen. Read a 24-bit color if the write
    ;routine accepts 24-bit images. Otherwise, read an 8-bit image using the current color
    ;table
    if keyword_set(gif) then begin
        tvlct, r, g, b, /get
        image = tvrd()
    endif else image = tvrd(TRUE=1)
	
	;write the image to a file
	if keyword_set(jpeg) then write_jpeg, filename, image, TRUE=1
	if keyword_set(png) then write_png, filename, image
	if keyword_set(tiff) then begin
            ;tvrd() scans from bottome to top. Tiff readers read top to bottom. Must reverse
            ;the vertical dimension of the image.
            image = reverse(image, 3)
            write_tiff, filename, image
    endif
    
    if keyword_set(gif) then write_gif, filename, image, r, g, b
end
