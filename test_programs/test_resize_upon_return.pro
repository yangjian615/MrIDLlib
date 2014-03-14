; docformat = 'rst'
;
;+
;   The purpose of this program is to see if a resize event is issued when the program
;   returns. This happens in a different program with a different resizable widget. I am
;   testing to see if I can duplicate the problem here with cgZPlot__define.
;
;   Solution::
;       The problem does not happen with cgZPlot.
;
; :Categories:
;   Test Program
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;-
pro test_resize_upon_return, x, y, myPlot

    myPlot = obj_new('cgZPlot', x, y, TITLE='My Plot')
    
end