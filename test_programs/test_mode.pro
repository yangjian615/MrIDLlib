; docformat = 'rst'
;
; NAME:
;       TEST_REF_EXTRA
;
; PURPOSE:
;+
;       The purpose of this program is to test a program for determining the mode of an
;       array. Discussion of this procedure can be found in the `IDL News Group
;       <https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/yUOadPvIXZI>`.
;
; :Categories:
;
;       Test Program
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;       
; :History:
;   Modification History::
;       07/09/2013  -   Written by Rob Klooster
;-
function test_mode, array
    compile_opt strictarr
    
    ;Sort the array so that unique values can be determined
    sortedarray = array[Sort(array)]
    
    ;Get the unique values
    arrayenum = sortedarray[Uniq(sortedarray)] 
    
    ;Match unique values with their duplicates, and map by index values
    mappedarray = Value_Locate(arrayenum, array)
    
    ;Group the index values together, using HISTOGRAM to count.
    hist = histogram(mappedarray, min=0)
    
    ;Return the bin with the maximum number of values.
    return, arrayenum[where(hist eq max(hist))] 
end

;--------------------------------------------------
;Example Program (IDL> .r test_mode) \\\\\\\\\\\\\\
;--------------------------------------------------
print, test_mode([1., 10.^8, 10.^8]) 
print, test_mode([10.^8, 10.^8+1, 1.]) 
print, test_mode([10.^8, 10.^8+10, 1.]) 
end