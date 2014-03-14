; docformat = 'rst'
;
; NAME:
;       MRMODE
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   The purpose of this program is to find the statistacal mode of a floating point
;   array.
;
; :Params:
;       ARRAY:              in, required, type=fltarr
;                           Array from which the mode is to be found.
;
; :Keywords:
;       EPSILON:            in, optional, type=float, default=1d-5
;                           Numbers are exact to within this range. I.e. if 0.2 is actually
;                               represented as 0.200001, then to within an EPSILON of
;                               1e-5, this and 0.2 are equal and thrown into the same
;                               statistical bin. This keyword is only valid if `METHOD`=2
;       METHOD:             in, optional, type=float, default=1
;                           The method by which the mode is to be calculated::
;                               1 - Bin by binary value (default)
;                               2 - Point-by-point comparison
;       IMODE:              out, optional, type=lonarr
;                           The indices of `ARRAY` corresponding to the mode value(s).
;       NMODE:              out, optional, type=lonarr
;                           The number of elements matching `MODE` within `ARRAY`.
;
; :Returns:
;       MODE:               out, required, type=float
;                           The mode of the distribution. If there is more than one mode,
;                               or no mode, then the first will appear.
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Categories:
;
;       CDF Utilities, File I/O, Data Reader
;
; :History:
;   Modification History::
;       07/05/2013  -   Written by Matthew Argall
;       07/30/2013  -   Included a new algorithm based on inforamtion from the `IDL news
;                           group <https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/yUOadPvIXZI>`.
;                           - MRA.
;-
function mrmode, array, $
EPSILON = epsilon, $
METHOD  = method, $
IMODE = iMode, $
NMODE = nMode
    compile_opt strictarr
    
    if n_elements(method) eq 0 then method = 1
    
;---------------------------------------------------------------------
;Point-by-Point Comparison ///////////////////////////////////////////
;---------------------------------------------------------------------
    
    if method eq 2 then begin
        ;Number of points in ARRAY
        npts = n_elements(array)
    
        ;Default value for EPSILON
        if n_elements(epsilon) eq 0 then epsilon = 1d-5
    
        ;[index, count] for keeping track of mode statistics
        mode_count = lonarr(2, npts)
    
        ;Store first ~unique number. Count the how many ~unique numbers there are.
        mode_count[*,0] = [0,1]
        nunique = 1
    
        ;Step through all points in ARRAY
        for i = 1, npts - 1 do begin
            match_found = 0
        
            ;Try to pair the new point with other mode candidates
            for j = 0, nunique - 1 do begin
                if array[i] gt array[mode_count[0,j]]-epsilon && $
                   array[i] lt array[mode_count[0,j]]+epsilon $
                then begin
               
                    mode_count[1,j] += 1
                    match_found = 1
                endif
            endfor
        
            ;If no match was found, create a new mode candidate
            if match_found eq 0 then begin
                mode_count[*,nunique] = [i,1]
                nunique += 1
            endif
        endfor
    
        ;Get the mode
        void = max(mode_count[1,*], iMode)
        mode = array[mode_count[0,iMode]]
    
;---------------------------------------------------------------------
;Binning /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    endif else begin
    
        ;Sort the array so that unique values can be determined
        sortedarray = array[Sort(array)]
    
        ;Get the unique values
        arrayenum = sortedarray[Uniq(sortedarray)] 
    
        ;Match unique values with their duplicates, and map by index values
        mappedarray = Value_Locate(arrayenum, array)
    
        ;Group the index values together, using HISTOGRAM to count.
        hist = histogram(mappedarray, min=0)
    
        ;Return the bin with the maximum number of values.
        iMode = where(hist eq max(hist), nMode)
        mode = arrayenum[iMode]
        if nMode eq 1 then mode = mode[0]
        
    endelse
    
    return, mode
end


;---------------------------------------------------------------------
;Example Program (.r mrmode) /////////////////////////////////////////
;---------------------------------------------------------------------
array = [1.2, 0.1, 3.3, 0.1, 2.0, 3.3, 4.8, 1.2, 0.1, 0.1, 6.7, 3.3]
mode1 = MrMode(array)
mode2 = MrMode(array, METHOD=2)
print, FORMAT='(%"Method 1: The mode is: %f")', mode1
print, FORMAT='(%"Method 2: The mode is: %f")', mode2

end