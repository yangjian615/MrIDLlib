; docformat = 'rst'
;
; NAME:
;       MRINTERP_TS
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
;   The purpose of this program is to interpolate one set of time series data to the
;   time tags of a different time series. The interpolation reduces the higher resolution
;   data to the resolution of the data with the slowest sampling rate.
;
;   This program assumes that the sampling rate of the two data sets is unknown upon input;
;   therefore, `A_OUT` and `B_OUT` are output positional parameters that hold the results
;   of the interpolation. Say, for example, that `A` has the slowest sampling rate. It
;   will be copied directly from `A` to `A_OUT` while `B` will be interpolated, then
;   assigned to `B_OUT`. The same argument holds if you switch A <--> B.
;
;   `A` and `B` may be arrays of vectors (e.g., the X, Y, and Z position of an object at
;   each point in time -- a 3xN array).
;
;   NOTE:
;       The sampling rate is calculated as the average time between samples, which means
;       that if there are data gaps, it will not be calculated accurately.
;
; :Categories:
;
;       Math Utilities, Interpolation
;
; :Params:
;       A:                  in, required, type=numeric array
;                           Time series data whose time stamps are given by `T_A`.
;       B:                  in, required, type=numeric array
;                           Time series data whose time stamps are given by `T_B`.
;       T_A:                in, required, type=numeric array
;                           Time stamps for data set `A`.
;       T_B:                in, required, type=numeric array
;                           Time stamps for data set `B`.
;       A_OUT:              out, required, type=numeric array
;                           The time series data, `A`, after interpolation. If `A` has the
;                               slower sampling range, `A_OUT` will be an exact copy of `A`.
;       B_OUT:              out, required, type=numeric array
;                           The time series data, `B`, after interpolation. If `B` has the
;                               slower sampling range, `B_OUT` will be an exact copy of `B`.
;       T_OUT:              out, optional, type=numeric array
;
; :Keywords:
;       DT_A:               out, optional, type=numeric
;                           Calculated sampling rate for `T_A`.
;       DT_B:               out, optional, type=numeric
;                           Calculated sampling rate for 'T_B`.
;                           Time stamps of the interpolated data.
;       DT_OUT:             out, optional, type=numeric
;                           Sampling rate of `T_OUT`.
;       NO_COPY:            out, optional, type=boolean, default=0
;                           Instead of copying the non-interpolated data set to its output
;                               parameter, it will be moved, making the input parameters
;                               undefined upon exit.
;
; :Uses:
;   Uses the following external programs::
;       error_message.pro (Coyote Graphics)
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;       
;
; :History:
;   Modification History::
;       07/06/2013  -   Written by Matthew Argall
;       09/06/2013  -   Number of points is taken from the time arrays, since A and B may
;                           be arrays of more than one dimension. - MRA
;       09/20/2013  -   Renamed DT to DT_OUT to remove ambiguous keyword abbreviation. - MRA
;       08/23/2013  -   Time arrays must have at least two points to calculate the
;                           sampling interval. - MRA
;-
;*****************************************************************************************
pro MrInterp_TS, A, B, t_A, t_B, A_out, B_out, t_out, $
NO_COPY = no_copy, $
DT_OUT = dt_out, $
DT_A = dt_A, $
DT_B = dt_B
    compile_opt idl2
        
;---------------------------------------------------------------------
;Error Handling //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        
        ;If NO_COPY was set, then A is no longer defined. Must put the data back.
        if ptr_valid(pA) then begin
            if keyword_set(no_copy) then A = *pA
            ptr_free, pA
        endif
        
        if ptr_valid(pB) then begin
            if keyword_set(no_copy) then B = *pB
            ptr_free, pB
        endif
        
        if ptr_valid(pt_A) then begin
            if keyword_set(no_copy) then t_A = *pt_A
            ptr_free, pt_A
        endif
        
        if ptr_valid(pt_B) then begin
            if keyword_set(no_copy) then t_B = *pt_B
            ptr_free, pt_B
        endif
        
        ;Do not return anything.
        A_out = !Null
        B_out = !Null
        t_out = !Null
        
        return
    endif
        
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    nA = n_elements(t_A)
    nB = n_elements(t_B)
    if (n_elements(A) eq 0) then message, '"A" must be provided.'
    if (n_elements(B) eq 0) then message, '"B" must be provided.'
    if nA lt 2 then message, 't_A must be provided and have at least two elements.'
    if nB lt 2 then message, 't_B must be provided and have at least two elements.'
    
    ;Defaults
    no_copy = keyword_set(no_copy)
    
    ;Copy the data
    pA = ptr_new(A, NO_COPY=no_copy)
    pB = ptr_new(B, NO_COPY=no_copy)
    pt_A = ptr_new(t_A, NO_COPY=no_copy)
    pt_B = ptr_new(t_B, NO_COPY=no_copy)
        
;---------------------------------------------------------------------
;Sampling Rates //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Take the average time between samples
    dt_A = ((*pt_A)[nA-1] - (*pt_A)[0]) / nA
    dt_B = ((*pt_B)[nB-1] - (*pt_B)[0]) / nB
    
;---------------------------------------------------------------------
;Interpolate /////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Interpolate A to the time stamps of B if B is sampled less frequently (dt_B > dt_A)
    if dt_A lt dt_B then begin
        A_out = MrInterpol(*pA, *pt_A, *pt_B)
        B_out = *pB
        if arg_present(t_out) then t_out = *pt_B
        if arg_present(dt_out) then dt_out = dt_B
    
    ;Interpolate B to the time stamps of A
    endif else begin
        A_out = *pA
        B_out = MrInterpol(*pB, *pt_B, *pt_A)
        if arg_present(t_out) then t_out = *pt_A
        if arg_present(dt_out) then dt_out = dt_A
    endelse
    
;---------------------------------------------------------------------
;Cleanup /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ptr_free, pA, pB, pt_A, pt_B

end

