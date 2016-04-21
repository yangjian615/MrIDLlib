; docformat = 'rst'
;
; NAME:
;       DETREND_DATA.PRO
;
; PURPOSE:
;+
;   The purpose of this program is to detrend a set of data by subtracting off a local
;   average. The averaging interval is centered on the data point. SMOOTH is used with
;   the /EDGE_TRUNCATE keyword.
;
;   Assuming DATA can be represented by a function representable by a Taylor series, then:
;
;       DATA = DATA_{0} + d(DATA)
;
;   where DATA_{0} is the uniform background, taken to be the average of DATA over NAVG
;   number of points, and d(DATA) is any fluctuation riding on top of it. The detrended
;   data, then, is given by:
;
;       DATA_DETREND = DATA - DATA_{0}
;
; :Categories:
;   Data Utilities
;
; :Params:
;
;       DATA:               in, required, type=any
;                           The data to be detrended.
;       NAVG:               in, required, type=int
;                           The number of points to average when calculating the
;                               background value.
;
; :Keywords:
;       BACKGROUND:         out, optional, type=Size(`DATA`\, /TYPE)
;                           The background values subtracted from `DATA`.
;       DIMENSION:          in, optional, tyep=int, default=1
;                           The dimension over which to detrend `DATA`.
;       EDGE_TRUNCATE:      in, optional, type=boolean, default=1
;                           If set, data will be edge-truncated. See IDL's SMOOTH function
;                               for details.
;       _REF_EXTRA:         in, optional, type=any
;                           Any additional keyword accepted by IDL's Smooth function
;
; :Returns:
;   
;       DATA_DETREND:       out, required, type=Size(`DATA`\, /TYPE)
;                           The detrended data.
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
;       06/13/2013  -   Written by Matthew Argall
;       2013-11-01  -   Added the EDGE_TRUNCATE keyword. - MRA
;-
function detrend_data, data, navg, $
 BACKGROUND = background, $
 DIMENSION = dimension, $
 EDGE_TRUNCATE = edge_truncate, $
_REF_EXTRA = extra
    compile_opt idl2
    on_error, 2
    
    ;Default to the leading dimension
    if n_elements(dimension)     eq 0 then dimension = 1
    if n_elements(edge_truncate) eq 0 then edge_truncate = 1
    
    ;Set the width so that Smooth acts over the proper dimension.
    dims  = size(data, /DIMENSIONS)
    ndims = size(data, /N_DIMENSIONS)
    width = lonarr(ndims)
    width[dimension-1] = navg
    
    ;Error with regard to smooth
    if navg gt dims[dimension-1] then begin
        message, string(navg, dims[dimension-1], $
                        FORMAT='(%"NDETREND (%i) must be less than length of data dimension (%i)")')
    endif
    
    ;Smooth the data to get a sense of the backgound
    background = smooth(data, width, EDGE_TRUNCATE=edge_truncate, _STRICT_EXTRA=extra)
    data_detrend = data - background

    return, data_detrend
end