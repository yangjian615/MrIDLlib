; docformat = 'rst'
;
;+
;   The purpose of this program is to convert time to distance::
;
;       d = \frac{v} {x0} \cdot (t - t_{0})
;
; :Categories:
;   Physics Utilties
;
; :Params:
;       TIME:               in, required, type=float
;                           Time array that is to be converted to distance.
;       VELOCITY:           in, required, type=float
;                           Velocity at which the target object is moving.
;       SCALE_SIZE:         in, optional, type=float, default=1.0
;                           Conversion of units from those given by `VELOCITY`. E.g., if
;                               `VELOCITY` is given in meters/second, and the output is
;                               suppose to be kilometers/second, set SCALE_SIZE=1e-3.
;       T0:                 in, optional, type=boolean, default=`TIME`[0]
;                           Reference time from which distance is to be measured.
;
; :Keywords:
;       RANGE:              out, optional, type=fltarr(2)
;                           [min, max] distance, `D`, calculated.
;
; :Returns:
;       D:                  Distance.
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
;       2013/11/19  -   Written by Matthew Argall
;-
function time_to_distance, time, velocity, scale_size, t0, $
RANGE=range
    compile_opt strictarr
    on_error, 2

    ;Reference time
	if n_elements(t0) eq 0 then t0 = time[0]

    ;Calculate the skin depth
	d = (time - t0) * (velocity / scale_size)
	
	;Calculate the data range?
	if arg_present(range) then range = [min(d, max=dMax), dMax]
	
	return, d
end