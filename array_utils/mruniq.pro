; docformat = 'rst'
;
; NAME:
;       MRUNIQ
;
; PURPOSE:
;+
;       The purpose of this program is to serve as a wrapper for the UNIQ command.
;       Additions to the UNIQ command are::
;           - SORT keyword does the sorting automatically
;           - COMPLEMENT keyword returns non-uniq indices
;           - NUNIQ keyword returns the number of unique elements
;           - NCOMPLEMENT keyword returns the number of non-uniqe elements
;
; :Categories:
;       Wrapper, Array Utilities
;
; :Examples:
;       See the main level program at the end of this file::
;           IDL> .r MrUniq
;
; :Params:
;       ARRAY:              in, required, type=any
;                           The array to be scanned. It must be in ascending, monotonic
;                               order unless `INDEX` or `SORT` are provided.
;       INDEX:              in, optional, type=intarr
;                           An array of indices into `ARRAY` that order the elements into
;                               monotonic order.
;
; :Keywords:
;       COMPLEMENT:         out, optional, type=intarr
;                           The index values of the non-unique elements of `ARRAY`
;       IARRAY:             out, optional, type=intarr
;                           The location of each non-unique value of `ARRAY` in the array
;                               of unique values. If UNIQUE_VALUES = `ARRAY`[`IUNIQ`],
;                               then UNIQUE_VALUES[IARRAY] = `ARRAY`.
;       NAN                 in, optional, type=boolean, default=0
;                           If set, all NaN values will be excluded from `RESULT`. Instead,
;                               they will be counted in `COMPLEMENT` and `NCOMPLEMENT`.
;       NUNIQ:              out, optional, type=integer
;                           The number of unique elements in `ARRAY`
;       NCOMPLEMENT:        out, optional, type=intarr
;                           The number of non-unique values in `ARRAY`
;       SORT:               in, optional, type=boolean, default=0
;                           If set, `ARRAY` will be sorted into ascending, monotonic order.
;
; :Returns:
;       IUNIQ:              out, required, type=intarr
;                           The index-values of the uniq elements of `ARRAY`
;
; :Uses:
;   Uses the following external programs::
;       isMember.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2011
;
; :History:
;   Modification History::
;       05/26/2013  -   Written by Matthew Argall
;       10/06/2013  -   If INDEX is in the formal parameter list, but not defined, UNIQ
;                           was throwing an error "Variable is undefined: IDX." Fixed. - MRA
;       2013/11/16  -   Added the NAN keyword. - MRA
;       2015/04/29  -   Added the IARRAY keyword. - MRA
;       2015/05/04  -   Problem creating IARRAY with one unique value. Fixed. - MRA
;-
function MrUniq, array, index, $
COMPLEMENT = complement, $
IARRAY = iarray, $
NAN = nan, $
NUNIQ = nuniq, $
NCOMPLEMENT = ncomplement, $
SORT = sort
	compile_opt idl2
	on_error, 2

	;Get the result
	case keyword_set(sort) of
		0: if n_elements(index) gt 0 $
			then iuniq = uniq(array, index) $
			else iuniq = uniq(array)
		1: iuniq = uniq(array, sort(array))
	endcase

	;Remove NaNs
	if keyword_set(nan) then begin
		iFinite = where(finite(array[iuniq]), nuniq)
		if nuniq eq 0 $
			then iuniq = -1 $
			else iuniq = iuniq[iFinite]
	endif

	;Count the number of uniq values
	narr  = n_elements(array)
	nuniq = n_elements(iuniq)

	;Get the index values of the non-unique elements
	if arg_present(complement) or arg_present(ncomplement) then begin
		index_array = lindgen(narr)
		void = MrIsMember(iuniq, index_array, COMPLEMENT=complement, NCOMPLEMENT=ncomplement)
	endif

	;Locate non-unique members and associate them with their unique result
	if arg_present(iUniq) || arg_present(iArray) then begin
		if nuniq le 1 $
			then iArray = replicate(0, narr) $
			else iArray = value_locate(array[iuniq], array)
		iArray = iuniq[iarray]
	endif

	return, iuniq
end

    
;---------------------------------------------------------------------
;% Main-Level Example Program (.r MrUniq) ////////////////////////////
;---------------------------------------------------------------------

a = [1.0, 3.0, !values.f_nan, 2.0, 5.0, 6.0, 2.0, 7.0, !values.f_nan]
result1 = MrUniq(a, /SORT, NUNIQ=nuniq1, COMPLEMENT=notUniq1, NCOMPLEMENT=n_notUniq1)
result2 = MrUniq(a, /SORT, NUNIQ=nuniq2, COMPLEMENT=notUniq2, NCOMPLEMENT=n_notUniq2, /NAN)

print, FORMAT='(%"Array:                  [%3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f]")', a
print, FORMAT='(%"Unique Values:          [%3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f]")', a[result1]
print, FORMAT='(%"Unique Indices:         [%i, %i, %i, %i, %i, %i, %i, %i]")', result1
print, FORMAT='(%"# of Unique Indices:     %i")', nuniq1
print, FORMAT='(%"Non-Unique Values:      [%3.1f]")', a[notUniq1]
print, FORMAT='(%"Non-Unique Indices:     [%i")', notUniq1
print, FORMAT='(%"# of Non-Unique Indices: %i")', n_notUniq1

print, ''
print, ''
print, FORMAT='(%"Array:                  [%f, %f, %f, %f, %f, %f, %f, %f, %f]")', a
print, FORMAT='(%"Unique Values:          [%3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f]")', a[result2]
print, FORMAT='(%"Unique Indices:         [%i, %i, %i, %i, %i, %i]")', result2
print, FORMAT='(%"# of Unique Indices:     %i")', nuniq2
print, FORMAT='(%"Non-Unique Values:      [%3.1f, %3.1f, %3.1f]")', a[notUniq2]
print, FORMAT='(%"Non-Unique Indices:     [%i, %i, %i]")', notUniq2
print, FORMAT='(%"# of Non-Unique Indices: %i")', n_notUniq2

end