; docformat = 'rst'
;
; NAME:
;       MrMake_Array
;
; PURPOSE:
;+
;   A wrapper for IDL's Make_Array() function. Differences include::
;       1)  INCREMENT and START keywords work for all versions of IDL.
;       2)  TNAME keyword to specify type by type name (e.g., "Float").
;       3)  LAST keyword to specify the largest element in the array (similar to start:increment:end format)
;       4)  LOGSPACE keyword to create data that will be linear on a log-scaled axis.
;       5)  INCREMENT automatically sets INDEX=1.
;       6)  If START is given and INDEX=0, it is the same as specifying VALUE.
;       7)  Multiply by INCREMENT only if it is not equal to 1.
;       8)  START is added only if not equal to zero.
;       9)  START and INCREMENT is forced to be the same type as the requested TYPE.
;       10) A consequence of 9) is that data and math take place in the requested
;           type. In IDL's version, START and INCREMENT are applied to the data array
;           in double precision, then the array is converted to the specified type. In
;           this version, START and INCREMENT are applied in the specified type. The
;           following illustrates the difference in use:
;               a) Make_Array()
;                   IDL> array = Make_Array(10, START=5, INCREMENT=0.2, TYPE=2)
;               b) MrMake_Array()
;                   IDL> array = MrMake_Array(10, START=5.0, INCREMENT=0.2, TNAME='Float')
;                   IDL> array = Fix(array, TYPE=2)
;
; :Examples:
;   Make a 12-element byte array::
;       IDL> help, MrMake_Array(12, TNAME='Byte')
;           <Expression>    BYTE      = Array[12]
;
;       IDL> help, MrMake_Array(12, TYPE=1)
;           <Expression>    BYTE      = Array[12]
;
;       IDL> help, MrMake_Array(SIZE=Size(bytarr(12)))
;           <Expression>    BYTE      = Array[12]
;
;       IDL> help, MrMake_Array(DIMENSION=12, TNAME='Byte')
;           <Expression>    BYTE      = Array[12]
;
;       IDL> help, MrMake_Array(DIMENSION=12, /BYTE)
;           <Expression>    BYTE      = Array[12]
;
;   Make a 5-element byte array initialized to the value 6::
;       IDL> print, MrMake_Array(5, TNAME='Byte', VALUE=6B)
;          6   6   6   6   6
;
;       IDL> print, MrMake_Array(5, TNAME='Byte', START=6B)
;          6   6   6   6   6
;
;   Make a 5-element byte array, incrementing by 5 and starting at 15::
;       IDL> print, MrMake_Array(5, TNAME='Byte', INCREMENT=5, START=15)
;             15      20      25      30      35
;
;       IDL> print, MrMake_Array(5, TNAME='Byte', INCREMENT=5, START=15, /INDEX)
;             15      20      25      30      35
;
;   Make a 5-element float array, starting at 7 and stopping at 19::
;       IDL> print, MrMake_Array(5, START=7, LAST=19)
;             7.00000      10.0000      13.0000      16.0000      19.0000
;
;       IDL> print, MrMake_Array(START=7, LAST=19, INCREMENT=3)
;             7.00000      10.0000      13.0000      16.0000      19.0000
;
;   Make a 5-element float array, starting at 7 and incrementing by 3::
;       IDL> print, MrMake_Array(5, START=7, INCREMENT=3)
;             7.00000      10.0000      13.0000      16.0000      19.0000
;
;   Copy a structure -- equivalent to structarr = replicate(stuct, 5)::
;       IDL> help, MrMake_Array(5, VALUE={tag1: 'value1', tag2: 'value2'})
;             <Expression>    STRUCT    = -> <Anonymous> Array[5]
;
;   Create a log-scaled vector from 1 to 10::
;       IDL> print, MrMake_Array(START=0, LAST=1, INCREMENT=0.1, /LOGSPACE)
;             1.00000  1.25893  1.58489  1.99526  2.51189  3.16228  3.98107  5.01187  6.30957  7.94328  10.0000
;
; :Categories:
;       Array Utility
;
; :Params:
;       D1:         in, required, type=integer/intarr
;                   A vector of no more than 8 elements specifying the size of each
;                       dimension in the output array. If more than one element is
;                       provided, the other input parameters are ignored.
;       D2:         in, optional, type=integer
;                   Size of the second dimension of the output array.
;       D3:         in, optional, type=integer
;                   Size of the third dimension of the output array.
;       D4:         in, optional, type=integer
;                   Size of the fourth dimension of the output array.
;       D5:         in, optional, type=integer
;                   Size of the fifth dimension of the output array.
;       D6:         in, optional, type=integer
;                   Size of the sixth dimension of the output array.
;       D7:         in, optional, type=integer
;                   Size of the seventh dimension of the output array.
;       D8:         in, optional, type=integer
;                   Size of the eight dimension of the output array.
;
; :Keywords:
;       DIMENSION:  in, optional, type=intarr
;                   A vector of no more than 8 elements specifying the size of each
;                       dimension in the output array. If any input parameters are given,
;                       this keyword is ignored.
;       INCREMENT:  in, optional, type=double, default=1
;                   Multiply each array element by this value and then convert to `TYPE`.
;                       Setting this keyword will automatically set `INDEX` equal to 1.
;                       A value of 1.0 is equivalent to setting `INDEX` and not setting
;                       `INCREMENT` (i.e. no multiplication is performed.).
;       INDEX:      in, optional, type=boolean, defualt=0
;                   If set, array elements are set equal to their index value.
;       LAST:       in, optional, type=same as `TYPE`
;                   The maximum (minimum if negative) value in the array. If provided, the
;                       output array will have one dimension. If the dimensions are
;                       specified, then::
;                           `INCREMENT` = Fix( (`LAST` - `START`) / (Product(`D1`) - 1), TYPE=`TYPE`)
;                       otherwise, if `INCREMENT` is specified::
;                           `D1` = Floor((`LAST` - `START`) / `INCREMENT`) + 1
;                       The actual last element will be the largest multiple of `INCREMENT`
;                       less than or equal to LAST.
;       LOGSPACE:   in, optional, type=boolean, default=0
;                   If set, then take the log log of the requested array, Values will
;                       appear linear on a log-scaled graph.
;       SIZE:       in, optional, type=intarr
;                   An array returned by the Size() function specifying the dimensions
;                       and type of the output array. The dimensions are ignored if either
;                       `D1` or `DIMENSIONS` is provided. The type is ignored if either
;                       `TYPE` or `TNAME` are provided.
;       START:      in, optional, type=same as `TYPE`, defualt=0
;                   The value of the first element in the array. If `INDEX` is set, then
;                       setting this keyword is equivalent to adding a constant offset to
;                       each element. If `INDEX` is not set, then this is equivalent to
;                       setting the `VALUE` keyword. Addition is performed only for non-
;                       zero values.
;       TYPE:       in, optional, type=byte, default=4
;                   Data type of the output array. Ignored if `TNAME` is given.
;       TNAME:      in, optional, type=string, default=''
;                   Name of the data type of the output array.
;       VALUE:      in, optional, type=same as `TYPE`
;                   The value to initialize each element of the resulting array.
;
; :Returns:
;       ARRAY:      Returns an array of the specified type, dimensions, and initialization.
;
; :Version:
;   Tested on IDL versions::
;       8.2
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
;       02/28/2015  -   Written by Matthew Argall
;-
function MrMake_Array, d1, d2, d3, d4, d5, d6, d7, d8, $
BOOLEAN = boolean, $
DIMENSION = dimension, $
INCREMENT = increment, $
INDEX = index, $
LAST = last, $
LOGSPACE = logspace, $
SIZE = dsize, $
START = start, $
TYPE = type, $
TNAME = tname, $
VALUE = value, $
;TYPE Keywords
BOOLEAN  = tboolean, $
BYTE     = tbyte, $
INTEGER  = tinteger, $
LONG     = tlong, $
FLOAT    = tfloat, $
DOUBLE   = tdouble, $
COMPLEX  = tcomplex, $
STRING   = tstring, $
DCOMPLEX = tdcomplex, $
PTR      = tptr, $
OBJ      = obj, $
UINT     = tuint, $
ULONG    = tulong, $
LONG64   = tlong64, $
ULONG64  = tulong64
	compile_opt strictarr
	on_error, 2
	
	;IDL introduction of keywords
	;      BOOLEAN              v8.4
	;      INCREMENT            v8.3
	;      START                v8.2.1
	
	;Inputs that require MrMake_Array
	index      = keyword_set(index)
	tboolean   = keyword_set(boolean)
	logspace   = keyword_set(logspace)
	nIncrement = n_elements(increment)
	nStart     = n_elements(start)
	nLast      = n_elements(last)
	
	;Make smarter
	;   - Assume we want indexing if INCREMENT was given.
	;   - If INDEX=0, but START was given, make VALUE=START
	if nIncrement gt 0 then index = 1
	if nLast      gt 0 then index = 1
	if nStart gt 0 $
		then if index eq 0 then value = start $
		else _start = start
	
	;Other keywords
	tbyte     = keyword_set(tbyte)
	tinteger  = keyword_set(tinteger)
	tlong     = keyword_set(tlong)
	tfloat    = keyword_set(tfloat)
	tdouble   = keyword_set(tdouble)
	tcomplex  = keyword_set(tcomplex)
	tstring   = keyword_set(tstring)
	tdcomplex = keyword_set(tdcomplex)
	tptr      = keyword_set(tptr)
	tobj      = keyword_set(tobj)
	tuint     = keyword_set(tuint)
	tulong    = keyword_set(tulong)
	tlong64   = keyword_set(tlong64)
	tulong64  = keyword_set(tulong64)
	if tbyte + tinteger + tlong + tfloat + tdouble + tcomplex + tstring + tdcomplex + $
	   tptr  + tobj     + tobj  + tuint  + tulong  + tlong64  + tulong64 gt 1 $
		then message, 'Only one "type" keyword may be set.'

;-----------------------------------------------------
; Type \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	_tname = n_elements(tname) eq 0 ? '' : strupcase(tname)
	case strupcase(_tname) of
		'': ;Do nothing
		'BOOLEAN':  _type = 1
		'BYTE':     _type = 1
		'INT':      _type = 2
		'INTEGER':  _type = 2
		'LONG':     _type = 3
		'FLOAT':    _type = 4
		'DOUBLE':   _type = 5
		'COMPLEX':  _type = 6
		'STRING':   _type = 7
		'DCOMPLEX': _type = 9
		'PTR':      _type = 10
		'POINTER':  _type = 10
		'OBJ':      _type = 11
		'OBJECT':   _type = 11
		'OBJREF':   _type = 11
		'UINT':     _type = 12
		'ULONG':    _type = 13
		'LONG64':   _type = 14
		'ULONG64':  _type = 15
		else: message, 'TNAME "' + tname + '" is not accepted.'
	endcase
	if n_elements(_type) eq 0 then begin
		_type = n_elements(type)  gt 0 ? type              : $
		        tboolean               ? 1                 : $
		        tbyte                  ? 1                 : $
		        tinteger               ? 2                 : $
		        tlong                  ? 3                 : $
		        tfloat                 ? 4                 : $
		        tdouble                ? 5                 : $
		        tcomplex               ? 6                 : $
		        tstring                ? 7                 : $
		        tdcomplex              ? 9                 : $
		        tptr                   ? 10                : $
		        tobj                   ? 11                : $
		        tuint                  ? 12                : $
		        tulong                 ? 13                : $
		        tlong64                ? 14                : $
		        tulong64               ? 15                : $
		        n_elements(dsize) gt 0 ? dsize[dsize[0]+1] : $
		        4
	endif
	if max(_type eq [1,2,3,4,5,6,7,9,10,11,12,13,14,15]) eq 0 $
		then message, 'Invalid type (' + strtrim(_type, 2) + ') specified for result.'
	
;-----------------------------------------------------
; Size \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Make_Array does not like undefined parameters.
	;   - Turn multiple input parameters into a single parameter
	case 1 of
		n_elements(d1) eq 0: ;Do nothing
		n_elements(d1) gt 1: _d1 = d1
		n_elements(d1) eq 1: _d1 = d1
		n_elements(d2) eq 1: _d1 = [d1, d2]
		n_elements(d3) eq 1: _d1 = [d1, d2, d3]
		n_elements(d4) eq 1: _d1 = [d1, d2, d3, d4]
		n_elements(d5) eq 1: _d1 = [d1, d2, d3, d4, d5]
		n_elements(d6) eq 1: _d1 = [d1, d2, d3, d4, d5, d6]
		n_elements(d7) eq 1: _d1 = [d1, d2, d3, d4, d5, d6, d7]
		n_elements(d8) eq 1: _d1 = [d1, d2, d3, d4, d5, d6, d7, d8]
		else: message, 'All inputs must be scalars or D1 must be an array of no more than 8 elements.'
	endcase
	if n_elements(_d1) eq 0 then begin
		if n_elements(dimension) gt 0 then begin
			_d1 = dimension
		endif else if n_elements(dsize) gt 0 then begin
			_d1 = dsize[0] eq 0 ? dsize[2] : dsize[1:dsize[0]]
		endif else if nLast eq 0 then begin
			message, 'No dimensions specified for result.'
		endif
	endif
	
;-----------------------------------------------------
; Other Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	_increment = nIncrement         eq 0 ? fix(1, TYPE=_type) : increment
	_start     = n_elements(_start) eq 0 ? fix(0, TYPE=_type) : _start
	if nLast gt 0 then begin
		if n_elements(_d1) gt 0 $
			then increment = fix( (last - start) / (product(_d1) - 1), TYPE=_type) $
			else _d1       = floor((last - start) / increment) + 1
	endif

	;Make the difference between this and IDL's version obvious.
;		tInc   = size(increment, /TYPE)
;		tStart = size(_start, /TYPE)
;		if size(_increment, /TYPE) ne _type then $
;			message, "INCREMENT's type (" + strtrim(tInc, 2) + ' must be the specified type (' + strtrim(_type, 2) + ').'
;		if size(_start, /TYPE) ne _type then $
;			message, "START's type (" + strtrim(tStart, 2) + ' must be the specified type (' + strtrim(_type, 2) + ').'
	
;-----------------------------------------------------
; Make Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
	;Incremented array
	if index then begin
		array = make_array(_d1, /INDEX, TYPE=_type)
		if increment ne 1 then array *= increment
		if _start    ne 0 then array += _start
	
	;Non-incremented array
	endif else begin
		if size(value, /TNAME) eq 'STRUCT' $
			then array = make_array(_d1, VALUE=value) $
			else array = make_array(_d1, VALUE=value, TYPE=_type)
	endelse
	
	;Make boolean?
	if _type eq 1 && (tboolean || _tname eq 'BOOLEAN') then begin
		if MrCmpVersion('8.4') le 0 $
			then array = boolean(array) $
			else array = array gt 0
	endif
	
	;Log-spaced?
	if logspace then begin
		if min(array) gt 0 $
			then array = alog10(array) $
			else message, 'Min(result) must be > 0 if LOGSPACE is set.'
	endif

	return, array
end

