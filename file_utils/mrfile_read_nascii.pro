; docformat = 'rst'
;
; NAME:
;    MrFile_Read_nAscii
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Read data from multiple ASCII files into a data structure.
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
;       2015/06/28  -   Written by Matthew Argall.
;-
;*****************************************************************************************
;+
;  Read multiple ascii files.
;
; :Params:
;    FILENAME:          in, optional, type=string, default=''
;                       Filename of file to read. If no file is given or the empty string,
;                           a dialog box will appear asking to pick a file. Furthermore,
;                           if `TEMPLATE`, `COLUMN_NAMES`, `COLUMN_TYPES` and `GROUP` are
;                           all undefined, Ascii_Template() will be called to assist in
;                           reading the file.
;
; :Keywords:
;    COUNT:             out, optional, type=long
;                       Set to a named variable to get the number of records read
;    HASH:              in, optional, type=boolean, default=0
;                       If set, `DATA` will be returned as a hash instead of a structure.
;                           The hash keys will be the same as `COLUMN_NAMES`.
;    TEMPLATE:          in, out, opitonal, type=structure
;                       An template returned by Ascii_Template(). If provided, all of the
;                           keywords related to Ascii_Template will be ignored. Upon exit,
;                           the template used to read the ascii file will be returned via
;                           this keyword.
;    REF_EXTRA:         in, out, opitonal, type=structure
;                       Any keyword accepted by MrFile_Read_Ascii is also accepted
;                           via keyword inheritance.
;
; :Returns:
;    DATA:              A data structure (or hash if `HASH` is set) with each field (key)
;                           containing a `GROUP` of data. If dialog boxes were cancelled,
;                           -1 will be returned.
;-
function MrFile_Read_nAscii, filename, $
COUNT=count, $
TEMPLATE=template, $
HASH=to_hash, $
_REF_EXTRA=extra
	compile_opt strictarr
	on_error, 2
	
;---------------------------------------------------------------------
; Read First File ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Number of files given
	nFiles  = n_elements(filename)
	to_hash = keyword_set(to_hash)

	;Read the first data file
	data = MrFile_Read_Ascii(filename[0], $
	                         COUNT         = count, $
	                         TEMPLATE      = template, $
	                         _STRICT_EXTRA = extra)
	
	;Name and number of tags
	fields  = tag_names(data)
	nFields = n_tags(data)
	
;---------------------------------------------------------------------
; Read First File ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Step through each subsequent file
	for i = 1, nFiles - 1 do begin
		;Read the file
		temp = MrFile_Read_Ascii(filename[i], $
		                         COUNT         = temp_count, $
		                         TEMPLATE      = template, $
		                         _STRICT_EXTRA = extra)
		                         
		;Increase total count
		count += temp_count

	;---------------------------------------------------------------------
	; Concatenate Data ///////////////////////////////////////////////////
	;---------------------------------------------------------------------
		for j = 0, nFields - 1 do begin
			;Number of data points
			sz    = size(data.(j))
			nDims = sz[0]
			dims1 = sz[1:nDims]
			type  = sz[sz[0]+1]

			;Get dimensions of new file.
			dims2 = size(temp.(j), /DIMENSIONS)
			
			;New size of array
			if nDims eq 1 $
				then newDims = dims1 + dims2 $
				else newDims = [dims1[0], dims1[1] + dims2[1]]
			
			;Create the field
			if j eq 0 $
				then data_tmp = create_struct(fields[j], make_array(newDims, TYPE=type)) $
				else data_tmp = create_struct(data_tmp, fields[j], make_array(newDims, TYPE=type))
			
			;Set the data
			if nDims eq 1 $
				then data_tmp.(j) = [data.(j), temp.(j)] $
				else data_tmp.(j) = [[data.(j)], [temp.(j)]]
		endfor
		
		;Update data
		temp = -1
		data = temporary(data_tmp)
	endfor

	;Return a hash or structure?
	if to_hash $
		then return, hash(data, /EXTRACT) $
		else return, data
end