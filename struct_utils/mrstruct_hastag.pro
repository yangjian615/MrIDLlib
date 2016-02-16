; doc_format = 'rst'
;
; NAME:
;       MrStruct_HasTag
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
; The purpose of this program is to check if a particular tag exists within a structure.
;
; :Categories:
;   Structure Utilities
;
; :Examples:
;   See the main level program at the end of this file.
;       IDL> .r MrStruct_HasTag
;
; :Params:
;       STRUCT:                 in, required, type=structure
;                               The structure for which all tags are to be returned.
;       SEARCH_TAGS:            in, required, type=string/strarr
;                               Tags to search for within `STRUCT`.
;
; :Keywords:
;       RECURSE:                in, optional, type=Boolean, default=0
;                               Recurse into each substructure to search for the tag.
;       LEVEL:                  in, optional, type=int, default=!values.f_infinity
;                               The maximum recursion level. The root level is 0.
;
; :Returns:
;       TF_TAGS                 Array the same size as `SEARCH_TAGS` with 1's indicating
;                                   that the tag exists within `STRUCT` and 0's indicating
;                                   that the tags are not present.
;
; :Uses:
;   Uses the following programs::
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Copyright 2013 by Matthew Argall
;
; :History:
;   Modification History::
;       02/14/2013  -   Written by Matthew Argall
;       03/23/2013  -   Added LEVEL keyword. - MRA
;       2015/02/08  -   Renamed from Has_Tag to MrStruct_HasTag. - MRA
;-
function MrStruct_HasTag, struct, search_tags, $
RECURSE=recurse, $
LEVEL=level 
	compile_opt idl2
	on_error, 2

	;If the structure is a null structure (i.e. struct = {}), return 0
	if n_elements(struct) eq 0 then return, replicate(0, n_elements(search_tags))

	;make sure tags_in is uppercase (tag_names returns tags in all uppercase letters)
	search_tags = strupcase(search_tags)

	;Get the tags of the structure. Recurse through all structures if requested.
	if keyword_set(recurse) $
		then tags = MrStruct_AllTags(struct, LEVEL=level, TREE=0) $
		else tags = tag_names(struct)

	;Does the tag exist?
	tf_tag = MrIsMember(tags, search_tags)

	return, tf_tag
end



;---------------------------------------------------
; Main Level Example Program (.r has_tag) //////////
;---------------------------------------------------
;create a structure with multiple sub-structures
struct = {tagA: 'A', $
          tagB: 'B', $
          tagC: 'C', $
          tagD: {tagi:   'i', $
                 tagii:  'ii', $
                 tagiii: {tag_a: 'a', $
                          tag_b: 'b', $
                          tag_c: 'c'}, $
                 tagiv:  'iv', $
                 tagv:   'v'}, $
          tagE: 'E', $
          tagF: 'F'}

;Search for the following tags
search_tags = ['tagA', 'tag_b', 'aardvark', 'tagv']

;see if the structure has the desired tags
tf_tags   = MrStruct_HasTag(struct, search_tags)
tf_tags_r = MrStruct_HasTag(struct, search_tags, /RECURSE)

;print the results
print, '--------------------------------------------------'
print, format='(%"Search tags:                 %s, %s, %s, %s")', search_tags
print, format='(%"Has tags (w/o recursion)?:   %i, %i, %i, %i")', tf_tags
print, format='(%"Has tags (with recursion)?:  %i, %i, %i, %i")', tf_tags_r
print, format='(%"Tags in struct:              %s, %s, %s")',     search_tags[where(tf_tags_r eq 1)]
print, format='(%"Tags not in struct:          %s")',             search_tags[where(tf_tags_r ne 1)]
print, ''

end