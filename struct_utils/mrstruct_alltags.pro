; doc_format = 'rst'
;
;+
; The purpose of this program is to return all of the tag values of a structure.
; The main structure is called "root". Structures within "root" will have their name
; appended to "root", separated by a dot, ".". Thus::
;
;       my_struct.sub_struct
;
; will be called "root.sub_struct"
;
; Essentially, this is a recursive extension of IDL's tag_names.pro.
;
; :Categories:
;   Structure Utilities
;
; :Examples:
;
;   See the main level program at the end of this file::
;       IDL> .r all_tags
;
; :Params:
;       STRUCT:                 in, required, type=structure
;                               The structure for which all tags are to be returned.
;       ROOT:                   in, optional, type=int, default='root'
;                               The name to use as the root when `TREE` is set.
;       CURRENT_LEVEL:          hidden, in, required
;                               The current recursion level.
;
; :Keywords:
;       TREE:                   in, optional, type=Boolean, default=1
;                               Append the parent structures to the tag name. This is the
;                                   default. Structure names are separated by dots (.).
;                                   If only the tag names are desired, then set TREE=0.
;       LEVEL:                  in, optional, type=int, default=!values.f_infinity
;                               The maximum recursion level. The root level is 0.
;
; :Returns:
;       TAGS:                   A string array of tag names. If a tag is also a structure,
;                                   struct2, the tags of struct2 will be appended to the
;                                   structure name with a dot, ".".
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
;       Copyright 2013 by Matthew Argall
;
; :History:
;   Modification History::
;
;       02/15/2013  -   Written by Matthew Argall
;       03/23/2013  -   Added CURRENT_LEVEL parameter and LEVEL keyword. - MRA
;       2016/02/08  -   Renamed from All_Tags to MrStruct_AllTags - MRA
;-
function MrStruct_AllTags, struct, root, current_level, $
TREE = tree, $
LEVEL=level
	compile_opt idl2
	on_error, 2

;---------------------------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
	;default to recursing to all levels.
	if n_elements(level) eq 0 then level = !values.f_infinity

	;Start at level 1. Return if we are beyond the max level
	if n_elements(current_level) eq 0 then current_level=0
	if current_level gt level then begin
		current_level -= 1
		return, !Null
	endif

	;Make sure a structure was given
	if size(struct, /TYPE) ne 8 then message, 'struct must be a structure'
	
	;If no root was given, use the empty string
	if n_params() eq 1 then root = ''
	
	;Default to returning the structure tree
	if n_elements(tree) eq 0 then tree = 1
    
;---------------------------------------------------------------------
;Get the Tags \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
	
	;get the number of tags in the structure
	tags = tag_names(struct)
	ntags = n_elements(tags)
	
	;allocate memory to the output array
	tags_out = strarr(ntags)
	
	;If a structure tree is to be appended to the tag name, then do that
	if keyword_set(tree) then begin
		if strcmp(root, '') $
			then tags = 'ROOT.' + tags $
			else tags = root + '.' + tags
	endif

	;loop through all of the tags
	for i = 0, ntags-1 do begin
		if size(struct.(i), /TNAME) eq 'STRUCT' then begin
			temp_tags = all_tags(struct.(i), tags[i], current_level+1, TREE=tree, LEVEL=level)
			tags = [tags, temporary(temp_tags)]
		endif
	endfor

	current_level -= 1
	return, tags
end


;---------------------------------------------------
; Main Level Example Program (.r all_tags) /////////
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

;Get all of the tags
tags_w_tree = MrStruct_AllTags(struct)
tags_alone  = MrStruct_AllTags(struct, TREE=0)
tags_level1 = MrStruct_AllTags(struct, LEVEL=1)

;Print the results
print, '======TAGS WITH TREE======='
print, transpose(tags_w_tree)
print, ''

print, '======TAGS WITHOUT TREE======='
print, transpose(tags_alone)
print, ''

print, '======TAGS TO LEVEL 1======='
print, transpose(tags_level1)

end

