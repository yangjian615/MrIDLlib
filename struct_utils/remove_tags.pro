; doc_format = 'rst'
;
;+
;   The purpose of this program is to remove a tag and their values from a structure.
;
; :Examples:
;   Try the main level program at the end of this document::
;       IDL> .r remove_tags
;
; :Categories:
;   Structure Utilities
;
; :Params:
;       STRUCTURE:          in, required, type=structure
;                           The structure for which tags are to be removed.
;       TAGS:               in, required, type=string/strarr
;                           Tags to be removed from `STRUCTURE`.
;       CURRENT_LEVEL:      in, optional, private, type=integer
;                           Current recursion level.
;
; :Keywords:
;       KEEP:               in, optional, type=Boolean, default=0
;                           Indicate that `TAGS` actually represents the complete
;                               list of names to be kept. All other tags will be
;                               removed.
;       LEVEL:              in, optional, type=integer
;                           Level of recursion. Default is to only remove tags from the
;                               root (LEVEL=0). If `RECURSIVE` is set, the default is
;                               LEVEL=infinity. Setting LEVEL automatically sets
;                               `RECURSIVE`=1
;       RECURSIVE:          in, optional, type=boolean, default=0
;                           If set, `STRUCTURE` will be searched recursively for
;                               instances of `TAGS`.
;
; :Returns:
;       STRUCTURE:          A copy of `STRUCTURE`, but with the desired tags removed.
;
; :Uses:
;   Uses the following programs::
;       MrIsMember.pro
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
;       10/16/2011  -   Written by Matthew Argall
;       03/22/2013  -   If all tags are removed, return the empty structure. - MRA
;       2014/05/26  -   Removed ALL keyword. Added RECURSIVE and LEVEL keywords. - MRA
;-
function remove_tags, structure, tags, current_level, $
KEEP=keep, $
LEVEL=level, $
RECURSIVE=recursive
    compile_opt idl2
    on_error, 2
    
	;tag_names() returns upper case strings, so convert all tags to be removed
	;to uppercase.
	_tags     = strupcase(tags)
	nTags     = n_elements(tags)
	recursive = keyword_set(recursive)
	keep      = keyword_set(keep)
	if n_elements(current_level) eq 0 then current_level = 0
	if n_elements(level) eq 0 then begin
	    _level = recursive ? !values.f_infinity : 0
	endif else begin
	    _level    = level
	    recursive = 1
	endelse

	;get the names of the tags within the given structure
	name_tags  = tag_names(structure)
	nName_tags = n_elements(name_tags)

	;loop through all of the name_tags, creating a new structure that excludes
	;any matches with tags
	for ii=0, nName_tags-1 do begin
	    ;Check if the current take is to be removed
		check = where(_tags eq name_tags[ii], nmatch)

		;if requested to keep the tags instead of remove them, we have to
		;switch nMatch
		if keep then nMatch = ~nMatch

        ;Keep the tag?
		if nMatch eq 0 then begin
			;if requested, remove the tags from sub-structures as well
			;otherwise get the data for the tag
			if recursive && current_level lt _level then begin
				;if the type of the element is a structure, then call remove_tags again
				;otherwise get the data for the tag
				type = size(structure.(ii), /TNAME)
				if type eq 'STRUCT' $
					then tag_val = remove_tags(structure.(ii), tags, current_level+1, RECURSIVE=recursive, LEVEL=_level, KEEP=keep) $
					else tag_val = structure.(ii)

			endif else begin
				tag_val = structure.(ii)
			endelse

			;create a new structure with only the tags to be kept
			if n_elements(tag_val) gt 0 then begin
                if n_elements(temp) eq 0 $
                    then temp = create_struct(name_tags[ii], tag_val) $
                    else temp = create_struct(temp, name_tags[ii], tag_val)
            endif
		endif
	endfor

    ;If all tags were removed, then return an empty structure 
    if n_elements(temp) eq 0 then temp = {}
    
    current_level -= 1
    return, temp
end



;---------------------------------------------------
; Main Level Example Program (.r remove_tags) //////
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

;Remove tags at different recursion levels
rTags = ['tagA', 'tagv', 'tag_b']
kTags = ['tagA', 'tagD', 'tagiii', 'tagF']
struct_level0 = remove_tags(struct, ['tagA', 'tagv', 'tag_b'])
struct_level1 = remove_tags(struct, ['tagA', 'tagv', 'tag_b'], LEVEL=1)
struct_levelN = remove_tags(struct, ['tagA', 'tagv', 'tag_b'], /RECURSIVE)
struct_keep   = remove_tags(struct, ['tagA', 'tagD', 'tagiii', 'tagF'], /KEEP, /RECURSIVE)

print, '------------------------------------'
all = all_tags(struct)
print, 'Structure Tree'
print, transpose(all)
print, ''


;Print results
print, '------------------------------------'
tree0 = all_tags(struct_level0)
print, FORMAT='(%"Remove tags [%s, %s, %s] up to level 0")', rTags
print, transpose(tree0)


print, '------------------------------------'
tree1 = all_tags(struct_level1)
print, FORMAT='(%"Remove tags [%s, %s, %s] up to level 1")', rTags
print, transpose(tree1)


print, '------------------------------------'
treeN = all_tags(struct_levelN)
print, FORMAT='(%"Remove tags [%s, %s, %s] recursively")', rTags
print, transpose(treeN)


print, '------------------------------------'
treeKeep = all_tags(struct_keep)
print, FORMAT='(%"Keep only tags [%s, %s, %s, %s].")', kTags
print, '  Note: Tags without contents are removed.'
print, transpose(treeKeep)

end