; doc_format = 'rst'
;
;+
;   The purpose of this program is to rename structure tags.
;
; :Examples:
;   Try the example program at the end of this document::
;       IDL> .r rename_tags
;
; :Categories:
;
;   Structure Utilities
;
; :Params:
;       STRUCTURE:          in, required, type=structure
;                           The structure for which tags are to be renamed.
;       OLD_NAMES:          in, required, type=string/strarr
;                           Unique tag names to be renamed.
;       NEW_NAMES:          in, required, type=string/strarr
;                           Unique tag names to replace `OLD_NAMES`. Must have the same
;                               number of elements as `OLD_NAMES`.
;       CURRENT_LEVEL:      in, optional, private, type=integer
;                           Current recursion level.
;
; :Keywords:
;       LEVEL:              in, optional, type=integer
;                           Level of recursion. Default is to only rename tags from the
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
; :History:
;   Modification History::
;       2014/05/26  -   Written by Matthew Argall
;-
function rename_tags, structure, old_names, new_names, current_level, $
LEVEL=level, $
RECURSIVE=recursive
    compile_opt idl2
    on_error, 2
    
	;tag_names() returns upper case strings, so convert all tags to be removed
	;to uppercase.
	_old      = strupcase(old_names)
	_new      = strupcase(new_names)
	nOld      = n_elements(old_names)
	recursive = keyword_set(recursive)
	keep      = keyword_set(keep)
	if n_elements(current_level) eq 0 then current_level = 0
	if n_elements(level) eq 0 then begin
	    _level = recursive ? !values.f_infinity : 0
	endif else begin
	    _level    = level
	    recursive = 1
	endelse
	
	;Make sure the tag names are unique
	if n_elements(uniq(old_names, sort(old_names))) ne nOld then $
	    message, 'OLD_NAMES must be unique.'
	if n_elements(uniq(new_names, sort(new_names))) ne n_elements(new_names) then $
	    message, 'NEW_NAMES must be unique.'

	;get the names of the tags within the given structure
	struct_tags  = tag_names(structure)
	nStruct_tags = n_elements(struct_tags)

	;loop through all of the name_tags, creating a new structure that excludes
	;any matches with tags
	for ii=0, nStruct_tags-1 do begin
	    ;Check if the current take is to be removed
		iCheck = where(_old eq struct_tags[ii], nMatch)

        ;Pick the proper structure name
        if nMatch eq 0 $
            then tag_name = struct_tags[ii] $
            else tag_name = _new[iCheck]
        
        ;Recurse?
        type = size(structure.(ii), /TNAME)
        if (type eq 'STRUCT') && recursive && (current_level lt _level) $
            then tag_val = rename_tags(structure.(ii), _old, _new, current_level+1, RECURSIVE=recursive, LEVEL=_level) $
            else tag_val = structure.(ii)
        
        ;Create the output structure
        if n_elements(temp) eq 0 $
            then temp = create_struct(tag_name, tag_val) $
            else temp = create_struct(temp, tag_name, tag_val)
    endfor
    
    ;Return    
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
oldTags = ['tagA', 'tagv', 'tag_b']
newTags = ['first_tag', 'I_am_Sam', 'Shh_Secret']
struct_level0 = rename_tags(struct, oldTags, newTags)
struct_level1 = rename_tags(struct, oldTags, newTags, LEVEL=1)
struct_levelN = rename_tags(struct, oldTags, newTags, /RECURSIVE)

print, '------------------------------------'
all = all_tags(struct)
print, 'Structure Tree'
print, transpose(all)
print, ''


;Print results
print, '------------------------------------'
tree0 = all_tags(struct_level0)
print, FORMAT='(%"Remove tags [%s, %s, %s] up to level 0")', oldTags
print, transpose(tree0)


print, '------------------------------------'
tree1 = all_tags(struct_level1)
print, FORMAT='(%"Rename tags [%s, %s, %s] up to level 1")', oldTags
print, transpose(tree1)


print, '------------------------------------'
treeN = all_tags(struct_levelN)
print, FORMAT='(%"Rename tags [%s, %s, %s] recursively")', oldTags
print, transpose(treeN)

end