; doc_format = 'rst'
;
;+
;   The purpose of this program is to remove a tag and their values from a structure.
;
; :Examples:
;   Try the main level program at the end of this document::
;       IDL> .r MrStruct_RemoveTags
;
; :Categories:
;   Structure Utilities
;
; :Params:
;       STRUCTURE:          in, required, type=structure/structarr
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
;       10/16/2011  -   Written by Matthew Argall
;       03/22/2013  -   If all tags are removed, return the empty structure. - MRA
;       2014/05/26  -   Removed ALL keyword. Added RECURSIVE and LEVEL keywords. - MRA
;       2015/10/24  -   STRUCTURE can be an array of structures. - MRA
;-
function MrStruct_RemoveTags, structure, tags, current_level, $
KEEP=keep, $
LEVEL=level, $
NO_COPY=no_copy, $
RECURSIVE=recursive
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;tag_names() returns upper case strings, so convert all tags to be removed
	;to uppercase.
	nstruct   = n_elements(structure)
	_tags     = strupcase(tags)
	nTags     = n_elements(tags)
	recursive = keyword_set(recursive)
	keep      = keyword_set(keep)
	no_copy   = keyword_set(no_copy)
	if n_elements(current_level) eq 0 then current_level = 0
	if n_elements(level) eq 0 then begin
		_level = recursive ? !values.f_infinity : 0
	endif else begin
		_level    = level
		recursive = 1
	endelse

;-----------------------------------------------------
; Structure Information \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;get the names of the tags within the given structure
	name_tags  = tag_names(structure)
	nName_tags = n_elements(name_tags)

;-----------------------------------------------------
; Remove Tags 1-by-1 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
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
			;Get the value
			tag_val = structure[0].(ii)
			
			;if requested, remove the tags from sub-structures as well
			;otherwise get the data for the tag
			if recursive               && $
			   current_level lt _level && $
			   size(tag_val, /TNAME) eq 'STRUCT' $
			then begin
				;if the type of the element is a structure, then recurse
				;otherwise get the data for the tag
				tag_val = MrStruct_RemoveTags(tag_val, tags, current_level+1, $
				                              RECURSIVE = recursive, $
				                              LEVEL     = _level, $
				                              KEEP      = keep, $
				                              /NO_COPY)
			endif

			;Create a new structure with tags removed
			if n_elements(tag_val) gt 0 then begin
				if n_elements(temp) eq 0 $
					then temp = create_struct(name_tags[ii], temporary(tag_val)) $
					else temp = create_struct(temp, name_tags[ii], temporary(tag_val))
			endif
		endif
	endfor

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;If an array of structures was given, replicate element
	if nstruct gt 1 then begin
		temp = replicate(temp, nstruct)
		
		;Copy values from old to new
		if current_level gt 0 $
			then struct_assign, temporary(structure), temp, /NOZERO $
			else struct_assign, structure, temp, /NOZERO
	endif

	;If all tags were removed, then return an empty structure 
	if n_elements(temp) eq 0 then temp = {}

	current_level -= 1
	return, temp
end



;-------------------------------------------------------
; Main Level Example Program (.r MrStruct_RemoveTags) //
;-------------------------------------------------------
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
struct_level0 = MrStruct_RemoveTags(struct, rTags)
struct_level1 = MrStruct_RemoveTags(struct, rTags, LEVEL=1)
struct_levelN = MrStruct_RemoveTags(struct, rTags, /RECURSIVE)
struct_keep   = MrStruct_RemoveTags(struct, kTags, /KEEP, /RECURSIVE)

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

;-------------------------------------------------------
; Array of Structures //////////////////////////////////
;-------------------------------------------------------
struct = { a: 0B, $
           b: findgen(6), $
           c: 'Tag', $
           d: {x: intarr(10), y: findgen(10)} $
         }
struct = replicate(struct, 10)
struct = MrStruct_RemoveTags(struct, ['c', 'a'])

print, '------------------------------------'
help, struct
help, struct[0]


end