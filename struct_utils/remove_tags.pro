; doc_format = 'rst'
;
;+
;   The purpose of this program is to remove a tag and their values from a structure.
;
; :Categories:
;
;   Structure Utilities
;
; :Params:
;
;       STRUCTURE:              in, required, type=structure
;                               The structure for which tags are to be removed.
;       TAGS:                   in, required, type=string/strarr
;                               Tags to be removed from `STRUCTURE`.
;
; :Keywords:
;       ALL:                    in, optional, type=Boolean, default=0
;                               Recursively remove `TAGS` from all nested structures.
;       KEEP:                   in, optional, type=Boolean, default=0
;                               Indicate that  `TAGS` actually represents the complete
;                                   list of names to be kept. All other tags will be
;                                   removed.
;
; :Returns:
;       STRUCTURE:              A copy of `STRUCTURE`, but with the desired tags removed.
;
; :Uses:
;   Uses the following programs::
;       all_tags.pro
;       ismember.pro
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
;
;       Copyright 2013 by Matthew Argall
;
; :History:
;   Modification History::
;
;       10/16/2011  -   Written by Matthew Argall
;       03/22/2013  -   If all tags are removed, return the empty structure. - MRA
;-
function remove_tags, structure, tags, $
ALL = all, $
KEEP = keep
    compile_opt idl2
    on_error, 2
    
	;tag_names() returns upper case strings, so convert all tags to be removed
	;to uppercase.
	tags = strupcase(tags)
	ntags = n_elements(tags)

	;get the names of the tags within the given structure
	name_tags = tag_names(structure)
	n = n_elements(name_tags)

	;loop through all of the name_tags, creating a new structure that excludes
	;any matches with tags
	for ii=0, n-1 do begin
		check = where(tags eq name_tags[ii], nmatch)

		;if requested to keep the tags instead of remove them, we have to
		;switch nmatch
		if keyword_set(keep) then begin
			if nmatch then nmatch=0 else nmatch=1
		endif

		if nmatch eq 0 then begin

			;if requested, remove the tags from sub-structures as well
			;otherwise get the data for the tag
			if keyword_set(all) then begin
				;if the type of the element is a structure, then call remove_tags again
				;otherwise get the data for the tag
				sz = size(structure.(ii))
				if sz[sz[0]+1] eq 8 then begin
					tag_val = remove_tags(structure.(ii), tags, /all)
				endif else begin
					tag_val = structure.(ii)
				endelse
			endif else begin
				tag_val = structure.(ii)
			endelse

			;create a new structure with only the tags to be kept
			if n_elements(temp) eq 0 then begin
				temp = create_struct(name_tags[ii], tag_val)
			endif else begin
				temp = create_struct(temp, name_tags[ii], tag_val)
			endelse
		endif
	endfor

    ;If all tags were removed, then return an empty structure 
    if n_elements(temp) eq 0 then temp = {}
    
    
    return, temp
end