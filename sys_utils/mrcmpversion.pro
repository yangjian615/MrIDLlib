;+
; NAME:
;       MrIDLVersion
;
; PURPOSE:
;
;       The purpose of this program is to return the current version of IDL or to
;       test if the given version is older, newer, or the same as the current version.
;
; :Examples:
;   Compare version 8.0 to the current version::
;       print, MrCmpVersion('8.0')
;
;   Compare version 8.0 to version 8.0.1::
;       print, MrCmpVersion('8.0', '8.0.1')
;
;   For more examples, run the main level program::
;       IDL> .r MrCmpVersion
;
; :Categories:
;       Utility
;
; :Params:
;       VERSION1:           in, required, type=string
;                           The version of idl to be compared against `VERSION2`.
;       VERSION2:           in, optional, type=string, default=!version.release
;                           The version if IDL to be compared against `VERSION1`. If not
;                               provided, but `VERSION1` is, then the current version will
;                               be used: !Version.Release.
;                           
; :Returns:
;       RESULT:             Return::
;                               -1  - if `VERSION1` < `VERSION2`
;                                0  - if `VERSION1` = `VERSION2`
;                                1  - if `VERSION1` > `VERSION2`
;
; :Uses:
;   Uses the following external programs::
;       MrIDLVersion
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
;       2013/11/11  -   Written by Matthew Argall
;-
function MrCmpVersion, version1, version2
	compile_opt idl2
	on_error, 2
	
	;If VERSION2 was not given, use the current version
	if n_elements(version2) eq 0 then version2 = !version.release
	
	;Extract the version numbers
	v1 = fix(stregex(version1, '([0-9]+)\.([0-9]+)\.?([0-9]+)?', /EXTRACT, /SUBEXP))
	v2 = fix(stregex(version2, '([0-9]+)\.([0-9]+)\.?([0-9]+)?', /EXTRACT, /SUBEXP))
	
	;Version1 < Version2
	if (v1[1] lt v2[1]) || (v1[1] eq v2[1] and v1[2] lt v2[2]) || $
	   (v1[1] eq v2[1] and v1[2] eq v2[2] and v1[3] lt v2[3]) then return, -1
	
	;Version1 = Version2
	if v1[1] eq v2[1] and v1[2] eq v2[2] and v1[3] eq v2[3] then return, 0
	
	;Version1 > Version2
	if (v1[1] gt v2[1]) || (v1[1] eq v2[1] and v1[2] gt v2[2]) || $
	   (v1[1] eq v2[1] and v1[2] eq v2[2] and v1[3] gt v2[3]) then return, 1
end


;-----------------------------------------------------
;Main Level Example Program: IDL> .r MrCmpVersion) \\\
;-----------------------------------------------------
;Run a couple of tests
result1 = MrCmpVersion('1.2', '1.2.1')
result2 = MrCmpVersion('1.2', '1.3')
result3 = MrCmpVersion('1.2', '1.2')
result4 = MrCmpVersion('1.2', '1.1.2')
result5 = MrCmpVersion('1.2.1', '1.2')

;Print the Results
print, FORMAT='(%"Compare v1.2 to v1.2.1:  %2i")', result1
print, FORMAT='(%"Compare v1.2 to v1.3:    %2i")', result2
print, FORMAT='(%"Compare v1.2 to v1.2:    %2i")', result3
print, FORMAT='(%"Compare v1.2 to v1.1.2:  %2i")', result4
print, FORMAT='(%"Compare v1.2.1 to v1.2:  %2i")', result5

end