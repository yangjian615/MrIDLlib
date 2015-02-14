; docformat = 'rst'
;
; NAME:
;       MrResolve_All
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
;   Resolve all routine return names of those routines that are subsequently compiled.
;   See IDL's `Resolve_All <http://exelisvis.com/docs/RESOLVE_ALL.html>`
;   
; :Categories:
;    Wrapper, System Utility
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
;   Change History::
;       2015/02/07  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Helper function for MrResolve_All. Parse routine names from the output of HELP.
;
; :Keywords:
;       COUNT:              out, optional, type=long
;                           Number of routine names parsed.
;       PROCEDURES:         in, optional, type=boolean
;                           If set, procedure names are parsed. This is the default if
;                               `FUNCTIONS` and `OBJECTS` are both 0.
;       FUNCTIONS:          in, optional, type=boolean, default=0
;                           If set, function names are parsed.
;       OBJECTS:            in, optional, type=boolean, default=0
;                           If set, object class names are parsed.
;-
function MrResolve_All_ParseHelp, $
COUNT=nRoutines, $
FUNCTIONS=functions, $
PROCEDURES=procedures, $
OBJECTS=objects
    compile_opt idl2
    on_error, 2
    
    functions  = keyword_set(functions)
    procedures = keyword_set(procedures)
    objects    = keyword_set(objects)
    if functions + procedures + objects eq 0 then procedures = 1
    
    ;More than one?
    if functions + procedures + objects gt 1 then begin
        ;Parse each one individually
        pros = MrResolve_All_ParseHelp(/PROCEDURES)
        funs = MrResolve_All_ParseHelp(/FUNCTIONS)
        objs = MrResolve_All_ParseHelp(/OBJECTS)
        
        ;Return them all
        return, [objs, pros, funs]
    endif
    
;-----------------------------------------------------
; Find Procedures \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the help
	if objects $
	    then help, OUTPUT=output, OBJECTS=objects, /BRIEF $
	    else help, OUTPUT=output, FUNCTIONS=functions, PROCEDURES=procedures, /BRIEF
	
	nOutput = n_elements(output)
	case 1 of
	    objects:    iStart = 0
	    procedures: iStart = 1
	    functions:  iStart = 1
	    else: message, 'One of OBJECTS, PROCEDURES and FUNCTIONS must be set.'
	endcase
	
	;Allocate memory
	nAlloc    = 100
	routines  = strarr(nAlloc)
	nRoutines = 0L
	
	;Step through each line of output
	for i = iStart, nOutput - 1 do begin
	    temp_routines = strsplit(output[i], ' ', /EXTRACT, COUNT=routineCount)
	    if routineCount eq 0 then continue
	    
	    ;Check for over-inflation
	    if nRoutines+routineCount ge nAlloc then begin
	        routines  = [routines, strarr(100)]
	        nAlloc     += 100
	    endif

	    ;Store the procedure names
	    routines[nRoutines:nRoutines+routineCount-1] = temp_routines
	    nRoutines += routineCount
	endfor
	routines = routines[0:nRoutines-1]

    return, routines
end


;+
;   Resolve (by compiling) any unresolved procedures or functions.
;
; :Keywords:
;       CLASSES:            out, optional, type=string/strarr
;                           Names of the classes that have been resolved.
;       FILES_CLASS:        out, optional, type=string/strarr
;                           File names associated with each compiled function. Empty
;                               strings represent elements of `CLASSES` that do not have
;                               an associated CLASSES[i] + '__define.pro' file name.
;       FILES_FUN:          out, optional, type=string/strarr
;                           File names associated with each compiled function. Empty
;                               strings represent elements of `FUNCTIONS` that do not have
;                               an associated FUNCTIONS[i] + '.pro' file name.
;       FILES_PRO:          out, optional, type=string/strarr
;                           File names associated with each compiled function. Empty
;                               strings represent elements of `FUNCTIONS` that do not have
;                               an associated PROCEDURES[i] + '.pro' file name.
;       FUNCTIONS:          out, optional, type=string/strarr
;                           Names of the functions that have been resolved.
;       METHOD_FUNS:        out, optional, type=string/strarr
;                           Names of the resolved function methods.
;       METHOD_PROS:        out, optional, type=string/strarr
;                           Names of the resolved procedures methods.
;       NFUNS:              out, optional, type=long
;                           Number of functions resolved.
;       NMETHODFUNS:        out, optional, type=long
;                           Number of function methods resolved.
;       NMETHODPROS:        out, optional, type=long
;                           Number of procedure methods resolved.
;       NPROS:              out, optional, type=long
;                           Number of procedures resolved.
;       NCLASS:             out, optional, type=long
;                           Number of classes resolved.
;       PROCEDURES:         out, optional, type=string/strarr
;                           Names of the procedures that have been resolved.
;       RESOLVE_CLASS:      in, optional, type=string/strarr
;                           Names of the classes to be resolved.
;       RESOLVE_PROCEDURES: in, optional, type=string/strarr
;                           Names of the procedures to be resolved.
;       RESOLVE_FUNCTIONS:  in, optional, type=string/strarr
;                           Names of the functions to be resovled.
;       SHOW:               in, optional, type=boolean, default=0
;                           If set, resolved procedures, functions, and classes will
;                               be printed to the command window.
;       SKIP_ROUTINES:      in, optional, type=string/strarr
;                           Names of the routines to skip while resolving.
;       UNRESOLVED:         out, optional, type=string/strarr
;                           Names of the unresolved routines. Routines are unresolved if
;                               their source code cannot be found and compiled. If not
;                               present, unresolved routine names will be printed to the
;                               command window with a warning message.
;-
pro MrResolve_All, $
CLASSES=classes, $
FILES_PRO=files_pro, $
FILES_FUN=files_fun, $
FILES_CLASS=files_class, $
FUNCTIONS=functions, $
IDL_ROOT=idl_root, $
METHOD_PROS=method_pros, $
METHOD_FUNS=method_funs, $
NCLASS=nClass, $
NFUNS=nFuns, $
NMETHODFUNS=nMethodFuns, $
NMETHODPROS=nMethodPros, $
NO_IDL_ROUTINES=no_idl_routines, $
NPROS=nPros, $
NUNRESOLVED=nUnresolved, $
PROCEDURES=procedures, $
RESOLVE_CLASS=resolve_class, $
RESOLVE_PROCEDURE=resolve_procedure, $
RESOLVE_FUNCTION=resolve_function, $
RESOLVE_EITHER=resolve_either, $
SHOW=show, $
SKIP_ROUTINES=skip_routines, $
UNRESOLVED=unresolved
	compile_opt strictarr
    on_error, 2
	
	;Show results?
	show            = keyword_set(show)
	no_idl_routines = keyword_set(no_idl_routines)
	get_files_class = arg_present(files_class)
	get_files_pro   = arg_present(files_pro)
	get_files_fun   = arg_present(files_fun)
	if n_elements(idl_root) eq 0 then idl_root = '/Applications/exelis'
	
	;If we are removing IDL routines, we must find file names
	if no_idl_routines then begin
	    get_files_class = 1
	    get_files_pro   = 1
	    get_files_fun   = 1
	endif
	
	;Resolve all that were given
	resolve_all, /CONTINUE_ON_ERROR, /QUIET, $
	             CLASS             = resolve_class, $
	             RESOLVE_EITHER    = resolve_either, $
	             RESOLVE_PROCEDURE = resolve_procedure, $
	             RESOLVE_FUNCTION  = resolve_function, $
	             SKIP_ROUTINES     = skip_routines, $
	             UNRESOLVED        = unresolved
	
;-----------------------------------------------------
; Fine Routine Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	procedures = MrResolve_All_ParseHelp(COUNT=nPros,  /PROCEDURES)
	functions  = MrResolve_All_ParseHelp(COUNT=nFuns,  /FUNCTIONS)
	classes    = MrResolve_All_ParseHelp(COUNT=nClass, /OBJECTS)
    
;-----------------------------------------------------
; Separate Classes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Find procedure and function methods
    iProMethod = where( ( strpos(procedures, '::') ne -1 ), nMethodPros, COMPLEMENT=iPro, NCOMPLEMENT=nPros)
    iFunMethod = where( ( strpos(functions,  '::') ne -1 ), nMethodFuns, COMPLEMENT=iFun, NCOMPLEMENT=nFuns)

    ;Remove methods
    if nMethodPros gt 0 then method_pros = procedures[iProMethod] else method_pros = ''
    if nMethodFuns gt 0 then method_funs = functions[iFunMethod]  else method_funs = ''
    if nPros       gt 0 then procedures  = procedures[iPro]       else procedures  = ''
    if nFuns       gt 0 then functions   = functions[iFun]        else functions   = ''
    
    ;Find class definitions
    iClassDef = where( ( strpos(procedures, '__DEFINE') ne -1 ), nMethodPros, COMPLEMENT=iPro, NCOMPLEMENT=nPros)
    
    ;Remove class definitions
    if nPros gt 0 then procedures = procedures[iPro] else procedures  = ''
    
    ;Scalars?
    if nPros       eq 1 then procedures  = procedures[0]
    if nFuns       eq 1 then functions   = functions[0]
    if nClass      eq 1 then classes     = classes[0]
    if nMethodPros eq 1 then method_pros = method_pros[0]
    if nMethodFuns eq 1 then method_funs = method_funs[0]
    
;-----------------------------------------------------
; Show Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if show then begin
        print, 'Compiled Classes:'
        print, '   ' + reform(classes, 1, nClass)
        print, 'Compiled Procedures:'
        print, '   ' + reform(procedures, 1, nPros)
        print, 'Compiled Functions:'
        print, '   ' + reform(functions, 1, nFuns)
    endif
    
;-----------------------------------------------------
; Unresolved Items \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    nUnresolved = n_elements(unresolved)
    if (nUnresolved gt 0) && (show || (arg_present(unresolved) eq 0) ) then begin
        message, 'Unresolved Routines:', /INFORMATIONAL
        print, '   ' + reform(unresolved, 1, nUnresolved)
    endif
    
;-----------------------------------------------------
; Get File Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Classes
    if get_files_class && nClass gt 0 then begin
        files_class = strarr(nClass)
        for i = 0, nClass - 1 do files_class[i] = file_which(strlowcase(classes[i]) + '__define.pro')
        if nClass eq 1 then files_class = files_class[0]
    endif
    
    ;Functions
    if get_files_fun && nFuns gt 0 then begin
        files_fun = strarr(nFuns)
        for i = 0, nFuns - 1 do files_fun[i] = file_which(strlowcase(functions[i]) + '.pro')
        if nFuns eq 1 then file_fun = file_fun[0]
    endif
    
    ;Procedures
    if get_files_pro && nPros gt 0 then begin
        files_pro = strarr(nPros)
        for i = 0, nPros - 1 do files_pro[i] = file_which(strlowcase(procedures[i]) + '.pro')
        if nPros eq 1 then file_pro = file_pro[0]
    endif
    
;-----------------------------------------------------
; Remove IDL Routines \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if no_idl_routines then begin
        ;Find non-IDL routines
        iClass = where(strpos(files_class, idl_root) eq -1, nClass)
        iPro   = where(strpos(files_pro,   idl_root) eq -1, nPros)
        iFun   = where(strpos(files_fun,   idl_root) eq -1, nFuns)
        
        ;Remove IDL routines
        if nClass gt 0 then classes    = classes[iClass]  else classes    = ''
        if nFuns  gt 0 then functions  = functions[iFun]  else functions  = ''
        if nPros  gt 0 then procedures = procedures[iPro] else procedures = ''
        
        ;Remove IDL routines from file name list
        if nClass gt 0 then files_class = files_class[iClass] else files_class = ''
        if nPros  gt 0 then files_pro   = files_pro[iPro]     else files_pro   = ''
        if nFuns  gt 0 then files_fun   = files_fun[iFun]     else files_fun   = ''
    endif
end