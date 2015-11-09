; doc_format = 'rst'
;
;+
;   Append a tag or structure to another structure or array of structures.
;   For a scalar structure, this is equivalent to calling IDL's
;   Create_Struct(). For an array of structures, `TAG` (and `VALUE`) are
;   added to a sample structure, then replicated, with values copied into
;   the new structure array.
;
; :Categories:
;   Structure Utilities
;
; :Params:
;       STRUCT:    in, required, type=structure/structarr
;                  A structure or array of structures to which a
;                      `TAG` is to be added.
;       ARGN:      in, required, type=string/struct
;                  Up to 18 additional arguments accepted by Create_Struct()
;
; :Returns:
;       SNEW:      A copy of `STRUCTURE`, but with the desired tags removed.
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
;       Copyright 2015 by Matthew Argall
;
; :History:
;   Modification History::
;       2015/10/23  -   Written by Matthew Argall
;-
function MrStruct_AddTags, struct,  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, $
                                   arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, $
                                   arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, $
                                   arg28, arg29, arg30
	compile_opt idl2
	on_error, 2

	nstruct = n_elements(struct)
		
	;Append the inputs 
	case n_params() of
		31: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30)
		30: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29)
		29: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28)
		28: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27)
		27: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26)
		26: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25)
		25: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24)
		24: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23)
		23: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22)
		22: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21)
		21: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20)
		20: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19)
		19: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18)
		18: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17)
		17: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16)
		16: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15)
		15: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14)
		14: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13)
		13: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12)
		12: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11)
		11: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10)
		10: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9)
		 9: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8)
		 8: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7)
		 7: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6)
		 6: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5)
		 5: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4)
		 4: snew = create_struct(struct[0],  arg1,  arg2,  arg3)
		 3: snew = create_struct(struct[0],  arg1,  arg2)
		 2: snew = create_struct(struct[0],  arg1)
		else: message, 'Must provide between 2 and 31 parameters.'
	endcase
		
	;Replicate the structure into an array
	if nstruct gt 1 then begin
		snew = replicate(snew, nstruct)
	
		;Copy old values
		struct_assign, struct, snew, /NOZERO
	endif

	return, snew
end