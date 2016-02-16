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
;                  Up to 100 additional arguments accepted by Create_Struct(), e.g.
;                      50 tag-value pairs.
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
;       2016/01/23  -   Increased number of parameters to accept 50 tag-value pairs.
;-
function MrStruct_AddTags, struct,  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
                                   arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
                                   arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
                                   arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
                                   arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
                                   arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
                                   arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
                                   arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
                                   arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
                                   arg91, arg92, arg93, arg94, arg95, arg96, arg97, arg98, arg99, arg100
	compile_opt idl2
	on_error, 2

	nstruct = n_elements(struct)
		
	;Append the inputs 
	case n_params() of
		101: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93, arg94, arg95, arg96, arg97, arg98, arg99, arg100)
		100: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93, arg94, arg95, arg96, arg97, arg98, arg99)
		99: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93, arg94, arg95, arg96, arg97, arg98)
		98: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93, arg94, arg95, arg96, arg97)
		97: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93, arg94, arg95, arg96)
		96: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93, arg94, arg95)
		95: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93, arg94)
		94: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92, arg93)
		93: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91, arg92)
		92: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90, $
		                                    arg91)
		91: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90)
		90: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89)
		89: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88)
		88: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86, arg87)
		87: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85, arg86)
		86: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84, arg85)
		85: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83, arg84)
		84: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82, arg83)
		83: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81, arg82)
		82: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, $
		                                    arg81)
		81: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80)
		80: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79)
		79: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78)
		78: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76, arg77)
		77: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75, arg76)
		76: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74, arg75)
		75: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73, arg74)
		74: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72, arg73)
		73: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71, arg72)
		73: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, $
		                                    arg71)
		72: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70)
		70: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69)
		69: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68)
		68: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66, arg67)
		67: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65, arg66)
		66: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64, arg65)
		65: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63, arg64)
		64: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62, arg63)
		63: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61, arg62)
		62: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, $
		                                    arg61)
		61: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60)
		60: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59)
		59: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58)
		58: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56, arg57)
		57: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55, arg56)
		56: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54, arg55)
		55: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53, arg54)
		54: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52, arg53)
		53: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51, arg52)
		52: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, $
		                                    arg51)
		51: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                                    arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                                    arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, $
		                                    arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50)
		50: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42, $
		                                    arg43, arg44, arg45, arg46, arg47, arg48, $
		                                    arg49)
		49: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42, $
		                                    arg43, arg44, arg45, arg46, arg47, arg48)
		48: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42, $
		                                    arg43, arg44, arg45, arg46, arg47)
		47: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42, $
		                                    arg43, arg44, arg45, arg46)
		46: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42, $
		                                    arg43, arg44, arg45)
		45: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42, $
		                                    arg43, arg44)
		44: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42, $
		                                    arg43)
		43: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41, arg42)
		42: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40, arg41)
		41: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39, arg40)
		40: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38, arg39)
		39: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37, arg38)
		38: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36, $
		                                    arg37)
		37: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35, arg36)
		36: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34, arg35)
		35: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33, arg34)
		34: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32, arg33)
		33: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31, arg32)
		32: snew = create_struct(struct[0],  arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                                     arg7,  arg8,  arg9, arg10, arg11, arg12, $
		                                    arg13, arg14, arg15, arg16, arg17, arg18, $
		                                    arg19, arg20, arg21, arg22, arg23, arg24, $
		                                    arg25, arg26, arg27, arg28, arg29, arg30, $
		                                    arg31)
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
		else: message, 'Incorrect number of parameters.'
	endcase
		
	;Replicate the structure into an array
	if nstruct gt 1 then begin
		snew = replicate(snew, nstruct)
	
		;Copy old values
		struct_assign, struct, snew, /NOZERO
	endif

	return, snew
end