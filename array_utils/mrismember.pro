; docformat = 'rst'
;
; NAME:
;       MrIsMember
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
;+
;   The purpose of this program is to determine if the elements of and array, B,
;   are also elements of a different array, A, regardless of order.
;
; :Examples:
;
;       See Main-level program after end of function. From the IDL command line::
;           IDL> .r isMember
;
; :Params:
;
;       A:                  in, required, type=vector of any type except complex or NaN
;                           Array that is searched for matching values
;       B:                  in, required, type=scalar or vector of same type as A
;                           Determine if its elements can also be found in `A`
;       B_INDICES:          out, optional, type=Intarr
;                           The indices of the values of B that are members of A.
;
; :Keywords:
;
;       A_INDICES:          out, optional, type=Intarr
;                           The indices of the values of `A` that are members of `B`.
;       FOLD_CASE:          in, optional, type=Boolean, default=0
;                           Ignore case when inputs are strings.
;       REMOVE_SPACE:       in, optional, type=boolean, default=0
;                           If set, all spaces are removed from strings.
;       COUNT:              out, optional, type=int
;                           The number of values in B that are fount within `A`
;       COMPLEMENT:         out, optional, type=Intarr
;                           The indices of the values of `B` that are not members of `A`
;       NULL:               in, optional, type=boolean, default=0
;                           If set, then the !NULL system variable will be returned instead
;                               of -1 in the event that `COUNT` or `N_NONMEMEBERS` is
;                               zero.
;
; :Returns:
;
;       TF_ISMEMBER:        Has the same number of elements as B::
;                               1: if the element of `B` is in `A
;                               0: if the element of `B` is not in `A`
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
;       09/03/2012  -   Written by Matthew Argall
;       09/04/2012  -   Added NONMEMBERS keyword - MRA
;       02/13/2013  -   Added example to demonstrate use with strings. Check the number of
;                           non-members - MRA
;       02/16/2013  -   Added FOLD_CASE and A_INDICES keywords. Renamed INDICES to 
;                           B_INDICES - MRA
;       02/17/2013  -   Added N_MATCHES - MRA
;       03/06/2013  -   Fixed bug when A had one element. - MRA
;       03/21/2013  -   B_INDICES is now a parameter, not a keyword. - MRA
;       05/20/2013  -   If A has one element, make sure it is a scalar so that comparison
;                           with B is element-by-element. Added N_NONMEMBERS keyword. - MRA
;       08/09/2013  -   Return scalars instead of 1-element arrays -MRA
;       10/06/2013  -   A and B can be object references. Added NULL keyword. - MRA
;       10/11/2013  -   A_INDICES returns incides for all members, not just unique members,
;                           of A that are members of B. This is done by calling
;                           isMember again with the parameters reversed. Improved example. - MRA
;       2013/11/10  -   NULL keyword not accepted in WHERE() for IDL < 8.0. Fixed. - MRA
;       2013/11/22  -   Bracket overloading was throwing off object comparisons. Fixed. - MRA
;       2014/11/01  -   Added the REMOVE_SPACE keyword. - MRA
;       2014/11/11  -   Renamed from isMember.pro to MrIsMember.pro. Renamed keywords
;                           NONMEMBER_INDS to COMPLEMENT, N_NONMEMBERS to NCOMPLEMENT, and
;                           N_MATCHES to COUNT to be more like the Where() function. - MRA.
;       2016/02/14  -   Handle objects by comparing their heap identifiers. - MRA.
;-
function MrIsMember, A, B, B_indices, $
A_INDICES = a_indices, $
FOLD_CASE = fold_case, $
COUNT = count, $
NULL = null, $
COMPLEMENT = complement, $
NCOMPLEMENT = nComplement, $
REMOVE_SPACE = remove_space
    compile_opt strictarr
    on_error, 2

    A_type = size(A, /TNAME)
    null   = keyword_set(null)

    if null and !version.release lt '8.0' then begin
        message, 'IDL Version < 8.0. Setting NULL=0', /INFORMATIONAL
        null = 0
    endif
        
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;When no matches are found, return !Null if requested. Otherwise, return, -1
    if null then null_return = !Null else null_return = -1
    
    ;Editable copies
    AA = A
    BB = B

    ;STRINGS: make the search case-insensitive if requested.
    if (A_type eq 'STRING') then begin
        if keyword_set(fold_case) then begin
            AA = strupcase(A)
            BB = strupcase(B)
        endif
        
        if keyword_set(remove_space) then begin
            AA = strcompress(AA, /REMOVE_ALL)
            BB = strcompress(BB, /REMOVE_ALL)
        endif
        
    ;OBJECTS: Compare heap identifiers
    endif else if A_type eq 'OBJREF' then begin
        AA = obj_valid(AA, /GET_HEAP_IDENTIFIER)
        BB = obj_valid(BB, /GET_HEAP_IDENTIFIER)
    endif
    
    nAA = n_elements(AA)
    nBB = n_elements(BB)

;---------------------------------------------------------------------
;If A only has One Element ///////////////////////////////////////////
;---------------------------------------------------------------------
    if nAA eq 1 then begin
        ;Compare the members of B directly with A. Careful of objects
        ;with bracket overloading and comparisons with arrays with one
        ;elements.
        if MrIsA(AA, /SCALAR) $
            then tf_isMember = BB eq AA $
            else tf_isMember = BB eq AA[0]
        
;---------------------------------------------------------------------
;If A Has > 1 Element ////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;sort A
        AA_sorted = AA[sort(AA)]

        ;Use VALUE_LOCATE to find matches...
        ;  - VALUE_LOCATE rounds down if an exact match was not found.
        ;  - Check if the results of VALUE_LOCATE are exact matches of B.
        ;  - If BB < AA value locate returns -1. Return 0 instead.
        element = value_locate(AA_sorted, BB) > 0
        tf_isMember = AA_sorted[element] eq BB
    endelse

;---------------------------------------------------------------------
;Get Matching Indices ////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Pick out the indices corresponding to the values of B that are members of A. If there
    ;are none, return !Null
    if arg_present(B_indices) || arg_present(complement) || arg_present(nComplement) then begin
        B_Indices = where(tf_isMember eq 1, count, $
                          COMPLEMENT  = complement, $
                          NCOMPLEMENT = nComplement)
        if count eq 0 then B_Indices = null_return
    endif else begin
        count       = total(tf_isMember)
    endelse

    ;Which elements of A are contained within B?
    if arg_present(A_Indices) then begin
        tf_A_in_B = MrIsMember(B, A, A_Indices, $
                               FOLD_CASE    = fold_case, $
                               REMOVE_SPACE = remove_space)
    endif

    ;Return scalars instead of 1-element arrays
    if (n_elements(A_Indices)   eq 1) then A_Indices   = A_Indices[0]
    if (n_elements(B_Indices)   eq 1) then B_Indices   = B_Indices[0]
    if (n_elements(complement)  eq 1) then complement  = complement[0]
    if (nBB                     eq 1) then tf_isMember = tf_isMember[0]

    return, tf_isMember
end


;-----------------------------------------------------------
;MAIN level program to show utility. Run as IDL> .r ismember
;-----------------------------------------------------------

;EXAMPLE 1
;   Make two arrays. Find the values of B that are members of A.
A = [4,8,1,5,2,6,4,9,1,3,5,6,7,0,6]
B = [11,16,4,21,6,3]
are_members = ismember(A, B, indices, A_INDICES=A_Indices)

;print the arrays
print, '--------------------------------------------'
print, format='(%"Values of A:       [%d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d]")', A
print, format='(%"Values of B:       [%d, %d, %d, %d, %d, %d]")', B

;Show which are members and which are not.
;Print the values of B that are also members of A.
print, format='(%"B as members of A: [%d, %d, %d, %d, %d, %d]")', are_members
print, format='(%"Members of B in A: [%d, %d, %d]")', B[indices]
print, FORMAT='(%"Members of A in B: [%d, %d, %d, %d, %d, %d]")', A[A_Indices]
print, ''

;EXAMPLE 2
;   Show that it works for strings as well
A = ['a', 'h', 'd', 'b', 'e', 'c', 'g', 'f']
B = ['e', 'k', 'h', 'a']
are_members = ismember(A, B, indices, COMPLEMENT=complement)

;print the results
print, '--------------------------------------------'
print, format='(%"Members of A:          [%s, %s, %s, %s, %s, %s, %s, %s]")', A
print, format='(%"Members of B:          [%s, %s, %s, %s]")', B
print, format='(%"B as members of A:     [%i, %i, %i, %i]")', are_members
print, format='(%"Members of B in A:     [%s, %s, %s]")', B[indices]
print, format='(%"Members of B not in A: [%s]")', B[complement]

end