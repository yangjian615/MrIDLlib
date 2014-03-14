; docformat = 'rst'
;
;+
;   The purpose of this program is to test functionality of the SWITCH statement. The
;   ELSE clause seems to be happening no matter what.
;
;   RESOLUTION
;   The documentation says, "The ELSE statement is executed only if none of the preceding
;   statement expressions match." Despite this, a previous statement says, "If included,
;   it matches any selector expression, causing its code to be executed."
;
;   Essentially, the ELSE statement is always executed, no matter what. It will always
;   match the clause given.
;
;   Here is a post from the IDL news group saying that there is a "typo" in the help file
;   https://groups.google.com/forum/?fromgroups=#!searchin/comp.lang.idl-pvwave/switch$20statement/comp.lang.idl-pvwave/AxEH-WdPYi4/ZIJKORP-EcQJ
;
; :Categories:
;   Test Program
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;-
pro test_switch_expression

    a = [2]
    b = 2
    
    ;Pass a variable to SWITCH
    switch b of
        2: print, 'A scalar variable'
        1: print, 'seems to work fine.'
        else: message, 'No message is thrown.'
    endswitch
    
    ;Pass an expression to SWITCH
    switch a[0] of
        2: print, 'I am printing'
        1: print, 'because I equal 2'
        else: message, 'But this should not happen'
    endswitch
end