; docformat = 'rst'
;
; NAME:
;       MrTimeZoneNameToOffset.pro
;
;*****************************************************************************************
;   Copyright (c) 2014, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Convert an abbreviated time zone name to an offset in hours from UTC.
;
; :Examples:
;   Show all of the time zones::
;       IDL> void = MrTimeZoneNameToOffset(/SHOW_TZ)
;
;
;   Print the offset of Eastern Standard Time (EST) from UTC::
;       IDL> print, MrTimeZoneNameToOffset('EST')
;               -5
;
;   Print the offset of 'PST', 'MST', 'CST' and 'EST' from UTC::
;       IDL> print, MrTimeZoneNameToOffset(['PST', 'MST', 'CST', 'EST'])
;               -8 -7 -6 -5
;
;   Return the time zone information::
;       IDL> void = MrTimeZoneNameToOffset(KEY=key)
;
;
; :Categories:
;   Time Utilities
;
; :Params:
;       ABBR:           The abbreviated time zone name corresponding to offset.
;
; :Keywords:
;       KEY:            out, optional, type=4xN strarr
;                       An array of military letter designations, time zone abbreviations,
;                           offsets, and time zone names. The same information is printed
;                           when the `SHOW_TZ` keyword is set.
;       MILITARY:       in, optional, type=boolean, default=0
;                       If set, `ABBR` will be the military time code A-Z.
;       SHOW_TZ:        in, optional, type=boolean, default=0
;                       If set, time zone information will be printed to the command window.
;
; :Returns:
;       RESULT:         in, required, type=string
;                       The offset from Grenich Mean Time, starting with a "+" or "-'
;                           and followed by a number "0" through "12".
;
; :References:
;   http://www.timeanddate.com/time/zones/
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
;       2014/05/10  -   Written by Matthew Argall
;       2015/06/05  -   More comprehensive list of time zones. - MRA
;-
function MrTimeZoneNameToOffset, abbr, $
KEY=key, $
MILITARY=military, $
SHOW_TZ=show_tz
    compile_opt idl2
    on_error, 2
    
    show_tz  = keyword_set(show_tz)
    military = keyword_set(military)
    
    key = [['A', 'BST',   '+01', 'British Summer Time'], $
           ['A', 'CET',   '+01', 'Central European Time'], $
           ['A', 'FWT',   '+01', 'French Winter Time'], $
           ['A', 'IST',   '+01', 'Irish Standard Time'], $
           ['A', 'MET',   '+01', 'Middle Europe Time'], $
           ['A', 'MEWT',  '+01', 'Middle Europe Winter Time'], $
           ['A', 'SWT',   '+01', 'Swedish Winter Time'], $
           ['A', 'WAT',   '+01', 'West Africa Time'], $
           ['A', 'WEST',  '+01', 'Western European Summer Time'], $
           ['A', 'WST',   '+01', 'Western Sahara Summer Time'], $
           
           ['B', 'CAT',   '+02', 'Central Africa Time'], $
           ['B', 'CEST',  '+02', 'Central European Summer Time'], $
           ['B', 'EET',   '+02', 'Eastern European Time, USSR Zone 1'], $
           ['B', 'IST',   '+02', 'Israel Standard Time'], $
           ['B', 'SAST',  '+02', 'South Africa Standard Time'], $
           ['B', 'WAST',  '+02', 'West Africa Summer Time'], $
           
           ['C', 'ADT',   '+03', 'Arabia Daylight Time'], $
           ['C', 'AST',   '+03', 'Arabia Standard Time'], $
           ['C', 'BT',    '+03', 'Baghdad Time, USSR Zone 2'], $
           ['C', 'EAT',   '+03', 'Eastern Africa Time'], $
           ['C', 'EEST',  '+03', 'Eastern European Summer Time'], $
           ['C', 'FET',   '+03', 'Further'], $
           ['C', 'IDT',   '+03', 'Israel Daylight Time'], $
           ['C', 'MSK',   '+03', 'Moscow Standard Time'], $
           ['C', 'SYOT',  '+03', 'Syowa Time'], $
           
           ['D', 'AMT',   '+04', 'Armenia Time'], $
           ['D', 'AZT',   '+04', 'Azerbaijan Time'], $
           ['D', 'GET',   '+04', 'Georgia Standard Time'], $
           ['D', 'GST',   '+04', 'Gulf Standard Time'], $
           ['D', 'KUYT',  '+04', 'Kuybyshev Time'], $
           ['D', 'MSD',   '+04', 'Moscow Daylight Time'], $
           ['D', 'MUT',   '+04', 'Mauritius Time'], $
           ['D', 'RET',   '+04', 'Reunion Time'], $
           ['D', 'SAMT',  '+04', 'Samara Time'], $
           ['D', 'SCT',   '+04', 'Seychelles Time'], $
           ['D', 'ZP4',   '+04', 'USSR Zone 3'], $
           
           ['E', 'AMST',  '+05', 'Armenia Summer Time'], $
           ['E', 'AQTT',  '+05', 'Aqtobe Time'], $
           ['E', 'AZST',  '+05', 'Azerbaijan Summer Time'], $
           ['E', 'MAWT',  '+05', 'Mawson Time'], $
           ['E', 'MVT',   '+05', 'Maldives Time'], $
           ['E', 'ORAT',  '+05', 'Oral Time'], $
           ['E', 'PKT',   '+05', 'Pakistan Standard Time'], $
           ['E', 'TFT',   '+05', 'French Southern and Antarctic Time'], $
           ['E', 'TJT',   '+05', 'Tajikistan Time'], $
           ['E', 'TMT',   '+05', 'Turkmenistan Time'], $
           ['E', 'UZT',   '+05', 'Uzbekistan Time'], $
           ['E', 'YEKT',  '+05', 'Yekaterinburg Time'], $
           ['E', 'ZP5',   '+05', 'USSR Zone 4'], $
           
           ['F', 'ALMT',  '+06', 'Alma-Ata Time'], $
           ['F', 'BST',   '+06', 'Bangladesh Standard Time'], $
           ['F', 'BTT',   '+06', 'Bhutan Time'], $
           ['F', 'IOT',   '+06', 'Indian Chagos Time'], $
           ['F', 'KGT',   '+06', 'Kyrgyzstan Time'], $
           ['F', 'NOVT',  '+06', 'Novosibirsk Time'], $
           ['F', 'OMST',  '+06', 'Omsk Standard Time'], $
           ['F', 'QYZT',  '+06', 'Qyzylorda Time'], $
           ['F', 'VOST',  '+06', 'Vostok Time'], $
           ['F', 'YEKST', '+06', 'Yekaterinburg Summer Time'], $
           ['F', 'ZP6',   '+06', 'USSR Zone 5'], $

           ['G', 'CXT',   '+07', 'Christmas Island Time'], $
           ['G', 'DAVT',  '+07', 'Davis Time'], $
           ['G', 'HOVT',  '+07', 'Hovd Time'], $
           ['G', 'ICT',   '+07', 'Indochina Time'], $
           ['G', 'KRAT',  '+07', 'Krasnoyarsk Time'], $
           ['G', 'NOVST', '+07', 'Novosibirsk Summer Time'], $
           ['G', 'OMSST', '+07', 'Omsk Summer Time'], $
           ['G', 'WIB',   '+07', 'Western Indonesian Time'], $

           ['H', 'AWST',  '+08', 'Australian Western Standard Time'], $
           ['H', 'BNT',   '+08', 'Brunei Darussalam Time'], $
           ['H', 'CAST',  '+08', 'Casey Time'], $
           ['H', 'CCT',   '+08', 'China Coast Time, USSR Zone 7'], $
           ['H', 'CHOT',  '+08', 'Choibalsan Time'], $
           ['H', 'CST',   '+08', 'China Standard Time'], $
           ['H', 'HKT',   '+08', 'Hong Kong Time'], $
           ['H', 'IRKT',  '+08', 'Irkutsk Time'], $
           ['H', 'KRAST', '+08', 'Krasnoyarsk Summer Time'], $
           ['H', 'MYT',   '+08', 'Malaysia Time'], $
           ['H', 'PHT',   '+08', 'Philippine Time'], $
           ['H', 'SGT',   '+08', 'Singapore Time'], $
           ['H', 'ULAT',  '+08', 'Ulaanbaatar Time'], $
           ['H', 'WITA',  '+08', 'Central Indonesian Time'], $
           ['H', 'WST',   '+08', 'Western Standard Time'], $

           ['I', 'AWDT',  '+09', 'Australian Western Daylight Time'], $
           ['I', 'IRKST', '+09', 'Irkutsk Summer Time'], $
           ['I', 'JST',   '+09', 'Japan Standard Time, USSR Zone 8'], $
           ['I', 'KST',   '+09', 'Korea Standard Time'], $
           ['I', 'PWT',   '+09', 'Palau Time'], $
           ['I', 'TLT',   '+09', 'East Timor Time'], $
           ['I', 'WIT',   '+09', 'Eastern Indonesian Time'], $
           ['I', 'YAKT',  '+09', 'Yakutsk Time'], $

           ['K', 'AEST',  '+10', 'Australian Eastern Standard Time'], $
           ['K', 'CHUT',  '+10', 'Chuuk Time'], $
           ['K', 'ChST',  '+10', 'Chamorro Standard Time'], $
           ['K', 'DDUT',  '+10', "Dumont-d'Urville Time"], $
           ['K', 'GST',   '+10', 'Guam Standaard Time, USSR Zone 9'], $
           ['K', 'MAGT',  '+10', 'Magadan Time'], $
           ['K', 'PGT',   '+10', 'Papua New Guinea Time'], $
           ['K', 'SAKT',  '+10', 'Sakhalin Time'], $
           ['K', 'VLAT',  '+10', 'Vladivostok Time'], $
           ['K', 'YAKST', '+10', 'Yakutsk Summer Time'], $
           ['K', 'YAPT',  '+10', 'Yap Time'], $

           ['L', 'AEDT',  '+11', 'Australian Eastern Daylight Time'], $
           ['L', 'KOST',  '+11', 'Kosrae Time'], $
           ['L', 'LHDT',  '+11', 'Lord Howe Daylight Time'], $
           ['L', 'NCT',   '+11', 'New Caledonia Time'], $
           ['L', 'PONT',  '+11', 'Pohnpei Standard Time'], $
           ['L', 'SBT',   '+11', 'Solomon Islands Time'], $
           ['L', 'SRET',  '+11', 'Srednekolymsk Time'], $
           ['L', 'VLAST', '+11', 'Vladivostok Summer Time'], $
           ['L', 'VUT',   '+11', 'Vanuatu Time'], $

           ['M', 'ANAST', '+12', 'Anadyr Summer Time'], $
           ['M', 'ANAT',  '+12', 'Anadyr Time'], $
           ['M', 'FJT',   '+12', 'Fiji Time'], $
           ['M', 'GILT',  '+12', 'Gilbert Island Time'], $
           ['M', 'IDLE',  '+12', 'International Date Line East'], $
           ['M', 'MAGST', '+12', 'Magadan Summer Time'], $
           ['M', 'MHT',   '+12', 'Marshall Islands Time'], $
           ['M', 'NRT',   '+12', 'Nauru Time'], $
           ['M', 'NZST',  '+12', 'New Zealand Standard Time'], $
           ['M', 'PETST', '+12', 'Kamchatka Summer Time'], $
           ['M', 'PETT',  '+12', 'Kamchatka Time'], $
           ['M', 'TVT',   '+12', 'Tuvalu Time'], $
           ['M', 'WAKT',  '+12', 'Wake Time'], $
           ['M', 'WFT',   '+12', 'Wallis and Futuna Time'], $

           ['N', 'AZOT',  '-01', 'Azores Time'], $
           ['N', 'CVT',   '-01', 'Cape Verde Time'], $
           ['N', 'EGT',   '-01', 'East Greenland Time'], $
           ['N', 'WAT',   '-01', 'West Africa Time'], $

           ['O', 'BRST',  '-02', 'Brasilia Summer Time'], $
           ['O', 'FNT',   '-02', 'Fernando de Noronha Time'], $
           ['O', 'GST',   '-02', 'South Georgia Time'], $
           ['O', 'PMDT',  '-02', 'Pierre & Miquelon Daylight Time'], $
           ['O', 'UYST',  '-02', 'Uruguay Summer Time'], $
           ['O', 'WGST',  '-02', 'Western Greenland Summer Time'], $

           ['P', 'ADT',   '-03', 'Atlantic Daylight Time'], $
           ['P', 'AMST',  '-03', 'Amazon Summer Time'], $
           ['P', 'ART',   '-03', 'Argentina Time'], $
           ['P', 'BRT',   '-03', 'Brasilia Time'], $
           ['P', 'CLST',  '-03', 'Chile Summer Time'], $
           ['P', 'FKST',  '-03', 'Falkland Islands Summer Time'], $
           ['P', 'GFT',   '-03', 'French Guiana Time'], $
           ['P', 'PMST',  '-03', 'Pierre & Miquelon Standard Time'], $
           ['P', 'PYST',  '-03', 'Paraguay Summer Time'], $
           ['P', 'ROTT',  '-03', 'Rothera Time'], $
           ['P', 'SRT',   '-03', 'Suriname Time'], $
           ['P', 'UYT',   '-03', 'Uruguay Time'], $
           ['P', 'WARST', '-03', 'Western Argentine Summer Time'], $
           ['P', 'WGT',   '-03', 'West Greenland Time'], $

           ['Q', 'AMT',   '-04', 'Amazon Time'], $
           ['Q', 'AST',   '-04', 'Atlantic Standard Time'], $
           ['Q', 'BOT',   '-04', 'Bolivia Time'], $
           ['Q', 'CDT',   '-04', 'Cuba Daylight Time'], $
           ['Q', 'CLT',   '-04', 'Chile Standard Time'], $
           ['Q', 'EDT',   '-04', 'Eastern Daylight Time'], $
           ['Q', 'FKT',   '-04', 'Falkland Island Time'], $
           ['Q', 'GYT',   '-04', 'Guyana Time'], $
           ['Q', 'PYT',   '-04', 'Paraguay Time'], $

           ['R', 'ACT',   '-05', 'Acre Time'], $
           ['R', 'CDT',   '-05', 'Central Daylight Time'], $
           ['R', 'COT',   '-05', 'Colombia Time'], $
           ['R', 'CST',   '-05', 'Cuba Standard Time'], $
           ['R', 'EASST', '-05', 'Easter Island Summer Time'], $
           ['R', 'EAST',  '-05', 'Easter Island Standard Time'], $
           ['R', 'ECT',   '-05', 'Ecuador Time'], $
           ['R', 'EST',   '-05', 'Eastern Standard Time'], $
           ['R', 'PET',   '-05', 'Peru Time'], $

           ['S', 'CST',   '-06', 'Central Standard Time'], $
           ['S', 'GALT',  '-06', 'Galapagos Time'], $
           ['S', 'MDT',   '-06', 'Mountain Daylight Time'], $

           ['T', 'MST',   '-07', 'Mountain Standard Time'], $
           ['T', 'PDT',   '-07', 'Pacific Daylight Time'], $

           ['U', 'AKDT',  '-08', 'Alaska Daylight Time'], $
           ['U', 'PST',   '-08', 'Pitcairn Standard Time'], $
           ['U', 'PST',   '-08', 'Pacific Standard Time'], $

           ['V', 'AKST',  '-09', 'Alaska Standard Time'], $
           ['V', 'GAMT',  '-09', 'Gambier Time'], $
           ['V', 'HADT',  '-09', 'Hawaii-Aleutian Daylight Time'], $
           ['V', 'YST',   '-09', 'Yukon Standard Time'], $

           ['W', 'CKT',   '-10', 'Cook Island Time'], $
           ['W', 'HAST',  '-10', 'Hawaii-Aleutian Standard Time'], $
           ['W', 'TAHT',  '-10', 'Tahiti Time'], $

           ['X', 'NUT',   '-11', 'Niue Time'], $
           ['X', 'SST',   '-11', 'Samoa Standard Time'], $

           ['Y', 'AOE',   '-12', 'Anywhere on Earth'], $
           ['Y', 'IDLW',  '-12', 'International Date Line West'], $

           ['Z', 'AZOST', '+00', 'Azores Summer Time'], $
           ['Z', 'EGST',  '+00', 'Eastern Greenland Summer Time'], $
           ['Z', 'GMT',   '+00', 'Greenwich Mean Time'], $
           ['Z', 'UT',    '+00', 'Universal Time'], $
           ['Z', 'UTC',   '+00', 'Universal Coordinated Time'], $
           ['Z', 'WET',   '+00', 'Western European Time'], $
           ['Z', 'WT',    '+00', 'Western Sahara Standard Time']]

    ;Print all of the time zones?
    if show_tz then begin
        print, 'Military', 'Abbr', 'Offset', 'Name', $
                    FORMAT='(a8, 3x, a4, 3x, a6, 3x, a4)'
        print, key, FORMAT='(3x, a1, 7x, a-4, 5x, a3, 4x, a0)'
        return, ''
    endif
    
    ;Was something given?
    if n_elements(abbr) eq 0 then begin
        if arg_present(key) $
            then return, '' $
            else message, 'Usage: offset = MrTimeZoneNameToOffset(abbr)'
    endif

    ;Military
    if military then begin
        ;Search for the letter
        iOffset = value_locate(key[0,*], strupcase(abbr))
        result  = reform(key[2, iOffset])
        
    ;Standard Abbrs
    endif else begin
        ;Alphabetize by abbreviated name
        iSort   = sort(key[1,*])
        
        ;Search for the offsets
        iOffset = value_locate(key[1,iSort], strupcase(abbr))
        iOffset = iSort[iOffset]
        
        ;Pick the offsets
        result  = reform(key[2,iOffset])
    endelse

    if n_elements(abbr) eq 1 then result = result[0]
    return, result
end







