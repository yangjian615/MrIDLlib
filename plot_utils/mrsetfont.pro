; docformat = 'rst'
;
; NAME:
;       MrSetFont
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
; PURPOSE:
;+
;   The purpose of this program is to provide a general means of setting the font.
;
; :Examples:
;   Try the main level program at the end of this document::
;       IDL> .r MrSetFont
;
;   View a particular Hershey font::
;       MrSetFont, 'Gothic Italian', /HERSHEY, /SHOWFONT
;
;   View a particular True-Type font::
;       MrSetFont, 'DejaVuSans', /TRUETYPE, /SHOWFONT
;
; :Categories:
;       Graphics, Plot Utility
;
; :Uses:
;   Uses the following external programs::
;       cgTT_Font.pro
;       cgText.pro (only for examples)
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/04/21  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Utility program for setting the current Hershey font.
;
; :Params:
;-
pro MrSetFont_Hershey, fontName, $
GET_FONTNAMES=get_fontnames, $
GET_FONTNUM=get_fontnum, $
SHOWFONTS=showfonts
    compile_opt strictarr
    on_error, 2
    
    ;Font information
    common _MR_SETFONT_, sFont, ps_fonts, ps_styles
    
    ;Default font
    if n_elements(fontName) eq 0 then fontName = 'Simplex Roman'
    
    ;Available Hershey fonts
    fonts = ['', '', 'SIMPLEX ROMAN', 'SIMPLEX GREEK', 'DUPLEX ROMAN', 'COMPLEX ROMAN', $
             'COMPLEX ROMAN', 'COMPLEX ITALIC', 'MATH AND SPECIAL', '', 'GOTHIC ENGLISH', $
             'SIMPLEX SCRIPT', 'COMPLEX SCRIPT', 'GOTHIC ITALIAN', 'GOTHIC GERMAN', $
             'CYRILLIC', 'TRIPLEX ROMAN', 'TRIPLEX ITALIAN', '', 'MISCELLANEOUS']
             
    ;Font index
    if size(fontName, /TNAME) eq 'STRING' then begin
        fontIndex = where(fonts eq strupcase(fontName), count)
        if count ne 1 then message, 'No font names match "' + fontName + '".'
        fontIndex = fontIndex[0]
        _fontName = fontName
    endif else begin
        fontIndex = fontName
        _fontName = fonts[fontIndex]
    endelse

;---------------------------------------------------------------------
; Get Font Names? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(get_fontnames) then begin
        get_fontnames = fonts
        get_fontnum   = n_elements(fonts)
        return
    endif

;---------------------------------------------------------------------
; Show the Font? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(showfonts) then begin
        ;IDL's default filename
        if !d.name eq 'ps' then begin
            cd, CURRENT=current
            idl_ps_name = filepath('idl.ps', ROOT_DIR=current)
        
            ;Input filename
            if n_elements(filename) eq 0 then filename = idl_ps_name
            _filename = cgRootName(filename, DIRECTORY=directory, EXTENSION=extension)
            _filename = filepath(_filename + '.' + extension, ROOT_DIR=directory)
        endif

        ;Show the fonts
        showfont, fontIndex, _fontName, _STRICT_EXTRA=extra
        
        ;Rename the file
        if !d.name eq 'PS' then begin
            if filename ne idl_ps_name then file_move, idl_ps_name, filename
            print, 'PS Font file saved to: ' + _filename
        endif
        
        return
    endif

;---------------------------------------------------------------------
; Set Font ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Make the current font the old font
    sFont.old_font          = sFont.font
    sFont.old_hers_fontname = sFont.hers_fontname
    
    ;Set the current font
    sFont.font          = -1
    sFont.hers_fontName = _fontName

    ;Set the font by writing something to an invisible pixmap window
    case !d.name of
        'WIN': begin
            theWin = MrGetWindow(/FREE, /PIXMAP)
            xyouts, 0, 0, '!' + string(fontIndex, FORMAT='(i0)') + 'Change Font 123'
            wdelete, theWin
        endcase
        
        'X': begin
            theWin = MrGetWindow(/FREE, /PIXMAP)
            xyouts, 0, 0, '!' + string(fontIndex, FORMAT='(i0)') + 'Change Font 123'
            wdelete, theWin
        endcase
        
        'PS': begin
            ;Temporarily swtich out of PS mode
            if strupcase(!version.os_family) eq 'WINDOWS' $
                then set_plot, 'WIN' $
                else set_plot, 'x'
            
            ;Set the font
            theWin = MrGetWindow(/FREE, /PIXMAP)
            xyouts, 0, 0, '!' + string(fontIndex, FORMAT='(i0)') + 'Change Font 123'
            wdelete, theWin
            
            ;Return to postscript
            set_plot, 'PS'
        endcase
        
        else: ;Do nothing
    endcase
end


;+
;
;-
pro MrSetFont_Hardware, fontName, $
FILTER=filter, $
GET_FONTNAMES=get_fontnames, $
GET_FONTNUM=get_fontnum, $
;FONT STYLES (Microsoft)
BOLD=bold, $
FIXED=fixed, $
FONT_SIZE=font_size, $
DRAFT=draft, $
HEAVY=heavy, $
ITALIC=italic, $
LIGHT=light, $
PROOF=proof, $
STRIKEOUT=strikeout, $
THIN=thin, $
UNDERLINE=underline, $
VARIABLE=variable
    compile_opt strictarr
    on_error, 2
    
    ;Font information
    common _MR_SETFONT_, sFont, ps_fonts, ps_styles

    ;Default font name    
    _fontName = n_elements(fontName) eq 0 ? sFont.old_hard_fontname : fontName
        
    ;Windows allows modifiers to the font name
    if strupcase(!version.os_family) eq 'WINDOWS' then begin
        if keyword_set(bold)          then _fontName += '*BOLD'
        if keyword_set(fixed)         then _fontName += '*FIXED'
        if keyword_set(draft)         then _fontName += '*DRAFT'
        if keyword_set(heavy)         then _fontName += '*HEAVY'
        if keyword_set(italic)        then _fontName += '*ITALIC'
        if keyword_set(light)         then _fontName += '*LIGHT'
        if keyword_set(proof)         then _fontName += '*PROOF'
        if keyword_set(strikeout)     then _fontName += '*STRIKEOUT'
        if keyword_set(thin)          then _fontName += '*THIN'
        if keyword_set(underline)     then _fontName += '*UNDERLINE'
        if keyword_set(variable)      then _fontName += '*VARIABLE'
        if n_elements(font_size) gt 0 then _fontName += '*' + strtrim(font_size, 2)
    endif

;---------------------------------------------------------------------
; Get Font Names? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(get_fontnames) || keyword_set(get_fontnum) then begin
        device, GET_FONTNAMES=get_fontnames, GET_FONTNUM=get_fontnum, SET_FONT='*'

        ;Filter the font names?
        if n_elements(filter) then begin
            iFonts = where(stregex(get_fontnames, filter, /BOOLEAN, /FOLD_CASE) eq 1, get_fontnum)
            if get_fontnum gt 0 $
                then get_fontnames = get_fontnames[iFonts] $
                else get_fontnames = ''
        endif
        
        return
    endif

;---------------------------------------------------------------------
; Set the Font ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Make the current font the old font
    sFont.old_font          = sFont.font
    sFont.old_hard_fontname = sFont.hard_fontname
    
    ;Set the current font
    sFont.font          = 1
    sFont.hard_fontName = _fontName
    
    device, SET_FONT=_fontName
end


;+
;
;-
pro MrSetFont_PostScript, fontName, $
FILENAME=filename, $
GET_FONTNAMES=get_fontnames, $
GET_FONTNUM=get_fontnum, $
ISOLATIN1=isolatin1, $
NOLATIN=nolatin, $
SHOWFONTS=showfonts, $
;FONT NAMES
AVANTGARDE=avantgarde, $
BKMAN=bookman, $
COURIER=courier, $
HELVETICA=helvetica, $
PALATINO=palatino, $
SCHOOLBOOK=schoolbook, $
SYMBOL=symbol, $
TIMES=times, $
ZAPFCHANCERY=zapfchancery, $
ZAPFDINGBATS=zapfdingbats, $
;FONT STYLES
BOLD=bold, $
BOOK=book, $
DEMI=demi, $
ITALIC=italic, $
LIGHT=light, $
MEDIUM=medium, $
NARROW=narrow, $
OBLIQUE=oblique
    compile_opt strictarr
    on_error, 2
    
    ;Font information
    common _MR_SETFONT_, sFont, ps_fonts, ps_styles

;---------------------------------------------------------------------
; Get FontNames? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    names = ['Avantgarde'   + ['', ' Book', ' Book Oblique', ' Demi', ' Demi Oblique'], $
             'Bkman'        + ['', ' Demi', ' Demi Italic', ' Light', ' Light Italic'], $
             'Courier'      + ['', ' Bold', ' Oblique', ' Bold Oblique'], $
             'Helvetica'    + ['', ' Bold', ' Oblique', ' Bold Oblique', $
                                   ' Narrow', ' Narrow Oblique', ' Narrow Bold Oblique'] , $
             'Palatino'     + ['', ' Bold', ' Italic', ' Bold Italic'], $
             'Schoolbook'   + ['', ' Bold', ' Italic', ' Bold Italic'], $
             'Symbol', $
             'Times'        + ['', ' Bold', ' Italic', ' Bold Italic'], $
             'Zapfchancery' + ['', ' Medium Italic'], $
             'Zapfdingbats']

    if keyword_set(get_fontnames) || keyword_set(get_fontnum) then begin
        get_fontnames = names
        get_fontnum   = n_elements(names)
        return
    endif

;---------------------------------------------------------------------
; Show Fonts? ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(showfonts) then begin
        ;IDL's default filename
        cd, CURRENT=current
        idl_ps_name = filepath('idl.ps', ROOT_DIR=current)
        
        ;Input filename
        if n_elements(filename) eq 0 then filename = idl_ps_name
        _filename = cgRootName(filename, DIRECTORY=directory, EXTENSION=extension)
        _filename = filepath(_filename + '.ps', ROOT_DIR=directory)
        
        ;Show the fonts
        ps_show_fonts, NOLATIN=nolatin
        
        ;Rename the file
        if filename ne idl_ps_name then file_move, idl_ps_name, filename
        print, 'PS Font file saved to: ' + _filename
        
        return
    endif
        
    isolatin1 = keyword_set(isolatin1)

;---------------------------------------------------------------------
; Font Name //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Check font keywords first
    avantgarde   = keyword_set(avantgarde)
    bookman      = keyword_set(bookman)
    courier      = keyword_set(courier)
    helvetica    = keyword_set(helvetica)
    palatino     = keyword_set(palatino)
    schoolbook   = keyword_set(schoolbook)
    symbol       = keyword_set(symbol)
    times        = keyword_set(times)
    zapfchancery = keyword_set(zapfchancery)
    zapfdingbats = keyword_set(zapfdingbats)
    fonts = [avantgarde, bookman, courier, helvetica, palatino, schoolbook, symbol, $
             times, zapfchancery, zapfdingbats]
    
    ;Check the font name
    if total(fonts) eq 0 then begin
        if n_elements(fontName) eq 0 then fontName = sFont.ps_fontName
        
        ;Was a valid font name was given?
        iFont = where(strupcase(names) eq strupcase(fontName), nFont)
        if nFont ne 1 then message, 'Font name "' + fontName + '" not found.'
        
        ;Set the font
        _fontName    = strupcase(fontName)
        avantgarde   = stregex(_fontName, 'AVANTGARDE',   /BOOLEAN)
        bookman      = stregex(_fontName, 'BOOKMAN',      /BOOLEAN)
        courier      = stregex(_fontName, 'COURIER',      /BOOLEAN)
        helvetica    = stregex(_fontName, 'HELVECTICA',   /BOOLEAN)
        palatino     = stregex(_fontName, 'PALATINO',     /BOOLEAN)
        schoolbook   = stregex(_fontName, 'SCHOOLBOOK',   /BOOLEAN)
        symbol       = stregex(_fontName, 'SYMBOL',       /BOOLEAN)
        times        = stregex(_fontName, 'TIMES',        /BOOLEAN)
        zapfchancery = stregex(_fontName, 'ZAPFCHANCERY', /BOOLEAN)
        zapfdingbats = stregex(_fontName, 'ZAPFDINGBATS', /BOOLEAN)
        fonts = [avantgarde, bookman, courier, helvetica, palatino, schoolbook, symbol, $
                 times, zapfchancery, zapfdingbats]
                 
        ;Modifiers
        if strmid(_fontName, 'BOLD')    ne -1 then bold    = 1
        if strmid(_fontName, 'BOOK')    ne -1 then book    = 1
        if strmid(_fontName, 'DEMI')    ne -1 then demi    = 1
        if strmid(_fontName, 'ITALIC')  ne -1 then italic  = 1
        if strmid(_fontName, 'LIGHT')   ne -1 then light   = 1
        if strmid(_fontName, 'MEDIUM')  ne -1 then medium  = 1
        if strmid(_fontName, 'NARROW')  ne -1 then narrow  = 1
        if strmid(_fontName, 'OBLIQUE') ne -1 then oblique = 1
        
    ;Only one font
    endif else begin
        iFont = where(fonts eq 1, nFonts)
    
        ;Make sure only one font name is selected
        if nFonts gt 1 then begin
            message, 'More than one font selected. Choosing "' + names[iFont[0]] + '".', /INFORMATIONAL
            fonts[*] = 0B
            fonts[iFont[0]] = 1B
        endif
        
        ;Pick the font name
        short_names = ['Avantgarde', 'Bkman', 'Courier', 'Helvetica', 'Palatino', $
                       'Schoolbook', 'Symbol', 'Times', 'Zapfchancery', 'Zapfdingbats']
        _fontName = short_names[iFont[0]]
    endelse
    
    ;Store the font
    ps_fonts.avantgarde   = fonts[0]
    ps_fonts.bookman      = fonts[1]
    ps_fonts.courier      = fonts[2]
    ps_fonts.helvectica   = fonts[3]
    ps_fonts.palatino     = fonts[4]
    ps_fonts.schoolbook   = fonts[5]
    ps_fonts.symbol       = fonts[6]
    ps_fonts.times        = fonts[7]
    ps_fonts.zapfchancery = fonts[8]
    ps_fonts.zapfdingbats = fonts[9]

;---------------------------------------------------------------------
; Font Styles ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ps_styles.bold    = keyword_set(bold)
    ps_styles.book    = keyword_set(book)    
    ps_styles.demi    = keyword_set(demi)    
    ps_styles.italic  = keyword_set(italic)  
    ps_styles.light   = keyword_set(light)   
    ps_styles.medium  = keyword_set(medium)  
    ps_styles.narrow  = keyword_set(narrow)  
    ps_styles.oblique = keyword_set(oblique) 
    fontstyles = [bold, book, demi, italic, light, medium, narrow, oblique]

;---------------------------------------------------------------------
; Change Fonts ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Make the current font the old font
    sFont.old_font        = sFont.font
    sFont.old_ps_fontname = sFont.ps_fontname
    sFont.old_ps_styles   = sFont.ps_styles
    
    ;Update the font
    sFont.font        = 2
    sFont.ps_fontname = _fontName
    sFont.ps_styles   = fontstyles

    ;Switch to the postscript device and configure it.
    thisDevice = !d.name
    set_plot, 'PS'
    
    ;Set the font
    keywords = create_struct(ps_fonts, ps_styles, 'isolatin1', isolatin1)
    device, _STRICT_EXTRA=keywords
    
    ;Return to original device
    set_plot, thisDevice
end


;+
;
;-
pro MrSetFont_TrueType, fontName, $
FILENAME=filename, $
FILTER=filter, $
GET_FONTNAMES=get_fontnames, $
GET_FONTNUM=get_fontnum, $
SHOWFONTS=showfonts
    compile_opt strictarr
    on_error, 2
    
    ;Font information
    common _MR_SETFONT_, sFont, ps_fonts, ps_styles

    ;Default fontname
    _fontName = n_elements(fontName) eq 0 ? sFont.old_tt_fontname : fontName

;---------------------------------------------------------------------
; Get Font Names? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(get_fontnames) || keyword_set(get_fontnum) then begin
        device, GET_FONTNAMES=get_fontnames, GET_FONTNUM=get_fontnum, SET_FONT='*', /TT_FONT

        ;Filter the font names?
        if n_elements(filter) then begin
            iFonts = where(stregex(get_fontnames, filter, /BOOLEAN, /FOLD_CASE) eq 1, get_fontnum)
            if get_fontnum gt 0 $
                then get_fontnames = fonts[iFonts] $
                else get_fontnames = ''
        endif
        
        return
    endif

;---------------------------------------------------------------------
; Show the Font? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(showfonts) then begin
        ;IDL's default filename
        if !d.name eq 'ps' then begin
            cd, CURRENT=current
            idl_ps_name = filepath('idl.ps', ROOT_DIR=current)
        
            ;Input filename
            if n_elements(filename) eq 0 then filename = idl_ps_name
            _filename = cgRootName(filename, DIRECTORY=directory, EXTENSION=extension)
            _filename = filepath(_filename + '.' + extension, ROOT_DIR=directory)
        endif

        ;Show the fonts
        showfont, fontName, fontName, _STRICT_EXTRA=extra, /TT_FONT
        
        ;Rename the file
        if !d.name eq 'PS' then begin
            if filename ne idl_ps_name then file_move, idl_ps_name, filename
            print, 'PS Font file saved to: ' + _filename
        endif
        
        return
    endif

;---------------------------------------------------------------------
; Set Font ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Make the current font the old font
    sFont.old_font        = sFont.font
    sFont.old_tt_fontname = sFont.tt_fontname
    
    ;Set the current font
    sFont.font        = -1
    sFont.tt_fontName = fontName
    
    ;Set the font
    cgSet_TTFont, fontName
end


;+
;   A method for configuring postscript and ImageMagick output. See MrSaveAs::SetProperties
;   for details.
;
; Notes:
;   `Font Size: Points <http://en.wikipedia.org/wiki/Point_(typography)>`
;
; :Params:
;       FONTNAME:           in, required, type=string
;                           Name of the font to be set. 
;
; :Keywords:
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by MrSaveAs::SetProperty is accepted via
;                               keyword inheritance.
;-
pro MrSetFont, fontName, $
FONT_SIZE=font_size, $
DEFAULT=default, $
GET_FONTNAMES=get_fontnames, $
GET_FONTNUM=get_fontnum, $
HARDWARE=hardware, $
HERSHEY=hershey, $
LINESPACING=linespacing, $
POSTSCRIPT=postscript, $
SET_DEFAULT=set_default, $
SET_PFONT=set_pfont, $
SHOWFONTS=showfonts, $
TRUETYPE=trueType, $
RESET=reset, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Global variables to keep track of font
    common _MR_SETFONT_, sFont, ps_fonts, ps_styles
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        ;Reset the device
        if n_elements(thisDevice) gt 0 then set_plot, thisDevice
        if n_elements(old_sFont)  gt 0 then sFont = old_sFont
        if n_elements(old_ps_fonts) gt 0 then begin
            device, _STRICT_EXTRA=old_ps_fonts
            ps_fonts = old_ps_fonts
        endif
        if n_elements(old_ps_styles) gt 0 then begin
            device, _STRICT_EXTRA=old_ps_styles
            ps_styles = old_ps_styles
        endif
        
        MrPrintF, 'LogErr'
        return
    endif

;---------------------------------------------------------------------
; Setup //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Structure for reverting to default or previous font
    if n_elements(sFont) eq 0 then begin
        sFont = { $
                  ;Default Font
                  d_font:          -1, $
                  d_hard_fontname: 'Helvetica', $
                  d_hers_fontname: 'Simplex Roman', $
                  d_ps_fontname:   'Helvetica', $
                  d_ps_styles:     bytarr(6), $
                  d_tt_fontname:   'Helvetica', $
                  
                  ;Old (previous) font name
                  old_font:          -1, $
                  old_hard_fontname: 'Helvetica', $
                  old_hers_fontname: 'Simplex Roman', $
                  old_ps_fontname:   'Helvetica', $
                  old_ps_styles:     bytarr(6), $
                  old_tt_fontname:   'Helvetica', $
                  
                  ;Current font name
                  font:          -1, $
                  hard_fontname: 'Helvetica', $
                  hers_fontname: 'Simplex Roman', $
                  ps_fontnme:    'Helvetica', $
                  ps_styles:     bytarr(6), $
                  tt_fontname:   'Helvetica' $
                }
    endif
    
    if n_elements(ps_fonts) eq 0 then begin
        ps_fonts =  { avantgarde:   0B, $
                      bkman:        0B, $
                      courier:      0B, $
                      helvetica:    0B, $
                      palatino:     0B, $
                      schoolbook:   0B, $
                      symbol:       0B, $
                      times:        0B, $
                      zapfchancery: 0B, $
                      zapfdingbats: 0B  $
                    }
    endif
    
    if n_elements(ps_styles) eq 0 then begin
        ps_styles = { bold:         0B, $
                      book:         0B, $
                      demi:         0B, $
                      italic:       0B, $
                      light:        0B, $
                      medium:       0B, $
                      narrow:       0B, $
                      oblique:      0B  $
                    }
    endif

;---------------------------------------------------------------------
; Reset to Previous Font? ////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(reset) then begin
        hershey    = 0
        hardware   = 0
        trueType   = 0
        postscript = 0
        
        ;Previous font system
        case sFont.old_font of
            -1: hershey   = 1
             0: hardware  = 1
             1: trueType  = 1
             2: postsript = 1
        endcase
        
        ;Previous font
        case 1 of
            hardware:   fontName = sFont.old_hard_fontname
            hershey:    fontName = sFont.old_hers_fontname
            postscript: fontName = sFont.old_ps_fontname
            trueType:   fontName = sFont.old_tt_fontname
        endcase
    endif

;---------------------------------------------------------------------
; Load Default Font? /////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(default) then begin
        hershey    = 0
        hardware   = 0
        trueType   = 0
        postscript = 0
    
        ;Default font system
        case sFont.d_font of
            -1: hershey    = 1
             0: hardware   = 1
             1: trueType   = 1
             2: postscript = 1
        endcase
        
        ;Default font
        case 1 of
            hardware:   fontName = sFont.d_hard_fontname
            hershey:    fontName = sFont.d_hers_fontname
            postscript: fontName = sFont.d_ps_fontname
            trueType:   fontName = sFont.d_tt_fontname
        endcase
    endif

;---------------------------------------------------------------------
; Defaults ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    hardware    = keyword_set(hardware)
    hershey     = keyword_set(hershey)
    postscript  = keyword_set(postscript)
    set_default = keyword_set(set_default)
    set_pfont   = keyword_set(set_pfont)
    showfonts   = keyword_set(showfonts)
    trueType    = keyword_set(trueType)
    
    ;If no font was selected, choose the current font
    if (hardware + hershey + postscript + trueType) eq 0 then begin
        case sFont.old_font of
            -1: hershey    = 1
             0: hardware   = 1
             1: trueType   = 1
             2: postscript = 1
             else: ;No other options
        endcase
    endif
    
    ;Dependencies
    if hardware + hershey + postscript + trueType ne 1 then $
        message, 'One and only one font type may be chosen: HARDWARE | HERSHEY | POSTSCRIPT | TT_FONT.'

    ;Store the font
    case 1 of
        hershey:    sFont.font = -1
        hardware:   sFont.font =  0
        trueType:   sFont.font =  1
        postscript: sFont.font =  2
    endcase
        
;---------------------------------------------------------------------
; Set the Font ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Store the current state in case an error occurs
    thisDevice = !d.name
    old_sFont  = sFont
    if postscript then begin
        old_ps_font   = ps_font
        old_ps_styles = ps_styles
    endif

    if arg_present(get_fontNames) then get_fontNames = 1
    if arg_present(get_fontNum)  then get_fontNum    = 1

    case 1 of
        ;HARDWARE
        hardware:   MrSetFont_Hardware,   fontName, GET_FONTNAMES=get_fontNames, $
                                          GET_FONTNUM=get_fontnum, $
                                          _STRICT_EXTRA=extra
        ;HERSHEY
        hershey:    MrSetFont_Hershey,    fontName, GET_FONTNAMES=get_fontNames, $
                                          GET_FONTNUM=get_fontnum, SHOWFONTS=showfonts

        ;POSTSCRIPT
        postscript: MrSetFont_PostScript, fontName, GET_FONTNAMES=get_fontNames, $
                                          GET_FONTNUM=get_fontnum, SHOWFONTS=showfonts, $
                                          _STRICT_EXTRA=extra

        ;TRUETYPE
        truetype:   MrSetFont_TrueType,   fontName, GET_FONTNAMES=get_fontNames, $
                                          GET_FONTNUM=get_fontnum, SHOWFONTS=showfonts, $
                                          _STRICT_EXTRA=extra
    endcase

;---------------------------------------------------------------------
; Set this as the Default Font? //////////////////////////////////////
;---------------------------------------------------------------------
    if set_default then begin
        case 1 of
            hardware: begin
                sFont.d_font = 0
                sFont.d_hard_fontname = fontName
            endcase
            
            hershey: begin
                sFont.d_font = -1
                sFont.d_hers_fontname = fontName
            endcase
            
            postscript: begin
                sFont.d_font = 2
                sFont.d_ps_fontname = fontName
            endcase
            
            truetype: begin
                sFont.d_font = 1
                sFont.d_tt_fontname = fontName
            endcase
        endcase
    endif
    
;---------------------------------------------------------------------
; Set the System Variable? ///////////////////////////////////////////
;---------------------------------------------------------------------
    if set_pfont then begin
        case 1 of
            hardware:   !p.font =  0
            hershey:    !p.font = -1
            postscript: !p.font =  0
            truetype:   !p.font =  1
        endcase
    endif
end




;---------------------------------------------------------------------
; Main Level Example Program: IDL>.r MrSetFont) //////////////////////
;---------------------------------------------------------------------
;Get font names and print them
MrSetFont, /HARDWARE, GET_FONTNAMES=hard_fonts, FILTER='New Century Schoolbook-Bold-i'
MrSetFont, /HERSHEY,  GET_FONTNAMES=hers_fonts
MrSetFont, /TRUETYPE, GET_FONTNAMES=true_fonts
print, '---------------------------------------------------------------'
print, 'Hardware Fonts:'
print, '  ' + transpose(hard_fonts)
print, ''
print, 'Hershey Fonts:'
print, '  ' + transpose(hers_fonts)
print, ''
print, 'True-Type Fonts:'
print, '  ' + transpose(true_fonts)
print, ''


;Show the current font
cgText, 0.01, 0.95, 'Current Hershey Font'

;Set the Hershey font
MrSetFont, 'Gothic German', /HERSHEY
cgText, 0.05, 0.90, 'Hershey: "Gothic German"', /NORMAL

;Set the True-type font
MrSetFont, 'Times Italic', /TRUETYPE
cgText, 0.05, 0.85, 'True-Type: "Times Italic"', FONT=1, /NORMAL

;Set the Hardware font
MrSetFont, '-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-14', /HARDWARE
cgText, 0.05, 0.80, 'Hardware: "New Century Schoolbook"', FONT=0, /NORMAL

;Return to the default font
MrSetFont, /DEFAULT
cgText, 0.05, 0.75, 'Default Font', /NORMAL

end