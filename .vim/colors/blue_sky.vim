" Vim color file: sunny_day.vim
"
" Maintainer:  Marcin Szamotulksi [mszamot [at] gmail [dot] com
" Last Change:Tue Sep 06, 2011 at 02:16  +0100
" Based On: rastafari.vim (but now quite far away from that)
"

hi clear
if exists("syntax on")
    syntax reset
endif

if has('eval')
  let g:colors_name="blue_sky"
endif

if !exists("g:blue_sky_style") || g:blue_sky_style == "light"
    let g:blue_sky_style = "light"
    set background=light
else
    let g:blue_sky_style = "dark"
    set background=dark
endif

" Change: light/dark background.
command! BlueSky  let g:blue_sky_style = ( g:blue_sky_style == 'light' ? 'dark' : 'light' ) | colorscheme blue_sky

" DARK BACKGROUND {{{1
if g:blue_sky_style == 'dark'
" Normal colors {{{2
"hi Normal       guifg=#aaaaaa guibg=#000000 gui=none term=none     cterm=none    ctermfg=grey ctermbg=black
hi Normal       guifg=#aaaaaa guibg=#101010 gui=none term=none     cterm=none    ctermfg=grey 		ctermbg=232
hi Title        guifg=#ffd700 guibg=#020202 gui=bold term=bold     cterm=bold    ctermfg=yellow		ctermbg=233
hi Ignore       guifg=#888888 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi Comment      guifg=#009300 guibg=bg      gui=none term=none     cterm=none    ctermfg=darkgreen
hi LineNr       guifg=#ffd700 guibg=bg      gui=bold term=none     cterm=none    ctermfg=yellow		
hi Include      guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi Define       guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi Macro        guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi PreProc      guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi PreCondit    guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi NonText      guifg=#eeee10 guibg=bg      gui=none term=none     cterm=none    ctermfg=yellow
hi Directory    guifg=#eeee10 guibg=bg      gui=bold term=none     cterm=none    ctermfg=yellow
hi SpecialKey   guifg=#eeee10 guibg=bg      gui=none term=none     cterm=none    ctermfg=yellow
hi Type         guifg=#ffffff guibg=bg      gui=none term=none     cterm=none    ctermfg=230
hi String       guifg=#af0057 guibg=bg      gui=none term=none     cterm=none    ctermfg=125
hi Constant     guifg=#700000 guibg=bg      gui=none term=none     cterm=none    ctermfg=darkred
hi Special      guifg=#870087 guibg=bg      gui=none term=none     cterm=none    ctermfg=90
hi Number       guifg=#5f00ff guibg=bg      gui=none term=none     cterm=none    ctermfg=57
hi MoreMsg      guifg=#80ff80 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=green
hi Identifier   guifg=#dddddd guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=230
hi Conditional  guifg=#dddddd guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=230
hi Repeat       guifg=#dddddd guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=230
hi Statement    guifg=#720084 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=90
hi Label        guifg=#dddddd guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=grey
hi Operator     guifg=white   guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=230
hi Function     guifg=white   guibg=bg      gui=none term=none     cterm=none    ctermfg=230
hi MatchParen   guifg=white   guibg=#5f00ff gui=none term=none     cterm=none    ctermfg=230        ctermbg=blue
hi ErrorMsg     guifg=#eb0000 guibg=bg	    gui=bold term=none     cterm=none    ctermfg=yellow     ctermbg=red
hi WildMenu     guifg=#5f00ff guibg=#020202 gui=bold term=none     cterm=bold    ctermfg=57         ctermbg=233
hi Folded  	guifg=#837598 guibg=#1c1c1c 			   		 ctermfg=57 	    ctermbg=233	
hi FoldColumn   guifg=#5CB80C guibg=#1c1c1c 			   cterm=none    ctermfg=green 	    ctermbg=233 
hi SignColumn   guifg=#eeee10 guibg=bg      gui=none term=none     cterm=none    ctermfg=yellow     ctermbg=233
hi Search       guifg=#dddddd guibg=#0000ff gui=none term=none     cterm=none    ctermfg=230        ctermbg=blue
hi IncSearch    guifg=#dddddd guibg=#008080 gui=none term=none     cterm=none    ctermfg=230        ctermbg=cyan
hi WarningMsg   guifg=#ffff60 guibg=#008800 gui=bold term=none     cterm=bold    ctermfg=178        ctermbg=234
hi Question     guifg=#008800               gui=bold term=standout cterm=bold    ctermfg=darkgreen  ctermbg=234
hi Pmenu        guifg=#80ff80 guibg=#008800 gui=bold term=standout cterm=none    ctermfg=white      ctermbg=236
hi PmenuSel     guifg=#eeee10 guibg=#aa0000 gui=bold term=standout cterm=bold    ctermfg=darkblue   ctermbg=236
hi PmenuThumb   guifg=#eeee10 guibg=#aa0000 gui=bold term=standout cterm=bold    ctermfg=darkblue   ctermbg=darkblue
hi PmenuSbar    guifg=#eeee10 guibg=#aa0000 gui=bold term=standout cterm=bold    ctermfg=white      ctermbg=white
hi Visual                     guibg=#333333 gui=none term=none     cterm=none                       ctermbg=236
hi TabLineFill  guifg=#eeee10 guibg=#121212 gui=bold term=bold     cterm=none    ctermfg=yellow	    ctermbg=247
hi TabLine      guifg=#eeee10 guibg=#121212 gui=bold term=standout cterm=none    ctermfg=yellow	    ctermbg=247
hi TabLine      guifg=#eeee10 guibg=#121212 gui=bold term=bold     cterm=none    ctermfg=yellow	    ctermbg=247
hi TabLineSel   guifg=#ffff60 guibg=#121212 gui=bold term=none     cterm=bold    ctermfg=226 	    ctermbg=240
hi VertSplit    guifg=#006606 guibg=#080808 gui=none term=none     cterm=none    ctermfg=darkgreen  ctermbg=234
hi StatusLine   guifg=#00a7b0 guibg=#020202 gui=bold term=none     cterm=bold    ctermfg=38         ctermbg=233
hi StatusLineNC 	      guibg=#020202	     term=none     cterm=bold    ctermfg=grey 	    ctermbg=233
hi CursorColumn guifg=NONE    guibg=#111111 gui=none term=none     cterm=none    ctermfg=darkgrey   ctermbg=grey
hi CursorLine   guifg=NONE    guibg=#111111 gui=none term=none     cterm=none    ctermfg=none 	    ctermbg=53
" Specific for c {{{2
hi cOctalZero    guifg=#ff8800 guibg=bg      gui=none term=none     cterm=none    ctermfg=darkyellow
hi cFormat       guifg=#ff8800 guibg=bg      gui=none term=none     cterm=none    ctermfg=darkyellow
" Specific for diff {{{2
" hi DiffAdd      guifg=#ffff33 guibg=#404010 gui=bold term=none cterm=none ctermfg=green
" hi DiffChange                 guibg=#202020 gui=bold term=none            ctermfg=darkgrey
" hi DiffText     guifg=#3333ff guibg=#100040 gui=bold term=none cterm=none ctermfg=blue
" hi DiffDelete   guifg=#ff0000 guibg=#401010 gui=bold term=none cterm=none ctermfg=red
hi DiffAdd        cterm=none           	ctermfg=15  ctermbg=56  guifg=white	guibg=SlateBlue4 gui=bold
hi DiffDelete     cterm=none           	ctermfg=255  ctermbg=125 guifg=white	guibg=DeepPink4
hi DiffChange     cterm=none           	ctermfg=173 ctermbg=125	guifg=salmon	guibg=DeepPink4
hi DiffText       cterm=bold           	ctermfg=230 ctermbg=125 guifg=white	guibg=DeepPink4
hi diffLine     guifg=#444444 guibg=bg gui=bold term=none cterm=bold ctermfg=darkgrey
hi diffOldLine  guifg=#444444 guibg=bg gui=none term=none cterm=none ctermfg=darkgrey
hi diffOldFile  guifg=#444444 guibg=bg gui=none term=none cterm=none ctermfg=darkgrey
hi diffNewFile  guifg=#444444 guibg=bg gui=none term=none cterm=none ctermfg=darkgrey
hi diffAdded    guifg=#80ff80 guibg=bg gui=none term=none cterm=none ctermfg=green
hi diffRemoved  guifg=#ff0000 guibg=bg gui=none term=none cterm=none ctermfg=red
hi diffChanged  guifg=#0000ff guibg=bg gui=none term=none cterm=none ctermfg=blue
" Specific for doxygen {{{2
hi doxygenStart                guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenStartL               guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenBriefLine            guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenBrief                guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenBriefL               guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenPrevL                guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenComment              guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenCommentL             guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenSpecialMultiLineDesc guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenSpecial              guifg=#80ff80 guibg=bg gui=none term=none cterm=none ctermfg=lightgreen
hi doxygenParam                guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenParamName            guifg=#3333ff guibg=bg gui=bold term=none cterm=none ctermfg=blue
hi doxygenParamDirection       guifg=#ffff60 guibg=bg gui=bold term=none cterm=none ctermfg=yellow
hi doxygenArgumentWord         guifg=#3333ff guibg=bg gui=none term=none cterm=none ctermfg=blue
hi doxygenCodeWord             guifg=#3333ff guibg=bg gui=bold term=none cterm=none ctermfg=blue
hi doxygenHyperLink            guifg=#3333ff guibg=bg gui=bold term=none cterm=none ctermfg=blue
" Specific for help files {{{2
hi helpHyperTextJump guifg=#ffaa00 guibg=bg gui=none term=none cterm=none ctermfg=57
hi helpBar 		ctermfg=57
hi helpStar		ctermfg=90

" Specific for Perl {{{2
hi perlSharpBang        guifg=#80ff80 guibg=bg gui=bold term=standout cterm=bold ctermfg=lightgreen
hi perlStatement        guifg=#aaaaaa guibg=bg gui=none term=none     cterm=none ctermfg=grey
hi perlStatementStorage guifg=#dddddd guibg=bg gui=bold term=none     cterm=none ctermfg=230
hi perlVarPlain         guifg=#aaaaaa guibg=bg gui=none term=none     cterm=none ctermfg=grey
hi perlVarPlain2        guifg=#aaaaaa guibg=bg gui=none term=none     cterm=none ctermfg=grey
" Specific for Ruby {{{2
hi rubySharpBang guifg=#80ff80 guibg=bg gui=bold term=none cterm=bold ctermfg=lightgreen
" Specific for the statusline {{{2
" My ~/.vimrc uses User1 and User2 in active statusline.
hi User1 guifg=gold1   guibg=#080808 gui=bold        	    cterm=bold ctermfg=226 ctermbg=233 			 
hi User2 guifg=red4    guibg=#080808	        	    cterm=none ctermfg=red ctermbg=233 			 
" My ~/.vimrc uses User3 and User4 in non-active statusline.
hi User4 guifg=#66ff66 guibg=#008000 gui=bold term=none     cterm=bold ctermfg=lightgreen ctermbg=lightgreen
" Specific for netrw {{{2
hi netrwTilde   guifg=#aaaaaa guibg=bg      gui=bold term=none cterm=none ctermfg=grey
hi netrwExe     guifg=#aa2222 guibg=bg      gui=none term=none cterm=none ctermfg=darkred
hi netrwTags    guifg=#666666 guibg=bg      gui=bold term=bold cterm=bold ctermfg=darkgrey
hi netrwTilde   guifg=#666666 guibg=bg      gui=bold term=bold cterm=bold ctermfg=darkgrey
hi netrwSymLink guifg=#1111ff guibg=bg      gui=none term=none cterm=none ctermfg=blue
hi netrwList    guifg=#aaaaaa guibg=#000000 gui=none term=none cterm=none ctermfg=grey
" Specific for confluence wiki {{{2
hi confluenceHeadingMarker guifg=#aa2222 guibg=bg gui=none term=none cterm=none ctermfg=darkred
hi confluenceHeading       guifg=#ffff60 guibg=bg gui=bold term=none cterm=none ctermfg=yellow
hi confluenceVerbatim      guifg=#dddddd guibg=bg gui=none term=none cterm=none ctermfg=230
" Specific for HTML {{{2
hi htmlLink guifg=#3333ff guibg=bg gui=none term=none cterm=none ctermfg=lightblue
" YankRing {{{2
hi yankringItemNumber guifg=#000070 guibg=bg      gui=none term=none     cterm=none    ctermfg=125
" Spell checking {{{2
if version >= 700
  hi SpellBad   guisp=red    gui=undercurl guifg=NONE guibg=NONE ctermfg=196 ctermbg=NONE term=underline cterm=NONE
  hi SpellCap   guisp=yellow gui=undercurl guifg=NONE guibg=NONE ctermfg=196 ctermbg=NONE term=underline cterm=NONE
  hi SpellRare  guisp=blue   gui=undercurl guifg=NONE guibg=NONE ctermfg=203 ctermbg=NONE term=underline cterm=NONE
  hi SpellLocal guisp=orange gui=undercurl guifg=NONE guibg=NONE ctermfg=202 ctermbg=NONE term=underline cterm=NONE
endif
" Cursor color for GUI {{{2
hi Cursor   guifg=#000000 guibg=#ffffff
hi lCursor  guifg=#000000 guibg=#80ff80
hi CursorIM guifg=#000000 guibg=#0090ff
" Specific for TeX {{{2
hi texSectionMarker	guifg=#870087 guibg=bg gui=bold	cterm=bold	ctermfg=90
hi texSection		guifg=#ffffff guibg=bg gui=none cterm=none	ctermfg=230
hi texDocType		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=90
hi texInputFile		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=90
hi texDocTypeArgs	ctermfg=161	
hi texInputFileopt	ctermfg=161
hi texType		guifg=#5BCC0F guibg=bg gui=none cterm=none	ctermfg=57
hi texMath		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=57
hi texStatement 	guifg=#5730CC guibg=bg gui=bold cterm=none	ctermfg=91
hi Conceal 		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=91	ctermbg=232
hi link texComment	Comment
hi texString		guifg=#2E68CC	ctermfg=39	
hi texSpecialChar	ctermfg=39	
hi texOnlyMath		ctermfg=27
hi texTikzCoord 	guifg=#5730CC 	ctermbg=61

" highlight texTikz			ctermfg=204	ctermbg=233
" ATP {{{2
highlight atp_label_sectionnr	ctermfg=white
" ywtxt {{{2
hi ywtxt_bold 		cterm=bold 	gui=bold
hi ywtxt_italic 	cterm=italic  	gui=italic
hi ywtxt_underline 	cterm=underline gui=underline

" Vim Specific {{{2
hi vimCommentTitle	guifg=#47e000 guibg=bg      gui=bold cterm=bold     ctermfg=darkgreen
hi link vimGroupName Type	
" vim: foldmethod=marker foldmarker={{{,}}}:

" LIGHT BACKGROUND {{{1
else 
" Normal: colors {{{2
"hi Normal       guifg=#aaaaaa guibg=#000000 gui=none term=none     cterm=none    ctermfg=grey ctermbg=black
hi Normal       guifg=black   guibg=white gui=none term=none       cterm=none    ctermfg=235 		ctermbg=white
hi Conceal	guifg=#5730CC guibg=white					 ctermfg=57		ctermbg=white
hi Title        guifg=#dd0049 guibg=#ffffff gui=bold term=bold     cterm=bold    ctermfg=yellow		ctermbg=white
hi Ignore       guifg=#888888 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi Comment      guifg=#949494 guibg=bg      gui=none term=none     cterm=none    ctermfg=53
hi LineNr       guifg=#000000 guibg=bg      gui=bold term=none     cterm=bold    ctermfg=black		
hi Include      guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi Define       guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi Macro        guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi PreProc      guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi PreCondit    guifg=#666666 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=darkgrey
hi NonText      guifg=#00aa00 guibg=bg      gui=bold term=none     cterm=none    ctermfg=darkgreen
hi Directory    guifg=#CC5118 guibg=bg      gui=bold term=none     cterm=none    ctermfg=202
hi SpecialKey   guifg=#d5034d guibg=bg      gui=none term=none     cterm=none    ctermfg=197
hi Type         guifg=#5730CC guibg=bg      gui=bold term=none     cterm=bold    ctermfg=233
hi String       guifg=#000070 guibg=bg      gui=none term=none     cterm=none    ctermfg=125
hi Constant     guifg=#700000 guibg=bg      gui=none term=none     cterm=none    ctermfg=darkred
hi Special      guifg=#642567 guibg=bg      gui=none term=none     cterm=none    ctermfg=90
hi Number       guifg=#8a04dd guibg=bg      gui=none term=none     cterm=none    ctermfg=57
hi MoreMsg      guifg=#dd0049 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=green
hi Identifier   guifg=#2E004B guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=234
hi Conditional  guifg=#707070 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=234
hi Repeat       guifg=#707070 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=234
hi Statement    guifg=#000080 guibg=bg      gui=bold term=bold     cterm=none    ctermfg=93
hi Label        guifg=#707070 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=grey
hi Operator     guifg=#444444 guibg=bg      gui=bold term=bold     cterm=bold    ctermfg=233
hi Function     guifg=#2d008e guibg=bg      gui=none term=none     cterm=none    ctermfg=233
hi MatchParen   guifg=#ffffff guibg=#4800FF gui=none term=none     cterm=none    ctermfg=233            ctermbg=blue
hi ErrorMsg     guifg=#eb0000 guibg=bg	    gui=bold term=none     cterm=none    ctermfg=white          ctermbg=red
hi WildMenu     guifg=#4c05ff guibg=#ffffff gui=bold term=none     cterm=bold    ctermfg=57             ctermbg=white
hi Folded  	guifg=#1c1c1c guibg=#545a98 			   cterm=none	 ctermfg=232 	        ctermbg=247
hi FoldColumn   guifg=#1c1c1c guibg=#545a98 			   cterm=none    ctermfg=232 	        ctermbg=247 
hi SignColumn   guifg=#eeee10 guibg=bg      gui=none term=none     cterm=none    ctermfg=yellow         ctermbg=233
hi Search       guifg=#ffffff guibg=#0050f0 gui=none term=none     cterm=none    ctermfg=234            ctermbg=blue
hi IncSearch    guifg=#ffffff guibg=#00349c gui=none term=none     cterm=none    ctermfg=234            ctermbg=cyan
hi WarningMsg   guifg=#ff5900 guibg=#ffffff gui=bold term=none     cterm=bold    ctermfg=white          ctermbg=209
hi Question     guifg=#008800               gui=bold term=standout cterm=none    ctermfg=126            ctermbg=white
hi Pmenu        guifg=#ffffff guibg=#4800ff gui=bold term=standout cterm=none    ctermfg=white          ctermbg=236
hi PmenuSel     guifg=#ffffff guibg=#1a005d gui=bold term=standout cterm=bold    ctermfg=darkblue       ctermbg=236
hi PmenuThumb   guifg=#1a005d guibg=#1a005d gui=bold term=standout cterm=bold    ctermfg=darkblue       ctermbg=darkblue
hi PmenuSbar    guifg=#ffffff guibg=#ffffff gui=bold term=standout cterm=bold    ctermfg=white          ctermbg=white
hi Visual                     guibg=#c6c6c6 gui=none term=standout cterm=none                           ctermbg=253
hi TabLineFill  guifg=#eeee10 guibg=#121212 gui=bold term=bold     cterm=bold    ctermfg=16	        ctermbg=249
hi TabLine      guifg=#eeee10 guibg=#121212 gui=bold term=standout cterm=bold    ctermfg=16	        ctermbg=249
hi TabLine      guifg=#eeee10 guibg=#121212 gui=bold term=bold     cterm=bold    ctermfg=16	        ctermbg=249
hi TabLineSel   guifg=#ffff60 guibg=#121212 gui=bold term=none     cterm=bold    ctermfg=white 	        ctermbg=250
hi VertSplit    guifg=#a3a3a3 guibg=#ffffff gui=none term=none     cterm=none    ctermfg=black 	        ctermbg=white
hi StatusLine   guifg=#000000 guibg=#ffffff gui=bold term=none     cterm=bold    ctermfg=black          ctermbg=white
hi StatusLineNC guifg=#000000 guifg=#a3a3a3	      		   cterm=bold    ctermfg=white 	        ctermbg=gray
hi CursorColumn guifg=NONE    guibg=#111111 gui=none term=none     cterm=none    ctermfg=darkgrey       ctermbg=darkgrey
hi CursorLine   guifg=NONE    guibg=#a2a2a2 gui=none term=none     cterm=none    ctermfg=none 	        ctermbg=53
" Specific for c {{{2                                                                                   
hi cOctalZero    guifg=#ff8800 guibg=bg      gui=none term=none     cterm=none    ctermfg=darkyellow    
hi cFormat       guifg=#ff8800 guibg=bg      gui=none term=none     cterm=none    ctermfg=darkyellow
" Specific for diff {{{2
" hi DiffAdd      guifg=#ffff33 guibg=#404010 gui=bold term=none cterm=none ctermfg=green
" hi DiffChange                 guibg=#202020 gui=bold term=none            ctermfg=darkgrey
" hi DiffText     guifg=#3333ff guibg=#100040 gui=bold term=none cterm=none ctermfg=blue
" hi DiffDelete   guifg=#ff0000 guibg=#401010 gui=bold term=none cterm=none ctermfg=red
hi DiffAdd        cterm=none           	ctermfg=15  ctermbg=56  guifg=white	guibg=SlateBlue4 gui=bold
hi DiffDelete     cterm=none           	ctermfg=19  ctermbg=56	guifg=VioletRed	guibg=SlateBlue4
hi DiffChange     cterm=none           	ctermfg=173 ctermbg=125	guifg=salmon	guibg=DeepPink4
hi DiffText       cterm=bold           	ctermfg=234 ctermbg=125 guifg=white	guibg=DeepPink4
hi diffLine     guifg=#444444 guibg=bg gui=bold term=none cterm=bold ctermfg=darkgrey
hi diffOldLine  guifg=#444444 guibg=bg gui=none term=none cterm=none ctermfg=darkgrey
hi diffOldFile  guifg=#444444 guibg=bg gui=none term=none cterm=none ctermfg=darkgrey
hi diffNewFile  guifg=#444444 guibg=bg gui=none term=none cterm=none ctermfg=darkgrey
hi diffAdded    guifg=#80ff80 guibg=bg gui=none term=none cterm=none ctermfg=green
hi diffRemoved  guifg=#ff0000 guibg=bg gui=none term=none cterm=none ctermfg=red
hi diffChanged  guifg=#0000ff guibg=bg gui=none term=none cterm=none ctermfg=blue
" Specific for doxygen {{{2
hi doxygenStart                guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenStartL               guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenBriefLine            guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenBrief                guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenBriefL               guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenPrevL                guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenComment              guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenCommentL             guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenSpecialMultiLineDesc guifg=#00aa00 guibg=bg gui=none term=none cterm=none ctermfg=darkgreen
hi doxygenSpecial              guifg=#80ff80 guibg=bg gui=none term=none cterm=none ctermfg=lightgreen
hi doxygenParam                guifg=#80ff80 guibg=bg gui=bold term=none cterm=none ctermfg=lightgreen
hi doxygenParamName            guifg=#3333ff guibg=bg gui=bold term=none cterm=none ctermfg=blue
hi doxygenParamDirection       guifg=#ffff60 guibg=bg gui=bold term=none cterm=none ctermfg=yellow
hi doxygenArgumentWord         guifg=#3333ff guibg=bg gui=none term=none cterm=none ctermfg=blue
hi doxygenCodeWord             guifg=#3333ff guibg=bg gui=bold term=none cterm=none ctermfg=blue
hi doxygenHyperLink            guifg=#3333ff guibg=bg gui=bold term=none cterm=none ctermfg=blue
" Specific for help files {{{2
hi helpHyperTextJump guifg=#800080 guibg=bg gui=none term=none cterm=none ctermfg=57
hi helpBar 		ctermfg=57
hi helpStar		ctermfg=90

" Specific for Perl {{{2
hi perlSharpBang        guifg=#80ff80 guibg=bg gui=bold term=standout cterm=bold ctermfg=lightgreen
hi perlStatement        guifg=#aaaaaa guibg=bg gui=none term=none     cterm=none ctermfg=grey
hi perlStatementStorage guifg=#707070 guibg=bg gui=bold term=none     cterm=none ctermfg=234
hi perlVarPlain         guifg=#aaaaaa guibg=bg gui=none term=none     cterm=none ctermfg=grey
hi perlVarPlain2        guifg=#aaaaaa guibg=bg gui=none term=none     cterm=none ctermfg=grey
" Specific for Ruby {{{2
hi rubySharpBang guifg=#80ff80 guibg=bg gui=bold term=none cterm=bold ctermfg=lightgreen
" Specific for the statusline {{{2
" My ~/.vimrc uses User1 and User2 in active statusline.
hi User1 	guifg=#000000 guibg=#ffffff gui=bold        	    cterm=bold ctermfg=54 ctermbg=white
hi User2 	guifg=#000000 guibg=#ffffff	        	    cterm=bold ctermfg=161 ctermbg=white
" My ~/.vimrc uses User3 and User4 in non-active statusline.
hi User3 	guifg=#000000 guibg=#ffffff gui=none        	    cterm=bold ctermfg=125 ctermbg=white
hi User4 	guifg=#000000 guibg=#ffffff gui=bold term=none      cterm=bold ctermfg=darkgreen ctermbg=white
" Specific for netrw {{{2
hi netrwTilde   guifg=#aaaaaa guibg=bg      gui=bold term=none cterm=none ctermfg=grey
hi netrwExe     guifg=#aa2222 guibg=bg      gui=none term=none cterm=none ctermfg=darkred
hi netrwTags    guifg=#666666 guibg=bg      gui=bold term=bold cterm=bold ctermfg=darkgrey
hi netrwTilde   guifg=#666666 guibg=bg      gui=bold term=bold cterm=bold ctermfg=darkgrey
hi netrwSymLink guifg=#1111ff guibg=bg      gui=none term=none cterm=none ctermfg=blue
hi netrwList    guifg=#aaaaaa guibg=#000000 gui=none term=none cterm=none ctermfg=grey
" Specific for confluence wiki {{{2
hi confluenceHeadingMarker guifg=#aa2222 guibg=bg gui=none term=none cterm=none ctermfg=darkred
hi confluenceHeading       guifg=#ffff60 guibg=bg gui=bold term=none cterm=none ctermfg=yellow
hi confluenceVerbatim      guifg=#dddddd guibg=bg gui=none term=none cterm=none ctermfg=234
" Specific for HTML {{{2
hi htmlLink guifg=#3333ff guibg=bg gui=none term=none cterm=none ctermfg=lightblue
" YankRing {{{2
hi yankringItemNumber guifg=#000070 guibg=bg      gui=none term=none     cterm=none    ctermfg=125
" Spell checking {{{2
if version >= 700
  hi SpellBad   guisp=red    gui=undercurl guifg=NONE guibg=NONE ctermfg=196 ctermbg=NONE term=underline cterm=NONE
  hi SpellCap   guisp=yellow gui=undercurl guifg=NONE guibg=NONE ctermfg=196 ctermbg=NONE term=underline cterm=NONE
  hi SpellRare  guisp=blue   gui=undercurl guifg=NONE guibg=NONE ctermfg=203 ctermbg=NONE term=underline cterm=NONE
  hi SpellLocal guisp=orange gui=undercurl guifg=NONE guibg=NONE ctermfg=202 ctermbg=NONE term=underline cterm=NONE
endif
" Cursor color for GUI {{{2
hi Cursor   guifg=#ffffff guibg=#949394
hi lCursor  guifg=#000000 guibg=#115e73
hi CursorIM guifg=#000000 guibg=#1c96b8
" Specific for TeX {{{2
hi texSectionMarker	guifg=#631da8 guibg=bg gui=bold	cterm=bold	ctermfg=93
hi texSection		guifg=#3d1268 guibg=bg gui=bold cterm=bold	ctermfg=54
hi texDocType		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=90
hi texInputFile		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=90
hi texDocTypeArgs		ctermfg=204	
hi texInputFileopt		ctermfg=204	
hi texType		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=57
hi texMath		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=57
hi texStatement 		guifg=#5730CC guibg=bg gui=none cterm=none	ctermfg=91
hi link texComment	Comment
hi texString		guifg=#2E68CC	ctermfg=39	
hi texSpecialChar		ctermfg=39	
hi texOnlyMath			ctermfg=27
hi texTikzCoord 		guifg=#5730CC 	ctermbg=61

" highlight texTikz			ctermfg=204	ctermbg=233
" ATP {{{2 
hi atp_label_sectionnr	ctermfg=black
" ywtxt {{{2
hi ywtxt_bold 		cterm=bold 	gui=bold
hi ywtxt_italic 	cterm=italic  	gui=italic
hi ywtxt_underline 	cterm=underline gui=underline

" Vim Specific {{{2
hi vimCommentTitle	guifg=#00aa00 guibg=bg      gui=bold cterm=bold     ctermfg=90
hi vimVar						                    ctermfg=244	
hi vimFuncBody								    ctermfg=244
hi link vimFunction  Function
hi link vimGroupName Type	
" vim: foldmethod=marker foldmarker={{{,}}}:
endif "}}}1
