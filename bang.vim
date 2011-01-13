" Bang syntax file.
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syn match bsModule          "\<module\>"
syn match bsStructure       "\<\(class\|datatype\|instance\)\>"
syn match bsTypedef         "\<\(type\|newtype\)\>"
syn match bsColonColon      "::"
syn keyword bsConditional   case of if then else otherwise
syn keyword bsModifier      export restrict
syn region bsComment matchgroup=bsCommentStart start="/\*" end="\*/"


hi def link bsModule       Include
hi def link bsStructure    Structure
hi def link bsTypedef      Typedef
hi def link bsModifier     StorageClass
hi def link bsConditional  Conditional
hi def link bsColonColon   Type
hi def link bsComment      Comment
hi def link bsCommentStart Comment
