module Bang.Syntax.PostProcess(
         runPostProcessor
       )

import Bang.Syntax.AST

runPostProcessor :: Module -> Compiler ps Module
runPostProcessor mod = undefined

-- -----------------------------------------------------------------------------

type DeclarationTable = Map Name (Maybe TypeDeclaration, Maybe ValueDeclaration)

makeDeclarationTable :: Module -> DeclarationTable

