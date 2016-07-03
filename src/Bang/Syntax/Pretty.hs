module Bang.Syntax.Pretty(
         ppModule
       , ppDeclaration
       , ppExpression
       , ppType
       , ppName
       )
 where

import Bang.Syntax.AST
import Data.Text.Lazy(Text, unpack)
import Text.PrettyPrint.Annotated

ppName :: Name -> Doc a
ppName (Name _ _ w t) = text' t <> colon <> integer (fromIntegral w)

ppModule :: Module -> Doc a
ppModule (Module name decls) =
  vcat ([text "module" <> space <> ppName name, text ""] ++
        map ppDeclaration decls)

ppDeclaration :: Declaration -> Doc a
ppDeclaration d =
  case d of
    TypeDeclaration n t ->
      ppName n <> space <> text "::" <> space <> ppType t
    ValueDeclaration n e ->
      ppName n <> space <> text "=" <> space <> ppExpression e
    PrimTypeDeclaration n t ->
      text "primitive" <> space <> text "type" <> space <>
      ppName n <> space <> text "=" <> space <> text' t

ppExpression :: Expression -> Doc a
ppExpression x =
  case x of
    ConstantExp  _ v    -> ppConstantValue v
    ReferenceExp _ n    -> ppName n
    LambdaExp    _ ns e ->
      text "λ" <> space <> hsep (map ppName ns) <> space <> text "->" <>
      space <> ppExpression e

ppConstantValue :: ConstantValue -> Doc a
ppConstantValue cv =
  case cv of
    ConstantInt    2  t -> text "0b" <> text' t
    ConstantInt    8  t -> text "0o" <> text' t
    ConstantInt    10 t ->              text' t
    ConstantInt    16 t -> text "0x" <> text' t
    ConstantInt    _  _ -> error "Internal error: bad base for constant"
    ConstantChar      c -> text' c
    ConstantString    s -> text' s
    ConstantFloat     f -> text' f

ppType :: Type -> Doc a
ppType t =
  case t of
    TypeUnit   _ _      -> text "()"
    TypePrim   _ _ n    -> text (unpack n)
    TypeRef    _ _ n    -> ppName n
    TypeLambda _ _ as b -> hsep (map ppType as) <> space <> text "->" <> space <> ppType b
    TypeApp    _ _ a  b -> ppType a <> space <> ppType b

text' :: Text -> Doc a
text' = text . unpack
