module GPP where

import StrategyLib
import Text.PrettyPrint.HughesPJ
import Control.Monad.Identity

-- | The type of generic pretty-printers (universally quantified).
type GPP = forall a . Term a => a -> Doc

-- | The type of generic pretty-printers (as TU strategy)
type TUDoc = TU Doc Identity

-- | Class of pre-fix-pointed pretty-printers (overloaded)
class PP a where
  pp :: GPP -> a -> Doc

-- | Type of updatable pretty-printers.
type UPP = GPP -> TUDoc

-- | Helper function for pretty-printing lists.
gppList :: Term a => GPP -> [a] -> Doc
gppList gpp xs
  = sep (map gpp xs)

-- | Helper function for pretty-printing separator lists.
gppListSep :: (Term sep, Term a) => GPP -> sep -> [a] -> Doc
gppListSep gpp sp xs
  = sep (punctuate (gpp sp) (map gpp xs))

-- | Helper function for pretty-printing optionals.
gppMaybe :: Term a => GPP -> Maybe a -> Doc
gppMaybe gpp xs
  = maybe empty gpp xs
  
-- | Helper function for pretty-printing alternatives.
gppEither :: (Term a, Term b) => GPP -> Either a b -> Doc
gppEither gpp xs
  = either gpp gpp xs

-- | Render with the fix-point of a pre-fix-point pretty-printer
renderFix :: Term a => UPP -> a -> String
renderFix gpp2tudoc
  = render . gpp
    where
      gpp :: GPP
      gpp = runIdentity . applyTU (gpp2tudoc gpp)

-- | Render with the fix-point of a pre-fix-point pretty-printer
renderFixMode :: Term a => Mode -> UPP -> a -> String
renderFixMode renderMode gpp2tudoc
  = renderStyle style . gpp
    where
      style = defaultStyle { mode = renderMode }
      defaultStyle = Style PageMode 100 1.5
      gpp :: GPP
      gpp = runIdentity . applyTU (gpp2tudoc gpp)

-- | For easy type annotation
type MonoPP a = a -> Doc

-- | For easy non-monadic adhoc
adhocQ :: (Term t, Monad m) => TU a m -> (t -> a) -> TU a m
adhocQ f g = adhocTU f (return . g)

-- | Instance to deal with lexical sorts and literals,
--   which are all represented by String.
instance PP String where
  pp gpp = text
