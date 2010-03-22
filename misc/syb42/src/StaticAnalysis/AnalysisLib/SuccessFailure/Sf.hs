module AnalysisLib.SuccessFailure.Sf where
 
import Prelude hiding (Ord,(<=))
import SemanticsLib.Domain

data Sf
 = None          -- Don't know
 | ForallSuccess -- For all terms, the result, if any, is never failure.
 | ExistsFailure -- There exists a term such that the result is failure.
 | Any           -- Both success and failure may be feasible.
 deriving (Eq, Show)


-- Partial order on Sf

instance POrd Sf
 where
  None <= _   = True
  _    <= Any = True
  x    <= y   = x == y

instance Bottom Sf
 where
  bottom = None

instance Top Sf
 where
  top = Any

instance Lub Sf 
 where
  lub None x    = x
  lub x    None = x
  lub Any  x    = Any
  lub x    Any  = Any
  lub x    y    = if x == y then x else Any


-- Abstract interpretation of sequential composition

seq :: Sf -> Sf -> Sf
seq None          _             = None
seq ForallSuccess None          = None
seq ForallSuccess ForallSuccess = ForallSuccess
seq ForallSuccess _             = Any
seq ExistsFailure _             = ExistsFailure
seq Any           _             = Any


-- Abstract interpretation of left-biased choice

choice ForallSuccess _             = ForallSuccess
choice _             ForallSuccess = ForallSuccess
choice None          _             = None
choice _             None          = None
choice _             _             = Any


{-

-- One option

choice :: Sf -> Sf -> Sf
choice None          _             = None
choice _             None          = None
choice ForallSuccess _             = ForallSuccess
choice _             ForallSuccess = ForallSuccess
choice _             _             = Any

-- Another option

choice :: Sf -> Sf -> Sf
choice None          _             = None
choice ForallSuccess _             = ForallSuccess
choice ExistsFailure None          = None
choice ExistsFailure ForallSuccess = ForallSuccess
choice ExistsFailure _             = Any
choice Any           None          = None
choice Any           ForallSuccess = ForallSuccess
choice Any           _             = Any

-}
