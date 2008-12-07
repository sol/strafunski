------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Ralf Laemmel, Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating a set of
-- Haskell data types from an SDF grammar. This module provides functionality
-- to compensate for bugs in implodePT.

------------------------------------------------------------------------------

module ImplodePT where

import ATermLib ( ATerm(..))


------------------------------------------------------------------------------
-- * Function that aggregates all functions to compensate for bugs in
-- implodePT and applies it iteratively until a fixpoint is reached.

compensateImplodePT :: ATerm -> ATerm
compensateImplodePT = limit (rmSTART . implodeNil . implodeAlternative . implodeTernary . implodeAppl . implodeBracket . implodeTCons . implodeIter . implodeOpt . implodeLit)


------------------------------------------------------------------------------
-- * Functions to compensate for bugs in implodePT

-- | Implode TCons 
implodeTCons :: ATerm -> ATerm
implodeTCons (AAppl "TCons" [AList lst, AAppl "TNil" []])
                                                 = AList (map implodeTCons lst)
implodeTCons (AAppl "TCons" [AAppl str lst, _] ) = AAppl str lst
implodeTCons (AAppl f ts)                        = AAppl f (map implodeTCons ts)
implodeTCons (AList ts)                          = AList (map implodeTCons ts)
implodeTCons t                                   = t

implodeAppl :: ATerm -> ATerm
implodeAppl (AAppl "appl" [ AAppl "prod" [ AList [ symbol ],
                                           AAppl "cf" [ AAppl "sort" sorts ],
                                           attrs
                                         ],
                            AList [ term ]
                          ]) = term
implodeAppl (AAppl f ts)                        = AAppl f (map implodeAppl ts)
implodeAppl (AList ts)                          = AList (map implodeAppl ts)
implodeAppl t                                   = t


-- | Implode Ternary
-- This is need when using a ternary expressions in SDF. For example:
-- Expression ("in set") Expression
implodeTernary :: ATerm -> ATerm
implodeTernary (AAppl x [ AAppl x1 l1, AAppl "TNil" [], AAppl x2 l2]) =
   AAppl x [ AAppl x1 l1, AAppl x2 l2]
implodeTernary (AAppl f ts) = AAppl f (map implodeTernary ts)
implodeTernary (AList ts)   = AList (map implodeTernary ts)
implodeTernary t            = t


-- | Implode literals.
implodeLit :: ATerm -> ATerm
implodeLit (AAppl "lit" [AAppl str []])	= AAppl "" []
implodeLit (AAppl f ts) 		        = AAppl f (map implodeLit ts)
implodeLit (AList ts)    		        = AList (map implodeLit ts)
implodeLit t				            = t

-- | Implode optionals.
implodeOpt :: ATerm -> ATerm
implodeOpt (AAppl "appl" [ AAppl "prod" [ AList [],
                                          AAppl "cf" [ AAppl "opt" [ symbol ]],
			                  attrs
					],
                           AList []
			 ])
  = AAppl "None" [ ]
implodeOpt (AAppl "appl" [ AAppl "prod" [ AList [ symbol ],
                                          AAppl "cf" [ AAppl "opt" [ symbol' ]],
					  attrs
					],
                           AList [ term ]
			 ])
  = AAppl "Some" [ implodeOpt term ]
implodeOpt (AAppl f ts) 		= AAppl f (map implodeOpt ts)
implodeOpt (AList ts)    		= AList (map implodeOpt ts)
implodeOpt t				= t

-- | Implode iterations.
implodeIter :: ATerm -> ATerm
implodeIter (AAppl "appl" 
              [ AAppl "prod" 
	          [ AList [],
                    AAppl "cf" [AAppl "iter" [ sort ]],
		    attrs
		  ],
		AList []
	      ]
	    )
  = AList []	    
implodeIter (AAppl f ts) 		= AAppl f (map implodeIter ts)
implodeIter (AList ts)    		= AList (map implodeIter ts)
implodeIter t				= t

-- | Implode brackets.
implodeBracket :: ATerm -> ATerm
implodeBracket (AAppl "appl"
                 [ AAppl "prod"
		     [ AList [ symbol ],
		       symbol',
		       AAppl "attrs" [AList [AAppl "bracket" []]]
		     ],
		   AList [ term ]
		 ]
	)
  = term
implodeBracket (AAppl f ts) 		= AAppl f (map implodeBracket ts)
implodeBracket (AList ts)    		= AList (map implodeBracket ts)
implodeBracket t			= t

-- | Implode alternatives
-- This is necessary because the ATerm representation after implodePT is not
-- what was required for ATermConvertible
implodeAlternative :: ATerm -> ATerm
implodeAlternative (AAppl "alt" [AInt 1, AList a])
   = AAppl "Left" (map implodeAlternative a)
implodeAlternative (AAppl "alt" [AInt 2, AList a])
   = AAppl "Right" (map implodeAlternative a)
implodeAlternative (AAppl f ts)
   = AAppl f (map implodeAlternative ts)
implodeAlternative (AList ts)
   = AList (map implodeAlternative ts)
implodeAlternative t
   = t
   
-- | Implode nil
-- This is necessary because the ATerm representation after implodePT is not
-- what was required for ATermConvertible
implodeNil :: ATerm -> ATerm
implodeNil (AAppl "TNil" []) = AAppl "()" []
implodeNil (AAppl f ts)      = AAppl f (map implodeNil ts)
implodeNil (AList ts)        = AList (map implodeNil ts)
implodeNil t                 = t
   
-----------------------------------------------------------------------------

-- | Remove start symbol.
rmSTART :: ATerm -> ATerm
rmSTART (AAppl "appl" 
          [ AAppl "prod" 
	     [ AList [AAppl "cf" [AAppl "sort" [AAppl sort []]]],
	       AAppl "sort" [AAppl "\"<START>\"" []],
	       attrs
	     ],
	    AList [ term ] 
	  ]
	)
  = term	
rmSTART t = t


------------------------------------------------------------------------------
-- * Auxiliar function to interatively apply functions until reaching a
-- fixpoint

limit :: (Eq a) => (a -> a) -> a -> a
limit f x = let res = f x
            in if res == x 
               then res
               else limit f res
