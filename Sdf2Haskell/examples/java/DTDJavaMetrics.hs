module DTDJavaMetrics where

import Text.XML.HaXml.Xml2Haskell


{-Type decls-}

newtype JavaMetrics = JavaMetrics [CuMetric] 		deriving (Eq,Show)
data CuMetric = CuMetric CuMetric_Attrs [CuMetric_]
	      deriving (Eq,Show)
data CuMetric_Attrs = CuMetric_Attrs
    { cuMetricName :: String
    } deriving (Eq,Show)
data CuMetric_ = CuMetric_InterfaceMetric InterfaceMetric
	       | CuMetric_ClassMetric ClassMetric
	       deriving (Eq,Show)
data InterfaceMetric = InterfaceMetric
    { interfaceMetricName :: String
    , interfaceMetricMethodCount :: String
    , interfaceMetricFieldCount :: String
    } deriving (Eq,Show)
data ClassMetric = ClassMetric ClassMetric_Attrs [ClassMetric_]
		 deriving (Eq,Show)
data ClassMetric_Attrs = ClassMetric_Attrs
    { classMetricName :: String
    , classMetricFieldCount :: String
    } deriving (Eq,Show)
data ClassMetric_ = ClassMetric_MethodMetric MethodMetric
		  | ClassMetric_ClassMetric ClassMetric
		  deriving (Eq,Show)
data MethodMetric = MethodMetric MethodMetric_Attrs [ClassMetric]
		  deriving (Eq,Show)
data MethodMetric_Attrs = MethodMetric_Attrs
    { methodMetricName :: String
    , methodMetricStatementCount :: String
    , methodMetricMcCabe :: String
    , methodMetricNestingDepth :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent JavaMetrics where
    fromElem (CElem (Elem "javaMetrics" [] c0):rest) =
	(\(a,ca)->
	   (Just (JavaMetrics a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (JavaMetrics a) =
	[CElem (Elem "javaMetrics" [] (concatMap toElem a))]
instance XmlContent CuMetric where
    fromElem (CElem (Elem "cuMetric" as c0):rest) =
	(\(a,ca)->
	   (Just (CuMetric (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (CuMetric as a) =
	[CElem (Elem "cuMetric" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes CuMetric_Attrs where
    fromAttrs as =
	CuMetric_Attrs
	  { cuMetricName = definiteA fromAttrToStr "cuMetric" "name" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "name" (cuMetricName v)
	]
instance XmlContent CuMetric_ where
    fromElem c0 =
	case (fromElem c0) of
	(Just a,rest) -> (Just (CuMetric_InterfaceMetric a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (CuMetric_ClassMetric a), rest)
		(Nothing,_) ->
		    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (CuMetric_InterfaceMetric a) = toElem a
    toElem (CuMetric_ClassMetric a) = toElem a
instance XmlContent InterfaceMetric where
    fromElem (CElem (Elem "interfaceMetric" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "interfaceMetric" (toAttrs as) [])]
instance XmlAttributes InterfaceMetric where
    fromAttrs as =
	InterfaceMetric
	  { interfaceMetricName = definiteA fromAttrToStr "interfaceMetric" "name" as
	  , interfaceMetricMethodCount = definiteA fromAttrToStr "interfaceMetric" "methodCount" as
	  , interfaceMetricFieldCount = definiteA fromAttrToStr "interfaceMetric" "fieldCount" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "name" (interfaceMetricName v)
	, toAttrFrStr "methodCount" (interfaceMetricMethodCount v)
	, toAttrFrStr "fieldCount" (interfaceMetricFieldCount v)
	]
instance XmlContent ClassMetric where
    fromElem (CElem (Elem "classMetric" as c0):rest) =
	(\(a,ca)->
	   (Just (ClassMetric (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (ClassMetric as a) =
	[CElem (Elem "classMetric" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes ClassMetric_Attrs where
    fromAttrs as =
	ClassMetric_Attrs
	  { classMetricName = definiteA fromAttrToStr "classMetric" "name" as
	  , classMetricFieldCount = definiteA fromAttrToStr "classMetric" "fieldCount" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "name" (classMetricName v)
	, toAttrFrStr "fieldCount" (classMetricFieldCount v)
	]
instance XmlContent ClassMetric_ where
    fromElem c0 =
	case (fromElem c0) of
	(Just a,rest) -> (Just (ClassMetric_MethodMetric a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (ClassMetric_ClassMetric a), rest)
		(Nothing,_) ->
		    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (ClassMetric_MethodMetric a) = toElem a
    toElem (ClassMetric_ClassMetric a) = toElem a
instance XmlContent MethodMetric where
    fromElem (CElem (Elem "methodMetric" as c0):rest) =
	(\(a,ca)->
	   (Just (MethodMetric (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (MethodMetric as a) =
	[CElem (Elem "methodMetric" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes MethodMetric_Attrs where
    fromAttrs as =
	MethodMetric_Attrs
	  { methodMetricName = definiteA fromAttrToStr "methodMetric" "name" as
	  , methodMetricStatementCount = definiteA fromAttrToStr "methodMetric" "statementCount" as
	  , methodMetricMcCabe = definiteA fromAttrToStr "methodMetric" "mcCabe" as
	  , methodMetricNestingDepth = definiteA fromAttrToStr "methodMetric" "nestingDepth" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "name" (methodMetricName v)
	, toAttrFrStr "statementCount" (methodMetricStatementCount v)
	, toAttrFrStr "mcCabe" (methodMetricMcCabe v)
	, toAttrFrStr "nestingDepth" (methodMetricNestingDepth v)
	]


{-Done-}
