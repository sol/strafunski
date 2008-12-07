//  Arguments given: ["Verbatim"]
//  Rendering ...
package com. nwalsh. saxon ;
import java. util. Stack ;
import java. util. StringTokenizer ;
import org. xml. sax . * ;
import org. w3c. dom . * ;
import javax. xml. transform. TransformerException ;
import com. icl. saxon. Controller ;
import com. icl. saxon. expr . * ;
import com. icl. saxon. om . * ;
import com. icl. saxon. pattern . * ;
import com. icl. saxon. Context ;
import com. icl. saxon. tree . * ;
import com. icl. saxon. functions. Extensions ;
import com. nwalsh. saxon. NumberLinesEmitter ;
import com. nwalsh. saxon. CalloutEmitter ;
public class Verbatim
{
private static boolean foStylesheet = false ;
private static int modulus = 0 ;
private static int width = 0 ;
private static String separator = "" ;
private static boolean calloutsSetup = false ;
private static int defaultColumn = 60 ;
private static String graphicsPath = null ;
private static String graphicsExt = null ;
private static int graphicsMax = 10 ;
private static FormatCallout fCallout = null ;
public Verbatim ( ) { }
protected static String
getVariable ( Context context, String varName )
{ Value variable = null ;
  String varString = null ;
  try
  { variable = Extensions. evaluate ( context, "$" + varName ) ;
    varString = variable. asString ( ) ;
    return varString ;
  }
  catch ( TransformerException te )
  { System. out. println ( "Undefined variable: " + varName ) ;
    return "" ;
  }
  catch ( IllegalArgumentException iae )
  { System. out. println ( "Undefined variable: " + varName ) ;
    return "" ;
  }
}
private static void setupLineNumbering ( Context context )
{ modulus = 5 ;
  width = 3 ;
  separator = " " ;
  foStylesheet = false ;
  String varString = null ;
  varString = getVariable ( context, "linenumbering.everyNth" ) ;
  try { modulus = Integer. parseInt ( varString ) ; }
  catch ( NumberFormatException nfe )
  { System. out. println (
    "$linenumbering.everyNth is not a number: " + varString )
    ;
  }
  varString = getVariable ( context, "linenumbering.width" ) ;
  try { width = Integer. parseInt ( varString ) ; }
  catch ( NumberFormatException nfe )
  { System. out. println (
    "$linenumbering.width is not a number: " + varString )
    ;
  }
  varString = getVariable ( context, "linenumbering.separator" ) ;
  separator = varString ;
  varString = getVariable ( context, "stylesheet.result.type" ) ;
  foStylesheet = ( varString. equals ( "fo" ) ) ;
}
public static NodeSetValue
numberLines ( Context context, NodeSetValue rtf_ns )
{ FragmentValue rtf = ( FragmentValue ) rtf_ns ;
  setupLineNumbering ( context ) ;
  try
  { LineCountEmitter lcEmitter = new LineCountEmitter ( ) ;
    rtf. replay ( lcEmitter ) ;
    int numLines = lcEmitter. lineCount ( ) ;
    int listingModulus = numLines < modulus ? 1 : modulus ;
    double log10numLines = Math. log ( numLines ) / Math. log ( 10 ) ;
    int
    listingWidth =
    width < log10numLines + 1 ?
    ( int ) Math. floor ( log10numLines + 1 ) : width
    ;
    Controller controller = context. getController ( ) ;
    NamePool namePool = controller. getNamePool ( ) ;
    NumberLinesEmitter
    nlEmitter =
    new NumberLinesEmitter (
    controller,
    namePool,
    listingModulus,
    listingWidth,
    separator,
    foStylesheet
    )
    ;
    rtf. replay ( nlEmitter ) ;
    return nlEmitter. getResultTreeFragment ( ) ;
  }
  catch ( TransformerException e )
  { System. out. println ( "Transformer Exception in numberLines" ) ;
    return rtf ;
  }
}
private static void setupCallouts ( Context context )
{ NamePool namePool = context. getController ( ) . getNamePool ( )
  ;
  boolean useGraphics = false ;
  boolean useUnicode = false ;
  int unicodeStart = 49 ;
  int unicodeMax = 0 ;
  String unicodeFont = "" ;
  defaultColumn = 60 ;
  graphicsPath = null ;
  graphicsExt = null ;
  graphicsMax = 0 ;
  foStylesheet = false ;
  calloutsSetup = true ;
  Value variable = null ;
  String varString = null ;
  varString = getVariable ( context, "stylesheet.result.type" ) ;
  foStylesheet = ( varString. equals ( "fo" ) ) ;
  varString = getVariable ( context, "callout.defaultcolumn" ) ;
  try { defaultColumn = Integer. parseInt ( varString ) ; }
  catch ( NumberFormatException nfe )
  { System. out. println (
    "$callout.defaultcolumn is not a number: " + varString )
    ;
  }
  varString = getVariable ( context, "callout.graphics" ) ;
  useGraphics =
  ! ( varString. equals ( "0" ) || varString. equals ( "" ) )
  ;
  varString = getVariable ( context, "callout.unicode" ) ;
  useUnicode =
  ! ( varString. equals ( "0" ) || varString. equals ( "" ) )
  ;
  if ( useGraphics )
  { varString = getVariable ( context, "callout.graphics.path" ) ;
    graphicsPath = varString ;
    varString = getVariable ( context, "callout.graphics.extension" ) ;
    graphicsExt = varString ;
    varString =
    getVariable ( context, "callout.graphics.number.limit" )
    ;
    try { graphicsMax = Integer. parseInt ( varString ) ; }
    catch ( NumberFormatException nfe )
    { System. out. println (
      "$callout.graphics.number.limit is not a number: " + varString )
      ;
      graphicsMax = 0 ;
    }
    fCallout =
    new FormatGraphicCallout (
    namePool, graphicsPath, graphicsExt, graphicsMax, foStylesheet )
    ;
  }
  else
  if ( useUnicode )
  { varString =
    getVariable ( context, "callout.unicode.start.character" )
    ;
    try { unicodeStart = Integer. parseInt ( varString ) ; }
    catch ( NumberFormatException nfe )
    { System. out. println (
      "$callout.unicode.start.character is not a number: " + varString )
      ;
      unicodeStart = 48 ;
    }
    varString = getVariable ( context, "callout.unicode.number.limit" )
    ;
    try { unicodeMax = Integer. parseInt ( varString ) ; }
    catch ( NumberFormatException nfe )
    { System. out. println (
      "$callout.unicode.number.limit is not a number: " + varString )
      ;
      unicodeStart = 0 ;
    }
    unicodeFont = getVariable ( context, "callout.unicode.font" ) ;
    if ( unicodeFont == null ) { unicodeFont = "" ; }
    fCallout =
    new FormatUnicodeCallout (
    namePool, unicodeFont, unicodeStart, unicodeMax, foStylesheet )
    ;
  }
  else
  { fCallout = new FormatTextCallout ( namePool, foStylesheet ) ; }
}
public static NodeSetValue
insertCallouts (
Context context, NodeList areaspecNodeList, NodeSetValue rtf_ns )
{ FragmentValue rtf = ( FragmentValue ) rtf_ns ;
  setupCallouts ( context ) ;
  try
  { Controller controller = context. getController ( ) ;
    NamePool namePool = controller. getNamePool ( ) ;
    CalloutEmitter
    cEmitter =
    new CalloutEmitter (
    controller, namePool, defaultColumn, foStylesheet, fCallout )
    ;
    cEmitter. setupCallouts ( areaspecNodeList ) ;
    rtf. replay ( cEmitter ) ;
    return cEmitter. getResultTreeFragment ( ) ;
  }
  catch ( TransformerException e )
  { System. out. println ( "Transformer Exception in insertCallouts"
    )
    ;
    return rtf ;
  }
}
}
