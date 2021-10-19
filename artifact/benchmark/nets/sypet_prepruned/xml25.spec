vars
java.lang.String org.jsoup.Connection$Method[] org.jsoup.examples.HtmlToPlainText org.jsoup.examples.ListLinks org.jsoup.helper.DescendableLinkedList org.jsoup.helper.StringUtil org.jsoup.helper.W3CDom org.jsoup.nodes.Attributes org.jsoup.nodes.Document$OutputSettings$Syntax[] org.jsoup.nodes.Document$OutputSettings org.jsoup.nodes.Document$QuirksMode[] org.jsoup.nodes.Entities$CoreCharset[] org.jsoup.nodes.Entities$EscapeMode[] org.jsoup.parser.HtmlTreeBuilderState[] org.jsoup.parser.Parser org.jsoup.parser.Token$TokenType[] org.jsoup.parser.TokeniserState[] org.jsoup.parser.XmlTreeBuilder org.jsoup.safety.Whitelist org.jsoup.select.Elements org.jsoup.select.Evaluator$AllElements org.jsoup.select.Evaluator$IsEmpty org.jsoup.select.Evaluator$IsFirstChild org.jsoup.select.Evaluator$IsFirstOfType org.jsoup.select.Evaluator$IsLastChild org.jsoup.select.Evaluator$IsLastOfType org.jsoup.select.Evaluator$IsOnlyChild org.jsoup.select.Evaluator$IsOnlyOfType org.jsoup.select.Evaluator$IsRoot org.jsoup.Connection$Method indicator_place_java.lang.String org.jsoup.Connection org.jsoup.nodes.Document org.jsoup.UnsupportedMimeTypeException java.lang.Object org.jsoup.helper.HttpConnection$KeyVal boolean org.jsoup.nodes.Attribute int java.util.List java.util.Map org.jsoup.nodes.BooleanAttribute org.jsoup.nodes.Comment org.jsoup.nodes.DataNode org.jsoup.nodes.Document$OutputSettings$Syntax java.nio.charset.Charset org.jsoup.nodes.Entities$EscapeMode org.jsoup.nodes.Document$QuirksMode org.jsoup.nodes.Element org.jsoup.nodes.Node org.jsoup.nodes.DocumentType java.lang.Integer java.util.Set org.jsoup.parser.Tag org.jsoup.nodes.Entities$CoreCharset java.lang.Character org.jsoup.nodes.TextNode org.jsoup.parser.HtmlTreeBuilderState org.jsoup.parser.TreeBuilder org.jsoup.parser.Token$TokenType org.jsoup.parser.TokenQueue org.jsoup.parser.TokeniserState org.jsoup.safety.Cleaner org.jsoup.select.Evaluator$Attribute org.jsoup.select.Evaluator$AttributeKeyPair org.jsoup.select.Evaluator$AttributeStarting org.jsoup.select.Evaluator$AttributeWithValue org.jsoup.select.Evaluator$AttributeWithValueContaining org.jsoup.select.Evaluator$AttributeWithValueEnding org.jsoup.select.Evaluator$AttributeWithValueNot org.jsoup.select.Evaluator$AttributeWithValueStarting org.jsoup.select.Evaluator$Class org.jsoup.select.Evaluator$ContainsOwnText org.jsoup.select.Evaluator$ContainsText org.jsoup.select.Evaluator$CssNthEvaluator org.jsoup.select.Evaluator$Id org.jsoup.select.Evaluator$IndexEquals org.jsoup.select.Evaluator$IndexEvaluator org.jsoup.select.Evaluator$IndexGreaterThan org.jsoup.select.Evaluator$IndexLessThan org.jsoup.select.Evaluator$IsNthChild org.jsoup.select.Evaluator$IsNthLastChild org.jsoup.select.Evaluator$IsNthLastOfType org.jsoup.select.Evaluator$IsNthOfType org.jsoup.select.Evaluator$Tag org.jsoup.select.Evaluator org.jsoup.select.StructuralEvaluator$Has org.jsoup.select.StructuralEvaluator$ImmediateParent org.jsoup.select.StructuralEvaluator$ImmediatePreviousSibling org.jsoup.select.StructuralEvaluator$Not org.jsoup.select.StructuralEvaluator$Parent org.jsoup.select.StructuralEvaluator$PreviousSibling org.jsoup.HttpStatusException org.jsoup.Connection$KeyVal org.w3c.dom.Document org.jsoup.nodes.XmlDeclaration org.jsoup.nodes.FormElement java.io.InputStream org.jsoup.Connection$Request org.jsoup.Connection$Response char org.jsoup.helper.W3CDom$W3CBuilder java.util.Collection byte[]

rules
orgdotjsoupdotConnectiondollarKeyVal >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal-1,
boolean' = boolean+1;

orgdotjsoupdotConnectiondollarKeyVal >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal-1,
javadotiodotInputStream' = javadotiodotInputStream+1;

orgdotjsoupdotConnectiondollarKeyVal >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotConnectiondollarKeyVal >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotConnectiondollarKeyVal >= 1,javadotiodotInputStream >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal+0,
javadotiodotInputStream' = javadotiodotInputStream-1;

orgdotjsoupdotConnectiondollarKeyVal >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotConnectiondollarKeyVal >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotConnectiondollarMethod >= 1 ->
orgdotjsoupdotConnectiondollarMethod' = orgdotjsoupdotConnectiondollarMethod-1,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotConnectiondollarMethod' = orgdotjsoupdotConnectiondollarMethod+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.Connection$Method[] >= 0 ->
orgdotjsoupdotConnectiondollarMethodleftbracketrightbracket' = orgdotjsoupdotConnectiondollarMethodleftbracketrightbracket+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
boolean' = boolean+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
boolean' = boolean+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
boolean' = boolean+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
boolean' = boolean+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
int' = int+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
int' = int+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
javadotutildotCollection' = javadotutildotCollection+1;

orgdotjsoupdotConnectiondollarKeyVal >= 1,orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal-1,
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0;

boolean >= 1,orgdotjsoupdotConnectiondollarRequest >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0;

boolean >= 1,orgdotjsoupdotConnectiondollarRequest >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0;

boolean >= 1,orgdotjsoupdotConnectiondollarRequest >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0;

orgdotjsoupdotConnectiondollarRequest >= 1,int >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0,
int' = int-1;

orgdotjsoupdotConnectiondollarRequest >= 1,orgdotjsoupdotparserdotParser >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1;

orgdotjsoupdotConnectiondollarRequest >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotConnectiondollarRequest >= 1,int >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+0,
int' = int-1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser+1;

boolean >= 1,orgdotjsoupdotConnectiondollarRequest >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
byteleftbracketrightbracket' = byteleftbracketrightbracket+1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
int' = int+1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotutildotMap >= 1,orgdotjsoupdotConnection >= 1 ->
javadotutildotMap' = javadotutildotMap-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 2,javadotiodotInputStream >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-2,
javadotiodotInputStream' = javadotiodotInputStream-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotutildotCollection >= 1,orgdotjsoupdotConnection >= 1 ->
javadotutildotCollection' = javadotutildotCollection-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

javadotutildotMap >= 1,orgdotjsoupdotConnection >= 1 ->
javadotutildotMap' = javadotutildotMap-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

boolean >= 1,orgdotjsoupdotConnection >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

boolean >= 1,orgdotjsoupdotConnection >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

boolean >= 1,orgdotjsoupdotConnection >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

orgdotjsoupdotConnection >= 1,int >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
int' = int-1;

orgdotjsoupdotConnectiondollarMethod >= 1,orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnectiondollarMethod' = orgdotjsoupdotConnectiondollarMethod-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

orgdotjsoupdotConnection >= 1,orgdotjsoupdotparserdotParser >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotConnection >= 1,orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest-1;

orgdotjsoupdotConnectiondollarResponse >= 1,orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

orgdotjsoupdotConnection >= 1,int >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
int' = int-1;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotConnection >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

boolean >= 1,orgdotjsoupdotConnection >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+0;

orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection-1,
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+1;

orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection-1,
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse+1;

orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection-1,
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse+1;

orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1;

orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1;

orgdotjsoupdotHttpStatusException >= 1 ->
orgdotjsoupdotHttpStatusException' = orgdotjsoupdotHttpStatusException-1,
int' = int+1;

orgdotjsoupdotHttpStatusException >= 1 ->
orgdotjsoupdotHttpStatusException' = orgdotjsoupdotHttpStatusException-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 2,int >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
int' = int-1,
orgdotjsoupdotHttpStatusException' = orgdotjsoupdotHttpStatusException+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotsafetydotWhitelist >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotsafetydotWhitelist >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotsafetydotWhitelist >= 1,orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist-1,
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotsafetydotWhitelist >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2,javadotiodotInputStream >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2,orgdotjsoupdotparserdotParser >= 1,javadotiodotInputStream >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2,orgdotjsoupdotparserdotParser >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotUnsupportedMimeTypeException >= 1 ->
orgdotjsoupdotUnsupportedMimeTypeException' = orgdotjsoupdotUnsupportedMimeTypeException-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotUnsupportedMimeTypeException >= 1 ->
orgdotjsoupdotUnsupportedMimeTypeException' = orgdotjsoupdotUnsupportedMimeTypeException-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 3 ->
javadotlangdotString' = javadotlangdotString-3,
orgdotjsoupdotUnsupportedMimeTypeException' = orgdotjsoupdotUnsupportedMimeTypeException+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,orgdotjsoupdotexamplesdotHtmlToPlainText >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotexamplesdotHtmlToPlainText' = orgdotjsoupdotexamplesdotHtmlToPlainText-1,
javadotlangdotString' = javadotlangdotString+1;

org.jsoup.examples.HtmlToPlainText >= 0 ->
orgdotjsoupdotexamplesdotHtmlToPlainText' = orgdotjsoupdotexamplesdotHtmlToPlainText+1;

org.jsoup.examples.ListLinks >= 0 ->
orgdotjsoupdotexamplesdotListLinks' = orgdotjsoupdotexamplesdotListLinks+1;

javadotlangdotString >= 2,javadotiodotInputStream >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2,orgdotjsoupdotparserdotParser >= 1,javadotiodotInputStream >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdothelperdotDescendableLinkedList >= 1 ->
orgdotjsoupdothelperdotDescendableLinkedList' = orgdotjsoupdothelperdotDescendableLinkedList-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdothelperdotDescendableLinkedList >= 1 ->
orgdotjsoupdothelperdotDescendableLinkedList' = orgdotjsoupdothelperdotDescendableLinkedList-1,
javadotlangdotObject' = javadotlangdotObject+1;

org.jsoup.helper.DescendableLinkedList >= 0 ->
orgdotjsoupdothelperdotDescendableLinkedList' = orgdotjsoupdothelperdotDescendableLinkedList+1;

orgdotjsoupdothelperdotDescendableLinkedList >= 1,javadotlangdotObject >= 1 ->
orgdotjsoupdothelperdotDescendableLinkedList' = orgdotjsoupdothelperdotDescendableLinkedList-1,
javadotlangdotObject' = javadotlangdotObject-1;

orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal-1,
boolean' = boolean+1;

orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal-1,
javadotiodotInputStream' = javadotiodotInputStream+1;

orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1,javadotiodotInputStream >= 1 ->
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal-1,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal+1;

javadotlangdotString >= 1,orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal-1,
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1,orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal-1,
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2,javadotiodotInputStream >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1,javadotiodotInputStream >= 1 ->
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal+0,
javadotiodotInputStream' = javadotiodotInputStream-1;

javadotlangdotString >= 1,orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1,orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

int >= 1 ->
int' = int-1,
boolean' = boolean+1;

javadotutildotCollection >= 1,javadotlangdotString >= 1 ->
javadotutildotCollection' = javadotutildotCollection-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

int >= 1 ->
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.helper.StringUtil >= 0 ->
orgdotjsoupdothelperdotStringUtil' = orgdotjsoupdothelperdotStringUtil+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

boolean >= 1 ->
boolean' = boolean-1;

boolean >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

boolean >= 1 ->
boolean' = boolean-1;

boolean >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1;

javadotlangdotObject >= 1,javadotlangdotString >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotw3cdotdomdotDocument >= 1,orgdotjsoupdothelperdotW3CDom >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotjsoupdothelperdotW3CDom' = orgdotjsoupdothelperdotW3CDom-1,
orgdotjsoupdothelperdotW3CDomdollarW3CBuilder' = orgdotjsoupdothelperdotW3CDomdollarW3CBuilder+1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdothelperdotW3CDomdollarW3CBuilder >= 1,int >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdothelperdotW3CDomdollarW3CBuilder' = orgdotjsoupdothelperdotW3CDomdollarW3CBuilder-1,
int' = int-1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdothelperdotW3CDomdollarW3CBuilder >= 1,int >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdothelperdotW3CDomdollarW3CBuilder' = orgdotjsoupdothelperdotW3CDomdollarW3CBuilder-1,
int' = int-1;

orgdotw3cdotdomdotDocument >= 1,orgdotjsoupdothelperdotW3CDom >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotjsoupdothelperdotW3CDom' = orgdotjsoupdothelperdotW3CDom-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdothelperdotW3CDom >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdothelperdotW3CDom' = orgdotjsoupdothelperdotW3CDom-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

org.jsoup.helper.W3CDom >= 0 ->
orgdotjsoupdothelperdotW3CDom' = orgdotjsoupdothelperdotW3CDom+1;

orgdotw3cdotdomdotDocument >= 1,orgdotjsoupdothelperdotW3CDom >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotjsoupdothelperdotW3CDom' = orgdotjsoupdothelperdotW3CDom-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1;

orgdotjsoupdotnodesdotAttribute >= 1,javadotlangdotObject >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotAttribute >= 1,javadotlangdotObject >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotObject' = javadotlangdotObject+0;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotAttribute >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute+0;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttribute >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotObject >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
int' = int+1;

orgdotjsoupdotnodesdotAttributes >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotAttributes >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotutildotMap' = javadotutildotMap+1;

orgdotjsoupdotnodesdotAttributes >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes+0;

org.jsoup.nodes.Attributes >= 0 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes+1;

orgdotjsoupdotnodesdotAttributes >= 2 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-2;

orgdotjsoupdotnodesdotAttributes >= 1,boolean >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1,orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute-1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotBooleanAttribute' = orgdotjsoupdotnodesdotBooleanAttribute+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotComment >= 1 ->
orgdotjsoupdotnodesdotComment' = orgdotjsoupdotnodesdotComment-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotComment >= 1 ->
orgdotjsoupdotnodesdotComment' = orgdotjsoupdotnodesdotComment-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotComment' = orgdotjsoupdotnodesdotComment+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotDataNode >= 1 ->
orgdotjsoupdotnodesdotDataNode' = orgdotjsoupdotnodesdotDataNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotDataNode >= 1 ->
orgdotjsoupdotnodesdotDataNode' = orgdotjsoupdotnodesdotDataNode-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotDataNode' = orgdotjsoupdotnodesdotDataNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotDataNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotDataNode' = orgdotjsoupdotnodesdotDataNode+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotDataNode' = orgdotjsoupdotnodesdotDataNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax' = orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.nodes.Document$OutputSettings$Syntax[] >= 0 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntaxleftbracketrightbracket' = orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntaxleftbracketrightbracket+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
int' = int+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
javadotniodotcharsetdotCharset' = javadotniodotcharsetdotCharset+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotniodotcharsetdotCharset >= 1,orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
javadotniodotcharsetdotCharset' = javadotniodotcharsetdotCharset-1,
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1,orgdotjsoupdotnodesdotEntitiesdollarEscapeMode >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0,
orgdotjsoupdotnodesdotEntitiesdollarEscapeMode' = orgdotjsoupdotnodesdotEntitiesdollarEscapeMode-1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1,int >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0,
int' = int-1;

boolean >= 1,orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0;

boolean >= 1,orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1,orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+0,
orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax' = orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax-1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax' = orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
orgdotjsoupdotnodesdotEntitiesdollarEscapeMode' = orgdotjsoupdotnodesdotEntitiesdollarEscapeMode+1;

org.jsoup.nodes.Document$OutputSettings >= 0 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotDocumentdollarQuirksMode' = orgdotjsoupdotnodesdotDocumentdollarQuirksMode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.nodes.Document$QuirksMode[] >= 0 ->
orgdotjsoupdotnodesdotDocumentdollarQuirksModeleftbracketrightbracket' = orgdotjsoupdotnodesdotDocumentdollarQuirksModeleftbracketrightbracket+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotniodotcharsetdotCharset' = javadotniodotcharsetdotCharset+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+0;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+0;

orgdotjsoupdotnodesdotDocumentdollarQuirksMode >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarQuirksMode' = orgdotjsoupdotnodesdotDocumentdollarQuirksMode-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+0;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotjsoupdotnodesdotDocumentdollarQuirksMode' = orgdotjsoupdotnodesdotDocumentdollarQuirksMode+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1;

orgdotjsoupdotnodesdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1;

orgdotjsoupdotnodesdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotniodotcharsetdotCharset >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
javadotniodotcharsetdotCharset' = javadotniodotcharsetdotCharset-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1;

orgdotjsoupdotnodesdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

boolean >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1;

orgdotjsoupdotnodesdotDocumentType >= 1 ->
orgdotjsoupdotnodesdotDocumentType' = orgdotjsoupdotnodesdotDocumentType-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 4 ->
javadotlangdotString' = javadotlangdotString-4,
orgdotjsoupdotnodesdotDocumentType' = orgdotjsoupdotnodesdotDocumentType+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotObject >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotInteger' = javadotlangdotInteger+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotutildotMap' = javadotutildotMap+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotutildotSet' = javadotutildotSet+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

boolean >= 1,orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1,int >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
int' = int-1;

orgdotjsoupdotnodesdotElement >= 1,javadotutildotSet >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotutildotSet' = javadotutildotSet-1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotutildotCollection >= 1,int >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotutildotCollection' = javadotutildotCollection-1,
int' = int-1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,int >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
int' = int-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotElement >= 1,int >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
int' = int-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotElement >= 1,int >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
int' = int-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

javadotlangdotString >= 1,orgdotjsoupdotparserdotTag >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotString >= 1,orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotEntitiesdollarCoreCharset' = orgdotjsoupdotnodesdotEntitiesdollarCoreCharset+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.nodes.Entities$CoreCharset[] >= 0 ->
orgdotjsoupdotnodesdotEntitiesdollarCoreCharsetleftbracketrightbracket' = orgdotjsoupdotnodesdotEntitiesdollarCoreCharsetleftbracketrightbracket+1;

orgdotjsoupdotnodesdotEntitiesdollarEscapeMode >= 1 ->
orgdotjsoupdotnodesdotEntitiesdollarEscapeMode' = orgdotjsoupdotnodesdotEntitiesdollarEscapeMode-1,
javadotutildotMap' = javadotutildotMap+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotEntitiesdollarEscapeMode' = orgdotjsoupdotnodesdotEntitiesdollarEscapeMode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.nodes.Entities$EscapeMode[] >= 0 ->
orgdotjsoupdotnodesdotEntitiesdollarEscapeModeleftbracketrightbracket' = orgdotjsoupdotnodesdotEntitiesdollarEscapeModeleftbracketrightbracket+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotCharacter' = javadotlangdotCharacter+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotFormElement >= 1 ->
orgdotjsoupdotnodesdotFormElement' = orgdotjsoupdotnodesdotFormElement-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotFormElement >= 1 ->
orgdotjsoupdotnodesdotFormElement' = orgdotjsoupdotnodesdotFormElement-1,
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+1;

orgdotjsoupdotnodesdotElement >= 1,orgdotjsoupdotnodesdotFormElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotnodesdotFormElement' = orgdotjsoupdotnodesdotFormElement+0;

orgdotjsoupdotnodesdotFormElement >= 1 ->
orgdotjsoupdotnodesdotFormElement' = orgdotjsoupdotnodesdotFormElement-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotString >= 1,orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
orgdotjsoupdotnodesdotFormElement' = orgdotjsoupdotnodesdotFormElement+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotObject >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
int' = int+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
int' = int+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 2 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 2 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1;

orgdotjsoupdotnodesdotNode >= 1,int >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
int' = int-1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1;

orgdotjsoupdotnodesdotNode >= 2 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-2;

orgdotjsoupdotnodesdotNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1,javadotlangdotObject >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotTextNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
boolean' = boolean+1;

orgdotjsoupdotnodesdotTextNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes+1;

orgdotjsoupdotnodesdotTextNode >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotTextNode >= 1,int >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode+0,
int' = int-1;

orgdotjsoupdotnodesdotTextNode >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotXmlDeclaration >= 1 ->
orgdotjsoupdotnodesdotXmlDeclaration' = orgdotjsoupdotnodesdotXmlDeclaration-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotnodesdotXmlDeclaration >= 1 ->
orgdotjsoupdotnodesdotXmlDeclaration' = orgdotjsoupdotnodesdotXmlDeclaration-1,
javadotlangdotString' = javadotlangdotString+1;

boolean >= 1,javadotlangdotString >= 2 ->
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotXmlDeclaration' = orgdotjsoupdotnodesdotXmlDeclaration+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotHtmlTreeBuilderState' = orgdotjsoupdotparserdotHtmlTreeBuilderState+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.parser.HtmlTreeBuilderState[] >= 0 ->
orgdotjsoupdotparserdotHtmlTreeBuilderStateleftbracketrightbracket' = orgdotjsoupdotparserdotHtmlTreeBuilderStateleftbracketrightbracket+1;

orgdotjsoupdotparserdotParser >= 1 ->
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1,
boolean' = boolean+1;

boolean >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotParser >= 1 ->
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
javadotutildotList' = javadotutildotList+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
javadotutildotList' = javadotutildotList+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2,orgdotjsoupdotparserdotParser >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.parser.Parser >= 0 ->
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser+1;

orgdotjsoupdotparserdotParser >= 1,int >= 1 ->
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser+0,
int' = int-1;

orgdotjsoupdotparserdotTreeBuilder >= 1,orgdotjsoupdotparserdotParser >= 1 ->
orgdotjsoupdotparserdotTreeBuilder' = orgdotjsoupdotparserdotTreeBuilder-1,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser+0;

org.jsoup.parser.Parser >= 0 ->
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser+1;

orgdotjsoupdotparserdotParser >= 1 ->
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser-1,
orgdotjsoupdotparserdotTreeBuilder' = orgdotjsoupdotparserdotTreeBuilder+1;

orgdotjsoupdotparserdotTreeBuilder >= 1 ->
orgdotjsoupdotparserdotTreeBuilder' = orgdotjsoupdotparserdotTreeBuilder-1,
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

javadotlangdotObject >= 1,orgdotjsoupdotparserdotTag >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotTokendollarTokenType' = orgdotjsoupdotparserdotTokendollarTokenType+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.parser.Token$TokenType[] >= 0 ->
orgdotjsoupdotparserdotTokendollarTokenTypeleftbracketrightbracket' = orgdotjsoupdotparserdotTokendollarTokenTypeleftbracketrightbracket+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
boolean' = boolean+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
char' = char+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
char' = char+1;

orgdotjsoupdotparserdotTokenQueue >= 1,char >= 2 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
char' = char-2,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotCharacter >= 1,orgdotjsoupdotparserdotTokenQueue >= 1 ->
javadotlangdotCharacter' = javadotlangdotCharacter-1,
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1;

orgdotjsoupdotparserdotTokenQueue >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue-1,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotTokeniserState' = orgdotjsoupdotparserdotTokeniserState+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.parser.TokeniserState[] >= 0 ->
orgdotjsoupdotparserdotTokeniserStateleftbracketrightbracket' = orgdotjsoupdotparserdotTokeniserStateleftbracketrightbracket+1;

orgdotjsoupdotnodesdotAttributes >= 1,orgdotjsoupdotparserdotTreeBuilder >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
orgdotjsoupdotparserdotTreeBuilder' = orgdotjsoupdotparserdotTreeBuilder-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotAttributes >= 1,javadotlangdotString >= 1,orgdotjsoupdotparserdotXmlTreeBuilder >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotparserdotXmlTreeBuilder' = orgdotjsoupdotparserdotXmlTreeBuilder-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.parser.XmlTreeBuilder >= 0 ->
orgdotjsoupdotparserdotXmlTreeBuilder' = orgdotjsoupdotparserdotXmlTreeBuilder+1;

orgdotjsoupdotsafetydotCleaner >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotsafetydotCleaner' = orgdotjsoupdotsafetydotCleaner-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
boolean' = boolean+1;

orgdotjsoupdotsafetydotCleaner >= 1,orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotsafetydotCleaner' = orgdotjsoupdotsafetydotCleaner-1,
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+0;

orgdotjsoupdotsafetydotWhitelist >= 1 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist-1,
orgdotjsoupdotsafetydotCleaner' = orgdotjsoupdotsafetydotCleaner+1;

orgdotjsoupdotsafetydotWhitelist >= 1,javadotlangdotString >= 3 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+0,
javadotlangdotString' = javadotlangdotString-3,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.safety.Whitelist >= 0 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+1;

org.jsoup.safety.Whitelist >= 0 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+1;

org.jsoup.safety.Whitelist >= 0 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+1;

orgdotjsoupdotsafetydotWhitelist >= 1,boolean >= 1 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+0,
boolean' = boolean-1;

org.jsoup.safety.Whitelist >= 0 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+1;

orgdotjsoupdotsafetydotWhitelist >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+0,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.safety.Whitelist >= 0 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+1;

org.jsoup.safety.Whitelist >= 0 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+1;

orgdotjsoupdotselectdotEvaluator >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
boolean' = boolean+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString+0,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
javadotutildotList' = javadotutildotList+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 2 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-2,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0;

orgdotjsoupdotselectdotElements >= 1,int >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
int' = int-1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotElements >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+0,
javadotlangdotString' = javadotlangdotString-1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

org.jsoup.select.Elements >= 0 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

int >= 1 ->
int' = int-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

javadotutildotCollection >= 1 ->
javadotutildotCollection' = javadotutildotCollection-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

javadotutildotList >= 1 ->
javadotutildotList' = javadotutildotList-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarAllElements >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarAllElements' = orgdotjsoupdotselectdotEvaluatordollarAllElements-1,
boolean' = boolean+1;

org.jsoup.select.Evaluator$AllElements >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarAllElements' = orgdotjsoupdotselectdotEvaluatordollarAllElements+1;

orgdotjsoupdotselectdotEvaluatordollarAttribute >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarAttribute' = orgdotjsoupdotselectdotEvaluatordollarAttribute-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluatordollarAttribute' = orgdotjsoupdotselectdotEvaluatordollarAttribute+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeKeyPair' = orgdotjsoupdotselectdotEvaluatordollarAttributeKeyPair+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarAttributeStarting >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeStarting' = orgdotjsoupdotselectdotEvaluatordollarAttributeStarting-1,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluatordollarAttributeStarting' = orgdotjsoupdotselectdotEvaluatordollarAttributeStarting+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining-1,
boolean' = boolean+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot-1,
boolean' = boolean+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarClass >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarClass' = orgdotjsoupdotselectdotEvaluatordollarClass-1,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluatordollarClass' = orgdotjsoupdotselectdotEvaluatordollarClass+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarContainsOwnText >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarContainsOwnText' = orgdotjsoupdotselectdotEvaluatordollarContainsOwnText-1,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluatordollarContainsOwnText' = orgdotjsoupdotselectdotEvaluatordollarContainsOwnText+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluatordollarContainsText >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarContainsText' = orgdotjsoupdotselectdotEvaluatordollarContainsText-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluatordollarContainsText' = orgdotjsoupdotselectdotEvaluatordollarContainsText+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator' = orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

int >= 1 ->
int' = int-1,
orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator' = orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator+1;

int >= 2 ->
int' = int-2,
orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator' = orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarId >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarId' = orgdotjsoupdotselectdotEvaluatordollarId-1,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluatordollarId' = orgdotjsoupdotselectdotEvaluatordollarId+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluatordollarIndexEquals >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarIndexEquals' = orgdotjsoupdotselectdotEvaluatordollarIndexEquals-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

int >= 1 ->
int' = int-1,
orgdotjsoupdotselectdotEvaluatordollarIndexEquals' = orgdotjsoupdotselectdotEvaluatordollarIndexEquals+1;

int >= 1 ->
int' = int-1,
orgdotjsoupdotselectdotEvaluatordollarIndexEvaluator' = orgdotjsoupdotselectdotEvaluatordollarIndexEvaluator+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan' = orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan-1,
boolean' = boolean+1;

int >= 1 ->
int' = int-1,
orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan' = orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan+1;

orgdotjsoupdotselectdotEvaluatordollarIndexLessThan >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarIndexLessThan' = orgdotjsoupdotselectdotEvaluatordollarIndexLessThan-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

int >= 1 ->
int' = int-1,
orgdotjsoupdotselectdotEvaluatordollarIndexLessThan' = orgdotjsoupdotselectdotEvaluatordollarIndexLessThan+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarIsEmpty >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarIsEmpty' = orgdotjsoupdotselectdotEvaluatordollarIsEmpty-1,
boolean' = boolean+1;

org.jsoup.select.Evaluator$IsEmpty >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsEmpty' = orgdotjsoupdotselectdotEvaluatordollarIsEmpty+1;

orgdotjsoupdotselectdotEvaluatordollarIsFirstChild >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarIsFirstChild' = orgdotjsoupdotselectdotEvaluatordollarIsFirstChild-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

org.jsoup.select.Evaluator$IsFirstChild >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsFirstChild' = orgdotjsoupdotselectdotEvaluatordollarIsFirstChild+1;

org.jsoup.select.Evaluator$IsFirstOfType >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsFirstOfType' = orgdotjsoupdotselectdotEvaluatordollarIsFirstOfType+1;

orgdotjsoupdotselectdotEvaluatordollarIsLastChild >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarIsLastChild' = orgdotjsoupdotselectdotEvaluatordollarIsLastChild-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

org.jsoup.select.Evaluator$IsLastChild >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsLastChild' = orgdotjsoupdotselectdotEvaluatordollarIsLastChild+1;

org.jsoup.select.Evaluator$IsLastOfType >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsLastOfType' = orgdotjsoupdotselectdotEvaluatordollarIsLastOfType+1;

int >= 2 ->
int' = int-2,
orgdotjsoupdotselectdotEvaluatordollarIsNthChild' = orgdotjsoupdotselectdotEvaluatordollarIsNthChild+1;

int >= 2 ->
int' = int-2,
orgdotjsoupdotselectdotEvaluatordollarIsNthLastChild' = orgdotjsoupdotselectdotEvaluatordollarIsNthLastChild+1;

int >= 2 ->
int' = int-2,
orgdotjsoupdotselectdotEvaluatordollarIsNthLastOfType' = orgdotjsoupdotselectdotEvaluatordollarIsNthLastOfType+1;

int >= 2 ->
int' = int-2,
orgdotjsoupdotselectdotEvaluatordollarIsNthOfType' = orgdotjsoupdotselectdotEvaluatordollarIsNthOfType+1;

orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild' = orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

org.jsoup.select.Evaluator$IsOnlyChild >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild' = orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType' = orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType-1,
boolean' = boolean+1;

org.jsoup.select.Evaluator$IsOnlyOfType >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType' = orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarIsRoot >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarIsRoot' = orgdotjsoupdotselectdotEvaluatordollarIsRoot-1,
boolean' = boolean+1;

org.jsoup.select.Evaluator$IsRoot >= 0 ->
orgdotjsoupdotselectdotEvaluatordollarIsRoot' = orgdotjsoupdotselectdotEvaluatordollarIsRoot+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotEvaluatordollarTag >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotEvaluatordollarTag' = orgdotjsoupdotselectdotEvaluatordollarTag-1,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluatordollarTag' = orgdotjsoupdotselectdotEvaluatordollarTag+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluator >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotnodesdotElement >= 1,javadotlangdotString >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

orgdotjsoupdotselectdotEvaluator >= 1,orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-1,
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarHas >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarHas' = orgdotjsoupdotselectdotStructuralEvaluatordollarHas-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

orgdotjsoupdotselectdotEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotselectdotStructuralEvaluatordollarHas' = orgdotjsoupdotselectdotStructuralEvaluatordollarHas+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent' = orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

orgdotjsoupdotselectdotEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent' = orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling' = orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

orgdotjsoupdotselectdotEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling' = orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotStructuralEvaluatordollarNot >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotStructuralEvaluatordollarNot' = orgdotjsoupdotselectdotStructuralEvaluatordollarNot-1,
boolean' = boolean+1;

orgdotjsoupdotselectdotEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotselectdotStructuralEvaluatordollarNot' = orgdotjsoupdotselectdotStructuralEvaluatordollarNot+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarParent >= 1,orgdotjsoupdotnodesdotElement >= 2 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarParent' = orgdotjsoupdotselectdotStructuralEvaluatordollarParent-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
boolean' = boolean+1;

orgdotjsoupdotselectdotEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotselectdotStructuralEvaluatordollarParent' = orgdotjsoupdotselectdotStructuralEvaluatordollarParent+1;

orgdotjsoupdotnodesdotElement >= 2,orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement-2,
orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling' = orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling-1,
boolean' = boolean+1;

orgdotjsoupdotselectdotEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator-1,
orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling' = orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument-1,
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotObject' = javadotlangdotObject+1;

boolean >= 1 ->
boolean' = boolean+1;

byteleftbracketrightbracket >= 1 ->
byteleftbracketrightbracket' = byteleftbracketrightbracket+1;

char >= 1 ->
char' = char+1;

int >= 1 ->
int' = int+1;

javadotiodotInputStream >= 1 ->
javadotiodotInputStream' = javadotiodotInputStream+1;

javadotlangdotCharacter >= 1 ->
javadotlangdotCharacter' = javadotlangdotCharacter+1;

javadotlangdotInteger >= 1 ->
javadotlangdotInteger' = javadotlangdotInteger+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString+1,
indicator_place_javadotlangdotString' = indicator_place_javadotlangdotString+1;

javadotniodotcharsetdotCharset >= 1 ->
javadotniodotcharsetdotCharset' = javadotniodotcharsetdotCharset+1;

javadotutildotCollection >= 1 ->
javadotutildotCollection' = javadotutildotCollection+1;

javadotutildotList >= 1 ->
javadotutildotList' = javadotutildotList+1;

javadotutildotMap >= 1 ->
javadotutildotMap' = javadotutildotMap+1;

javadotutildotSet >= 1 ->
javadotutildotSet' = javadotutildotSet+1;

orgdotjsoupdotConnection >= 1 ->
orgdotjsoupdotConnection' = orgdotjsoupdotConnection+1;

orgdotjsoupdotConnectiondollarKeyVal >= 1 ->
orgdotjsoupdotConnectiondollarKeyVal' = orgdotjsoupdotConnectiondollarKeyVal+1;

orgdotjsoupdotConnectiondollarMethod >= 1 ->
orgdotjsoupdotConnectiondollarMethod' = orgdotjsoupdotConnectiondollarMethod+1;

orgdotjsoupdotConnectiondollarMethodleftbracketrightbracket >= 1 ->
orgdotjsoupdotConnectiondollarMethodleftbracketrightbracket' = orgdotjsoupdotConnectiondollarMethodleftbracketrightbracket+1;

orgdotjsoupdotConnectiondollarRequest >= 1 ->
orgdotjsoupdotConnectiondollarRequest' = orgdotjsoupdotConnectiondollarRequest+1;

orgdotjsoupdotConnectiondollarResponse >= 1 ->
orgdotjsoupdotConnectiondollarResponse' = orgdotjsoupdotConnectiondollarResponse+1;

orgdotjsoupdotHttpStatusException >= 1 ->
orgdotjsoupdotHttpStatusException' = orgdotjsoupdotHttpStatusException+1;

orgdotjsoupdotUnsupportedMimeTypeException >= 1 ->
orgdotjsoupdotUnsupportedMimeTypeException' = orgdotjsoupdotUnsupportedMimeTypeException+1;

orgdotjsoupdotexamplesdotHtmlToPlainText >= 1 ->
orgdotjsoupdotexamplesdotHtmlToPlainText' = orgdotjsoupdotexamplesdotHtmlToPlainText+1;

orgdotjsoupdotexamplesdotListLinks >= 1 ->
orgdotjsoupdotexamplesdotListLinks' = orgdotjsoupdotexamplesdotListLinks+1;

orgdotjsoupdothelperdotDescendableLinkedList >= 1 ->
orgdotjsoupdothelperdotDescendableLinkedList' = orgdotjsoupdothelperdotDescendableLinkedList+1;

orgdotjsoupdothelperdotHttpConnectiondollarKeyVal >= 1 ->
orgdotjsoupdothelperdotHttpConnectiondollarKeyVal' = orgdotjsoupdothelperdotHttpConnectiondollarKeyVal+1;

orgdotjsoupdothelperdotStringUtil >= 1 ->
orgdotjsoupdothelperdotStringUtil' = orgdotjsoupdothelperdotStringUtil+1;

orgdotjsoupdothelperdotW3CDom >= 1 ->
orgdotjsoupdothelperdotW3CDom' = orgdotjsoupdothelperdotW3CDom+1;

orgdotjsoupdothelperdotW3CDomdollarW3CBuilder >= 1 ->
orgdotjsoupdothelperdotW3CDomdollarW3CBuilder' = orgdotjsoupdothelperdotW3CDomdollarW3CBuilder+1;

orgdotjsoupdotnodesdotAttribute >= 1 ->
orgdotjsoupdotnodesdotAttribute' = orgdotjsoupdotnodesdotAttribute+1;

orgdotjsoupdotnodesdotAttributes >= 1 ->
orgdotjsoupdotnodesdotAttributes' = orgdotjsoupdotnodesdotAttributes+1;

orgdotjsoupdotnodesdotBooleanAttribute >= 1 ->
orgdotjsoupdotnodesdotBooleanAttribute' = orgdotjsoupdotnodesdotBooleanAttribute+1;

orgdotjsoupdotnodesdotComment >= 1 ->
orgdotjsoupdotnodesdotComment' = orgdotjsoupdotnodesdotComment+1;

orgdotjsoupdotnodesdotDataNode >= 1 ->
orgdotjsoupdotnodesdotDataNode' = orgdotjsoupdotnodesdotDataNode+1;

orgdotjsoupdotnodesdotDocument >= 1 ->
orgdotjsoupdotnodesdotDocument' = orgdotjsoupdotnodesdotDocument+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettings >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettings' = orgdotjsoupdotnodesdotDocumentdollarOutputSettings+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax' = orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntax+1;

orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntaxleftbracketrightbracket >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntaxleftbracketrightbracket' = orgdotjsoupdotnodesdotDocumentdollarOutputSettingsdollarSyntaxleftbracketrightbracket+1;

orgdotjsoupdotnodesdotDocumentdollarQuirksMode >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarQuirksMode' = orgdotjsoupdotnodesdotDocumentdollarQuirksMode+1;

orgdotjsoupdotnodesdotDocumentdollarQuirksModeleftbracketrightbracket >= 1 ->
orgdotjsoupdotnodesdotDocumentdollarQuirksModeleftbracketrightbracket' = orgdotjsoupdotnodesdotDocumentdollarQuirksModeleftbracketrightbracket+1;

orgdotjsoupdotnodesdotDocumentType >= 1 ->
orgdotjsoupdotnodesdotDocumentType' = orgdotjsoupdotnodesdotDocumentType+1;

orgdotjsoupdotnodesdotElement >= 1 ->
orgdotjsoupdotnodesdotElement' = orgdotjsoupdotnodesdotElement+1;

orgdotjsoupdotnodesdotEntitiesdollarCoreCharset >= 1 ->
orgdotjsoupdotnodesdotEntitiesdollarCoreCharset' = orgdotjsoupdotnodesdotEntitiesdollarCoreCharset+1;

orgdotjsoupdotnodesdotEntitiesdollarCoreCharsetleftbracketrightbracket >= 1 ->
orgdotjsoupdotnodesdotEntitiesdollarCoreCharsetleftbracketrightbracket' = orgdotjsoupdotnodesdotEntitiesdollarCoreCharsetleftbracketrightbracket+1;

orgdotjsoupdotnodesdotEntitiesdollarEscapeMode >= 1 ->
orgdotjsoupdotnodesdotEntitiesdollarEscapeMode' = orgdotjsoupdotnodesdotEntitiesdollarEscapeMode+1;

orgdotjsoupdotnodesdotEntitiesdollarEscapeModeleftbracketrightbracket >= 1 ->
orgdotjsoupdotnodesdotEntitiesdollarEscapeModeleftbracketrightbracket' = orgdotjsoupdotnodesdotEntitiesdollarEscapeModeleftbracketrightbracket+1;

orgdotjsoupdotnodesdotFormElement >= 1 ->
orgdotjsoupdotnodesdotFormElement' = orgdotjsoupdotnodesdotFormElement+1;

orgdotjsoupdotnodesdotNode >= 1 ->
orgdotjsoupdotnodesdotNode' = orgdotjsoupdotnodesdotNode+1;

orgdotjsoupdotnodesdotTextNode >= 1 ->
orgdotjsoupdotnodesdotTextNode' = orgdotjsoupdotnodesdotTextNode+1;

orgdotjsoupdotnodesdotXmlDeclaration >= 1 ->
orgdotjsoupdotnodesdotXmlDeclaration' = orgdotjsoupdotnodesdotXmlDeclaration+1;

orgdotjsoupdotparserdotHtmlTreeBuilderState >= 1 ->
orgdotjsoupdotparserdotHtmlTreeBuilderState' = orgdotjsoupdotparserdotHtmlTreeBuilderState+1;

orgdotjsoupdotparserdotHtmlTreeBuilderStateleftbracketrightbracket >= 1 ->
orgdotjsoupdotparserdotHtmlTreeBuilderStateleftbracketrightbracket' = orgdotjsoupdotparserdotHtmlTreeBuilderStateleftbracketrightbracket+1;

orgdotjsoupdotparserdotParser >= 1 ->
orgdotjsoupdotparserdotParser' = orgdotjsoupdotparserdotParser+1;

orgdotjsoupdotparserdotTag >= 1 ->
orgdotjsoupdotparserdotTag' = orgdotjsoupdotparserdotTag+1;

orgdotjsoupdotparserdotTokendollarTokenType >= 1 ->
orgdotjsoupdotparserdotTokendollarTokenType' = orgdotjsoupdotparserdotTokendollarTokenType+1;

orgdotjsoupdotparserdotTokendollarTokenTypeleftbracketrightbracket >= 1 ->
orgdotjsoupdotparserdotTokendollarTokenTypeleftbracketrightbracket' = orgdotjsoupdotparserdotTokendollarTokenTypeleftbracketrightbracket+1;

orgdotjsoupdotparserdotTokenQueue >= 1 ->
orgdotjsoupdotparserdotTokenQueue' = orgdotjsoupdotparserdotTokenQueue+1;

orgdotjsoupdotparserdotTokeniserState >= 1 ->
orgdotjsoupdotparserdotTokeniserState' = orgdotjsoupdotparserdotTokeniserState+1;

orgdotjsoupdotparserdotTokeniserStateleftbracketrightbracket >= 1 ->
orgdotjsoupdotparserdotTokeniserStateleftbracketrightbracket' = orgdotjsoupdotparserdotTokeniserStateleftbracketrightbracket+1;

orgdotjsoupdotparserdotTreeBuilder >= 1 ->
orgdotjsoupdotparserdotTreeBuilder' = orgdotjsoupdotparserdotTreeBuilder+1;

orgdotjsoupdotparserdotXmlTreeBuilder >= 1 ->
orgdotjsoupdotparserdotXmlTreeBuilder' = orgdotjsoupdotparserdotXmlTreeBuilder+1;

orgdotjsoupdotsafetydotCleaner >= 1 ->
orgdotjsoupdotsafetydotCleaner' = orgdotjsoupdotsafetydotCleaner+1;

orgdotjsoupdotsafetydotWhitelist >= 1 ->
orgdotjsoupdotsafetydotWhitelist' = orgdotjsoupdotsafetydotWhitelist+1;

orgdotjsoupdotselectdotElements >= 1 ->
orgdotjsoupdotselectdotElements' = orgdotjsoupdotselectdotElements+1;

orgdotjsoupdotselectdotEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluator' = orgdotjsoupdotselectdotEvaluator+1;

orgdotjsoupdotselectdotEvaluatordollarAllElements >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAllElements' = orgdotjsoupdotselectdotEvaluatordollarAllElements+1;

orgdotjsoupdotselectdotEvaluatordollarAttribute >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttribute' = orgdotjsoupdotselectdotEvaluatordollarAttribute+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeKeyPair >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeKeyPair' = orgdotjsoupdotselectdotEvaluatordollarAttributeKeyPair+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeStarting >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeStarting' = orgdotjsoupdotselectdotEvaluatordollarAttributeStarting+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValue+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueContaining+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueEnding+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueNot+1;

orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting' = orgdotjsoupdotselectdotEvaluatordollarAttributeWithValueStarting+1;

orgdotjsoupdotselectdotEvaluatordollarClass >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarClass' = orgdotjsoupdotselectdotEvaluatordollarClass+1;

orgdotjsoupdotselectdotEvaluatordollarContainsOwnText >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarContainsOwnText' = orgdotjsoupdotselectdotEvaluatordollarContainsOwnText+1;

orgdotjsoupdotselectdotEvaluatordollarContainsText >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarContainsText' = orgdotjsoupdotselectdotEvaluatordollarContainsText+1;

orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator' = orgdotjsoupdotselectdotEvaluatordollarCssNthEvaluator+1;

orgdotjsoupdotselectdotEvaluatordollarId >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarId' = orgdotjsoupdotselectdotEvaluatordollarId+1;

orgdotjsoupdotselectdotEvaluatordollarIndexEquals >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIndexEquals' = orgdotjsoupdotselectdotEvaluatordollarIndexEquals+1;

orgdotjsoupdotselectdotEvaluatordollarIndexEvaluator >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIndexEvaluator' = orgdotjsoupdotselectdotEvaluatordollarIndexEvaluator+1;

orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan' = orgdotjsoupdotselectdotEvaluatordollarIndexGreaterThan+1;

orgdotjsoupdotselectdotEvaluatordollarIndexLessThan >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIndexLessThan' = orgdotjsoupdotselectdotEvaluatordollarIndexLessThan+1;

orgdotjsoupdotselectdotEvaluatordollarIsEmpty >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsEmpty' = orgdotjsoupdotselectdotEvaluatordollarIsEmpty+1;

orgdotjsoupdotselectdotEvaluatordollarIsFirstChild >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsFirstChild' = orgdotjsoupdotselectdotEvaluatordollarIsFirstChild+1;

orgdotjsoupdotselectdotEvaluatordollarIsFirstOfType >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsFirstOfType' = orgdotjsoupdotselectdotEvaluatordollarIsFirstOfType+1;

orgdotjsoupdotselectdotEvaluatordollarIsLastChild >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsLastChild' = orgdotjsoupdotselectdotEvaluatordollarIsLastChild+1;

orgdotjsoupdotselectdotEvaluatordollarIsLastOfType >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsLastOfType' = orgdotjsoupdotselectdotEvaluatordollarIsLastOfType+1;

orgdotjsoupdotselectdotEvaluatordollarIsNthChild >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsNthChild' = orgdotjsoupdotselectdotEvaluatordollarIsNthChild+1;

orgdotjsoupdotselectdotEvaluatordollarIsNthLastChild >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsNthLastChild' = orgdotjsoupdotselectdotEvaluatordollarIsNthLastChild+1;

orgdotjsoupdotselectdotEvaluatordollarIsNthLastOfType >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsNthLastOfType' = orgdotjsoupdotselectdotEvaluatordollarIsNthLastOfType+1;

orgdotjsoupdotselectdotEvaluatordollarIsNthOfType >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsNthOfType' = orgdotjsoupdotselectdotEvaluatordollarIsNthOfType+1;

orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild' = orgdotjsoupdotselectdotEvaluatordollarIsOnlyChild+1;

orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType' = orgdotjsoupdotselectdotEvaluatordollarIsOnlyOfType+1;

orgdotjsoupdotselectdotEvaluatordollarIsRoot >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarIsRoot' = orgdotjsoupdotselectdotEvaluatordollarIsRoot+1;

orgdotjsoupdotselectdotEvaluatordollarTag >= 1 ->
orgdotjsoupdotselectdotEvaluatordollarTag' = orgdotjsoupdotselectdotEvaluatordollarTag+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarHas >= 1 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarHas' = orgdotjsoupdotselectdotStructuralEvaluatordollarHas+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent >= 1 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent' = orgdotjsoupdotselectdotStructuralEvaluatordollarImmediateParent+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling >= 1 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling' = orgdotjsoupdotselectdotStructuralEvaluatordollarImmediatePreviousSibling+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarNot >= 1 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarNot' = orgdotjsoupdotselectdotStructuralEvaluatordollarNot+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarParent >= 1 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarParent' = orgdotjsoupdotselectdotStructuralEvaluatordollarParent+1;

orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling >= 1 ->
orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling' = orgdotjsoupdotselectdotStructuralEvaluatordollarPreviousSibling+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;



init
java.lang.String=1, org.jsoup.Connection$Method[]=0, org.jsoup.examples.HtmlToPlainText=0, org.jsoup.examples.ListLinks=0, org.jsoup.helper.DescendableLinkedList=0, org.jsoup.helper.StringUtil=0, org.jsoup.helper.W3CDom=0, org.jsoup.nodes.Attributes=0, org.jsoup.nodes.Document$OutputSettings$Syntax[]=0, org.jsoup.nodes.Document$OutputSettings=0, org.jsoup.nodes.Document$QuirksMode[]=0, org.jsoup.nodes.Entities$CoreCharset[]=0, org.jsoup.nodes.Entities$EscapeMode[]=0, org.jsoup.parser.HtmlTreeBuilderState[]=0, org.jsoup.parser.Parser=0, org.jsoup.parser.Token$TokenType[]=0, org.jsoup.parser.TokeniserState[]=0, org.jsoup.parser.XmlTreeBuilder=0, org.jsoup.safety.Whitelist=0, org.jsoup.select.Elements=0, org.jsoup.select.Evaluator$AllElements=0, org.jsoup.select.Evaluator$IsEmpty=0, org.jsoup.select.Evaluator$IsFirstChild=0, org.jsoup.select.Evaluator$IsFirstOfType=0, org.jsoup.select.Evaluator$IsLastChild=0, org.jsoup.select.Evaluator$IsLastOfType=0, org.jsoup.select.Evaluator$IsOnlyChild=0, org.jsoup.select.Evaluator$IsOnlyOfType=0, org.jsoup.select.Evaluator$IsRoot=0, org.jsoup.Connection$Method=0, indicator_place_java.lang.String=0, org.jsoup.Connection=0, org.jsoup.nodes.Document=0, org.jsoup.UnsupportedMimeTypeException=0, java.lang.Object=0, org.jsoup.helper.HttpConnection$KeyVal=0, boolean=0, org.jsoup.nodes.Attribute=0, int=0, java.util.List=0, java.util.Map=0, org.jsoup.nodes.BooleanAttribute=0, org.jsoup.nodes.Comment=0, org.jsoup.nodes.DataNode=0, org.jsoup.nodes.Document$OutputSettings$Syntax=0, java.nio.charset.Charset=0, org.jsoup.nodes.Entities$EscapeMode=0, org.jsoup.nodes.Document$QuirksMode=0, org.jsoup.nodes.Element=0, org.jsoup.nodes.Node=0, org.jsoup.nodes.DocumentType=0, java.lang.Integer=0, java.util.Set=0, org.jsoup.parser.Tag=0, org.jsoup.nodes.Entities$CoreCharset=0, java.lang.Character=0, org.jsoup.nodes.TextNode=0, org.jsoup.parser.HtmlTreeBuilderState=0, org.jsoup.parser.TreeBuilder=0, org.jsoup.parser.Token$TokenType=0, org.jsoup.parser.TokenQueue=0, org.jsoup.parser.TokeniserState=0, org.jsoup.safety.Cleaner=0, org.jsoup.select.Evaluator$Attribute=0, org.jsoup.select.Evaluator$AttributeKeyPair=0, org.jsoup.select.Evaluator$AttributeStarting=0, org.jsoup.select.Evaluator$AttributeWithValue=0, org.jsoup.select.Evaluator$AttributeWithValueContaining=0, org.jsoup.select.Evaluator$AttributeWithValueEnding=0, org.jsoup.select.Evaluator$AttributeWithValueNot=0, org.jsoup.select.Evaluator$AttributeWithValueStarting=0, org.jsoup.select.Evaluator$Class=0, org.jsoup.select.Evaluator$ContainsOwnText=0, org.jsoup.select.Evaluator$ContainsText=0, org.jsoup.select.Evaluator$CssNthEvaluator=0, org.jsoup.select.Evaluator$Id=0, org.jsoup.select.Evaluator$IndexEquals=0, org.jsoup.select.Evaluator$IndexEvaluator=0, org.jsoup.select.Evaluator$IndexGreaterThan=0, org.jsoup.select.Evaluator$IndexLessThan=0, org.jsoup.select.Evaluator$IsNthChild=0, org.jsoup.select.Evaluator$IsNthLastChild=0, org.jsoup.select.Evaluator$IsNthLastOfType=0, org.jsoup.select.Evaluator$IsNthOfType=0, org.jsoup.select.Evaluator$Tag=0, org.jsoup.select.Evaluator=0, org.jsoup.select.StructuralEvaluator$Has=0, org.jsoup.select.StructuralEvaluator$ImmediateParent=0, org.jsoup.select.StructuralEvaluator$ImmediatePreviousSibling=0, org.jsoup.select.StructuralEvaluator$Not=0, org.jsoup.select.StructuralEvaluator$Parent=0, org.jsoup.select.StructuralEvaluator$PreviousSibling=0, org.jsoup.HttpStatusException=0, org.jsoup.Connection$KeyVal=0, org.w3c.dom.Document=0, org.jsoup.nodes.XmlDeclaration=0, org.jsoup.nodes.FormElement=0, java.io.InputStream=0, org.jsoup.Connection$Request=0, org.jsoup.Connection$Response=0, char=0, org.jsoup.helper.W3CDom$W3CBuilder=0, java.util.Collection=0, byte[]=0

target
java.lang.String=1,boolean=0,byte[]=0,char=0,int=0,java.io.InputStream=0,java.lang.Character=0,java.lang.Integer=0,java.lang.Object=0,java.nio.charset.Charset=0,java.util.Collection=0,java.util.List=0,java.util.Map=0,java.util.Set=0,org.jsoup.Connection=0,org.jsoup.Connection$KeyVal=0,org.jsoup.Connection$Method=0,org.jsoup.Connection$Method[]=0,org.jsoup.Connection$Request=0,org.jsoup.Connection$Response=0,org.jsoup.HttpStatusException=0,org.jsoup.UnsupportedMimeTypeException=0,org.jsoup.examples.HtmlToPlainText=0,org.jsoup.examples.ListLinks=0,org.jsoup.helper.DescendableLinkedList=0,org.jsoup.helper.HttpConnection$KeyVal=0,org.jsoup.helper.StringUtil=0,org.jsoup.helper.W3CDom=0,org.jsoup.helper.W3CDom$W3CBuilder=0,org.jsoup.nodes.Attribute=0,org.jsoup.nodes.Attributes=0,org.jsoup.nodes.BooleanAttribute=0,org.jsoup.nodes.Comment=0,org.jsoup.nodes.DataNode=0,org.jsoup.nodes.Document=0,org.jsoup.nodes.Document$OutputSettings=0,org.jsoup.nodes.Document$OutputSettings$Syntax=0,org.jsoup.nodes.Document$OutputSettings$Syntax[]=0,org.jsoup.nodes.Document$QuirksMode=0,org.jsoup.nodes.Document$QuirksMode[]=0,org.jsoup.nodes.DocumentType=0,org.jsoup.nodes.Element=0,org.jsoup.nodes.Entities$CoreCharset=0,org.jsoup.nodes.Entities$CoreCharset[]=0,org.jsoup.nodes.Entities$EscapeMode=0,org.jsoup.nodes.Entities$EscapeMode[]=0,org.jsoup.nodes.FormElement=0,org.jsoup.nodes.Node=0,org.jsoup.nodes.TextNode=0,org.jsoup.nodes.XmlDeclaration=0,org.jsoup.parser.HtmlTreeBuilderState=0,org.jsoup.parser.HtmlTreeBuilderState[]=0,org.jsoup.parser.Parser=0,org.jsoup.parser.Tag=0,org.jsoup.parser.Token$TokenType=0,org.jsoup.parser.Token$TokenType[]=0,org.jsoup.parser.TokenQueue=0,org.jsoup.parser.TokeniserState=0,org.jsoup.parser.TokeniserState[]=0,org.jsoup.parser.TreeBuilder=0,org.jsoup.parser.XmlTreeBuilder=0,org.jsoup.safety.Cleaner=0,org.jsoup.safety.Whitelist=0,org.jsoup.select.Elements=0,org.jsoup.select.Evaluator=0,org.jsoup.select.Evaluator$AllElements=0,org.jsoup.select.Evaluator$Attribute=0,org.jsoup.select.Evaluator$AttributeKeyPair=0,org.jsoup.select.Evaluator$AttributeStarting=0,org.jsoup.select.Evaluator$AttributeWithValue=0,org.jsoup.select.Evaluator$AttributeWithValueContaining=0,org.jsoup.select.Evaluator$AttributeWithValueEnding=0,org.jsoup.select.Evaluator$AttributeWithValueNot=0,org.jsoup.select.Evaluator$AttributeWithValueStarting=0,org.jsoup.select.Evaluator$Class=0,org.jsoup.select.Evaluator$ContainsOwnText=0,org.jsoup.select.Evaluator$ContainsText=0,org.jsoup.select.Evaluator$CssNthEvaluator=0,org.jsoup.select.Evaluator$Id=0,org.jsoup.select.Evaluator$IndexEquals=0,org.jsoup.select.Evaluator$IndexEvaluator=0,org.jsoup.select.Evaluator$IndexGreaterThan=0,org.jsoup.select.Evaluator$IndexLessThan=0,org.jsoup.select.Evaluator$IsEmpty=0,org.jsoup.select.Evaluator$IsFirstChild=0,org.jsoup.select.Evaluator$IsFirstOfType=0,org.jsoup.select.Evaluator$IsLastChild=0,org.jsoup.select.Evaluator$IsLastOfType=0,org.jsoup.select.Evaluator$IsNthChild=0,org.jsoup.select.Evaluator$IsNthLastChild=0,org.jsoup.select.Evaluator$IsNthLastOfType=0,org.jsoup.select.Evaluator$IsNthOfType=0,org.jsoup.select.Evaluator$IsOnlyChild=0,org.jsoup.select.Evaluator$IsOnlyOfType=0,org.jsoup.select.Evaluator$IsRoot=0,org.jsoup.select.Evaluator$Tag=0,org.jsoup.select.StructuralEvaluator$Has=0,org.jsoup.select.StructuralEvaluator$ImmediateParent=0,org.jsoup.select.StructuralEvaluator$ImmediatePreviousSibling=0,org.jsoup.select.StructuralEvaluator$Not=0,org.jsoup.select.StructuralEvaluator$Parent=0,org.jsoup.select.StructuralEvaluator$PreviousSibling=0,org.w3c.dom.Document=0,indicator_place_java.lang.String>=1
