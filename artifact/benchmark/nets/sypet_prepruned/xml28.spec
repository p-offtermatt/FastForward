vars
java.io.File javax.xml.parsers.DocumentBuilderFactory javax.xml.parsers.FactoryConfigurationError javax.xml.parsers.ParserConfigurationException javax.xml.parsers.SAXParserFactory org.w3c.dom.bootstrap.DOMImplementationRegistry org.xml.sax.HandlerBase org.xml.sax.InputSource org.xml.sax.SAXException org.xml.sax.SAXNotRecognizedException org.xml.sax.SAXNotSupportedException org.xml.sax.ext.Attributes2Impl org.xml.sax.ext.DefaultHandler2 org.xml.sax.ext.Locator2Impl org.xml.sax.helpers.AttributeListImpl org.xml.sax.helpers.AttributesImpl org.xml.sax.helpers.DefaultHandler org.xml.sax.helpers.LocatorImpl org.xml.sax.helpers.NamespaceSupport org.xml.sax.helpers.ParserAdapter org.xml.sax.Parser org.xml.sax.helpers.XMLFilterImpl org.xml.sax.helpers.XMLReaderAdapter org.xml.sax.XMLReader boolean javax.xml.parsers.DocumentBuilder javax.xml.validation.Schema java.lang.Exception java.lang.String java.lang.Throwable javax.xml.parsers.SAXParser java.io.InputStream java.io.Reader org.xml.sax.ContentHandler org.xml.sax.DTDHandler org.xml.sax.EntityResolver org.xml.sax.ErrorHandler int java.util.Enumeration indicator_place_java.io.File org.w3c.dom.Document java.lang.Object org.w3c.dom.Attr org.w3c.dom.CDATASection org.w3c.dom.Comment org.w3c.dom.Element org.w3c.dom.EntityReference org.w3c.dom.NodeList org.w3c.dom.ProcessingInstruction org.w3c.dom.Text org.w3c.dom.Node org.w3c.dom.DOMImplementation org.w3c.dom.DOMImplementationList java.io.StringReader org.w3c.dom.TypeInfo org.w3c.dom.DOMConfiguration org.w3c.dom.DocumentFragment org.w3c.dom.DocumentType org.w3c.dom.NamedNodeMap short org.w3c.dom.DOMException org.w3c.dom.events.EventException org.w3c.dom.ls.LSException org.w3c.dom.ranges.RangeException org.w3c.dom.xpath.XPathException org.w3c.dom.DOMStringList

rules
javadotiodotStringReader >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader-1,
boolean' = boolean+1;

javadotiodotStringReader >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader-1,
boolean' = boolean+1;

javadotiodotStringReader >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader-1,
int' = int+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
javadotiodotStringReader' = javadotiodotStringReader+1;

javadotiodotStringReader >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader-1;

javadotiodotStringReader >= 1,int >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader-1,
int' = int-1;

javadotiodotStringReader >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader-1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
javaxdotxmldotvalidationdotSchema' = javaxdotxmldotvalidationdotSchema+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1,javadotiodotFile >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
javadotiodotFile' = javadotiodotFile-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1,
indicator_place_javadotiodotFile' = indicator_place_javadotiodotFile+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1,javadotiodotInputStream >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1,javadotlangdotString >= 1,javadotiodotInputStream >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
javadotlangdotString' = javadotlangdotString-1,
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1,javadotlangdotString >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

orgdotxmldotsaxdotInputSource >= 1,javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1;

javaxdotxmldotparsersdotDocumentBuilder >= 1,orgdotxmldotsaxdotEntityResolver >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver-1;

orgdotxmldotsaxdotErrorHandler >= 1,javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler-1,
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder-1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1,javadotlangdotString >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1,javadotlangdotString >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotObject' = javadotlangdotObject+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder+1;

javax.xml.parsers.DocumentBuilderFactory >= 0 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
javaxdotxmldotvalidationdotSchema' = javaxdotxmldotvalidationdotSchema+1;

javadotlangdotObject >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1,javadotlangdotString >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
javadotlangdotString' = javadotlangdotString-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
javadotlangdotString' = javadotlangdotString-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1,javaxdotxmldotvalidationdotSchema >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1,
javaxdotxmldotvalidationdotSchema' = javaxdotxmldotvalidationdotSchema-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1;

boolean >= 1,javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory-1;

javaxdotxmldotparsersdotFactoryConfigurationError >= 1 ->
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError-1,
javadotlangdotException' = javadotlangdotException+1;

javaxdotxmldotparsersdotFactoryConfigurationError >= 1 ->
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError-1,
javadotlangdotString' = javadotlangdotString+1;

javaxdotxmldotparsersdotFactoryConfigurationError >= 1 ->
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError-1,
javadotlangdotThrowable' = javadotlangdotThrowable+1;

javax.xml.parsers.FactoryConfigurationError >= 0 ->
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError+1;

javadotlangdotException >= 1 ->
javadotlangdotException' = javadotlangdotException-1,
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError+1;

javadotlangdotString >= 1,javadotlangdotException >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotException' = javadotlangdotException-1,
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError+1;

javax.xml.parsers.ParserConfigurationException >= 0 ->
javaxdotxmldotparsersdotParserConfigurationException' = javaxdotxmldotparsersdotParserConfigurationException+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
javaxdotxmldotparsersdotParserConfigurationException' = javaxdotxmldotparsersdotParserConfigurationException+1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotSAXParser >= 1,javadotlangdotString >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotObject' = javadotlangdotObject+1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javaxdotxmldotvalidationdotSchema' = javaxdotxmldotvalidationdotSchema+1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser+1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader+1;

javaxdotxmldotparsersdotSAXParser >= 1,orgdotxmldotsaxdotHandlerBase >= 1,javadotiodotFile >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotiodotFile' = javadotiodotFile-1,
indicator_place_javadotiodotFile' = indicator_place_javadotiodotFile+1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javaxdotxmldotparsersdotSAXParser >= 1,javadotiodotFile >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javadotiodotFile' = javadotiodotFile-1,
indicator_place_javadotiodotFile' = indicator_place_javadotiodotFile+1;

javaxdotxmldotparsersdotSAXParser >= 1,orgdotxmldotsaxdotHandlerBase >= 1,javadotiodotInputStream >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotiodotInputStream' = javadotiodotInputStream-1;

javaxdotxmldotparsersdotSAXParser >= 1,orgdotxmldotsaxdotHandlerBase >= 1,javadotlangdotString >= 1,javadotiodotInputStream >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotlangdotString' = javadotlangdotString-1,
javadotiodotInputStream' = javadotiodotInputStream-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javaxdotxmldotparsersdotSAXParser >= 1,javadotiodotInputStream >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javadotiodotInputStream' = javadotiodotInputStream-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javaxdotxmldotparsersdotSAXParser >= 1,javadotlangdotString >= 1,javadotiodotInputStream >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javadotlangdotString' = javadotlangdotString-1,
javadotiodotInputStream' = javadotiodotInputStream-1;

javaxdotxmldotparsersdotSAXParser >= 1,javadotlangdotString >= 1,orgdotxmldotsaxdotHandlerBase >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javaxdotxmldotparsersdotSAXParser >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotInputSource >= 1,javaxdotxmldotparsersdotSAXParser >= 1,orgdotxmldotsaxdotHandlerBase >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,orgdotxmldotsaxdotInputSource >= 1,javaxdotxmldotparsersdotSAXParser >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1;

javaxdotxmldotparsersdotSAXParser >= 1,javadotlangdotObject >= 1,javadotlangdotString >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser-1,
javadotlangdotObject' = javadotlangdotObject-1,
javadotlangdotString' = javadotlangdotString-1;

javaxdotxmldotparsersdotSAXParserFactory >= 1,javadotlangdotString >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
boolean' = boolean+1;

javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser+1;

javax.xml.parsers.SAXParserFactory >= 0 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory+1;

javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
javaxdotxmldotvalidationdotSchema' = javaxdotxmldotvalidationdotSchema+1;

boolean >= 1,javaxdotxmldotparsersdotSAXParserFactory >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
javadotlangdotString' = javadotlangdotString-1;

boolean >= 1,javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1;

javaxdotxmldotparsersdotSAXParserFactory >= 1,javaxdotxmldotvalidationdotSchema >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1,
javaxdotxmldotvalidationdotSchema' = javaxdotxmldotvalidationdotSchema-1;

boolean >= 1,javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1;

boolean >= 1,javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
boolean' = boolean-1,
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory-1;

orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1,
boolean' = boolean+1;

orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1,
boolean' = boolean+1;

orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1,
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement+1;

orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1,
orgdotw3cdotdomdotTypeInfo' = orgdotw3cdotdomdotTypeInfo+1;

javadotlangdotString >= 1,orgdotw3cdotdomdotAttr >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1;

orgdotw3cdotdomdotDOMConfiguration >= 1,javadotlangdotObject >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDOMConfiguration' = orgdotw3cdotdomdotDOMConfiguration-1,
javadotlangdotObject' = javadotlangdotObject-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotw3cdotdomdotDOMConfiguration >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDOMConfiguration' = orgdotw3cdotdomdotDOMConfiguration-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotw3cdotdomdotDOMConfiguration >= 1 ->
orgdotw3cdotdomdotDOMConfiguration' = orgdotw3cdotdomdotDOMConfiguration-1,
orgdotw3cdotdomdotDOMStringList' = orgdotw3cdotdomdotDOMStringList+1;

orgdotw3cdotdomdotDOMConfiguration >= 1,javadotlangdotObject >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDOMConfiguration' = orgdotw3cdotdomdotDOMConfiguration-1,
javadotlangdotObject' = javadotlangdotObject-1,
javadotlangdotString' = javadotlangdotString-1;

short >= 1,javadotlangdotString >= 1 ->
short' = short-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotDOMException' = orgdotw3cdotdomdotDOMException+1;

javadotlangdotString >= 2,orgdotw3cdotdomdotDOMImplementation >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation-1,
boolean' = boolean+1;

javadotlangdotString >= 2,orgdotw3cdotdomdotDOMImplementation >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation-1,
javadotlangdotObject' = javadotlangdotObject+1;

javadotlangdotString >= 2,orgdotw3cdotdomdotDOMImplementation >= 1,orgdotw3cdotdomdotDocumentType >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation-1,
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

javadotlangdotString >= 3,orgdotw3cdotdomdotDOMImplementation >= 1 ->
javadotlangdotString' = javadotlangdotString-3,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation-1,
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType+1;

orgdotw3cdotdomdotDOMImplementationList >= 1 ->
orgdotw3cdotdomdotDOMImplementationList' = orgdotw3cdotdomdotDOMImplementationList-1,
int' = int+1;

orgdotw3cdotdomdotDOMImplementationList >= 1,int >= 1 ->
orgdotw3cdotdomdotDOMImplementationList' = orgdotw3cdotdomdotDOMImplementationList-1,
int' = int-1,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation+1;

orgdotw3cdotdomdotDOMStringList >= 1 ->
orgdotw3cdotdomdotDOMStringList' = orgdotw3cdotdomdotDOMStringList-1,
int' = int+1;

orgdotw3cdotdomdotDOMStringList >= 1,int >= 1 ->
orgdotw3cdotdomdotDOMStringList' = orgdotw3cdotdomdotDOMStringList-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
boolean' = boolean+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
boolean' = boolean+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotCDATASection' = orgdotw3cdotdomdotCDATASection+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotComment' = orgdotw3cdotdomdotComment+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotDOMConfiguration' = orgdotw3cdotdomdotDOMConfiguration+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotDocumentFragment' = orgdotw3cdotdomdotDocumentFragment+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotEntityReference' = orgdotw3cdotdomdotEntityReference+1;

orgdotw3cdotdomdotDocument >= 1,orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

boolean >= 1,orgdotw3cdotdomdotDocument >= 1,orgdotw3cdotdomdotNode >= 1 ->
boolean' = boolean-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

orgdotw3cdotdomdotDocument >= 1,orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0,
javadotlangdotString' = javadotlangdotString-2;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotProcessingInstruction' = orgdotw3cdotdomdotProcessingInstruction+1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotText' = orgdotw3cdotdomdotText+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1;

boolean >= 1,orgdotw3cdotdomdotDocument >= 1 ->
boolean' = boolean-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1;

boolean >= 1,orgdotw3cdotdomdotDocument >= 1 ->
boolean' = boolean-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1;

orgdotw3cdotdomdotDocument >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotDocumentType >= 1 ->
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocumentType >= 1 ->
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocumentType >= 1 ->
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocumentType >= 1 ->
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotDocumentType >= 1 ->
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType-1,
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap+1;

orgdotw3cdotdomdotDocumentType >= 1 ->
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType-1,
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap+1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
boolean' = boolean+1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotElement >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+1;

orgdotw3cdotdomdotElement >= 1,orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+0;

orgdotw3cdotdomdotElement >= 1,orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+0;

orgdotw3cdotdomdotElement >= 1,orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+0;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList+1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList+1;

orgdotw3cdotdomdotElement >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
orgdotw3cdotdomdotTypeInfo' = orgdotw3cdotdomdotTypeInfo+1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotw3cdotdomdotElement >= 1,javadotlangdotString >= 3 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotw3cdotdomdotElement >= 1,boolean >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotElement >= 1,boolean >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotw3cdotdomdotElement >= 1,boolean >= 1,orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
boolean' = boolean-1,
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr-1;

orgdotw3cdotdomdotNamedNodeMap >= 1 ->
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1,
int' = int+1;

orgdotw3cdotdomdotNamedNodeMap >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

orgdotw3cdotdomdotNamedNodeMap >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

orgdotw3cdotdomdotNamedNodeMap >= 1,int >= 1 ->
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1,
int' = int-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

orgdotw3cdotdomdotNamedNodeMap >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

orgdotw3cdotdomdotNamedNodeMap >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

orgdotw3cdotdomdotNode >= 1,orgdotw3cdotdomdotNamedNodeMap >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0,
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1;

orgdotw3cdotdomdotNode >= 1,orgdotw3cdotdomdotNamedNodeMap >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0,
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap-1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
boolean' = boolean+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
boolean' = boolean+1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotw3cdotdomdotNode >= 2 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-2,
boolean' = boolean+1;

orgdotw3cdotdomdotNode >= 2 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-2,
boolean' = boolean+1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString-2,
boolean' = boolean+1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 2 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString-2,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap+1;

orgdotw3cdotdomdotNode >= 2 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1;

boolean >= 1,orgdotw3cdotdomdotNode >= 1 ->
boolean' = boolean-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+0;

orgdotw3cdotdomdotNode >= 3 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-2;

orgdotw3cdotdomdotNode >= 2 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1;

orgdotw3cdotdomdotNode >= 3 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-2;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList+1;

orgdotw3cdotdomdotNode >= 2 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-2,
short' = short+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
short' = short+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotNode >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotNodeList >= 1 ->
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList-1,
int' = int+1;

orgdotw3cdotdomdotNodeList >= 1,int >= 1 ->
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList-1,
int' = int-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

orgdotw3cdotdomdotProcessingInstruction >= 1 ->
orgdotw3cdotdomdotProcessingInstruction' = orgdotw3cdotdomdotProcessingInstruction-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotProcessingInstruction >= 1 ->
orgdotw3cdotdomdotProcessingInstruction' = orgdotw3cdotdomdotProcessingInstruction-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotProcessingInstruction >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotProcessingInstruction' = orgdotw3cdotdomdotProcessingInstruction-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotText >= 1 ->
orgdotw3cdotdomdotText' = orgdotw3cdotdomdotText-1,
boolean' = boolean+1;

orgdotw3cdotdomdotText >= 1 ->
orgdotw3cdotdomdotText' = orgdotw3cdotdomdotText-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotText >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotText' = orgdotw3cdotdomdotText+0,
javadotlangdotString' = javadotlangdotString-1;

orgdotw3cdotdomdotText >= 1,int >= 1 ->
orgdotw3cdotdomdotText' = orgdotw3cdotdomdotText+0,
int' = int-1;

orgdotw3cdotdomdotTypeInfo >= 1,javadotlangdotString >= 2,int >= 1 ->
orgdotw3cdotdomdotTypeInfo' = orgdotw3cdotdomdotTypeInfo-1,
javadotlangdotString' = javadotlangdotString-2,
int' = int-1,
boolean' = boolean+1;

orgdotw3cdotdomdotTypeInfo >= 1 ->
orgdotw3cdotdomdotTypeInfo' = orgdotw3cdotdomdotTypeInfo-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotTypeInfo >= 1 ->
orgdotw3cdotdomdotTypeInfo' = orgdotw3cdotdomdotTypeInfo-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry' = orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation+1;

orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry >= 1,javadotlangdotString >= 1 ->
orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry' = orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotDOMImplementationList' = orgdotw3cdotdomdotDOMImplementationList+1;

org.w3c.dom.bootstrap.DOMImplementationRegistry >= 0 ->
orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry' = orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry+1;

short >= 1,javadotlangdotString >= 1 ->
short' = short-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdoteventsdotEventException' = orgdotw3cdotdomdoteventsdotEventException+1;

short >= 1,javadotlangdotString >= 1 ->
short' = short-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotlsdotLSException' = orgdotw3cdotdomdotlsdotLSException+1;

short >= 1,javadotlangdotString >= 1 ->
short' = short-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotrangesdotRangeException' = orgdotw3cdotdomdotrangesdotRangeException+1;

short >= 1,javadotlangdotString >= 1 ->
short' = short-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotw3cdotdomdotxpathdotXPathException' = orgdotw3cdotdomdotxpathdotXPathException+1;

orgdotxmldotsaxdotContentHandler >= 1 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1;

orgdotxmldotsaxdotContentHandler >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdotContentHandler >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotContentHandler >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdotContentHandler >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotContentHandler >= 1 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1;

orgdotxmldotsaxdotContentHandler >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdotDTDHandler >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdotDTDHandler >= 1,javadotlangdotString >= 4 ->
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler-1,
javadotlangdotString' = javadotlangdotString-4;

javadotlangdotString >= 2,orgdotxmldotsaxdotEntityResolver >= 1 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

orgdotxmldotsaxdotHandlerBase >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

org.xml.sax.HandlerBase >= 0 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase+1;

orgdotxmldotsaxdotHandlerBase >= 1 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1;

orgdotxmldotsaxdotHandlerBase >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotHandlerBase >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdotHandlerBase >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdotHandlerBase >= 1 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1;

orgdotxmldotsaxdotHandlerBase >= 1,javadotlangdotString >= 4 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase-1,
javadotlangdotString' = javadotlangdotString-4;

orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotiodotInputStream' = javadotiodotInputStream+1;

orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotiodotReader' = javadotiodotReader+1;

orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotlangdotString' = javadotlangdotString+1;

org.xml.sax.InputSource >= 0 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

javadotiodotInputStream >= 1 ->
javadotiodotInputStream' = javadotiodotInputStream-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

javadotiodotReader >= 1 ->
javadotiodotReader' = javadotiodotReader-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

orgdotxmldotsaxdotInputSource >= 1,javadotiodotInputStream >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotiodotInputStream' = javadotiodotInputStream-1;

orgdotxmldotsaxdotInputSource >= 1,javadotiodotReader >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotiodotReader' = javadotiodotReader-1;

orgdotxmldotsaxdotInputSource >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotInputSource >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotInputSource >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotParser >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotParser >= 1,orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1;

orgdotxmldotsaxdotParser >= 1,orgdotxmldotsaxdotDTDHandler >= 1 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser-1,
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler-1;

orgdotxmldotsaxdotParser >= 1,orgdotxmldotsaxdotEntityResolver >= 1 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver-1;

orgdotxmldotsaxdotParser >= 1,orgdotxmldotsaxdotErrorHandler >= 1 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser-1,
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler-1;

orgdotxmldotsaxdotSAXException >= 1 ->
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException-1,
javadotlangdotException' = javadotlangdotException+1;

orgdotxmldotsaxdotSAXException >= 1 ->
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdotSAXException >= 1 ->
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException-1,
javadotlangdotThrowable' = javadotlangdotThrowable+1;

org.xml.sax.SAXException >= 0 ->
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException+1;

javadotlangdotException >= 1 ->
javadotlangdotException' = javadotlangdotException-1,
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException+1;

javadotlangdotString >= 1,javadotlangdotException >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotException' = javadotlangdotException-1,
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException+1;

org.xml.sax.SAXNotRecognizedException >= 0 ->
orgdotxmldotsaxdotSAXNotRecognizedException' = orgdotxmldotsaxdotSAXNotRecognizedException+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotxmldotsaxdotSAXNotRecognizedException' = orgdotxmldotsaxdotSAXNotRecognizedException+1;

org.xml.sax.SAXNotSupportedException >= 0 ->
orgdotxmldotsaxdotSAXNotSupportedException' = orgdotxmldotsaxdotSAXNotSupportedException+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotxmldotsaxdotSAXNotSupportedException' = orgdotxmldotsaxdotSAXNotSupportedException+1;

orgdotxmldotsaxdotXMLReader >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotxmldotsaxdotXMLReader >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotxmldotsaxdotXMLReader >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler+1;

orgdotxmldotsaxdotXMLReader >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler+1;

orgdotxmldotsaxdotXMLReader >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver+1;

orgdotxmldotsaxdotXMLReader >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler+1;

orgdotxmldotsaxdotXMLReader >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotXMLReader >= 1,orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1;

orgdotxmldotsaxdotXMLReader >= 1,orgdotxmldotsaxdotContentHandler >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1;

orgdotxmldotsaxdotXMLReader >= 1,orgdotxmldotsaxdotDTDHandler >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler-1;

orgdotxmldotsaxdotXMLReader >= 1,orgdotxmldotsaxdotEntityResolver >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver-1;

orgdotxmldotsaxdotXMLReader >= 1,orgdotxmldotsaxdotErrorHandler >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler-1;

orgdotxmldotsaxdotXMLReader >= 1,boolean >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotXMLReader >= 1,javadotlangdotObject >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
javadotlangdotObject' = javadotlangdotObject-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,int >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
int' = int-1,
boolean' = boolean+1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
javadotlangdotString' = javadotlangdotString-2,
boolean' = boolean+1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,int >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
int' = int-1,
boolean' = boolean+1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
javadotlangdotString' = javadotlangdotString-2,
boolean' = boolean+1;

org.xml.sax.ext.Attributes2Impl >= 0 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl+1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,int >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
int' = int-1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,boolean >= 1,int >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
boolean' = boolean-1,
int' = int-1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1,boolean >= 1,int >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl-1,
boolean' = boolean-1,
int' = int-1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 4 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-4,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

org.xml.sax.ext.DefaultHandler2 >= 0 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2+1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotextdotLocator2Impl >= 1 ->
orgdotxmldotsaxdotextdotLocator2Impl' = orgdotxmldotsaxdotextdotLocator2Impl-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdotextdotLocator2Impl >= 1 ->
orgdotxmldotsaxdotextdotLocator2Impl' = orgdotxmldotsaxdotextdotLocator2Impl-1,
javadotlangdotString' = javadotlangdotString+1;

org.xml.sax.ext.Locator2Impl >= 0 ->
orgdotxmldotsaxdotextdotLocator2Impl' = orgdotxmldotsaxdotextdotLocator2Impl+1;

orgdotxmldotsaxdotextdotLocator2Impl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotextdotLocator2Impl' = orgdotxmldotsaxdotextdotLocator2Impl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotextdotLocator2Impl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdotextdotLocator2Impl' = orgdotxmldotsaxdotextdotLocator2Impl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
int' = int+1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
javadotlangdotString' = javadotlangdotString+0;

org.xml.sax.helpers.AttributeListImpl >= 0 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl+1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1,
int' = int+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-2,
int' = int+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
int' = int+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1;

org.xml.sax.helpers.AttributesImpl >= 0 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1,javadotlangdotString >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl-1,
javadotlangdotString' = javadotlangdotString-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

org.xml.sax.helpers.DefaultHandler >= 0 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler+1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1,javadotlangdotString >= 4 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler-1,
javadotlangdotString' = javadotlangdotString-4;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
int' = int+1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
int' = int+1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
javadotlangdotString' = javadotlangdotString+1;

org.xml.sax.helpers.LocatorImpl >= 0 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl+1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1,int >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
int' = int-1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
javadotlangdotString' = javadotlangdotString-2,
boolean' = boolean+1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
boolean' = boolean+1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
javadotlangdotString' = javadotlangdotString+0;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
javadotutildotEnumeration' = javadotutildotEnumeration+1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
javadotutildotEnumeration' = javadotutildotEnumeration+1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
javadotlangdotString' = javadotlangdotString-1,
javadotutildotEnumeration' = javadotutildotEnumeration+1;

org.xml.sax.helpers.NamespaceSupport >= 0 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport+1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1,boolean >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport-1,
boolean' = boolean-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler+1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler+1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver+1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler+1;

org.xml.sax.helpers.ParserAdapter >= 0 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter+1;

orgdotxmldotsaxdotParser >= 1 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser-1,
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter+1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,orgdotxmldotsaxdotContentHandler >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,orgdotxmldotsaxdotDTDHandler >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,orgdotxmldotsaxdotEntityResolver >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,orgdotxmldotsaxdotErrorHandler >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,boolean >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
boolean' = boolean-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1,javadotlangdotObject >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1,
javadotlangdotObject' = javadotlangdotObject-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter-1;

org.xml.sax.Parser >= 0 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-1,
boolean' = boolean+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-2,
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader+1;

org.xml.sax.helpers.XMLFilterImpl >= 0 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl+1;

orgdotxmldotsaxdotXMLReader >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotInputSource >= 1,orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,orgdotxmldotsaxdotContentHandler >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler-1;

orgdotxmldotsaxdotDTDHandler >= 1,orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler-1,
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,orgdotxmldotsaxdotEntityResolver >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver-1;

orgdotxmldotsaxdotErrorHandler >= 1,orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler-1,
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1;

boolean >= 1,orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotXMLReader >= 1,orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotObject >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotObject' = javadotlangdotObject-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1,javadotlangdotString >= 4 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl-1,
javadotlangdotString' = javadotlangdotString-4;

org.xml.sax.helpers.XMLReaderAdapter >= 0 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter+1;

orgdotxmldotsaxdotXMLReader >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader-1,
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter+1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,javadotlangdotString >= 3 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
javadotlangdotString' = javadotlangdotString-3;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdotInputSource >= 1,orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource-1,
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
javadotlangdotString' = javadotlangdotString-2;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,orgdotxmldotsaxdotDTDHandler >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,orgdotxmldotsaxdotEntityResolver >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,orgdotxmldotsaxdotErrorHandler >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,javadotlangdotString >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1,javadotlangdotString >= 2 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter-1,
javadotlangdotString' = javadotlangdotString-2;

org.xml.sax.XMLReader >= 0 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader+1;

orgdotw3cdotdomdotElement >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement-1,
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

javadotiodotStringReader >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader-1,
javadotiodotReader' = javadotiodotReader+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument-1,
javadotlangdotObject' = javadotlangdotObject+1;

boolean >= 1 ->
boolean' = boolean+1;

int >= 1 ->
int' = int+1;

javadotiodotFile >= 1 ->
javadotiodotFile' = javadotiodotFile+1,
indicator_place_javadotiodotFile' = indicator_place_javadotiodotFile+1;

javadotiodotInputStream >= 1 ->
javadotiodotInputStream' = javadotiodotInputStream+1;

javadotiodotReader >= 1 ->
javadotiodotReader' = javadotiodotReader+1;

javadotiodotStringReader >= 1 ->
javadotiodotStringReader' = javadotiodotStringReader+1;

javadotlangdotException >= 1 ->
javadotlangdotException' = javadotlangdotException+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString+1;

javadotlangdotThrowable >= 1 ->
javadotlangdotThrowable' = javadotlangdotThrowable+1;

javadotutildotEnumeration >= 1 ->
javadotutildotEnumeration' = javadotutildotEnumeration+1;

javaxdotxmldotparsersdotDocumentBuilder >= 1 ->
javaxdotxmldotparsersdotDocumentBuilder' = javaxdotxmldotparsersdotDocumentBuilder+1;

javaxdotxmldotparsersdotDocumentBuilderFactory >= 1 ->
javaxdotxmldotparsersdotDocumentBuilderFactory' = javaxdotxmldotparsersdotDocumentBuilderFactory+1;

javaxdotxmldotparsersdotFactoryConfigurationError >= 1 ->
javaxdotxmldotparsersdotFactoryConfigurationError' = javaxdotxmldotparsersdotFactoryConfigurationError+1;

javaxdotxmldotparsersdotParserConfigurationException >= 1 ->
javaxdotxmldotparsersdotParserConfigurationException' = javaxdotxmldotparsersdotParserConfigurationException+1;

javaxdotxmldotparsersdotSAXParser >= 1 ->
javaxdotxmldotparsersdotSAXParser' = javaxdotxmldotparsersdotSAXParser+1;

javaxdotxmldotparsersdotSAXParserFactory >= 1 ->
javaxdotxmldotparsersdotSAXParserFactory' = javaxdotxmldotparsersdotSAXParserFactory+1;

javaxdotxmldotvalidationdotSchema >= 1 ->
javaxdotxmldotvalidationdotSchema' = javaxdotxmldotvalidationdotSchema+1;

orgdotw3cdotdomdotAttr >= 1 ->
orgdotw3cdotdomdotAttr' = orgdotw3cdotdomdotAttr+1;

orgdotw3cdotdomdotCDATASection >= 1 ->
orgdotw3cdotdomdotCDATASection' = orgdotw3cdotdomdotCDATASection+1;

orgdotw3cdotdomdotComment >= 1 ->
orgdotw3cdotdomdotComment' = orgdotw3cdotdomdotComment+1;

orgdotw3cdotdomdotDOMConfiguration >= 1 ->
orgdotw3cdotdomdotDOMConfiguration' = orgdotw3cdotdomdotDOMConfiguration+1;

orgdotw3cdotdomdotDOMException >= 1 ->
orgdotw3cdotdomdotDOMException' = orgdotw3cdotdomdotDOMException+1;

orgdotw3cdotdomdotDOMImplementation >= 1 ->
orgdotw3cdotdomdotDOMImplementation' = orgdotw3cdotdomdotDOMImplementation+1;

orgdotw3cdotdomdotDOMImplementationList >= 1 ->
orgdotw3cdotdomdotDOMImplementationList' = orgdotw3cdotdomdotDOMImplementationList+1;

orgdotw3cdotdomdotDOMStringList >= 1 ->
orgdotw3cdotdomdotDOMStringList' = orgdotw3cdotdomdotDOMStringList+1;

orgdotw3cdotdomdotDocument >= 1 ->
orgdotw3cdotdomdotDocument' = orgdotw3cdotdomdotDocument+1;

orgdotw3cdotdomdotDocumentFragment >= 1 ->
orgdotw3cdotdomdotDocumentFragment' = orgdotw3cdotdomdotDocumentFragment+1;

orgdotw3cdotdomdotDocumentType >= 1 ->
orgdotw3cdotdomdotDocumentType' = orgdotw3cdotdomdotDocumentType+1;

orgdotw3cdotdomdotElement >= 1 ->
orgdotw3cdotdomdotElement' = orgdotw3cdotdomdotElement+1;

orgdotw3cdotdomdotEntityReference >= 1 ->
orgdotw3cdotdomdotEntityReference' = orgdotw3cdotdomdotEntityReference+1;

orgdotw3cdotdomdotNamedNodeMap >= 1 ->
orgdotw3cdotdomdotNamedNodeMap' = orgdotw3cdotdomdotNamedNodeMap+1;

orgdotw3cdotdomdotNode >= 1 ->
orgdotw3cdotdomdotNode' = orgdotw3cdotdomdotNode+1;

orgdotw3cdotdomdotNodeList >= 1 ->
orgdotw3cdotdomdotNodeList' = orgdotw3cdotdomdotNodeList+1;

orgdotw3cdotdomdotProcessingInstruction >= 1 ->
orgdotw3cdotdomdotProcessingInstruction' = orgdotw3cdotdomdotProcessingInstruction+1;

orgdotw3cdotdomdotText >= 1 ->
orgdotw3cdotdomdotText' = orgdotw3cdotdomdotText+1;

orgdotw3cdotdomdotTypeInfo >= 1 ->
orgdotw3cdotdomdotTypeInfo' = orgdotw3cdotdomdotTypeInfo+1;

orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry >= 1 ->
orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry' = orgdotw3cdotdomdotbootstrapdotDOMImplementationRegistry+1;

orgdotw3cdotdomdoteventsdotEventException >= 1 ->
orgdotw3cdotdomdoteventsdotEventException' = orgdotw3cdotdomdoteventsdotEventException+1;

orgdotw3cdotdomdotlsdotLSException >= 1 ->
orgdotw3cdotdomdotlsdotLSException' = orgdotw3cdotdomdotlsdotLSException+1;

orgdotw3cdotdomdotrangesdotRangeException >= 1 ->
orgdotw3cdotdomdotrangesdotRangeException' = orgdotw3cdotdomdotrangesdotRangeException+1;

orgdotw3cdotdomdotxpathdotXPathException >= 1 ->
orgdotw3cdotdomdotxpathdotXPathException' = orgdotw3cdotdomdotxpathdotXPathException+1;

orgdotxmldotsaxdotContentHandler >= 1 ->
orgdotxmldotsaxdotContentHandler' = orgdotxmldotsaxdotContentHandler+1;

orgdotxmldotsaxdotDTDHandler >= 1 ->
orgdotxmldotsaxdotDTDHandler' = orgdotxmldotsaxdotDTDHandler+1;

orgdotxmldotsaxdotEntityResolver >= 1 ->
orgdotxmldotsaxdotEntityResolver' = orgdotxmldotsaxdotEntityResolver+1;

orgdotxmldotsaxdotErrorHandler >= 1 ->
orgdotxmldotsaxdotErrorHandler' = orgdotxmldotsaxdotErrorHandler+1;

orgdotxmldotsaxdotHandlerBase >= 1 ->
orgdotxmldotsaxdotHandlerBase' = orgdotxmldotsaxdotHandlerBase+1;

orgdotxmldotsaxdotInputSource >= 1 ->
orgdotxmldotsaxdotInputSource' = orgdotxmldotsaxdotInputSource+1;

orgdotxmldotsaxdotParser >= 1 ->
orgdotxmldotsaxdotParser' = orgdotxmldotsaxdotParser+1;

orgdotxmldotsaxdotSAXException >= 1 ->
orgdotxmldotsaxdotSAXException' = orgdotxmldotsaxdotSAXException+1;

orgdotxmldotsaxdotSAXNotRecognizedException >= 1 ->
orgdotxmldotsaxdotSAXNotRecognizedException' = orgdotxmldotsaxdotSAXNotRecognizedException+1;

orgdotxmldotsaxdotSAXNotSupportedException >= 1 ->
orgdotxmldotsaxdotSAXNotSupportedException' = orgdotxmldotsaxdotSAXNotSupportedException+1;

orgdotxmldotsaxdotXMLReader >= 1 ->
orgdotxmldotsaxdotXMLReader' = orgdotxmldotsaxdotXMLReader+1;

orgdotxmldotsaxdotextdotAttributes2Impl >= 1 ->
orgdotxmldotsaxdotextdotAttributes2Impl' = orgdotxmldotsaxdotextdotAttributes2Impl+1;

orgdotxmldotsaxdotextdotDefaultHandler2 >= 1 ->
orgdotxmldotsaxdotextdotDefaultHandler2' = orgdotxmldotsaxdotextdotDefaultHandler2+1;

orgdotxmldotsaxdotextdotLocator2Impl >= 1 ->
orgdotxmldotsaxdotextdotLocator2Impl' = orgdotxmldotsaxdotextdotLocator2Impl+1;

orgdotxmldotsaxdothelpersdotAttributeListImpl >= 1 ->
orgdotxmldotsaxdothelpersdotAttributeListImpl' = orgdotxmldotsaxdothelpersdotAttributeListImpl+1;

orgdotxmldotsaxdothelpersdotAttributesImpl >= 1 ->
orgdotxmldotsaxdothelpersdotAttributesImpl' = orgdotxmldotsaxdothelpersdotAttributesImpl+1;

orgdotxmldotsaxdothelpersdotDefaultHandler >= 1 ->
orgdotxmldotsaxdothelpersdotDefaultHandler' = orgdotxmldotsaxdothelpersdotDefaultHandler+1;

orgdotxmldotsaxdothelpersdotLocatorImpl >= 1 ->
orgdotxmldotsaxdothelpersdotLocatorImpl' = orgdotxmldotsaxdothelpersdotLocatorImpl+1;

orgdotxmldotsaxdothelpersdotNamespaceSupport >= 1 ->
orgdotxmldotsaxdothelpersdotNamespaceSupport' = orgdotxmldotsaxdothelpersdotNamespaceSupport+1;

orgdotxmldotsaxdothelpersdotParserAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotParserAdapter' = orgdotxmldotsaxdothelpersdotParserAdapter+1;

orgdotxmldotsaxdothelpersdotXMLFilterImpl >= 1 ->
orgdotxmldotsaxdothelpersdotXMLFilterImpl' = orgdotxmldotsaxdothelpersdotXMLFilterImpl+1;

orgdotxmldotsaxdothelpersdotXMLReaderAdapter >= 1 ->
orgdotxmldotsaxdothelpersdotXMLReaderAdapter' = orgdotxmldotsaxdothelpersdotXMLReaderAdapter+1;

short >= 1 ->
short' = short+1;



init
java.io.File=1, javax.xml.parsers.DocumentBuilderFactory=0, javax.xml.parsers.FactoryConfigurationError=0, javax.xml.parsers.ParserConfigurationException=0, javax.xml.parsers.SAXParserFactory=0, org.w3c.dom.bootstrap.DOMImplementationRegistry=0, org.xml.sax.HandlerBase=0, org.xml.sax.InputSource=0, org.xml.sax.SAXException=0, org.xml.sax.SAXNotRecognizedException=0, org.xml.sax.SAXNotSupportedException=0, org.xml.sax.ext.Attributes2Impl=0, org.xml.sax.ext.DefaultHandler2=0, org.xml.sax.ext.Locator2Impl=0, org.xml.sax.helpers.AttributeListImpl=0, org.xml.sax.helpers.AttributesImpl=0, org.xml.sax.helpers.DefaultHandler=0, org.xml.sax.helpers.LocatorImpl=0, org.xml.sax.helpers.NamespaceSupport=0, org.xml.sax.helpers.ParserAdapter=0, org.xml.sax.Parser=0, org.xml.sax.helpers.XMLFilterImpl=0, org.xml.sax.helpers.XMLReaderAdapter=0, org.xml.sax.XMLReader=0, boolean=0, javax.xml.parsers.DocumentBuilder=0, javax.xml.validation.Schema=0, java.lang.Exception=0, java.lang.String=0, java.lang.Throwable=0, javax.xml.parsers.SAXParser=0, java.io.InputStream=0, java.io.Reader=0, org.xml.sax.ContentHandler=0, org.xml.sax.DTDHandler=0, org.xml.sax.EntityResolver=0, org.xml.sax.ErrorHandler=0, int=0, java.util.Enumeration=0, indicator_place_java.io.File=0, org.w3c.dom.Document=0, java.lang.Object=0, org.w3c.dom.Attr=0, org.w3c.dom.CDATASection=0, org.w3c.dom.Comment=0, org.w3c.dom.Element=0, org.w3c.dom.EntityReference=0, org.w3c.dom.NodeList=0, org.w3c.dom.ProcessingInstruction=0, org.w3c.dom.Text=0, org.w3c.dom.Node=0, org.w3c.dom.DOMImplementation=0, org.w3c.dom.DOMImplementationList=0, java.io.StringReader=0, org.w3c.dom.TypeInfo=0, org.w3c.dom.DOMConfiguration=0, org.w3c.dom.DocumentFragment=0, org.w3c.dom.DocumentType=0, org.w3c.dom.NamedNodeMap=0, short=0, org.w3c.dom.DOMException=0, org.w3c.dom.events.EventException=0, org.w3c.dom.ls.LSException=0, org.w3c.dom.ranges.RangeException=0, org.w3c.dom.xpath.XPathException=0, org.w3c.dom.DOMStringList=0

target
org.w3c.dom.Document=1,boolean=0,int=0,java.io.File=0,java.io.InputStream=0,java.io.Reader=0,java.io.StringReader=0,java.lang.Exception=0,java.lang.Object=0,java.lang.String=0,java.lang.Throwable=0,java.util.Enumeration=0,javax.xml.parsers.DocumentBuilder=0,javax.xml.parsers.DocumentBuilderFactory=0,javax.xml.parsers.FactoryConfigurationError=0,javax.xml.parsers.ParserConfigurationException=0,javax.xml.parsers.SAXParser=0,javax.xml.parsers.SAXParserFactory=0,javax.xml.validation.Schema=0,org.w3c.dom.Attr=0,org.w3c.dom.CDATASection=0,org.w3c.dom.Comment=0,org.w3c.dom.DOMConfiguration=0,org.w3c.dom.DOMException=0,org.w3c.dom.DOMImplementation=0,org.w3c.dom.DOMImplementationList=0,org.w3c.dom.DOMStringList=0,org.w3c.dom.DocumentFragment=0,org.w3c.dom.DocumentType=0,org.w3c.dom.Element=0,org.w3c.dom.EntityReference=0,org.w3c.dom.NamedNodeMap=0,org.w3c.dom.Node=0,org.w3c.dom.NodeList=0,org.w3c.dom.ProcessingInstruction=0,org.w3c.dom.Text=0,org.w3c.dom.TypeInfo=0,org.w3c.dom.bootstrap.DOMImplementationRegistry=0,org.w3c.dom.events.EventException=0,org.w3c.dom.ls.LSException=0,org.w3c.dom.ranges.RangeException=0,org.w3c.dom.xpath.XPathException=0,org.xml.sax.ContentHandler=0,org.xml.sax.DTDHandler=0,org.xml.sax.EntityResolver=0,org.xml.sax.ErrorHandler=0,org.xml.sax.HandlerBase=0,org.xml.sax.InputSource=0,org.xml.sax.Parser=0,org.xml.sax.SAXException=0,org.xml.sax.SAXNotRecognizedException=0,org.xml.sax.SAXNotSupportedException=0,org.xml.sax.XMLReader=0,org.xml.sax.ext.Attributes2Impl=0,org.xml.sax.ext.DefaultHandler2=0,org.xml.sax.ext.Locator2Impl=0,org.xml.sax.helpers.AttributeListImpl=0,org.xml.sax.helpers.AttributesImpl=0,org.xml.sax.helpers.DefaultHandler=0,org.xml.sax.helpers.LocatorImpl=0,org.xml.sax.helpers.NamespaceSupport=0,org.xml.sax.helpers.ParserAdapter=0,org.xml.sax.helpers.XMLFilterImpl=0,org.xml.sax.helpers.XMLReaderAdapter=0,short=0,indicator_place_java.io.File>=1
