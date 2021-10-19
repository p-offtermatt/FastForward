vars
java.awt.geom.Point2D java.awt.geom.Area double java.awt.geom.AffineTransform java.awt.geom.Arc2D$Double java.awt.geom.Arc2D$Float java.awt.geom.CubicCurve2D$Double java.awt.geom.CubicCurve2D$Float java.awt.geom.Ellipse2D$Double java.awt.geom.Ellipse2D$Float java.awt.geom.GeneralPath java.awt.geom.IllegalPathStateException java.awt.geom.Line2D$Double java.awt.geom.Line2D$Float java.awt.geom.Path2D$Double java.awt.geom.Path2D$Float java.awt.geom.Point2D$Double java.awt.geom.Point2D$Float java.awt.geom.QuadCurve2D$Double java.awt.geom.QuadCurve2D$Float java.awt.geom.Rectangle2D$Double java.awt.geom.Rectangle2D$Float java.awt.geom.RoundRectangle2D$Double java.awt.geom.RoundRectangle2D$Float boolean int indicator_place_double java.lang.Object indicator_place_java.awt.geom.Area java.awt.Rectangle java.awt.geom.Rectangle2D indicator_place_java.awt.geom.Point2D java.awt.Shape

rules
javadotlangdotObject >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
boolean' = boolean+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
boolean' = boolean+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
double' = double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
double' = double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
double' = double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
double' = double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
double' = double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
double' = double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
double' = double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
int' = int+1;

javadotawtdotShape >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotShape' = javadotawtdotShape+0,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+0;

int >= 1 ->
int' = int-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1;

double >= 2,int >= 1 ->
double' = double-2,
int' = int-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

double >= 1 ->
double' = double-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

double >= 2 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

double >= 3 ->
double' = double-3,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

double >= 4 ->
double' = double-4,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

double >= 2 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

double >= 2 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

double >= 2 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
javadotlangdotObject' = javadotlangdotObject+1;

java.awt.geom.AffineTransform >= 0 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+0;

javadotawtdotgeomdotAffineTransform >= 2 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-2;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1;

javadotawtdotgeomdotAffineTransform >= 2 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-2;

javadotawtdotgeomdotAffineTransform >= 1,int >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
int' = int-1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1,int >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
int' = int-1,
indicator_place_double' = indicator_place_double+1;

double >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 3,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-3,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 4,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-4,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1;

javadotawtdotgeomdotAffineTransform >= 1,int >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
int' = int-1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1,int >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
int' = int-1,
indicator_place_double' = indicator_place_double+1;

double >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 3,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-3,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 4,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-4,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotAffineTransform >= 2 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-2;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotAffineTransform >= 1 ->
double' = double-2,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
boolean' = boolean+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double+1;

java.awt.geom.Arc2D$Double >= 0 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble+1;

int >= 1 ->
int' = int-1,
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble+1;

double >= 2,javadotawtdotgeomdotRectangle2D >= 1,int >= 1 ->
double' = double-2,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
int' = int-1,
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1,double >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1,double >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
boolean' = boolean+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
double' = double+1;

java.awt.geom.Arc2D$Float >= 0 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat+1;

int >= 1 ->
int' = int-1,
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat+1;

double >= 1,javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
double' = double-1,
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
indicator_place_double' = indicator_place_double+1;

double >= 1,javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
double' = double-1,
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotArea >= 2 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-2,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1,double >= 4 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
double' = double-4,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotArea >= 1,javadotawtdotgeomdotRectangle2D >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
javadotawtdotRectangle' = javadotawtdotRectangle+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea+0,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
javadotlangdotObject' = javadotlangdotObject+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

java.awt.geom.Area >= 0 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea+1;

javadotawtdotShape >= 1 ->
javadotawtdotShape' = javadotawtdotShape-1,
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 2 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-2,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 2 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-2,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 2 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-2,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 2 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-2,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotArea >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.CubicCurve2D$Double >= 0 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.CubicCurve2D$Float >= 0 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble-1,
boolean' = boolean+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.Ellipse2D$Double >= 0 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble+1;

double >= 4 ->
double' = double-4,
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1,double >= 4 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble-1,
double' = double-4,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat-1,
boolean' = boolean+1;

javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.Ellipse2D$Float >= 0 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat+1;

double >= 4,javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
double' = double-4,
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat-1,
indicator_place_double' = indicator_place_double+1;

java.awt.geom.GeneralPath >= 0 ->
javadotawtdotgeomdotGeneralPath' = javadotawtdotgeomdotGeneralPath+1;

int >= 1 ->
int' = int-1,
javadotawtdotgeomdotGeneralPath' = javadotawtdotgeomdotGeneralPath+1;

int >= 2 ->
int' = int-2,
javadotawtdotgeomdotGeneralPath' = javadotawtdotgeomdotGeneralPath+1;

javadotawtdotShape >= 1 ->
javadotawtdotShape' = javadotawtdotShape-1,
javadotawtdotgeomdotGeneralPath' = javadotawtdotgeomdotGeneralPath+1;

java.awt.geom.IllegalPathStateException >= 0 ->
javadotawtdotgeomdotIllegalPathStateException' = javadotawtdotgeomdotIllegalPathStateException+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.Line2D$Double >= 0 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble+1;

double >= 4 ->
double' = double-4,
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-2,
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1,double >= 4 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble-1,
double' = double-4,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.Line2D$Float >= 0 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat+1;

javadotawtdotgeomdotPoint2D >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-2,
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1,double >= 4 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat-1,
double' = double-4,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPath2DdollarDouble >= 1 ->
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

javadotawtdotgeomdotPath2DdollarDouble >= 1 ->
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble-1,
javadotlangdotObject' = javadotlangdotObject+1;

java.awt.geom.Path2D$Double >= 0 ->
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble+1;

int >= 1 ->
int' = int-1,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble+1;

int >= 2 ->
int' = int-2,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble+1;

javadotawtdotShape >= 1 ->
javadotawtdotShape' = javadotawtdotShape-1,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble+1;

javadotawtdotShape >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotShape' = javadotawtdotShape-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble+1;

double >= 2,javadotawtdotgeomdotPath2DdollarDouble >= 1 ->
double' = double-2,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotPath2DdollarDouble >= 1 ->
double' = double-2,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble-1,
indicator_place_double' = indicator_place_double+1;

double >= 4,javadotawtdotgeomdotPath2DdollarDouble >= 1 ->
double' = double-4,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotAffineTransform >= 1,javadotawtdotgeomdotPath2DdollarDouble >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble-1;

javadotawtdotgeomdotPath2DdollarFloat >= 1 ->
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

javadotawtdotgeomdotPath2DdollarFloat >= 1 ->
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat-1,
javadotlangdotObject' = javadotlangdotObject+1;

java.awt.geom.Path2D$Float >= 0 ->
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat+1;

int >= 1 ->
int' = int-1,
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat+1;

int >= 2 ->
int' = int-2,
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat+1;

javadotawtdotShape >= 1 ->
javadotawtdotShape' = javadotawtdotShape-1,
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat+1;

javadotawtdotShape >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotShape' = javadotawtdotShape-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1,
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat+1;

double >= 2,javadotawtdotgeomdotPath2DdollarFloat >= 1 ->
double' = double-2,
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat-1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotPath2DdollarFloat >= 1 ->
double' = double-2,
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat-1,
indicator_place_double' = indicator_place_double+1;

double >= 4,javadotawtdotgeomdotPath2DdollarFloat >= 1 ->
double' = double-4,
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPath2DdollarFloat >= 1,javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat-1,
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform-1;

javadotawtdotgeomdotPoint2DdollarDouble >= 1 ->
javadotawtdotgeomdotPoint2DdollarDouble' = javadotawtdotgeomdotPoint2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotPoint2DdollarDouble >= 1 ->
javadotawtdotgeomdotPoint2DdollarDouble' = javadotawtdotgeomdotPoint2DdollarDouble-1,
double' = double+1;

java.awt.geom.Point2D$Double >= 0 ->
javadotawtdotgeomdotPoint2DdollarDouble' = javadotawtdotgeomdotPoint2DdollarDouble+1;

double >= 2 ->
double' = double-2,
javadotawtdotgeomdotPoint2DdollarDouble' = javadotawtdotgeomdotPoint2DdollarDouble+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2DdollarDouble >= 1,double >= 2 ->
javadotawtdotgeomdotPoint2DdollarDouble' = javadotawtdotgeomdotPoint2DdollarDouble-1,
double' = double-2,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2DdollarFloat >= 1 ->
javadotawtdotgeomdotPoint2DdollarFloat' = javadotawtdotgeomdotPoint2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotPoint2DdollarFloat >= 1 ->
javadotawtdotgeomdotPoint2DdollarFloat' = javadotawtdotgeomdotPoint2DdollarFloat-1,
double' = double+1;

java.awt.geom.Point2D$Float >= 0 ->
javadotawtdotgeomdotPoint2DdollarFloat' = javadotawtdotgeomdotPoint2DdollarFloat+1;

javadotawtdotgeomdotPoint2DdollarFloat >= 1,double >= 2 ->
javadotawtdotgeomdotPoint2DdollarFloat' = javadotawtdotgeomdotPoint2DdollarFloat-1,
double' = double-2,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 1,javadotlangdotObject >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 1,double >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
double' = double-1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1,
indicator_place_double' = indicator_place_double+1;

double >= 4 ->
double' = double-3,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-2,
double' = double+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 1,double >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
double' = double-1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1,
indicator_place_double' = indicator_place_double+1;

double >= 4 ->
double' = double-3,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-2,
double' = double+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
double' = double+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
double' = double+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
javadotlangdotObject' = javadotlangdotObject+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2D >= 1,double >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
double' = double-2,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 2 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-2,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.QuadCurve2D$Double >= 0 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.QuadCurve2D$Float >= 0 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat+1;

javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
boolean' = boolean+1;

javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
double' = double+1;

double >= 2,javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
double' = double-2,
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
int' = int+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotRectangle2D >= 1,javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+0,
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1;

javadotawtdotgeomdotRectangle2D >= 1,javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+0,
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1;

javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.Rectangle2D$Double >= 0 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble+1;

double >= 4 ->
double' = double-4,
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble+1,
indicator_place_double' = indicator_place_double+1;

double >= 4,javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
double' = double-4,
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotRectangle2D >= 1,javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble-1;

javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
boolean' = boolean+1;

javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
double' = double+1;

double >= 2,javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
double' = double-2,
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
int' = int+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotRectangle2D >= 1,javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+0,
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1;

javadotawtdotgeomdotRectangle2D >= 1,javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+0,
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1;

javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.Rectangle2D$Float >= 0 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat+1;

double >= 4,javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
double' = double-4,
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotRectangle2D >= 1,javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat-1;

javadotawtdotgeomdotRectangle2D >= 1,javadotlangdotObject >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

double >= 4,javadotawtdotgeomdotRectangle2D >= 1 ->
double' = double-4,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

double >= 4,javadotawtdotgeomdotRectangle2D >= 1 ->
double' = double-4,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

double >= 2,javadotawtdotgeomdotRectangle2D >= 1 ->
double' = double-2,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
int' = int+1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 1,javadotawtdotgeomdotRectangle2D >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
int' = int+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotRectangle2D >= 2 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1;

javadotawtdotgeomdotRectangle2D >= 2 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1;

javadotawtdotgeomdotRectangle2D >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+0;

double >= 2,javadotawtdotgeomdotRectangle2D >= 1 ->
double' = double-2,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotPoint2D >= 1,javadotawtdotgeomdotRectangle2D >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotRectangle2D >= 2 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-2;

javadotawtdotgeomdotRectangle2D >= 3 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-3;

double >= 4,javadotawtdotgeomdotRectangle2D >= 1 ->
double' = double-4,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
indicator_place_double' = indicator_place_double+1;

double >= 4,javadotawtdotgeomdotRectangle2D >= 1 ->
double' = double-4,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
indicator_place_double' = indicator_place_double+1;

javadotawtdotgeomdotRectangle2D >= 2 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-2;

javadotawtdotgeomdotRectangle2D >= 3 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-3;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
boolean' = boolean+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.RoundRectangle2D$Double >= 0 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
boolean' = boolean+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
double' = double+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat-1,
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

java.awt.geom.RoundRectangle2D$Float >= 0 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat+1;

javadotawtdotgeomdotRectangle2D >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D-1,
javadotawtdotShape' = javadotawtdotShape+1;

boolean >= 1 ->
boolean' = boolean+1;

double >= 1 ->
double' = double+1,
indicator_place_double' = indicator_place_double+1;

int >= 1 ->
int' = int+1;

javadotawtdotRectangle >= 1 ->
javadotawtdotRectangle' = javadotawtdotRectangle+1;

javadotawtdotShape >= 1 ->
javadotawtdotShape' = javadotawtdotShape+1;

javadotawtdotgeomdotAffineTransform >= 1 ->
javadotawtdotgeomdotAffineTransform' = javadotawtdotgeomdotAffineTransform+1;

javadotawtdotgeomdotArc2DdollarDouble >= 1 ->
javadotawtdotgeomdotArc2DdollarDouble' = javadotawtdotgeomdotArc2DdollarDouble+1;

javadotawtdotgeomdotArc2DdollarFloat >= 1 ->
javadotawtdotgeomdotArc2DdollarFloat' = javadotawtdotgeomdotArc2DdollarFloat+1;

javadotawtdotgeomdotArea >= 1 ->
javadotawtdotgeomdotArea' = javadotawtdotgeomdotArea+1,
indicator_place_javadotawtdotgeomdotArea' = indicator_place_javadotawtdotgeomdotArea+1;

javadotawtdotgeomdotCubicCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarDouble' = javadotawtdotgeomdotCubicCurve2DdollarDouble+1;

javadotawtdotgeomdotCubicCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotCubicCurve2DdollarFloat' = javadotawtdotgeomdotCubicCurve2DdollarFloat+1;

javadotawtdotgeomdotEllipse2DdollarDouble >= 1 ->
javadotawtdotgeomdotEllipse2DdollarDouble' = javadotawtdotgeomdotEllipse2DdollarDouble+1;

javadotawtdotgeomdotEllipse2DdollarFloat >= 1 ->
javadotawtdotgeomdotEllipse2DdollarFloat' = javadotawtdotgeomdotEllipse2DdollarFloat+1;

javadotawtdotgeomdotGeneralPath >= 1 ->
javadotawtdotgeomdotGeneralPath' = javadotawtdotgeomdotGeneralPath+1;

javadotawtdotgeomdotIllegalPathStateException >= 1 ->
javadotawtdotgeomdotIllegalPathStateException' = javadotawtdotgeomdotIllegalPathStateException+1;

javadotawtdotgeomdotLine2DdollarDouble >= 1 ->
javadotawtdotgeomdotLine2DdollarDouble' = javadotawtdotgeomdotLine2DdollarDouble+1;

javadotawtdotgeomdotLine2DdollarFloat >= 1 ->
javadotawtdotgeomdotLine2DdollarFloat' = javadotawtdotgeomdotLine2DdollarFloat+1;

javadotawtdotgeomdotPath2DdollarDouble >= 1 ->
javadotawtdotgeomdotPath2DdollarDouble' = javadotawtdotgeomdotPath2DdollarDouble+1;

javadotawtdotgeomdotPath2DdollarFloat >= 1 ->
javadotawtdotgeomdotPath2DdollarFloat' = javadotawtdotgeomdotPath2DdollarFloat+1;

javadotawtdotgeomdotPoint2D >= 1 ->
javadotawtdotgeomdotPoint2D' = javadotawtdotgeomdotPoint2D+1,
indicator_place_javadotawtdotgeomdotPoint2D' = indicator_place_javadotawtdotgeomdotPoint2D+1;

javadotawtdotgeomdotPoint2DdollarDouble >= 1 ->
javadotawtdotgeomdotPoint2DdollarDouble' = javadotawtdotgeomdotPoint2DdollarDouble+1;

javadotawtdotgeomdotPoint2DdollarFloat >= 1 ->
javadotawtdotgeomdotPoint2DdollarFloat' = javadotawtdotgeomdotPoint2DdollarFloat+1;

javadotawtdotgeomdotQuadCurve2DdollarDouble >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarDouble' = javadotawtdotgeomdotQuadCurve2DdollarDouble+1;

javadotawtdotgeomdotQuadCurve2DdollarFloat >= 1 ->
javadotawtdotgeomdotQuadCurve2DdollarFloat' = javadotawtdotgeomdotQuadCurve2DdollarFloat+1;

javadotawtdotgeomdotRectangle2D >= 1 ->
javadotawtdotgeomdotRectangle2D' = javadotawtdotgeomdotRectangle2D+1;

javadotawtdotgeomdotRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRectangle2DdollarDouble' = javadotawtdotgeomdotRectangle2DdollarDouble+1;

javadotawtdotgeomdotRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRectangle2DdollarFloat' = javadotawtdotgeomdotRectangle2DdollarFloat+1;

javadotawtdotgeomdotRoundRectangle2DdollarDouble >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarDouble' = javadotawtdotgeomdotRoundRectangle2DdollarDouble+1;

javadotawtdotgeomdotRoundRectangle2DdollarFloat >= 1 ->
javadotawtdotgeomdotRoundRectangle2DdollarFloat' = javadotawtdotgeomdotRoundRectangle2DdollarFloat+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject+1;



init
java.awt.geom.Point2D=1, java.awt.geom.Area=1, double=1, java.awt.geom.AffineTransform=0, java.awt.geom.Arc2D$Double=0, java.awt.geom.Arc2D$Float=0, java.awt.geom.CubicCurve2D$Double=0, java.awt.geom.CubicCurve2D$Float=0, java.awt.geom.Ellipse2D$Double=0, java.awt.geom.Ellipse2D$Float=0, java.awt.geom.GeneralPath=0, java.awt.geom.IllegalPathStateException=0, java.awt.geom.Line2D$Double=0, java.awt.geom.Line2D$Float=0, java.awt.geom.Path2D$Double=0, java.awt.geom.Path2D$Float=0, java.awt.geom.Point2D$Double=0, java.awt.geom.Point2D$Float=0, java.awt.geom.QuadCurve2D$Double=0, java.awt.geom.QuadCurve2D$Float=0, java.awt.geom.Rectangle2D$Double=0, java.awt.geom.Rectangle2D$Float=0, java.awt.geom.RoundRectangle2D$Double=0, java.awt.geom.RoundRectangle2D$Float=0, boolean=0, int=0, indicator_place_double=0, java.lang.Object=0, indicator_place_java.awt.geom.Area=0, java.awt.Rectangle=0, java.awt.geom.Rectangle2D=0, indicator_place_java.awt.geom.Point2D=0, java.awt.Shape=0

target
java.awt.geom.Area=1,boolean=0,double=0,int=0,java.awt.Rectangle=0,java.awt.Shape=0,java.awt.geom.AffineTransform=0,java.awt.geom.Arc2D$Double=0,java.awt.geom.Arc2D$Float=0,java.awt.geom.CubicCurve2D$Double=0,java.awt.geom.CubicCurve2D$Float=0,java.awt.geom.Ellipse2D$Double=0,java.awt.geom.Ellipse2D$Float=0,java.awt.geom.GeneralPath=0,java.awt.geom.IllegalPathStateException=0,java.awt.geom.Line2D$Double=0,java.awt.geom.Line2D$Float=0,java.awt.geom.Path2D$Double=0,java.awt.geom.Path2D$Float=0,java.awt.geom.Point2D=0,java.awt.geom.Point2D$Double=0,java.awt.geom.Point2D$Float=0,java.awt.geom.QuadCurve2D$Double=0,java.awt.geom.QuadCurve2D$Float=0,java.awt.geom.Rectangle2D=0,java.awt.geom.Rectangle2D$Double=0,java.awt.geom.Rectangle2D$Float=0,java.awt.geom.RoundRectangle2D$Double=0,java.awt.geom.RoundRectangle2D$Float=0,java.lang.Object=0,indicator_place_java.awt.geom.Point2D>=1,indicator_place_java.awt.geom.Area>=1,indicator_place_double>=1
