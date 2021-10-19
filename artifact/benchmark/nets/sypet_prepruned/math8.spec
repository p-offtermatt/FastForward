vars
double double[][] org.apache.commons.math.stat.Frequency org.apache.commons.math.stat.clustering.KMeansPlusPlusClusterer$EmptyClusterStrategy[] org.apache.commons.math.stat.correlation.Covariance org.apache.commons.math.stat.correlation.PearsonsCorrelation org.apache.commons.math.stat.correlation.SpearmansCorrelation org.apache.commons.math.stat.descriptive.AbstractStorelessUnivariateStatistic org.apache.commons.math.stat.descriptive.AbstractUnivariateStatistic org.apache.commons.math.stat.descriptive.AggregateSummaryStatistics org.apache.commons.math.stat.descriptive.DescriptiveStatistics org.apache.commons.math.stat.descriptive.SummaryStatistics org.apache.commons.math.stat.descriptive.SynchronizedDescriptiveStatistics org.apache.commons.math.stat.descriptive.SynchronizedSummaryStatistics org.apache.commons.math.stat.descriptive.moment.FirstMoment org.apache.commons.math.stat.descriptive.moment.FourthMoment org.apache.commons.math.stat.descriptive.moment.GeometricMean org.apache.commons.math.stat.descriptive.moment.Kurtosis org.apache.commons.math.stat.descriptive.moment.Mean org.apache.commons.math.stat.descriptive.moment.SecondMoment org.apache.commons.math.stat.descriptive.moment.SemiVariance$Direction[] org.apache.commons.math.stat.descriptive.moment.SemiVariance org.apache.commons.math.stat.descriptive.moment.Skewness org.apache.commons.math.stat.descriptive.moment.StandardDeviation org.apache.commons.math.stat.descriptive.moment.ThirdMoment org.apache.commons.math.stat.descriptive.moment.Variance org.apache.commons.math.stat.descriptive.rank.Max org.apache.commons.math.stat.descriptive.rank.Median org.apache.commons.math.stat.descriptive.rank.Min org.apache.commons.math.stat.descriptive.rank.Percentile org.apache.commons.math.stat.descriptive.summary.Product org.apache.commons.math.stat.descriptive.summary.Sum org.apache.commons.math.stat.descriptive.summary.SumOfLogs org.apache.commons.math.stat.descriptive.summary.SumOfSquares org.apache.commons.math.stat.inference.ChiSquareTestImpl org.apache.commons.math.stat.inference.OneWayAnovaImpl org.apache.commons.math.stat.inference.TTestImpl org.apache.commons.math.stat.inference.ChiSquareTest org.apache.commons.math.stat.inference.OneWayAnova org.apache.commons.math.stat.inference.TTest org.apache.commons.math.stat.inference.UnknownDistributionChiSquareTest org.apache.commons.math.stat.ranking.NaNStrategy[] org.apache.commons.math.stat.ranking.NaturalRanking org.apache.commons.math.stat.ranking.TiesStrategy[] org.apache.commons.math.stat.regression.AbstractMultipleLinearRegression org.apache.commons.math.stat.regression.GLSMultipleLinearRegression org.apache.commons.math.stat.regression.OLSMultipleLinearRegression org.apache.commons.math.stat.regression.SimpleRegression int long org.apache.commons.math.linear.RealMatrix indicator_place_double[][] org.apache.commons.math.stat.descriptive.StorelessUnivariateStatistic org.apache.commons.math.stat.descriptive.UnivariateStatistic double[] org.apache.commons.math.stat.descriptive.AggregateSummaryStatistics$AggregatingSummaryStatistics org.apache.commons.math.stat.descriptive.StatisticalSummary boolean org.apache.commons.math.stat.descriptive.moment.SemiVariance$Direction org.apache.commons.math.stat.descriptive.moment.VectorialMean indicator_place_double org.apache.commons.math.stat.ranking.NaNStrategy org.apache.commons.math.stat.ranking.TiesStrategy org.apache.commons.math.stat.descriptive.MultivariateSummaryStatistics org.apache.commons.math.stat.descriptive.SynchronizedMultivariateSummaryStatistics org.apache.commons.math.stat.descriptive.moment.VectorialCovariance org.apache.commons.math.stat.ranking.NaturalRanking$IntDoublePair org.apache.commons.math.stat.descriptive.StorelessUnivariateStatistic[]

rules
orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
int' = int-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,long >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
long' = long-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
int' = int-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,long >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
long' = long-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
int' = int+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
int' = int-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,long >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
long' = long+0;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
int' = int-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,long >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
long' = long+0;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
long' = long+1;

org.apache.commons.math.stat.Frequency >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
int' = int-1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1,long >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1,
long' = long-1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency-1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,double >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
int' = int-2,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,double >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
int' = int-2,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 2,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+0;

org.apache.commons.math.stat.clustering.KMeansPlusPlusClusterer$EmptyClusterStrategy[] >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotclusteringdotKMeansPlusPlusClustererdollarEmptyClusterStrategyleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotclusteringdotKMeansPlusPlusClustererdollarEmptyClusterStrategyleftbracketrightbracket+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance-1,
double' = double+1;

boolean >= 1,doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance >= 1 ->
boolean' = boolean-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance-1,
int' = int+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

org.apache.commons.math.stat.correlation.Covariance >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance+1;

doubleleftbracketrightbracketleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance+1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

boolean >= 1,doubleleftbracketrightbracketleftbracketrightbracket >= 1 ->
boolean' = boolean-1,
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance+1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1 ->
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance+1;

boolean >= 1,orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1,doubleleftbracketrightbracket >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1,doubleleftbracketrightbracketleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation-1,
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1,orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+0;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1,orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+0;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

org.apache.commons.math.stat.correlation.PearsonsCorrelation >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation+1;

doubleleftbracketrightbracketleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation+1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1 ->
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation+1;

orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix-1,
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation-1,
double' = double+1;

doubleleftbracketrightbracketleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation >= 1 ->
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1,orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+0,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation-1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation+1;

org.apache.commons.math.stat.correlation.SpearmansCorrelation >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation+1;

orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1 ->
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix-1,
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1,doubleleftbracketrightbracket >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

org.apache.commons.math.stat.descriptive.AbstractStorelessUnivariateStatistic >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1,doubleleftbracketrightbracket >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

org.apache.commons.math.stat.descriptive.AbstractUnivariateStatistic >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic-1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic-1,
int' = int-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics+1;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics+1;

org.apache.commons.math.stat.descriptive.AggregateSummaryStatistics >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-2,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
int' = int-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
double' = double+1;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
int' = int+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

org.apache.commons.math.stat.descriptive.DescriptiveStatistics >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics+1;

int >= 1 ->
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics+0;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics-1,
int' = int-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
int' = int+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

boolean >= 1,int >= 1 ->
boolean' = boolean-1,
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
int' = int-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics+0;

org.apache.commons.math.stat.descriptive.SummaryStatistics >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics+0;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
int' = int-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
int' = int+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics+0;

org.apache.commons.math.stat.descriptive.SynchronizedDescriptiveStatistics >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics+1;

int >= 1 ->
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1,int >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics-1,
int' = int-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
int' = int+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

boolean >= 1,int >= 1 ->
boolean' = boolean-1,
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics+0;

org.apache.commons.math.stat.descriptive.SynchronizedSummaryStatistics >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1,doubleleftbracketrightbracket >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment+0;

org.apache.commons.math.stat.descriptive.moment.FirstMoment >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment+1;

org.apache.commons.math.stat.descriptive.moment.FourthMoment >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean+0;

org.apache.commons.math.stat.descriptive.moment.GeometricMean >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean-1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis+0;

org.apache.commons.math.stat.descriptive.moment.Kurtosis >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean+0;

org.apache.commons.math.stat.descriptive.moment.Mean >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment+0;

org.apache.commons.math.stat.descriptive.moment.SecondMoment >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
indicator_place_double' = indicator_place_double+1;

org.apache.commons.math.stat.descriptive.moment.SemiVariance$Direction[] >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirectionleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirectionleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
boolean' = boolean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1,doubleleftbracketrightbracket >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection >= 1,doubleleftbracketrightbracket >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1,doubleleftbracketrightbracket >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection+1;

org.apache.commons.math.stat.descriptive.moment.SemiVariance >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance+1;

boolean >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance+1;

boolean >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1,boolean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
boolean' = boolean-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection-1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness+0;

org.apache.commons.math.stat.descriptive.moment.Skewness >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
boolean' = boolean+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
int' = int-2,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation+0;

org.apache.commons.math.stat.descriptive.moment.StandardDeviation >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation+1;

boolean >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation+1;

boolean >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1,
indicator_place_double' = indicator_place_double+1;

boolean >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment+0;

org.apache.commons.math.stat.descriptive.moment.ThirdMoment >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
boolean' = boolean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,doubleleftbracketrightbracket >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,doubleleftbracketrightbracket >= 1,double >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
int' = int-2,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,doubleleftbracketrightbracket >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,doubleleftbracketrightbracket >= 2,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,doubleleftbracketrightbracket >= 2,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,doubleleftbracketrightbracket >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance+0;

org.apache.commons.math.stat.descriptive.moment.Variance >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance+1;

boolean >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance+1;

boolean >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1,boolean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance-1,
boolean' = boolean-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

boolean >= 1,int >= 1 ->
boolean' = boolean-1,
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance >= 1,doubleleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean-1,
long' = long+1;

int >= 1 ->
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1,doubleleftbracketrightbracket >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax+0;

org.apache.commons.math.stat.descriptive.rank.Max >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

org.apache.commons.math.stat.descriptive.rank.Median >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian+0;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin+0;

org.apache.commons.math.stat.descriptive.rank.Min >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
int' = int-2,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile+0;

org.apache.commons.math.stat.descriptive.rank.Percentile >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile+1;

double >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile+1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-2;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
int' = int-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
int' = int-2,
double' = double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct+0;

org.apache.commons.math.stat.descriptive.summary.Product >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1,doubleleftbracketrightbracket >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1,doubleleftbracketrightbracket >= 2,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1,doubleleftbracketrightbracket >= 1,int >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum+0;

org.apache.commons.math.stat.descriptive.summary.Sum >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-2;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum-1,
double' = double-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs+0;

org.apache.commons.math.stat.descriptive.summary.SumOfLogs >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-1,
int' = int-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-1,
long' = long+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares+0;

org.apache.commons.math.stat.descriptive.summary.SumOfSquares >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares+0;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-2;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares-1,
indicator_place_double' = indicator_place_double+1;

org.apache.commons.math.stat.inference.ChiSquareTestImpl >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTestImpl+1;

org.apache.commons.math.stat.inference.OneWayAnovaImpl >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnovaImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnovaImpl+1;

doubleleftbracketrightbracket >= 2,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,double >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double-1,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,double >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double-2,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double-2,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1,orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl-1,
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

org.apache.commons.math.stat.inference.TTestImpl >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl+1;

doubleleftbracketrightbracket >= 2,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,double >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double-2,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double-2,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double-1,
boolean' = boolean+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

doubleleftbracketrightbracket >= 1,double >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1,double >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-1,
double' = double+0,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-2,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 2 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary-2,
double' = double+1;

org.apache.commons.math.stat.inference.ChiSquareTest >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest+1;

org.apache.commons.math.stat.inference.OneWayAnova >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova' = orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova+1;

org.apache.commons.math.stat.inference.TTest >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest+1;

org.apache.commons.math.stat.inference.UnknownDistributionChiSquareTest >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest-1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest-1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova' = orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova-1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest-1;

org.apache.commons.math.stat.ranking.NaNStrategy[] >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategyleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategyleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair-1,
int' = int+1;

double >= 1,int >= 1 ->
double' = double-1,
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+0,
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking-1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking-1,
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking-1,
orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy+1;

org.apache.commons.math.stat.ranking.NaturalRanking >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy-1,
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy >= 1,orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy-1,
orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy-1,
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy-1,
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking+1;

org.apache.commons.math.stat.ranking.TiesStrategy[] >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategyleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategyleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
boolean' = boolean+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket+1;

org.apache.commons.math.stat.regression.AbstractMultipleLinearRegression >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1,
int' = int-2;

boolean >= 1,orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
boolean' = boolean-1,
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression-1;

org.apache.commons.math.stat.regression.GLSMultipleLinearRegression >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression+1;

doubleleftbracketrightbracket >= 1,doubleleftbracketrightbracketleftbracketrightbracket >= 2,orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-2,
orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression-1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression-1,
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

org.apache.commons.math.stat.regression.OLSMultipleLinearRegression >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression+1;

doubleleftbracketrightbracket >= 1,doubleleftbracketrightbracketleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression-1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

doubleleftbracketrightbracket >= 1,orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1,int >= 2 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket-1,
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression-1,
int' = int-2;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
double' = double+1;

double >= 1,orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
double' = double+0,
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
long' = long+1;

org.apache.commons.math.stat.regression.SimpleRegression >= 0 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression+1;

int >= 1 ->
int' = int-1,
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression+1;

double >= 2,orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
double' = double-2,
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1,doubleleftbracketrightbracketleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1;

double >= 2,orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
double' = double-2,
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
indicator_place_double' = indicator_place_double+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1,doubleleftbracketrightbracketleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression-1,
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket-1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

boolean >= 1 ->
boolean' = boolean+1;

double >= 1 ->
double' = double+1,
indicator_place_double' = indicator_place_double+1;

doubleleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracket' = doubleleftbracketrightbracket+1;

doubleleftbracketrightbracketleftbracketrightbracket >= 1 ->
doubleleftbracketrightbracketleftbracketrightbracket' = doubleleftbracketrightbracketleftbracketrightbracket+1,
indicator_place_doubleleftbracketrightbracketleftbracketrightbracket' = indicator_place_doubleleftbracketrightbracketleftbracketrightbracket+1;

int >= 1 ->
int' = int+1;

long >= 1 ->
long' = long+1;

orgdotapachedotcommonsdotmathdotlineardotRealMatrix >= 1 ->
orgdotapachedotcommonsdotmathdotlineardotRealMatrix' = orgdotapachedotcommonsdotmathdotlineardotRealMatrix+1;

orgdotapachedotcommonsdotmathdotstatdotFrequency >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotFrequency' = orgdotapachedotcommonsdotmathdotstatdotFrequency+1;

orgdotapachedotcommonsdotmathdotstatdotclusteringdotKMeansPlusPlusClustererdollarEmptyClusterStrategyleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotclusteringdotKMeansPlusPlusClustererdollarEmptyClusterStrategyleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotclusteringdotKMeansPlusPlusClustererdollarEmptyClusterStrategyleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotCovariance+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotPearsonsCorrelation+1;

orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation' = orgdotapachedotcommonsdotmathdotstatdotcorrelationdotSpearmansCorrelation+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAbstractUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotAggregateSummaryStatisticsdollarAggregatingSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotDescriptiveStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotMultivariateSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStatisticalSummary+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotStorelessUnivariateStatisticleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedDescriptiveStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedMultivariateSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotSynchronizedSummaryStatistics+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotUnivariateStatistic+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFirstMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotFourthMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotGeometricMean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotKurtosis+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotMean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSecondMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirection+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirectionleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirectionleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSemiVariancedollarDirectionleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotSkewness+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotStandardDeviation+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotThirdMoment+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialCovariance+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotmomentdotVectorialMean+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMax+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMedian+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotMin+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotrankdotPercentile+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotProduct+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSum+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfLogs+1;

orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares' = orgdotapachedotcommonsdotmathdotstatdotdescriptivedotsummarydotSumOfSquares+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTest+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTestImpl >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotChiSquareTestImpl+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova' = orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnova+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnovaImpl >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnovaImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotOneWayAnovaImpl+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTest+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl' = orgdotapachedotcommonsdotmathdotstatdotinferencedotTTestImpl+1;

orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest' = orgdotapachedotcommonsdotmathdotstatdotinferencedotUnknownDistributionChiSquareTest+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategy+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategyleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategyleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaNStrategyleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRanking+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair' = orgdotapachedotcommonsdotmathdotstatdotrankingdotNaturalRankingdollarIntDoublePair+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy' = orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategy+1;

orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategyleftbracketrightbracket >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategyleftbracketrightbracket' = orgdotapachedotcommonsdotmathdotstatdotrankingdotTiesStrategyleftbracketrightbracket+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotAbstractMultipleLinearRegression+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotGLSMultipleLinearRegression+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotOLSMultipleLinearRegression+1;

orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression >= 1 ->
orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression' = orgdotapachedotcommonsdotmathdotstatdotregressiondotSimpleRegression+1;



init
double=1, double[][]=1, org.apache.commons.math.stat.Frequency=0, org.apache.commons.math.stat.clustering.KMeansPlusPlusClusterer$EmptyClusterStrategy[]=0, org.apache.commons.math.stat.correlation.Covariance=0, org.apache.commons.math.stat.correlation.PearsonsCorrelation=0, org.apache.commons.math.stat.correlation.SpearmansCorrelation=0, org.apache.commons.math.stat.descriptive.AbstractStorelessUnivariateStatistic=0, org.apache.commons.math.stat.descriptive.AbstractUnivariateStatistic=0, org.apache.commons.math.stat.descriptive.AggregateSummaryStatistics=0, org.apache.commons.math.stat.descriptive.DescriptiveStatistics=0, org.apache.commons.math.stat.descriptive.SummaryStatistics=0, org.apache.commons.math.stat.descriptive.SynchronizedDescriptiveStatistics=0, org.apache.commons.math.stat.descriptive.SynchronizedSummaryStatistics=0, org.apache.commons.math.stat.descriptive.moment.FirstMoment=0, org.apache.commons.math.stat.descriptive.moment.FourthMoment=0, org.apache.commons.math.stat.descriptive.moment.GeometricMean=0, org.apache.commons.math.stat.descriptive.moment.Kurtosis=0, org.apache.commons.math.stat.descriptive.moment.Mean=0, org.apache.commons.math.stat.descriptive.moment.SecondMoment=0, org.apache.commons.math.stat.descriptive.moment.SemiVariance$Direction[]=0, org.apache.commons.math.stat.descriptive.moment.SemiVariance=0, org.apache.commons.math.stat.descriptive.moment.Skewness=0, org.apache.commons.math.stat.descriptive.moment.StandardDeviation=0, org.apache.commons.math.stat.descriptive.moment.ThirdMoment=0, org.apache.commons.math.stat.descriptive.moment.Variance=0, org.apache.commons.math.stat.descriptive.rank.Max=0, org.apache.commons.math.stat.descriptive.rank.Median=0, org.apache.commons.math.stat.descriptive.rank.Min=0, org.apache.commons.math.stat.descriptive.rank.Percentile=0, org.apache.commons.math.stat.descriptive.summary.Product=0, org.apache.commons.math.stat.descriptive.summary.Sum=0, org.apache.commons.math.stat.descriptive.summary.SumOfLogs=0, org.apache.commons.math.stat.descriptive.summary.SumOfSquares=0, org.apache.commons.math.stat.inference.ChiSquareTestImpl=0, org.apache.commons.math.stat.inference.OneWayAnovaImpl=0, org.apache.commons.math.stat.inference.TTestImpl=0, org.apache.commons.math.stat.inference.ChiSquareTest=0, org.apache.commons.math.stat.inference.OneWayAnova=0, org.apache.commons.math.stat.inference.TTest=0, org.apache.commons.math.stat.inference.UnknownDistributionChiSquareTest=0, org.apache.commons.math.stat.ranking.NaNStrategy[]=0, org.apache.commons.math.stat.ranking.NaturalRanking=0, org.apache.commons.math.stat.ranking.TiesStrategy[]=0, org.apache.commons.math.stat.regression.AbstractMultipleLinearRegression=0, org.apache.commons.math.stat.regression.GLSMultipleLinearRegression=0, org.apache.commons.math.stat.regression.OLSMultipleLinearRegression=0, org.apache.commons.math.stat.regression.SimpleRegression=0, int=0, long=0, org.apache.commons.math.linear.RealMatrix=0, indicator_place_double[][]=0, org.apache.commons.math.stat.descriptive.StorelessUnivariateStatistic=0, org.apache.commons.math.stat.descriptive.UnivariateStatistic=0, double[]=0, org.apache.commons.math.stat.descriptive.AggregateSummaryStatistics$AggregatingSummaryStatistics=0, org.apache.commons.math.stat.descriptive.StatisticalSummary=0, boolean=0, org.apache.commons.math.stat.descriptive.moment.SemiVariance$Direction=0, org.apache.commons.math.stat.descriptive.moment.VectorialMean=0, indicator_place_double=0, org.apache.commons.math.stat.ranking.NaNStrategy=0, org.apache.commons.math.stat.ranking.TiesStrategy=0, org.apache.commons.math.stat.descriptive.MultivariateSummaryStatistics=0, org.apache.commons.math.stat.descriptive.SynchronizedMultivariateSummaryStatistics=0, org.apache.commons.math.stat.descriptive.moment.VectorialCovariance=0, org.apache.commons.math.stat.ranking.NaturalRanking$IntDoublePair=0, org.apache.commons.math.stat.descriptive.StorelessUnivariateStatistic[]=0

target
double=1,boolean=0,double[]=0,double[][]=0,int=0,long=0,org.apache.commons.math.linear.RealMatrix=0,org.apache.commons.math.stat.Frequency=0,org.apache.commons.math.stat.clustering.KMeansPlusPlusClusterer$EmptyClusterStrategy[]=0,org.apache.commons.math.stat.correlation.Covariance=0,org.apache.commons.math.stat.correlation.PearsonsCorrelation=0,org.apache.commons.math.stat.correlation.SpearmansCorrelation=0,org.apache.commons.math.stat.descriptive.AbstractStorelessUnivariateStatistic=0,org.apache.commons.math.stat.descriptive.AbstractUnivariateStatistic=0,org.apache.commons.math.stat.descriptive.AggregateSummaryStatistics=0,org.apache.commons.math.stat.descriptive.AggregateSummaryStatistics$AggregatingSummaryStatistics=0,org.apache.commons.math.stat.descriptive.DescriptiveStatistics=0,org.apache.commons.math.stat.descriptive.MultivariateSummaryStatistics=0,org.apache.commons.math.stat.descriptive.StatisticalSummary=0,org.apache.commons.math.stat.descriptive.StorelessUnivariateStatistic=0,org.apache.commons.math.stat.descriptive.StorelessUnivariateStatistic[]=0,org.apache.commons.math.stat.descriptive.SummaryStatistics=0,org.apache.commons.math.stat.descriptive.SynchronizedDescriptiveStatistics=0,org.apache.commons.math.stat.descriptive.SynchronizedMultivariateSummaryStatistics=0,org.apache.commons.math.stat.descriptive.SynchronizedSummaryStatistics=0,org.apache.commons.math.stat.descriptive.UnivariateStatistic=0,org.apache.commons.math.stat.descriptive.moment.FirstMoment=0,org.apache.commons.math.stat.descriptive.moment.FourthMoment=0,org.apache.commons.math.stat.descriptive.moment.GeometricMean=0,org.apache.commons.math.stat.descriptive.moment.Kurtosis=0,org.apache.commons.math.stat.descriptive.moment.Mean=0,org.apache.commons.math.stat.descriptive.moment.SecondMoment=0,org.apache.commons.math.stat.descriptive.moment.SemiVariance=0,org.apache.commons.math.stat.descriptive.moment.SemiVariance$Direction=0,org.apache.commons.math.stat.descriptive.moment.SemiVariance$Direction[]=0,org.apache.commons.math.stat.descriptive.moment.Skewness=0,org.apache.commons.math.stat.descriptive.moment.StandardDeviation=0,org.apache.commons.math.stat.descriptive.moment.ThirdMoment=0,org.apache.commons.math.stat.descriptive.moment.Variance=0,org.apache.commons.math.stat.descriptive.moment.VectorialCovariance=0,org.apache.commons.math.stat.descriptive.moment.VectorialMean=0,org.apache.commons.math.stat.descriptive.rank.Max=0,org.apache.commons.math.stat.descriptive.rank.Median=0,org.apache.commons.math.stat.descriptive.rank.Min=0,org.apache.commons.math.stat.descriptive.rank.Percentile=0,org.apache.commons.math.stat.descriptive.summary.Product=0,org.apache.commons.math.stat.descriptive.summary.Sum=0,org.apache.commons.math.stat.descriptive.summary.SumOfLogs=0,org.apache.commons.math.stat.descriptive.summary.SumOfSquares=0,org.apache.commons.math.stat.inference.ChiSquareTest=0,org.apache.commons.math.stat.inference.ChiSquareTestImpl=0,org.apache.commons.math.stat.inference.OneWayAnova=0,org.apache.commons.math.stat.inference.OneWayAnovaImpl=0,org.apache.commons.math.stat.inference.TTest=0,org.apache.commons.math.stat.inference.TTestImpl=0,org.apache.commons.math.stat.inference.UnknownDistributionChiSquareTest=0,org.apache.commons.math.stat.ranking.NaNStrategy=0,org.apache.commons.math.stat.ranking.NaNStrategy[]=0,org.apache.commons.math.stat.ranking.NaturalRanking=0,org.apache.commons.math.stat.ranking.NaturalRanking$IntDoublePair=0,org.apache.commons.math.stat.ranking.TiesStrategy=0,org.apache.commons.math.stat.ranking.TiesStrategy[]=0,org.apache.commons.math.stat.regression.AbstractMultipleLinearRegression=0,org.apache.commons.math.stat.regression.GLSMultipleLinearRegression=0,org.apache.commons.math.stat.regression.OLSMultipleLinearRegression=0,org.apache.commons.math.stat.regression.SimpleRegression=0,indicator_place_double>=1,indicator_place_double[][]>=1
