vars
org.joda.time.DateTime org.joda.time.Chronology org.joda.time.DateMidnight org.joda.time.DateTimeComparator org.joda.time.DateTimeField org.joda.time.DateTimeFieldType java.util.Map long java.util.Set org.joda.time.DateTimeZone org.joda.time.tz.NameProvider org.joda.time.tz.Provider org.joda.time.DurationField org.joda.time.DurationFieldType org.joda.time.Instant org.joda.time.LocalDate org.joda.time.LocalDateTime org.joda.time.LocalTime org.joda.time.MonthDay org.joda.time.MutableDateTime org.joda.time.MutableInterval org.joda.time.MutablePeriod org.joda.time.Partial org.joda.time.Period org.joda.time.PeriodType org.joda.time.TimeOfDay org.joda.time.UTCDateTimeZone org.joda.time.YearMonth org.joda.time.YearMonthDay org.joda.time.base.BaseDateTime org.joda.time.chrono.BuddhistChronology org.joda.time.chrono.CopticChronology org.joda.time.chrono.EthiopicChronology org.joda.time.chrono.GJChronology org.joda.time.chrono.GregorianChronology org.joda.time.chrono.ISOChronology org.joda.time.chrono.IslamicChronology org.joda.time.chrono.JulianChronology org.joda.time.convert.ConverterManager org.joda.time.field.AbstractReadableInstantFieldProperty org.joda.time.format.DateTimeFormatter org.joda.time.format.PeriodFormatter org.joda.time.format.PeriodFormatterBuilder org.joda.time.tz.DateTimeZoneBuilder org.joda.time.tz.DefaultNameProvider org.joda.time.tz.UTCProvider org.joda.time.tz.ZoneInfoCompiler boolean org.joda.time.tz.ZoneInfoLogger org.joda.time.DateMidnight$Property org.joda.time.Interval indicator_place_org.joda.time.DateTime org.joda.time.DateTime$Property int java.lang.String double java.util.TimeZone org.joda.time.Days org.joda.time.Duration org.joda.time.Hours org.joda.time.Minutes org.joda.time.Seconds org.joda.time.Weeks org.joda.time.IllegalFieldValueException org.joda.time.IllegalInstantException org.joda.time.JodaTimePermission java.util.Date org.joda.time.LocalDate$Property org.joda.time.LocalDateTime$Property org.joda.time.LocalTime$Property org.joda.time.DateTimeFieldType[] org.joda.time.MonthDay$Property org.joda.time.Months java.lang.Object org.joda.time.MutableDateTime$Property int[] org.joda.time.TimeOfDay$Property org.joda.time.YearMonth$Property org.joda.time.YearMonthDay$Property org.joda.time.Years org.joda.time.chrono.IslamicChronology$LeapYearPatternType org.joda.time.chrono.LenientChronology org.joda.time.chrono.StrictChronology org.joda.time.convert.DurationConverter[] org.joda.time.convert.InstantConverter[] org.joda.time.convert.IntervalConverter[] org.joda.time.convert.PartialConverter[] org.joda.time.convert.PeriodConverter[] org.joda.time.field.DelegatedDateTimeField org.joda.time.field.UnsupportedDurationField java.lang.Integer java.util.Locale org.joda.time.format.DateTimeParser org.joda.time.format.DateTimePrinter org.joda.time.format.PeriodParser org.joda.time.format.PeriodPrinter org.joda.time.tz.CachedDateTimeZone org.joda.time.tz.ZoneInfoProvider org.joda.time.ReadablePartial org.joda.time.ReadableInstant org.joda.time.base.AbstractDateTime org.joda.time.format.DateTimeFormatterBuilder$TimeZoneId org.joda.time.Partial$Property java.util.Calendar org.joda.time.chrono.ZonedChronology org.joda.time.convert.DurationConverter org.joda.time.convert.InstantConverter org.joda.time.convert.IntervalConverter org.joda.time.convert.PartialConverter org.joda.time.convert.PeriodConverter org.joda.time.field.DecoratedDurationField org.joda.time.field.ImpreciseDateTimeField org.joda.time.field.OffsetDateTimeField org.joda.time.field.PreciseDateTimeField org.joda.time.field.PreciseDurationDateTimeField org.joda.time.field.PreciseDurationField org.joda.time.field.SkipDateTimeField org.joda.time.field.SkipUndoDateTimeField org.joda.time.field.UnsupportedDateTimeField org.joda.time.field.ZeroIsMaxDateTimeField org.joda.time.format.DateTimeFormat$StyleFormatterCacheKey org.joda.time.tz.FixedDateTimeZone org.joda.time.field.DividedDateTimeField org.joda.time.field.RemainderDateTimeField org.joda.time.field.ScaledDurationField org.joda.time.format.DateTimeParserBucket java.text.DateFormatSymbols java.lang.Number java.util.GregorianCalendar

rules
orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotReadablePartial >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
long' = long-1,
intleftbracketrightbracket' = intleftbracketrightbracket+1;

orgdotjodadottimedotChronology >= 1,long >= 2,int >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
int' = int-1;

orgdotjodadottimedotChronology >= 1,int >= 4 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-4,
long' = long+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotReadablePartial >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
long' = long+0;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+0;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+0;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

org.joda.time.Chronology >= 0 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
int' = int-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1,long >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
long' = long-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
int' = int-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
int' = int-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

javadotlangdotString >= 1,orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

javadotutildotLocale >= 1,javadotlangdotString >= 1,orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotDateMidnight >= 1,long >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
long' = long-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

org.joda.time.DateMidnight >= 0 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnight >= 1,long >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
long' = long-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
long' = long-1,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0;

orgdotjodadottimedotDateMidnight >= 1,long >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
long' = long-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateMidnight >= 1,int >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0,
int' = int-1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+0;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

org.joda.time.DateMidnight >= 0 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

int >= 3 ->
int' = int-3,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotChronology >= 1,int >= 3 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-3,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateTimeZone >= 1,int >= 3 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
int' = int-3,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1,long >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
long' = long-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,long >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
long' = long-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

org.joda.time.DateTime >= 0 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,long >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
long' = long-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 3 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-3,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
long' = long-1,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,long >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
long' = long-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 4 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-4,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
int' = int-1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+0,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

org.joda.time.DateTime >= 0 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeComparator >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeComparator >= 1,javadotlangdotObject >= 2 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator-1,
javadotlangdotObject' = javadotlangdotObject-2,
int' = int+1;

org.joda.time.DateTimeComparator >= 0 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator+1;

org.joda.time.DateTimeComparator >= 0 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator+1;

orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator+1;

orgdotjodadottimedotDateTimeFieldType >= 2 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-2,
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator+1;

org.joda.time.DateTimeComparator >= 0 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator+1;

orgdotjodadottimedotDateTimeComparator >= 1 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotDateTimeComparator >= 1 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,intleftbracketrightbracket >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
int' = int+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,intleftbracketrightbracket >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long-1;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0;

orgdotjodadottimedotDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0;

orgdotjodadottimedotDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotDateTimeField >= 1,javadotlangdotString >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
javadotlangdotString' = javadotlangdotString-1,
long' = long+0;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeField >= 1,javadotlangdotString >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
javadotlangdotString' = javadotlangdotString-1,
long' = long+0;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

org.joda.time.DateTimeField >= 0 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

org.joda.time.DateTimeFieldType >= 0 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
boolean' = boolean+1;

long >= 1 ->
long' = long-1,
double' = double+1;

javadotutildotLocale >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
javadottextdotDateFormatSymbols' = javadottextdotDateFormatSymbols+1;

java.util.Map >= 0 ->
javadotutildotMap' = javadotutildotMap+1;

long >= 0 ->
long' = long+1;

double >= 1 ->
double' = double-1,
long' = long+1;

orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
long' = long+1;

long >= 1 ->
long' = long+0;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+0;

orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

long >= 1 ->
long' = long-1;

long >= 1 ->
long' = long-1;



javadotutildotMap >= 1 ->
javadotutildotMap' = javadotutildotMap-1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
int' = int+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotutildotLocale >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotutildotLocale' = javadotutildotLocale-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotutildotLocale >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotutildotLocale' = javadotutildotLocale-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

java.util.Set >= 0 ->
javadotutildotSet' = javadotutildotSet+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotutildotTimeZone' = javadotutildotTimeZone+1;

orgdotjodadottimedotDateTimeZone >= 1,boolean >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
boolean' = boolean-1,
long' = long+0;

orgdotjodadottimedotDateTimeZone >= 1,boolean >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
boolean' = boolean-1,
long' = long+0;

orgdotjodadottimedotDateTimeZone >= 1,boolean >= 1,long >= 2 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
boolean' = boolean-1,
long' = long-1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long+0;

orgdotjodadottimedotDateTimeZone >= 2,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-2,
long' = long+0;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long+0;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

int >= 2 ->
int' = int-2,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

javadotutildotTimeZone >= 1 ->
javadotutildotTimeZone' = javadotutildotTimeZone-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

org.joda.time.DateTimeZone >= 0 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

org.joda.time.tz.NameProvider >= 0 ->
orgdotjodadottimedottzdotNameProvider' = orgdotjodadottimedottzdotNameProvider+1;

org.joda.time.tz.Provider >= 0 ->
orgdotjodadottimedottzdotProvider' = orgdotjodadottimedottzdotProvider+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1;

orgdotjodadottimedottzdotNameProvider >= 1 ->
orgdotjodadottimedottzdotNameProvider' = orgdotjodadottimedottzdotNameProvider-1;

orgdotjodadottimedottzdotProvider >= 1 ->
orgdotjodadottimedottzdotProvider' = orgdotjodadottimedottzdotProvider-1;

orgdotjodadottimedotDays >= 2 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-2,
boolean' = boolean+1;

orgdotjodadottimedotDays >= 2 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-2,
boolean' = boolean+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
int' = int+1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotDays >= 1,int >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+0,
int' = int-1;

orgdotjodadottimedotDays >= 1,int >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+0,
int' = int-1;

orgdotjodadottimedotDays >= 2 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1;

orgdotjodadottimedotDays >= 1,int >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+0,
int' = int-1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotDays >= 1,int >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+0,
int' = int-1;

orgdotjodadottimedotDays >= 2 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays-1,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
long' = long+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
long' = long+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
long' = long+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
long' = long+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotDuration >= 1,long >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0,
long' = long-1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotDuration >= 1,long >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0,
long' = long-1;

orgdotjodadottimedotDuration >= 1,long >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0,
long' = long-1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotDuration >= 1,long >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0,
long' = long-1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0;

orgdotjodadottimedotDuration >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0,
long' = long-1,
int' = int-1;

orgdotjodadottimedotDuration >= 1,long >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+0,
long' = long-1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

long >= 2 ->
long' = long-2,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotDurationField >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotDurationField >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotDurationField >= 1,long >= 2 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotDurationField >= 1,long >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotDurationField >= 1,long >= 2 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotDurationField >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDurationField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotDurationField >= 1,long >= 2 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-1;

orgdotjodadottimedotDurationField >= 1,long >= 2 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-1;

orgdotjodadottimedotDurationField >= 1,int >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
int' = int-1,
long' = long+1;

orgdotjodadottimedotDurationField >= 1,int >= 1,long >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
int' = int-1,
long' = long+0;

orgdotjodadottimedotDurationField >= 1,long >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long+0;

orgdotjodadottimedotDurationField >= 1,long >= 2 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-1;

orgdotjodadottimedotDurationField >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long+1;

orgdotjodadottimedotDurationField >= 1,long >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long+0;

orgdotjodadottimedotDurationField >= 1,long >= 2 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-1;

orgdotjodadottimedotDurationField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotDurationField >= 1,long >= 2 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
long' = long-1;

orgdotjodadottimedotDurationField >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationField >= 0 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
boolean' = boolean+1;

orgdotjodadottimedotDurationFieldType >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.DurationFieldType >= 0 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotHours >= 2 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-2,
boolean' = boolean+1;

orgdotjodadottimedotHours >= 2 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-2,
boolean' = boolean+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
int' = int+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotHours >= 1,int >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+0,
int' = int-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotHours >= 1,int >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+0,
int' = int-1;

orgdotjodadottimedotHours >= 2 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1;

orgdotjodadottimedotHours >= 1,int >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+0,
int' = int-1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotHours >= 1,int >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+0,
int' = int-1;

orgdotjodadottimedotHours >= 2 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours-1,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotNumber' = javadotlangdotNumber+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotNumber' = javadotlangdotNumber+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotNumber' = javadotlangdotNumber+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

javadotlangdotString >= 1,javadotlangdotNumber >= 3 ->
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotNumber' = javadotlangdotNumber-3,
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

javadotlangdotString >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

orgdotjodadottimedotDateTimeFieldType >= 1,javadotlangdotNumber >= 3 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
javadotlangdotNumber' = javadotlangdotNumber-3,
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

orgdotjodadottimedotDateTimeFieldType >= 1,javadotlangdotString >= 1,javadotlangdotNumber >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
javadotlangdotString' = javadotlangdotString-1,
javadotlangdotNumber' = javadotlangdotNumber-1,
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

orgdotjodadottimedotDateTimeFieldType >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

orgdotjodadottimedotDurationFieldType >= 1,javadotlangdotNumber >= 3 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
javadotlangdotNumber' = javadotlangdotNumber-3,
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

orgdotjodadottimedotDurationFieldType >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

orgdotjodadottimedotIllegalFieldValueException >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException-1,
javadotlangdotString' = javadotlangdotString-1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotIllegalInstantException' = orgdotjodadottimedotIllegalInstantException+1;

javadotlangdotString >= 1,long >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
long' = long-1,
orgdotjodadottimedotIllegalInstantException' = orgdotjodadottimedotIllegalInstantException+1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant-1,
long' = long+1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotInstant >= 1,long >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+0,
long' = long-1;

org.joda.time.Instant >= 0 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

orgdotjodadottimedotInstant >= 1,long >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+0,
long' = long-1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+0;

orgdotjodadottimedotInstant >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+0,
long' = long-1,
int' = int-1;

orgdotjodadottimedotInstant >= 1,long >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+0,
long' = long-1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

org.joda.time.Instant >= 0 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotInterval >= 1 ->
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+0;

orgdotjodadottimedotInterval >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1;

orgdotjodadottimedotInterval >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+0,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1;

orgdotjodadottimedotInterval >= 1,long >= 1 ->
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+0,
long' = long-1;

orgdotjodadottimedotInterval >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+0,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1;

orgdotjodadottimedotInterval >= 1,long >= 1 ->
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+0,
long' = long-1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

long >= 2 ->
long' = long-2,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotChronology >= 1,long >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-2,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 2 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-2,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotJodaTimePermission' = orgdotjodadottimedotJodaTimePermission+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotlangdotString >= 1,orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotutildotLocale >= 1,javadotlangdotString >= 1,orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDate >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
boolean' = boolean+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDurationFieldType >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
boolean' = boolean+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+0;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
int' = int+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
javadotutildotDate' = javadotutildotDate+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

org.joda.time.LocalDate >= 0 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1,int >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+0,
int' = int-1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDate >= 1,orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

org.joda.time.LocalDate >= 0 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

int >= 3 ->
int' = int-3,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotChronology >= 1,int >= 3 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-3,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1,long >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
long' = long-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

javadotutildotLocale >= 1,orgdotjodadottimedotLocalDateTimedollarProperty >= 1,javadotlangdotString >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

javadotlangdotObject >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
boolean' = boolean+1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
boolean' = boolean+1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+0;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
int' = int+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
javadotutildotDate' = javadotutildotDate+1;

javadotutildotTimeZone >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
javadotutildotTimeZone' = javadotutildotTimeZone-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
javadotutildotDate' = javadotutildotDate+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

org.joda.time.LocalDateTime >= 0 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 3 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-3;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 4 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-4;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+0,
int' = int-1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

org.joda.time.LocalDateTime >= 0 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1,long >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
long' = long-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTime >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotLocalTime >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
boolean' = boolean+1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
boolean' = boolean+1;

orgdotjodadottimedotLocalTime >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
int' = int+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
int' = int+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
int' = int+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
int' = int+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
int' = int+1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
int' = int+0;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
int' = int+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

org.joda.time.LocalTime >= 0 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1,int >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+0,
int' = int-1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty+1;

orgdotjodadottimedotLocalTime >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime-1,
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty+1;

org.joda.time.LocalTime >= 0 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

int >= 2 ->
int' = int-2,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

int >= 3 ->
int' = int-3,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

int >= 4 ->
int' = int-4,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotMinutes >= 2 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-2,
boolean' = boolean+1;

orgdotjodadottimedotMinutes >= 2 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-2,
boolean' = boolean+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
int' = int+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotMinutes >= 1,int >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+0,
int' = int-1;

orgdotjodadottimedotMinutes >= 1,int >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+0,
int' = int-1;

orgdotjodadottimedotMinutes >= 2 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotMinutes >= 1,int >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+0,
int' = int-1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotMinutes >= 1,int >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+0,
int' = int-1;

orgdotjodadottimedotMinutes >= 2 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes-1,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
int' = int+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
int' = int+1;

orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
int' = int+1;

orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
int' = int+1;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
int' = int-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket+1;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
int' = int-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
int' = int-1;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
int' = int-1;

org.joda.time.MonthDay >= 0 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
int' = int-1;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
int' = int-1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
int' = int-1;

orgdotjodadottimedotMonthDay >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
int' = int-1;

orgdotjodadottimedotMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+0,
int' = int-1;

orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty+1;

orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty+1;

orgdotjodadottimedotMonthDay >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty+1;

org.joda.time.MonthDay >= 0 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

int >= 2 ->
int' = int-2,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotChronology >= 1,int >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-2,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonths >= 2 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths-2,
boolean' = boolean+1;

orgdotjodadottimedotMonths >= 2 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths-2,
boolean' = boolean+1;

orgdotjodadottimedotMonths >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths-1,
int' = int+1;

orgdotjodadottimedotMonths >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotMonths >= 1,int >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+0,
int' = int-1;

orgdotjodadottimedotMonths >= 1,int >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+0,
int' = int-1;

orgdotjodadottimedotMonths >= 2 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+1;

orgdotjodadottimedotMonths >= 1,int >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+0,
int' = int-1;

orgdotjodadottimedotMonths >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+1;

orgdotjodadottimedotMonths >= 1,int >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+0,
int' = int-1;

orgdotjodadottimedotMonths >= 2 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths-1;

orgdotjodadottimedotMonths >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1,long >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
long' = long-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
int' = int-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+0;

org.joda.time.MutableDateTime >= 0 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

org.joda.time.MutableDateTime >= 0 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTime >= 1,long >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
long' = long-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 3 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-3;

orgdotjodadottimedotMutableDateTime >= 1,long >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
long' = long-1;

orgdotjodadottimedotReadableInstant >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,long >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
long' = long-1;

orgdotjodadottimedotReadableInstant >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 4 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-4;

orgdotjodadottimedotMutableDateTime >= 1,long >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
long' = long-1;

orgdotjodadottimedotReadableInstant >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotMutableDateTime >= 1,int >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1,
int' = int-1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime-1;

orgdotjodadottimedotMutableInterval >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjodadottimedotMutableInterval >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

org.joda.time.MutableInterval >= 0 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

long >= 2 ->
long' = long-2,
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

orgdotjodadottimedotChronology >= 1,long >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-2,
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

orgdotjodadottimedotMutableInterval >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1;

orgdotjodadottimedotMutableInterval >= 1,long >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
long' = long-1;

orgdotjodadottimedotMutableInterval >= 1,long >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
long' = long-1;

orgdotjodadottimedotMutableInterval >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1;

orgdotjodadottimedotMutableInterval >= 1,long >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
long' = long-1;

orgdotjodadottimedotMutableInterval >= 1,long >= 2 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
long' = long-2;

orgdotjodadottimedotMutableInterval >= 1,orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2;

orgdotjodadottimedotMutableInterval >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1;

orgdotjodadottimedotMutableInterval >= 1,long >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval-1,
long' = long-1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

javadotlangdotString >= 1,orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

org.joda.time.MutablePeriod >= 0 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

int >= 4 ->
int' = int-4,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

javadotlangdotObject >= 1,orgdotjodadottimedotPeriodType >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

long >= 2 ->
long' = long-2,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotChronology >= 1,long >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-2,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotPeriodType >= 1,long >= 2 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-2,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotPeriodType >= 1,long >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-2,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotPeriodType >= 1,long >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotPeriodType >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotReadableInstant >= 2,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotMutablePeriod >= 1,long >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
long' = long-1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotMutablePeriod >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
long' = long-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,long >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
long' = long-1;

orgdotjodadottimedotMutablePeriod >= 1,long >= 2 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
long' = long-2;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotMutablePeriod >= 1,long >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
long' = long-2;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotMutablePeriod >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
long' = long-1;

orgdotjodadottimedotReadableInstant >= 2,orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 2 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-2;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotMutablePeriod >= 1,int >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod-1,
int' = int-1;

orgdotjodadottimedotPartialdollarProperty >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
int' = int+1;

orgdotjodadottimedotPartialdollarProperty >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotPartialdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
int' = int-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartialdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
int' = int-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartialdollarProperty >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartialdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
int' = int-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartialdollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

javadotutildotLocale >= 1,orgdotjodadottimedotPartialdollarProperty >= 1,javadotlangdotString >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartialdollarProperty >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartialdollarProperty >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
boolean' = boolean+1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
boolean' = boolean+1;

orgdotjodadottimedotPartial >= 1,int >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
int' = int+0;

orgdotjodadottimedotPartial >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
int' = int+1;

orgdotjodadottimedotPartial >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+1;

orgdotjodadottimedotPartial >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotPartial >= 1,int >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
int' = int-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotPartial >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket+1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+0,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+0,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1;

orgdotjodadottimedotPartial >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty+1;

orgdotjodadottimedotPartial >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial-1,
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.Partial >= 0 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

intleftbracketrightbracket >= 1,orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket >= 1 ->
intleftbracketrightbracket' = intleftbracketrightbracket-1,
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotChronology >= 1,intleftbracketrightbracket >= 1,orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
int' = int+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0;

orgdotjodadottimedotPeriod >= 1,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

javadotlangdotString >= 1,orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

orgdotjodadottimedotPeriod >= 1,int >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+0,
int' = int-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod-1,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

org.joda.time.Period >= 0 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

int >= 4 ->
int' = int-4,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

javadotlangdotObject >= 1,orgdotjodadottimedotPeriodType >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

long >= 2 ->
long' = long-2,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotChronology >= 1,long >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-2,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriodType >= 1,long >= 2 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-2,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotPeriodType >= 1,long >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-2,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriodType >= 1,long >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotPeriodType >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
long' = long-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotReadableInstant >= 2,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotReadablePartial >= 2,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

javadotlangdotObject >= 1,orgdotjodadottimedotPeriodType >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
boolean' = boolean+1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
boolean' = boolean+1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
int' = int+1;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
int' = int+1;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotPeriodType >= 1,int >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1,
int' = int-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+0;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

org.joda.time.PeriodType >= 0 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotReadableInstant >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
boolean' = boolean+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
boolean' = boolean+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
boolean' = boolean+1;

orgdotjodadottimedotReadableInstant >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
boolean' = boolean+1;

orgdotjodadottimedotReadableInstant >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int+1;

orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
long' = long+1;

orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

orgdotjodadottimedotReadablePartial >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
boolean' = boolean+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,int >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int+0;

orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotReadablePartial >= 1,int >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotReadablePartial >= 1,int >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotSeconds >= 2 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-2,
boolean' = boolean+1;

orgdotjodadottimedotSeconds >= 2 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-2,
boolean' = boolean+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
int' = int+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotSeconds >= 1,int >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+0,
int' = int-1;

orgdotjodadottimedotSeconds >= 1,int >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+0,
int' = int-1;

orgdotjodadottimedotSeconds >= 2 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1;

orgdotjodadottimedotSeconds >= 1,int >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+0,
int' = int-1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotSeconds >= 1,int >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+0,
int' = int-1;

orgdotjodadottimedotSeconds >= 2 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds-1,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
int' = int+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
int' = int+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
int' = int+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
int' = int+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
int' = int+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
int' = int+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
int' = int-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1;

orgdotjodadottimedotTimeOfDay >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1,int >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+0,
int' = int-1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty+1;

orgdotjodadottimedotTimeOfDay >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty+1;

org.joda.time.TimeOfDay >= 0 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

int >= 2 ->
int' = int-2,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

int >= 3 ->
int' = int-3,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

int >= 4 ->
int' = int-4,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotChronology >= 1,int >= 3 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-3,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotChronology >= 1,int >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-2,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotUTCDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotUTCDateTimeZone >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
boolean' = boolean+1;

orgdotjodadottimedotUTCDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotUTCDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotUTCDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotUTCDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotUTCDateTimeZone >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
javadotutildotTimeZone' = javadotutildotTimeZone+1;

orgdotjodadottimedotUTCDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
long' = long+0;

orgdotjodadottimedotUTCDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone-1,
long' = long+0;

org.joda.time.UTCDateTimeZone >= 0 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone+1;

orgdotjodadottimedotWeeks >= 2 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-2,
boolean' = boolean+1;

orgdotjodadottimedotWeeks >= 2 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-2,
boolean' = boolean+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
int' = int+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1,
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotWeeks >= 1,int >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+0,
int' = int-1;

orgdotjodadottimedotWeeks >= 1,int >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+0,
int' = int-1;

orgdotjodadottimedotWeeks >= 2 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1;

orgdotjodadottimedotWeeks >= 1,int >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+0,
int' = int-1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotWeeks >= 1,int >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+0,
int' = int-1;

orgdotjodadottimedotWeeks >= 2 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
int' = int+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
int' = int-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
int' = int-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
int' = int-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
int' = int+1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
int' = int+1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
int' = int+1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
int' = int-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket+1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
int' = int-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
int' = int-1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
int' = int-1;

org.joda.time.YearMonth >= 0 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
int' = int-1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
int' = int-1;

orgdotjodadottimedotYearMonth >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1;

orgdotjodadottimedotYearMonth >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotDurationFieldType >= 1,orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
int' = int-1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
int' = int-1;

orgdotjodadottimedotYearMonth >= 1,int >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+0,
int' = int-1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty+1;

orgdotjodadottimedotYearMonth >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty+1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth-1,
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty+1;

org.joda.time.YearMonth >= 0 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

int >= 2 ->
int' = int-2,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotChronology >= 1,int >= 2 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-2,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
int' = int+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
int' = int-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

javadotutildotLocale >= 1,orgdotjodadottimedotYearMonthDaydollarProperty >= 1,javadotlangdotString >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
int' = int+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
int' = int+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
int' = int+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
int' = int+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotYearMonthDay >= 1,orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotYearMonthDay >= 1,orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
int' = int-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1,int >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+0,
int' = int-1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty+1;

orgdotjodadottimedotYearMonthDay >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay-1,
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty+1;

org.joda.time.YearMonthDay >= 0 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

int >= 3 ->
int' = int-3,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotChronology >= 1,int >= 3 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
int' = int-3,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYears >= 2 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears-2,
boolean' = boolean+1;

orgdotjodadottimedotYears >= 2 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears-2,
boolean' = boolean+1;

orgdotjodadottimedotYears >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears-1,
int' = int+1;

orgdotjodadottimedotYears >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotYears >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotYears >= 1,int >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+0,
int' = int-1;

orgdotjodadottimedotYears >= 1,int >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+0,
int' = int-1;

orgdotjodadottimedotYears >= 2 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears-1;

orgdotjodadottimedotYears >= 1,int >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+0,
int' = int-1;

orgdotjodadottimedotYears >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+1;

orgdotjodadottimedotYears >= 1,int >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+0,
int' = int-1;

orgdotjodadottimedotYears >= 2 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears-1;

int >= 1 ->
int' = int-1,
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+1;

orgdotjodadottimedotReadableInstant >= 2 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-2,
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+1;

orgdotjodadottimedotReadablePartial >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-2,
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
int' = int+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1,javadotutildotLocale >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotutildotCalendar' = javadotutildotCalendar+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime-1,
javadotutildotGregorianCalendar' = javadotutildotGregorianCalendar+1;

orgdotjodadottimedotbasedotBaseDateTime >= 1 ->
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime-1,
long' = long+1;

orgdotjodadottimedotbasedotBaseDateTime >= 1 ->
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.base.BaseDateTime >= 0 ->
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

long >= 1 ->
long' = long-1,
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

orgdotjodadottimedotChronology >= 1,long >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

javadotlangdotObject >= 1,orgdotjodadottimedotchronodotBuddhistChronology >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotchronodotBuddhistChronology' = orgdotjodadottimedotchronodotBuddhistChronology-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotBuddhistChronology >= 1 ->
orgdotjodadottimedotchronodotBuddhistChronology' = orgdotjodadottimedotchronodotBuddhistChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotBuddhistChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotBuddhistChronology' = orgdotjodadottimedotchronodotBuddhistChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.chrono.BuddhistChronology >= 0 ->
orgdotjodadottimedotchronodotBuddhistChronology' = orgdotjodadottimedotchronodotBuddhistChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotBuddhistChronology' = orgdotjodadottimedotchronodotBuddhistChronology+1;

org.joda.time.chrono.BuddhistChronology >= 0 ->
orgdotjodadottimedotchronodotBuddhistChronology' = orgdotjodadottimedotchronodotBuddhistChronology+1;

orgdotjodadottimedotchronodotCopticChronology >= 1 ->
orgdotjodadottimedotchronodotCopticChronology' = orgdotjodadottimedotchronodotCopticChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotCopticChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotCopticChronology' = orgdotjodadottimedotchronodotCopticChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.chrono.CopticChronology >= 0 ->
orgdotjodadottimedotchronodotCopticChronology' = orgdotjodadottimedotchronodotCopticChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotCopticChronology' = orgdotjodadottimedotchronodotCopticChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
int' = int-1,
orgdotjodadottimedotchronodotCopticChronology' = orgdotjodadottimedotchronodotCopticChronology+1;

org.joda.time.chrono.CopticChronology >= 0 ->
orgdotjodadottimedotchronodotCopticChronology' = orgdotjodadottimedotchronodotCopticChronology+1;

orgdotjodadottimedotchronodotEthiopicChronology >= 1 ->
orgdotjodadottimedotchronodotEthiopicChronology' = orgdotjodadottimedotchronodotEthiopicChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotEthiopicChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotEthiopicChronology' = orgdotjodadottimedotchronodotEthiopicChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.chrono.EthiopicChronology >= 0 ->
orgdotjodadottimedotchronodotEthiopicChronology' = orgdotjodadottimedotchronodotEthiopicChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotEthiopicChronology' = orgdotjodadottimedotchronodotEthiopicChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
int' = int-1,
orgdotjodadottimedotchronodotEthiopicChronology' = orgdotjodadottimedotchronodotEthiopicChronology+1;

org.joda.time.chrono.EthiopicChronology >= 0 ->
orgdotjodadottimedotchronodotEthiopicChronology' = orgdotjodadottimedotchronodotEthiopicChronology+1;

orgdotjodadottimedotchronodotGJChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotGJChronology >= 1 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology-1,
int' = int+1;

orgdotjodadottimedotchronodotGJChronology >= 1,int >= 4 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology-1,
int' = int-4,
long' = long+1;

orgdotjodadottimedotchronodotGJChronology >= 1 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotchronodotGJChronology >= 1,orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotchronodotGJChronology >= 1 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedotchronodotGJChronology >= 1 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology-1,
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

org.joda.time.chrono.GJChronology >= 0 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
long' = long-1,
int' = int-1,
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotReadableInstant >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
int' = int-1,
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology+1;

org.joda.time.chrono.GJChronology >= 0 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology+1;

orgdotjodadottimedotchronodotGregorianChronology >= 1 ->
orgdotjodadottimedotchronodotGregorianChronology' = orgdotjodadottimedotchronodotGregorianChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotGregorianChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotGregorianChronology' = orgdotjodadottimedotchronodotGregorianChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.chrono.GregorianChronology >= 0 ->
orgdotjodadottimedotchronodotGregorianChronology' = orgdotjodadottimedotchronodotGregorianChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotGregorianChronology' = orgdotjodadottimedotchronodotGregorianChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
int' = int-1,
orgdotjodadottimedotchronodotGregorianChronology' = orgdotjodadottimedotchronodotGregorianChronology+1;

org.joda.time.chrono.GregorianChronology >= 0 ->
orgdotjodadottimedotchronodotGregorianChronology' = orgdotjodadottimedotchronodotGregorianChronology+1;

orgdotjodadottimedotchronodotISOChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotchronodotISOChronology' = orgdotjodadottimedotchronodotISOChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotISOChronology >= 1 ->
orgdotjodadottimedotchronodotISOChronology' = orgdotjodadottimedotchronodotISOChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotISOChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotISOChronology' = orgdotjodadottimedotchronodotISOChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.chrono.ISOChronology >= 0 ->
orgdotjodadottimedotchronodotISOChronology' = orgdotjodadottimedotchronodotISOChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotISOChronology' = orgdotjodadottimedotchronodotISOChronology+1;

org.joda.time.chrono.ISOChronology >= 0 ->
orgdotjodadottimedotchronodotISOChronology' = orgdotjodadottimedotchronodotISOChronology+1;

orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType' = orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotIslamicChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotIslamicChronology >= 1 ->
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotIslamicChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.chrono.IslamicChronology >= 0 ->
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType' = orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType-1,
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology+1;

org.joda.time.chrono.IslamicChronology >= 0 ->
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology+1;

orgdotjodadottimedotchronodotIslamicChronology >= 1 ->
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology-1,
orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType' = orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType+1;

orgdotjodadottimedotchronodotJulianChronology >= 1 ->
orgdotjodadottimedotchronodotJulianChronology' = orgdotjodadottimedotchronodotJulianChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotJulianChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotJulianChronology' = orgdotjodadottimedotchronodotJulianChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

org.joda.time.chrono.JulianChronology >= 0 ->
orgdotjodadottimedotchronodotJulianChronology' = orgdotjodadottimedotchronodotJulianChronology+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotJulianChronology' = orgdotjodadottimedotchronodotJulianChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
int' = int-1,
orgdotjodadottimedotchronodotJulianChronology' = orgdotjodadottimedotchronodotJulianChronology+1;

org.joda.time.chrono.JulianChronology >= 0 ->
orgdotjodadottimedotchronodotJulianChronology' = orgdotjodadottimedotchronodotJulianChronology+1;

orgdotjodadottimedotchronodotLenientChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotchronodotLenientChronology' = orgdotjodadottimedotchronodotLenientChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotLenientChronology >= 1 ->
orgdotjodadottimedotchronodotLenientChronology' = orgdotjodadottimedotchronodotLenientChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotLenientChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotLenientChronology' = orgdotjodadottimedotchronodotLenientChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotchronodotLenientChronology' = orgdotjodadottimedotchronodotLenientChronology+1;

orgdotjodadottimedotchronodotStrictChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotchronodotStrictChronology' = orgdotjodadottimedotchronodotStrictChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotStrictChronology >= 1 ->
orgdotjodadottimedotchronodotStrictChronology' = orgdotjodadottimedotchronodotStrictChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotchronodotStrictChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotchronodotStrictChronology' = orgdotjodadottimedotchronodotStrictChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotchronodotStrictChronology' = orgdotjodadottimedotchronodotStrictChronology+1;

orgdotjodadottimedotchronodotZonedChronology >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotchronodotZonedChronology' = orgdotjodadottimedotchronodotZonedChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotchronodotZonedChronology >= 1,int >= 4 ->
orgdotjodadottimedotchronodotZonedChronology' = orgdotjodadottimedotchronodotZonedChronology-1,
int' = int-4,
long' = long+1;

orgdotjodadottimedotchronodotZonedChronology >= 1 ->
orgdotjodadottimedotchronodotZonedChronology' = orgdotjodadottimedotchronodotZonedChronology-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotchronodotZonedChronology >= 1,orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotchronodotZonedChronology' = orgdotjodadottimedotchronodotZonedChronology-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotchronodotZonedChronology >= 1 ->
orgdotjodadottimedotchronodotZonedChronology' = orgdotjodadottimedotchronodotZonedChronology-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotchronodotZonedChronology' = orgdotjodadottimedotchronodotZonedChronology+1;

org.joda.time.convert.ConverterManager >= 0 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager+1;

orgdotjodadottimedotconvertdotDurationConverter >= 1,orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotDurationConverter' = orgdotjodadottimedotconvertdotDurationConverter+0,
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1;

orgdotjodadottimedotconvertdotConverterManager >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotDurationConverter' = orgdotjodadottimedotconvertdotDurationConverter+1;

orgdotjodadottimedotconvertdotDurationConverter >= 1,orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotDurationConverter' = orgdotjodadottimedotconvertdotDurationConverter+0,
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1;

orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotDurationConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotDurationConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotConverterManager >= 1,orgdotjodadottimedotconvertdotInstantConverter >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotInstantConverter' = orgdotjodadottimedotconvertdotInstantConverter+0;

orgdotjodadottimedotconvertdotConverterManager >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotInstantConverter' = orgdotjodadottimedotconvertdotInstantConverter+1;

orgdotjodadottimedotconvertdotConverterManager >= 1,orgdotjodadottimedotconvertdotInstantConverter >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotInstantConverter' = orgdotjodadottimedotconvertdotInstantConverter+0;

orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotInstantConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotInstantConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotConverterManager >= 1,orgdotjodadottimedotconvertdotIntervalConverter >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotIntervalConverter' = orgdotjodadottimedotconvertdotIntervalConverter+0;

orgdotjodadottimedotconvertdotConverterManager >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotIntervalConverter' = orgdotjodadottimedotconvertdotIntervalConverter+1;

orgdotjodadottimedotconvertdotConverterManager >= 1,orgdotjodadottimedotconvertdotIntervalConverter >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotIntervalConverter' = orgdotjodadottimedotconvertdotIntervalConverter+0;

orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotIntervalConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotIntervalConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotConverterManager >= 1,orgdotjodadottimedotconvertdotPartialConverter >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter+0;

orgdotjodadottimedotconvertdotConverterManager >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter+1;

orgdotjodadottimedotconvertdotConverterManager >= 1,orgdotjodadottimedotconvertdotPartialConverter >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter+0;

orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotPartialConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotPartialConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotPeriodConverter >= 1,orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotPeriodConverter' = orgdotjodadottimedotconvertdotPeriodConverter+0,
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1;

orgdotjodadottimedotconvertdotConverterManager >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotPeriodConverter' = orgdotjodadottimedotconvertdotPeriodConverter+1;

orgdotjodadottimedotconvertdotPeriodConverter >= 1,orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotPeriodConverter' = orgdotjodadottimedotconvertdotPeriodConverter+0,
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1;

orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager-1,
orgdotjodadottimedotconvertdotPeriodConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotPeriodConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotDurationConverter >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotconvertdotDurationConverter' = orgdotjodadottimedotconvertdotDurationConverter-1,
javadotlangdotObject' = javadotlangdotObject-1,
long' = long+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotInstantConverter >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotInstantConverter' = orgdotjodadottimedotconvertdotInstantConverter-1,
long' = long+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotInstantConverter >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+0,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotInstantConverter' = orgdotjodadottimedotconvertdotInstantConverter-1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotInstantConverter >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotInstantConverter' = orgdotjodadottimedotconvertdotInstantConverter-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotIntervalConverter >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotIntervalConverter' = orgdotjodadottimedotconvertdotIntervalConverter-1,
boolean' = boolean+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotPartialConverter >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter-1,
intleftbracketrightbracket' = intleftbracketrightbracket+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotPartialConverter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter-1,
intleftbracketrightbracket' = intleftbracketrightbracket+1;

orgdotjodadottimedotChronology >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotPartialConverter >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+0,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter-1;

orgdotjodadottimedotDateTimeZone >= 1,javadotlangdotObject >= 1,orgdotjodadottimedotconvertdotPartialConverter >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotconvertdotPeriodConverter >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotconvertdotPeriodConverter' = orgdotjodadottimedotconvertdotPeriodConverter-1,
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
int' = int+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
int' = int+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
long' = long+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
long' = long+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty-1,
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

org.joda.time.field.AbstractReadableInstantFieldProperty >= 0 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty+1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1,int >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
int' = int-1,
long' = long+0;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
long' = long+1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDurationFieldType >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
int' = int+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,javadotlangdotString >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
javadotlangdotString' = javadotlangdotString-1,
long' = long+0;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1,javadotlangdotString >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
javadotlangdotString' = javadotlangdotString-1,
long' = long+0;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField+1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField+1;

javadotlangdotObject >= 2 ->
javadotlangdotObject' = javadotlangdotObject-2,
boolean' = boolean+1;

int >= 3 ->
int' = int-2;

int >= 4 ->
int' = int-3;

int >= 2 ->
int' = int-1;

int >= 2 ->
int' = int-1;

long >= 2 ->
long' = long-2,
int' = int+1;

int >= 1 ->
int' = int+0;

long >= 1 ->
long' = long-1,
int' = int+1;

long >= 2 ->
long' = long-1;

long >= 2 ->
long' = long-1;

long >= 1,int >= 1 ->
long' = long+0,
int' = int-1;

long >= 2 ->
long' = long-1;

long >= 2 ->
long' = long-1;

javadotlangdotString >= 1,int >= 3 ->
javadotlangdotString' = javadotlangdotString-1,
int' = int-3;

orgdotjodadottimedotDateTimeField >= 1,int >= 3 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-3;

orgdotjodadottimedotDateTimeFieldType >= 1,int >= 3 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-3;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeFieldType >= 1,long >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
long' = long-1,
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+0;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeField >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-1,
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField+1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField+1;

long >= 1,orgdotjodadottimedotfielddotPreciseDateTimeField >= 1 ->
long' = long-1,
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotPreciseDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotPreciseDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField-1,
int' = int+1;

long >= 1,int >= 1,orgdotjodadottimedotfielddotPreciseDateTimeField >= 1 ->
long' = long+0,
int' = int-1,
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField-1;

long >= 1,int >= 1,orgdotjodadottimedotfielddotPreciseDateTimeField >= 1 ->
long' = long+0,
int' = int-1,
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField-1;

orgdotjodadottimedotfielddotPreciseDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDurationField >= 2,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-2,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField+1;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
long' = long+1;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField+1;

javadotlangdotObject >= 1,orgdotjodadottimedotfielddotPreciseDurationField >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1,int >= 1,long >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
int' = int-1,
long' = long+0;

orgdotjodadottimedotfielddotPreciseDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
long' = long+1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField-1,
long' = long-1;

orgdotjodadottimedotDurationFieldType >= 1,long >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
long' = long-1,
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
int' = int-1,
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField+1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField-1,
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
int' = int+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,int >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
int' = int-1,
long' = long+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,int >= 1,long >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
int' = int-1,
long' = long+0;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long+0;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotScaledDurationField >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long+0;

orgdotjodadottimedotfielddotScaledDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField-1,
long' = long-1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDurationFieldType >= 1,int >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
int' = int-1,
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField+1;

orgdotjodadottimedotfielddotSkipDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotSkipDateTimeField' = orgdotjodadottimedotfielddotSkipDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotSkipDateTimeField >= 1 ->
orgdotjodadottimedotfielddotSkipDateTimeField' = orgdotjodadottimedotfielddotSkipDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotSkipDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotSkipDateTimeField' = orgdotjodadottimedotfielddotSkipDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotfielddotSkipDateTimeField' = orgdotjodadottimedotfielddotSkipDateTimeField+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeField >= 1,int >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-1,
orgdotjodadottimedotfielddotSkipDateTimeField' = orgdotjodadottimedotfielddotSkipDateTimeField+1;

orgdotjodadottimedotfielddotSkipUndoDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotSkipUndoDateTimeField' = orgdotjodadottimedotfielddotSkipUndoDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotSkipUndoDateTimeField >= 1 ->
orgdotjodadottimedotfielddotSkipUndoDateTimeField' = orgdotjodadottimedotfielddotSkipUndoDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotSkipUndoDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotSkipUndoDateTimeField' = orgdotjodadottimedotfielddotSkipUndoDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotfielddotSkipUndoDateTimeField' = orgdotjodadottimedotfielddotSkipUndoDateTimeField+1;

orgdotjodadottimedotChronology >= 1,orgdotjodadottimedotDateTimeField >= 1,int >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
int' = int-1,
orgdotjodadottimedotfielddotSkipUndoDateTimeField' = orgdotjodadottimedotfielddotSkipUndoDateTimeField+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int+1;

orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-2;

orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-2;

orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-2;

orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 2 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-2;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,int >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
int' = int-1,
javadotlangdotString' = javadotlangdotString+1;

javadotutildotLocale >= 1,orgdotjodadottimedotReadablePartial >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0,
int' = int-1;

javadotlangdotString >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

javadotutildotLocale >= 1,javadotlangdotString >= 1,orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDurationField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField+1;

javadotlangdotObject >= 1,orgdotjodadottimedotfielddotUnsupportedDurationField >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,int >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
int' = int-1,
long' = long+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,int >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
int' = int-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long+0;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
long' = long-1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField-1,
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotDurationFieldType >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType-1,
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-1,
boolean' = boolean+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-2,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket-1,
int' = int+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,orgdotjodadottimedotReadablePartial >= 1,intleftbracketrightbracket >= 1,int >= 2 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
intleftbracketrightbracket' = intleftbracketrightbracket+0,
int' = int-2;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 2 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long-1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1,long >= 1,int >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
long' = long+0,
int' = int-1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField-1,
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField+1;

orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey >= 1,javadotlangdotObject >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey' = orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey-1,
javadotlangdotObject' = javadotlangdotObject-1,
boolean' = boolean+1;

javadotutildotLocale >= 1,int >= 3 ->
javadotutildotLocale' = javadotutildotLocale-1,
int' = int-3,
orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey' = orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey+1;

javadotutildotLocale >= 1,javadotlangdotString >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString+0;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
boolean' = boolean+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
boolean' = boolean+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
boolean' = boolean+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
int' = int+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotInteger' = javadotlangdotInteger+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,long >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotutildotLocale' = javadotutildotLocale+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
long' = long+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,int >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0,
int' = int-1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotutildotLocale >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0,
javadotutildotLocale' = javadotutildotLocale-1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,int >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0,
int' = int-1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1,javadotlangdotInteger >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0,
javadotlangdotInteger' = javadotlangdotInteger-1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+0;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotformatdotDateTimeParser' = orgdotjodadottimedotformatdotDateTimeParser+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter-1,
orgdotjodadottimedotformatdotDateTimePrinter' = orgdotjodadottimedotformatdotDateTimePrinter+1;

orgdotjodadottimedotformatdotDateTimeParser >= 1,orgdotjodadottimedotformatdotDateTimePrinter >= 1 ->
orgdotjodadottimedotformatdotDateTimeParser' = orgdotjodadottimedotformatdotDateTimeParser-1,
orgdotjodadottimedotformatdotDateTimePrinter' = orgdotjodadottimedotformatdotDateTimePrinter-1,
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

orgdotjodadottimedotformatdotDateTimeParser >= 1 ->
orgdotjodadottimedotformatdotDateTimeParser' = orgdotjodadottimedotformatdotDateTimeParser-1,
int' = int+1;

orgdotjodadottimedotformatdotDateTimeParser >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,javadotlangdotString >= 1,int >= 1 ->
orgdotjodadottimedotformatdotDateTimeParser' = orgdotjodadottimedotformatdotDateTimeParser-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotString' = javadotlangdotString-1,
int' = int+0;

javadotlangdotObject >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
boolean' = boolean+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
int' = int+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotInteger' = javadotlangdotInteger+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotInteger' = javadotlangdotInteger+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotObject' = javadotlangdotObject+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotutildotLocale' = javadotutildotLocale+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
long' = long+1;

boolean >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
boolean' = boolean-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
long' = long+1;

boolean >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotString' = javadotlangdotString-1,
long' = long+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

javadotutildotLocale >= 1,orgdotjodadottimedotChronology >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
long' = long-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket+1;

javadotutildotLocale >= 1,orgdotjodadottimedotChronology >= 1,javadotlangdotInteger >= 1,long >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology-1,
javadotlangdotInteger' = javadotlangdotInteger-1,
long' = long-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1;

orgdotjodadottimedotDateTimeField >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
int' = int-1;

orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,int >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
int' = int-1;

javadotutildotLocale >= 1,orgdotjodadottimedotDateTimeFieldType >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,javadotlangdotString >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,int >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
int' = int-1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,javadotlangdotInteger >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotInteger' = javadotlangdotInteger-1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1,javadotlangdotInteger >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1,
javadotlangdotInteger' = javadotlangdotInteger-1;

orgdotjodadottimedotDateTimeZone >= 1,orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket-1;

orgdotjodadottimedotformatdotDateTimePrinter >= 1 ->
orgdotjodadottimedotformatdotDateTimePrinter' = orgdotjodadottimedotformatdotDateTimePrinter-1,
int' = int+1;

long >= 1 ->
long' = long-1,
int' = int+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.DateTimeFormatter >= 0 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

org.joda.time.format.PeriodFormatter >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

org.joda.time.format.PeriodFormatter >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

org.joda.time.format.PeriodFormatter >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

org.joda.time.format.PeriodFormatter >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

org.joda.time.format.PeriodFormatter >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

org.joda.time.format.PeriodFormatter >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

org.joda.time.format.PeriodFormatter >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

javadotutildotLocale >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
boolean' = boolean+1;

orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
boolean' = boolean+1;

orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
javadotutildotLocale' = javadotutildotLocale+1;

javadotlangdotString >= 1,orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

javadotlangdotString >= 1,orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

javadotutildotLocale >= 1,orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+0;

orgdotjodadottimedotformatdotPeriodFormatter >= 1,orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+0,
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType-1;

orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
orgdotjodadottimedotformatdotPeriodParser' = orgdotjodadottimedotformatdotPeriodParser+1;

orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1,
orgdotjodadottimedotformatdotPeriodPrinter' = orgdotjodadottimedotformatdotPeriodPrinter+1;

orgdotjodadottimedotformatdotPeriodParser >= 1,orgdotjodadottimedotformatdotPeriodPrinter >= 1 ->
orgdotjodadottimedotformatdotPeriodParser' = orgdotjodadottimedotformatdotPeriodParser-1,
orgdotjodadottimedotformatdotPeriodPrinter' = orgdotjodadottimedotformatdotPeriodPrinter-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder-1,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,orgdotjodadottimedotformatdotPeriodParser >= 1,orgdotjodadottimedotformatdotPeriodPrinter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
orgdotjodadottimedotformatdotPeriodParser' = orgdotjodadottimedotformatdotPeriodParser-1,
orgdotjodadottimedotformatdotPeriodPrinter' = orgdotjodadottimedotformatdotPeriodPrinter-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 2 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-2;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 2 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-2;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,javadotlangdotString >= 2 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
javadotlangdotString' = javadotlangdotString-2;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,int >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
int' = int-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1,int >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0,
int' = int-1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

boolean >= 1,orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
boolean' = boolean-1,
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+0;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder-1,
orgdotjodadottimedotformatdotPeriodParser' = orgdotjodadottimedotformatdotPeriodParser+1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder-1,
orgdotjodadottimedotformatdotPeriodPrinter' = orgdotjodadottimedotformatdotPeriodPrinter+1;

org.joda.time.format.PeriodFormatterBuilder >= 0 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder-1;

javadotlangdotObject >= 1,orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1,
boolean' = boolean+1;

orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1,
boolean' = boolean+1;

long >= 1,orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
long' = long-1,
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1,
int' = int+1;

long >= 1,orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
long' = long-1,
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1,
int' = int+1;

long >= 1,orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
long' = long-1,
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1,
javadotlangdotString' = javadotlangdotString+1;

long >= 1,orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
long' = long+0,
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1;

long >= 1,orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
long' = long+0,
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1;

orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone+1;

boolean >= 1,orgdotjodadottimedottzdotDateTimeZoneBuilder >= 1,javadotlangdotString >= 1 ->
boolean' = boolean-1,
orgdotjodadottimedottzdotDateTimeZoneBuilder' = orgdotjodadottimedottzdotDateTimeZoneBuilder-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedottzdotDateTimeZoneBuilder >= 1,javadotlangdotString >= 1,int >= 1 ->
orgdotjodadottimedottzdotDateTimeZoneBuilder' = orgdotjodadottimedottzdotDateTimeZoneBuilder+0,
javadotlangdotString' = javadotlangdotString-1,
int' = int-1;

orgdotjodadottimedottzdotDateTimeZoneBuilder >= 1,int >= 1 ->
orgdotjodadottimedottzdotDateTimeZoneBuilder' = orgdotjodadottimedottzdotDateTimeZoneBuilder+0,
int' = int-1;

org.joda.time.tz.DateTimeZoneBuilder >= 0 ->
orgdotjodadottimedottzdotDateTimeZoneBuilder' = orgdotjodadottimedottzdotDateTimeZoneBuilder+1;

javadotutildotLocale >= 1,javadotlangdotString >= 2,orgdotjodadottimedottzdotDefaultNameProvider >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedottzdotDefaultNameProvider' = orgdotjodadottimedottzdotDefaultNameProvider-1;

boolean >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 2,orgdotjodadottimedottzdotDefaultNameProvider >= 1 ->
boolean' = boolean-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedottzdotDefaultNameProvider' = orgdotjodadottimedottzdotDefaultNameProvider-1;

javadotutildotLocale >= 1,javadotlangdotString >= 2,orgdotjodadottimedottzdotDefaultNameProvider >= 1 ->
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedottzdotDefaultNameProvider' = orgdotjodadottimedottzdotDefaultNameProvider-1;

boolean >= 1,javadotutildotLocale >= 1,javadotlangdotString >= 2,orgdotjodadottimedottzdotDefaultNameProvider >= 1 ->
boolean' = boolean-1,
javadotutildotLocale' = javadotutildotLocale-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedottzdotDefaultNameProvider' = orgdotjodadottimedottzdotDefaultNameProvider-1;

org.joda.time.tz.DefaultNameProvider >= 0 ->
orgdotjodadottimedottzdotDefaultNameProvider' = orgdotjodadottimedottzdotDefaultNameProvider+1;

javadotlangdotObject >= 1,orgdotjodadottimedottzdotFixedDateTimeZone >= 1 ->
javadotlangdotObject' = javadotlangdotObject-1,
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
boolean' = boolean+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
boolean' = boolean+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
long' = long-1,
int' = int+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
long' = long-1,
javadotlangdotString' = javadotlangdotString+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
javadotutildotTimeZone' = javadotutildotTimeZone+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
long' = long+0;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1,long >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone-1,
long' = long+0;

javadotlangdotString >= 2,int >= 2 ->
javadotlangdotString' = javadotlangdotString-2,
int' = int-2,
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone+1;

javadotutildotLocale >= 1,orgdotjodadottimedottzdotNameProvider >= 1,javadotlangdotString >= 2 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedottzdotNameProvider' = orgdotjodadottimedottzdotNameProvider-1,
javadotlangdotString' = javadotlangdotString-1;

javadotutildotLocale >= 1,orgdotjodadottimedottzdotNameProvider >= 1,javadotlangdotString >= 2 ->
javadotutildotLocale' = javadotutildotLocale-1,
orgdotjodadottimedottzdotNameProvider' = orgdotjodadottimedottzdotNameProvider-1,
javadotlangdotString' = javadotlangdotString-1;

orgdotjodadottimedottzdotProvider >= 1 ->
orgdotjodadottimedottzdotProvider' = orgdotjodadottimedottzdotProvider-1,
javadotutildotSet' = javadotutildotSet+1;

orgdotjodadottimedottzdotProvider >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedottzdotProvider' = orgdotjodadottimedottzdotProvider-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedottzdotUTCProvider >= 1 ->
orgdotjodadottimedottzdotUTCProvider' = orgdotjodadottimedottzdotUTCProvider-1,
javadotutildotSet' = javadotutildotSet+1;

orgdotjodadottimedottzdotUTCProvider >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedottzdotUTCProvider' = orgdotjodadottimedottzdotUTCProvider-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

org.joda.time.tz.UTCProvider >= 0 ->
orgdotjodadottimedottzdotUTCProvider' = orgdotjodadottimedottzdotUTCProvider+1;

org.joda.time.tz.ZoneInfoCompiler >= 0 ->
orgdotjodadottimedottzdotZoneInfoCompiler' = orgdotjodadottimedottzdotZoneInfoCompiler+1;

boolean >= 0 ->
boolean' = boolean+1;

org.joda.time.tz.ZoneInfoLogger >= 0 ->
orgdotjodadottimedottzdotZoneInfoLogger' = orgdotjodadottimedottzdotZoneInfoLogger+1;

boolean >= 1 ->
boolean' = boolean-1;

orgdotjodadottimedottzdotZoneInfoProvider >= 1 ->
orgdotjodadottimedottzdotZoneInfoProvider' = orgdotjodadottimedottzdotZoneInfoProvider-1,
javadotutildotSet' = javadotutildotSet+1;

orgdotjodadottimedottzdotZoneInfoProvider >= 1,javadotlangdotString >= 1 ->
orgdotjodadottimedottzdotZoneInfoProvider' = orgdotjodadottimedottzdotZoneInfoProvider-1,
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString-1,
orgdotjodadottimedottzdotZoneInfoProvider' = orgdotjodadottimedottzdotZoneInfoProvider+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate-1,
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime-1,
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty-1,
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone-1,
orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId' = orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId+1;

orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId' = orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId-1,
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

boolean >= 1 ->
boolean' = boolean+1;

double >= 1 ->
double' = double+1;

int >= 1 ->
int' = int+1;

intleftbracketrightbracket >= 1 ->
intleftbracketrightbracket' = intleftbracketrightbracket+1;

javadotlangdotInteger >= 1 ->
javadotlangdotInteger' = javadotlangdotInteger+1;

javadotlangdotNumber >= 1 ->
javadotlangdotNumber' = javadotlangdotNumber+1;

javadotlangdotObject >= 1 ->
javadotlangdotObject' = javadotlangdotObject+1;

javadotlangdotString >= 1 ->
javadotlangdotString' = javadotlangdotString+1;

javadottextdotDateFormatSymbols >= 1 ->
javadottextdotDateFormatSymbols' = javadottextdotDateFormatSymbols+1;

javadotutildotCalendar >= 1 ->
javadotutildotCalendar' = javadotutildotCalendar+1;

javadotutildotDate >= 1 ->
javadotutildotDate' = javadotutildotDate+1;

javadotutildotGregorianCalendar >= 1 ->
javadotutildotGregorianCalendar' = javadotutildotGregorianCalendar+1;

javadotutildotLocale >= 1 ->
javadotutildotLocale' = javadotutildotLocale+1;

javadotutildotMap >= 1 ->
javadotutildotMap' = javadotutildotMap+1;

javadotutildotSet >= 1 ->
javadotutildotSet' = javadotutildotSet+1;

javadotutildotTimeZone >= 1 ->
javadotutildotTimeZone' = javadotutildotTimeZone+1;

long >= 1 ->
long' = long+1;

orgdotjodadottimedotChronology >= 1 ->
orgdotjodadottimedotChronology' = orgdotjodadottimedotChronology+1;

orgdotjodadottimedotDateMidnight >= 1 ->
orgdotjodadottimedotDateMidnight' = orgdotjodadottimedotDateMidnight+1;

orgdotjodadottimedotDateMidnightdollarProperty >= 1 ->
orgdotjodadottimedotDateMidnightdollarProperty' = orgdotjodadottimedotDateMidnightdollarProperty+1;

orgdotjodadottimedotDateTime >= 1 ->
orgdotjodadottimedotDateTime' = orgdotjodadottimedotDateTime+1,
indicator_place_orgdotjodadottimedotDateTime' = indicator_place_orgdotjodadottimedotDateTime+1;

orgdotjodadottimedotDateTimedollarProperty >= 1 ->
orgdotjodadottimedotDateTimedollarProperty' = orgdotjodadottimedotDateTimedollarProperty+1;

orgdotjodadottimedotDateTimeComparator >= 1 ->
orgdotjodadottimedotDateTimeComparator' = orgdotjodadottimedotDateTimeComparator+1;

orgdotjodadottimedotDateTimeField >= 1 ->
orgdotjodadottimedotDateTimeField' = orgdotjodadottimedotDateTimeField+1;

orgdotjodadottimedotDateTimeFieldType >= 1 ->
orgdotjodadottimedotDateTimeFieldType' = orgdotjodadottimedotDateTimeFieldType+1;

orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket >= 1 ->
orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket' = orgdotjodadottimedotDateTimeFieldTypeleftbracketrightbracket+1;

orgdotjodadottimedotDateTimeZone >= 1 ->
orgdotjodadottimedotDateTimeZone' = orgdotjodadottimedotDateTimeZone+1;

orgdotjodadottimedotDays >= 1 ->
orgdotjodadottimedotDays' = orgdotjodadottimedotDays+1;

orgdotjodadottimedotDuration >= 1 ->
orgdotjodadottimedotDuration' = orgdotjodadottimedotDuration+1;

orgdotjodadottimedotDurationField >= 1 ->
orgdotjodadottimedotDurationField' = orgdotjodadottimedotDurationField+1;

orgdotjodadottimedotDurationFieldType >= 1 ->
orgdotjodadottimedotDurationFieldType' = orgdotjodadottimedotDurationFieldType+1;

orgdotjodadottimedotHours >= 1 ->
orgdotjodadottimedotHours' = orgdotjodadottimedotHours+1;

orgdotjodadottimedotIllegalFieldValueException >= 1 ->
orgdotjodadottimedotIllegalFieldValueException' = orgdotjodadottimedotIllegalFieldValueException+1;

orgdotjodadottimedotIllegalInstantException >= 1 ->
orgdotjodadottimedotIllegalInstantException' = orgdotjodadottimedotIllegalInstantException+1;

orgdotjodadottimedotInstant >= 1 ->
orgdotjodadottimedotInstant' = orgdotjodadottimedotInstant+1;

orgdotjodadottimedotInterval >= 1 ->
orgdotjodadottimedotInterval' = orgdotjodadottimedotInterval+1;

orgdotjodadottimedotJodaTimePermission >= 1 ->
orgdotjodadottimedotJodaTimePermission' = orgdotjodadottimedotJodaTimePermission+1;

orgdotjodadottimedotLocalDate >= 1 ->
orgdotjodadottimedotLocalDate' = orgdotjodadottimedotLocalDate+1;

orgdotjodadottimedotLocalDatedollarProperty >= 1 ->
orgdotjodadottimedotLocalDatedollarProperty' = orgdotjodadottimedotLocalDatedollarProperty+1;

orgdotjodadottimedotLocalDateTime >= 1 ->
orgdotjodadottimedotLocalDateTime' = orgdotjodadottimedotLocalDateTime+1;

orgdotjodadottimedotLocalDateTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalDateTimedollarProperty' = orgdotjodadottimedotLocalDateTimedollarProperty+1;

orgdotjodadottimedotLocalTime >= 1 ->
orgdotjodadottimedotLocalTime' = orgdotjodadottimedotLocalTime+1;

orgdotjodadottimedotLocalTimedollarProperty >= 1 ->
orgdotjodadottimedotLocalTimedollarProperty' = orgdotjodadottimedotLocalTimedollarProperty+1;

orgdotjodadottimedotMinutes >= 1 ->
orgdotjodadottimedotMinutes' = orgdotjodadottimedotMinutes+1;

orgdotjodadottimedotMonthDay >= 1 ->
orgdotjodadottimedotMonthDay' = orgdotjodadottimedotMonthDay+1;

orgdotjodadottimedotMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotMonthDaydollarProperty' = orgdotjodadottimedotMonthDaydollarProperty+1;

orgdotjodadottimedotMonths >= 1 ->
orgdotjodadottimedotMonths' = orgdotjodadottimedotMonths+1;

orgdotjodadottimedotMutableDateTime >= 1 ->
orgdotjodadottimedotMutableDateTime' = orgdotjodadottimedotMutableDateTime+1;

orgdotjodadottimedotMutableDateTimedollarProperty >= 1 ->
orgdotjodadottimedotMutableDateTimedollarProperty' = orgdotjodadottimedotMutableDateTimedollarProperty+1;

orgdotjodadottimedotMutableInterval >= 1 ->
orgdotjodadottimedotMutableInterval' = orgdotjodadottimedotMutableInterval+1;

orgdotjodadottimedotMutablePeriod >= 1 ->
orgdotjodadottimedotMutablePeriod' = orgdotjodadottimedotMutablePeriod+1;

orgdotjodadottimedotPartial >= 1 ->
orgdotjodadottimedotPartial' = orgdotjodadottimedotPartial+1;

orgdotjodadottimedotPartialdollarProperty >= 1 ->
orgdotjodadottimedotPartialdollarProperty' = orgdotjodadottimedotPartialdollarProperty+1;

orgdotjodadottimedotPeriod >= 1 ->
orgdotjodadottimedotPeriod' = orgdotjodadottimedotPeriod+1;

orgdotjodadottimedotPeriodType >= 1 ->
orgdotjodadottimedotPeriodType' = orgdotjodadottimedotPeriodType+1;

orgdotjodadottimedotReadableInstant >= 1 ->
orgdotjodadottimedotReadableInstant' = orgdotjodadottimedotReadableInstant+1;

orgdotjodadottimedotReadablePartial >= 1 ->
orgdotjodadottimedotReadablePartial' = orgdotjodadottimedotReadablePartial+1;

orgdotjodadottimedotSeconds >= 1 ->
orgdotjodadottimedotSeconds' = orgdotjodadottimedotSeconds+1;

orgdotjodadottimedotTimeOfDay >= 1 ->
orgdotjodadottimedotTimeOfDay' = orgdotjodadottimedotTimeOfDay+1;

orgdotjodadottimedotTimeOfDaydollarProperty >= 1 ->
orgdotjodadottimedotTimeOfDaydollarProperty' = orgdotjodadottimedotTimeOfDaydollarProperty+1;

orgdotjodadottimedotUTCDateTimeZone >= 1 ->
orgdotjodadottimedotUTCDateTimeZone' = orgdotjodadottimedotUTCDateTimeZone+1;

orgdotjodadottimedotWeeks >= 1 ->
orgdotjodadottimedotWeeks' = orgdotjodadottimedotWeeks+1;

orgdotjodadottimedotYearMonth >= 1 ->
orgdotjodadottimedotYearMonth' = orgdotjodadottimedotYearMonth+1;

orgdotjodadottimedotYearMonthdollarProperty >= 1 ->
orgdotjodadottimedotYearMonthdollarProperty' = orgdotjodadottimedotYearMonthdollarProperty+1;

orgdotjodadottimedotYearMonthDay >= 1 ->
orgdotjodadottimedotYearMonthDay' = orgdotjodadottimedotYearMonthDay+1;

orgdotjodadottimedotYearMonthDaydollarProperty >= 1 ->
orgdotjodadottimedotYearMonthDaydollarProperty' = orgdotjodadottimedotYearMonthDaydollarProperty+1;

orgdotjodadottimedotYears >= 1 ->
orgdotjodadottimedotYears' = orgdotjodadottimedotYears+1;

orgdotjodadottimedotbasedotAbstractDateTime >= 1 ->
orgdotjodadottimedotbasedotAbstractDateTime' = orgdotjodadottimedotbasedotAbstractDateTime+1;

orgdotjodadottimedotbasedotBaseDateTime >= 1 ->
orgdotjodadottimedotbasedotBaseDateTime' = orgdotjodadottimedotbasedotBaseDateTime+1;

orgdotjodadottimedotchronodotBuddhistChronology >= 1 ->
orgdotjodadottimedotchronodotBuddhistChronology' = orgdotjodadottimedotchronodotBuddhistChronology+1;

orgdotjodadottimedotchronodotCopticChronology >= 1 ->
orgdotjodadottimedotchronodotCopticChronology' = orgdotjodadottimedotchronodotCopticChronology+1;

orgdotjodadottimedotchronodotEthiopicChronology >= 1 ->
orgdotjodadottimedotchronodotEthiopicChronology' = orgdotjodadottimedotchronodotEthiopicChronology+1;

orgdotjodadottimedotchronodotGJChronology >= 1 ->
orgdotjodadottimedotchronodotGJChronology' = orgdotjodadottimedotchronodotGJChronology+1;

orgdotjodadottimedotchronodotGregorianChronology >= 1 ->
orgdotjodadottimedotchronodotGregorianChronology' = orgdotjodadottimedotchronodotGregorianChronology+1;

orgdotjodadottimedotchronodotISOChronology >= 1 ->
orgdotjodadottimedotchronodotISOChronology' = orgdotjodadottimedotchronodotISOChronology+1;

orgdotjodadottimedotchronodotIslamicChronology >= 1 ->
orgdotjodadottimedotchronodotIslamicChronology' = orgdotjodadottimedotchronodotIslamicChronology+1;

orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType >= 1 ->
orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType' = orgdotjodadottimedotchronodotIslamicChronologydollarLeapYearPatternType+1;

orgdotjodadottimedotchronodotJulianChronology >= 1 ->
orgdotjodadottimedotchronodotJulianChronology' = orgdotjodadottimedotchronodotJulianChronology+1;

orgdotjodadottimedotchronodotLenientChronology >= 1 ->
orgdotjodadottimedotchronodotLenientChronology' = orgdotjodadottimedotchronodotLenientChronology+1;

orgdotjodadottimedotchronodotStrictChronology >= 1 ->
orgdotjodadottimedotchronodotStrictChronology' = orgdotjodadottimedotchronodotStrictChronology+1;

orgdotjodadottimedotchronodotZonedChronology >= 1 ->
orgdotjodadottimedotchronodotZonedChronology' = orgdotjodadottimedotchronodotZonedChronology+1;

orgdotjodadottimedotconvertdotConverterManager >= 1 ->
orgdotjodadottimedotconvertdotConverterManager' = orgdotjodadottimedotconvertdotConverterManager+1;

orgdotjodadottimedotconvertdotDurationConverter >= 1 ->
orgdotjodadottimedotconvertdotDurationConverter' = orgdotjodadottimedotconvertdotDurationConverter+1;

orgdotjodadottimedotconvertdotDurationConverterleftbracketrightbracket >= 1 ->
orgdotjodadottimedotconvertdotDurationConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotDurationConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotInstantConverter >= 1 ->
orgdotjodadottimedotconvertdotInstantConverter' = orgdotjodadottimedotconvertdotInstantConverter+1;

orgdotjodadottimedotconvertdotInstantConverterleftbracketrightbracket >= 1 ->
orgdotjodadottimedotconvertdotInstantConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotInstantConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotIntervalConverter >= 1 ->
orgdotjodadottimedotconvertdotIntervalConverter' = orgdotjodadottimedotconvertdotIntervalConverter+1;

orgdotjodadottimedotconvertdotIntervalConverterleftbracketrightbracket >= 1 ->
orgdotjodadottimedotconvertdotIntervalConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotIntervalConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotPartialConverter >= 1 ->
orgdotjodadottimedotconvertdotPartialConverter' = orgdotjodadottimedotconvertdotPartialConverter+1;

orgdotjodadottimedotconvertdotPartialConverterleftbracketrightbracket >= 1 ->
orgdotjodadottimedotconvertdotPartialConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotPartialConverterleftbracketrightbracket+1;

orgdotjodadottimedotconvertdotPeriodConverter >= 1 ->
orgdotjodadottimedotconvertdotPeriodConverter' = orgdotjodadottimedotconvertdotPeriodConverter+1;

orgdotjodadottimedotconvertdotPeriodConverterleftbracketrightbracket >= 1 ->
orgdotjodadottimedotconvertdotPeriodConverterleftbracketrightbracket' = orgdotjodadottimedotconvertdotPeriodConverterleftbracketrightbracket+1;

orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty >= 1 ->
orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty' = orgdotjodadottimedotfielddotAbstractReadableInstantFieldProperty+1;

orgdotjodadottimedotfielddotDecoratedDurationField >= 1 ->
orgdotjodadottimedotfielddotDecoratedDurationField' = orgdotjodadottimedotfielddotDecoratedDurationField+1;

orgdotjodadottimedotfielddotDelegatedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDelegatedDateTimeField' = orgdotjodadottimedotfielddotDelegatedDateTimeField+1;

orgdotjodadottimedotfielddotDividedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotDividedDateTimeField' = orgdotjodadottimedotfielddotDividedDateTimeField+1;

orgdotjodadottimedotfielddotImpreciseDateTimeField >= 1 ->
orgdotjodadottimedotfielddotImpreciseDateTimeField' = orgdotjodadottimedotfielddotImpreciseDateTimeField+1;

orgdotjodadottimedotfielddotOffsetDateTimeField >= 1 ->
orgdotjodadottimedotfielddotOffsetDateTimeField' = orgdotjodadottimedotfielddotOffsetDateTimeField+1;

orgdotjodadottimedotfielddotPreciseDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDateTimeField' = orgdotjodadottimedotfielddotPreciseDateTimeField+1;

orgdotjodadottimedotfielddotPreciseDurationDateTimeField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationDateTimeField' = orgdotjodadottimedotfielddotPreciseDurationDateTimeField+1;

orgdotjodadottimedotfielddotPreciseDurationField >= 1 ->
orgdotjodadottimedotfielddotPreciseDurationField' = orgdotjodadottimedotfielddotPreciseDurationField+1;

orgdotjodadottimedotfielddotRemainderDateTimeField >= 1 ->
orgdotjodadottimedotfielddotRemainderDateTimeField' = orgdotjodadottimedotfielddotRemainderDateTimeField+1;

orgdotjodadottimedotfielddotScaledDurationField >= 1 ->
orgdotjodadottimedotfielddotScaledDurationField' = orgdotjodadottimedotfielddotScaledDurationField+1;

orgdotjodadottimedotfielddotSkipDateTimeField >= 1 ->
orgdotjodadottimedotfielddotSkipDateTimeField' = orgdotjodadottimedotfielddotSkipDateTimeField+1;

orgdotjodadottimedotfielddotSkipUndoDateTimeField >= 1 ->
orgdotjodadottimedotfielddotSkipUndoDateTimeField' = orgdotjodadottimedotfielddotSkipUndoDateTimeField+1;

orgdotjodadottimedotfielddotUnsupportedDateTimeField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDateTimeField' = orgdotjodadottimedotfielddotUnsupportedDateTimeField+1;

orgdotjodadottimedotfielddotUnsupportedDurationField >= 1 ->
orgdotjodadottimedotfielddotUnsupportedDurationField' = orgdotjodadottimedotfielddotUnsupportedDurationField+1;

orgdotjodadottimedotfielddotZeroIsMaxDateTimeField >= 1 ->
orgdotjodadottimedotfielddotZeroIsMaxDateTimeField' = orgdotjodadottimedotfielddotZeroIsMaxDateTimeField+1;

orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey' = orgdotjodadottimedotformatdotDateTimeFormatdollarStyleFormatterCacheKey+1;

orgdotjodadottimedotformatdotDateTimeFormatter >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatter' = orgdotjodadottimedotformatdotDateTimeFormatter+1;

orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId >= 1 ->
orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId' = orgdotjodadottimedotformatdotDateTimeFormatterBuilderdollarTimeZoneId+1;

orgdotjodadottimedotformatdotDateTimeParser >= 1 ->
orgdotjodadottimedotformatdotDateTimeParser' = orgdotjodadottimedotformatdotDateTimeParser+1;

orgdotjodadottimedotformatdotDateTimeParserBucket >= 1 ->
orgdotjodadottimedotformatdotDateTimeParserBucket' = orgdotjodadottimedotformatdotDateTimeParserBucket+1;

orgdotjodadottimedotformatdotDateTimePrinter >= 1 ->
orgdotjodadottimedotformatdotDateTimePrinter' = orgdotjodadottimedotformatdotDateTimePrinter+1;

orgdotjodadottimedotformatdotPeriodFormatter >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatter' = orgdotjodadottimedotformatdotPeriodFormatter+1;

orgdotjodadottimedotformatdotPeriodFormatterBuilder >= 1 ->
orgdotjodadottimedotformatdotPeriodFormatterBuilder' = orgdotjodadottimedotformatdotPeriodFormatterBuilder+1;

orgdotjodadottimedotformatdotPeriodParser >= 1 ->
orgdotjodadottimedotformatdotPeriodParser' = orgdotjodadottimedotformatdotPeriodParser+1;

orgdotjodadottimedotformatdotPeriodPrinter >= 1 ->
orgdotjodadottimedotformatdotPeriodPrinter' = orgdotjodadottimedotformatdotPeriodPrinter+1;

orgdotjodadottimedottzdotCachedDateTimeZone >= 1 ->
orgdotjodadottimedottzdotCachedDateTimeZone' = orgdotjodadottimedottzdotCachedDateTimeZone+1;

orgdotjodadottimedottzdotDateTimeZoneBuilder >= 1 ->
orgdotjodadottimedottzdotDateTimeZoneBuilder' = orgdotjodadottimedottzdotDateTimeZoneBuilder+1;

orgdotjodadottimedottzdotDefaultNameProvider >= 1 ->
orgdotjodadottimedottzdotDefaultNameProvider' = orgdotjodadottimedottzdotDefaultNameProvider+1;

orgdotjodadottimedottzdotFixedDateTimeZone >= 1 ->
orgdotjodadottimedottzdotFixedDateTimeZone' = orgdotjodadottimedottzdotFixedDateTimeZone+1;

orgdotjodadottimedottzdotNameProvider >= 1 ->
orgdotjodadottimedottzdotNameProvider' = orgdotjodadottimedottzdotNameProvider+1;

orgdotjodadottimedottzdotProvider >= 1 ->
orgdotjodadottimedottzdotProvider' = orgdotjodadottimedottzdotProvider+1;

orgdotjodadottimedottzdotUTCProvider >= 1 ->
orgdotjodadottimedottzdotUTCProvider' = orgdotjodadottimedottzdotUTCProvider+1;

orgdotjodadottimedottzdotZoneInfoCompiler >= 1 ->
orgdotjodadottimedottzdotZoneInfoCompiler' = orgdotjodadottimedottzdotZoneInfoCompiler+1;

orgdotjodadottimedottzdotZoneInfoLogger >= 1 ->
orgdotjodadottimedottzdotZoneInfoLogger' = orgdotjodadottimedottzdotZoneInfoLogger+1;

orgdotjodadottimedottzdotZoneInfoProvider >= 1 ->
orgdotjodadottimedottzdotZoneInfoProvider' = orgdotjodadottimedottzdotZoneInfoProvider+1;



init
org.joda.time.DateTime=1, org.joda.time.Chronology=0, org.joda.time.DateMidnight=0, org.joda.time.DateTimeComparator=0, org.joda.time.DateTimeField=0, org.joda.time.DateTimeFieldType=0, java.util.Map=0, long=0, java.util.Set=0, org.joda.time.DateTimeZone=0, org.joda.time.tz.NameProvider=0, org.joda.time.tz.Provider=0, org.joda.time.DurationField=0, org.joda.time.DurationFieldType=0, org.joda.time.Instant=0, org.joda.time.LocalDate=0, org.joda.time.LocalDateTime=0, org.joda.time.LocalTime=0, org.joda.time.MonthDay=0, org.joda.time.MutableDateTime=0, org.joda.time.MutableInterval=0, org.joda.time.MutablePeriod=0, org.joda.time.Partial=0, org.joda.time.Period=0, org.joda.time.PeriodType=0, org.joda.time.TimeOfDay=0, org.joda.time.UTCDateTimeZone=0, org.joda.time.YearMonth=0, org.joda.time.YearMonthDay=0, org.joda.time.base.BaseDateTime=0, org.joda.time.chrono.BuddhistChronology=0, org.joda.time.chrono.CopticChronology=0, org.joda.time.chrono.EthiopicChronology=0, org.joda.time.chrono.GJChronology=0, org.joda.time.chrono.GregorianChronology=0, org.joda.time.chrono.ISOChronology=0, org.joda.time.chrono.IslamicChronology=0, org.joda.time.chrono.JulianChronology=0, org.joda.time.convert.ConverterManager=0, org.joda.time.field.AbstractReadableInstantFieldProperty=0, org.joda.time.format.DateTimeFormatter=0, org.joda.time.format.PeriodFormatter=0, org.joda.time.format.PeriodFormatterBuilder=0, org.joda.time.tz.DateTimeZoneBuilder=0, org.joda.time.tz.DefaultNameProvider=0, org.joda.time.tz.UTCProvider=0, org.joda.time.tz.ZoneInfoCompiler=0, boolean=0, org.joda.time.tz.ZoneInfoLogger=0, org.joda.time.DateMidnight$Property=0, org.joda.time.Interval=0, indicator_place_org.joda.time.DateTime=0, org.joda.time.DateTime$Property=0, int=0, java.lang.String=0, double=0, java.util.TimeZone=0, org.joda.time.Days=0, org.joda.time.Duration=0, org.joda.time.Hours=0, org.joda.time.Minutes=0, org.joda.time.Seconds=0, org.joda.time.Weeks=0, org.joda.time.IllegalFieldValueException=0, org.joda.time.IllegalInstantException=0, org.joda.time.JodaTimePermission=0, java.util.Date=0, org.joda.time.LocalDate$Property=0, org.joda.time.LocalDateTime$Property=0, org.joda.time.LocalTime$Property=0, org.joda.time.DateTimeFieldType[]=0, org.joda.time.MonthDay$Property=0, org.joda.time.Months=0, java.lang.Object=0, org.joda.time.MutableDateTime$Property=0, int[]=0, org.joda.time.TimeOfDay$Property=0, org.joda.time.YearMonth$Property=0, org.joda.time.YearMonthDay$Property=0, org.joda.time.Years=0, org.joda.time.chrono.IslamicChronology$LeapYearPatternType=0, org.joda.time.chrono.LenientChronology=0, org.joda.time.chrono.StrictChronology=0, org.joda.time.convert.DurationConverter[]=0, org.joda.time.convert.InstantConverter[]=0, org.joda.time.convert.IntervalConverter[]=0, org.joda.time.convert.PartialConverter[]=0, org.joda.time.convert.PeriodConverter[]=0, org.joda.time.field.DelegatedDateTimeField=0, org.joda.time.field.UnsupportedDurationField=0, java.lang.Integer=0, java.util.Locale=0, org.joda.time.format.DateTimeParser=0, org.joda.time.format.DateTimePrinter=0, org.joda.time.format.PeriodParser=0, org.joda.time.format.PeriodPrinter=0, org.joda.time.tz.CachedDateTimeZone=0, org.joda.time.tz.ZoneInfoProvider=0, org.joda.time.ReadablePartial=0, org.joda.time.ReadableInstant=0, org.joda.time.base.AbstractDateTime=0, org.joda.time.format.DateTimeFormatterBuilder$TimeZoneId=0, org.joda.time.Partial$Property=0, java.util.Calendar=0, org.joda.time.chrono.ZonedChronology=0, org.joda.time.convert.DurationConverter=0, org.joda.time.convert.InstantConverter=0, org.joda.time.convert.IntervalConverter=0, org.joda.time.convert.PartialConverter=0, org.joda.time.convert.PeriodConverter=0, org.joda.time.field.DecoratedDurationField=0, org.joda.time.field.ImpreciseDateTimeField=0, org.joda.time.field.OffsetDateTimeField=0, org.joda.time.field.PreciseDateTimeField=0, org.joda.time.field.PreciseDurationDateTimeField=0, org.joda.time.field.PreciseDurationField=0, org.joda.time.field.SkipDateTimeField=0, org.joda.time.field.SkipUndoDateTimeField=0, org.joda.time.field.UnsupportedDateTimeField=0, org.joda.time.field.ZeroIsMaxDateTimeField=0, org.joda.time.format.DateTimeFormat$StyleFormatterCacheKey=0, org.joda.time.tz.FixedDateTimeZone=0, org.joda.time.field.DividedDateTimeField=0, org.joda.time.field.RemainderDateTimeField=0, org.joda.time.field.ScaledDurationField=0, org.joda.time.format.DateTimeParserBucket=0, java.text.DateFormatSymbols=0, java.lang.Number=0, java.util.GregorianCalendar=0

target
int=1,boolean=0,double=0,int[]=0,java.lang.Integer=0,java.lang.Number=0,java.lang.Object=0,java.lang.String=0,java.text.DateFormatSymbols=0,java.util.Calendar=0,java.util.Date=0,java.util.GregorianCalendar=0,java.util.Locale=0,java.util.Map=0,java.util.Set=0,java.util.TimeZone=0,long=0,org.joda.time.Chronology=0,org.joda.time.DateMidnight=0,org.joda.time.DateMidnight$Property=0,org.joda.time.DateTime=0,org.joda.time.DateTime$Property=0,org.joda.time.DateTimeComparator=0,org.joda.time.DateTimeField=0,org.joda.time.DateTimeFieldType=0,org.joda.time.DateTimeFieldType[]=0,org.joda.time.DateTimeZone=0,org.joda.time.Days=0,org.joda.time.Duration=0,org.joda.time.DurationField=0,org.joda.time.DurationFieldType=0,org.joda.time.Hours=0,org.joda.time.IllegalFieldValueException=0,org.joda.time.IllegalInstantException=0,org.joda.time.Instant=0,org.joda.time.Interval=0,org.joda.time.JodaTimePermission=0,org.joda.time.LocalDate=0,org.joda.time.LocalDate$Property=0,org.joda.time.LocalDateTime=0,org.joda.time.LocalDateTime$Property=0,org.joda.time.LocalTime=0,org.joda.time.LocalTime$Property=0,org.joda.time.Minutes=0,org.joda.time.MonthDay=0,org.joda.time.MonthDay$Property=0,org.joda.time.Months=0,org.joda.time.MutableDateTime=0,org.joda.time.MutableDateTime$Property=0,org.joda.time.MutableInterval=0,org.joda.time.MutablePeriod=0,org.joda.time.Partial=0,org.joda.time.Partial$Property=0,org.joda.time.Period=0,org.joda.time.PeriodType=0,org.joda.time.ReadableInstant=0,org.joda.time.ReadablePartial=0,org.joda.time.Seconds=0,org.joda.time.TimeOfDay=0,org.joda.time.TimeOfDay$Property=0,org.joda.time.UTCDateTimeZone=0,org.joda.time.Weeks=0,org.joda.time.YearMonth=0,org.joda.time.YearMonth$Property=0,org.joda.time.YearMonthDay=0,org.joda.time.YearMonthDay$Property=0,org.joda.time.Years=0,org.joda.time.base.AbstractDateTime=0,org.joda.time.base.BaseDateTime=0,org.joda.time.chrono.BuddhistChronology=0,org.joda.time.chrono.CopticChronology=0,org.joda.time.chrono.EthiopicChronology=0,org.joda.time.chrono.GJChronology=0,org.joda.time.chrono.GregorianChronology=0,org.joda.time.chrono.ISOChronology=0,org.joda.time.chrono.IslamicChronology=0,org.joda.time.chrono.IslamicChronology$LeapYearPatternType=0,org.joda.time.chrono.JulianChronology=0,org.joda.time.chrono.LenientChronology=0,org.joda.time.chrono.StrictChronology=0,org.joda.time.chrono.ZonedChronology=0,org.joda.time.convert.ConverterManager=0,org.joda.time.convert.DurationConverter=0,org.joda.time.convert.DurationConverter[]=0,org.joda.time.convert.InstantConverter=0,org.joda.time.convert.InstantConverter[]=0,org.joda.time.convert.IntervalConverter=0,org.joda.time.convert.IntervalConverter[]=0,org.joda.time.convert.PartialConverter=0,org.joda.time.convert.PartialConverter[]=0,org.joda.time.convert.PeriodConverter=0,org.joda.time.convert.PeriodConverter[]=0,org.joda.time.field.AbstractReadableInstantFieldProperty=0,org.joda.time.field.DecoratedDurationField=0,org.joda.time.field.DelegatedDateTimeField=0,org.joda.time.field.DividedDateTimeField=0,org.joda.time.field.ImpreciseDateTimeField=0,org.joda.time.field.OffsetDateTimeField=0,org.joda.time.field.PreciseDateTimeField=0,org.joda.time.field.PreciseDurationDateTimeField=0,org.joda.time.field.PreciseDurationField=0,org.joda.time.field.RemainderDateTimeField=0,org.joda.time.field.ScaledDurationField=0,org.joda.time.field.SkipDateTimeField=0,org.joda.time.field.SkipUndoDateTimeField=0,org.joda.time.field.UnsupportedDateTimeField=0,org.joda.time.field.UnsupportedDurationField=0,org.joda.time.field.ZeroIsMaxDateTimeField=0,org.joda.time.format.DateTimeFormat$StyleFormatterCacheKey=0,org.joda.time.format.DateTimeFormatter=0,org.joda.time.format.DateTimeFormatterBuilder$TimeZoneId=0,org.joda.time.format.DateTimeParser=0,org.joda.time.format.DateTimeParserBucket=0,org.joda.time.format.DateTimePrinter=0,org.joda.time.format.PeriodFormatter=0,org.joda.time.format.PeriodFormatterBuilder=0,org.joda.time.format.PeriodParser=0,org.joda.time.format.PeriodPrinter=0,org.joda.time.tz.CachedDateTimeZone=0,org.joda.time.tz.DateTimeZoneBuilder=0,org.joda.time.tz.DefaultNameProvider=0,org.joda.time.tz.FixedDateTimeZone=0,org.joda.time.tz.NameProvider=0,org.joda.time.tz.Provider=0,org.joda.time.tz.UTCProvider=0,org.joda.time.tz.ZoneInfoCompiler=0,org.joda.time.tz.ZoneInfoLogger=0,org.joda.time.tz.ZoneInfoProvider=0,indicator_place_org.joda.time.DateTime>=1
