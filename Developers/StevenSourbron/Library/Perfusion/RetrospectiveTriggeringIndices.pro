FUNCTION RetrospectiveTriggeringIndices, $
	triggercurve, $
	sampling_time, $
	cutoff_frequency, $
	tolerance, $
	minima=minima, $
	cnt=cnt

	if keyword_set(minima) then triggercurve = -triggercurve
	triggercurvesmth = FourierFilter(sampling_time, triggercurve, cutoff_frequency)
	diff = triggercurvesmth - triggercurve
 	pdiff = percentiles(abs(diff),[100-tolerance])
 	RETURN, where(diff gt pdiff[0], cnt)
END
