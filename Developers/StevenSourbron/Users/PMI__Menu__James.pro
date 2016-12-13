pro PMI__Menu__James, parent

	PMI__Menu__Skeleton, parent

	id = widget_button(parent, value='Velocity',/menu)

	Sid = PMI__Button__PulseWaveVelocityTime(id, value = 'Foot-to-Foot Delay')
	Sid = PMI__Button__PulseWaveForwardBackward(id, value = 'Forward/Backward Flow')
	Sid = PMI__Button__RegionSelectByRangeInteractive(id,value='Threshold ROI selection',/separator)
	Sid = PMI__Button__DumpDicom(id)

end
