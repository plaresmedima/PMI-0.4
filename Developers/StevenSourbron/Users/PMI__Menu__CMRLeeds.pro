pro PMI__Menu__CMRLeeds, parent

	PMI__Menu__Skeleton, parent
	PMI__Menu__Slices, parent
	PMI__Menu__Dynamic, parent

	id = widget_button(parent, value='CMR Leeds',/menu)

	Sid = PMI__Button__ArshadDeconvolution(id,value='DCE: Model-free analysis (Pixel)')
	Sid = PMI__Button__ArshadDeconvolutionRoi(id,value='DCE: Model-free analysis (ROI)', /separator)
	Sid = PMI__Button__CardiacFermiRoi(id,value='DCE: Fermi model analysis (ROI)')
	Sid = PMI__Button__CardiacSingleInletRoiSlider(id, value='DCE: All models (ROI)')
	Sid = PMI__Button__PulseWaveVelocityTime(id, value = 'PWV: Foot-to-foot delay',/separator)
	Sid = PMI__Button__PulseWaveForwardBackward(id, value = 'PWV: Forward/backward flow')

end
