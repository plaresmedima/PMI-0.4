FUNCTION MoCoModelFit, Source, ModelName, Independent, PARAMETERS=Par

	Model = OBJ_NEW('MoCoModel_' + ModelName, Independent)
	Model -> PARAMETERS, source, Par
	Model -> FORWARD, source, Par, Fit

	OBJ_DESTROY, Model

	RETURN, Fit
END