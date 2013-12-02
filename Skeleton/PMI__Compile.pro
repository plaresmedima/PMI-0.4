pro PMI__Compile

;	path = str_sep(!path,';')
;	path = path[n_elements(path)-1]

	path = dialog_pickfile(/directory, $
		path 	= 'C:\', $
		title	= 'Save pmi.sav in the directory..' )
	if path eq '' then return
	file = path + 'pmi.sav'


	;COMPILE


	resolve_routine, 'pmi'
	resolve_all



	PMI__OBJECTS



	;iTools

	ITRESOLVE


	save, /routines, filename=file,/compress
	print, 'saved to ', file
end