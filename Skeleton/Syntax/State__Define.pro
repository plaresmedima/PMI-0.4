
FUNCTION STATE::FILES, n

	n = self -> n()
	if n eq 0 then return, 0B
	files = strarr(n)
	for i=0L,n-1 do files[i] = (self->obj(i)) -> file()
	return, files
END
FUNCTION STATE::NAMES
	if self->n() eq 0 then return, 0B
	names = self->files(n)
	for i=0L,n-1 do names[i] = fname(names[i])
	return, names
END
FUNCTION STATE::ITEM, k, i

	if self->nitems(k) eq 0 then return, 0B
	stdy = self -> obj()
	return, stdy -> obj(k,i)
END
FUNCTION STATE::NITEMS, k

	if self->n() eq 0 then return, 0
	if self->sel() eq -1 then return, 0
	return,(self->obj())->n(k)
END
PRO STATE::CLEANUP

	n = self -> n()
	if n gt 0 then self -> delete, lindgen(n)
END
FUNCTION STATE::INIT

	ok = self -> file_manager::init()
	return, 1
END
PRO STATE__DEFINE

	struct = {STATE							$
	,	INHERITS file_manager				$
	,	INHERITS array						$  ;array of loaded studies
	}
END