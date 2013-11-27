;
;    Copyright (C) 2013 Steven Sourbron
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SAVING AND LOADING        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO STDY_STATE::SAVESTDY

    if PMI__fopenw(self.file) then begin
    	stdy = self.arr
    	save, stdy, filename = self.file
    	self.saved = 1
    endif else ok = dialog_message(!ERROR_STATE.MSG)

END

PRO STDY_STATE::UPDATEDIR, file

    if n_params() eq 1                    $
    then saved = self -> saved(file,number=n)  $
    else saved = self -> saved(number=n)

    if n eq 0 then return

    if n_params() eq 1 then begin
       o = obj_new('STDY_STATE',file)
       files = o -> files()
       obj_destroy, o
    endif else files = self -> files()

    for i=0L,n-1 do begin
       nd = total(saved[i] eq files)
       if nd eq 0 then DeleteFile, saved[i]
    endfor
END




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   EDITING                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





FUNCTION STDY_STATE::NEW $
,   Objname $
,   Name    = Name $
,   Domain   = Dom $
,   Dim   = dim $
,   Color    = clr $
,   Data = Dat $
,   Default = Def $
,   ClrWin    = trim

    arr = ObjName eq 'REGION'

    if n_elements(Name) eq 0 then Name = Objname

    Name = str_in_list(self->names(arr),Name)
    File = str_in_list(self->saved(/name),ObjName)
    File = cleanstr(File) + '.dat'
    PMI__fcreate, self->DataPath() + File

    Obj = obj_new(Objname,File,Name=Name)

    if n_elements(Def) ne 0 then begin
       Obj -> Dom, Def->Dom()
       Obj -> Clr, Def->Clr()
       Obj -> CopyHeader, Def
       if ObjName eq 'SERIES' then Obj->Trim, Def->Trim()
    endif
    if n_elements(dom)     ne 0 then Obj -> Dom   , dom
    if n_elements(dim)     ne 0 then Obj -> D     , dim
    if n_elements(clr)     ne 0 then Obj -> Clr   , clr
    if n_elements(trim) ne 0 then Obj -> Trim  , trim
    if n_elements(dat)     ne 0 then Obj -> Write , self->DataPath(), dat

    self -> Insert, arr, Obj

    return, Obj
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   STDY_STATE::FUNCTIONS     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




FUNCTION STDY_STATE::NAMES, k, n, Ind=ind, Sel=sel, Dim=d, DefDim=DefDim

	if n_elements(DefDim) ne 0 then begin
		names = self->names(k,n)
		if n eq 0 then return, ''
       	def = bytarr(n)
       	for i=0L,n-1 do begin
         	obj = self -> obj(k,i)
         	def[i] = obj->d(DefDim) gt 1
       	endfor
        ind = where(def,n)
       	if n eq 0 then return, ''
       	sel = (where(self->sel(k) eq ind))[0]
       	if sel eq -1 then sel=0L
       	return, names[ind]
	endif

    if n_elements(d) ne 0 then begin
       names = self->names(k,n)
       if n eq 0 then return, ''
       comp = bytarr(n)
       for i=0L,n-1 do begin
         obj = self -> obj(k,i)
         comp[i] = arrcomp(d, obj->d())
       endfor
       ind = where(comp,n)
       if n eq 0 then return, ''
       sel = (where(self->sel(k) eq ind))[0]
       return, names[ind]
    endif

    n = self -> n(k)
    if n eq 0 then return, ''
    names = strarr(n)
    for i=0,n-1 do begin
       obj = self -> obj(k,i)
       names[i] = obj -> name()
    endfor
    return, names
END
FUNCTION STDY_STATE::DATAPATH, file
    if n_params() eq 0 then file = self.file
    return, fpath(file) + fname(file) + '_'
END
FUNCTION STDY_STATE::EXPORTPATH
   	Path = Self->Datapath() + 'Export'
	file_mkdir, Path
	return, Path + '\'
END
FUNCTION STDY_STATE::FILES, n
    n = self -> n(0) + self -> n(1)
    if n eq 0 then return, ''
    files = strarr(n)
    path = self->DataPath()
    for k=0L,1 do begin
       for i=0, self->n(k)-1 do begin
         Data = Self->Obj(k,i)
         files[k*self->n(0)+i] = path + Data->File()
       endfor
    endfor
    return, files
END
FUNCTION STDY_STATE::SAVED, file, number=n, name=name
    if n_params() eq 0 then file = self.file
    path = self -> DataPath(file)
    files = findfile(path + '*.dat',count=n)
    if keyword_set(name) then for i=0L,n-1 do $
       files[i] = strmid(files[i],strlen(path),strlen(files[i])-strlen(path)-4)
    return, files
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   PRIMARY METHODS           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO STDY_STATE::INSERT, k, obj
    self.arr[k] -> insert, obj
    self.saved = 0
END
PRO STDY_STATE::DELETE, k, i
    self.arr[k] -> delete, i
    self.saved = 0
END
PRO STDY_STATE::SAVED, x
    self.saved = x
END
PRO STDY_STATE::SEL, k, i
    self.arr[k] -> sel, i
END
PRO STDY_STATE::FILE, file
    self.file = file
END
FUNCTION STDY_STATE::N, k
    if not obj_valid(self.arr[k]) then return, 0
    return, self.arr[k] -> n()
END
FUNCTION STDY_STATE::SEL, k
    return, self.arr[k] -> sel()
END
FUNCTION STDY_STATE::GETVALUE, k
    return, self.arr[k] -> getvalue()
END
FUNCTION STDY_STATE::OBJ, k, i
    return, self.arr[k] -> obj(i)
END
FUNCTION STDY_STATE::FILE
    return, self.file
END
FUNCTION STDY_STATE::Name
	return, fname(self->file())
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




PRO STDY_STATE::CLEANUP

    if not self.saved then begin
       title = [fname(self.file) + ' has changed.', 'Would you like to save it?']
       if dialog_message(/question,title) eq 'Yes' $
       then begin
         self -> SaveStdy
         self -> UpdateDir
       endif else self -> UpdateDir, self.file
    endif
    obj_destroy, self.arr
END

FUNCTION STDY_STATE::INIT, file

    ok = self -> file_manager::init()
    self.file  = file
    self.saved = 1
    if PMI__fopenr(file) then begin
       restore, file, /relaxed_structure_assignment
       self.arr = stdy;                                                     ???
    endif else self.arr = [obj_new('ARRAY'),obj_new('ARRAY')]
    return, 1
END


PRO STDY_STATE__DEFINE

    struct = {STDY_STATE          $
    ,  INHERITS file_manager         $
    ,  file:          ''       $
    ,  saved:          0B        $
    ,  arr:          objarr(2)    }
END