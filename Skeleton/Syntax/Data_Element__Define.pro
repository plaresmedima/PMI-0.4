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


PRO DATA_ELEMENT::TEXTVALUE, text

	self->value, PMI__Dicom__TextToValue(text, self.vr)
END
FUNCTION DATA_ELEMENT::TEXTVALUE

	return, PMI__Dicom__TextValue(self->value(), self.vr)
END
FUNCTION DATA_ELEMENT::TEXT

	gr = change_base(self->gr(),16)
	el = change_base(self->el(),16)
	tag = '[' + strnr(gr,4) + ',' + strnr(el,4) + ']'
	vr = ' (' + self.vr + ') '
	if (self->gr() eq '7FE0'x) and (self->el() eq '0010'x) $
	then value = 'Not Printed' $
	else value = self->textvalue()

	return, tag + vr + value
END

PRO DATA_ELEMENT::CONVERTTOTEXT, tag, vr, prname, value, name=name
	gr = change_base(self->gr(),16)
	el = change_base(self->el(),16)
	tag = '[' + strnr(gr,4) + ',' + strnr(el,4) + ']'
	vr = ' (' + self.vr + ') '
	prname = strjoin(make_array(36,value=' '))
	strput, prname, name
	if (self->gr() eq '7FE0'x) and (self->el() eq '0010'x) $
	then value = 'Not Printed' $
	else value = self->textvalue()
END

PRO DATA_ELEMENT::WRITETEXT, unit, name=name, depth=depth

	Self -> ConvertToText, tag, vr, prname, value, name=name
	tab = '|   '
	if n_elements(depth) eq 0 then depth = ''
	printf, unit, depth + tag + vr + prname + value
	IF self.vr EQ 'SQ' THEN BEGIN
		Seq = self->value()
		if not obj_valid(Seq[0]) then return
		for i=0L, n_elements(Seq)-1 do begin
			printf, unit, depth + tab
			printf, unit, depth + tab
			printf, unit, depth + tab + 'DATA SET ' + strcompress(i,/remove_all)
			printf, unit, depth + tab
			Seq[i] -> WriteText, unit=unit, depth=depth+tab
		endfor
		printf, unit, depth + tab
		printf, unit, depth + tab
	ENDIF
END

PRO DATA_ELEMENT::WRITE, unit

	length = self -> length(value)

	writeu, unit, self->gr()
	writeu, unit, self->el()
	writeu, unit, self.vr

	if (self.vr eq 'OB') $
	or (self.vr eq 'OW') $
	or (self.vr eq 'OF') $
	or (self.vr eq 'SQ') $
	or (self.vr eq 'UT') $
	or (self.vr eq 'UN') $
	then begin
		point_lun, -unit, position
		point_lun, unit, position+2
	endif else length = fix(length)

	writeu, unit, length
	if length eq 0 then return

	case self.vr of
		'DS':value = strcompress(value,/remove_all)
		'IS':value = strcompress(value,/remove_all)
		'TM':value = Seconds_To_TM(value)
		else:
	endcase

	writeu, unit, value
END

PRO DATA_ELEMENT::READ, unit, gr, el, TransferSyntaxUID=TS, Template=Template

	IF n_elements(el) NE 0 $
	THEN ok = PMI__Dicom__ReadDataElement(unit, gr, el, vr=vr, value=v, TransferSyntaxUID=TS, Template=Template) $
	ELSE ok = PMI__Dicom__ReadDataElement(unit, gr=gr, el=el, vr=vr, value=v, TransferSyntaxUID=TS, Template=Template)

	self.gr = gr  	;Note: Converted from US to S at this point (by mistake SPS 03-feb-2011) !!
	self.el = el	;Note: Converted from US to S at this point (by mistake SPS 03-feb-2011) !!
	self.vr = vr
	self->value, v

END

FUNCTION DATA_ELEMENT::LENGTH, value

	value = self->value(valid)
	if valid then begin
		case self.vr of
			'DS':value = strcompress(value,/remove_all)
			'IS':value = strcompress(value,/remove_all)
			'TM':value = Seconds_To_TM(value)
			else:
		endcase
		length = number_of_bytes(value)
	endif else length=0L
	return, length
END

FUNCTION DATA_ELEMENT::NAME

	if self.name eq '' then return, 'Unknown data element'
	return, self.name
END
FUNCTION DATA_ELEMENT::VR
	return, self.vr
END
FUNCTION DATA_ELEMENT::GR
	return, uint(self.gr)  	;Convert from S to US (to correct for error, see note above)
END
FUNCTION DATA_ELEMENT::EL
	return, uint(self.el)	;Convert from S to US (to correct for error, see note above)
END
FUNCTION DATA_ELEMENT::VALUE, valid

	valid = ptr_valid(self.p)
	if not valid then return, 0B
	return, *self.p
END
PRO DATA_ELEMENT::VALUE, val

	ptr_free, self.p
	if n_elements(val) eq 0 then return
	self.p = ptr_new(val,/no_copy)
END
PRO DATA_ELEMENT::VR, vr

	if n_params() eq 1 then begin
		self.vr = vr
		return
	endif
	template = LMU__DicomTemplate()
	self.vr = template->GetVR(self.gr,self.el)
	obj_destroy, template
END
PRO DATA_ELEMENT::NAME, name
	self.name = name
END




;PRIVATE METHODS




PRO DATA_ELEMENT::CLEANUP

	if self.vr EQ 'SQ' then begin
		Seq = self->value()
		if size(Seq,/type) eq 11 then obj_destroy, Seq
	endif
	ptr_free, self.p
END

FUNCTION DATA_ELEMENT::INIT, gr, el, value=value, vr=vr, name=name, unit=unit, TransferSyntaxUID=TS, Template=Template


	if n_elements(unit) ne 0 then begin

		self->Read, unit, gr, el, TransferSyntaxUID=TS, Template=Template
		return, 1B
	endif

	self.gr	= gr
	self.el = el

	if n_elements(value) 	ne 0 then self.p = ptr_new(value,/no_copy)
	if n_elements(vr) 		ne 0 then self.vr = vr else self -> VR
	if n_elements(name) 	ne 0 then self.name=name

	return, 1B
END



PRO DATA_ELEMENT__DEFINE

	struct = {DATA_ELEMENT				$
	,	gr		:	'0000'x				$	;group tag
	,	el		:	'0000'x				$	;element tag
	,	vr		:	'XX'				$	;value representation
	,	name	:	''					$	;data element name
	,	p		:	ptr_new()			$	;pointer to the value
	}

END