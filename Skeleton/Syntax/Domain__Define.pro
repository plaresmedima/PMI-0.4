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


PRO DOMAIN::C, k, c
	case k of
	0:	self->z, c
	1:	self->t, c
	endcase
END
FUNCTION DOMAIN::C,k, i0,i1
	case n_params() of
		1:if k eq 0 then return, self->z() 		else return, self->t()
		2:if k eq 0 then return, self->z(i0) 	else return, self->t(i0)
		3:if k eq 0 then return, self->z(i0,i1) else return, self->t(i0,i1)
	endcase
END

FUNCTION DOMAIN::M
	return, self.m
END
PRO DOMAIN::M, m
	self.m = m
END
PRO DOMAIN::T, t, i
	case n_params() of
		1:begin
			ptr_free, self.t
			self.t = ptr_new(t)
			end
		2:(*self.t)[i] = t
	endcase
END
FUNCTION DOMAIN::T, i0, i1
	case n_params() of
		0:return, (*Self.t)
		1:return, (*Self.t)[i0]
		2:return, (*Self.t)[i0:i1]
	endcase
END

PRO DOMAIN::Z, z, i
	case n_params() of
		1:begin
			ptr_free, self.z
			self.z = ptr_new(z)
			end
		2:(*self.z)[i] = z
	endcase
END
FUNCTION DOMAIN::Z, i0, i1
	case n_params() of
		0:return, (*Self.z)
		1:return, (*Self.z)[i0]
		2:return, (*Self.z)[i0:i1]
	endcase
END

PRO DOMAIN::D, d
	self->dom, {z:findgen(d[2]), t:findgen(d[3]), m:d[0:1]}
END
FUNCTION DOMAIN::D, i
	Dim = [Self.m,n_elements(*Self.z),n_elements(*Self.t)]
	if n_params() eq 0 then return, Dim
	return, Dim[i]
END


PRO DOMAIN::DOM, dom
	ptr_free, self.z, self.t
	self.z 		= ptr_new(dom.z)
	self.t 		= ptr_new(dom.t)
	self.m 		= dom.m
END
FUNCTION DOMAIN::DOM

	return, {z:*self.z, t:*self.t, m:self.m}
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



PRO DOMAIN::CLEANUP
	ptr_free, self.z, self.t
END

FUNCTION DOMAIN::INIT
	return, 1
END

PRO DOMAIN__DEFINE

	struct = {DOMAIN				$
	,	z:			ptr_new()		$	;dblarr(nslices)
	,	t:			ptr_new()		$	;dblarr(ntimes)
	,	m:			lonarr(2)		}
END