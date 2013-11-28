;
;
;    Copyright (C) 2009 Steven Sourbron
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
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;
;
;

function ShiftAif, Aif, Time, Delay

	if Delay eq 0 then return, Aif

;	ShAif = o1_interpol(Time,Aif,Time-Delay)
	ShAif = interpol(Aif,Time,Time-Delay)

	nneg = total(Time le Delay)
	if nneg gt 0 then ShAif[0:nneg-1] = 0

	return, ShAif
end