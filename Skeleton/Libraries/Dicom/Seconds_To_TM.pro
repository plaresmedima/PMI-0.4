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


function Seconds_To_TM, time

	hrs  = floor(time/3600.0)
	mins = floor((time - hrs*3600.0)/60.0)
	secs = floor(time - hrs*3600.0 - mins*60.0)
	rem = time - hrs*3600.0 - mins*60.0 - secs

	hrs  = strnr(fix(hrs),2)
	mins = strnr(fix(mins),2)
	secs = strnr(fix(secs),2)
	rem  = strcompress(rem,/remove_all)

	return, hrs + mins + secs + strmid(rem,1,7)
end
