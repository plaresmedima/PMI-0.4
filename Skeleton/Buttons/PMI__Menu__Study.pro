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


pro PMI__Menu__Study, parent

	id 	= widget_button(parent, value = 'Study', /menu)

	Sid = PMI__Button__StudyNew(id)
	Sid = PMI__Button__StudyOpen(id)
	Sid = PMI__Button__RecentStudy(id)
	Sid = PMI__Button__StudyClose(id)
	Sid = PMI__Button__StudySave(id)
	Sid = PMI__Button__StudySaveAs(id)
	Sid = PMI__Button__Exit(id)

end