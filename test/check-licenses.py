import os
import sys
import pathlib
import mmap


copyright_elisp = '''
;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.
'''

patterns = {'el': copyright_elisp}
include_dirs = ('.', 'bin', 'share', 'site-lisp', 'test',)
exlude_files = ('.dir-locals.el', 'package-quickstart.el', 'show-point-mode.el')
bad_licenses = []

for ext in patterns:
    pattern = patterns[ext]

    for root, dirs, files in os.walk('.'):
        path = pathlib.Path(root)

        root_part = '.'
        if len(path.parts) > 0:
            root_part = path.parts[0]

        if root_part not in include_dirs:
            continue

        for file in files:
            if file.startswith('.#') or not file.endswith(ext):
                continue

            if os.path.relpath(file) in exlude_files:
                continue

            realpath = os.path.join(root, file)
            with open(realpath) as f:
                s = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
                if s.find(pattern.encode()) == -1:
                    bad_licenses.append(realpath)


if len(bad_licenses):
    bad_licenses.sort()

    print('Files without license notice (or with a wrong notice format) are:')
    for f in bad_licenses:
        print('  -', f)

    sys.exit(1)
