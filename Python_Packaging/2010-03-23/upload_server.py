import os
import re
import shutil

import paste.httpserver
import paste.request

UPLOAD_DIR = os.path.join('packages', os.path.dirname(__file__))

_name_re = re.compile(r'[A-Za-z-]+$')
_filename_re = re.compile(r'[A-Za-z_]+-.*\.egg$')

def application(env, start_response):
    fields = dict(paste.request.parse_formvars(env))
    headers = [('content-type', 'text-plain')]

    name, content = fields['name'], fields['content']
    filename = content.filename
    fp = content.file

    if _name_re.match(name) is None or \
       _filename_re.match(filename) is None:
        start_response('400 Bad Request', headers)
        return ['what is this?\n']

    pkg_dir = os.path.join(UPLOAD_DIR, name)
    if not os.path.exists(pkg_dir):
        os.mkdir(pkg_dir)

    with file(os.path.join(pkg_dir, filename), 'w') as f:
        shutil.copyfileobj(fp, f)

    start_response('200 OK', headers)
    return ['cool upload bro\n']

def main():
    paste.httpserver.serve(application, host='0.0.0.0', port='8000')

if __name__ == '__main__':
    main()
