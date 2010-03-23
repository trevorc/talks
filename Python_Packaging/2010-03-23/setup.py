from setuptools import setup

setup(
    name='pypi-upload-server',
    author='Trevor Caira',
    author_email='trevor@knowmore.com',
    version='0.0.1',
    py_modules=['upload_server'],
    install_requires=['Paste==1.7.2'],
    entry_points={'console_scripts': [
        'pypi-upload-server = upload_server:main',
    ]},
)
