# GLSL (OpenGL shading language) emacs major mode

This major mode was original authored by Xavier.Decoret@imag.fr and
modified/extended by Jim Hourihan. The mode currently handles GLSL 4.6.

This package provides the following features:

- Syntax coloring (via font-lock) for grammar symbols and builtin functions and variables for up to GLSL version 4.6
- Indentation for the current line (TAB) and selected region (C-M-\). 
- Switching between file.vert and file.frag with S-lefttab (via ff-find-other-file)
- interactive function glsl-find-man-page prompts for glsl built in function, formats opengl.org url and passes to browse-url

Original GLSL mode website: (http://artis.inrialpes.fr/~Xavier.Decoret/resources/glsl-mode)

See [GLSL 4.6 Spec (PDF)](https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.60.pdf)
the official reference document for GLSL 4.6.
