;;; glsl-mode.el --- major mode for Open GLSL shader files

;; Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
;; Copyright (C) 2011, 2014 Jim Hourihan
;;
;; Authors: Xavier.Decoret@imag.fr, 
;;          Jim Hourihan <jimhourihan ~at~ gmail.com> (updated for 4.5, etc)
;; Keywords: languages
;; Version: 2.0
;; X-URL: http://artis.inrialpes.fr/~Xavier.Decoret/resources/glsl-mode/
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Major mode for editing OpenGLSL grammar files, usually files ending with
;; `.vert', `.frag', `.glsl', `.geom'.  Is is based on c-mode plus some
;; features and pre-specified fontifications.
;;
;; Modifications from the 1.0 version of glsl-mode (jimhourihan):
;;  * Removed original optimized regexps for font-lock-keywords and
;;    replaced with keyword lists for easier maintenance
;;  * Added customization group and faces
;;  * Preprocessor faces
;;  * Updated to GLSL 4.5
;;  * Separate deprecated symbols
;;  * Made _ part of a word
;;  * man page lookup at opengl.org

;; This package provides the following features:
;;  * Syntax coloring (via font-lock) for grammar symbols and
;;    builtin functions and variables for up to GLSL version 4.5
;;  * Indentation for the current line (TAB) and selected region (C-M-\).
;;  * Switching between file.vert and file.frag
;;    with S-lefttab (via ff-find-other-file)
;;  * interactive function glsl-find-man-page prompts for glsl built
;;    in function, formats opengl.org url and passes to w3m

;;; Installation:

;; This file requires Emacs-20.3 or higher and package cc-mode.

;; If glsl-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;;   (autoload 'glsl-mode "glsl-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
;;   (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;;   (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;;   (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;; Code:

(provide 'glsl-mode)

(eval-when-compile			; required and optional libraries
  (require 'cc-mode)
  (require 'find-file))

(require 'align)

(defgroup glsl nil
  "OpenGL Shading Language Major Mode"
  :group 'languages)

(defconst glsl-language-version "4.5"
  "GLSL language version number.")

(defconst gl-version "4.5"
  "OpenGL major mode version number.")

(defvar glsl-type-face 'glsl-type-face)
(defface glsl-type-face
  '((t (:inherit font-lock-type-face))) "glsl: type face"
  :group 'glsl)

(defvar glsl-builtin-face 'glsl-builtin-face)
(defface glsl-builtin-face
  '((t (:inherit font-lock-builtin-face))) "glsl: builtin face"
  :group 'glsl)

(defvar glsl-deprecated-builtin-face 'glsl-deprecated-builtin-face)
(defface glsl-deprecated-builtin-face
  '((t (:inherit glsl-builtin-face))) "glsl: deprecated builtin face"
  :group 'glsl)

(defvar glsl-keyword-face 'glsl-keyword-face)
(defface glsl-keyword-face
  '((t (:inherit font-lock-keyword-face))) "glsl: keyword face"
  :group 'glsl)

(defvar glsl-deprecated-keyword-face 'glsl-deprecated-keyword-face)
(defface glsl-deprecated-keyword-face
  '((t (:inherit glsl-keyword-face))) "glsl: deprecated keyword face"
  :group 'glsl)

(defvar glsl-variable-name-face 'glsl-variable-name-face)
(defface glsl-variable-name-face
  '((t (:inherit font-lock-variable-name-face))) "glsl: variable face"
  :group 'glsl)

(defvar glsl-deprecated-variable-name-face 'glsl-deprecated-variable-name-face)
(defface glsl-deprecated-variable-name-face
  '((t (:inherit glsl-variable-name-face))) "glsl: deprecated variable face"
  :group 'glsl)

(defvar glsl-preprocessor-face 'glsl-preprocessor-face)
(defface glsl-preprocessor-face
  '((t (:inherit font-lock-preprocessor-face))) "glsl: preprocessor face"
  :group 'glsl)

(defvar glsl-mode-hook nil)

(defvar glsl-mode-map
  (let ((glsl-mode-map (make-sparse-keymap)))
    (define-key glsl-mode-map [S-iso-lefttab] 'ff-find-other-file)    
    glsl-mode-map)
  "Keymap for GLSL major mode")

(defcustom glsl-man-pages-base-url "http://www.opengl.org/sdk/docs/man/html/"
  "Location of GL man pages"
  :group 'glsl)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)))

(eval-and-compile
  ;;
  ;;    These vars are useful for completion so keep them around after
  ;;    compile as well. The goal here is to have the byte compiled code
  ;;    have optimized regexps so its not done at eval time.
  ;;

  (defvar glsl-type-list
    '("float" "double" "int" "void" "bool" "true" "false" "mat2" "mat3"
      "mat4" "dmat2" "dmat3" "dmat4" "mat2x2" "mat2x3" "mat2x4" "dmat2x2"
      "dmat2x3" "dmat2x4" "mat3x2" "mat3x3" "mat3x4" "dmat3x2" "dmat3x3"
      "dmat3x4" "mat4x2" "mat4x3" "mat4x4" "dmat4x2" "dmat4x3" "dmat4x4" "vec2"
      "vec3" "vec4" "ivec2" "ivec3" "ivec4" "bvec2" "bvec3" "bvec4" "dvec2"
      "dvec3" "dvec4" "uint" "uvec2" "uvec3" "uvec4" "sampler1D" "sampler2D"
      "sampler3D" "samplerCube" "sampler1DShadow" "sampler2DShadow"
      "samplerCubeShadow" "sampler1DArray" "sampler2DArray"
      "sampler1DArrayShadow" "sampler2DArrayShadow" "isampler1D" "isampler2D"
      "isampler3D" "isamplerCube" "isampler1DArray" "isampler2DArray"
      "usampler1D" "usampler2D" "usampler3D" "usamplerCube" "usampler1DArray"
      "usampler2DArray" "sampler2DRect" "sampler2DRectShadow" "isampler2DRect"
      "usampler2DRect" "samplerBuffer" "isamplerBuffer" "usamplerBuffer"
      "sampler2DMS" "isampler2DMS" "usampler2DMS" "sampler2DMSArray"
      "isampler2DMSArray" "usampler2DMSArray" "samplerCubeArray"
      "samplerCubeArrayShadow" "isamplerCubeArray" "usamplerCubeArray"
      "image1D" "iimage1D" "uimage1D" "image2D" "iimage2D" "uimage2D" "image3D"
      "iimage3D" "uimage3D" "image2DRect" "iimage2DRect" "uimage2DRect"
      "imageCube" "iimageCube" "uimageCube" "imageBuffer" "iimageBuffer"
      "uimageBuffer" "image1DArray" "iimage1DArray" "uimage1DArray"
      "image2DArray" "iimage2DArray" "uimage2DArray" "imageCubeArray"
      "iimageCubeArray" "uimageCubeArray" "image2DMS" "iimage2DMS" "uimage2DMS"
      "image2DMSArray" "iimage2DMSArray" "uimage2DMSArray" "long" "short"
      "half" "fixed" "unsigned" "hvec2" "hvec3" "hvec4" "fvec2" "fvec3" "fvec4"
      "sampler3DRect"))

  (defvar glsl-modifier-list
    '("attribute" "const" "uniform" "varying" "buffer" "shared" "coherent" "volatile" "restrict"
      "readonly" "writeonly" "atomic_uint" "layout" "centroid" "flat" "smooth"
      "noperspective" "patch" "sample" "break" "continue" "do" "for" "while"
      "switch" "case" "default" "if" "else" "subroutine" "in" "out" "inout"
      "invariant" "discard" "return" "lowp" "mediump" "highp" "precision"
      "struct" "common" "partition" "active" "asm" "class" "union" "enum"
      "typedef" "template" "this" "packed" "resource" "goto" "inline" "noinline"
      "public" "static" "extern" "external" "interface" "superp" "input" "output"
      "filter" "sizeof" "cast" "namespace" "using" "row_major"
      "early_fragment_tests"))

  (defvar glsl-deprecated-modifier-list
    '("varying" "attribute")) ; centroid is deprecated when used with varying

  (defvar glsl-builtin-list
    '("abs" "acos" "acosh" "all" "any" "asin" "asinh" "atan" "atanh"
      "atomicCounter" "atomicCounterDecrement" "atomicCounterIncrement"
      "barrier" "bitCount" "bitfieldExtract" "bitfieldInsert" "bitfieldReverse"
      "ceil" "clamp" "cos" "cosh" "cross" "degrees" "determinant" "dFdx" "dFdy"
      "dFdyFine" "dFdxFine" "dFdyCoarse" "dFdxCourse" 
      "fwidthFine" "fwidthCoarse"
      "distance" "dot" "EmitStreamVertex" "EmitVertex" "EndPrimitive"
      "EndStreamPrimitive" "equal" "exp" "exp2" "faceforward" "findLSB"
      "findMSB" "floatBitsToInt" "floatBitsToUint" "floor" "fma" "fract"
      "frexp" "fwidth" "greaterThan" "greaterThanEqual" "imageAtomicAdd"
      "imageAtomicAnd" "imageAtomicCompSwap" "imageAtomicExchange"
      "imageAtomicMax" "imageAtomicMin" "imageAtomicOr" "imageAtomicXor"
      "imageLoad" "imageSize" "imageStore" "imulExtended" "intBitsToFloat"
      "imageSamples"
      "interpolateAtCentroid" "interpolateAtOffset" "interpolateAtSample"
      "inverse" "inversesqrt" "isinf" "isnan" "ldexp" "length" "lessThan"
      "lessThanEqual" "log" "log2" "matrixCompMult" "max" "memoryBarrier" "min"
      "mix" "mod" "modf" "noise" "normalize" "not" "notEqual" "outerProduct"
      "packDouble2x32" "packHalf2x16" "packSnorm2x16" "packSnorm4x8"
      "packUnorm2x16" "packUnorm4x8" "pow" "radians" "reflect" "refract"
      "round" "roundEven" "sign" "sin" "sinh" "smoothstep" "sqrt" "step" "tan"
      "tanh" "texelFetch" "texelFetchOffset" "texture" "textureGather"
      "textureGatherOffset" "textureGatherOffsets" "textureGrad"
      "textureGradOffset" "textureLod" "textureLodOffset" "textureOffset"
      "textureProj" "textureProjGrad" "textureProjGradOffset" "textureProjLod"
      "textureProjLodOffset" "textureProjOffset" "textureQueryLevels" "textureQueryLod"
      "textureSize" "transpose" "trunc" "uaddCarry" "uintBitsToFloat"
      "umulExtended" "unpackDouble2x32" "unpackHalf2x16" "unpackSnorm2x16"
      "unpackSnorm4x8" "unpackUnorm2x16" "unpackUnorm4x8" "usubBorrow"))

  (defvar glsl-deprecated-builtin-list
    '("texture1D" "texture1DProj" "texture1DLod" "texture1DProjLod"
      "texture2D" "texture2DProj" "texture2DLod" "texture2DProjLod"
      "texture2DRect" "texture2DRectProj" 
      "texture3D" "texture3DProj" "texture3DLod" "texture3DProjLod"
      "shadow1D" "shadow1DProj" "shadow1DLod" "shadow1DProjLod"
      "shadow2D" "shadow2DProj" "shadow2DLod" "shadow2DProjLod"
      "textureCube" "textureCubeLod"))

  (defvar glsl-deprecated-variables-list
    '("gl_FragColor" "gl_FragData" "gl_MaxVarying" "gl_MaxVaryingFloats"
      "gl_MaxVaryingComponents"))

  (defvar glsl-preprocessor-directive-list
    '("define" "undef" "if" "ifdef" "ifndef" "else" "elif" "endif"
      "error" "pragma" "extension" "version" "line"))

  (defvar glsl-preprocessor-expr-list
    '("defined" "##"))

  (defvar glsl-preprocessor-builtin-list
    '("__LINE__" "__FILE__" "__VERSION__"))

  (autoload 'w3m-browse-url "w3m" "View URL using w3m")
  ) ; eval-and-compile

(eval-when-compile
  (defun glsl-ppre (re)
    (format "\\<\\(%s\\)\\>" (regexp-opt re))))

(defvar glsl-font-lock-keywords-1
  (list
   (cons (eval-when-compile 
           (format "^[ \t]*#[ \t]*\\<\\(%s\\)\\>" 
                   (regexp-opt glsl-preprocessor-directive-list)))
         glsl-preprocessor-face) 
   (cons (eval-when-compile
           (glsl-ppre glsl-type-list))
         glsl-type-face) 
   (cons (eval-when-compile
           (glsl-ppre glsl-deprecated-modifier-list))
         glsl-deprecated-keyword-face)
   (cons (eval-when-compile
           (glsl-ppre glsl-modifier-list))
         glsl-keyword-face)
   (cons (eval-when-compile
           (glsl-ppre glsl-preprocessor-builtin-list))
         glsl-keyword-face)
   (cons (eval-when-compile
           (glsl-ppre glsl-deprecated-builtin-list))
         glsl-deprecated-builtin-face)
   (cons (eval-when-compile
           (glsl-ppre glsl-builtin-list))
         glsl-builtin-face)
   (cons (eval-when-compile
           (glsl-ppre glsl-deprecated-variables-list))
         glsl-deprecated-variable-name-face)
   (cons "gl_[A-Z][A-Za-z_]+" glsl-variable-name-face)
   )
  "Minimal highlighting expressions for GLSL mode")


(defvar glsl-font-lock-keywords glsl-font-lock-keywords-1
  "Default highlighting expressions for GLSL mode")

(defvar glsl-mode-syntax-table
  (let ((glsl-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" glsl-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" glsl-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" glsl-mode-syntax-table)
    (modify-syntax-entry ?_ "w" glsl-mode-syntax-table)
    glsl-mode-syntax-table)
  "Syntax table for glsl-mode")

(defvar glsl-other-file-alist
  '(("\\.frag$" (".vert"))
    ("\\.vert$" (".frag"))
    )
  "Alist of extensions to find given the current file's extension")


(defun glsl-man-completion-list ()
  (append glsl-builtin-list glsl-deprecated-builtin-list))

(defun glsl-find-man-page (thing)
  (interactive
   (let ((word (current-word nil t)))
     (list 
      (completing-read
       (concat "OpenGL.org GLSL man page: (" word "): ")
       (glsl-man-completion-list)
       nil nil nil nil word))))
  (save-excursion
    (browse-url
     (concat glsl-man-pages-base-url thing ".xtml"))))

;;;###autoload
(define-derived-mode glsl-mode c-mode "GLSL"
  "Major mode for editing OpenGLSL shader files."
  (set (make-local-variable 'font-lock-defaults) '(glsl-font-lock-keywords))
  (set (make-local-variable 'ff-other-file-alist) 'glsl-other-file-alist)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) "")
  (add-to-list 'align-c++-modes 'glsl-mode)
  )

                                        ;(easy-menu-define c-glsl-menu glsl-mode-map "GLSL Mode Commands"
                                        ;		  (cons "GLSL" (c-lang-const c-mode-menu glsl)))

;;; glsl-mode.el ends here
