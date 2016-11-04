# Install script for directory: /home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-help.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-interp.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-keys.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-mode.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-util.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-minor-mode.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-server.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-language.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-browser.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-dev.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-document.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-widgets.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-menu.elc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/SuperCollider" TYPE FILE FILES "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/sclang-vars.elc")
endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/home/kapitan/.emacs.d/packs/dev/lang-pack/vendor/checkouts/scel-git-0425a8d/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
