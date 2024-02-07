# Ivy

* https://github.com/abo-abo/swiper

* rename : don't have history
* cannot paste a file location

* it can be messed with a C-c C-s in the minibuffer ???
* cannot copy the file location
  workaround ???
* cannot select the current working directory
  C-x C-f start from parent directory of the current buffer
  workaround : switch to buffer `*GNU Emacs*`
* /!\ rename : if old-CMakeList.txt exists ten it will we selected if we enter CMakeLists.txt
  workaround C-M-j
* completion for 'sp' propose 'libsp...' 'sp...' select 'libsp...'

* how to edit recent file
  get one proposition C-i -> edit -> C-M-j

break C-x C-f for recent file history -> counsel-recentf
              cannot past a path
break C-s C-w for search yank word -> C-s M-j
counsel-M-x: is it possible sort commands by recent used?
  https://github.com/abo-abo/swiper/issues/629
  -> install smex

# code comptletion

* PRIV->VATE_HEADER for PRIVATE
    C-k delete all text after
    tab complete proposition
    Esc x3 works

* type `mak` in front of `transformation` spoil
  `QcProjection4::mak<completion...>transformation(const QcProjection4 & proj2) const`

# company-box

* frame is white
* icon are displayed as a box for company-dabbrev
  and frame is to short

* Update frame's background — https://github.com/sebastiencs/company-box/pull/172

# LSP

slow ???

----------------------------------------------------------------------------------------------------

# C-x C-f

* C-k broken

# C-s

* mouse. -> mouse\.
* C-s C-w broken
