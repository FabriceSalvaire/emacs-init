# Emacs init.el

This repository contains a [GNU Emacs](https://www.gnu.org/software/emacs) setup for release **29**.

**Features**
- inspired by [Doom Emacs](https://github.com/doomemacs/doomemacs)
- [radian-software/straight.el — package manager](https://github.com/radian-software/straight.el)
- [Magit](https://magit.vc)
- languages
  - Python
  - C/C++

## Should I Use Emacs or Visual Studio Code ?

Emacs, an acronym for "Editor Macros", is one of the oldest software that is still used nowadays. The development of Emacs started in 1976 and GNU Emacs later in 1984.  For this reason, Emacs is written internally in C and is extended in Emacs Lisp.

Visual Studio Code was released thirty years later in 2015 and use a framework inspired by the web world.  It is written in Javascript (Typescript) and uses a browser to render its user interface in HTML/CSS.

So obviously, VS Code has advantages over Emacs:
- It benefits of a modern language and a modern graphic stack for its user interface.
- It is easier to start, if you don't use Doom Emacs or something similar.

However, they are several reasons why Emacs is still pertinent in 2024:
- A text editor implies to use a — monospace font — which simplifies the UI requirements.  Emacs can still do most of the job.
- Emacs benefits of thirty years of developments.

So, the right answer is probably install both, compare them and use the best one for each task.

Notice, during these last thirty years, several source-code editors emerged and lost popularity over the years, e.g. Eclipse and IntelliJ IDEA.

So, there is any proof VS Code will not follow the same fate...

## Should I use my own config or Doom Emacs

Doom Emacs is amazing work done by Henrik Lissner! But there advantages and disadvantages to rely on it:
- For sure, Doom startup is fast!
- Doom was designed so as to be a framework rather than a personal configuration.
- But up to now Henrik did 99.999% of the development.  Notice he is looking for maintainers!
- Doom's documentation is perfectible, which is not unusual for a project maintained by only one person.
- Doom has inevitably opinionated settings that you will dislike.
- Doom is using by default the VI emulation mode (evil).
- Doom source code is quite large.
- Doom's source code organisation can be messy. What is that ??? Why it is there ??? Do I need really that ???
- Doom contains a lot of interesting things, but also a lot of things you will never worry about their existences.
- Doom implements a lot of tricks to improve performances. Remember Donald Knuth: "premature optimization is the root of   all evil".  Somehow it is fine, but should we take care to optimize Emacs startup for just one second ?  Why those optimisations are not pulled up in Emacs if they make sense for everybody ?
- Doom monkey patches a lot of thinks (thanks to Emacs Lisp features like `advice`).
- Doom implements its own extension of `straight`, `use-package`, etc. Why don't improve those packages instead ?
- Doom implements a lot of codes that are commented by: "PR upstream...".  Notice PR can be time consuming and discouraging.
- Doom pins git SHA for each package.  It makes sense but it also means you rely on Doom development to get a newer package release.
- If you fork to customize more and more the source code, a merge with upstream could be a nightmare, since you will merge your life with somebody else. In fact with Doom, you can only customize your `init.el` and not the whole stuff.

Personally I think these advantages and disadvantages are deeply intricated.  There is no perfect answer.

So, the right answer is probably install both. Start with Doom and later decide what you should do.
