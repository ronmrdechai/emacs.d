# My Emacs settings #
My constantly-changing Emacs settings. The code in here is extreamly messy, please don't try to learn from it.

This configuration is geared mainly towards C/C++, Python, ELisp, Perl, JavaScript assembly development, but it will probably change as my intrests shift.

What won't change is the nice autocompletion setup I have going.

# Requirements #
I haven't tested this on anything older than Emacs 24, so I think it's safe to say that that's the minumum version required. You might also need LLVM and Clang if want C++ completion, and Jedi if you want Python completion.

# Installation #
Just clone this repository into `~/.emacs.d`. Make sure to clone all submodules as well, this repository relies on [`irony-mode`](https://github.com/Sarcasm/irony-mode) or C++ completion, and [`slime`](https://github.com/slime/slime) for Lisp development.
