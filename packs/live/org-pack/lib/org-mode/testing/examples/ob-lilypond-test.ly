
% [[file:~/.emacs.d/martyn/martyn/ob-lilypond/test/test-build/test.org::*LilyPond%2520Version][LilyPond-Version:1]]

\version "2.12.3"

% LilyPond-Version:1 ends here

% [[file:~/.emacs.d/martyn/martyn/ob-lilypond/test/test-build/test.org::*lilypond%2520block%2520for%2520test%2520purposes][lilypond-block-for-test-purposes:1]]

\score {
  \relative c' {
    c8 d e f g a b c |
    b a g f e d c4   |
  }

% lilypond-block-for-test-purposes:1 ends here

% [[file:~/.emacs.d/martyn/martyn/ob-lilypond/test/test-build/test.org::*lilypond%2520block%2520for%2520test%2520purposes][lilypond-block-for-test-purposes:2]]

\layout {
  }
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 150 4)
    }
  }
}

% lilypond-block-for-test-purposes:2 ends here
