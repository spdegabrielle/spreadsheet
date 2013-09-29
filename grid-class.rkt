#lang racket/gui
;; grid pasteboard
;; defines a subclass of pasteboard% that only allows inserts into cells on a grid.

(define grid% (class pasteboard% 
                (inherit insert)
               ; (define/override (insertat )
                
                (super-new)
                
                ))



;; tests

(define frame (new frame% [label "Simple grid"]
                      [width 200]
                      [height 200]))
(define mb (new menu-bar% [parent frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)

(define editor-canvas (new editor-canvas% [parent frame]))
(send editor-canvas horiz-margin 0)

(define grid (new grid%))
(define t (new text%))

(send editor-canvas set-editor grid)
(send frame show #t)

(define s (make-object editor-snip% t)) ; t is the old text editor
(send grid insert s 3 0)