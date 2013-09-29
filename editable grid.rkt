#lang racket/gui

(define editable-grid% (class panel% 
                         (init-field rows columns) 
                         (super-new [stretchable-width #t]
                                         [stretchable-height #t])
                         (define pb (new pasteboard%))
                         (define ec (new editor-canvas%   
                                         [parent this]	 
                                         [editor pb]
                                         [min-width 200]
                                         [min-height 200]))
                         
                         ))


;;; tests
(define f (new frame% [label "test for editable grid"] [width 500][height 200]))
(define eg (new editable-grid% [parent f][rows 3] [columns 4]))
(send f show #t)