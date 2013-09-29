#lang racket/gui

;; spreadsheet
;; subclass canvas to create cells
;; clicking in a cell allows entering data
;; data stored as sparse array



(provide spreadsheet-pastboard%)

;; ---------------------------------------------------------------------------


(define spreadsheet-pastboard% 
  (class pasteboard%
    (inherit get-dc)
    
    ;mouse position
    (define mx 0)
    (define my 0)
    
    ;; cells are 48 px wide and 14 px high
    (define w 48)
    (define h 14)
    (struct cell (row column))
    (define (col->x column) (* column w))
    (define (row->y row) (* row h))
    
    
    (define (highlight-cell dc r c)
      (send dc set-pen "yellow" 1 'hilite)
      (send dc set-brush "yellow" 1 'hilite)
      (send a-dc draw-rounded-rectangle (col->x c) (row->y r) 48 14 3))
    (define (x->col x) (quotient x w))
    (define (y->row y) (quotient y h))
    
    (define/override (on-default-event mouse-event)
      (when (send mouse-event moving?) 
        (let ((x (send mouse-event get-x))
              (y (send mouse-event get-y)))
          (set! mx x)
          (set! my y)
          ))
      (super on-default-event mouse-event)
      )
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (send dc suspend-flush)
      (send dc set-pen "yellow" 1 'hilite)
      (send dc set-brush "yellow" 1 'hilite)
      (send a-dc draw-rounded-rectangle (x) (y) 48 14 3)
      (send dc draw-lines (list (cons 0 0) (cons mx my)))
      (send dc resume-flush)
      (super on-paint before? dc left top right bottom dx dy draw-caret))
    
    (super-new)
    
    
    ))





;;; tests


(define f (new frame% (label "spreadsheet") [width 400] [height 400]))
(send f show #t)
(define text-field (new text-field%
                        (label "Text")
                        (parent f)
                        (init-value "Field")))
(define spreadsheet-pastboard (new spreadsheet-pastboard%))
(define ec (new editor-canvas% [parent f] [editor spreadsheet-pastboard]))

