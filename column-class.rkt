#lang racket/gui
(require framework)


(define column%
  (class vertical-panel%
    (init-field parent header rows)
    (super-new [parent parent])
    (define col-header-bt (new button% (parent this) [label header]
(vert-margin 0) (horiz-margin 0)))
    (define make-tf (lambda (d) (list d (new text-field% (label #f)
(parent this) (vert-margin 0) (horiz-margin 0)))))
    (define tf-cells (map make-tf rows))
    ;; set-cell : row-num val -> (or val #f)
    (define/public (set-cell row val)
      (when (member row rows)
        (send (cadr (assoc row tf-cells)) set-value val)
        )
      )

     (define/public (get-cell row)
      (when (member row rows)
        (send (cadr (assoc row tf-cells)) get-value)
        )
      )

    ))


(define table%
  (class panel:horizontal-dragable%
    (init-field parent columns rows) ;; columns list of column headers
    (super-new [parent parent] [style '(border)])
    (define make-col (lambda (d) (list d (new column% (parent this)
(header d) (rows rows)))))
    (define col-objects (map make-col columns))  ; listof (id column)
    ;; set-cell : row-num col-str val -> (or val #f)
    (define/public (set-cell row column val)
      (when (member column columns)
        (send (cadr (assoc column col-objects)) set-cell row val)
        )
      )
    (define/public (get-cell row column)
      (when (member column columns)
        (send (cadr (assoc column col-objects)) get-cell row)
        )
      )
    ))


;;testing
(define f (new frame% [label "dir test"]
               [width 400]
               [height 200]))
(define test (new table% (parent f) (rows '("1" "2" "3")) (columns
'("a" "b" "c"))))
(send test set-cell "3" "b" "val")
(send f show #t)