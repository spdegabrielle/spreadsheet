#lang racket/gui

(define frame
  (instantiate frame% ("Spreadsheet") (width 600) (height 400)))

(define menu-bar
  (instantiate menu-bar% (frame)))

(define file-menu
  (instantiate menu% ("File" menu-bar)))
(define (load-ss path)
  void)
(define open-item
  (instantiate menu-item%
    ("Open..."
     file-menu
     (lambda (_ event) 
       (cond
         [(get-file)
          =>
          load-ss])))
    (shortcut #\O)))

(define save-item
  (instantiate menu-item%
    ("Save as..."
     file-menu
     (lambda (_ event)
       (cond
         [(put-file)
          =>
          (lambda (filename)
            (when (file-exists? filename)
              (delete-file filename))
            (let ([p (open-output-file filename)])
              #;(for ([i cols])
                  (let ([v (hash-map (vector-ref vec i) (lambda (row cell) (list row (ss-cell-expr cell))))])
                    #;(printf "~a~n" v)
                    (write v p)))
              (flush-output p)
              (close-output-port p)))])))
    (shortcut #\S)))

(define edit-menu
  (instantiate menu% ("Edit" menu-bar)))

(define text-field
  (instantiate text-field%
    ("Formula:"
     frame
     (lambda (this control-event)
       (case (send control-event get-event-type)
         [(text-field-enter) (send canvas new-expression (send this get-value))])))))

(define value-field
  (instantiate text-field%
    ("Value:" frame void)))
(send value-field enable #f)

(define rows 10)
(define cols 10)


(define spreadsheet-canvas% 
  (class canvas%
    (super-instantiate ())
    
    (inherit
      refresh
      get-dc
      get-scroll-pos
      get-client-size
      set-scroll-range
      set-scroll-page
      init-manual-scrollbars)
    
    
    (field
     (column-widths (make-hash))
     [can-refresh? #t]
     [char-width (inexact->exact (send offscreen-dc get-char-width))]
     [cell-width (* chars-per-cell char-width)]
     [cell-height (+ 2 (inexact->exact (send offscreen-dc get-char-height)))]
     
     [left-margin (* 5 char-width)]
     [top-margin cell-height]
     
     ;       [canvas-width-rcvr (event-receiver)]
     ;       [canvas-height-rcvr (event-receiver)]
     ;       [h-scroll-rcvr (event-receiver)]
     ;       [v-scroll-rcvr (event-receiver)]
     ;       [mouse-x-rcvr (event-receiver)]
     ;       [mouse-y-rcvr (event-receiver)]
     ;       [left-clicks (event-receiver)]
     ;       [left-releases (event-receiver)]
     ;       [key-events (event-receiver)]
     
     ;       [canvas-width~ (hold canvas-width-rcvr)]
     ;       [canvas-height~ (hold canvas-height-rcvr)]
     ;       
     ;       [mouse-x~ (hold mouse-x-rcvr 0)]
     ;      [mouse-y~ (hold mouse-y-rcvr 0)]
     
     ;       [left-button-down~ (hold (merge-e (left-clicks   . -=> . #t)
     ;                                        (left-releases . -=> . #f))
     ;                                #f)]
     
     [h-chars-per-page~ (quotient (- canvas-width~ left-margin) char-width)]
     [v-cells-per-page~ (quotient (- canvas-height~ top-margin) cell-height)]
     [h-scroll-range~ (max 0 (- (* cols chars-per-cell) h-chars-per-page~))]
     [v-scroll-range~ (max 0 (- rows v-cells-per-page~))]
     
     ;    [h-scroll-pos~ (hold h-scroll-rcvr 0)]
     ;    [v-scroll-pos~ (hold v-scroll-rcvr 0)]
     [h-scroll-cells~ (quotient h-scroll-pos~ chars-per-cell)]
     [h-scroll-offset~ (* char-width (remainder h-scroll-pos~ chars-per-cell))]
     [v-scroll-cells~ v-scroll-pos~]
     
     [mouse-row~ (y->row mouse-y~)]
     [mouse-col~ (x->col mouse-x~)]
     
     [first-vis-row~ (y->row (add1 top-margin))]
     [last-vis-row~ (y->row (sub1 canvas-height~))]
     [first-vis-col~ (x->col (add1 left-margin))]
     [last-vis-col~ (x->col (sub1 canvas-width~))]
     
     [start-sel-row~
      (accum-b
       (merge-e
        (left-clicks . -=> . (lambda (_) (value-now mouse-row~)))
        (key-events  . ==> . (lambda (key)
                               (lambda (prev)
                                 (case (send key get-key-code)
                                   [(up) (max 0 (sub1 prev))]
                                   [(down) (min (sub1 rows) (add1 prev))]
                                   [else prev])))))
       0)]
     [start-sel-col~
      (accum-b
       (merge-e
        (left-clicks . -=> . (lambda (_) (value-now mouse-col~)))
        (key-events  . ==> . (lambda (key)
                               (lambda (prev)
                                 (case (send key get-key-code)
                                   [(left) (max 0 (sub1 prev))]
                                   [(right) (min (sub1 cols) (add1 prev))]
                                   [else prev])))))
       0)]
     
     [cur-sel-row~
      (hold (merge-e
             (changes start-sel-row~)
             ((changes start-sel-col~) . -=> . (value-now start-sel-row~))
             ((changes mouse-row~) . =#> . (lambda (_)
                                             left-button-down~))) 0)]
     [cur-sel-col~
      (hold (merge-e
             (changes start-sel-col~)
             ((changes start-sel-row~) . -=> . (value-now start-sel-col~))
             ((changes mouse-col~) . =#> . (lambda (_)
                                             left-button-down~))) 0)]
     
     [scrollbar-updater
      (list
       (lift-strict (lambda (pg) (set-scroll-page 'horizontal (clip 1 (- pg chars-per-cell -1) 10000))) h-chars-per-page~)
       (lift-strict (lambda (pg) (set-scroll-page 'vertical (clip 1 (sub1 pg) 10000))) v-cells-per-page~)
       (lift-strict (lambda (rng) (set-scroll-range 'horizontal (clip 1 rng 10000))) h-scroll-range~)
       (lift-strict (lambda (rng) (set-scroll-range 'vertical (clip 1 rng 10000))) v-scroll-range~))]
     
     [scroller ((merge-e (changes h-scroll-pos~)
                         (changes v-scroll-pos~)) . -=> . (refresh))]
     
     [v-auto-scroller (merge-e
                       ((while-e (and left-button-down~
                                      (>= cur-sel-row~ last-vis-row~)
                                      (< cur-sel-row~ (sub1 rows))
                                      (not (= cur-sel-row~ start-sel-row~))) 50)
                        . -=> . (set-scroll-pos 'vertical (add1 (value-now v-scroll-pos~))))
                       ((while-e (and left-button-down~
                                      (<= cur-sel-row~ first-vis-row~)
                                      (> cur-sel-row~ 0)
                                      (not (= cur-sel-row~ start-sel-row~))) 50)
                        . -=> . (set-scroll-pos 'vertical (sub1 (value-now v-scroll-pos~))))
                       (key-events
                        . ==> .
                        (lambda (ev)
                          (case (send ev get-key-code)
                            [(prior) (set-scroll-pos 'vertical (max 0 (- (value-now v-scroll-pos~) (value-now v-cells-per-page~))))]
                            [(next) (set-scroll-pos 'vertical (min (value-now v-scroll-range~)
                                                                   (+ (value-now v-scroll-pos~) (value-now v-cells-per-page~))))]))))]
     
     [h-auto-scroller (merge-e
                       ((while-e (and left-button-down~
                                      (>= cur-sel-col~ last-vis-col~)
                                      (< h-scroll-pos~ h-scroll-range~)) 50)
                        . -=> . (set-scroll-pos 'horizontal (+ 3 (value-now h-scroll-pos~))))
                       ((while-e (and left-button-down~
                                      (<= cur-sel-col~ first-vis-col~)
                                      (> h-scroll-pos~ 0)) 50)
                        . -=> . (set-scroll-pos 'horizontal (+ -3 (value-now h-scroll-pos~)))))]
     ;       
     
     
     [focuser ((key-events . =#> . (lambda (ev) (eq? #\return (send ev get-key-code))))
               . -=> . (send text-field focus))]
     
     [text-field-switcher (lift-strict (lambda (row col)
                                         (unless (or (negative? row)
                                                     (negative? col))
                                           (send text-field set-value (ss-get-cell-text row col))))
                                       start-sel-row~ start-sel-col~)]
     
     [light-steel-blue (make-object color% "LightSteelBlue")]
     [lavender (make-object color% "Lavender")]
     [white (make-object color% "White")]
     [line-pen (make-object pen% (make-object color% "DimGray") 1 'solid)]
     [light-gray (make-object color% "LightGray")]
     [trans-pen (make-object pen%)]
     [default-font (send offscreen-dc get-font)]
     [label-font (make-object font% 11 'roman 'normal 'bold)]
     [gray-brush (make-object brush% light-gray 'solid)]
     [highlight-brush (make-object brush% lavender 'solid)]
     [selected-brush (make-object brush% light-steel-blue 'solid)]
     [clear-brush (make-object brush% white 'solid)])
    
    
    (define/private (x->col x)
      (if (> x left-margin)
          (+ h-scroll-cells~ (quotient (+ (- x left-margin) h-scroll-offset~) cell-width))
          -1))
    
    (define/private (y->row y)
      (if (> y top-margin)
          (+ v-scroll-cells~ (quotient (- y top-margin) cell-height))
          -1))
    
    (define/private (row->y-top row)
      (+ (* cell-height (- row v-scroll-cells~))
         top-margin))
    
    (define/private (col->x-left col)
      (h-scroll-cells~ h-scroll-offset~)
      (+ (* (- col h-scroll-cells~) cell-width)
         (- h-scroll-offset~)
         left-margin))
    ;      
    
    
    
    ))


(define canvas
  (instantiate spreadsheet-canvas% (frame) (style (list 'hscroll 'vscroll))))

(send frame show #t)
(send canvas focus)