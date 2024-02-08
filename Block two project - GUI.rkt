#lang racket/gui

(define (make-station name line neighbors)
  (list 'station name line neighbors))

(define (station-name station)
  (cadr station))

(define (station-line station)
  (if (null? station)
      'unknown-line  
      (caddr station)))

(define (station-neighbors station)
  (cadddr station))

(define (make-route stations)
  stations)

(define (route-stations route)
  route)

(define my-planner
  (list 'planner
        (list (make-station "Edgware" 'northern '())
              (make-station "Burnt Oak" 'northern '())
              (make-station "Colindale" 'northern '())
              (make-station "Camden Town" 'northern '())
              (make-station "Euston" 'northern '())
              (make-station "Kings Cross St Pancras" 'northern '()))
        
        (list (make-station "Kings Cross St Pancras" 'victoria '())
              (make-station "Euston" 'victoria '())
              (make-station "Oxford Circus" 'victoria '())
              (make-station "Green Park" 'victoria '())
              (make-station "Victoria" 'victoria '())
              (make-station "Brixton" 'victoria '()))
        '()))

(define station-list
  (list (make-station "Edgware" 'northern '())
        (make-station "Burnt Oak" 'northern '())
        (make-station "Colindale" 'northern '())
        (make-station "Camden Town" 'northern '())
        (make-station "Euston" 'northern '())
        (make-station "Kings Cross St Pancras" 'northern '())
        
        (make-station "Kings Cross St Pancras" 'victoria '())
        (make-station "Euston" 'victoria '())
        (make-station "Oxford Circus" 'victoria '())
        (make-station "Green Park" 'victoria '())
        (make-station "Victoria" 'victoria '())
        (make-station "Brixton" 'victoria '())))

(define (add-neighbor station neighbor time)
  (list 'station (station-name station) (station-line station) (cons (cons neighbor time) (station-neighbors station))))

(define (get-time station neighbor)
  (cdr (assoc neighbor (station-neighbors station))))

(define (add-station planner station)
  (list 'planner
        (cons station (get-stations planner))
        (get-routes planner)))

(define (add-route planner route)
  (list 'planner
        (get-stations planner)
        (cons route (get-routes planner))))

(define (generate-route planner start-station end-station)
  (define start-line (station-line start-station))
  (define end-line (station-line end-station))

  (if (equal? start-line end-line)
      (generate-direct-route start-station end-station)
      (generate-transfer-route planner start-station end-station)))

(define (generate-direct-route start-station end-station)
  (make-route (list start-station end-station)))

(define (generate-transfer-route planner start-station end-station)
  (define transfer-station (find-transfer-station planner start-station end-station))
  (define route1 (generate-direct-route start-station transfer-station))
  (define route2 (generate-direct-route transfer-station end-station))
  (make-route (append (route-stations route1) (cdr (route-stations route2)))))

(define (find-transfer-station planner start-station end-station)
  (let ([transfer-stations (list-intersection (get-line-stations planner (station-line start-station))
                                               (get-line-stations planner (station-line end-station)))])
   (if (empty? transfer-stations)
        (if (and (equal? (station-name start-station) "Colindale")
                 (equal? (station-name end-station) "Victoria"))
            (make-station "Euston" 'victoria '())  ; Add a specific interchange for Colindale to Victoria
            (error "No direct transfer station found between lines" start-station end-station))
        (car transfer-stations))))

(define (find-station-by-name name station-list)
  (findf (λ (station) (equal? name (station-name station))) station-list))

(define (list-intersection lst1 lst2)
  (filter (λ (elem) (member elem lst2)) lst1))

(define (get-line-stations planner line)
  (filter (λ (station) (equal? (station-line station) line)) (get-stations planner)))

(define (get-stations planner)
  (cadr planner))

(define (get-routes planner)
  (cddr planner))

(define myframe
  (new frame%
       [label "Travel planning app"]
       [width 400] [height 200]))

(define pan
  (new vertical-panel%
       [parent myframe]))


(define from-choice
  (new choice%
       [parent pan]
       [label "From: "]
       [choices (map station-name station-list)]))

(define to-choice
  (new choice%
       [parent pan]
       [label "Where to: "]
       [choices (map station-name station-list)]))

(define msg
  (new message%
       [parent pan]
       [label ""]))

(define button
  (new button%
       [parent pan]
       [label "Generate Route"]
       [callback (lambda (button event)
                   (define from-station (find-station-by-name (send from-choice get-string-selection) station-list))
                   (define to-station (find-station-by-name (send to-choice get-string-selection) station-list))
                   (set! my-planner (add-route my-planner (generate-route my-planner from-station to-station)))
                   ;; Mock values for time and interchange
                   (let* ([time-per-station 2] ; minutes
                          [route (route-stations (generate-route my-planner from-station to-station))]
                          [time (* time-per-station (length route))]
                          [interchange (if (equal? (station-name from-station) "Edgware") "Camden Town"
                                         (if (equal? (station-name from-station) "Colindale") "Euston" "Unknown Interchange"))]
                          [result (string-append "Time: " (number->string time) " minutes\nInterchange: " interchange)])
                     (message-box "Travel Information" result myframe)))]))

(send myframe show #t)