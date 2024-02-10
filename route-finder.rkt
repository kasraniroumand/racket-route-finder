#lang racket

(require racket/gui)

(define network
  '(
    ("Hendon Central" "Brent Cross" "Northern Line" 2)
    ("Brent Cross" "Golders Green" "Northern Line" 3)
    ("Golders Green" "Hampstead" "Northern Line" 4)
    ("Hampstead" "Belsize Park" "Northern Line" 3)
    ("Belsize Park" "Chalk Farm" "Northern Line" 2)
    ("Chalk Farm" "Camden Town" "Northern Line" 1)
    ("Camden Town" "Mornington Crescent" "Northern Line" 2)
    ("Mornington Crescent" "Euston" "Northern Line" 2)
    ("Euston" "Warren Street" "Northern Line" 1)
    ("Warren Street" "Goodge Street" "Northern Line" 2)
    ("Goodge Street" "Tottenham Court Road"  "Northern Line" 1)
    ("Tottenham Court Road" "Leicester Square"  "Northern Line" 1)
    ("Leicester Square" "Charing Cross" "Northern Line" 2)
    ("Charing Cross" "Embankment" "Northern Line" 1)
    ("Embankment" "Waterloo" "Northern Line" 2)
    ("Waterloo" "Kennington" "Northern Line" 3)
    ("Kennington" "Oval" "Northern Line" 3)
    ("Oval" "Stockwell" "Northern Line" 2)
    ("Euston" "Kings Cross St Pancras" "Northern Line Bank Branch" 2)
    ("Kings Cross St Pancras" "Angel" "Northern Line Bank Branch" 3)
    ("Angel" "Old Street" "Northern Line Bank Branch" 2)
    ("Old Street" "Moorgate" "Northern Line Bank Branch" 2)
    ("Moorgate" "Bank" "Northern Line Bank Branch" 2)
    ("Bank" "London Bridge" "Northern Line Bank Branch" 2)
    ("London Bridge" "Borough" "Northern Line Bank Branch" 2)
    ("Borough" "Elephant and Castle" "Northern Line Bank Branch" 2)
    ("Elephant and Castle" "Kennington" "Northern Line Bank Branch" 2)
    ("Camden Town" "Kentish Town" "Northern Line" 2)
    ("Kentish Town" "Tuffnell Park" "Northern Line" 1)
    ("Tuffnell Park" "Archway" "Northern Line" 2)
    ("Archway" "Highgate" "Northern Line" 3)
))

(define station%
  (class object%
    (super-new)3
    (init-field name) ; Existing field for the station name
    (init-field line-name) ; New field for the line name
    (define/public (get-name) name) ; Getter for the station name
    (define/public (get-line-name) line-name) ; Getter for the line name
    ;; You can add more methods or fields as needed
  ))

(define connection%
  (class object%
   (super-new)
    (init-field station1 station2 time)
    (define/public (get-station1) station1)
    (define/public (get-station2) station2)
    (define/public (get-time) time)))

(define graph%
  (class object%
    (super-new)
    ;; Fields
    (define stations-hash (make-hash))
    (define adj-list (make-hash))

    ; Add or get a station object by name
    (define/public add-or-get-station
      (lambda (station-name line-name)
        (or (hash-ref stations-hash station-name #f)
            (let ((station (new station% [name station-name] [line-name line-name])))
              (hash-set! stations-hash station-name station)
              station))))

    ; Add a connection to the graph
    (define/public add-connection
      (lambda ( station1-name station2-name line-name time)
        (let* ((station1 (add-or-get-station station1-name line-name))
              (station2 (add-or-get-station station2-name line-name))
              (conn (new connection% [station1 station1] [station2 station2] [time time])))
        (hash-update! adj-list (send station1 get-name) (curry cons conn) (list))
        (hash-update! adj-list (send station2 get-name) (curry cons conn) (list)))
      )
    )

    ; populate the graph with connections
    (define/public populate-graph
      (lambda (network)
        (for-each
        (lambda (connection)
          (let ((station1-name (first connection))
                (station2-name (second connection))
                (line-name (third connection))
                (time (fourth connection)))
            (add-connection  station1-name station2-name line-name time)))
        network)))

    ; print the graph
    (define/public (display-graph)
      (for-each
        (lambda (pair)
          (let* ((station-name (car pair))
                (station-object (hash-ref stations-hash station-name #f)) ;; Retrieve the station object using its name
                (line-name (send station-object get-line-name)) ;; Retrieve the line name of the station
                (connections (cdr pair))) ;; connections is a list of connection% objects
            (printf "~a on line ~a connects to:\n" station-name line-name)
            (for-each
              (lambda (conn)
                (let* ((dest-station-object (if (string=? station-name (send (send conn get-station1) get-name))
                                                (send conn get-station2)
                                                (send conn get-station1)))
                      (dest-station-name (send dest-station-object get-name))
                      (dest-line-name (send dest-station-object get-line-name))
                      (time (send conn get-time)))
                  (printf "  - ~a on line ~a in ~a minutes\n" dest-station-name dest-line-name time)))
              connections)))
        (hash->list adj-list)))

    ; find all paths between two stations
    (define/public (find-two-paths start-station-name end-station-name)
  ;; Helper function to perform BFS and find up to two paths
  (define (bfs queue visited paths-found)
    (if (or (empty? queue) (>= (length paths-found) 2))
        paths-found
        (let* ((current (first queue))
               (current-station-name (first current))
               (path (second current))
               (next-steps paths-found))
          (set! visited (set-add visited current-station-name))
          (for-each
            (lambda (conn)
              (let* ((next-station (if (string=? current-station-name (send (send conn get-station1) get-name))
                                        (send conn get-station2)
                                        (send conn get-station1)))
                     (next-station-name (send next-station get-name)))
                ;; Avoid cycles and limit search to when fewer than two paths are found
                (when (and (not (set-member? visited next-station-name))
                           (< (length next-steps) 2))
                  (if (string=? next-station-name end-station-name)
                      (set! next-steps (cons (reverse (cons conn path)) next-steps))
                      (set! queue (append queue (list (list next-station-name (cons conn path)))))))))
            (hash-ref adj-list current-station-name '()))
          (bfs (rest queue) visited next-steps))))

  ;; Initialize the BFS search with a queue containing the start station, empty visited set, and empty list of found paths
  (bfs (list (list start-station-name '())) (set) '()))


  )
)

(define (print-paths paths)
  (let ((path-counter 1))
    (for-each
      (lambda (path)
        (printf "Path ~a:\n" path-counter) ; Corrected to use ~a for path-counter
        (for-each
          (lambda (conn)
            (let ((station1-name (send (send conn get-station1) get-name))
                  (line-name1 (send (send conn get-station1) get-line-name))
                  (station2-name (send (send conn get-station2) get-name))
                  (line-name2 (send (send conn get-station2) get-line-name))
                  (time (send conn get-time)))
              (printf "  ~a in ~a to ~a in ~a in ~a minutes\n" station1-name line-name1 station2-name line-name2 time))) ; Corrected to use ~a for all arguments
          (reverse path))
        (newline)
        (set! path-counter (+ path-counter 1)))
      paths)))

(define (get-shortest-path paths)
  (let ((shortest-path '())
        (shortest-time +inf.0)) ; Initialize shortest time to positive infinity
    (for-each
      (lambda (path)
        (let ((total-time (apply + (map (lambda (conn) (send conn get-time)) path)))) ; Sum up the time of each connection in the path
          (when (< total-time shortest-time) ; If the total time of this path is less than the shortest time found so far
            (set! shortest-time total-time) ; Update the shortest time
            (set! shortest-path path)))) ; Update the shortest path
      paths)
    shortest-path)) ; Return the shortest path found

(define (get-path-with-fewest-station-changes two-paths)
  (if (null? two-paths) ; Check if two-paths is empty
      '() ; Return an empty list if there are no paths
      (let* ((path1 (first two-paths)) ; First path in the list
             (path2 (if (null? (rest two-paths)) '() (second two-paths))) ; Second path if it exists
             (length1 (length path1)) ; Length of the first path
             (length2 (length path2))) ; Length of the second path
        (cond
          ((and (null? path1) (null? path2)) '()) ; If both paths are empty or do not exist
          ((null? path2) path1) ; If only the first path exists
          ((< length1 length2) path1) ; If the first path has fewer stations
          (else path2)))) ; If the second path has fewer or equal stations
)

(define my-graph (new graph%))
(send my-graph populate-graph network)

;; Create the main window
(define main-frame (new frame%
                     [label "Route Finder"] ; Title of the window
                     [width 300]
                     [height 300]))

(define second-frame (new frame%
                     [label "Route Finder"] ; Title of the window
                     [width 300]
                     [height 300]))

;; Create source input field
(define source-input (new text-field%
                        [parent main-frame]
                        [label "Source Input"]))

;; Create destination input field
(define destination-input (new text-field%
                             [parent main-frame]
                             [label "Destination Input"]))


(define two-paths '())
(define shortest-path '())
(define path-with-fewest-station-changes '())

(define (total-time path)
  (apply + (map (lambda (conn) (send conn get-time)) path)))

(define (show-popup1 button event)
                                          (define shortest-path-string
                                        (apply string-append
                                               (map (lambda (conn)
                                                      (format "~a in ~a to ~a in ~a in ~a minutes\n"
                                                              (send (send conn get-station1) get-name)
                                                              (send (send conn get-station1) get-line-name)
                                                              (send (send conn get-station2) get-name)
                                                              (send (send conn get-station2) get-line-name)
                                                              (send conn get-time)))
                                                     shortest-path)))



  (define popup-frame1 (new frame% [label "Shortest Path"] [width 500] [height 500]))
    (define text-field2 (new text%))
    (define quickest-path-output
      (new editor-canvas%
          [parent popup-frame1]
          [editor text-field2]
          [min-width 200]
          [min-height 100]))
    (send text-field2 insert shortest-path-string)



  (send popup-frame1 show #t))

(define (show-popup2 button event)

 (define path-with-fewest-station-changes-string
                                          (apply string-append
                                                (map (lambda (conn)
                                                        (format "~a in ~a to ~a in ~a in ~a minutes\n"
                                                                (send (send conn get-station1) get-name)
                                                                (send (send conn get-station1) get-line-name)
                                                                (send (send conn get-station2) get-name)
                                                                (send (send conn get-station2) get-line-name)
                                                                (send conn get-time)))
                                                       path-with-fewest-station-changes)))



  (define popup-frame2 (new frame% [label "fewest changes"] [width 500] [height 500]))

  (define text-field (new text%))
  (define quickest-station-output
    (new editor-canvas%
        [parent popup-frame2]
        [editor text-field]
        [min-width 200]
        [min-height 100]))
    (send text-field insert path-with-fewest-station-changes-string)
  (send popup-frame2 show #t))

;; Create the process button
(define (process-button-callback button event)
  ;; Get the text from the source and destination inputs
  (define source-text (send source-input get-value))
  (define destination-text (send destination-input get-value))

  (printf "Source: ~a\n" source-text)
  (printf "Destination: ~a\n" destination-text)

  ;; Find the shortest path
  (set! two-paths (send my-graph find-two-paths source-text destination-text))
  (set! shortest-path (get-shortest-path two-paths))
  (set! path-with-fewest-station-changes (get-path-with-fewest-station-changes two-paths))

  ;; Update the labels of the buttons
  (send process-button1 set-label (format "Shortest path: ~a minutes" (total-time shortest-path)))
  (send process-button2 set-label (format "Path with fewest station changes: ~a minutes" (total-time path-with-fewest-station-changes)))
)

;; Create input fields for the station details
;; Create input fields for the new station and connection details
(define new-station-name-input (new text-field% [parent second-frame] [label "New Station Name"]))
(define new-station-line-name-input (new text-field% [parent second-frame] [label "New Station Line Name"]))
(define connected-station-name-input (new text-field% [parent second-frame] [label "Connected Station Name"]))
(define connection-line-name-input (new text-field% [parent second-frame] [label "Connection Line Name"]))
(define connection-time-input (new text-field% [parent second-frame] [label "Travel Time (minutes)"]))

;; add connection button
(define add-connection-button (new button%
                               [parent second-frame]
                               [label "Add Connection"]
                               [callback (lambda (button event)
                                           (define new-station-name (send new-station-name-input get-value))
                                           (define new-station-line-name (send new-station-line-name-input get-value))
                                           (define connected-station-name (send connected-station-name-input get-value))
                                           (define connection-line-name (send connection-line-name-input get-value))
                                           (define connection-time (string->number (send connection-time-input get-value)))
                                           (send my-graph add-connection new-station-name connected-station-name connection-line-name connection-time)
                                           )] ;; close frame2
))

;; Show the main window
(define print-path-button (new button%
                                [parent second-frame]
                                [label "Print Path"]
                                [callback (lambda (button event)
                                            ;; Get the text from the source and destination inputs
                                            (send my-graph display-graph ))]
                                          ))




(define my-button
  (new button%
       [parent main-frame]
       [label "Computer Route"]
       [callback process-button-callback]))

(define process-button1
  (new button%
       [parent main-frame]
       [label (format "Shortest path: ~a minutes" (total-time shortest-path))]
       [callback show-popup1]))

(define process-button2
  (new button%
       [parent main-frame]
       [label (format "Path with fewest station changes: ~a minutes" (total-time path-with-fewest-station-changes))]
       [callback show-popup2]))

(send second-frame show #t)

(send main-frame show #t)
