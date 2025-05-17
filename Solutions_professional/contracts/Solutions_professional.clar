;; Define data variables
(define-data-var contract-owner principal tx-sender)
(define-map performers principal 
  {
    name: (string-utf8 100),
    hourly-rate: uint,
    total-earned: uint,
    active: bool
  }
)

(define-map performances uint 
  {
    performer: principal,
    venue: (string-utf8 100),
    duration: uint,
    payment-amount: uint,
    payment-status: (string-utf8 20),
    date: uint
  }
)

(define-data-var performance-counter uint u0)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-PERFORMER-NOT-FOUND u2)
(define-constant ERR-PERFORMANCE-NOT-FOUND u3)
(define-constant ERR-INSUFFICIENT-FUNDS u4)
(define-constant ERR-INVALID-DURATION u5)
(define-constant ERR-INVALID-PERCENTAGES u6)
(define-constant ERR-LIST-SIZE-MISMATCH u7)
;; Register a new performer
(define-public (register-performer (name (string-utf8 100)) (hourly-rate uint))
  (begin
    (map-set performers tx-sender {
      name: name,
      hourly-rate: hourly-rate,
      total-earned: u0,
      active: true
    })
    (ok true)
  )
)

;; Update performer details
(define-public (update-performer-rate (new-rate uint))
  (let ((performer-data (default-to 
                        {
                          name: u"",
                          hourly-rate: u0,
                          total-earned: u0,
                          active: false
                        } 
                        (map-get? performers tx-sender))))
    (if (get active performer-data)
      (begin
        (map-set performers tx-sender (merge performer-data { hourly-rate: new-rate }))
        (ok true))
      (err ERR-PERFORMER-NOT-FOUND)
    )
  )
)
;; Schedule a new performance
(define-public (schedule-performance (venue (string-utf8 100)) (duration uint) (date uint))
  (let ((performer-data (default-to 
                        {
                          name: u"",
                          hourly-rate: u0,
                          total-earned: u0,
                          active: false
                        } 
                        (map-get? performers tx-sender))))
    (if (and (get active performer-data) (> duration u0))
      (let ((payment-amount (* duration (get hourly-rate performer-data))))
        (var-set performance-counter (+ (var-get performance-counter) u1))
        (map-set performances (var-get performance-counter) {
          performer: tx-sender,
          venue: venue,
          duration: duration,
          payment-amount: payment-amount,
          payment-status: u"scheduled",
          date: date
        })
        (ok (var-get performance-counter)))
      (if (<= duration u0)
        (err ERR-INVALID-DURATION)
        (err ERR-PERFORMER-NOT-FOUND))
    )
  )
)
;; Make payment for a performance
(define-public (make-payment (performance-id uint))
  (let ((performance-data (default-to 
                          {
                            performer: tx-sender,
                            venue: u"",
                            duration: u0,
                            payment-amount: u0,
                            payment-status: u"",
                            date: u0
                          } 
                          (map-get? performances performance-id))))
    (if (is-eq (get payment-status performance-data) u"scheduled")
      (let ((performer-data (default-to 
                            {
                              name: u"",
                              hourly-rate: u0,
                              total-earned: u0,
                              active: false
                            }
                            (map-get? performers (get performer performance-data)))))
        (if (>= (stx-get-balance tx-sender) (get payment-amount performance-data))
          (begin
            (try! (stx-transfer? (get payment-amount performance-data) tx-sender (get performer performance-data)))
            (map-set performances performance-id (merge performance-data { payment-status: u"paid" }))
            (map-set performers (get performer performance-data) 
              (merge performer-data { total-earned: (+ (get total-earned performer-data) (get payment-amount performance-data)) }))
            (ok true))
          (err ERR-INSUFFICIENT-FUNDS)))
      (err ERR-PERFORMANCE-NOT-FOUND)
    )
  )
)

;; Get performer details
(define-read-only (get-performer-details (performer-address principal))
  (default-to 
    {
      name: u"",
      hourly-rate: u0,
      total-earned: u0,
      active: false
    }
    (map-get? performers performer-address)
  )
)
;; Get performance details
(define-read-only (get-performance-details (performance-id uint))
  (default-to 
    {
      performer: tx-sender,
      venue: u"",
      duration: u0,
      payment-amount: u0,
      payment-status: u"",
      date: u0
    }
    (map-get? performances performance-id)
  )
)

;; Get all performances for a performer
(define-read-only (get-performer-earnings (performer-address principal))
  (let ((performer-data (default-to 
                        {
                          name: u"",
                          hourly-rate: u0,
                          total-earned: u0,
                          active: false
                        }
                        (map-get? performers performer-address))))
    (get total-earned performer-data)
  )
)
;; Generate receipt for a performance
(define-read-only (generate-receipt (performance-id uint))
  (let ((performance-data (default-to 
                          {
                            performer: tx-sender,
                            venue: u"",
                            duration: u0,
                            payment-amount: u0,
                            payment-status: u"",
                            date: u0
                          }
                          (map-get? performances performance-id))))
    (if (is-eq (get payment-status performance-data) u"paid")
      (let ((performer-data (default-to 
                            {
                              name: u"",
                              hourly-rate: u0,
                              total-earned: u0,
                              active: false
                            }
                            (map-get? performers (get performer performance-data)))))
        {
          performer-name: (get name performer-data),
          performer-address: (get performer performance-data),
          venue: (get venue performance-data),
          duration: (get duration performance-data),
          hourly-rate: (get hourly-rate performer-data),
          total-payment: (get payment-amount performance-data),
          date: (get date performance-data),
          status: (get payment-status performance-data)
        })
      { 
        performer-name: u"",
        performer-address: tx-sender,
        venue: u"",
        duration: u0,
        hourly-rate: u0,
        total-payment: u0,
        date: u0,
        status: u"not paid"
      }
    )
  )
)

;; Cancel a scheduled performance
(define-public (cancel-performance (performance-id uint))
  (let ((performance-data (default-to 
                          {
                            performer: tx-sender,
                            venue: u"",
                            duration: u0,
                            payment-amount: u0,
                            payment-status: u"",
                            date: u0
                          }
                          (map-get? performances performance-id))))
    (if (and 
          (is-eq (get payment-status performance-data) u"scheduled")
          (is-eq (get performer performance-data) tx-sender))
      (begin
        (map-set performances performance-id (merge performance-data { payment-status: u"cancelled" }))
        (ok true))
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Split payment between multiple performers
(define-public (split-payment (performance-id uint) (performer-list (list 10 principal)) (percentages (list 10 uint)))
  (let ((performance-data (default-to 
                          {
                            performer: tx-sender,
                            venue: u"",
                            duration: u0,
                            payment-amount: u0,
                            payment-status: u"",
                            date: u0
                          }
                          (map-get? performances performance-id))))
    (if (is-eq (get payment-status performance-data) u"scheduled")
      (if (is-eq (sum-percentages percentages u0) u100)
        (if (is-eq (len performer-list) (len percentages))
          (begin
            (map-set performances performance-id (merge performance-data { payment-status: u"split-paid" }))
            (process-payments performer-list percentages (get payment-amount performance-data)))
          (err ERR-LIST-SIZE-MISMATCH))
        (err ERR-INVALID-PERCENTAGES)) ;; Percentages don't add up to 100
      (err ERR-PERFORMANCE-NOT-FOUND)
    )
  )
)

;; Helper function to sum the percentages
(define-private (sum-percentages (percent-list (list 10 uint)) (index uint))
  (let ((sum u0)
        (p0 (default-to u0 (element-at percent-list u0)))
        (p1 (default-to u0 (element-at percent-list u1)))
        (p2 (default-to u0 (element-at percent-list u2)))
        (p3 (default-to u0 (element-at percent-list u3)))
        (p4 (default-to u0 (element-at percent-list u4)))
        (p5 (default-to u0 (element-at percent-list u5)))
        (p6 (default-to u0 (element-at percent-list u6)))
        (p7 (default-to u0 (element-at percent-list u7)))
        (p8 (default-to u0 (element-at percent-list u8)))
        (p9 (default-to u0 (element-at percent-list u9))))
    (+ p0 (+ p1 (+ p2 (+ p3 (+ p4 (+ p5 (+ p6 (+ p7 (+ p8 p9)))))))))
  )
)

;; Process payments for all performers by using the specific indices
(define-private (process-payments (performer-list (list 10 principal)) 
                                 (percentages (list 10 uint)) 
                                 (total-amount uint))
  (let ((length (len performer-list)))
    (if (> length u0)
      (begin
        (try! (process-payment-at-index performer-list percentages total-amount u0))
        (if (> length u1)
          (begin
            (try! (process-payment-at-index performer-list percentages total-amount u1))
            (if (> length u2)
              (begin
                (try! (process-payment-at-index performer-list percentages total-amount u2))
                (if (> length u3)
                  (begin
                    (try! (process-payment-at-index performer-list percentages total-amount u3))
                    (if (> length u4)
                      (begin
                        (try! (process-payment-at-index performer-list percentages total-amount u4))
                        (if (> length u5)
                          (begin
                            (try! (process-payment-at-index performer-list percentages total-amount u5))
                            (if (> length u6)
                              (begin
                                (try! (process-payment-at-index performer-list percentages total-amount u6))
                                (if (> length u7)
                                  (begin
                                    (try! (process-payment-at-index performer-list percentages total-amount u7))
                                    (if (> length u8)
                                      (begin
                                        (try! (process-payment-at-index performer-list percentages total-amount u8))
                                        (if (> length u9)
                                          (process-payment-at-index performer-list percentages total-amount u9)
                                          (ok true)))
                                      (ok true)))
                                  (ok true)))
                              (ok true)))
                          (ok true)))
                      (ok true)))
                  (ok true)))
              (ok true)))
          (ok true)))
      (ok true))
  )
)

;; Process payment at a specific index
(define-private (process-payment-at-index (performer-list (list 10 principal)) 
                                          (percentages (list 10 uint)) 
                                          (total-amount uint)
                                          (index uint))
  (let ((current-performer (unwrap! (element-at performer-list index) (err ERR-NOT-AUTHORIZED)))
        (current-percentage (unwrap! (element-at percentages index) (err ERR-INVALID-PERCENTAGES)))
        (amount (/ (* total-amount current-percentage) u100))
        (performer-data (default-to 
                        {
                          name: u"",
                          hourly-rate: u0,
                          total-earned: u0,
                          active: false
                        }
                        (map-get? performers current-performer))))
    (if (and (get active performer-data) (>= (stx-get-balance tx-sender) amount))
      (begin
        (try! (stx-transfer? amount tx-sender current-performer))
        (map-set performers current-performer 
          (merge performer-data { total-earned: (+ (get total-earned performer-data) amount) }))
        (ok true))
      (err ERR-PERFORMER-NOT-FOUND)
    )
  )
)