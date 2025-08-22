(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-input (err u101))
(define-constant err-overflow (err u102))
(define-constant err-division-by-zero (err u103))

;; Maximum safe integer to prevent overflow
(define-constant max-safe-int u340282366920938463463374607431768211455)

;; Math constants (scaled by 10^6 for precision)
(define-constant math-e u2718281)  ;; e ~= 2.718281
(define-constant math-pi u3141592) ;; pi ~= 3.141592

;; Data variables for tracking usage
(define-data-var total-calculations uint u0)
(define-map calculation-history principal uint)

;; Function 1: Advanced Power Calculation with Overflow Protection
;; Calculates base^exponent with efficient exponentiation by squaring
(define-public (power-calculation (base uint) (exponent uint))
  (begin
    ;; Input validation
    (asserts! (> base u0) err-invalid-input)
    (asserts! (>= exponent u0) err-invalid-input)
    
    ;; Handle edge cases
    (if (is-eq exponent u0)
        (ok u1)  ;; Any number to the power of 0 is 1
        (if (is-eq exponent u1)
            (ok base)  ;; Any number to the power of 1 is itself
            (if (is-eq base u1)
                (ok u1)  ;; 1 to any power is 1
                (let ((result (try! (power-helper base exponent u1))))
                  (begin
                    ;; Update calculation tracking
                    (var-set total-calculations (+ (var-get total-calculations) u1))
                    (map-set calculation-history tx-sender 
                            (+ (default-to u0 (map-get? calculation-history tx-sender)) u1))
                    (ok result))))))))

;; Helper function for power calculation using exponentiation by squaring
(define-public (power-helper (base uint) (exp uint) (result uint))
  (if (is-eq exp u0)
      (ok result)
      (if (is-eq (mod exp u2) u1)
          ;; Odd exponent: multiply result by base
          (let ((new-result (try! (safe-multiply result base))))
            (power-helper (try! (safe-multiply base base)) (/ exp u2) new-result))
          ;; Even exponent: square the base and halve the exponent
          (power-helper (try! (safe-multiply base base)) (/ exp u2) result))))

;; Function 2: Square Root Approximation using Newton's Method
;; Calculates square root using iterative approximation with precision control
(define-public (sqrt-approximation (number uint) (precision uint))
  (begin
    ;; Input validation
    (asserts! (> number u0) err-invalid-input)
    (asserts! (and (> precision u0) (<= precision u10)) err-invalid-input)
    
    ;; Handle perfect squares and edge cases
    (if (is-eq number u1)
        (ok u1)
        (let ((result (try! (newton-sqrt-method number precision (/ number u2)))))
          (begin
            ;; Update calculation tracking
            (var-set total-calculations (+ (var-get total-calculations) u1))
            (map-set calculation-history tx-sender 
                    (+ (default-to u0 (map-get? calculation-history tx-sender)) u1))
            (ok result))))))

;; Newton's method for square root calculation
(define-private (newton-sqrt-method (number uint) (iterations uint) (guess uint))
  (if (is-eq iterations u0)
      (ok guess)
      (let ((new-guess (/ (+ guess (/ number guess)) u2)))
        (newton-sqrt-method number (- iterations u1) new-guess))))

;; Utility function: Safe multiplication with overflow check
(define-private (safe-multiply (a uint) (b uint))
  (let ((result (* a b)))
    (if (and (> a u0) (> (/ max-safe-int a) b))
        (err err-overflow)
        (ok result))))

;; Read-only functions for contract information
(define-read-only (get-total-calculations)
  (ok (var-get total-calculations)))

(define-read-only (get-user-calculations (user principal))
  (ok (default-to u0 (map-get? calculation-history user))))

(define-read-only (get-math-constants)
  (ok {
    e: (var-get math-e),
    pi: (var-get math-pi)
  }))

;; Administrative functions
(define-read-only (get-contract-info)
  (ok {
    owner: contract-owner,
    total-calculations: (var-get total-calculations),
    math-constants: {
      e: math-e,
      pi: math-pi
    }
  }))

;; Emergency function to reset calculation counters (owner only)
(define-public (reset-calculations)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set total-calculations u0)
    (ok true)))