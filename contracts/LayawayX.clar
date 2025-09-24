;; layaway-store.clar
;; Decentralized Layaway Store
;; Sellers list items. Buyers pay in installments. Refunds with penalty. Seller can reclaim on failure.

(define-data-var next-item-id uint u0)

;; Items: id -> (seller principal) (price uint) (deadline uint) (claimed bool) (active bool)
(define-map items 
  { id: uint }
  {
    price: uint,
    seller: principal,
    deadline: uint,
    claimed: bool,
    active: bool
  })

(define-map layaways
  { id: uint }
  {
    buyer: principal,
    paid: uint,
    active: bool
  })

;; Cancellation fee percent in whole percentages (e.g., 10 = 10%)
(define-constant CANCELLATION_FEE_PERCENT u10)

;; Error codes
(define-constant ERR_NOT_FOUND (err u100))
(define-constant ERR_NOT_AUTHORIZED (err u101))
(define-constant ERR_ITEM_NOT_FOUND (err u102))
(define-constant ERR_NO_LAYAWAY (err u103))
(define-constant ERR_NOT_BUYER (err u104))
(define-constant ERR_NOT_SELLER (err u105))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u106))
(define-constant ERR_ALREADY_CLAIMED (err u107))
(define-constant ERR_DEADLINE_PASSED (err u108))
(define-constant ERR_ITEM_INACTIVE (err u109))
(define-constant ERR_INVALID_ID (err u110))
(define-constant ERR_INVALID_AMOUNT (err u111))
(define-constant ERR_NOT_ITEM_OWNER (err u112))
(define-constant ERR_ACTIVE_LAYAWAY (err u113))
(define-constant ERR_LAYAWAY_NOT_FOUND (err u114))
(define-constant ERR_ALREADY_HAS_LAYAWAY (err u115))
(define-constant ERR_TRANSFER_FAILED (err u116))
(define-constant ERR_DEADLINE_NOT_PASSED (err u117))

;; Validation functions
(define-private (validate-uint (value uint))
  (begin
    (asserts! (> value u0) ERR_INVALID_AMOUNT)
    (ok value)))

(define-private (validate-principal (value principal))
  (begin
    (asserts! (not (is-eq value tx-sender)) ERR_NOT_AUTHORIZED)
    (ok value)))

;; Functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seller / Item functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Seller creates an item listing.
;; price is in microSTX (1 STX = 1,000,000 microSTX).
;; deadline is block-height by which the layaway must be completed.
(define-public (create-item (price uint) (deadline uint))
  (let ((next-id (var-get next-item-id)))
    (begin
      (try! (validate-price price))
      (try! (validate-deadline deadline))
      (map-set items { id: next-id }
        { seller: tx-sender,
          price: price,
          deadline: deadline,
          claimed: false,
          active: true })
      (var-set next-item-id (+ next-id u1))
      (ok next-id))))

;; Seller can deactivate an item that has no active layaway (withdraw from sale)
(define-public (deactivate-item (id uint))
  (let ((item (unwrap! (map-get? items {id: id}) ERR_ITEM_NOT_FOUND)))
    (let ((seller (get seller item)))
      (asserts! (is-eq tx-sender seller) ERR_NOT_ITEM_OWNER)
      (asserts! (get active item) ERR_ITEM_INACTIVE)
      (match (map-get? layaways {id: id})
        lay (if (get active lay)
            ERR_ACTIVE_LAYAWAY 
            (begin
              (map-set items {id: id}
                {seller: seller,
                 price: (get price item),
                 deadline: (get deadline item),
                 claimed: (get claimed item),
                 active: false})
              (ok true)))
        ERR_LAYAWAY_NOT_FOUND))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buyer / Layaway functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Buyer initiates a layaway for an item. Only one layaway allowed per item at a time.
(define-public (initiate-layaway (id uint))
  (let ((item (unwrap! (map-get? items {id: id}) ERR_ITEM_NOT_FOUND)))
    (asserts! (get active item) ERR_ITEM_INACTIVE)
    (match (map-get? layaways {id: id})
      existing-layaway
      (if (get active existing-layaway)
          ERR_ALREADY_HAS_LAYAWAY
          ;; if inactive, allow new layaway
          (begin
            (map-set layaways {id: id}
              {buyer: tx-sender,
               paid: u0,
               active: true})
            (ok true)))
      ;; no existing layaway: create new
      (begin
        (map-set layaways {id: id}
          {buyer: tx-sender,
           paid: u0,
           active: true})
        (ok true)))))

;; Pay an installment toward a layaway
(define-public (pay-installment (id uint) (amount uint))
  (let ((lay (unwrap! (map-get? layaways {id: id}) ERR_NO_LAYAWAY)))
    (let ((buyer (get buyer lay))
          (paid (get paid lay)))
      (asserts! (> amount u0) ERR_INVALID_AMOUNT)
      (asserts! (is-eq buyer tx-sender) ERR_NOT_BUYER)
      ;; check item still active and before deadline
      (let ((item (unwrap! (map-get? items {id: id}) ERR_ITEM_NOT_FOUND)))
        (asserts! (< u0 (get deadline item)) ERR_DEADLINE_PASSED)
        ;; accept transfer from buyer to contract
        (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) ERR_TRANSFER_FAILED)
        (map-set layaways {id: id}
          {buyer: buyer,
           paid: (+ paid amount),
           active: (get active lay)})
        (ok (get paid (unwrap! (map-get? layaways {id: id}) ERR_NO_LAYAWAY)))))))

;; Buyer claims item when fully paid
(define-public (claim-item (id uint))
  (let ((validated-id (try! (validate-uint id))))
    (let ((lay (unwrap! (map-get? layaways {id: validated-id}) ERR_NO_LAYAWAY))
          (item (unwrap! (map-get? items {id: validated-id}) ERR_ITEM_NOT_FOUND)))
      (let ((buyer (get buyer lay))
            (paid (get paid lay))
            (price (get price item))
            (deadline (get deadline item))
            (seller (get seller item)))
        (begin
          ;; Verify conditions
          (asserts! (is-eq buyer tx-sender) ERR_NOT_BUYER)
          (asserts! (get active lay) ERR_NO_LAYAWAY)
          (asserts! (get active item) ERR_ITEM_INACTIVE)
          (asserts! (not (get claimed item)) ERR_ALREADY_CLAIMED)
          (asserts! (< u0 deadline) ERR_DEADLINE_PASSED)
          (asserts! (>= paid price) ERR_INSUFFICIENT_PAYMENT)

          ;; Process transfer
          (let ((overpay (- paid price)))
            (try! (as-contract (stx-transfer? price tx-sender seller)))
            ;; Return any overpay to buyer
            (if (> overpay u0)
                (try! (as-contract (stx-transfer? overpay tx-sender buyer)))
                true)
            ;; Close item and layaway
            (map-set layaways
                {id: validated-id}
                (merge lay {active: false}))
            (map-set items
                {id: validated-id}
                (merge item {claimed: true, active: false}))
            (ok true)))))))


;; Buyer cancels layaway
(define-public (cancel-layaway (id uint))
  (let ((lay (unwrap! (map-get? layaways {id: id}) ERR_NO_LAYAWAY)))
    (let ((buyer (get buyer lay))
          (paid (get paid lay)))
      (asserts! (is-eq buyer tx-sender) ERR_NOT_BUYER)
      (asserts! (get active lay) ERR_NO_LAYAWAY)
      (let ((item (unwrap! (map-get? items {id: id}) ERR_ITEM_NOT_FOUND)))
        (let ((deadline (get deadline item))
              (seller (get seller item)))
          (asserts! (< u0 deadline) ERR_DEADLINE_PASSED)
          (if (is-eq paid u0)
              ;; nothing to refund, just clear layaway
              (begin
                (map-set layaways {id: id}
                  {buyer: buyer,
                   paid: u0,
                   active: false})
                (ok true))
              (let ((fee (* paid CANCELLATION_FEE_PERCENT))
                    (fee-div (/ fee u100))
                    (refund (- paid fee-div)))
                ;; transfer fee to seller
                (unwrap! (stx-transfer? fee-div (as-contract tx-sender) seller) ERR_TRANSFER_FAILED)
                (unwrap! (stx-transfer? refund (as-contract tx-sender) buyer) ERR_TRANSFER_FAILED)
                ;; clear layaway (item remains active)
                (map-set layaways {id: id}
                  {buyer: buyer,
                   paid: u0,
                   active: false})
                (ok true))))))))

;; Seller withdraws failed layaway
(define-public (withdraw-failed (id uint))
  (begin
    (asserts! (> id u0) ERR_INVALID_ID)
    (let ((item (unwrap! (map-get? items {id: id}) ERR_ITEM_NOT_FOUND))
          (lay (unwrap! (map-get? layaways {id: id}) ERR_NO_LAYAWAY)))
      (let ((seller (get seller item))
            (deadline (get deadline item))
            (price (get price item))
            (active (get active item))
            (paid (get paid lay)))
        (begin
          ;; Verify conditions
          (asserts! (is-eq seller tx-sender) ERR_NOT_ITEM_OWNER)
          (asserts! (>= u0 deadline) ERR_DEADLINE_NOT_PASSED)
          (asserts! active ERR_ITEM_INACTIVE)

          (if (is-eq paid u0)
              ;; nothing to withdraw, just close item
              (begin
                (map-set items {id: id}
                  {seller: seller,
                   price: price,
                   deadline: deadline,
                   claimed: false,
                   active: false})
                (map-set layaways {id: id}
                  {buyer: (get buyer lay),
                   paid: u0,
                   active: false})
                (ok true))
              ;; transfer paid to seller
              (begin
                (try! (as-contract (stx-transfer? paid (as-contract tx-sender) seller)))
                (map-set items {id: id}
                  {seller: seller,
                   price: price,
                   deadline: deadline,
                   claimed: false,
                   active: false})
                (map-set layaways {id: id}
                  {buyer: (get buyer lay),
                   paid: u0,
                   active: false})
                (ok true))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read-only helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-item (id uint))
  (ok (unwrap! (map-get? items {id: id}) ERR_ITEM_NOT_FOUND)))

(define-read-only (get-layaway (id uint))
  (match (map-get? layaways {id: id})
    lay (ok lay)
    ERR_NO_LAYAWAY))

(define-read-only (is-fully-paid? (id uint))
  (match (map-get? items { id: id })
    some-item (match (map-get? layaways { id: id })
                some-lay (ok (>= (get paid some-lay) (get price some-item)))
                (ok false))
    (ok false)))

;; Helper functions
(define-private (validate-deadline (value uint))
  (if (> value u0)
      (ok value)
      ERR_DEADLINE_PASSED))

(define-private (validate-price (value uint))
  (if (> value u0)
      (ok value)
      ERR_INVALID_AMOUNT))