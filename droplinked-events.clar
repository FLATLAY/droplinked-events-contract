(impl-trait .nft-trait.nft-trait)

(define-constant admin 'SPTYMZEAQ7CGTGF8Z7EP62CEP1WCFSBH6D1J0NRB)
(define-constant stakeholder 'SPTYMZEAQ7CGTGF8Z7EP62CEP1WCFSBH6D1J0NRB)

(define-non-fungible-token droplinked-event-nft uint)

(define-constant normal "normal")
(define-constant vip "vip")
(define-constant vip-plus "vip-plus")

(define-map nft-count (string-ascii 256) uint)
(map-insert nft-count normal u1000)
(map-insert nft-count vip u25)
(map-insert nft-count vip-plus u25)

(define-map nft-mint-count (string-ascii 256) uint)
(map-insert nft-mint-count normal u0)
(map-insert nft-mint-count vip u0)
(map-insert nft-mint-count vip-plus u0)

(define-map nft-mint-price (string-ascii 256) uint)
(map-insert nft-mint-price normal u100000)
(map-insert nft-mint-price vip u115000)
(map-insert nft-mint-price vip-plus u120000)

(define-map nft-uri (string-ascii 256) (string-ascii 256))
(map-insert nft-uri normal "")
(map-insert nft-uri vip "")
(map-insert nft-uri vip-plus "")

(define-map nft-allowed-transfer (string-ascii 256) bool)
(map-insert nft-allowed-transfer normal true)
(map-insert nft-allowed-transfer vip false)
(map-insert nft-allowed-transfer vip-plus false)

(define-map nft-allowed-mint (string-ascii 256) bool)
(map-insert nft-allowed-mint normal true)
(map-insert nft-allowed-mint vip false)
(map-insert nft-allowed-mint vip-plus false)

(define-map nft-type uint (string-ascii 256))

(define-data-var last-id uint u0)

(define-constant err-nft-type-invalid (err u1000))
(define-constant err-nft-type-untransferable (err u1001))
(define-constant err-nft-type-mint-locked (err u1001))
(define-constant err-nft-type-max-count-reached (err u1001))
(define-constant err-unauthorized (err u1005))

(define-read-only (get-nft-type (id uint)) 
  (ok (map-get? nft-type id))
)

(define-read-only (get-last-token-id)
  (ok (var-get last-id))
)

(define-read-only (get-token-uri (id uint)) 
  (let 
    (
      (type (unwrap! (unwrap-panic (get-nft-type id)) (ok none)))
    )
    (ok (map-get? nft-uri type))
  )
)

(define-read-only (get-owner (id uint)) 
  (ok (nft-get-owner? droplinked-event-nft id))
)

(define-public
  (transfer
    (id uint)
    (sender principal)
    (recipient principal)
  )
  (let 
    (
      (type (unwrap! (unwrap-panic (get-nft-type id)) err-nft-type-invalid ))
      (allowed-transfer (default-to false (map-get? nft-allowed-transfer type)))
    )
    (asserts! (is-eq sender tx-sender) err-unauthorized)
    (try! (nft-transfer? droplinked-event-nft id sender recipient))
    (ok true)
  )
)

(define-public
  (mint
    (recipient principal)
    (type (string-ascii 256))
  )
  (let 
    (
      (id (+ (var-get last-id) u1))
      (allowed-mint (unwrap! (map-get? nft-allowed-mint type) err-nft-type-invalid))
      (price (unwrap! (map-get? nft-mint-price type) err-nft-type-invalid))
      (max-count (unwrap! (map-get? nft-count type) err-nft-type-invalid))
      (mint-count (unwrap! (map-get? nft-mint-count type) err-nft-type-invalid))
    )
    (begin
      (if 
        (is-eq tx-sender admin)
        (mint-nft recipient type id max-count mint-count)
        (begin
          (asserts! (is-eq allowed-mint true) err-nft-type-mint-locked)
          (asserts! (is-eq tx-sender recipient) err-unauthorized)
          (try! (stx-transfer? price tx-sender stakeholder))
          (mint-nft recipient type id max-count mint-count)
        )
      )
    )
  )
)

(define-private
  (mint-nft
    (recipient principal)
    (type (string-ascii 256))
    (id uint)
    (max-count uint)
    (mint-count uint)
  ) 
  (begin 
    (asserts! (<= mint-count max-count) err-nft-type-max-count-reached)
    (try! (nft-mint? droplinked-event-nft id recipient))
    (map-insert nft-type id type)
    (map-set nft-mint-count type (+ mint-count u1))
    (ok (var-set last-id id))
  )
)

(define-public
  (update-transfer-permissions
    (type (string-ascii 256))
    (allowed bool)
  ) 
  (begin
    (asserts! (is-eq tx-sender admin) err-unauthorized)
    (ok (map-set nft-allowed-transfer type allowed))
  )
)

(define-public
  (update-mint-permissions
    (type (string-ascii 256))
    (allowed bool)
  ) 
  (begin
    (asserts! (is-eq tx-sender admin) err-unauthorized)
    (ok (map-set nft-allowed-mint type allowed))
  )
)