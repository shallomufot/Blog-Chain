;; Decentralized Blog Smart Contract
;; A comprehensive blog platform on Stacks blockchain

;; Contract constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_POST_NOT_FOUND (err u101))
(define-constant ERR_INVALID_PARAMS (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_POST_LOCKED (err u105))
(define-constant ERR_COMMENT_NOT_FOUND (err u106))
(define-constant ERR_USER_NOT_FOUND (err u107))
(define-constant DEFAULT_SUBSCRIPTION_DURATION u144000) ;; ~1000 blocks ~ 1 week

;; Contract variables
(define-data-var post-counter uint u0)
(define-data-var comment-counter uint u0)
(define-data-var platform-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var contract-paused bool false)

;; Data structures
(define-map posts
  { post-id: uint }
  {
    author: principal,
    title: (string-ascii 100),
    content: (string-utf8 2000),
    timestamp: uint,
    likes: uint,
    comments-count: uint,
    category: (string-ascii 50),
    tags: (list 10 (string-ascii 20)),
    locked: bool,
    premium: bool,
    price: uint
  }
)

(define-map users
  { user: principal }
  {
    username: (string-ascii 50),
    bio: (string-utf8 500),
    posts-count: uint,
    reputation: uint,
    joined-at: uint,
    verified: bool
  }
)

(define-map comments
  { comment-id: uint }
  {
    post-id: uint,
    author: principal,
    content: (string-utf8 500),
    timestamp: uint,
    likes: uint,
    parent-comment: (optional uint)
  }
)

(define-map post-likes
  { post-id: uint, user: principal }
  { liked: bool }
)

(define-map comment-likes
  { comment-id: uint, user: principal }
  { liked: bool }
)

(define-map followers
  { follower: principal, following: principal }
  { active: bool }
)

(define-map user-subscriptions
  { subscriber: principal, author: principal }
  { active: bool, expires-at: uint }
)

(define-map categories
  { category: (string-ascii 50) }
  { active: bool, post-count: uint }
)

;; Authorization functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (is-post-author (post-id uint))
  (match (map-get? posts { post-id: post-id })
    post-data (is-eq tx-sender (get author post-data))
    false
  )
)

(define-private (contract-not-paused)
  (not (var-get contract-paused))
)

;; User management functions
(define-public (register-user (username (string-ascii 50)) (bio (string-utf8 500)))
  (let ((user-exists (is-some (map-get? users { user: tx-sender }))))
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (not user-exists) ERR_ALREADY_EXISTS)
    (asserts! (> (len username) u0) ERR_INVALID_PARAMS)
    (asserts! (<= (len username) u50) ERR_INVALID_PARAMS)
    (asserts! (<= (len bio) u500) ERR_INVALID_PARAMS)
    (ok (map-set users
      { user: tx-sender }
      {
        username: username,
        bio: bio,
        posts-count: u0,
        reputation: u0,
        joined-at: stacks-block-height,
        verified: false
      }
    ))
  )
)

(define-public (update-user-profile (username (string-ascii 50)) (bio (string-utf8 500)))
  (let ((user-data (unwrap! (map-get? users { user: tx-sender }) ERR_USER_NOT_FOUND)))
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (> (len username) u0) ERR_INVALID_PARAMS)
    (ok (map-set users
      { user: tx-sender }
      (merge user-data { username: username, bio: bio })
    ))
  )
)

;; Post management functions
(define-public (create-post 
  (title (string-ascii 100)) 
  (content (string-utf8 2000)) 
  (category (string-ascii 50))
  (tags (list 10 (string-ascii 20)))
  (premium bool)
  (price uint))
  (let (
    (post-id (+ (var-get post-counter) u1))
    (user-data (map-get? users { user: tx-sender }))
    (current-category (map-get? categories { category: category }))
  )
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (> (len title) u0) ERR_INVALID_PARAMS)
    (asserts! (> (len content) u0) ERR_INVALID_PARAMS)
    (asserts! (> (len category) u0) ERR_INVALID_PARAMS)
    (asserts! (<= (len tags) u10) ERR_INVALID_PARAMS)
    (asserts! (<= price u1000000000) ERR_INVALID_PARAMS) ;; Max 1000 STX
    (asserts! (or (not premium) (> price u0)) ERR_INVALID_PARAMS) ;; Premium posts must have price > 0
    
    ;; Create or update category
    (map-set categories
      { category: category }
      { 
        active: true, 
        post-count: (+ 
          (match current-category
            category-data (get post-count category-data)
            u0
          ) 
          u1
        )
      }
    )
    
    ;; Create post
    (map-set posts
      { post-id: post-id }
      {
        author: tx-sender,
        title: title,
        content: content,
        timestamp: stacks-block-height,
        likes: u0,
        comments-count: u0,
        category: category,
        tags: tags,
        locked: false,
        premium: premium,
        price: price
      }
    )
    
    ;; Update user posts count
    (match user-data
      user-info (map-set users
        { user: tx-sender }
        (merge user-info { posts-count: (+ (get posts-count user-info) u1) })
      )
      ;; Auto-register user if not exists
      (map-set users
        { user: tx-sender }
        {
          username: "Anonymous",
          bio: u"",
          posts-count: u1,
          reputation: u0,
          joined-at: stacks-block-height,
          verified: false
        }
      )
    )
    
    (var-set post-counter post-id)
    (ok post-id)
  )
)

(define-public (edit-post 
  (post-id uint) 
  (title (string-ascii 100)) 
  (content (string-utf8 2000))
  (category (string-ascii 50))
  (tags (list 10 (string-ascii 20))))
  (let ((post-data (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND)))
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (is-post-author post-id) ERR_UNAUTHORIZED)
    (asserts! (not (get locked post-data)) ERR_POST_LOCKED)
    (asserts! (> (len title) u0) ERR_INVALID_PARAMS)
    (asserts! (> (len content) u0) ERR_INVALID_PARAMS)
    (asserts! (> (len category) u0) ERR_INVALID_PARAMS)
    
    (ok (map-set posts
      { post-id: post-id }
      (merge post-data {
        title: title,
        content: content,
        category: category,
        tags: tags
      })
    ))
  )
)

(define-public (delete-post (post-id uint))
  (let ((post-data (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND)))
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (> post-id u0) ERR_INVALID_PARAMS)
    (asserts! (<= post-id (var-get post-counter)) ERR_INVALID_PARAMS)
    (asserts! (or (is-post-author post-id) (is-contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (not (get locked post-data)) ERR_POST_LOCKED)
    
    ;; Update user posts count
    (match (map-get? users { user: (get author post-data) })
      user-info (map-set users
        { user: (get author post-data) }
        (merge user-info { posts-count: (- (get posts-count user-info) u1) })
      )
      true
    )
    
    (ok (map-delete posts { post-id: post-id }))
  )
)

;; Like/Unlike functions
(define-public (like-post (post-id uint))
  (let (
    (post-data (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND))
    (current-like (match (map-get? post-likes { post-id: post-id, user: tx-sender })
      like-data (get liked like-data)
      false
    ))
  )
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (> post-id u0) ERR_INVALID_PARAMS)
    (asserts! (<= post-id (var-get post-counter)) ERR_INVALID_PARAMS)
    (asserts! (not (is-eq tx-sender (get author post-data))) ERR_UNAUTHORIZED)
    
    (if current-like
      ;; Unlike post
      (begin
        (map-set post-likes { post-id: post-id, user: tx-sender } { liked: false })
        (map-set posts { post-id: post-id } 
          (merge post-data { likes: (- (get likes post-data) u1) }))
        (ok false)
      )
      ;; Like post
      (begin
        (map-set post-likes { post-id: post-id, user: tx-sender } { liked: true })
        (map-set posts { post-id: post-id } 
          (merge post-data { likes: (+ (get likes post-data) u1) }))
        ;; Increase author reputation
        (match (map-get? users { user: (get author post-data) })
          user-info (map-set users
            { user: (get author post-data) }
            (merge user-info { reputation: (+ (get reputation user-info) u1) })
          )
          true
        )
        (ok true)
      )
    )
  )
)

;; Comment functions
(define-public (add-comment (post-id uint) (content (string-utf8 500)) (parent-comment (optional uint)))
  (let (
    (post-data (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND))
    (comment-id (+ (var-get comment-counter) u1))
  )
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (> post-id u0) ERR_INVALID_PARAMS)
    (asserts! (<= post-id (var-get post-counter)) ERR_INVALID_PARAMS)
    (asserts! (> (len content) u0) ERR_INVALID_PARAMS)
    (asserts! (<= (len content) u500) ERR_INVALID_PARAMS)
    
    ;; Validate parent comment if provided
    (match parent-comment
      parent-id (begin
        (asserts! (> parent-id u0) ERR_INVALID_PARAMS)
        (asserts! (<= parent-id (var-get comment-counter)) ERR_INVALID_PARAMS)
        (asserts! (is-some (map-get? comments { comment-id: parent-id })) ERR_COMMENT_NOT_FOUND)
      )
      true
    )
    
    ;; Create comment
    (map-set comments
      { comment-id: comment-id }
      {
        post-id: post-id,
        author: tx-sender,
        content: content,
        timestamp: stacks-block-height,
        likes: u0,
        parent-comment: parent-comment
      }
    )
    
    ;; Update post comments count
    (map-set posts { post-id: post-id }
      (merge post-data { comments-count: (+ (get comments-count post-data) u1) })
    )
    
    (var-set comment-counter comment-id)
    (ok comment-id)
  )
)

(define-public (like-comment (comment-id uint))
  (let (
    (comment-data (unwrap! (map-get? comments { comment-id: comment-id }) ERR_COMMENT_NOT_FOUND))
    (current-like (match (map-get? comment-likes { comment-id: comment-id, user: tx-sender })
      like-data (get liked like-data)
      false
    ))
  )
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (> comment-id u0) ERR_INVALID_PARAMS)
    (asserts! (<= comment-id (var-get comment-counter)) ERR_INVALID_PARAMS)
    (asserts! (not (is-eq tx-sender (get author comment-data))) ERR_UNAUTHORIZED)
    
    (if current-like
      ;; Unlike comment
      (begin
        (map-set comment-likes { comment-id: comment-id, user: tx-sender } { liked: false })
        (map-set comments { comment-id: comment-id } 
          (merge comment-data { likes: (- (get likes comment-data) u1) }))
        (ok false)
      )
      ;; Like comment
      (begin
        (map-set comment-likes { comment-id: comment-id, user: tx-sender } { liked: true })
        (map-set comments { comment-id: comment-id } 
          (merge comment-data { likes: (+ (get likes comment-data) u1) }))
        (ok true)
      )
    )
  )
)

;; Follow/Unfollow functions
(define-public (follow-user (user-to-follow principal))
  (let ((current-follow (match (map-get? followers { follower: tx-sender, following: user-to-follow })
    follow-data (get active follow-data)
    false
  )))
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq tx-sender user-to-follow)) ERR_UNAUTHORIZED)
    
    (if current-follow
      ;; Unfollow
      (begin
        (map-set followers { follower: tx-sender, following: user-to-follow } { active: false })
        (ok false)
      )
      ;; Follow
      (begin
        (map-set followers { follower: tx-sender, following: user-to-follow } { active: true })
        (ok true)
      )
    )
  )
)

;; Premium content functions
(define-public (purchase-premium-post (post-id uint))
  (let (
    (post-data (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND))
    (post-price (get price post-data))
  )
    (asserts! (contract-not-paused) ERR_UNAUTHORIZED)
    (asserts! (get premium post-data) ERR_INVALID_PARAMS)
    (asserts! (> post-price u0) ERR_INVALID_PARAMS)
    
    ;; Transfer payment to author
    (try! (stx-transfer? post-price tx-sender (get author post-data)))
    
    ;; Record subscription
    (map-set user-subscriptions
      { subscriber: tx-sender, author: (get author post-data) }
      { active: true, expires-at: (+ stacks-block-height DEFAULT_SUBSCRIPTION_DURATION) }
    )
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-post (post-id uint))
  (map-get? posts { post-id: post-id })
)

(define-read-only (get-user (user principal))
  (map-get? users { user: user })
)

(define-read-only (get-comment (comment-id uint))
  (map-get? comments { comment-id: comment-id })
)

(define-read-only (get-post-count)
  (var-get post-counter)
)

(define-read-only (get-comment-count)
  (var-get comment-counter)
)

(define-read-only (is-post-liked (post-id uint) (user principal))
  (match (map-get? post-likes { post-id: post-id, user: user })
    like-data (get liked like-data)
    false
  )
)

(define-read-only (is-comment-liked (comment-id uint) (user principal))
  (match (map-get? comment-likes { comment-id: comment-id, user: user })
    like-data (get liked like-data)
    false
  )
)

(define-read-only (is-following (follower principal) (following principal))
  (match (map-get? followers { follower: follower, following: following })
    follow-data (get active follow-data)
    false
  )
)

(define-read-only (has-premium-access (subscriber principal) (author principal))
  (match (map-get? user-subscriptions { subscriber: subscriber, author: author })
    subscription-data (and (get active subscription-data) (> (get expires-at subscription-data) stacks-block-height))
    false
  )
)

(define-read-only (get-category-info (category (string-ascii 50)))
  (map-get? categories { category: category })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

;; Admin functions
(define-public (toggle-contract-pause)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (var-set contract-paused (not (var-get contract-paused)))
    (ok (var-get contract-paused))
  )
)

(define-public (verify-user (user principal))
  (let ((user-data (unwrap! (map-get? users { user: user }) ERR_USER_NOT_FOUND)))
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq user tx-sender)) ERR_INVALID_PARAMS) ;; Can't verify self
    (ok (map-set users { user: user } (merge user-data { verified: true })))
  )
)

(define-public (lock-post (post-id uint))
  (let ((post-data (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND)))
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (> post-id u0) ERR_INVALID_PARAMS)
    (asserts! (<= post-id (var-get post-counter)) ERR_INVALID_PARAMS)
    (asserts! (not (get locked post-data)) ERR_POST_LOCKED) ;; Already locked
    (ok (map-set posts { post-id: post-id } (merge post-data { locked: true })))
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee u10000000) ERR_INVALID_PARAMS) ;; Max 10 STX
    (var-set platform-fee new-fee)
    (ok new-fee)
  )
)

;; Emergency functions
(define-public (emergency-withdraw)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (try! (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender CONTRACT_OWNER)))
    (ok true)
  )
)